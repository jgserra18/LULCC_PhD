
library(abind)

mod_fillGridDates = function(grid, tz='') {
  
  station = ("loc" %in% getDim(grid)) 
  grid = redim(grid, runtime = TRUE, var = TRUE)
  start = getRefDates(grid)
  end = getRefDates(grid, which = "end")
  day.step = as.numeric(names(which.max(table(difftime(c(start, NA), c(NA, start), units = "days")))))
  message("Time difference of ", day.step, " days")
  formato = "%Y-%m-%d %H:%M:%S"
  if (day.step >= 1) formato = "%Y-%m-%d"
  start = as.POSIXlt(start, format = formato, tz = tz)
  end = as.POSIXlt(end, format = formato, tz = tz)
  #xs = as.POSIXlt(as.character(seq.POSIXt(start[1], start[length(start)],
  #                                        by = day.step*24*60*60)),
  #                format = formato, tz = tz)
  # xe = as.POSIXlt(as.character(seq.POSIXt(end[1], end[length(end)],
  #                                         by = day.step*24*60*60)),
  #               format = formato, tz = tz)
  #end = NULL
  test = data.frame("date" = start, "wh" = TRUE)
  #start = NULL
  result = merge(data.frame("date" = start), test,
                 by.y = "date", by.x = "date", all.x = TRUE)
  ind = which(result[, "wh"])
  sh = getShape(grid)
  sh[names(sh) == "time"] = nrow(result)
  result = NULL
  arr = array(data = NA, dim = sh)
  arr[,,, ind ,,] = grid[["Data"]] 
  grid[["Data"]] = arr
  arr = NULL
  attr(grid[["Data"]], "dimensions") = names(sh)
  grid[["Dates"]][["start"]] = strftime(start, format = formato, tz = tz, usetz = TRUE)
  grid[["Dates"]][["end"]] = strftime(end, format = formato, tz = tz, usetz = TRUE)
  # xs = xe = NULL
  grid = redim(grid, drop = TRUE, loc = station)
  
  return(grid)
}


mod_biasCorrection = function(y, x, newdata = NULL, precipitation = FALSE,
                              method = c("delta", "scaling", "eqm", "pqm", "gpqm", "loci","dqm","qdm", "isimip3"),
                              cross.val = c("none", "loo", "kfold"),
                              folds = NULL,
                              consecutive = TRUE,
                              window = NULL,
                              scaling.type = c("additive", "multiplicative"),
                              fitdistr.args = list(densfun = "normal"),
                              wet.threshold = 1,
                              n.quantiles = NULL,
                              extrapolation = c("none", "constant"), 
                              theta = c(.95,.05),
                              detrend = TRUE,
                              isimip3.args = NULL,
                              join.members = FALSE,
                              return.raw = FALSE,
                              interpGrid.args = list(),
                              parallel = FALSE,
                              max.ncores = 16,
                              ncores = NULL) {
  if (method == "gqm") stop("'gqm' is not a valid choice anymore. Use method = 'pqm' instead and set fitdistr.args = list(densfun = 'gamma')")
  method = match.arg(method, choices = c("delta", "scaling", "eqm", "pqm", "gpqm", "mva", "loci", "ptr", "variance","dqm","qdm", "isimip3"))
  cross.val = match.arg(cross.val, choices = c("none", "loo", "kfold"))
  scaling.type = match.arg(scaling.type, choices = c("additive", "multiplicative"))
  extrapolation = match.arg(extrapolation, choices = c("none", "constant"))
  stopifnot(is.logical(join.members))
  nwdatamssg = TRUE
  if (is.null(newdata)) {
    newdata = x 
    nwdatamssg = FALSE
  }
  # ####temporal solution for applying the isimip method###########
  # if (method == "isimip") {
  #       warning("cross-validation, window and joining member options are not implemented for the isimip method yet.")
  #       suppressMessages(x = interpGrid(x, getGrid(y)))
  #       suppressMessages(newdata = interpGrid(newdata, getGrid(y)))
  #       output = do.call("isimip", list(y = y, x = x, newdata = newdata, threshold = wet.threshold, type = scaling.type))
  # } else {
  # ##################################################
  seas = getSeason(y)
  y = mod_fillGridDates(y)
  x = mod_fillGridDates(x)
  newdata = mod_fillGridDates(newdata)
  yx = intersectGrid(y, x, type = "temporal", which.return = 1:2)
  y = yx[[1]]
  x = yx[[2]]
  if (cross.val == "none") {
    output = biasCorrectionXD(y = y, x = x, newdata = newdata, 
                              precipitation = precipitation,
                              method = method,
                              window = window,
                              scaling.type = scaling.type,
                              fitdistr.args = fitdistr.args,
                              pr.threshold = wet.threshold, 
                              n.quantiles = n.quantiles, 
                              extrapolation = extrapolation, 
                              theta = theta,
                              join.members = join.members,
                              detrend = detrend,
                              isimip3.args = isimip3.args,
                              return.raw = return.raw,
                              interpGrid.args = interpGrid.args,
                              parallel = parallel,
                              max.ncores = max.ncores,
                              ncores = ncores)
    output$Data[which(is.infinite(output$Data))] = NA
  } else {
    if (nwdatamssg) {
      message("'newdata' will be ignored for cross-validation")
    }
    if (cross.val == "loo") {
      years = as.list(unique(getYearsAsINDEX(x)))
    } else if (cross.val == "kfold" & !is.null(folds)) {
      if (!is.list(folds)) {
        avy = unique(getYearsAsINDEX(y))
        ind = rep(1:folds, length(avy)/folds, length.out = length(avy))
        ind = if (consecutive) {
          sort(ind) 
        } else {
          sample(ind, length(ind))
        }
        folds = split(avy, f = ind)
      }
      years = folds
    } else if (cross.val == "kfold" & is.null(folds)) {
      stop("Fold specification is missing, with no default")
    }
    output.list = lapply(1:length(years), function(i) {
      target.year = years[[i]]
      rest.years = setdiff(unlist(years), target.year)
      station = FALSE
      if ("loc" %in% getDim(y)) station = TRUE
      yy = redim(y, member = FALSE)
      yy = if (method == "delta") {
        subsetGrid(yy, years = target.year, drop = FALSE)
      } else {
        subsetGrid(yy, years = rest.years, drop = FALSE)
      }
      if (isTRUE(station)) {
        yy$Data = abind::adrop(yy$Data, drop = 3)
        attr(yy$Data, "dimensions") = c(setdiff(getDim(yy), c("lat", "lon")), "loc")
      } else {
        yy = redim(yy, drop = TRUE)
      }
      newdata2 = subsetGrid(x, years = target.year, drop = F)
      xx = subsetGrid(x, years = rest.years, drop = F)
      message("Validation ", i, ", ", length(unique(years)) - i, " remaining")
      biasCorrectionXD(y = yy, x = xx, newdata = newdata2, precipitation = precipitation,
                       method = method,
                       window = window,
                       scaling.type = scaling.type,
                       fitdistr.args = fitdistr.args,
                       pr.threshold = wet.threshold, n.quantiles = n.quantiles, extrapolation = extrapolation, 
                       theta = theta, join.members = join.members,
                       detrend = detrend,
                       isimip3.args = isimip3.args,
                       return.raw = return.raw,
                       interpGrid.args = interpGrid.args,
                       parallel = parallel,
                       max.ncores = max.ncores,
                       ncores = ncores)
    })
    output = redim(bindGrid(output.list, dimension = "time"), drop = TRUE)
    # al = which(getDim(x) == "time")
    # Data = sapply(output.list, function(n) unname(n$Data), simplify = FALSE)
    # bindata = unname(do.call("abind", c(Data, along = al)))
    # output = output.list[[1]]
    # dimNames = attr(output$Data, "dimensions")
    # output$Data = bindata
    # attr(output$Data, "dimensions") = dimNames
    # output$Dates = x$Dates
    output$Data[which(is.infinite(output$Data))] = NA
  }
  output = subsetGrid(output, season = seas)
  return(output)
}

biasCorrectionXD = function(y, x, newdata, 
                            precipitation, 
                            method,
                            window,
                            scaling.type,
                            fitdistr.args,
                            pr.threshold, 
                            n.quantiles, 
                            extrapolation, 
                            theta,
                            join.members,
                            detrend,
                            isimip3.args,
                            return.raw = FALSE,
                            interpGrid.args = list(),
                            parallel = FALSE,
                            max.ncores = 16,
                            ncores = NULL) {
  if (method == "isimip3") {
    window = NULL
    # warning("Only parameter isimip3.args is considered")
    if (is.null(isimip3.args)) isimip3.args = list()
    isimip3.args[["dates"]] = list(obs_hist = y[["Dates"]][["start"]],
                                   sim_hist = x[["Dates"]][["start"]],
                                   sim_fut = newdata[["Dates"]][["start"]])
  }
  station = FALSE
  if ("loc" %in% getDim(y)) station = TRUE
  xy = y$xyCoords
  # suppressWarnings(suppressMessages(pred = interpGrid(x, getGrid(y), force.non.overlapping = TRUE)))
  # suppressWarnings(suppressMessages(sim = interpGrid(newdata, getGrid(y), force.non.overlapping = TRUE)))
  interpGrid.args[["new.coordinates"]] = getGrid(y)
  interpGrid.args[["grid"]] = x
  suppressWarnings(suppressMessages(pred = do.call("interpGrid", interpGrid.args)))
  interpGrid.args[["grid"]] = newdata
  suppressWarnings(suppressMessages(sim = do.call("interpGrid", interpGrid.args)))
  delta.method = method == "delta"
  precip = precipitation
  message("[", Sys.time(), "] Argument precipitation is set as ", precip, ", please ensure that this matches your data.")
  bc = y
  if (isTRUE(join.members) & getShape(redim(sim))["member"] > 1) {
    n.mem.aux = getShape(sim)["member"]
    pred = flatMemberDim(pred, station)
    pred = redim(pred, drop = T)
    sim = flatMemberDim(sim, station)
    sim = redim(sim, drop = T)
    y = bindGrid(rep(list(y), n.mem.aux), dimension = "time")
  } else if (isTRUE(join.members) & !getShape(redim(sim))["member"] > 1) {
    warning("There is only one member, argument join.members ignored.")
    join.members = FALSE
  }
  y = redim(y, drop = TRUE)
  y = redim(y, member = FALSE, runtime = FALSE)
  pred = redim(pred, member = TRUE, runtime = TRUE)
  sim = redim(sim, member = TRUE, runtime = TRUE)
  dimNames = attr(y$Data, "dimensions")
  n.run = getShape(sim)["runtime"]
  n.mem = getShape(sim)["member"]
  if (join.members & !is.null(window)) {
    message("[", Sys.time(), "] Window option is currently not supported for joined members and will be ignored")
    window = NULL
  }
  if (!is.null(window)) {
    win = getWindowIndex(y = y, x = pred, newdata = sim, window = window, delta.method = delta.method)
  } else {
    win = list()
    indobservations = match(as.POSIXct(pred$Dates$start), as.POSIXct(y$Dates$start))
    ## esto no mola, es para el caso especial de join members...hay que mirarlo
    if (length(indobservations) > length(unique(indobservations))) indobservations = 1:length(indobservations) 
    win[["Window1"]] = list("obsWindow" = indobservations, "window" = 1:getShape(pred)["time"], "step" = 1:getShape(sim)["time"])
    if (delta.method) win[["Window1"]][["deltaind"]] = indobservations
  }
  message("[", Sys.time(), "] Number of windows considered: ", length(win), "...")
  winarr = array(dim = dim(sim$Data))
  if (delta.method) winarr = array(dim = c(n.run, n.mem, getShape(y)))
  for (j in 1:length(win)) {
    yind = win[[j]]$obsWindow
    outind = win[[j]]$step
    if (delta.method) {
      yind = win[[j]]$deltaind
      outind = win[[j]]$deltaind
    } 
    yw = y$Data[yind,,, drop = FALSE]
    pw = pred$Data[,,win[[j]]$window,,, drop = FALSE]
    sw = sim$Data[,,win[[j]]$step,,, drop = FALSE]
    runarr = lapply(1:n.run, function(l){
      memarr = lapply(1:n.mem, function(m){
        #join members message
        if (j == 1 & m == 1) {
          if (!isTRUE(join.members)) {
            message("[", Sys.time(), "] Bias-correcting ", n.mem, " members separately...")
          } else {
            message("[", Sys.time(), "] Bias-correcting ", attr(pred, "orig.mem.shape"), " members considering their joint distribution...")
          }
        }
        o = yw[, , , drop = FALSE]
        p = abind::adrop(pw[l, m, , , , drop = FALSE], drop = c(T, T, F, F, F))
        s = abind::adrop(sw[l, m, , , , drop = FALSE], drop = c(T, T, F, F, F))
        data = list(o, p, s)
        if (!station) {
          data = lapply(1:length(data), function(x) {
            attr(data[[x]], "dimensions") = dimNames
            abind::abind(array3Dto2Dmat(data[[x]]), along = 3)
          }) 
        }
        o = lapply(seq_len(ncol(data[[1]])), function(i) data[[1]][,i,1])
        p = lapply(seq_len(ncol(data[[2]])), function(i) data[[2]][,i,1])
        s = lapply(seq_len(ncol(data[[3]])), function(i) data[[3]][,i,1])
        data = NULL
        mat = biasCorrection1D(o, p, s,
                               method = method,
                               scaling.type = scaling.type,
                               fitdistr.args = fitdistr.args,
                               precip = precip,
                               pr.threshold = pr.threshold,
                               n.quantiles = n.quantiles,
                               extrapolation = extrapolation,
                               theta = theta,
                               detrend = detrend,
                               isimip3.args = isimip3.args,
                               parallel = parallel,
                               max.ncores = max.ncores,
                               ncores = ncores)  
        if (!station) mat = mat2Dto3Darray(mat, xy$x, xy$y)
        mat
      })
      unname(do.call("abind", list(memarr, along = 0)))
    })
    yw = pw = sw = NULL
    winarr[,,outind,,] = unname(do.call("abind", list(runarr, along = 0))) 
    runarr = NULL
  }
  bc$Data = unname(do.call("abind", list(winarr, along = 3)))
  winarr = NULL
  attr(bc$Data, "dimensions") = attr(sim$Data, "dimensions")
  if (station) bc = redim(bc, loc = TRUE)
  bc$Dates = sim$Dates
  ## Recover the member dimension when join.members=TRUE:
  if (isTRUE(join.members)) {
    if (method == "delta") {
      bc = recoverMemberDim(plain.grid = pred, bc.grid = bc, newdata = newdata)
    }else{
      bc = recoverMemberDim(plain.grid = sim, bc.grid = bc, newdata = newdata)      
    }
  } else {
    bc$InitializationDates = sim$InitializationDates
    bc$Members = sim$Members
  }
  if (return.raw) {
    sim[["Variable"]][["varName"]] = paste0(bc[["Variable"]][["varName"]], "_raw")
    bc = makeMultiGrid(bc, sim)
    if (station){
      bc = redim(bc, loc = TRUE)
    }
  }
  pred = newdata = sim = y = NULL
  attr(bc$Variable, "correction") = method
  bc = redim(bc, drop = TRUE)
  message("[", Sys.time(), "] Done.")
  return(bc)
}


biasCorrection1D = function(o, p, s,
                            method, 
                            scaling.type,
                            fitdistr.args,
                            precip, 
                            pr.threshold,
                            n.quantiles,
                            extrapolation, 
                            theta,
                            detrend,
                            isimip3.args,
                            parallel = FALSE,
                            max.ncores = 16,
                            ncores = NULL) {
  parallel.pars = parallelCheck(parallel, max.ncores, ncores)
  mapply_fun = selectPar.pplyFun(parallel.pars, .pplyFUN = "mapply")
  if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
  if (method == "delta") {
    mapply_fun(delta, o, p, s)
  } else if (method == "scaling") {
    mapply_fun(scaling, o, p, s, MoreArgs = list(scaling.type = scaling.type))
  } else if (method == "eqm") {
    suppressWarnings(
      mapply_fun(eqm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles, extrapolation))
    )
  } else if (method == "pqm") {
    suppressWarnings(
      mapply_fun(pqm, o, p, s, MoreArgs = list(fitdistr.args, precip, pr.threshold))
    )
  } else if (method == "gpqm") {
    mapply_fun(gpqm, o, p, s, MoreArgs = list(precip, pr.threshold, theta))
  } else if (method == "mva") {
    mapply_fun(mva, o, p, s) 
  } else if (method == "variance") {
    mapply_fun(variance, o, p, s, MoreArgs = list(precip))
  } else if (method == "loci") {
    mapply_fun(loci, o, p, s, MoreArgs = list(precip, pr.threshold))
  } else if (method == "ptr") {
    mapply_fun(ptr, o, p, s, MoreArgs = list(precip))
  } else if (method == "dqm") {
    mapply_fun(dqm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles, detrend))
  } else if (method == "qdm") {
    mapply_fun(qdm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles))
  } else if (method == "isimip3") {
    mapply_fun(isimip3, o, p, s, MoreArgs = isimip3.args) #this method is in a separate file
  }
  #INCLUIR AQUI METODOS NUEVOS
}

adjustPrecipFreq = function(obs, pred, threshold){
  o = obs[!is.na(obs)]
  p = pred[!is.na(pred)]
  # Number of dry days in 'o' 
  nPo = sum(as.double(o < threshold))
  # Number of dry days that must be in 'p' to equal precip frequency in 'o'
  nPp = ceiling(length(p) * nPo / length(o))
  # Index and values of ordered 'p'
  ix = sort(p, decreasing = FALSE, index.return = TRUE)$ix
  Ps = sort(p, decreasing = FALSE)
  Pth = max(Ps[nPp:(nPp + 1)], na.rm = TRUE) # in case nPp == length(Ps)
  # Themeßl (Themessl) modification (simulating rain for model dry days) 
  inddrzl = which(Ps[(nPp + 1):length(Ps)] < threshold)
  if (length(inddrzl) > 0) { 
    Os = sort(o, decreasing = FALSE, na.last = NA)
    indO = ceiling(length(Os) * (nPp + max(inddrzl))/length(Ps))
    auxOs = Os[(nPo + 1):indO]
    if (length(unique(auxOs)) > 6) {
      # simulate precip for 'p' with a gamma adjusted in 'o' for values between
      auxGamma = fitdistr(auxOs, "gamma")
      Ps[(nPp + 1):(nPp + max(inddrzl))] = rgamma(length(inddrzl), auxGamma$estimate[1], rate = auxGamma$estimate[2])
    } else {
      Ps[(nPp + 1):(nPp + max(inddrzl))] = mean(auxOs)
    }
    # order 'Ps' after simulation
    Ps = sort(Ps, decreasing = FALSE, na.last = NA)
  }
  # Make 0-s
  if (nPo > 0) {
    ind = min(nPp, length(p))
    Ps[1:ind] = 0
  }
  p[ix] = Ps
  pred[!is.na(pred)] = p
  return(list("nP" = c(nPo,nPp), "Pth" = Pth, "p" = pred)) 
}
#end


delta = function(o, p, s){
  corrected = o + (mean(s) - mean(p))
  return(corrected)
}

scaling = function(o, p, s, scaling.type){
  if (scaling.type == "additive") {
    s - mean(p) + mean(o, na.rm = TRUE)
  } else if (scaling.type == "multiplicative") {
    (s/mean(p)) * mean(o, na.rm = TRUE)
  }
}
pqm = function(o, p, s, fitdistr.args, precip, pr.threshold){
  dfdistr = cbind("df" = c( "beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t", "weibull"),
                  "p" = c("pbeta", "pcauchy", "pchisq", "pexp", "pf", "pgamma", "pegeom", "plnorm", "plnorm", "plogis", "pnbinom", "pnorm", "ppois", "pt", "pweibull"),
                  "q" = c("qbeta", "qcauchy", "qchisq", "qexp", "qf", "qgamma", "qegeom", "qlnorm", "qlnorm", "qlogis", "qnbinom", "qnorm", "qpois", "qt", "qweibull"))
  fitdistr.args = fitdistr.args[which(names(fitdistr.args) != "x")]
  statsfunp = unname(dfdistr[which(dfdistr[,"df"] == fitdistr.args$densfun), "p"])
  statsfunq = unname(dfdistr[which(dfdistr[,"df"] == fitdistr.args$densfun), "q"])
  run = TRUE
  ind.o = 1:length(o)
  ind.p = 1:length(p)
  rain = 1:length(s)
  if (precip) {
    threshold = pr.threshold
    if (any(!is.na(o))) {
      params =  adjustPrecipFreq(o, p, threshold)
      p = params$p
      nP = params$nP
      Pth = params$Pth
    } else {
      nP = NULL
    }
    if (is.null(nP)) {
      run = FALSE
      s = rep(NA, length(s))
    } else if (nP[1] < length(o)) {
      ind.o = which(o > threshold & !is.na(o))
      ind.p = which(p > 0 & !is.na(p))
      rain = which(s > Pth & !is.na(s))
      noRain = which(s <= Pth & !is.na(s))
    } else {
      run = FALSE
      warning("For the window step selected, location without rainfall above the threshold.\n no bias correction applied in location.")
    } 
  }
  if (all(is.na(o[ind.o]))) {
    run = FALSE
    s = rep(NA, length(s))
  }
  if (run) {
    fitdistr.args.o = c("x" = list(o[ind.o]), fitdistr.args)
    fitdistr.args.p = c("x" = list(p[ind.p]), fitdistr.args)
    obsGamma = tryCatch({do.call("fitdistr", fitdistr.args.o)}, error = function(err){NULL})
    prdGamma = tryCatch({do.call("fitdistr", fitdistr.args.p)}, error = function(err){NULL})
    if (!is.null(prdGamma) & !is.null(obsGamma)) {
      statsfun.args = c(list(s[rain]), as.list(prdGamma$estimate))
      auxF = do.call(statsfunp, statsfun.args)
      statsfun.args = c(list(auxF), as.list(obsGamma$estimate))
      s[rain] = do.call(statsfunq, statsfun.args)
      if (precip) s[noRain] = 0
    } else {
      warning("Fitting error for location and selected 'densfun'.")
    }
  }   
  return(s)      
}   
#end

eqm = function(o, p, s, precip, pr.threshold, n.quantiles, extrapolation){
  if (precip == TRUE) {
    threshold = pr.threshold
    if (any(!is.na(o))) {
      params =  adjustPrecipFreq(o, p, threshold)
      p = params$p
      nP = params$nP
      Pth = params$Pth
    } else {
      nP = NULL
    }
    smap = rep(NA, length(s))
    if (any(!is.na(p)) & any(!is.na(o))) {
      if (length(which(p > Pth)) > 0) { 
        noRain = which(s <= Pth & !is.na(s))
        rain = which(s > Pth & !is.na(s))
        drizzle = which(s > Pth  & s  <= min(p[which(p > Pth)], na.rm = TRUE) & !is.na(s))
        if (length(rain) > 0) {
          eFrc = tryCatch({ecdf(s[rain])}, error = function(err) {stop("There are not precipitation days in newdata for the step length selected in one or more locations. Try to enlarge the window step")})
          if (is.null(n.quantiles)) n.quantiles = length(p)
          bins = n.quantiles
          qo = quantile(o[which(o > threshold & !is.na(o))], prob = seq(1/bins,1 - 1/bins,1/bins), na.rm = T)
          qp = quantile(p[which(p > Pth)], prob = seq(1/bins,1 - 1/bins,1/bins), na.rm = T)
          p2o = tryCatch({approxfun(qp, qo, method = "linear")}, error = function(err) {NA})
          smap = s
          smap[rain] = if (suppressWarnings(!is.na(p2o))) {
            p2o(s[rain])
          }else{
            s[rain] = NA
          }
          # Linear extrapolation was discarded due to lack of robustness 
          if (extrapolation == "constant") {
            smap[rain][which(s[rain] > max(qp, na.rm = TRUE))] = s[rain][which(s[rain] > max(qp, na.rm = TRUE))] + (qo[length(qo)] - qp[length(qo)])
            smap[rain][which(s[rain] < min(qp, na.rm = TRUE))] = s[rain][which(s[rain] < min(qp, na.rm = TRUE))] + (qo[1] - qp[1]) 
          } else {
            smap[rain][which(s[rain] > max(qp, na.rm = TRUE))] = qo[length(qo)]
            smap[rain][which(s[rain] < min(qp, na.rm = TRUE))] = qo[1]
          }
        }else{
          smap = rep(0, length(s))
          warning("There are not precipitation days in newdata for the step length selected in one or more locations. Consider the possibility of enlarging the window step")
        }
        if (length(drizzle) > 0) {
          smap[drizzle] = quantile(s[which(s > min(p[which(p > Pth)], na.rm = TRUE) & !is.na(s))], probs = eFrc(s[drizzle]), na.rm = TRUE, type = 4)
        }
        smap[noRain] = 0
      } else { ## For dry series
        smap = s
        warning('No rainy days in the prediction. Bias correction is not applied') 
      }
    }
  } else {
    if (all(is.na(o))) {
      smap = rep(NA, length(s))
    } else if (all(is.na(p))) {
      smap = rep(NA, length(s))
    }else if (any(!is.na(p)) & any(!is.na(o))) {
      if (is.null(n.quantiles)) n.quantiles = length(p)
      bins = n.quantiles
      qo = quantile(o, prob = seq(1/bins,1 - 1/bins,1/bins), na.rm = TRUE)
      qp = quantile(p, prob = seq(1/bins,1 - 1/bins,1/bins), na.rm = TRUE)
      p2o = approxfun(qp, qo, method = "linear")
      smap = p2o(s)
      if (extrapolation == "constant") {
        smap[which(s > max(qp, na.rm = TRUE))] = s[which(s > max(qp, na.rm = TRUE))] + (qo[length(qo)] - qp[length(qo)])
        smap[which(s < min(qp, na.rm = TRUE))] = s[which(s < min(qp, na.rm = TRUE))] + (qo[1] - qp[1]) 
      } else {
        smap[which(s > max(qp, na.rm = TRUE))] = qo[length(qo)]
        smap[which(s < min(qp, na.rm = TRUE))] = qo[1]
      }
    } 
  }
  return(smap)
}
#end
gpqm = function(o, p, s, precip, pr.threshold, theta) { 
  if (precip == FALSE) {
    # stop("method gpqm is only applied to precipitation data")
    # For temperature, lower (values below theta.low) and upper (values above theta) tails of the distribution are fitted with GPD.
    if (all(is.na(o)) | all(is.na(p))) {
      s = rep(NA, length(s))
    } else{
      theta.low = theta[2]
      theta = theta[1]
      ind = which(!is.na(o))
      indnormal = ind[which((o[ind] < quantile(o[ind], theta)) & (o[ind] > quantile(o[ind], theta.low)))]
      indparetoUp = ind[which(o[ind] >= quantile(o[ind], theta))]
      indparetoLow = ind[which(o[ind] <= quantile(o[ind], theta.low))]
      indp = which(!is.na(p))
      indnormalp = indp[which((p[indp] < quantile(p[indp],theta)) & (p[indp] > quantile(p[indp], theta.low)))]
      indparetopUp = indp[which(p[indp] >= quantile(p[indp], theta))]
      indparetopLow = indp[which(p[indp] <= quantile(p[indp], theta.low))]
      inds = which(!is.na(s))
      indnormalsim = inds[which((s[inds] < quantile(p[indp], theta)) & (s[inds] > quantile(p[indp], theta.low)))]
      indparetosimUp = inds[which(s[inds] >= quantile(p[indp], theta))]
      indparetosimLow = inds[which(s[inds] <= quantile(p[indp], theta.low))]
      # normal distribution
      obsGQM = fitdistr(o[indnormal],"normal")
      prdGQM = fitdistr(p[indnormalp], "normal")
      auxF = pnorm(s[indnormalsim], prdGQM$estimate[1], prdGQM$estimate[2])
      s[indnormalsim] = qnorm(auxF, obsGQM$estimate[1], obsGQM$estimate[2])
      # upper tail
      obsGQM2Up = fpot(o[indparetoUp], quantile(o[ind], theta), "gpd", std.err = FALSE)
      prdGQM2Up = fpot(p[indparetopUp], quantile(p[indp], theta), "gpd", std.err = FALSE)
      auxF2Up = pgpd(s[indparetosimUp], loc = prdGQM2Up$threshold, scale = prdGQM2Up$estimate[1], shape = prdGQM2Up$estimate[2])
      s[indparetosimUp[which(auxF2Up < 1  & auxF2Up > 0)]] = qgpd(auxF2Up[which(auxF2Up < 1 & auxF2Up > 0)], loc = obsGQM2Up$threshold, scale = obsGQM2Up$estimate[1], shape = obsGQM2Up$estimate[2])
      s[indparetosimUp[which(auxF2Up == 1)]] = max(o[indparetoUp], na.rm = TRUE)
      s[indparetosimUp[which(auxF2Up == 0)]] = min(o[indparetoUp], na.rm = TRUE)
      # lower tail
      obsGQM2Low = fpot(-o[indparetoLow], -quantile(o[ind], theta.low), "gpd", std.err = FALSE)
      prdGQM2Low = fpot(-p[indparetopLow], -quantile(p[indp], theta.low), "gpd", std.err = FALSE)
      auxF2Low = pgpd(-s[indparetosimLow], loc = prdGQM2Low$threshold, scale = prdGQM2Low$estimate[1], shape = prdGQM2Low$estimate[2])
      s[indparetosimLow[which(auxF2Low < 1 & auxF2Low > 0)]] = -qgpd(auxF2Low[which(auxF2Low < 1 & auxF2Low > 0)], loc = obsGQM2Low$threshold, scale = obsGQM2Low$estimate[1], shape = obsGQM2Low$estimate[2])
      s[indparetosimLow[which(auxF2Low == 1)]] = max(o[indparetoLow], na.rm = TRUE)
      s[indparetosimLow[which(auxF2Low == 0)]] = min(o[indparetoLow], na.rm = TRUE)
    }  
  } else {
    theta = theta[1]
    threshold = pr.threshold
    if (any(!is.na(o)) & any(!is.na(p))) {
      params =  adjustPrecipFreq(o, p, threshold)
      p = params$p
      nP = params$nP
      Pth = params$Pth
    } else {
      nP = NULL
    }
    if (is.null(nP)) {
      s = rep(NA, length(s))
    } else if (nP[1] < length(o)) {
      ind = which(o > threshold & !is.na(o))
      indgamma = ind[which(o[ind] < quantile(o[ind], theta))]
      indpareto = ind[which(o[ind] >= quantile(o[ind], theta))]
      indp = which(p > 0 & !is.na(p))
      indgammap = indp[which(p[indp] < quantile(p[indp],theta))]
      indparetop = indp[which(p[indp] >= quantile(p[indp], theta))]
      rain = which(s > Pth & !is.na(s))
      noRain = which(s <= Pth & !is.na(s))
      indgammasim = rain[which(s[rain] < quantile(p[indp], theta))]
      indparetosim = rain[which(s[rain] >= quantile(p[indp], theta))]
      # gamma distribution
      if(length(indgamma)>1 & length(indgammap)>1 & length(indgammasim)>1){
        obsGQM = tryCatch(fitdistr(o[indgamma],"gamma"), error = function(err){NULL})
        prdGQM = tryCatch(fitdistr(p[indgammap], "gamma"), error = function(err){NULL})
        if (!is.null(prdGQM) & !is.null(obsGQM)) {
          auxF = pgamma(s[indgammasim], prdGQM$estimate[1], rate = prdGQM$estimate[2])
          s[indgammasim] = qgamma(auxF, obsGQM$estimate[1], rate = obsGQM$estimate[2])
        } else {
          warning("Fitting error for location and selected 'densfun'.")
          s[indgammasim] = NA
        }
      } else{
        s[indgammasim] =0
      }
      # upper tail
      if(any(o[indpareto] > quantile(o[ind], theta)) & any(p[indparetop] > quantile(p[indp], theta))){
        obsGQM2 = fpot(o[indpareto], quantile(o[ind], theta), "gpd", std.err = FALSE)
        prdGQM2 = fpot(p[indparetop], quantile(p[indp], theta), "gpd", std.err = FALSE)
        auxF2 = pgpd(s[indparetosim], loc = 0, scale = prdGQM2$estimate[1], shape = prdGQM2$estimate[2])
        s[indparetosim[which(auxF2 < 1  & auxF2 > 0)]] = qgpd(auxF2[which(auxF2 < 1  & auxF2 > 0)], loc = 0, scale = obsGQM2$estimate[1], shape = obsGQM2$estimate[2])
        s[indparetosim[which(auxF2 == 1)]] = max(o[indpareto], na.rm = TRUE)
        s[indparetosim[which(auxF2 == 0)]] = min(o[indpareto], na.rm = TRUE)
      } else {
        s[indparetosim] = 0
      }
      # dry days
      s[noRain] = 0
      # inf to NA
      s[is.infinite(s)] = NA
      s[s>1e3] = NA
    } else {
      warning("There is at least one location without rainfall above the threshold.\n In this (these) location(s) none bias correction has been applied.")
    }  
  }
  return(s)
}


mva = function(o, p, s){
  corrected = (s - mean(p, na.rm = TRUE)) + sd(o, na.rm = TRUE)/sd(p, na.rm = TRUE) + mean(o, na.rm = TRUE)
  return(corrected)
}

#' @title Variance scaling of temperature
#' @description Implementation of Variance scaling of temperature method for bias correction
#' @param o A vector (e.g. station data) containing the observed climate data for the training period
#' @param p A vector containing the simulated climate by the model for the training period. 
#' @param s A vector containing the simulated climate for the variable used in \code{p}, but considering the test period.
#' @param precip Logical indicating if o, p, s is temperature data.
#' @keywords internal
#' @author B. Szabo-Takacs

variance = function(o, p, s, precip) {
  if (precip == FALSE) {
    t_dif = mean(o, na.rm = TRUE) - mean(p, na.rm = TRUE)
    t1 = p + rep(t_dif, length(p), 1)
    t1_m = mean(t1,na.rm = TRUE) 
    t2 = t1 - rep(t1_m,length(t1),1)
    o_s = sd(o,na.rm = TRUE) 
    t2_s = sd(t2,na.rm = TRUE) 
    tsig = o_s/t2_s
    t1 = t1_m = t2 = o_s = t2_s = NULL
    t1 = s + rep(t_dif, length(s), 1)
    t1_m = mean(t1, na.rm = TRUE)
    t2 = t1 - rep(t1_m, length(t1), 1)
    t3 = t2 * rep(tsig, length(t2), 1)
    tC = t3 + rep(t1_m, length(t3), 1)
    t1 = t1_m = t2 = t3 = NULL
    return(tC)
  } else {
    stop("method variance is only applied to temperature data")
  }
}


#' @title Local intensity scaling of precipitation
#' @description Implementation of Local intensity scaling of precipitation method for bias correction based on Vincent Moron's local_scaling function in weaclim toolbox in Matlab
#' @param o A vector (e.g. station data) containing the observed climate data for the training period
#' @param p A vector containing the simulated climate by the model for the training or test period. 
#' @param s A vector containing the simulated climate for the variable used in \code{p}, but considering the test period.
#' @param precip Logical indicating if o, p, s is precipitation data.
#' @param pr.threshold The minimum value that is considered as a non-zero precipitation. Ignored when 
#' \code{precip = FALSE}. See details in function \code{biasCorrection}.
#' @author B. Szabo-Takacs

loci = function(o, p, s, precip, pr.threshold){
  if (precip == FALSE) { 
    stop("method loci is only applied to precipitation data")
  } else {
    threshold = pr.threshold
    l = length(which(o > threshold))
    gcmr = rev(sort(p))
    gcmrs = rev(sort(s))
    Pgcm = gcmr[l + 1]
    Pgcms = gcmrs[l + 1]
    mobs = mean(o[which(o > threshold)], na.rm = TRUE)
    mgcm = mean(p[which(p > Pgcm)], na.rm = TRUE)
    scaling = (mobs - threshold) / (mgcm - Pgcm)
    GCM = (scaling*(s - Pgcms)) + threshold
    GCM[which(GCM < threshold)] = 0
  }
  return(GCM)
}


#' @title Power transformation of precipitation
#' @description Implementation of Power transformation of precipitation method for bias correction 
#' @param o A vector (e.g. station data) containing the observed climate data for the training period
#' @param p A vector containing the simulated climate by the model for the training period. 
#' @param s A vector containing the simulated climate for the variable used in \code{p}, but considering the test period.
#' @param precip Logical indicating if o, p, s is precipitation data.
#' @importFrom stats uniroot
#' @keywords internal
#' @author S. Herrera and B. Szabo-Takacs

ptr = function(o, p, s, precip) {
  if (precip == FALSE) { 
    stop("method power transformation is only applied to precipitation data")
  } else {
    b = NaN
    cvO = sd(o,na.rm = TRUE) / mean(o, na.rm = TRUE)
    if (!is.na(cvO)) {
      bi = try(uniroot(function(x)
        varCoeficient(x, abs(p), cvO), c(0,1), extendInt = "yes"), silent = TRUE)
      if ("try-error" %in% class(bi)) {  # an error occurred
        b = NA
      } else {
        b = bi$root
      }
    }
    p[p < 0] =  0
    s[s < 0] =  0
    aux_c = p^rep(b,length(p),1)
    aux = s^rep(b,length(s),1)
    prC = aux * rep((mean(o, na.rm = TRUE) / mean(aux_c, na.rm = TRUE)), length(s), 1)
    aux = aux_c = NULL
  }
  return(prC)
}


#' @title VarCoeficient
#' @description preprocess to power transformation of precipitation
#' @param delta A vector of power parameter
#' @param data A vector containing the simulated climate by the model for training period
#' @param cv A vector containing coefficient of variation of observed climate data
#' @keywords internal
#' @author S. Herrera and B. Szabo-Takacs

varCoeficient = function(delta,data,cv){
  y = cv - sd((data^delta), na.rm = TRUE)/mean((data^delta), na.rm = TRUE)
  return(y)
}


#' @title Concatenate members
#' @description Concatenate members as a single time series for using their joint distribution in bias correction
#' @param grid Input (multimember) grid
#' @return A grid without members, with additional attributes to retrieve the original structure after bias correction
#' @seealso \code{\link{recoverMemberDim}}, for recovering the original structure after bias correction.
#' @keywords internal
#' @importFrom transformeR subsetGrid redim getShape bindGrid
#' @author J Bedia

flatMemberDim = function(grid, station) {
  grid = redim(grid, member = TRUE, loc = station)     
  n.mem.join = getShape(grid, "member")
  n.time.join = getShape(grid, "time")
  aux.ltime = lapply(1:n.mem.join, function(x) {
    subsetGrid(grid, members = x)
  })
  out = do.call("bindGrid", c(aux.ltime, dimension = "time"))
  attr(out, "orig.mem.shape") = n.mem.join
  attr(out, "orig.time.shape") = n.time.join
  return(out)
}

#' @title Recover member multimember structure
#' @description Recover member multimember structure after application of \code{\link{flatMemberDim}}
#' @param plain.grid A \dQuote{flattened} grid used as predictor in \code{biasCorrection} (the 'pred' object)
#' @param bc.grid The bias-corrected output (the 'bc' object), still without its member structure 
#' @param newdata The 'newdata' object, needed to recover relevant metadata (i.e. initialization dates and member names)
#' @return A (bias-corrected) multimember grid
#' @keywords internal
#' @importFrom transformeR subsetDimension bindGrid
#' @seealso \code{\link{flatMemberDim}}, for \dQuote{flattening} the member structure
#' @author J Bedia

recoverMemberDim = function(plain.grid, bc.grid, newdata) {
  bc = bc.grid
  nmem = attr(plain.grid, "orig.mem.shape")
  ntimes = attr(plain.grid, "orig.time.shape")
  # bc$Dates = lapply(bc$Dates, "rep", nmem)
  aux.list = lapply(1:nmem, function(m) {
    aux = subsetDimension(grid = bc, dimension = "time", indices = seq(m, nmem * ntimes, nmem))
    aux$InitializationDates = newdata$InitializationDates[[m]]
    aux$Members = newdata$Members[[m]]
    return(aux)
  })
  do.call("bindGrid", c(aux.list, dimension = "member"))
}

#' @title Detrended quantile matching 
#' @description Detrended quantile matching with delta-method extrapolation 
#' @param o A vector (e.g. station data) containing the observed climate data for the training period
#' @param p A vector containing the simulated climate by the model for the training period. 
#' @param s A vector containing the simulated climate for the variable used in \code{p}, but considering the test period.
#' @param precip Logical indicating if o, p, s is precipitation data.
#' @param pr.threshold Integer. The minimum value that is considered as a non-zero precipitation. 
#' @param detrend logical. Detrend data prior to bias correction? Default. TRUE.
#' @param n.quantiles  Integer. Maximum number of quantiles to estimate. Default: same as data length.
#' @details DQM method developed by A. Canon, from \url{https://github.com/pacificclimate/ClimDown}, \url{https://cran.r-project.org/web/packages/ClimDown/}.
#'
#' @references Cannon, A.J., S.R. Sobie, and T.Q. Murdock (2015) Bias Correction of GCM Precipitation by Quantile Mapping: How Well Do Methods Preserve Changes in Quantiles and Extremes?. J. Climate, 28, 6938-6959, \url{https://doi.org/10.1175/JCLI-D-14-00754.1}
#' @keywords internal
#' @author A. Cannon (acannon@@uvic.ca), A. Casanueva

dqm = function(o, p, s, precip, pr.threshold, n.quantiles, detrend=TRUE){
  
  if (all(is.na(o)) | all(is.na(p)) | all(is.na(s))) {
    return(yout=rep(NA, length(s)))
    
  } else{
    
    if(precip){
      # For ratio data, treat exact zeros as left censored values less than pr.threshold
      epsilon = .Machine$double.eps
      o[o < pr.threshold & !is.na(o)] = runif(sum(o < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
      p[p < pr.threshold & !is.na(p)] = runif(sum(p < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
      s[s < pr.threshold & !is.na(s)] = runif(sum(s < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
    }
    
    o.mn = mean(o, na.rm=T)
    p.mn = mean(p, na.rm=T)
    if(precip){
      s = s/p.mn*o.mn
    } else{
      s = s-p.mn+o.mn
    }
    
    if(detrend){
      s.mn = lm.fit(cbind(1, seq_along(s)), s)$fitted
    } else{
      s.mn = o.mn
    }
    if(is.null(n.quantiles)) n.quantiles = max(length(o), length(p))
    tau = c(0, (1:n.quantiles)/(n.quantiles+1), 1)
    if(precip & any(o < sqrt(.Machine$double.eps), na.rm=TRUE)){
      x = quantile(p/p.mn, tau, na.rm=T)
      y = quantile(o/o.mn, tau, na.rm=T)
      yout = approx(x, y, xout=s/s.mn, rule=2:1)$y # if rule = 1, NAs are returned outside the training interval; if rule= 2, the value at the closest data extreme is used. rule = 2:1, if the left and right side extrapolation should differ.
      extrap = is.na(yout)
      yout[extrap] = max(o/o.mn, na.rm=T)*((s/s.mn)[extrap]/max(p/p.mn, na.rm=T)) # extrapolation on the upper tail
      yout = yout*s.mn
      #yout.h = approx(x, y, xout=p/p.mn, rule=1)$y*o.mn
    } else if(precip & !any(o < sqrt(.Machine$double.eps), na.rm=TRUE)){
      x = quantile(p/p.mn, tau, na.rm=T)
      y = quantile(o/o.mn, tau, na.rm=T)
      yout = approx(x, y, xout=s/s.mn, rule=1)$y
      extrap.lower = is.na(yout) & ((s/s.mn) < min(p/p.mn, na.rm=T))
      extrap.upper = is.na(yout) & ((s/s.mn) > max(p/p.mn, na.rm=T))
      yout[extrap.lower] = min(o/o.mn, na.rm=T)*((s/s.mn)[extrap.lower]/
                                                   min(p/p.mn, na.rm=T))
      yout[extrap.upper] = max(o/o.mn, na.rm=T)*((s/s.mn)[extrap.upper]/
                                                   max(p/p.mn, na.rm=T))
      yout = yout*s.mn
      #yout.h = approx(x, y, xout=p/p.mn, rule=1)$y*o.mn
    } else{
      x = quantile(p-p.mn, tau, na.rm=T)
      y = quantile(o-o.mn, tau, na.rm=T)
      yout = approx(x, y, xout=s-s.mn, rule=1)$y
      extrap.lower = is.na(yout) & ((s-s.mn) < min(p-p.mn, na.rm=T))
      extrap.upper = is.na(yout) & ((s-s.mn) > max(p-p.mn, na.rm=T))
      yout[extrap.lower] = min(o-o.mn) + ((s-s.mn)[extrap.lower]-
                                            min(p-p.mn, na.rm=T))
      yout[extrap.upper] = max(o-o.mn) + ((s-s.mn)[extrap.upper]-
                                            max(p-p.mn, na.rm=T))
      yout = yout+s.mn
      #yout.h = approx(x, y, xout=p-p.mn, rule=1)$y+o.mn
    }
    if(precip){
      yout[which(yout < sqrt(.Machine$double.eps))] = 0
      #yout.h[which(yout.h < sqrt(.Machine$double.eps))] = 0
    }
    return(yout)
  }
}


#' @title Quantile delta mapping
#' @description Quantile delta mapping 
#' @param o A vector (e.g. station data) containing the observed climate data for the training period
#' @param p A vector containing the simulated climate by the model for the training period. 
#' @param s A vector containing the simulated climate for the variable used in \code{p}, but considering the test period.
#' @param precip Logical indicating if o, p, s is precipitation data.
#' @param pr.threshold Integer. The minimum value that is considered as a non-zero precipitation. 
#' \code{precip = FALSE}.
#' @param jitter.factor Integer. Jittering to accomodate ties. Default: 0.01.
#' @param n.quantiles  Integer. Maximum number of quantiles to estimate. Default: same as data length.
#' @details QDM method developed by A. Canon, from \url{https://github.com/pacificclimate/ClimDown}, \url{https://cran.r-project.org/web/packages/ClimDown/}.
#' 
#' @references Cannon, A.J., S.R. Sobie, and T.Q. Murdock (2015) Bias Correction of GCM Precipitation by Quantile Mapping: How Well Do Methods Preserve Changes in Quantiles and Extremes?. J. Climate, 28, 6938-6959, \url{https://doi.org/10.1175/JCLI-D-14-00754.1}
#' @keywords internal
#' @author A. Cannon (acannon@@uvic.ca), A. Casanueva

qdm = function(o, p, s, precip, pr.threshold, n.quantiles, jitter.factor=0.01){
  
  # tau.s = F.s(x.s)
  # delta = x.s {/,-} F.p^-1(tau.s)
  # yout = F.o^-1(tau.s) {*,+} delta
  
  if (all(is.na(o)) | all(is.na(p)) | all(is.na(s))) {
    return(yout=rep(NA, length(s)))
  } else{
    
    # Apply a small amount of jitter to accomodate ties due to limited measurement precision
    o = jitter(o, jitter.factor)
    p = jitter(p, jitter.factor)
    s = jitter(s, jitter.factor)
    
    # For ratio data, treat exact zeros as left censored values less than pr.threshold
    if(precip){
      epsilon = .Machine$double.eps
      o[o < pr.threshold & !is.na(o)] = runif(sum(o < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
      p[p < pr.threshold & !is.na(p)] = runif(sum(p < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
      s[s < pr.threshold & !is.na(s)] = runif(sum(s < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
    }
    
    # Calculate empirical quantiles using Weibull plotting position
    n = max(length(o), length(p), length(s))
    if(is.null(n.quantiles)) n.quantiles = n
    tau = seq(1/(n+1), n/(n+1), length=n.quantiles)
    quant.o = quantile(o, tau, type=6, na.rm=TRUE)
    quant.p = quantile(p, tau, type=6, na.rm=TRUE)
    quant.s = quantile(s, tau, type=6, na.rm=TRUE)
    
    # Apply QDM bias correction
    tau.s = approx(quant.s, tau, s, rule=2)$y    
    if(precip){
      delta = s/approx(tau, quant.p, tau.s, rule=2)$y # if rule= 2, the value at the closest data extreme is used
      yout = approx(tau, quant.o, tau.s, rule=2)$y*delta
    } else{
      delta = s - approx(tau, quant.p, tau.s, rule=2)$y
      yout = approx(tau, quant.o, tau.s, rule=2)$y + delta
    }
    #yout.h = approx(quant.p, quant.o, p, rule=2)$y
    
    # For precip data, set values less than threshold to zero
    if(precip){
      #yout.h[yout.h < pr.threshold] = 0
      yout[yout < pr.threshold] = 0
    }
    
    return(yout)
  }
}


