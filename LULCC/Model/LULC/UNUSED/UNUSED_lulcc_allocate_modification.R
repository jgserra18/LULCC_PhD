

# updates the function ".update.data.frame" from as.data.frame.R in the lulcc package ----------



getExpVarRasterList_ <- function(maps, t, obs) {
  index <- which(t == obs@t)
  for (i in 1:length(maps)) {
    s <- maps[[i]]
    n <- raster::nlayers(s)
    if (length(index) == 1){
      if (n == 1) {
        maps[[i]] <- s[[1]]
      } else if (index <= n) {
        maps[[i]] <- s[[index]] 
      } else {
        warning(paste0("< For dynamic variables: ", paste(names(maps[[i]]), collapse=", ")," >",
                       " Invalid timestep: no data for t = ", t, "\n",
                       " Using closest timestep t = ", obs@t[n]," instead."), call. = F)
        maps[[i]] <- s[[obs@t[n]]]
      }
    } else{
      warning(paste0("< For variables: ", paste(names(maps[[i]]), collapse=", ")," >", "\n",
                     "Invalid timestep: no variable for this time. Using setting t = 0 instead."), call. = F)
      maps[[i]] <- s[[1]]
    }
  }
  maps
}

as.data.frame.ExpVarRasterList_ <- function(x, row.names=NULL, optional=FALSE, cells, obs, t=0, ...) {
  
  ##maps <- c(.getExpVarRasterList(x@maps, t), lapply(x@calls, function(x) x@map))
  maps <- getExpVarRasterList_(x@maps, t, obs)
  df <- as.data.frame(matrix(data=NA, nrow=length(cells), ncol=length(maps)))
  for (i in 1:length(maps)) {
    df[,i] <- extract(maps[[i]], cells, ...)
  }
  
  ## s <- raster::stack(maps, ...) ## this will fail if map characteristics do not agree
  ## df <- as.data.frame(s[cells], row.names=row.names, optional=optional)
  names(df) <- x@names
  df
}




update_data_frame <- function(x, y, map, cells, t, ...) {
  
  ## hidden function to update a data.frame containing dynamic explanatory variables
  ##
  ## Args:
  ##   x: a data.frame
  ##   y: an ExpVarRasterList object
  ##   map: ???
  ##   cells: ???
  ##   t: the time for which dynamic explanatory variables should be updated
  ##
  ## Returns:
  ##   a data.frame
  
  ix <- t + 1
  nms <- names(x)
  if (length(y@maps) > 0) {
    dynamic.ix <- which(as.logical(sapply(y@maps, function(x) (nlayers(x) > 1)))) # find position of dynamic var
    if (length(dynamic.ix) > 0) {
      s <- raster::stack(lapply(y@maps[dynamic.ix], function(x) x[[ix]]))
      update.vals <- s[cells]
      
      for (i in 1:length(dynamic.ix)) {
        x[,dynamic.ix[i]] = update.vals[, i]
      }
     # x[, 1:ncol(x)] = sapply(1:ncol(x), function(y) as.numeric(newdata[,y])) # convert each col to numerice
    }
  }
  
  names(x) <- nms
  x
}



# updates "allocate.R" function from OrderedModele from the package lulcc -----------------------------------------------------------


#' @useDynLib lulcc
ordered_ <- function(tprob, map0.vals, demand, categories, order, stochastic) {
  
  map0.area <- .Call("total", map0.vals, categories)        ## initial condition
  diff <- demand - map0.area
  if (sum(abs(diff)) == 0) return(map0.vals)                
  map1.vals <- map0.vals
  
  for (i in 1:length(order)) {
    
    ix <- which(categories %in% order[i])
    cat <- categories[ix]
    n <- demand[ix] - length(which(map1.vals %in% cat))   ## number of cells to convert
    
    ## static demand
    if (n == 0) {
      ixx <- which(map0.vals %in% cat)                  ## index of all cells belonging to lu
      tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
    }
    
    ## increasing demand
    if (n > 0) {
      ixx <- which(!map1.vals %in% cat)                 ## index of all cells not currently belonging to lu
      p <- tprob[ixx,ix]                                ## suitability of all cells not currently belonging to lu (NB will include NAs)
      p.ix <- order(p, na.last=TRUE, decreasing=TRUE)   ## index of cells when arranged from high to low
      p <- p[p.ix]                                      ## suitability arranged from high to low
      p.ix <- p.ix[which(!is.na(p))]                    ## index with NAs removed
      p <- p[which(!is.na(p))]                          ## suitability with NAs removed
      ixx <- ixx[p.ix]                                  ## actual index of cells (as they appear in map1.vals)     
      #p.range <- range(p, na.rm=TRUE); print(p.range)                   
      #p <- (p - p.range[1]) / diff(p.range)             ## normalise suitability (0-1)
      
      ## repeat {
      ##     select.ix <- which(p >= runif(length(p)))     ## compare suitability to numbers drawn from random normal distribution
      ##     if (length(select.ix) >= abs(n)) break()      ## only exit loop if select.ix includes enough cells to meet demand
      ## }
      
      if (stochastic) {
        counter <- 0
        repeat {
          counter <- counter + 1
          select.ix <- which(p >= runif(length(p)))     ## compare suitability to numbers drawn from random normal distribution
          if (length(select.ix) >= abs(n) | counter > 1000) break()      ## only exit loop if select.ix includes enough cells to meet demand
        }
        
      } else {
        select.ix <- seq(1, length(p))
      }
      
      select.ix <- select.ix[1:n]                       ## select cells with the highest suitability
      ixx <- ixx[select.ix]                             ## index
      map1.vals[ixx] <- cat                             ## allocate change
      ixx <- which(map1.vals %in% cat)                  ## index of cells belonging to lu
      tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
    }
    
    ## decreasing demand
    if (n < 0) {
      ixx <- which(map0.vals %in% cat)                  ## index of all cells currently belonging to lu
      p <- tprob[ixx,ix]                                ## suitability of all cells currently belonging to lu (will include NAs)
      p.ix <- order(p, na.last=TRUE, decreasing=FALSE)   ## index of cells when arranged low to high
      p <- p[p.ix]                                      ## suitability arranged from low to high
      p.ix <- p.ix[which(!is.na(p))]                    ## index with NAs removed
      p <- p[which(!is.na(p))]                          ## suitability with NAs removed
      ixx <- ixx[p.ix]                                  ## actual index of cells (as they appear in map1.vals)  
      ## p.range <- range(p, na.rm=TRUE)                   
      ## p <- (p - p.range[1]) / diff(p.range)             ## normalise suitability
      if (stochastic) {
        counter <- 0
        repeat {
          counter <- counter + 1
          select.ix <- which(p < runif(length(p)))      ## compare suitability to numbers drawn from random normal distribution 
          if (length(select.ix) >= abs(n) | counter > 1000) break()      ## only exit loop if select.ix includes enough cells to meet demand
        }
      } else {
        select.ix <- seq(1, length(p))
      }
      
      select.ix <- select.ix[1:abs(n)]                       ## select cells with lowest suitability
      ixx <- ixx[select.ix]                             ## index 
      map1.vals[ixx] <- -1                              ## unclassified
      ixx <- which(map1.vals %in% cat)                  ## index of cells belonging to lu
      tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
    }
  }
  map1.vals
}

################################################################################

## helper functions



applyNeighbDecisionRules_ <- function(model, x, tprob) {
  if (!is.null(model@neighb) && !is.null(model@nb.rules)) {
    nb.allow <- allowNeighb(neighb=model@neighb, x=x, categories=model@categories, rules=model@nb.rules)
    tprob <- tprob * nb.allow
  } 
  tprob
}

applyDecisionRules_ <- function(model, x, hist, cd, tprob) {
  if (!is.null(model@rules)) {
    allow <- allow(x=x, hist=hist, categories=model@categories, cd=cd, rules=model@rules)
    tprob <- tprob * allow
  }
  tprob
}

#' @useDynLib lulcc
updatehist_ <- function(lu0, lu1, hist) {
  hist <- .Call("updatehist", lu0, lu1, hist)
}

maxtprob_ <- function(x) {    
  if (length(which(!is.na(x)) > 0)) {
    out <- max(x, na.rm=TRUE)
  } else {
    out <- NA
  }
}

#' @useDynLib lulcc
autoConvert_ <- function(x, prob, categories, mask=NULL, ...) {
  if (!is.null(mask) && length(x) != length(mask)) stop("mask must have same length as x")
  if (is.null(mask)) mask <- rep(1, length(x))
  ## TODO: change autoconvert function so mask is optional
  vals <- .Call("autoconvert", x, mask, prob, categories)
  ix <- which(!is.na(vals))
  vals <- vals[ix]
  out <- list(ix=ix, vals=vals)
}

################################################################################


allocate = function(model, stochastic=TRUE, ...) {
  # modify allocate from OrderedModel
  print('Allocation ....')
  
  map0 <- model@obs[[1]]
  cells <- which(!is.na(raster::getValues(map0)))
  map0.vals <- raster::extract(map0, cells)
  if (!is.null(model@hist)) hist.vals <- raster::extract(model@hist, cells) else NULL
  if (!is.null(model@mask)) mask.vals <- raster::extract(model@mask, cells) else NULL

  newdata <- as.data.frame.ExpVarRasterList_(x=model@ef, obs=model@obs, cells=cells)
  prob <- predict(object=model@models, newdata=newdata)
  maps <- raster::stack(map0)
  
  
  for (i in 1:(nrow(model@demand) - 1)) {

    d <- model@demand[(i+1),]
    
    ## 1. update land use suitability matrix if dynamic factors exis
    if (model@ef@dynamic==FALSE) {
      print('Static')
      newdata = as.data.frame(x=model@ef, obs=model@obs, cells=cells)
    }
    else  {
      print('Dynamic.')
      
      print('Creating new newdata')

      # update newdata
      newdata = update_data_frame(x=newdata, y=model@ef, map=map0, cells=cells, t = i-1)
      prob <- predict(object=model@models, newdata=newdata)
    }
    tprob <- prob
    
    
    ## 2. implement neighbourhood decision rules
     tprob <- applyNeighbDecisionRules_(model=model, x=map0, tprob=tprob)

   
    ## 3. implement other decision rules
    cd <- d - model@demand[i,] ## change direction
    tprob <- applyDecisionRules_(model=model, x=map0.vals, hist=hist.vals, cd=cd, tprob=tprob)

    
    ## 4. make automatic conversions if necessary
    auto <- autoConvert_(x=map0.vals, prob=tprob, categories=model@categories, mask=mask.vals)
    map0.vals[auto$ix] <- auto$vals
    tprob[auto$ix,] <- NA
    
    ## 5. allocation
    map1.vals <- do.call(ordered_, c(list(tprob=tprob, map0.vals=map0.vals, demand=d, categories=model@categories, order=model@order, stochastic=stochastic), model@params))
    map1 <- raster::raster(map0) 
    map1[cells] <- map1.vals
    maps <- raster::stack(maps, map1)
    
    ## 6. prepare model for next timestep
    if (i < nrow(model@demand)) {
      if (!is.null(model@hist)) hist.vals <- updatehist_(map0.vals, map1.vals, hist.vals)       
      map0 <- map1
      map0.vals <- map1.vals 
    }
  }

  model@output <- maps
  names(model@output) = paste0('LULC_',seq(1, nlayers(model@output)))
  return(model)     
  rm(list=c('cd','trob','auto','map0','map0.vals','map1.vals','map1','newdata','prob','maps'))
}

