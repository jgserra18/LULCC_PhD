set_proj_dir <- function(set_wd) {
  
  dir_path <- '/home/jserra/google-drive/LULCC_PhD/'
  dir_isa <- 'G:/My Drive/LULCC_PhD/'
  dir_home <- 'G:/O meu disco/LULCC_PhD/'
  
  store_db <- c(dir_path, dir_isa, dir_home)
  correct_path <- which(dir.exists(store_db)==TRUE)
  correct_path <- store_db[correct_path]
  
  if (missing(set_wd)==FALSE) {
    setwd(correct_path)
  }
  return(correct_path)
}

load_dir <- function(module) {
  
  if (module=='Main') {
    proj_dir <- set_proj_dir()
    setwd(proj_dir)
  }
  else {
    proj_dir <- set_proj_dir()
    correct_path <- list.files(path = proj_dir, pattern = module, full.names = TRUE)
    setwd(correct_path)
  }
}

load_dir('Main')


