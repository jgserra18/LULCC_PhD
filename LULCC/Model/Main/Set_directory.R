

load_dir <- function() {
  dir_path <- '/home/jserra/google-drive/LULCC_PhD/'
  dir_isa <- 'G:/My Drive/LULCC_PhD/'
  dir_home <- 'G:/O meu disco/LULCC_PhD/LULCC/'
  
  store_db <- c(dir_path, dir_isa, dir_home)
  correct_path <- which(dir.exists(store_db)==TRUE)
  setwd(store_db[correct_path])
}


load_dir()
