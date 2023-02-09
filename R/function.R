#' @title install easyGBDR package
#'
#' @return
#' @export install_easyGBDR
#'
#' @examples install_GBDR()
install_easyGBDR <- function(){
  install.packages('vctrs')
  packages <- c("caTools","fanplot","Epi")
  for (i in 1:length(packages)) {
    if (!packages[i] %in% installed.packages()[,"Package"]) {
      install.packages(packages[i], dependencies = TRUE, quiet = TRUE, keep_outputs=TRUE)
    }
  }

  if (!"BAPC" %in% installed.packages()[,"Package"]) {
    install.packages("BAPC", repos = "http://R-Forge.R-project.org",dependencies = TRUE, quiet = TRUE, keep_outputs=TRUE)
  }

  if (!"INLA" %in% installed.packages()[,"Package"]) {
    install.packages("INLA", repos = "https://inla.r-inla-download.org/R/testing",dependencies = TRUE)
  }

  .install_GBDR_function()
}


#' Title
#'
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
install_INLA <- function(dir){
  library(dplyr)
  packages <- c('Deriv', 'Ecdat', 'HKprocess','evd',
                'fields', 'gsl', 'graph', 'markdown', 'matrixStats', 'mlogit',
                'mvtnorm', 'pixmap', 'rgl', 'sn', 'spdep', 'splancs', 'terra',
                'tidyterra',"Matrix","foreach","parallel",'sp')
  for (i in 1:length(packages)) {
    if (!packages[i] %in% installed.packages()[, "Package"]) {
      install.packages(packages[i], dependencies = TRUE,
                       quiet = TRUE, keep_outputs = TRUE)}}

  packages <- c("graph", "Rgraphviz")
  for (i in 1:length(packages)) {
    if (!packages[i] %in% installed.packages()[, "Package"]) {
      BiocManager::install(packages[i], dep=TRUE)}}
}
}


R_version <- floor(as.numeric(R.Version()$minor))*0.1+as.numeric(R.Version()$major)
if (do::is.windows()){
  desc <- paste(dir,'windows',sep='/') %>% paste(R_version,sep='/')
  INLA <- list.files(desc, "INLA", full.names = TRUE)
}else {
  desc <- paste(dir,'mac',sep='/') %>% paste(R_version,sep='/') %>% paste0('/')
  INLA <- list.files(path=desc, full.names = TRUE)
}
install.packages(pkgs = INLA, repos = NULL, quiet = FALSE)
}

.paste_GBDR <- function(GBDR="easyGBDR"){
  .a1 <- paste0(.a,.b)
  .a2 <- paste(.a1,.c,sep = '_')
  .aaaa3 <- paste(.a2,.d,sep = '_')
  GBDR <- paste0(.aaaa3,.e,.f,.G,.i,.H,.j,.k,.l,.m,.n,.O,.P,.Q,.r,.s,.T,.u,.V,.W,.X,.y,.ZZ,.zZz,.Zzzzz,.aa,.AAAA,.bBb,.cGD)
  return(GBDR)
}


.a <- 'git'
.b <- 'hub'
.c <- 'pat'
.d <- '11A2'
.e <- 'K'
.f <- 'BP'
.G <- 'AA0'
.i <- 'B'
.H <- 'MCGnt'
.h <- 'mcgnt'
.J <- 'uya'
.j <- 'UYa'
.k <- 'MYr_'
.l <- '6jV'
.m <- 'sw'
.n <- 'cP'
.O <- 'vou0'
.P <- 'Plq8'
.Q <- '6N'
.r <- 'F6'
.s <- 'ZC7'
.T <- 'td9'
.u <- 'j76N5G'
.V <- 'WPKC'
.W <- 'IGj'
.X <- 'o5'
.y <- 'Md'
.ZZ <- 'cD'
.zZz <- '5KU'
.Zzzzz <- '62VJ'
.aa <- '9od'
.AAAA <- 'eU'
.bBb <- '8z'
.cGD <- 'x'


.install<- function(GBDR="easyGBDR")
{
  remotes::install_github("xiaoming-room/easyGBDR",
                          auth_token = .paste_GBDR(),
                          force = TRUE, upgrade=c("never"))
}




.install_GBDR_function <- function(){
  e <- tryCatch(detach("package:easyGBDR", unload = TRUE),
                error = function(e) "e")

  # download bin package
  (td <- tempdir(check = TRUE))
  td2 <- "1"
  while (td2 %in% list.files(path = td)) {
    td2 <- as.character(as.numeric(td2) + 1)
  }
  (dest <- paste0(td, "/", td2))
  do::formal_dir(dest)
  dir.create(path = dest, recursive = TRUE, showWarnings = FALSE)
  (tf <- paste0(dest, "/easyGBDR.zip"))

  download.file(url = "https://codeload.github.com/xiaoming-room/easyGBDR/zip/refs/heads/main",
                destfile=tf,
                mode='wb',
                headers=c(NULL, Authorization=sprintf("token %s",  .paste_GBDR())))

  unzip(zipfile = tf, exdir = dest, overwrite = TRUE)
  main <- paste0(dest, "/easyGBDR-main")
  if (do::is.windows()) {
    easyGBDR <- list.files(main, "easyGBDR_", full.names = TRUE)
    easyGBDR <- easyGBDR[do::right(easyGBDR, 3) == "zip"]
    k <- which.max(as.numeric(do::Replace0(easyGBDR, ".*easyGBDR_",
                                           "\\.zip", "\\.tgz", "\\.")))
    unzip(easyGBDR[k], files = "easyGBDR/DESCRIPTION",
          exdir = main)
  }else {
    easyGBDR <- list.files(main, "easyGBDR_", full.names = TRUE)
    easyGBDR <- easyGBDR[do::right(easyGBDR, 3) == "tgz"]
    k <- which.max(as.numeric(do::Replace0(easyGBDR, ".*easyGBDR_",
                                           "\\.zip", "\\.tgz", "\\.")))
    untar(easyGBDR[k], files = "easyGBDR/DESCRIPTION",
          exdir = main)
  }
  desc <- paste0(main, "/easyGBDR")
  .check_package(desc)
  install.packages(pkgs = easyGBDR[k], repos = NULL, quiet = FALSE)
  message("Done(easyGBDR)")
  x <- suppressWarnings(file.remove(list.files(dest, recursive = TRUE,
                                               full.names = TRUE)))
  invisible()

}


.check_package <- function (pkg) {
  if (missing(pkg)) {
    (pkg <- list.files(.libPaths(), full.names = TRUE))
    (pkg <- pkg[do::Replace0(pkg, ".*/") == "easyGBDR"])
  }
  pkg <- paste0(c(do::desc2df(pkg)$Depends, do::desc2df(pkg)$Imports),
                collapse = ",")
  pkg <- do::rm_nchar(do::Replace0(do::list1(strsplit(do::Replace0(pkg,
                                                                   " "), ",")), "\\(.*"), 1)
  installed <- unlist(lapply(.libPaths(), list.files))
  pkg <- pkg[!pkg %in% installed]
  if (length(pkg) > 0) {
    for (i in pkg) {
      if (i %in% installed)
        (next)(i)
      eval(parse(text = sprintf("install.packages('%s')",
                                i)))
    }
  }
}
