pacmap_module <- NULL

.onLoad <- function(libname, pkgname) {

  # set ignore classes
  options(embedR_mute_attr = c("dim","dimnames","class"))

  # load pacmap python module
  pacmap_module <<- reticulate::import("pacmap", delay_load = TRUE)

  }
