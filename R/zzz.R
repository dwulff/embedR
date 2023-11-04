pacmap_module <- NULL

.onLoad <- function(libname, pkgname) {
  pacmap_module <<- reticulate::import("pacmap", delay_load = TRUE)
  }
