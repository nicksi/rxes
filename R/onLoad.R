.onLoad <- function(libname, pkgname) {
    #options(java.parameters = "-Xmx2000m")
    .jpackage(pkgname, lib.loc = libname)
}
