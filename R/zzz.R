.First.lib <- function(libname, pkgname) {
  library.dynam("Cocoa", pkgname, libname)
  invisible(.C("InitializeCocoa", PACKAGE="Cocoa"))
}
