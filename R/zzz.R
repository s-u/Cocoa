.First.lib <- function(libname, pkgname) {
	library.dynam("cocoa", pkgname, libname)
	invisible(.C("InitializeCocoa"))
}
