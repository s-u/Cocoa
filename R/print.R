print.ObjCclass <- function(x, ...)
{
	cat(paste("Obj-C class:",.External("printObjC",x,PACKAGE="Cocoa"),"\n"))
}

print.ObjCsel <- function(x, ...)
{
	cat(paste("selector:",.External("printObjC",x,PACKAGE="Cocoa"),"\n"))
}

print.ObjCid <- function(x, ...)
{
	cat(paste(.External("printObjC",x,PACKAGE="Cocoa"),"\n"))
}
