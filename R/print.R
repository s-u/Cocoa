print.ObjCclass <- function(x, ...)
{
	cat(paste("Obj-C class:",.External("printObjC",x),"\n"))
}

print.ObjCsel <- function(x, ...)
{
	cat(paste("selector:",.External("printObjC",x),"\n"))
}

print.ObjCid <- function(x, ...)
{
	cat(paste(.External("printObjC",x),"\n"))
}
