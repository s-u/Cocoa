# send a message to a target; the target can be Obj-C reference, class reference or class name (as string)
.MCall <- function(target, selector, ...)
{
	.External("ObjCsendMsg", target, selector, ..., PACKAGE="Cocoa")
}

# find class and return class reference or NULL if the class doesn't exist
.MClass <- function(className)
{
	.External("ObjCclass", as.character(className), PACKAGE="Cocoa")
}

# create selector reference
.MSelector <- function(selector)
{
	.External("ObjCselector", selector, PACKAGE="Cocoa")
}
