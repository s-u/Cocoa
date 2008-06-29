# Foundation structures

`struct_names._NSRect` <- c("origin", "size")
`struct_names._NSPoint` <- c("x", "y")
`struct_names._NSSize` <- c("width", "length")
`struct_names._NSRange` <- c("location", "length")

NSMakePoint <- function(x, y) {
  v <- pairlist(x=x, y=y)
  attr(v, "struct.name") <- "_NSPoint"
  v
}

NSMakeSize <- function(width, length) {
  v <- pairlist(width=width, length=length)
  attr(v, "struct.name") <- "_NSSize"
  v
}

NSMakeRange <- function(location, length) {
  v <- pairlist(location=location, length=length)
  attr(v, "struct.name") <- "_NSRange"
  v
}

NSMakeRect <- function(x, y, width, length) {
  ax <- attr(x, "struct.name")
  if (isTRUE(attr(x, "struct.name") == "_NSPoint")) {
    origin <- x
    size <-
      if (isTRUE(attr(y, "struct.name") == "_NSSize")) y else NSMakeSize(width, length)
  } else {
    origin <- NSMakePoint(x, y)
    size <-
       if (isTRUE(attr(width, "struct.name") == "_NSSize")) width else NSMakeSize(width, length)
  }
  v <- pairlist(origin=origin, size=size)
  attr(v, "struct.name") <- "_NSRect"
  v 
}
