initXESTools <- function() {
    xesTools <- .jnew("ru.ramax.processmining.XEStools")
    return (xesTools)
}

xesParseLog <- function(xesTools, filename) {
    # returned object success or failure
    result <- .jcall(xesTools, "Z", "parseLog", filename  )

    return (result)
}

xesTraceCount <- function(xesTools) {
    result <- .jcall(xesTools, "I", "getXLogSize")
    return (result)
}
