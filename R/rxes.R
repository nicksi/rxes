xes.init <- function() {
    xesTools <- .jnew("ru.ramax.processmining.XEStools")
    return (xesTools)
}

xes.parseLog <- function(xesTools, filename) {
    # returned object success or failure
    result <- .jcall(xesTools, "Z", "parseLog", filename  )

    return (result)
}

xes.traceCount <- function(xesTools) {
    result <- .jcall(xesTools, "I", "getXLogSize")
    return (result)
}

xes.getTraceDurations <- function(xesTools) {
    res <- .jcall(xesTools, "Ljava/util/Map;", "getTraceDurations")
    kset <- .jcall(res, "Ljava/util/Set;", "keySet")
    kSetA <- .jcall(kset, "[Ljava/lang/Object;", "toArray")
    traces <- sapply(kSetA, function(item) { .jstrVal(item)})
    values <- .jcall(res, "Ljava/util/Collection;", "values")
    valuesA <- .jcall(values, "[Ljava/lang/Object;", "toArray")
    durations <- sapply(valuesA, function(item) {  .jcall(item, "J", "longValue") })
    df <- data.frame(trace = traces, duration = durations)
}
