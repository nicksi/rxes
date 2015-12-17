#' Initialize XES Log processing class
#'
#' \code{xes.init} returns pointer to the javaClass with empty log
#'
#' You should pass this reference to the other funcions, so we can use cache and avoiud log reload on any
#' operation
#'
#' @return reference to XESTools object
xes.init <- function() {
    xesTools <- .jnew("ru.ramax.processmining.XEStools")
    return (xesTools)
}

#' Loads and parses XES log in GZIP xml format
#'
#' loads and saves in the class object log specified by the filename
#'
#' @param xesTools reference to XEStools class object created via \code{\link{xes.init}}
#' @param filename string pointing to the event log file
#' @return boolean success flag
xes.parseLog <- function(xesTools, filename) {
    # returned object success or failure
    result <- .jcall(xesTools, "Z", "parseLog", filename  )

    return (result)
}

#' Return count of traces in the event log
#'
#' @param xesTools reference to XEStools class object created via \code{\link{xes.init}}
#' @return success flag
xes.traceCount <- function(xesTools) {
    result <- .jcall(xesTools, "I", "getXLogSize")
    return (result)
}

#' Return data.frame with the trace name (concept:name) and trace duration expressed in seconds
#'
#' @param xesTools reference to XEStools class object created via \code{\link{xes.init}}
#' @return data.frame with trace durations
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
