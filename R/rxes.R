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
    data.frame(trace = traces, duration = durations)
}

#' Return list of all trace segments limited by event names with attributes
#'
#' @param xesTools reference to XEStools class object created via \code{\link{xes.init}}
#' @param startEvent name of first event in trace segment
#' @param endEvent name of last event in trace segment
#' @return data.frame with traces
xes.getFullSubTraceList <- function(xesTools, startEvent, endEvent) {
    emptyMap <- .jnew("java.util.HashMap")
    res <- .jcall(xesTools, "Ljava/util/List;", "getFullSubTraceList", .jcast(emptyMap, "java/util/Map"), startEvent, endEvent, use.true.class = T)
    xes.createDF(res)
}

#' Return list of all traces with attributes
#'
#' @param xesTools reference to XEStools class object created via \code{\link{xes.init}}
#' @return data.frame with traces
xes.getFullTraceList <- function(xesTools) {
    #java.util.List ru.ramax.processmining.XEStools.getFullTraceList(java.util.Map)
    #java.util.HashMap com.google.common.collect.Maps.newHashMap()
    emptyMap <- .jnew("java.util.HashMap")
    res <- .jcall(xesTools, "Ljava/util/List;", "getFullTraceList", .jcast(emptyMap, "java/util/Map"), use.true.class = T)
    xes.createDF(res)
}

#' Supporting function - transform map of traces into dataframe
#' @param res javaref object containing map of flatXTraces
#' @return data.frame with traces
xes.createDF <- function(res) {
    # we will create data frame with following columns: name, duration, startTime, endTime, eventCount, resource, role, eventRepetition
    valuesA <- res$toArray()
    name <- sapply(valuesA, function(item) { .jcall(item, "S", "getConceptName") })
    duration <- sapply(valuesA, function(item) { .jcall(item, "J", "getDuration")})
    eventCount <- sapply(valuesA, function(item) { .jcall(item, "I", "getEventCount")})
    resource <- sapply(valuesA, function(item) { .jcall(item, "S", "getOrgResource")})
    role <- sapply(valuesA, function(item) { .jcall(item, "S", "getOrgRole")})
    eventRepetitions <- sapply(valuesA, function(item) { .jcall(item, "I", "getEventRepetitions")})
    df <- J("java.time.format.DateTimeFormatter")$ofPattern("yyyy-MM-dd hh:mm:ss.z")
    ts <- sapply(valuesA, function(item) {
        ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getStartTime");
        .jcall(ldt, "S", "format", df)});
    startTime <- ymd_hms(ts)

    ts <- sapply(valuesA, function(item) {
        ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getEndTime");
        .jcall(ldt, "S", "format", df)});
    endTime <- ymd_hms(ts)
    data.frame(trace = name, duration = duration, eventCount = eventCount, resource = resource, role = role, eventRepetitions = eventRepetitions, startTime = startTime, endTime = endTime)

}

#' Calculate average share of each event class duration of total trace duration over full log
#'
#' @param xesTools reference to XEStools class object created via \code{\link{xes.init}}
#' @param median if true, use median instead of mean
#' @return data.frame with event shares
xes.getEventShare <- function(xesTools, median = FALSE) {
    # java.util.Map ru.ramax.processmining.XEStools.eventDurationShares(java.util.Map,boolean)
    emptyMap <- .jnew("java.util.HashMap")
    res <- xesTools$eventDurationShares(emptyMap, FALSE)
    kset <- .jcall(res, "Ljava/util/Set;", "keySet")
    kSetA <- .jcall(kset, "[Ljava/lang/Object;", "toArray")
    events <- sapply(kSetA, function(item) { .jstrVal(item)})
    values <- .jcall(res, "Ljava/util/Collection;", "values")
    valuesA <- .jcall(values, "[Ljava/lang/Object;", "toArray")
    shares <- sapply(valuesA, function(item) {  .jcall(item, "D", "doubleValue") })
    data.frame(event = events, share = shares)
}
