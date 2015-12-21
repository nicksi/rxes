#' Initialize XES Log processing class
#'
#' \code{xes.init} returns pointer to the javaClass with empty log
#'
#' You should pass this reference to the other funcions, so we can use cache and avoiud log reload on any
#' operation
#'
#' @return reference to xes object
xes.init <- function() {
    xes <- .jnew("org.processmining.xestools.XEStools")
    return (xes)
}

#' Loads and parses XES log in GZIP xml format
#'
#' loads and saves in the class object log specified by the filename
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @param filename string pointing to the event log file
#' @return boolean success flag
xes.parseLog <- function(xes, filename) {
    checkxes(xes)
    # returned object success or failure
    result <- .jcall(xes, "Z", "parseLog", filename  )

    return (result)
}

#' Return count of traces in the event log
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @return success flag
xes.traceCount <- function(xes) {
    checkxes(xes)
    result <- .jcall(xes, "I", "getXLogSize")
    return (result)
}

#' Return data.frame with the trace name (concept:name) and trace duration expressed in seconds
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @return data.frame with trace durations
xes.getTraceDurations <- function(xes) {
    checkxes(xes)
    res <- .jcall(xes, "Ljava/util/Map;", "getTraceDurations")
    kset <- .jcall(res, "Ljava/util/Set;", "keySet")
    kseta <- .jcall(kset, "[Ljava/lang/Object;", "toArray")
    traces <- sapply(kseta, function(item) {
            .jstrVal(item)
        }
    )
    values <- .jcall(res, "Ljava/util/Collection;", "values")
    valuesa <- .jcall(values, "[Ljava/lang/Object;", "toArray")
    durations <- sapply(valuesa, function(item) {
            .jcall(item, "J", "longValue")
        }
    )
    data.frame(trace = traces, duration = durations)
}

#' Return list of all trace segments limited by event names with attributes
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @param start_event name of first event in trace segment
#' @param end_event name of last event in trace segment
#' @return data.frame with traces
xes.getFullSubTraceList <- function(xes, start_event, end_event) {
    checkxes(xes)
    emptymap <- .jnew("java.util.HashMap")
    res <- .jcall(xes,
                  "Ljava/util/List;",
                  "getFullSubTraceList",
                  .jcast(emptymap, "java/util/Map"),
                  start_event,
                  end_event,
                  use.true.class = T)
    createdf(res)
}

#' Return list of all traces with attributes
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @param resources list of resources present in trace's event to pass filter. MULTI for multiple resources per trace
#' @param groups list of resources present in trace's event to pass filter. MULTI for multiple groups per trace
#' @param roles list of resources present in trace's event to pass filter. MULTI for multiple roles per trace
#' @param eventcount range of events per trace to pass filter
#' @param tracestart range of trace start dates to pass filter
#' @param tracesend range of trace end dates to pass filter
#' @param eventnames list of events trace should contain (at least one) to pass filter
#' @param tracestartwday list of trace start DoW (at least one)  to pass filter
#' @param tracesendwday list of trace end DoW (at least one) to pass filter
#' @param tracesend list of event transitions (at least one) to pass filter
#' @return data.frame with traces
xes.getFullTraceList <- function(xes,
                                 resources = NULL,
                                 groups = NULL,
                                 roles = NULL,
                                 eventcount = NULL,
                                 tracestart = NULL,
                                 traceend = NULL,
                                 eventnames = NULL,
                                 tracestartwday = NULL,
                                 traceendwday = NULL,
                                 transitions = NULL
                                 ) {
    checkxes(xes)
    filtermap <- processfilter(resources,
                               groups,
                               roles,
                               eventcount,
                               tracestart,
                               traceend,
                               eventnames,
                               tracestartwday,
                               traceendwday,
                               transitions
                               )
    #java.util.List ru.ramax.processmining.xes.getFullTraceList(java.util.Map)
    #java.util.HashMap com.google.common.collect.Maps.newHashMap()
    # emptymap <- .jnew("java.util.HashMap")
    res <- .jcall(xes,
                  "Ljava/util/List;",
                  "getFullTraceList",
                  .jcast(filtermap, "java/util/Map"), use.true.class = T)
    createdf(res)
}

#' Calculate average share of each event class duration of total trace duration over full log
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @param median if true, use median instead of mean
#' @return data.frame with event shares
xes.getEventShare <- function(xes, median = FALSE) {
    checkxes(xes)
    # java.util.Map ru.ramax.processmining.xes.eventDurationShares(java.util.Map,boolean)
    emptymap <- .jnew("java.util.HashMap")
    res <- xes$eventDurationShares(emptymap, FALSE)
    kset <- .jcall(res, "Ljava/util/Set;", "keySet")
    kseta <- .jcall(kset, "[Ljava/lang/Object;", "toArray")
    events <- sapply(kseta, function(item) {
            .jstrVal(item)
        }
    )
    values <- .jcall(res, "Ljava/util/Collection;", "values")
    valuesa <- .jcall(values, "[Ljava/lang/Object;", "toArray")
    shares <- sapply(valuesa, function(item) {
            .jcall(item, "D", "doubleValue")
        }
    )
    data.frame(event = events, share = shares)
}

xes.getWorkload <- function(xes, filter = NULL) {
    # parameters handling
    checkxes(xes)

    df <- J("java.time.format.DateTimeFormatter")$ofPattern("yyyy-MM-dd hh:mm:ss.z")
    emptymap <- .jnew("java.util.HashMap")
    res <- xes$calculateResourceWorkload(emptymap)
    res_array <- .jcall(res, "[Ljava/lang/Object;", "toArray", use.true.class = T)
    resource <- sapply(res_array, function(item) {
            .jcall(item, "S", "getResource")
        }
    )
    role <- sapply(res_array, function(item) {
            .jcall(item, "S", "getRole")
        }
    )
    group <- sapply(res_array, function(item) {
            .jcall(item, "S", "getGroup")
        }
    )
    workload <- sapply(res_array, function(item) {
            ljo <- .jcall(item, "Ljava/lang/Long;", "getWorkload")
            .jcall(ljo, "J", "longValue")
        }
    )
    timestamp <- with_tz(ymd_hms(sapply(res_array, function(item) {
            ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getTimestamp");
            .jcall(ldt, "S", "format", df)
        }
    )), tzone = Sys.timezone())

    data.frame(
        resource = resource,
        role = role,
        group = group,
        workload = workload,
        timestamp = timestamp
        )
}

