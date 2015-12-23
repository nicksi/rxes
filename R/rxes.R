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
#' @inheritParams xes.processfilter
#' @return data.frame with traces
xes.getFullSubTraceList <- function(xes, start_event, end_event, ... ) {
    checkxes(xes)
    filtermap <- xes.processfilter(...)
    res <- .jcall(xes,
                  "Ljava/util/List;",
                  "getFullSubTraceList",
                  .jcast(filtermap, "java/util/Map"),
                  start_event,
                  end_event,
                  use.true.class = T)
    createdf(res)
}

#' Return list of all traces with attributes
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @inheritParams xes.processfilter
#' @return data.frame with traces
xes.getFullTraceList <- function(xes, ...) {
    checkxes(xes)
    filtermap <- xes.processfilter(...)
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
#' @inheritParams xes.processfilter
#' @return data.frame with event shares
xes.getEventShare <- function(xes, median = FALSE,...) {
    checkxes(xes)
    filtermap <- xes.processfilter(...)
    # java.util.Map ru.ramax.processmining.xes.eventDurationShares(java.util.Map,boolean)
    res <- xes$eventDurationShares(filtermap, FALSE)
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

#' Calculate hourly workload - number of seconds spend each our by resource
#' doing events from event log
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @inheritParams xes.processfilter
#' @return data.frame with workloads
xes.getWorkload <- function(xes,...) {
    checkxes(xes)
    filtermap <- xes.processfilter(...)

    df <- J("java.time.format.DateTimeFormatter")$ofPattern("yyyy-MM-dd HH:mm:ss.z")
    res <- xes$calculateResourceWorkload(filtermap)
    res_array <- .jcall(res, "[Ljava/lang/Object;", "toArray", use.true.class = T)
    resources <- sapply(res_array, function(item) {
            .jcall(item, "S", "getResource")
        }
    )
    roles <- sapply(res_array, function(item) {
            .jcall(item, "S", "getRole")
        }
    )
    groups <- sapply(res_array, function(item) {
            .jcall(item, "S", "getGroup")
        }
    )
    workloads <- sapply(res_array, function(item) {
            ljo <- .jcall(item, "Ljava/lang/Long;", "getWorkload")
            .jcall(ljo, "J", "longValue")
        }
    )
    timestamps <- with_tz(ymd_hms(sapply(res_array, function(item) {
            ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getTimestamp");
            .jcall(ldt, "S", "format", df)
        }
    )), tzone = Sys.timezone())

    data.frame(
        resource = resources,
        role = roles,
        group = groups,
        workload = workloads,
        timestamp = timestamps
        )
}

#' Retreive event list form traces matching filter
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @param ... parameters to be passed to filter creation function \code{\link{xes.xes.processfilter}}
#' @return list of events
xes.getEventList <- function(xes, ...){
    checkxes(xes)
    filtermap <- xes.processfilter(...)
    res <- xes$getEventList(filtermap)
    df <- J("java.time.format.DateTimeFormatter")$ofPattern("yyyy-MM-dd HH:mm:ss.z")
    res_array <- .jcall(res, "[Ljava/lang/Object;", "toArray", use.true.class = T)

    resources <- sapply(res_array, function(item) {
        .jcall(item, "S", "getResource")
    }
    )
    roles <- sapply(res_array, function(item) {
        .jcall(item, "S", "getRole")
    }
    )
    groups <- sapply(res_array, function(item) {
        .jcall(item, "S", "getGroup")
    }
    )
    traces <- sapply(res_array, function(item) {
        .jcall(item, "S", "getTrace")
    }
    )
    names <- sapply(res_array, function(item) {
        .jcall(item, "S", "getName")
    }
    )
    starts <- with_tz(ymd_hms(sapply(res_array, function(item) {
        ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getStart");
        .jcall(ldt, "S", "format", df)
    }
    )), tzone = Sys.timezone())

    ends <- with_tz(ymd_hms(sapply(res_array, function(item) {
        ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getEnd");
        .jcall(ldt, "S", "format", df)
    }
    )), tzone = Sys.timezone())

    data.frame(
        resource = resources,
        role = roles,
        group = groups,
        name = names,
        trace = traces,
        start = starts,
        end = ends
    )

}

