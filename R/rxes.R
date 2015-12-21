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
    # returned object success or failure
    result <- .jcall(xes, "Z", "parseLog", filename  )

    return (result)
}

#' Return count of traces in the event log
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @return success flag
xes.traceCount <- function(xes) {
    result <- .jcall(xes, "I", "getXLogSize")
    return (result)
}

#' Return data.frame with the trace name (concept:name) and trace duration expressed in seconds
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @return data.frame with trace durations
xes.getTraceDurations <- function(xes) {
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
#' @param startEvent name of first event in trace segment
#' @param endEvent name of last event in trace segment
#' @return data.frame with traces
xes.getFullSubTraceList <- function(xes, start_event, end_event) {
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
#' @return data.frame with traces
xes.getFullTraceList <- function(xes) {
    #java.util.List ru.ramax.processmining.xes.getFullTraceList(java.util.Map)
    #java.util.HashMap com.google.common.collect.Maps.newHashMap()
    emptymap <- .jnew("java.util.HashMap")
    res <- .jcall(xes,
                  "Ljava/util/List;",
                  "getFullTraceList",
                  .jcast(emptymap, "java/util/Map"), use.true.class = T)
    createdf(res)
}

#' Supporting function - transform map of traces into dataframe
#' @param res javaref object containing map of flatXTraces
#' @return data.frame with traces
createdf <- function(res) {
    # we will create data frame with following columns: name, duration, startTime, endTime, eventCount, resource, role, eventRepetition
    valuesa <- res$toArray()
    name <- sapply(valuesa, function(item) {
            .jcall(item, "S", "getConceptName")
        }
    )
    duration <- sapply(valuesa, function(item) {
            .jcall(item, "J", "getDuration")
        }
    )
    eventcount <- sapply(valuesa, function(item) {
            .jcall(item, "I", "getEventCount")
        }
    )
    resource <- sapply(valuesa, function(item) {
            .jcall(item, "S", "getOrgResource")
        }
    )
    role <- sapply(valuesa, function(item) {
            .jcall(item, "S", "getOrgRole")
        }
    )
    event_repetitions <- sapply(valuesa, function(item) {
            .jcall(item, "I", "getEventRepetitions")
        }
    )
    df <- J("java.time.format.DateTimeFormatter")$ofPattern("yyyy-MM-dd hh:mm:ss.z")
    ts <- sapply(valuesa, function(item) {
            ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getStartTime");
            .jcall(ldt, "S", "format", df)
        }
    )
    start_time <- ymd_hms(ts)

    ts <- sapply(valuesa, function(item) {
            ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getEndTime");
            .jcall(ldt, "S", "format", df)
        }
    )
    end_time <- ymd_hms(ts)

    data.frame(
        trace = name,
        duration = duration,
        eventcount = eventcount,
        resource = resource,
        role = role,
        event_repetitions = event_repetitions,
        start_time = start_time,
        end_time = end_time)

}

#' Calculate average share of each event class duration of total trace duration over full log
#'
#' @param xes reference to xes class object created via \code{\link{xes.init}}
#' @param median if true, use median instead of mean
#' @return data.frame with event shares
xes.getEventShare <- function(xes, median = FALSE) {
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

xes.getWorkload <- function(xes, filter = null) {
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
    timestamp <- ymd_hms(sapply(res_array, function(item) {
            ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getTimestamp");
            .jcall(ldt, "S", "format", df)
        }
    ))

    data.frame(
        resource = resource,
        role = role,
        group = group,
        workload = workload,
        timestamp = timestamp
        )
}
