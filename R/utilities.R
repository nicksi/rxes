
checkxes <- function(xes) {
    if (class(xes)[1] != "jobjRef" ||
            ! xes %instanceof% "org.processmining.xestools.XEStools")
        stop("wrong value of xes argument - should be object returned by xes.init")
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
    df <- J("java.time.format.DateTimeFormatter")$ofPattern("yyyy-MM-dd HH:mm:ss.z")
    ts <- sapply(valuesa, function(item) {
        ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getStartTime");
        .jcall(ldt, "S", "format", df)
    }
    )
    start_time <- with_tz(ymd_hms(ts), tzone=Sys.timezone())

    ts <- sapply(valuesa, function(item) {
        ldt <- .jcall(item, "Ljava/time/ZonedDateTime;", "getEndTime");
        .jcall(ldt, "S", "format", df)
    }
    )
    end_time <- with_tz(ymd_hms(ts), tzone = Sys.timezone())

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
#' Create filter for xestool calls based on provided parameters
#' @param resources list of resources present in trace's event to pass filter. MULTI for multiple resources per trace
#' @param groups list of resources present in trace's event to pass filter. MULTI for multiple groups per trace
#' @param roles list of resources present in trace's event to pass filter. MULTI for multiple roles per trace
#' @param eventcount range of events per trace to pass filter
#' @param tracestart range of trace start dates to pass filter
#' @param traceend range of trace end dates to pass filter
#' @param eventnames list of events trace should contain (at least one) to pass filter
#' @param tracestartwday list of trace start DoW (at least one)  to pass filter
#' @param traceendwday list of trace end DoW (at least one)  to pass filter#'
#' @param transitions list of event transitions statuses to pass filter
#' @param tracenames list of trace names to pass filter
#'
#' @return filter object (hashmap)
xes.processfilter <- function(resources = NULL,
                          groups = NULL,
                          roles = NULL,
                          eventcount = NULL,
                          tracestart = NULL,
                          traceend = NULL,
                          eventnames = NULL,
                          tracestartwday = NULL,
                          traceendwday = NULL,
                          transitions = NULL,
                          tracenames = NULL ) {
    filter <- .jnew('java.util.HashMap')
    if ( !is.null(resources) ) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$RESOURCE_LIST
        filter$put(
            type,
            J("com.google.common.collect.Lists")$newArrayList(.jarray(resources))
        )
    }
    if ( !is.null(groups) ) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$GROUP_LIST
        filter$put(
            type,
            J("com.google.common.collect.Lists")$newArrayList(.jarray(groups))
        )
    }
    if ( !is.null(roles) ) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$ROLE_LIST
        filter$put(
            type,
            J("com.google.common.collect.Lists")$newArrayList(.jarray(resources))
        )
    }
    if ( !is.null(eventnames) ) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$EVENT_NAME_LIST
        filter$put(
            type,
            J("com.google.common.collect.Lists")$newArrayList(.jarray(eventnames))
        )
    }
    if ( !is.null(tracestartwday) ) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$TRACE_START_WEEKDAY_LIST
        filter$put(
            type,
            J("com.google.common.collect.Lists")$newArrayList(.jarray(tracestartwday))
        )
    }
    if ( !is.null(traceendwday) ) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$TRACE_END_WEEKDAY_LIST
        filter$put(
            type,
            J("com.google.common.collect.Lists")$newArrayList(.jarray(traceendwday))
        )
    }
    if ( !is.null(transitions) ) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$LIFECYCLE_TRANSITION_LIST
        filter$put(
            type,
            J("com.google.common.collect.Lists")$newArrayList(.jarray(transitions))
        )
    }
    if ( !is.null(tracenames) ) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$TRACE_NAME_LIST
        filter$put(
            type,
            J("com.google.common.collect.Lists")$newArrayList(.jarray(tracenames))
        )
    }
    if (!is.null(eventcount)) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$EVENT_COUNT_RANGE
        if ( "min" %in% names(eventcount) && "max" %in% names(eventcount)) {
            min <- new (J("java.lang.Integer"), as.character(eventcount["min"]))
            max <- new (J("java.lang.Integer"), as.character(eventcount["max"]))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "closed",
                            .jcast(min, "java.lang.Comparable"),
                            .jcast(max, "java.lang.Comparable")
            )
            filter$put(type, range)
        } else if ("min" %in% names(eventcount) && !"max" %in% names(eventcount)) {
            min <- new (J("java.lang.Integer"), as.character(eventcount["min"]))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "atLeast",
                            .jcast(min, "java.lang.Comparable")
            )
            filter$put(type, range)
        } else if (!"min" %in% names(eventcount) && "max" %in% names(eventcount)) {
            max <- new (J("java.lang.Integer"), as.character(eventcount["max"]))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "atMost",
                            .jcast(max, "java.lang.Comparable")
            )
            filter$put(type, range)
        }
    }
    if (!is.null(tracestart)) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$TRACE_START_RANGE
        jzd <- J("java.time.ZonedDateTime")
        if ( "min" %in% names(tracestart) && "max" %in% names(tracestart)) {
            min <- jzd$parse(sub("(\\d\\d)$", ":\\1", format(tracestart["min"],"%Y-%m-%dT%H:%M:%S.000%z")))
            max <- jzd$parse(sub("(\\d\\d)$", ":\\1", format(tracestart["max"],"%Y-%m-%dT%H:%M:%S.000%z")))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "closed",
                            .jcast(min, "java.lang.Comparable"),
                            .jcast(max, "java.lang.Comparable")
            )
            filter$put(type, range)
        } else if ("min" %in% names(tracestart) && !"max" %in% names(tracestart)) {
            min <- jzd$parse(sub("(\\d\\d)$", ":\\1", format(tracestart["min"],"%Y-%m-%dT%H:%M:%S.000%z")))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "atLeast",
                            .jcast(min, "java.lang.Comparable")
            )
            filter$put(type, range)
        } else if (!"min" %in% names(tracestart) && "max" %in% names(tracestart)) {
            max <- jzd$parse(sub("(\\d\\d)$", ":\\1", format(tracestart["max"],"%Y-%m-%dT%H:%M:%S.000%z")))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "atMost",
                            .jcast(max, "java.lang.Comparable")
            )
            filter$put(type, range)
        }
    }
    if (!is.null(traceend)) {
        type <- J("org.processmining.xestools.XEStools")$FilterType$TRACE_END_RANGE
        jzd <- J("java.time.ZonedDateTime")
        if ( "min" %in% names(traceend) && "max" %in% names(traceend)) {
            min <- jzd$parse(sub("(\\d\\d)$", ":\\1", format(traceend["min"],"%Y-%m-%dT%H:%M:%S.000%z")))
            max <- jzd$parse(sub("(\\d\\d)$", ":\\1", format(traceend["max"],"%Y-%m-%dT%H:%M:%S.000%z")))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "closed",
                            .jcast(min, "java.lang.Comparable"),
                            .jcast(max, "java.lang.Comparable")
            )
            filter$put(type, range)
        } else if ("min" %in% names(traceend) && !"max" %in% names(traceend)) {
            min <- jzd$parse(sub("(\\d\\d)$", ":\\1", format(traceend["min"],"%Y-%m-%dT%H:%M:%S.000%z")))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "atLeast",
                            .jcast(min, "java.lang.Comparable")
            )
            filter$put(type, range)
        } else if (!"min" %in% names(traceend) && "max" %in% names(traceend)) {
            max <- jzd$parse(sub("(\\d\\d)$", ":\\1", format(traceend["max"],"%Y-%m-%dT%H:%M:%S.000%z")))
            range <- .jcall(J("com.google.common.collect.Range"),
                            "Lcom/google/common/collect/Range;",
                            "atMost",
                            .jcast(max, "java.lang.Comparable")
            )
            filter$put(type, range)
        }
    }
    return (filter)
}
