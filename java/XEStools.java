package ru.ramax.processmining;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.Getter;
import lombok.NonNull;
import org.deckfour.xes.factory.XFactoryNaiveImpl;
import org.deckfour.xes.in.XesXmlGZIPParser;
import org.deckfour.xes.model.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.time.*;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

/**
 * Created by nsitnikov on 16/12/15.
 */
public class XEStools {

    static private final ZonedDateTime MINTIME = ZonedDateTime.of(LocalDateTime.MIN, ZoneId.systemDefault());
    static private final ZonedDateTime MAXTIME = ZonedDateTime.of(LocalDateTime.MAX, ZoneId.systemDefault());

    @Getter
    private XLog xlog;

    // cache to search for trace by concept:name
    private Map<String, Integer> name2index = Maps.newHashMap();

    public XEStools() {
        this.xlog = null;
    }

    public XEStools(@NonNull XLog xlog) {
        setXLog(xlog);
    }

    public boolean parseLog(String filename) {
        try {
            XesXmlGZIPParser parser = new XesXmlGZIPParser(new XFactoryNaiveImpl());
            File file = new File(filename);
            if (parser.canParse(file)) {
                InputStream inputStream = new FileInputStream(file);
                this.xlog = parser.parse(inputStream).get(0);
                return true;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        return false;
    }

    /***
     * Select log to work with and clear cache
     * @param xLog
     */
    public void setXLog(@NonNull XLog xLog) {
        this.xlog = xLog;
        clearCache();
    }

    /***
     * Return number of traces in log
     * @return
     */
    public int getXLogSize() {
        return xlog.size();
    }


    /***
     * Find the start timestamp of the trace
     * @param xTrace
     * @param eventName name of the event to search for, null to consider all events
     * @return LocalDateTime
     */
    static public ZonedDateTime traceStartTime(@NonNull XTrace xTrace, String eventName) {
        ZonedDateTime startTime = MINTIME;

        if (xTrace.size() > 0) {
            ListIterator events = xTrace.listIterator();
            while (events.hasNext()) {
                XEvent xEvent = (XEvent)events.next(); // TODO switch to minimal date time, not first
                if (eventName != null && !getAttribute(xEvent, "concept:name").equals(eventName)) continue;
                ZonedDateTime current = (ZonedDateTime) getAttribute(xEvent, "time:timestamp");

                if (current != null) {
                    if (startTime.equals(MINTIME) || current.isBefore(startTime)) {
                        startTime = current;
                    }
                }
            }
        }

        return startTime;
    }

    /***
     * Find startTime of the earliest event
     * @param xTrace
     * @return
     */
    static public ZonedDateTime traceStartTime(@NonNull XTrace xTrace) {
        return traceStartTime(xTrace, null);
    }

    /***
     * Find the start timestamp of the trace based on trace concept:name
     * @param index Concept name to find
     * @return
     */
    public ZonedDateTime traceStartTime(@NonNull String index) {
        ZonedDateTime startTime = MINTIME;

        XTrace xTrace = getXTrace(index);
        if (xTrace != null)
        {
            startTime = traceStartTime(xTrace, null);
        }

        return startTime;
    }

    /***
     * Find the first timestamp of the trace based on trace concept:name
     * @param index Concept name to find
     * @return
     */
    public ZonedDateTime traceStartTime(@NonNull String index, String eventName) {
        ZonedDateTime startTime = MINTIME;

        XTrace xTrace = getXTrace(index);
        if (xTrace != null)
        {
            startTime = traceStartTime(xTrace, eventName);
        }

        return startTime;
    }

    /***
     * Find the last timestamp of the trace based on trace concept:name
     * @param xTrace
     * @param eventName name of the event to search for, null to consider all events
     * @return
     */
    static public ZonedDateTime traceEndTime(@NonNull XTrace xTrace, String eventName) {
        ZonedDateTime endTime = MAXTIME;

        if (xTrace.size() > 0) {
            ListIterator events = xTrace.listIterator();
            while (events.hasNext()) {
                XEvent xEvent = (XEvent)events.next(); // TODO switch to minimal date time, not first
                if (eventName != null && !getAttribute(xEvent, "concept:name").equals(eventName)) continue;
                ZonedDateTime current = (ZonedDateTime) getAttribute(xEvent, "time:timestamp");

                if (current != null) {
                    if (endTime.equals(MAXTIME) || current.isAfter(endTime)) {
                        endTime = current;
                    }
                }
            }
        }

        return  endTime;
    }

    /***
     * Find the last timestamp of the trace based on trace concept:name
     * @param index Concept name to find
     * @return
     */
    public ZonedDateTime traceEndTime(@NonNull String index) {
        ZonedDateTime endTime = MAXTIME;

        XTrace xTrace = getXTrace(index);
        if (xTrace != null)
        {
            endTime = traceEndTime(xTrace);
        }

        return endTime;
    }

    /***
     * Find the last timestamp of the trace based on trace concept:name
     * @param xTrace
     * @return
     */
    static public ZonedDateTime traceEndTime(@NonNull XTrace xTrace) {
        return  traceEndTime(xTrace, null);
    }

    /***
     * Find the last timestamp of the trace based on trace concept:name
     * @param index Concept name to find
     * @return
     */
    public ZonedDateTime traceEndTime(@NonNull String index, String eventName) {
        ZonedDateTime endTime = MAXTIME;

        XTrace xTrace = getXTrace(index);
        if (xTrace != null)
        {
            endTime = traceEndTime(xTrace, eventName);
        }

        return endTime;
    }

    /***
     * Find the duration of trace
     * @param xTrace
     * @return
     */
    static public Long getTraceDuration(@NonNull XTrace xTrace) {
        return getTraceDuration(xTrace, null, null);
    }


    /***
     * Find the duration of trace based on trace concept:name
     * @param index Concept name to find trace
     * @return
     */
    public Long getTraceDuration(@NonNull String index) {
        Long duration = 0L;

        XTrace xTrace = getXTrace(index);
        if (xTrace != null)
        {
            duration = getTraceDuration(xTrace, null, null);
        }

        return duration;
    }

    /***
     * Calculate duration based on event names limits
     * @param xTrace
     * @param startEventName
     * @param endEventName
     * @return
     */
    static public Long getTraceDuration(@NonNull XTrace xTrace, String startEventName, String endEventName) {
        Long duration = 0L;

        ZonedDateTime start = traceStartTime(xTrace, startEventName);
        ZonedDateTime end = traceEndTime(xTrace, endEventName);
        if (start.isAfter(MINTIME) && end.isBefore(MAXTIME) && end.isAfter(start)) {
            duration = Duration.between(start, end).toMillis()/1000;
        }

        return duration;
    }

    /***
     * Return map of all traces durations limited by event names
     * @param startEvent
     * @param endEvent
     * @return
     */
    public Map<String, Long> getTraceDurations(String startEvent, String endEvent) {
        Map <String, Long> durations = Maps.newHashMap();

        Iterator traces = xlog.iterator();
        while(traces.hasNext()) {
            XTrace xTrace = (XTrace)traces.next();
            durations.put(getIndex(xTrace), getTraceDuration(xTrace, startEvent, endEvent));
        }

        return durations;
    }

    /***
     * Return trace durations for the whole log
     * @return
     */
    public Map<String, Long> getTraceDurations() {
        return getTraceDurations(null, null);
    }


    /***
     * Returns the list of flat records filtered by filter providers
     * @param filter Map of filters as attribute name = allowed value. Value сan be regex
     * @return
     */
    public List<FlatXTrace> getFullSubTraceList(Map<String, String> filter, String startName, String endName) {
        List<FlatXTrace> traces = Lists.newArrayList();

        Iterator traceIterator = xlog.iterator();
        while(traceIterator.hasNext()) {
            XTrace current = (XTrace) traceIterator.next();

            // TODO apply filter if any
            if (filter != null && filter.size() > 0) {

            }

            current = trimTrace(current, startName, endName);
            if (current != null && current.size() > 0) {
                FlatXTrace flatXTrace = new FlatXTrace(current);
                traces.add(flatXTrace);
            }

        }

        return traces;
    }

    static public XTrace trimTrace(XTrace trace, String startName, String endName) {
        XTrace trimmed = (XTrace)trace.clone();

        ZonedDateTime startTime = traceStartTime(trimmed, startName);
        ZonedDateTime endTime = traceEndTime(trimmed, endName);
        if (startTime.isAfter(MINTIME) && endTime.isBefore(MAXTIME) && !startTime.isAfter(endTime)) {
            trimmed.removeIf(
                    event -> (
                            ((ZonedDateTime)getAttribute(event, "time:timestamp")).isBefore(startTime) ||
                            ((ZonedDateTime)getAttribute(event, "time:timestamp")).isAfter(endTime)
                    )
            );
        }
        else {
            trimmed = null;
        }

        return trimmed;
    }

    /***
     * Returns the list of flat records filtered by filter providers
     * @param filter Map of filters as attribute name = allowed value. Value сan be regex
     * @return
     */
    public List<FlatXTrace> getFullTraceList(Map<String, String> filter) {
        List<FlatXTrace> traces = Lists.newArrayList();

        Iterator traceIterator = xlog.iterator();
        while(traceIterator.hasNext()) {
            XTrace current = (XTrace) traceIterator.next();

            // TODO apply filter if any
            if (filter != null && filter.size() > 0) {

            }

            FlatXTrace flatXTrace = new FlatXTrace(current);
            traces.add(flatXTrace);

        }

        return traces;
    }

    /***
     * Return event attribuite for trace. "NA" if no org:resource attributes in any event, "MULTI" if multiple resources
     * @param xTrace trace to look into
     * @param attribute name attribute to receive
     * @param eventName event name to check
     * @return org:resource value
     */
    static public String getTraceResource(XTrace xTrace, String attribute, String eventName) {
        String resource = "NA";

        Iterator eventIterator = xTrace.iterator();
        while (eventIterator.hasNext()) {
            XEvent xEvent = (XEvent) eventIterator.next();
            if ( eventName != null && !getAttribute(xEvent, "concept:name").equals(eventIterator)) continue;

            String current = (String)getAttribute(xEvent, attribute);
            if ( current == null ) continue;
            else if ( resource.equals("NA") && !current.equals(resource)) resource = current;
            else if ( !current.equals(resource) ) {
                resource = "MULTI";
                break;
            }

        }

        return resource;
    }

//    public Map<String, Long> getTraceDurationsWithResource(String filter) {
//
//    }
//
//    public Map<String, Long> getTraceDurationsWithResource(String filter) {
//
//    }


    /***
     * Search for trace by it's concept:name
     * @param name
     * @return
     */
    public XTrace getXTrace(String name) {
        XTrace xTrace = null;
        int i = 0;

        if(xlog.size() > 0) {

            if (name2index.containsKey(name))
                xTrace = xlog.get(name2index.get(name));
            else {
                Iterator xTraceIterator = xlog.iterator();
                while (xTraceIterator.hasNext()) {
                    XTrace currentTrace = (XTrace) xTraceIterator.next();

                    if (getIndex(currentTrace).equals(name)) {
                        // TODO not first but minimal
                        xTrace = currentTrace;
                        name2index.put(name, i);
                        break;
                    }

                    i++;
                }
            }
        }

        return xTrace;
    }

    /***
     * Get object index
     * @param object
     * @return
     */
    static public String getIndex(XAttributable object) {
        String index = null;

        if (XTrace.class.isInstance(object) || XEvent.class.isInstance(object)) {
            index = (String)getAttribute(object, "concept:name");
        }

        return index;
    }

    /***
     * Get attribute value
     * @param object
     * @param name
     * @return
     */
    static public Object getAttribute(XAttributable object, String name) {
        Object value = null;

        if (object.hasAttributes()) {
            XAttributeMap xAttributeMap = object.getAttributes();

            for (XAttribute attribute : xAttributeMap.values()) {
                if (attribute.getKey().equals(name)) {
                    if (XAttributeLiteral.class.isInstance(attribute)) {
                        value = ((XAttributeLiteral) attribute).getValue();
                        break;
                    }
                    else if (XAttributeTimestamp.class.isInstance(attribute)) {
                        long millis = ((XAttributeTimestamp) attribute).getValueMillis();
                        value = ZonedDateTime.ofInstant(Instant.ofEpochSecond(millis / 1000), ZoneId.of("UTC"));
                        break;
                    }
                }
            }
        }

        return value;
    }

    /* Private functions */

    /***
     * Clear all cache maps
     */
    private void clearCache() {
        // clear cache
        name2index.clear();
    }



}
