package ru.ramax.processmining;

import com.google.common.collect.Maps;
import com.sun.org.apache.xpath.internal.objects.XObject;
import lombok.Getter;
import lombok.NonNull;
import org.deckfour.xes.factory.XFactoryNaiveImpl;
import org.deckfour.xes.in.XesXmlGZIPParser;
import org.deckfour.xes.model.*;
import org.deckfour.xes.model.buffered.XTraceIterator;

import java.io.*;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Map;

/**
 * Created by nsitnikov on 16/12/15.
 */
public class XEStools {

    @Getter
    private XLog xlog;

    // cache for traceStartTime
    private Map<String, LocalDateTime> traceStartTime = Maps.newHashMap();
    // cache for traceStartTime
    private Map<String, LocalDateTime> traceEndTime = Maps.newHashMap();
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
     * Find the start timestamp of the trace
     * @param xTrace
     * @param eventName name of the event to search for, null to consider all events
     * @return LocalDateTime
     */
    public LocalDateTime traceStartTime(@NonNull XTrace xTrace, String eventName) {
        LocalDateTime startTime = LocalDateTime.MIN;

        String index = getIndex(xTrace);
        if ( eventName == null && traceStartTime.containsKey(index) ) {
            startTime = traceStartTime.get(index);
        }
        else {
            if (xTrace.size() > 0) {
                ListIterator events = xTrace.listIterator();
                while (events.hasNext()) {
                    XEvent xEvent = (XEvent)events.next(); // TODO switch to minimal date time, not first
                    if (eventName != null && !getAttribute(xEvent, "concept:name").equals(eventName)) continue;
                    LocalDateTime current = (LocalDateTime)getAttribute(xEvent, "time:timestamp");

                    if (current != null) {
                        if (startTime.equals(LocalDateTime.MIN) || current.isBefore(startTime)) {
                            startTime = current;
                            if (eventName == null)
                                traceStartTime.put(index, startTime);
                        }
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
    public LocalDateTime traceStartTime(@NonNull XTrace xTrace) {
        return traceStartTime(xTrace, null);
    }

    /***
     * Find the start timestamp of the trace based on trace concept:name
     * @param index Concept name to find
     * @return
     */
    public LocalDateTime traceStartTime(@NonNull String index) {
        LocalDateTime startTime = LocalDateTime.MIN;

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
    public LocalDateTime traceStartTime(@NonNull String index, String eventName) {
        LocalDateTime startTime = LocalDateTime.MAX;

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
    public LocalDateTime traceEndTime(@NonNull XTrace xTrace, String eventName) {
        LocalDateTime endTime = LocalDateTime.MAX;

        String index = getIndex(xTrace);
        if ( eventName == null && traceEndTime.containsKey(index) ) {
            endTime = traceEndTime.get(index);
        }
        else {
            if (xTrace.size() > 0) {
                ListIterator events = xTrace.listIterator();
                while (events.hasNext()) {
                    XEvent xEvent = (XEvent)events.next(); // TODO switch to minimal date time, not first
                    if (eventName != null && !getAttribute(xEvent, "concept:name").equals(eventName)) continue;
                    LocalDateTime current = (LocalDateTime)getAttribute(xEvent, "time:timestamp");

                    if (current != null) {
                        if (endTime.equals(LocalDateTime.MAX) || current.isAfter(endTime)) {
                            endTime = current;
                            if (eventName == null)
                                traceEndTime.put(index, endTime);
                        }
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
    public LocalDateTime traceEndTime(@NonNull String index) {
        LocalDateTime endTime = LocalDateTime.MAX;

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
    public LocalDateTime traceEndTime(@NonNull XTrace xTrace) {
        return  traceEndTime(xTrace, null);
    }

    /***
     * Find the last timestamp of the trace based on trace concept:name
     * @param index Concept name to find
     * @return
     */
    public LocalDateTime traceEndTime(@NonNull String index, String eventName) {
        LocalDateTime endTime = LocalDateTime.MAX;

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
    public Long getTraceDuration(@NonNull XTrace xTrace) {
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
    public Long getTraceDuration(@NonNull XTrace xTrace, String startEventName, String endEventName) {
        Long duration = 0L;

        LocalDateTime start = traceStartTime(xTrace, startEventName);
        LocalDateTime end = traceEndTime(xTrace, endEventName);
        if (start.isAfter(LocalDateTime.MIN) && end.isBefore(LocalDateTime.MAX) && end.isAfter(start)) {
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

//    public Map<String, Long> getTraceDurationsWithResource(String filter) {
//
//    }
//
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

    /* Private functions */

    /***
     * Clear all cache maps
     */
    private void clearCache() {
        // clear cache
        traceStartTime.clear();
        traceEndTime.clear();
        name2index.clear();
    }

    /***
     * Get attribute value
     * @param object
     * @param name
     * @return
     */
    private Object getAttribute(XAttributable object, String name) {
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
                        value = LocalDateTime.ofEpochSecond(((XAttributeTimestamp) attribute).getValueMillis() / 1000, 0, ZoneOffset.ofHours(0));
                        break;
                    }
                }
            }
        }

        return value;
    }

    /***
     * Get object index
     * @param object
     * @return
     */
    private String getIndex(XAttributable object) {
        String index = null;

        if (XTrace.class.isInstance(object) || XEvent.class.isInstance(object)) {
             index = (String)getAttribute(object, "concept:name");
        }

        return index;
    }
}
