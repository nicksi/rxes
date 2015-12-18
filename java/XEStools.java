package ru.ramax.processmining;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multiset;
import lombok.Getter;
import lombok.NonNull;
import org.apache.commons.math.stat.descriptive.DescriptiveStatistics;
import org.deckfour.xes.extension.std.XLifecycleExtension;
import org.deckfour.xes.extension.std.XTimeExtension;
import org.deckfour.xes.factory.XFactoryNaiveImpl;
import org.deckfour.xes.in.XesXmlGZIPParser;
import org.deckfour.xes.model.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Set of methods to work with XES logs
 * Created by Nikolai Sitnikov on 16/12/15.
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
     * @param xLog - new XLog
     */
    public void setXLog(@NonNull XLog xLog) {
        this.xlog = xLog;
        clearCache();
    }

    /***
     * Return number of traces in log
     * @return - number of traces. 0 if none
     */
    public int getXLogSize() {
        return xlog.size();
    }


    /***
     * Find the start timestamp of the trace
     * @param xTrace XTrace object to process
     * @param eventName name of the event to search for, null to consider all events
     * @return ZonedDateTime
     */
    static public ZonedDateTime traceStartTime(@NonNull XTrace xTrace, String eventName) {
        ZonedDateTime startTime = MINTIME;

        if (xTrace.size() > 0) {
            for(XEvent xEvent: xTrace) {
                if (eventName != null && !getAttribute(xEvent, "concept:name").equals(eventName)) continue;
                ZonedDateTime current = getTimeStamp(xEvent);

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
     * @param xTrace xTrace object to process
     * @return zoned date time
     */
    static public ZonedDateTime traceStartTime(@NonNull XTrace xTrace) {
        return traceStartTime(xTrace, null);
    }

    /***
     * Find the start timestamp of the trace based on trace concept:name
     * @param index Concept name to find
     * @return zoned date time
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
     * @param eventName name of Event to look for or null
     * @return zoned date time
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
     * @param xTrace xTrace to process
     * @param eventName name of the event to search for, null to consider all events
     * @return zoned date time
     */
    static public ZonedDateTime traceEndTime(@NonNull XTrace xTrace, String eventName) {
        ZonedDateTime endTime = MAXTIME;

        if (xTrace.size() > 0) {
            for(XEvent xEvent: xTrace) {
                if (eventName != null && !getAttribute(xEvent, "concept:name").equals(eventName)) continue;
                ZonedDateTime current = getTimeStamp(xEvent);

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
     * @return zoned date time
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
     * @param xTrace trace to process
     * @return zoned date time
     */
    static public ZonedDateTime traceEndTime(@NonNull XTrace xTrace) {
        return  traceEndTime(xTrace, null);
    }

    /***
     * Find the last timestamp of the trace based on trace concept:name
     * @param index Concept name to find
     * @param eventName name of the event to search for, null to consider all events
     * @return zoned date time
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
     * @param xTrace trace to process
     * @return trace duration
     */
    static public Long getTraceDuration(@NonNull XTrace xTrace) {
        return getTraceDuration(xTrace, null, null);
    }


    /***
     * Find the duration of trace based on trace concept:name
     * @param index Concept name to find trace
     * @return trace duration
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
     * Calculate duration of trace segment based on event names limits
     * @param xTrace trace to process
     * @param startEventName start event name in trace. Null to remove limit
     * @param endEventName end event name in trace. Null to remove limit
     * @return trace/segment duration
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
     * Return map of all traces/segments durations limited by event names
     * @param startEvent start event name in trace. Null to remove limit
     * @param endEvent end event name in trace. Null to remove limit
     * @return map of trace/segment durations
     */
    public Map<String, Long> getTraceDurations(String startEvent, String endEvent) {
        Map <String, Long> durations = Maps.newHashMap();

        for(XTrace xTrace: xlog) {
            durations.put(getIndex(xTrace), getTraceDuration(xTrace, startEvent, endEvent));
        }

        return durations;
    }

    /***
     * Return trace durations for the whole log
     * @return trace duration
     */
    public Map<String, Long> getTraceDurations() {
        return getTraceDurations(null, null);
    }


    /***
     * Returns the list of flat trace segments filtered by filter providers
     * @param filter Map of filters as attribute name = allowed value. Value сan be regex
     * @param startName event name of segment start
     * @param endName event name of segment end
     * @return list of trace segments with calculated statistics
     */
    public List<FlatXTrace> getFullSubTraceList(Map<String, String> filter, String startName, String endName) {
        List<FlatXTrace> traces = Lists.newArrayList();

        for (XTrace current: xlog) {

            // TODO apply filter if any
            current = trimTrace(current, startName, endName);
            if (current != null && current.size() > 0) {
                FlatXTrace flatXTrace = new FlatXTrace(current);
                traces.add(flatXTrace);
            }

        }

        return traces;
    }

    /***
     * Copy trace segment as separate trace
     * @param trace trace to process
     * @param startName name of segment starting event
     * @param endName name of segment ending event
     * @return trace or null if start/end combination do not exist in trace
     */
    static public XTrace trimTrace(XTrace trace, String startName, String endName) {
        XTrace trimmed = (XTrace)trace.clone();

        ZonedDateTime startTime = traceStartTime(trimmed, startName);
        ZonedDateTime endTime = traceEndTime(trimmed, endName);
        if (startTime.isAfter(MINTIME) && endTime.isBefore(MAXTIME) && !startTime.isAfter(endTime)) {
            trimmed.removeIf(
                    event -> (
                            getTimeStamp(event).isBefore(startTime) ||
                            getTimeStamp(event).isAfter(endTime)
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
     * @return map of traces
     */
    public List<FlatXTrace> getFullTraceList(Map<String, String> filter) {
        List<FlatXTrace> traces = Lists.newArrayList();

        for(XTrace current: xlog) {
            // TODO apply filter if any
            FlatXTrace flatXTrace = new FlatXTrace(current);
            traces.add(flatXTrace);

        }

        return traces;
    }

    /***
     * Return event attribute for trace. "NA" if no org:resource attributes in any event, "MULTI" if multiple resources
     * @param xTrace trace to look into
     * @param attribute name attribute to receive
     * @param eventName event name to check
     * @return attribute value
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
     * @param name name to search for
     * @return trace of null if not found
     */
    public XTrace getXTrace(String name) {
        XTrace xTrace = null;
        int i = 0;

        if(xlog.size() > 0) {

            if (name2index.containsKey(name))
                xTrace = xlog.get(name2index.get(name));
            else {
                for (XTrace currentTrace: xlog) {
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
     * @param object object with attributes (event or trace)
     * @return concept:name value
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
     * @param object object with attributes (event or trace)
     * @param name attribute name
     * @return attribute value
     */
    static public Object getAttribute(XAttributable object, String name) {
        Object value = null;

        if (object.hasAttributes()) {
            XAttributeMap xAttributeMap = object.getAttributes();
            XAttribute attribute = null;

            if (xAttributeMap.containsKey(name)) {
                attribute = xAttributeMap.get(name);
            }
            else {
                for (XAttribute current : xAttributeMap.values()) {
                    if (current.getKey().equals(name)) {
                        attribute = current;
                        break;
                    }
                }
            }

            if (attribute != null) {
                if (XAttributeLiteral.class.isInstance(attribute)) {
                    value = ((XAttributeLiteral) attribute).getValue();
                }
                else if (XAttributeTimestamp.class.isInstance(attribute)) {
                    value = ZonedDateTime.ofInstant(((XAttributeTimestamp) attribute).getValue().toInstant(), ZoneId.of("UTC"));
                }
            }
        }

        return value;
    }

    /***
     * Calculate average share of event duration in trace on whole log
     * @param filter log filter to apply
     * @param useMedian mode of calculation - mean or median (better if skewed)
     * @return
     */
    public Map<String, Double> eventDurationShares(Map<String, String> filter, boolean useMedian) {
        Map<String, Double> shares = Maps.newHashMap();
        Map<String, DescriptiveStatistics> buffer = Maps.newHashMap();

        for(XTrace xTrace: xlog) {
            Map<String, Double> traceShares = eventSharesInTrace(xTrace);
            for (Map.Entry<String, Double> entry: traceShares.entrySet()) {
                DescriptiveStatistics current = buffer.getOrDefault(entry.getKey(), new DescriptiveStatistics());
                current.addValue(entry.getValue());
                buffer.put(
                        entry.getKey(),
                        current
                );
            }
        }

        // average share
        for (Map.Entry<String, DescriptiveStatistics> entry: buffer.entrySet()) {
            if (useMedian)
                shares.put(entry.getKey(), entry.getValue().getPercentile(50));
            else
                shares.put(entry.getKey(), entry.getValue().getMean());
        }

        return shares;
    }

    /***
     * Calculate event class(unique name) share of trace duration
     * @param xTrace object to process
     * @return map with event name and share
     */
    static public Map<String, Double> eventSharesInTrace(XTrace xTrace) {
        Map<String, Double> shares = eventDurationInTrace(xTrace);

        // divide by total trace duration
        double duration = (double) getTraceDuration(xTrace);
        shares.replaceAll((key, value) -> value / duration);

        return shares;
    }

    /***
     * Calculate event class(unique name) duration in trace
     * @param xTrace object to process
     * @return map with event name and duration
     */
    static public Map<String, Double> eventDurationInTrace(XTrace xTrace) {
        Map<String, Double> durations = Maps.newHashMap();
        Map<XEvent, ZonedDateTime> buffer = Maps.newHashMap();

        if (xTrace.size() > 0) {
            // let sort events first
            Collections.sort(xTrace,
                    (e1, e2) -> XTimeExtension.instance().extractTimestamp(e1)
                            .compareTo(XTimeExtension.instance().extractTimestamp(e2)));
            XEvent lastEvent = null;

            for (XEvent xEvent: xTrace) {
                if (lastEvent != null && !lastEvent.getAttributes().containsKey(XLifecycleExtension.KEY_TRANSITION) ) {
                    buffer.put(lastEvent, getTimeStamp(xEvent));
                }
                if (xEvent.getAttributes().containsKey(XLifecycleExtension.KEY_TRANSITION)) {
                    if (XLifecycleExtension.instance().extractStandardTransition(xEvent) == XLifecycleExtension.StandardModel.START)
                    {
                        buffer.put(xEvent, MINTIME);
                    }
                    else if (
                            XLifecycleExtension.instance().extractStandardTransition(xEvent) == XLifecycleExtension.StandardModel.COMPLETE
                            ) {
                        // we only support START and COMPLETE transitions
                        // go back and update open event. We assume no nested events available, nor same event can run in parallel
                        // TODO improve code to accepts mentioned cases
                        String name = getIndex(xEvent);
                        for (XEvent key: buffer.keySet()) {
                            if (getIndex(key).equals(name) && buffer.get(key).equals(MINTIME)) {
                                buffer.put(key, getTimeStamp(xEvent));
                                break;
                            }
                        }
                    }

                }
                else {
                    buffer.put(xEvent, MINTIME);
                }

                lastEvent = xEvent;
            }

            // calculate total event durations
            for(Map.Entry<XEvent, ZonedDateTime> entry: buffer.entrySet()) {
                if (getTimeStamp(entry.getKey()).isBefore(entry.getValue())) {
                    durations.put(getIndex(entry.getKey()),
                            durations.getOrDefault(getIndex(entry.getKey()), 0D) +
                                    Duration.between(getTimeStamp(entry.getKey()), entry.getValue()).getSeconds());
                }
            }
        }
        return durations;
    }

    /***
     * Get event timestamp or MIN time of not present
     * @param xEvent to process
     * @return Zoned Time Date
     */
    static public ZonedDateTime getTimeStamp(XEvent xEvent) {
        ZonedDateTime stamp = MINTIME;

        if (xEvent.getExtensions().contains(XTimeExtension.instance())) {
            stamp = ZonedDateTime.ofInstant(XTimeExtension.instance().extractTimestamp(xEvent).toInstant(), ZoneId.of("UTC"));
        }

        return stamp;
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
