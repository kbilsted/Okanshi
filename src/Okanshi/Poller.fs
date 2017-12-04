﻿namespace Okanshi

open System
open System.Threading

/// The metric type
type Metric =
    {
        /// Name of the metric
        Name : string;
        /// The timestamp where the metric was observed
        Timestamp : DateTimeOffset;
        /// The metric tags
        Tags : Tag array;
        /// The value
        Value : obj
        /// The sub metrics
        SubMetrics : Metric array
    }

type MetricEventArgs(metrics : Metric array) =
    inherit EventArgs()
    member __.Metrics = metrics
type MetricEventDelegate = delegate of sender : obj * args : MetricEventArgs -> unit

/// A poller that can be used to fetch the current values for a list of metrics
type IMetricPoller =
    inherit IDisposable
    /// Event raised when metric have been polled.
    [<CLIEvent>]
    abstract MetricsPolled : IEvent<MetricEventDelegate, MetricEventArgs>
    /// Stop polling for new metrics
    abstract Stop : unit -> unit
    /// Force a poll of metrics
    abstract PollMetrics : unit -> unit

/// Poller for fetching metrics from a monitor registry
type MetricMonitorRegistryPoller(registry : IMonitorRegistry, interval : TimeSpan, pollOnExit : bool) as self =
    let metricsPolled = new Event<MetricEventDelegate, MetricEventArgs>()
    let cancellationTokenSource = new CancellationTokenSource()
    let cancellationToken = cancellationTokenSource.Token

    let rec convertMonitorToMetric (monitor : IMonitor) =
        let submetrics =
            monitor.GetAllMonitors()
            |> Seq.except (seq { yield monitor })
            |> Seq.map convertMonitorToMetric
            |> Seq.toArray
        {
            Name = monitor.Config.Name
            Timestamp = DateTimeOffset.UtcNow
            Tags = monitor.Config.Tags
            Value = monitor.GetValueAndReset()
            SubMetrics = submetrics
        }

    let pollMetrics () =
        let metrics =
            registry.GetRegisteredMonitors()
            |> Seq.map convertMonitorToMetric
            |> Seq.toArray
        metricsPolled.Trigger(self, new MetricEventArgs(metrics))

    let onExitSubscriber =
        if pollOnExit then
            let currentDomain = AppDomain.CurrentDomain
            let pollMetrics = fun _ ->
                Logger.Debug.Invoke("Polling metrics because of process exit or appdomain unload")
                pollMetrics()
            if currentDomain.IsDefaultAppDomain() then
                currentDomain.ProcessExit.Subscribe(pollMetrics)
            else
                currentDomain.DomainUnload.Subscribe(pollMetrics)
        else { new IDisposable with member __.Dispose() = () }

    let rec poll () =
        async {
            do! Async.Sleep(int interval.TotalMilliseconds)
            Logger.Debug.Invoke("Polling metrics...")
            pollMetrics()
            return! poll()
        }

    do
        Async.Start(poll(), cancellationToken)

    new (registry) = new MetricMonitorRegistryPoller(registry, TimeSpan.FromMinutes(float 1))
    new (registry, interval) = new MetricMonitorRegistryPoller(registry, interval, false)

    /// Event raised when metric have been polled.
    [<CLIEvent>]
    member __.MetricsPolled = metricsPolled.Publish

    /// Force poll the monitors from the registry
    member __.PollMetrics() = pollMetrics()
    
    /// Stop polling for new metrics
    member __.Stop() =
        cancellationTokenSource.Cancel()
        onExitSubscriber.Dispose()

    /// Disposes the poller, stopping metrics collection
    member self.Dispose() = self.Stop()

    interface IMetricPoller with
        [<CLIEvent>]
        member self.MetricsPolled = self.MetricsPolled
        member self.Stop() = self.Stop()
        member self.Dispose() = self.Dispose()
        member self.PollMetrics() = self.PollMetrics()
