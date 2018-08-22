namespace Okanshi

open System.Collections.Generic
open System.Diagnostics

/// Performance counter configuration
type PerformanceCounterConfig = 
    { /// The category
      Category : string
      /// The category
      Counter : string
      /// The category
      Instance : string }
    
    /// Create performance counter config without an instance
    static member Build(category, counter) = 
        { Category = category
          Counter = counter
          Instance = "" }
    
    /// Create performance counter config with an instance
    static member Build(category, counter, instance) = 
        { Category = category
          Counter = counter
          Instance = instance }

/// Used to monitor Windows performance counters. Be aware that some performance counters, requires multiple readings before
/// returning any value.
type PerformanceCounterMonitor(registry : IMonitorRegistry, monitorConfig : MonitorConfig, performanceCounterConfig) = 
    let performanceCounter = 
        new PerformanceCounter(performanceCounterConfig.Category, performanceCounterConfig.Counter, 
                               performanceCounterConfig.Instance, true)
    let gauge = new BasicGauge<_>(monitorConfig, fun () -> performanceCounter.NextValue())
    
    new(monitorConfig, performanceCounterConfig) = 
        PerformanceCounterMonitor(DefaultMonitorRegistry.Instance, monitorConfig, performanceCounterConfig)
    
    /// Gets the performance counter value
    member __.GetValues(list : List<IMeasurement>) = gauge.GetValues(list)
    
    /// Gets the monitor config
    member __.Config = gauge.Config

    /// Gets the value and resets the monitor
    member __.GetValuesAndReset(list : List<IMeasurement>) = gauge.GetValuesAndReset(list)
    
    interface IMonitor with
        member self.GetValues(list : List<IMeasurement>) = self.GetValues(list)
        member self.Config = self.Config
        member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValuesAndReset(list)
