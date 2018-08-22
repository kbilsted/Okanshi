namespace Okanshi

open System
open System.Collections.Generic
open Okanshi.Helpers

/// Monitor type that provides the current value, fx. the percentage of disk space used
type IGauge<'T> = 
    inherit IMonitor
    
    /// Sets the value
    abstract Set : 'T -> unit
    
    /// Resets the gauge
    abstract Reset : unit -> unit

/// A gauge implemenation that invokes a func to get the current value
type BasicGauge<'T>(config : MonitorConfig, getValue : Func<'T>) = 
    
    /// Gets the current value
    member __.GetValues(list : List<IMeasurement>) = list.Add(Measurement("value", getValue.Invoke()))
    member __.GetValuesAs(list : List<IMeasurement>, name : string) = list.Add(Measurement(name, getValue.Invoke()))
    
    /// Gets the monitor configuration
    member __.Config = config.WithTag(DataSourceType.Gauge)
    
    /// Gets the value and resets the monitor
    member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValues(list)
    
    interface IMonitor with
        member self.GetValues(list : List<IMeasurement>) = self.GetValues(list) 
        member self.Config = self.Config
        member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValuesAndReset(list)

/// Gauge that keeps track of the maximum value seen since the last reset. Updates should be
/// non-negative, the initial value is 0.
type MaxGauge(config : MonitorConfig) = 
    let value = new AtomicLong()
    
    let rec exchangeValue newValue = 
        let originalValue = value.Get()
        if originalValue < newValue then 
            let result = value.CompareAndSet(newValue, originalValue)
            if result <> originalValue then exchangeValue newValue
    
    /// Sets the value
    member __.Set(newValue) = exchangeValue newValue
    
    /// Gets the current value
    member __.GetValues(list : List<IMeasurement>) = list.Add(Measurement("value", value.Get()))
    member __.GetValueAs(list : List<IMeasurement>, name : string) = list.Add(Measurement(name, value.Get()))
    
    /// Gets the monitor configuration
    member __.Config = config.WithTag(DataSourceType.Gauge)
    
    /// Reset the gauge
    member __.Reset() = value.Set(0L)
    
    /// Gets the value and resets the monitor
    member __.GetValuesAndReset(list : List<IMeasurement>) =
        let value = value.GetAndSet(0L)
        list.Add(Measurement("value", value))

    interface IGauge<int64> with
        member self.Set(newValue) = self.Set(newValue)
        member self.GetValues(list : List<IMeasurement>) = self.GetValues(list)
        member self.Config = self.Config
        member self.Reset() = self.Reset()
        member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValuesAndReset(list)

/// Gauge that keeps track of the minimum value seen since the last reset. Updates should be
/// non-negative, the initial value is 0.
type MinGauge(config : MonitorConfig) = 
    let value = new AtomicLong()
    
    let rec exchangeValue newValue = 
        let originalValue = value.Get()
        if originalValue = 0L || originalValue > newValue then 
            let result = value.CompareAndSet(newValue, originalValue)
            if result <> originalValue then exchangeValue newValue
    
    /// Sets the value
    member __.Set(newValue) = exchangeValue newValue
    
    /// Gets the current value
    member __.GetValues(list : List<IMeasurement>) = list.Add(Measurement("value", value.Get()))
    member __.GetValueAs(list : List<IMeasurement>, name : string) = list.Add(Measurement(name, value.Get()))
    
    /// Gets the monitor configuration
    member __.Config = config.WithTag(DataSourceType.Gauge)
    
    /// Reset the gauge
    member __.Reset() = value.Set(0L)
    
    /// Gets the value and resets the monitor
    member __.GetValuesAndReset(list : List<IMeasurement>) = list.Add(Measurement("value", value.GetAndSet(0L)))
    
    interface IGauge<int64> with
        member self.Set(newValue) = self.Set(newValue)
        member self.GetValues(list : List<IMeasurement>) = self.GetValues(list)
        member self.Config = self.Config
        member self.Reset() = self.Reset()
        member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValuesAndReset(list)

/// A gauge the reports a long value
type LongGauge(config : MonitorConfig) = 
    let value = new AtomicLong()
    
    /// Sets the value
    member __.Set(newValue) = value.Set(newValue)
    
    /// Gets the current value
    member __.GetValues(list : List<IMeasurement>) = list.Add(Measurement("value", value.Get()))
    member __.GetValuesAs(list : List<IMeasurement>, name : string) = list.Add(Measurement(name, value.Get()))
    
    /// Gets the monitor configuration
    member __.Config = config.WithTag(DataSourceType.Gauge)
    
    /// Reset the gauge
    member __.Reset() = value.Set(0L)
    
    /// Gets the value and resets the monitor
    member __.GetValuesAndReset(list : List<IMeasurement>) = list.Add(Measurement("value", value.GetAndSet(0L)))
    
    interface IGauge<int64> with
        member self.Set(newValue) = self.Set(newValue)
        member self.GetValues(list : List<IMeasurement>) = self.GetValues(list)
        member self.Config = self.Config
        member self.Reset() = self.Reset()
        member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValuesAndReset(list)

/// A gauge that reports a double value
type DoubleGauge(config : MonitorConfig) = 
    let value = new AtomicDouble()
    
    /// Sets the value
    member __.Set(newValue) = value.Set(newValue)
    
    /// Gets the current value
    member __.GetValues(list : List<IMeasurement>) = list.Add(Measurement("value", value.Get()))
    member __.GetValuesAs(list : List<IMeasurement>, name : string) = list.Add(Measurement(name, value.Get()))
    
    /// Gets the monitor configuration
    member __.Config = config.WithTag(DataSourceType.Gauge)
    
    /// Reset the gauge
    member __.Reset() = value.Set(0.0)
    
    /// Gets the value and resets the monitor
    member __.GetValuesAndReset(list : List<IMeasurement>) =
        let value = value.GetAndSet(0.0)
        list.Add(Measurement("value", value))
    
    interface IGauge<double> with
        member self.Set(newValue) = self.Set(newValue)
        member self.GetValues(list : List<IMeasurement>) = self.GetValues(list)
        member self.Config = self.Config
        member self.Reset() = self.Reset()
        member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValuesAndReset(list)

/// A gauge that reports a decimal value
type DecimalGauge(config : MonitorConfig) = 
    let value = new AtomicDecimal()
    
    /// Sets the value
    member __.Set(newValue) = value.Set(newValue)
    
    /// Gets the current value
    member __.GetValues(list : List<IMeasurement>) = list.Add(Measurement("value", value.Get()))
    member __.GetValuesAs(list : List<IMeasurement>, name : string) = list.Add(Measurement(name, value.Get()))
    
    /// Gets the monitor configuration
    member __.Config = config.WithTag(DataSourceType.Gauge)
    
    /// Reset the gauge
    member __.Reset() = value.Set(0m)
    
    /// Gets the value and resets the monitor
    member __.GetValuesAndReset(list : List<IMeasurement>) = list.Add(Measurement("value", value.GetAndSet(0m)))
    
    interface IGauge<decimal> with
        member self.Set(newValue) = self.Set(newValue)
        member self.GetValues(list : List<IMeasurement>) = self.GetValues(list)
        member self.Config = self.Config
        member self.Reset() = self.Reset()
        member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValuesAndReset(list)

/// Gauge that keeps track of the average value since last reset. Initial value is 0.
type AverageGauge(config : MonitorConfig) = 
    let mutable value = 0.0
    let mutable count = 0L
    let syncRoot = new obj()
    
    let rec updateAverage v = 
        count <- count + 1L
        value <- ((value * ((count - 1L) |> float)) + v) / (count |> float)
    
    let getValue'(list : List<IMeasurement>) = list.Add(Measurement("value", value))

    let resetValue'() =
        count <- 0L
        value <- 0.0
    
    let getValueAndReset'(list : List<IMeasurement>) = 
        getValue'(list)
        resetValue'()
        ()

    /// Sets the value
    member __.Set(newValue) = lockWithArg syncRoot newValue updateAverage
    
    /// Gets the current value
    member __.GetValues(list : List<IMeasurement>) = Lock.lock syncRoot (fun () -> getValue'(list))
    member __.GetValueAs(list : List<IMeasurement>, name : string) = Lock.lock syncRoot (fun () -> list.Add(Measurement(name, value)))
    
    /// Gets the monitor configuration
    member __.Config = config.WithTag(DataSourceType.Gauge)
    
    /// Reset the gauge
    member __.Reset() = Lock.lock syncRoot resetValue'
    
    /// Gets the value and resets the monitor
    member __.GetValuesAndReset(list : List<IMeasurement>) : unit = Lock.lock syncRoot (fun () -> getValueAndReset'(list))
    
    interface IGauge<float> with
        member self.Set(newValue) = self.Set(newValue)
        member self.GetValues(list : List<IMeasurement>) = self.GetValues(list)
        member self.Config = self.Config
        member self.Reset() = self.Reset()
        member self.GetValuesAndReset(list : List<IMeasurement>) = self.GetValuesAndReset(list)
