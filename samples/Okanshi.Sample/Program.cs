using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace Okanshi.Sample
{
	public static class OkanshiIntegrator
	{
		public static MonitorFactory CreateFactory(TimeSpan pollFrequency, IEnumerable<Tag> defaultTags, bool pollOnExit = false)
		{
			defaultTags = defaultTags ?? new Tag[0];
			var registry = new OkanshiMonitorRegistry();
			new MyObserver(new MetricMonitorRegistryPoller(registry, pollFrequency, pollOnExit));
			var factory = new MonitorFactory(registry, defaultTags);
			return factory;
		}

		public static ZeroFilterFactory CreateZeroFactory(TimeSpan pollFrequency, IEnumerable<Tag> defaultTags, bool pollOnExit = false)
		{
			defaultTags = defaultTags ?? new Tag[0];
			var registry = new OkanshiMonitorRegistry();
			new MyObserver(new MetricMonitorRegistryPoller(registry, pollFrequency, pollOnExit));
			var factory = new ZeroFilterFactory(registry, defaultTags);
			return factory;
		}
	}

	class MyObserver : IMetricObserver
	{
		public MyObserver(IMetricPoller poller)
		{
			poller.RegisterObserver(Update);
		}

		public async Task Update(IEnumerable<Metric> metrics)
		{
			Console.WriteLine("***************************************************************");
			Console.WriteLine(JsonConvert.SerializeObject(metrics, Formatting.Indented));
			Console.WriteLine("***************************************************************\n\n");
		}

		public void Dispose()
		{ }
	}

	public class Program
	{
		public static MonitorFactory okanshiMonitor1h = OkanshiIntegrator.CreateFactory(
			TimeSpan.FromHours(1), 
			new[] { new Tag("Environment", "Production"), });

		public static ZeroFilterFactory okanshiMonitorZeroFilter2m = OkanshiIntegrator.CreateZeroFactory(
			TimeSpan.FromMinutes(2), 
			new[] { new Tag("Environment", "Production"), });

        public static void Main(string[] args)
		{
			Console.WriteLine("Monitor key presses. Start typing to measure");
			while (true)
			{
				okanshiMonitor1h.Counter("circles").Increment();
				var info = Console.ReadKey();
				string measurementName;
				if ((int) info.Modifiers == 0)
				{
					measurementName = info.Key.ToString();
				}
				else
				{
					measurementName = $"{info.Modifiers} + {info.Key}";
				}

				System.Console.WriteLine();

				okanshiMonitorZeroFilter2m.Counter("Key press", new[] {new Tag("combination", measurementName)}).Increment();
				//CreateCounter(measurementName, registry).Increment();
			}
		}
	}
}
