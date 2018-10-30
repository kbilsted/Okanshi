using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace Okanshi.Sample
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var registry = DefaultMonitorRegistry.Instance;
            new MyObserver(new MetricMonitorRegistryPoller(registry, TimeSpan.FromSeconds(2), false));

            Console.WriteLine("Monitor key presses. Start typing to measure");
            while (true)
            {
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

                CreateCounter(measurementName, registry).Increment();
            }
        }

        private static CounterZeroFilter<long> CreateCounter(string measurementName, OkanshiMonitorRegistry registry)
        {
            var config = MonitorConfig.Build("Key press").WithTags(OkanshiMonitor.DefaultTags)
                .WithTags(new[] {new Tag("combination", measurementName)});
            var monitor = registry.GetOrAdd(config, x => new CounterZeroFilter<long>(new Counter(x)));
            return monitor;
        }

	    private static CounterZeroFilter<long> FilterCounter(string name, IEnumerable<Tag> tags)
	    {
		    var config = MonitorConfig.Build(name)
			    .WithTags(OkanshiMonitor.DefaultTags)
			    .WithTags(tags);
		    var registry = DefaultMonitorRegistry.Instance;
		    var monitor = registry.GetOrAdd(config, x => new CounterZeroFilter<long>(new Counter(x)));
		    return monitor;
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

            public void Dispose() { }
        }
    }
}
