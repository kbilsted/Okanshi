using System;
using System.Linq;
using FluentAssertions;
using NSubstitute;
using Xunit;

namespace Okanshi.Test
{
    public class ApdexTestFromTimer
    {
        private readonly IStopwatch stopwatch = Substitute.For<IStopwatch>();
        private readonly ApdexTimer timer;

        public ApdexTestFromTimer()
        {
            DefaultMonitorRegistry.Instance.Clear();
            timer = new ApdexTimer(MonitorConfig.Build("Test"), () => stopwatch, TimeSpan.FromSeconds(1));
        }

        [Fact]
        public void Initial_count_value_is_zero()
        {
            var count = timer.GetCount();

            count.Value.Should().Be(0);
        }

        [Fact]
        public void Timing_a_call_sets_count()
        {
            timer.GetCount();

            timer.Record(() => { });

            timer.GetCount().Value.Should().Be(1);
        }

        [Fact]
        public void Get_and_reset_resets_count()
        {
            timer.GetCount();
            timer.Record(() => { });

            timer.GetValuesAndReset().ToList();

            timer.GetCount().Value.Should().Be(0);
        }

        [Fact]
        public void Manual_timing_sets_count()
        {
            timer.GetCount();
            var okanshiTimer = timer.Start();
            stopwatch.IsRunning.Returns(true);
            okanshiTimer.Stop();

            timer.GetCount().Value.Should().Be(1);
        }

        [Fact]
        public void Values_are_correct()
        {
            timer.Register(TimeSpan.Zero);

            var values = timer.GetValues().Select(x => x.Name);

            values.Should().BeEquivalentTo("value", "max", "min", "count", "totalTime", "apdex");
        }
    }
}