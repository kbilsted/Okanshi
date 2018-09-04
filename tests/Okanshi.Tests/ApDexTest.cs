using System;
using System.Linq;
using FluentAssertions;
using Xunit;

namespace Okanshi.Test
{
	public class ApdexTest
	{
		private readonly ApdexTimer timer;

		public ApdexTest()
		{
			DefaultMonitorRegistry.Instance.Clear();
			timer = new ApdexTimer(MonitorConfig.Build("Test"), TimeSpan.FromSeconds(1));
		}

		[Fact]
		public void ctor_adds_threshold_as_tag_if_absent()
		{
			timer.Config.Tags.Single(x => x.Key == ApdexConstants.ThresholdKey);
		}

		[Fact]
		public void ctor_fails_when_threshold_tag_is_present()
		{
			var forbiddenTag = new Tag(ApdexConstants.ThresholdKey, "");
			var config = MonitorConfig.Build("test").WithTag(forbiddenTag);

			Action act = () => new ApdexTimer(config, TimeSpan.FromHours(2));
			act.ShouldThrow<ArgumentException>();
		}

		[Fact]
		public void AppdexCalc_zero_calls()
		{
			timer.GetApDex().Should().Be(-1);
		}

		[Fact]
		public void Getvalues_when_0_calls()
		{
			timer.GetValues().ShouldBeEquivalentTo(new IMeasurement[0]);
		}

		[Fact]
		public void Getvalues_when_1_call()
		{
			timer.Record(() => { });

			timer.GetValues().Count(x => x.Name == "apdex").Should().Be(1);
		}

		[Fact]
		public void Getvalues_when_1_fcall()
		{
			timer.Record(() => { return 1;});

			timer.GetValues().Count(x => x.Name == "apdex").Should().Be(1);
		}

		[Fact]
		public void AppdexCalc_satisfied_one_call()
		{
			timer.Register(TimeSpan.FromMilliseconds(900));

			timer.GetApDex().Should().Be(1.0);
		}

		[Fact]
		public void AppdexCalc_satisfied_many_calls()
		{
			timer.Register(TimeSpan.FromMilliseconds(500));
			timer.Register(TimeSpan.FromMilliseconds(600));
			timer.Register(TimeSpan.FromMilliseconds(700));
			timer.Register(TimeSpan.FromMilliseconds(900));

			timer.GetApDex().Should().Be(1.0);
		}

		[Fact]
		public void AppdexCalc_tolerable_one_call()
		{
			timer.Register(TimeSpan.FromMilliseconds(1200));

			timer.GetApDex().Should().Be(0.5);
		}

		[Fact]
		public void AppdexCalc_tolerable_many_calls()
		{
			timer.Register(TimeSpan.FromMilliseconds(1500));
			timer.Register(TimeSpan.FromMilliseconds(2600));
			timer.Register(TimeSpan.FromMilliseconds(3700));
			timer.Register(TimeSpan.FromMilliseconds(3900));

			timer.GetApDex().Should().Be(0.5);
		}

		[Fact]
		public void AppdexCalc_frustrated_one_call()
		{
			timer.Register(TimeSpan.FromMilliseconds(4000));

			timer.GetApDex().Should().Be(0);
		}

		[Fact]
		public void AppdexCalc_frustrated_many_calls()
		{
			timer.Register(TimeSpan.FromMilliseconds(4500));
			timer.Register(TimeSpan.FromMilliseconds(5600));
			timer.Register(TimeSpan.FromMilliseconds(6700));
			timer.Register(TimeSpan.FromMilliseconds(7900));

			timer.GetApDex().Should().Be(0);
		}

		[Fact]
		public void AppdexCalc_one_of_each_kind()
		{
			timer.Register(TimeSpan.FromMilliseconds(900));
			timer.Register(TimeSpan.FromMilliseconds(2000));
			timer.Register(TimeSpan.FromMilliseconds(4000));

			timer.GetApDex().Should().Be(0.5);
		}

		[Fact]
		public void AppdexCalc_rounded_to_2_decimals()
		{
			timer.Register(TimeSpan.FromMilliseconds(900));
			timer.Register(TimeSpan.FromMilliseconds(900));
			timer.Register(TimeSpan.FromMilliseconds(2000));
			timer.Register(TimeSpan.FromMilliseconds(4000));

			timer.GetApDex().Should().Be(0.63);
		}

		[Fact]
		public void Record_func_should_register()
		{
			var i = timer.Record(() => 1);

			i.Should().Be(1);
			timer.GetApDex().Should().Be(1);
		}

		[Fact]
		public void Record_action_should_register()
		{
			int i = 0;
			timer.Record(() => i = 1);

			i.Should().Be(1);
			timer.GetApDex().Should().Be(1);
		}
	}
}