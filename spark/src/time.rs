// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

use core::{
    sync::atomic::{AtomicU64, Ordering},
    time::Duration,
};

const MICROS_PER_SECOND: u64 = 1000000;

/// System Time Frequency
///
/// This value is the rate at which the `time` CSR is incremented.
static TIMEBASE_FREQUENCY: AtomicU64 = AtomicU64::new(0);

fn timebase_frequency() -> u64 {
    TIMEBASE_FREQUENCY.load(Ordering::Relaxed)
}

#[cfg(target_arch = "riscv64")]
fn get_monotonic_count() -> u64 {
    let count: u64;

    unsafe {
        asm!("rdtime {}", out(reg) count, options(nomem, nostack, preserves_flags));
    }

    count
}

#[derive(Clone, Copy)]
pub struct Instant(u64);

impl Instant {
    pub fn now() -> Instant {
        Self(get_monotonic_count())
    }

    pub fn duration_since(self, earlier: Instant) -> Duration {
        self.checked_duration_since(earlier).unwrap_or_default()
    }

    pub fn checked_duration_since(self, earlier: Instant) -> Option<Duration> {
        let freq = timebase_frequency();
        let ticks_per_micro = (freq + MICROS_PER_SECOND - 1) / MICROS_PER_SECOND;

        let diff = self.0.checked_sub(earlier.0)?;

        let secs = diff / freq;
        let rems = diff % freq;
        let nanos = (rems / ticks_per_micro) * 1000;

        Some(Duration::new(secs, nanos as u32))
    }
}

impl core::ops::Sub for Instant {
    type Output = Duration;

    fn sub(self, rhs: Self) -> Self::Output {
        self.duration_since(rhs)
    }
}

pub fn init(frequency: u64) {
    TIMEBASE_FREQUENCY.store(frequency, Ordering::Relaxed);
    log::info!("timebase frequency: {}", timebase_frequency());
}

#[derive(Clone, Copy)]
pub struct Timeout {
    start: Instant,
    duration: Duration,
}

impl Timeout {
    pub fn start(duration: Duration) -> Timeout {
        Timeout {
            start: Instant::now(),
            duration,
        }
    }

    pub fn expired(&self) -> bool {
        Instant::now() - self.start >= self.duration
    }
}
