#[derive(Clone)]
pub struct Rand {
    rng: fastrand::Rng,
}

impl Default for Rand {
    fn default() -> Self {
        Self::new()
    }
}

impl Rand {
    pub fn new() -> Self {
        Self {
            rng: fastrand::Rng::new(),
        }
    }

    pub fn seed(seed: u64) -> Self {
        let mut rng = fastrand::Rng::new();
        rng.seed(seed);
        Self { rng }
    }

    pub fn choice<'t, T>(&mut self, slice: &'t [T]) -> &'t T {
        &slice[self.below(slice.len())]
    }

    pub fn choice_low<'t, T>(&mut self, slice: &'t [T]) -> &'t T {
        &slice[self.below_low(slice.len())]
    }

    pub fn below(&mut self, num: usize) -> usize {
        self.rng.usize(0..num)
    }

    pub fn below_low(&mut self, num: usize) -> usize {
        self.below(num).min(self.below(num))
    }

    pub fn odds(&mut self, chance: usize, quantum: usize) -> bool {
        self.below(quantum) < chance
    }
}

pub trait SliceExt {
    type Output;
    fn choice(&self, rng: &mut Rand) -> &Self::Output;
    fn choice_low(&self, rng: &mut Rand) -> &Self::Output;
}

impl<T> SliceExt for [T] {
    type Output = T;

    fn choice(&self, rng: &mut Rand) -> &Self::Output {
        rng.choice(self)
    }

    fn choice_low(&self, rng: &mut Rand) -> &Self::Output {
        rng.choice_low(self)
    }
}
