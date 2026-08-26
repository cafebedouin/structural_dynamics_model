# Is kimi a temperature outlier, and does the floors table pool two sampling regimes?

**Executed:** 2026-08-26. Prompted by the k2.6 finding that only `temperature: 1` is accepted:
*if other drivers run lower, kimi samples at a different temperature from every other leg, and its
within-model redraw floor is not on a common scale with the other pure pairs.*

## The specific worry dissolves: kimi is not in the floors table at all

`testsets_kimi` / `testsets_kimi2` is classified **`confounded:prompt`**, not `pure`. The five
pure pairs are flash2/flash3, flash_think/flash_think2, haiku2/haiku3, sonnet2/sonnet3,
stealth2/stealth3. **kimi contributes nothing to `within_pure_max` or `within_pure_median`**, so
there is no kimi floor to be off-scale.

## But a real version survives, and the outlier is Flash, not kimi

Temperature actually sent, read from the provenance stamps rather than the source:

| legs | temperature |
|---|---|
| flash, flash2, flash3, flash_think, flash_think2 | **0.1, explicitly set** |
| haiku ×3, sonnet ×3 | `api_default` |
| kimi ×2, nemotron ×2, stealth ×3 | `model_default` (k2.6's is 1 — the 400 says "only 1 is allowed") |

**Two of the five pure pairs are Flash at temperature 0.1**, the other three at vendor defaults.
The floors table pools two sampling regimes and does not say so. That is a real documentation gap
independent of whether it biases anything.

## Measured: no detectable systematic bias, and the test is WEAK

Per-pair spread on six statistics, grouped by regime:

| statistic | mean, 2 temp-0.1 pairs | mean, 3 default-temp pairs | which is tighter |
|---|---|---|---|
| `drift_events_per_story.warning` | 0.0060 | 0.0095 | low-T |
| `purity.coverage` | 0.0091 | 0.0050 | default-T |
| `network.drifting_share` | 0.0074 | 0.0124 | low-T |
| `coupling.strongly_coupled` | 0.0090 | 0.0069 | default-T |
| `type.tangled_rope` | 0.0142 | 0.0120 | default-T |
| `network.severe_share` | 0.0106 | 0.0161 | low-T |

**3 of 6 each way — no direction.** And `within_pure_max`, the quantity the borrow argument
actually uses, is set by **haiku2/haiku3 (a default-temperature pair) on 4 of 6** statistics, by
flash_think/flash_think2 on the other 2. So the max is not systematically set by the
low-temperature legs.

**Corroborating, from a different direction:** OQ-343's per-leg |Δε| ≥0.10 instability is 17% for
kimi (temperature 1) and 22% for thinking-on Flash (temperature 0.1). The *lower*-temperature leg
is the *more* unstable one, which is the opposite of what a temperature-driven story predicts.

**Declared: this is a 2-vs-3 comparison over five pairs and is very low-powered.** It cannot
exclude a modest effect. The honest statement is *no detectable systematic bias*, **not** *no
bias* — an untested-instrument zero would be the wrong reading, but so would treating a null at
n=5 as a clean absence.

## Disposition

- **Nothing to exclude from the borrowed floor** — the thing the worry named (kimi) is not in it.
- **A note is owed on the floors table**, since "5 pure pairs" reads as 5 draws from one
  population and they are a mixture of two sampling regimes.
- **OQ-346's revival condition** updated: the borrowed floor is a temperature MIXTURE, not a kimi
  contamination, and the mixture is unbiased as far as a weak test can tell.
