# OQ-87 twins characterization — FINDINGS

**Date:** 2026-07-03. **Code:** HEAD `dfe10734` (probes committed on top: `8ac24afc` PLAN,
`12b919bb` controls, `579c4089` raw rows). **Altitude:** CHARACTERIZATION on bait-bearing
substrate — never an existence proof; all rates are regime-bound to the bait-bearing generation
prompts (twins `22843cdf`, live leg `8080348c`). Pre-registration: `PLAN.md` (committed before
any run). Raw evidence: `arm_{testsets,kernel_v1,haiku,flash}.rows`, `analysis.out`,
`c5_attribution.txt`, `c1_kernel_fire.out`, `c23_testsets_controls.out`.

## Controls — all discharged (witness files in this dir)

| Control | Result | Witness |
|---|---|---|
| C1 kill: kernel_v1 fires at HEAD | **PASS** — 173 (story,atom) pairs over 129 stories; terminals on 722 | `c1_kernel_fire.out` |
| C2 kill: conjunction cell | **PASS** — hand-read `propagation_speed_asymmetry` (2× `empirically_contingent` + `gap(axiom_overriding,substantial,false)`; independent per-context read all-rope) landed `coherent|dead|fired` | `analysis.out`, `c23_testsets_controls.out` |
| C3 axis controls (testsets) | **PASS 3/3** — foreclosed/dead-via-drift/live all match hand reads; per-context dr_type consistent with H0 in all three | `c23_testsets_controls.out` |
| C4 overlay-took-effect | **PASS ×4** — loaded 119 / 1106 / 960 / 960 | `arm_*.rows` headers |
| C5 anchor + null-exclusion | **DISCHARGED** — see F2 | `analysis.out`, `c5_attribution.txt` |
| C6 bucket non-emptiness | **PASS ×4** — undetermined 7 / 10 / 77 / 91; coherent 24 / 136 / 192 / 228 | `analysis.out` |
| C7 join-rate before agreement | **PASS** — join 960/960 (twins share the seed pool; cid = kernel__reading) | `analysis.out` |

## Findings

**F1 — fired = grep-candidate on every corpus; the foreclosure conjunction is exact at HEAD.**
Engine-fired `cs_axiom_foreclosed` story-counts equal the fact-grep candidate counts on all four
arms: testsets 16/16, kernel_v1 129/129, haiku 136/136, flash 18/18. The banked grep-candidate
proxy was not a proxy at HEAD — the conjunction is file-locally decidable and fires wherever its
facts hold. (Corpus + code state: each pair measured on the named corpus at HEAD `dfe10734`.)

**F2 — Anchor reproduced; ALL deviation is observer-side; committer axis byte-stable across 26
days of engine evolution.** Banked diverge-A (2026-06-07) = 74/906. OQ-51 null-exclusion delta
= 0 (no banked diverge-A story is undetermined at HEAD) → reproduction target 74. Measured at
HEAD: **82** (+8 = 11 gained − 3 lost). Attribution (`c5_attribution.txt`): all 14 movers kept
`dead` on the committer axis and flipped only the observer bucket; corpus-wide,
**committer-verdict flips among the 906 = 0** while observer buckets changed on 42. The differ's
positive control is internal: the same code path that reads the committer column found the 42
observer changes, so the zero is measured-empty, not didn't-look. Characterization (not proof):
under a month of observer-engine changes (OQ-138 route conversion, OQ-27/OQ-195 era), the
committer verdict surface did not move — consistent with Theorem-7 detection independence, on
bait-bearing substrate.

**F3 — The magnitude convention is model-idiosyncratic, not shared: substantial-rate delta
LARGE.** Haiku 835/960 = 0.870 vs flash 485/960 = 0.505; |Δ| = 0.365 ≥ 0.10 (pre-registered
threshold). Consequence 5 (disarming) NOT activated — but the banked kernel_v1 story ("~89% one
drift convention") does NOT transfer to the twins unchanged: flash authors `stable` 280 gap-terms
vs haiku 37, `minor` 373 vs 56.

**F4 — Flash core = 18 < floor 20 → descriptive-only, the pre-registered modal outcome; the
evidence favors reading (a), convention-suppressed.** Flash authors the foreclosure-shaped
conjunction (`axiom_overriding` + non-minor + unack) on 26/960 = **0.027** of stories vs haiku
203/960 = 0.211; direction marginal `axiom_overriding` 34 vs 220. The starvation is upstream at
gap-authoring, not in grounding (flash's core still fires when the facts exist, F1). Reading (b)
(genuinely absent committer structure) remains undischargeable on this substrate — pre-registered.

**F5 — The foreclosure-shaped authoring rate clusters at ≈0.21 on all three Anthropic-era
corpora and collapses on the Gemini twin.** `ao+non-minor+unack` rate: testsets 0.213, kernel_v1
0.206, haiku 0.211, flash **0.027**. The convention rides the generating model (family), not the
topic mix — three corpora with disjoint topic sets and different eras land within 0.007 of each
other.

**F6 — The conditioned direction conjunct carries almost no cross-model content.** Over the 259
joined stories non-minor+unack in BOTH twins, direction agreement = 190/259 = 0.734 vs
base-rate chance 0.687 (+0.047). Cross-twin fired-core overlap: both = 9 (haiku 136, flash 18;
flash-side is descriptive-only per F4). Cross-twin diverge-A: both = 13 (haiku 110, flash 32).
On this substrate, model-shared foreclosure content is thin by every pre-registered read.

**F7 — Coverage healthy; undetermined is a real bucket.** Observer pools 883/960 (haiku),
869/960 (flash) ≥ 300 floor. Undetermined (H0=null): 77 haiku / 91 flash / 10 kernel_v1 /
7 testsets — the old 2-way probe would have silently misbucketed all of these as incoherent.

## Design parameters for the DRIFTNEUTRAL arm (the run's product)

Measured fired-core rates: **0.142** under haiku convention, **0.019** under flash convention
(9/960 = **0.009** model-shared). Sizing consequences, at characterization altitude:

- Single-model core of ≥20 stories: ~140 stories at haiku-like rates; **~1,050 at flash-like
  rates** — a DRIFTNEUTRAL build must budget for the flash-like floor, since removing the
  bait-bearing example is expected to LOWER foreclosure-shaped authoring, not raise it.
- A cross-model shared core of ≥20 at the observed 0.009 shared rate needs ~2,200 story-pairs;
  alternatively a single-model core plus the F2-style engine-stability argument.
- F6 says direction-conjunct agreement is near chance under bait: the DRIFTNEUTRAL arm is the
  only way to learn whether that is bait-suppression or genuine thinness.

## What this run does NOT establish

No existence proof (no bait-free substrate exists). No dominance or prevalence claims — every
rate above is an authoring-convention statistic of a named generation regime. Reading (b) of the
flash starvation (genuinely absent committer structure) is OPEN, typed Ω_E, witnessable only on
the drift-neutral substrate.
