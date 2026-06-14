# extraction_blindness is an existential-labeling artifact (2026-06-14)

**Question (advances OQ-129 OPEN-A):** is the `extraction_blindness` gap label a real
power-ordered finding, or an artifact of an existentially-quantified labeling clause?

**Finding:** substantially an existential-labeling artifact. `report_generator:label_gap/4`
(lines 275–279) fires `extraction_blindness` whenever *one* (extractive-typed seat at lower
power, functional-typed seat at higher power) pair exists — `member/member`, `De > Df`, then a
cut. It never checks the type/power relationship is monotone or even dominant. So the same
constraint's seats frequently co-license the **mirror** reading (an extractive seat at *higher*
power than a functional seat) at the same time, making the templated headline ("extractive at
lower-power seats but functional at higher-power seats — extraction masked by perspective") false
to its own data.

**Witness (`mirror_metric.txt`, this commit, re-run from `probe_mirror.pl`):**

| corpus | N | extraction_blindness | also mirror | % | avg distinct types |
|---|---|---|---|---|---|
| live `testsets` (positive control) | 57 | 20 | 16 | **80.0%** | 2.85 |
| `testsets_haiku` twin | 960 | 358 | 258 | **72.1%** | 2.73 |

The live 16/20 is the corpus-independent positive control: the same labeling logic, not a
haiku artifact. avg distinct types ≈ 2.7–2.9 of ~6 confirms the seats are non-monotone — the
headline asserts a 2-pole power ordering over a constraint that carries ~3 types.

**Method:** read-only swipl, the engine's own `detect_gap_pattern/2` / `seat_type_reading/3` /
`gap_extractive_type` / `gap_functional_type` (no reimplementation). Corpus selected by
`corpus_path` overlay via `asserta` (loader rule). Live positive control run *first* and matched
memo §8 before the twin numbers were trusted.

**What resolution changes:** gate the `extraction_blindness` label on a monotonicity/dominance
check (not bare existence), or demote the headline to "type-divergence across power, direction
non-monotone" when the mirror also holds. Until then, gap-omega `extraction_blindness` instances
must stay instances of the *open, contested* OQ-129 (OPEN-A) and must never be promoted to an
asserted per-record field (memo §1g: authority control checks token-resolution, not claim-truth —
a clean controlled value can be uniformly false-to-its-data).

**Files:** `probe_mirror.pl` (the probe), `mirror_metric.txt` (raw run output).
