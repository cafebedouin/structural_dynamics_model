# OQ-58 — Cross-corpus dangling-`cs_reading_relation` incompleteness census

**Date:** 2026-06-20  **Type:** characterization (in-seat: measurement)  **OQ:** OQ-58 (Ω_P, Priority 1)

OQ-58 = dangling `cs_reading_relation` targets: committer-axis edges whose target names a sibling
reading that does not exist on disk. Disposition policy was ruled 2026-06-02 (canonical → attach,
else → quarantine; no auto-rewrite / no plausible-form tier) and the mechanism is built. The
2026-06-05 corpus reset stale-ified every count in the ISSUES.md record. This audit re-measures the
live state, broadens to the twin + archived corpora, and **right-sizes the remaining work** so the
deferred backlog is honest.

All numbers below are reproduced by `census_driver.py` (read-only; output saved to
`census_output.txt`) and `git_trajectory.txt`. The driver runs the pure, no-engine functions of
`python/audits/reading_reference_linter.py`; its positive controls (R1 ghost / R2 short /
R3 gradated-graduated, no over-flag) **PASS** at the top of `census_output.txt`.

---

## 1. Cross-corpus census (witnessed)

`rate%` = distinct-missing readings / total `cs_reading_relation` edges (cs_rr). `cs_rr = 0` ⇒ the
corpus authors no committer edges (observer-only generation regime — Pattern-5 authored-absence, not
a clean bill).

| corpus | files | cs_rr | dangl | miss | id≥2 | r/kern | rate% |
|---|---|---|---|---|---|---|---|
| LIVE testsets | 92 | 169 | 163 | 158 | 5 | 1.03 | **93.5** |
| testsets_haiku | 960 | 2004 | 127 | 75 | 39 | 2.90 | **3.7** |
| testsets_flash | 960 | 2008 | 101 | 47 | 41 | 2.90 | **2.3** |
| arch kernel_v1 | 1106 | 1774 | 94 | 86 | 8 | 2.13 | **4.8** |
| arch kernel_test | 229 | 210 | 57 | 57 | 0 | 1.00 | 27.1 |
| arch kernel_v2_test | 100 | 12 | 12 | 12 | 0 | 1.00 | 100.0 |
| arch original_v5 | 702 | 0 | — | — | — | 1.00 | authored-absent |
| arch original_v6 | 3380 | 0 | — | — | — | 1.00 | authored-absent |
| arch sotu | 0 | 0 | — | — | — | — | empty glob |

## 2. Why LIVE is 93.5% — sparsity, not a probe artifact or gate trip

The live corpus is **97% singleton kernels**: 89 kernels across 92 files → **1.03 readings/kernel**.
Each lone reading authors `cs_reading_relation` edges to its 2–5 *declared* siblings that were never
generated, so nearly every edge dangles. Mature corpora (haiku/flash) sit at **2.90 r/kern** (≈1%
singleton) → ~2–4% dangling. The rate tracks kernel sparsity — reported as "tracks sparsity"
(a two-point fit 1.03→93.5% and 2.90→~3% plus the mechanism), **not** a fitted curve.

**Concrete per-edge witness** (`census_output.txt`, last block):
`jewish_sovereignty_palestine__cultural_zionist_reading` authors 4 `coexists_with` edges —
`settler_colonial_reading` resolves (exact, on disk); `liberal_nationalist`, `religious_zionist`,
`post_zionist` dangle (absent on disk). **1 resolves, 3 genuinely absent.**

The linter is a pure read-only `*.pl` glob + set-membership scan — no engine, no gate; its positive
controls pass and the independent r/kern count corroborates.

## 3. "Did something change?" — YES, a regime swap (witnessed from git, corrected)

> **Correction to the planning note.** The plan stated commit `0ccc03cf` "reconciled a 933-file
> corpus *into* `testsets/`, which was then cleared." The git record (`git_trajectory.txt`) shows
> the opposite direction and a different peak. Recorded here from the substrate, not the doc.

The witnessed `testsets/` `.pl`-count trajectory since the 2026-06-05 reset:

- Post-reset it ran as a **small topical working set** (20 → 91 → 102 → 111), was archived to 0
  (06-08), and rebuilt to ~69 via topic runs by 06-11; a COHORT-ZERO swap dropped it to 12 (06-12).
- **06-13 rebuild pilots** then BUILT `testsets/` up through pilot_01…08:
  42 → 193 → 343 → 493 → 639 → 788 → 940 → **1000** files. At peak (pilot_08), `testsets/` held the
  reconciled multi-reading corpus at **343 kernels, r/kern 2.92, ~3% dangling** — OQ-58 effectively
  at steady-state.
- Commit **`0ccc03cf`** ("Gemini Flash twin corpus + reconcile into testsets_haiku/ testsets_flash/
  testsets/") then **reduced `testsets/` from 1000 → 51**, moving the reconciled corpus **out** into
  the twins `testsets_haiku/` + `testsets_flash/`.
- Since then `testsets/` has grown back as a **singleton topical working set** (51 → 92 via CBDC,
  deepfake, polaris, signal-ecology, undisciplinary topic runs).

**The reconciled corpus is byte-intact in the twins:** haiku=960 / flash=960 today, identical to
their counts at `0ccc03cf`. So the 93.5% is NOT mid-convergence — the reconciled ~3% corpus already
existed and is preserved; the live dir is a *different, singleton* regime.

**What the swap settles regardless of the policy ruling (§6):** the live corpus is structurally not
on a time-based path to steady-state (it is singleton ingestion, not multi-reading kernel
completion), so the **linter-as-instrument — not waiting for convergence — is the only signal.**

## 4. GAP-07 / bounded-attractor answer (split by the two distinct invariants)

- **Dangling RATE is bounded ~2–5% across independent lineages** (haiku 3.7, flash 2.3,
  kernel_v1 4.8) — a cross-lineage invariant. Absolute counts (94–127) do **not** scale with
  700→3380 growth.
- **Defensible (id≥2) COUNT ~40 is reproducible within a lineage:** haiku 39 ≈ flash 41 (same kernel
  population, second-model twin); the **haiku ∩ flash id≥2 intersection = 39**. It is NOT universal:
  kernel_v1 = 8 (a different kernel population).
- ⇒ the dangling space is **bounded, not an open frontier**: rate bounded across lineages,
  defensible count reproducible within a lineage. The census right-sizes the work (158 panic → ~40
  real) without dissolving it.

> **Correction to the planning note.** The plan glossed the ~40 as "missing in haiku AND flash AND
> kernel_v1." The witnessed three-way intersection is **1**, not ~40 (kernel_v1 is a different
> kernel population — `census_output.txt` "Cross-lineage missing-set intersections"). The ~40 is the
> **haiku ∩ flash** intersection (39 at id≥2; 46 at any in-degree). The durable defensible set is
> twin-reproducible *within the mature lineage*, not a tri-lineage invariant.

## 5. Two generate-backlogs (recorded; generation deferred)

Generation is **deferred** per the strategy ruling, and deferred *independently* of the §6 identity
question. Two backlogs are recorded because the live corpus is an unstable referent:

1. **Durable defensible set = twin-reproducible id≥2 = 39 readings (haiku ∩ flash).** The holes that
   survive across the independent mature second-model build — the backlog the census actually proved
   real. It lives in the reconciled archive (the twins). (kernel_v1's 8 is a *different* population,
   recorded as such; the tri-lineage common core is 1.)
2. **Stream-relative set = live id≥2 = 5 readings / 3 kernels**, explicitly **stream-relative**:
   - `jewish_sovereignty_palestine`: `liberal_nationalist_reading`, `post_zionist_reading`,
     `religious_zionist_reading`
   - `press_reformation_causation`: `technological_determinism`
   - `zero_mathematical_status`: `number_reading`

   Five holes in a working set the session record shows can be cleared wholesale (as the pilots built
   the 1000-file corpus). For when kernel-completion is deliberately run on the live corpus.

The full live defensible list and the twin intersection are in `census_output.txt`.

## 6. ESCALATION — operator's to rule (narrowed by §3 evidence)

The reconciled 1000-file multi-reading corpus was moved out of `testsets/` into the twins at
`0ccc03cf`; `testsets/` is now a singleton topical ingestion stream. The factual half of the
plan's open question — *was the reconciled rebuild accidentally clobbered?* — is **falsified by
evidence**: the reconcile commit *preserved* it (twins byte-intact, §3). What remains genuinely
**operator-seated** is the forward policy, not a lost-work question:

> Is `testsets/` *intended* to be the active topical working set (twins = the reconciled
> multi-reading archive), or should the reconciled corpus be promoted back to "the live corpus"?

This determines what "the live corpus" means for OQ-58 and other corpus-relative OQs. The plan is
robust either way (instrument + characterize + the bound is already witnessed in the twins), so this
is **flagged, not blocking**. Surfaced in ISSUES.md OQ-58.

---

## Files / evidence in this directory
- `census_driver.py` — read-only cross-corpus driver (re-runnable)
- `census_output.txt` — saved driver output (the §1/§2/§4/§5 numbers + selftest PASS)
- `git_trajectory.txt` — the §3 `testsets/` count trajectory + twins-intact + peak r/kern witnesses
