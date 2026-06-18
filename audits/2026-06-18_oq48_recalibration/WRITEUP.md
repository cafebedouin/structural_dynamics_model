# OQ-48 — Recalibration-readiness audit against the twin corpora

**Date:** 2026-06-18 · **Scope:** Audit + proposal only (no `config.pl` edit) · **Verdict: no
threshold is safely recalibratable against the twins.** All seven in-scope calibrated thresholds
return **MODEL-CONFOUNDED**; **zero proposed values**. OQ-48 stays **open**.

---

## 1. Question & honesty constraints

The χ / ε / suppression classification cuts in `config.pl` are stamped *"Calibrated: Derived from
691-constraint corpus (2024–2026)"* (`logic_thresholds.md:15`). That 691-corpus predates the
2026-06-05 reset; the cuts have **never** been recalibrated against the rebuilt corpus. The live
`testsets/` (80 readings) is far below the recalibration bar, so this audit measures the matched
**twin corpora** `testsets_haiku` (960) and `testsets_flash` (960) — exact base-name mirrors, a
paired design (OQ-49).

Carried-in constraints, all honored:
- **Not supervised error-minimization.** The LLM-authored twins carry no external ground-truth
  type labels and the original tool `power_modifier_calibration.py` is absent. This is
  **distribution-break recalibration**: find where each metric distribution actually cleaves and
  ask whether the 691-era cut still sits in that gap or has drifted into mass.
- **No curve-fitting to OQ-37 holdouts.** Holdouts are test cases, never the driver of a cut.
- **Cross-twin agreement is the validity gate.** A break that differs between haiku and flash is
  model-confounded (OQ-26/OQ-78 generation stochasticity); only breaks agreeing across **both**
  twins are recalibration candidates.

All verdict criteria were **pinned before the run** (plan `do-oq-48-tranquil-dusk.md`); the verdict
is the output, not a prediction.

## 2. Method (as pinned)

Per reading at the default analytical context, on each twin (one swipl process per twin):
ε = `drl_core:base_extractiveness/2`, Supp = `drl_core:suppression_score/2`,
TR = `drl_core:effective_theater_ratio/3`, χ = `constraint_indexing:extractiveness_for_agent/3`
(the canonical sigmoid path the gates use), plus `metric_based_type_indexed/3` and `dr_type/3`.

**Validated break** = a Gaussian-KDE antimode that satisfies *all* of: bins on both sides ≥ 5
readings; **bandwidth robustness** (same antimode within |Δloc| ≤ 0.02 under Scott's-rule
bandwidths `h`, `0.8h`, `1.2h`); **lobe-mass ratio** (larger side ≤ 4× smaller); **Hartigan's Dip
test** rejects unimodality at α = 0.05.

**Per-threshold verdict:** ROBUST = cut within a validated trough on **both** twins; DRIFTED =
cut in mass on both + both twins show a validated break at a consistent alternate location
(|Δ| ≤ 0.05) + bootstrap-stable (SD ≤ 0.03 over 20 resamples, n = 600) + survives twin-swap
falsification; otherwise MODEL-CONFOUNDED. Cross-metric Spearman ρ vs χ flags POSSIBLY-INDUCED
(non-decisive).

**Two documented deviations** (flagged, not silent walk-backs):
1. χ is unbounded above 1 (χ = ε·f(d)·σ(S); observed up to ~1.23), so its KDE/histogram run over
   the **observed range**, not clipped to [0,1] — clipping would pile >1 mass into the boundary
   bin and manufacture a false antimode there. The in-scope χ cuts (0.35, 0.66) are interior and
   unaffected.
2. Hartigan's Dip test is a whole-distribution property, computed once per metric/twin as that
   metric's multimodality gate. **`diptest` IS installed** in this environment — no substitution
   was needed (the plan's fallback clause did not apply).

## 3. Positive controls (all PASS)

| Control | Result |
|---|---|
| Probe LOADCOUNT, haiku | **960** (hard-stop armed at ≠ 960; `asserta` overlay) |
| Probe LOADCOUNT, flash | **960** |
| ROW count per twin | exactly 960; **0 unknowns** in any metric column |
| Probe reproducibility | re-run **byte-identical** (haiku sha256 `7039d37b…`, flash `3c24b1d2…`) |
| Break-finder planted-gap | synthetic N(0.25,0.05)+N(0.65,0.05), gap 0.45 → **validated break recovered at 0.4506** (depth 2.77, Dip p = 0.0) |

The break-finder recovering the planted gap is the precondition for treating any "no validated
break" on real data as a finding rather than a dead probe.

## 4. Distribution evidence

Every metric is multimodal on both twins (Dip p = 0.0000 throughout). The candidate antimodes and
which survived the full validation triple:

| metric | twin | candidate antimodes | **validated** | rejection cause for non-validated |
|---|---|---|---|---|
| ε | haiku | 0.203, **0.484** | **0.484** | 0.203: bw-fail + mass-fail (L=81/R=879) |
| ε | flash | 0.289, 0.504, 0.774 | *(none)* | all three **bandwidth-fail** |
| Supp | haiku | 0.301, 0.508 | *(none)* | 0.301 mass-fail; 0.508 **bandwidth-fail** |
| Supp | flash | **0.490** | **0.490** | — |
| χ | haiku | 0.273, **0.666** | **0.666** | 0.273: bw-fail + mass-fail |
| χ | flash | 0.397, 0.689, 1.061 | *(none)* | all three **bandwidth-fail** |
| TR | haiku | **0.328** | **0.328** | — |
| TR | flash | 0.168, 0.311, 0.515 | *(none)* | all three **bandwidth-fail** |

**The dominant pattern: the two models place the cleavage in roughly the same location, but the
flash corpus's antimodes are not bandwidth-robust** — they shift > 0.02 under ±20% smoothing and
wash out, failing the pinned validation triple. Compare ε (haiku 0.484 / flash 0.504), χ (haiku
0.666 / flash 0.689), Supp (haiku 0.508 / flash 0.490): the *locations* nearly coincide, but only
one twin's break validates. This is "soft agreement, hard disagreement," and the strict pinned
rule correctly treats it as not-safely-recalibratable. (Flash *can* validate — Supp 0.490 passes —
so the detector is not globally broken on flash.)

**Per-metric twin divergence** (narrative context for the MODEL-CONFOUNDED calls; 50-bin discrete
KL with Laplace smoothing):

| metric | KL(haiku‖flash) | KL(flash‖haiku) | mean haiku | mean flash |
|---|---|---|---|---|
| ε | 2.81 | 3.19 | 0.565 | 0.508 |
| Supp | 3.00 | 2.28 | 0.554 | 0.532 |
| TR | 2.97 | 3.33 | 0.375 | 0.186 |
| χ | 2.79 | 3.11 | 0.766 | 0.696 |

Divergences of 2.3–3.3 nats confirm the twins' per-metric distributions differ materially; TR
differs most in mean (flash authors far lower theater ratios).

## 5. Verdict table

| threshold | metric | cut | verdict | proposed | note |
|---|---|---|---|---|---|
| `mountain_extractiveness_max` | ε | 0.25 | MODEL-CONFOUNDED | — | cut in mass; flash has no validated break |
| `snare_epsilon_floor` | ε | 0.46 | MODEL-CONFOUNDED | — | **in haiku trough 0.484**, in mass on flash |
| `tangled_rope_epsilon_floor` | ε | 0.30 | MODEL-CONFOUNDED | — | cut in mass; flash no validated break |
| `rope_chi_ceiling` | χ | 0.35 | MODEL-CONFOUNDED | — | cut in mass; flash no validated break |
| `snare_chi_floor` | χ | 0.66 | MODEL-CONFOUNDED | — | **in haiku trough 0.666**, in mass on flash |
| `snare_suppression_floor` | Supp | 0.60 | MODEL-CONFOUNDED | — | cut in mass; only flash has a validated break (0.490) |
| `tangled_rope_suppression_floor` | Supp | 0.40 | MODEL-CONFOUNDED | — | cut in mass on both |

`verdict_table.csv` / `threshold_evidence.json` carry the machine-readable form. No DRIFTED
candidate arose, so the twin-swap falsification and the POSSIBLY-INDUCED cross-metric flag were
**not exercised** (no false-positive to filter).

**Two cuts are corroborated by haiku.** `snare_chi_floor` (0.66) sits within 0.006 of haiku's
validated break 0.666, and `snare_epsilon_floor` (0.46) sits within 0.024 of haiku's validated
break 0.484. On the haiku twin alone these 691-era cuts are **not drifted** — they sit on a real
distributional cleavage. They fail the cross-twin gate only because flash's nearby antimode does
not validate. This is the opposite of "the old cuts are stale"; it is "the old cuts are confirmable
on one model's corpus but not the other's."

## 6. Holdout consequence-check (step 4)

Vacuous: with **zero DRIFTED thresholds there are no proposed values to apply**, so nothing can be
done to the four OQ-37 readings. (All four readings *are* present in both twins —
`republican_reading`, `living_constitutionalist_reading`, `diversity_reading`,
`competence_reading` — but the check cannot run without a proposal, and per the pinned rule it
could not have altered the verdict regardless.)

## 7. What stays 691-provenanced

**All seven** in-scope thresholds remain 691-corpus-provenanced. None was recalibrated; none could
be, under the cross-twin validity gate. The audit's positive contribution is the **negative
result**: the twin corpora do **not** license a recalibration, and the reason is specific and
documented (flash antimodes are not bandwidth-robust where their locations track haiku's).

## 8. Provenance stamps

- **Twin content hashes (the reproducibility anchor):** haiku TSV sha256
  `7039d37b09edc210dc9d22708fe33b95d43dc6447410d98491aef64a147a4d9a`; flash TSV sha256
  `3c24b1d2b877887f1c8a2f1779f2059e08606a9043901f20807634c4c2d70879`.
  (The loaded-id-set hash is identical across twins — `902e4e40…` — because the base names are
  mirrored; the **content** hash above is the meaningful anchor.)
- **Metric-code commit:** `0a629077` (last commit touching `drl_core.pl` / `config.pl`; verify
  with `git log -1 -- prolog/drl_core.pl prolog/config.pl`).
- **Corpus:** twins at 960 readings each (run 2026-06-18). RNG seed 20260618 (bootstrap +
  positive control + twin-swap).

## 9. Artifacts

Scripts (per audit convention, in `python/audits/`):
- `python/audits/oq48_threshold_distributions.py` — read-only probe (one swipl process per twin).
- `python/audits/oq48_analyze.py` — break detection + verdict rule.

Evidence (this directory):
- `rows_testsets_haiku.tsv`, `rows_testsets_flash.tsv` — raw per-reading metric dumps.
- `corpus_hash_*.txt` — loaded-id-set hashes.
- `verdict_table.csv`, `threshold_evidence.json` — machine-readable verdicts + raw evidence.

## 10. Graduation step (keeps OQ-48 open)

No floor moved → **do not mark mitigated/resolved.** The remaining graduation step is unchanged in
kind but now has an empirical floor under it: a recalibration becomes possible only when either
(a) a third independently-generated corpus breaks the haiku/flash tie at a threshold, or (b) the
live `testsets/` rebuild itself reaches the recalibration bar and supplies a single-corpus break.
Until then every in-scope cut is correctly left at its 691-era value.
