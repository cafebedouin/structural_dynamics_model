# FINDINGS — Twin conditioned re-analyses (OQ-125 resolved; OQ-123 mitigated)

**Date:** 2026-07-04. Pre-registration: `PRE_REGISTRATION.md` (committed `bc04d809`
BEFORE any run; five declared deviations from the plan doc, incl. the sharpened
HIGH⇒(a) comparative clause). Harness: `python/audits/twin_comparison.py` extended
additively (new `--conditioned-outdir/--source/--control` flags). Substrate: the
`bbf5c92` twin re-classification (haiku/flash, n=960 each, run 2026-06-27); input
sha256s stamped in `twin_conditioned.json`. All decisions below read literally off
the pre-registered rules.

**Scope headline (under-claim discipline):** every claim here is haiku-vs-flash,
this corpus, commit `bbf5c92` — "invariant/sensitive HERE," never "in general."

## Headline 1 — OQ-125 RESOLVED: value-invariance confirmed (4 typed χ seats)

The below-band |Δχ| tail is **real continuous value-invariance beyond H1**, not
threshold-colocation. Conditioned on same-side pairs (= same seat type, the frozen
operationalization), the observed mean|Δχ| stays BELOW the conditioned band5 at
**all four** typed seats — observed and band recomputed from the SAME same-side id
set (hashes stamped side-by-side in the output, the pre-registered make-or-break):

| seat | same-side n | observed | conditioned band5 | verdict |
|---|---|---|---|---|
| powerless | 378 (`fbb33698…`) | 0.0920 | 0.2605 | below ⇒ invariance beyond H1 |
| moderate | 566 (`4668fdbf…`) | 0.0754 | 0.2676 | below ⇒ invariance beyond H1 |
| institutional | 664 (`158e9890…`) | 0.0129 | 0.0188 | below ⇒ invariance beyond H1 |
| analytical | 546 (`c3bc2154…`) | 0.0879 | 0.3095 | below ⇒ invariance beyond H1 |

Positive control (reach): all four disagreeing-subset controls are INFORMATIVE —
the conditioned observed would register below-band inside every disagreeing-subset
band (band5s 0.2480/0.2663/0.2114/0.3337 all above the conditioned observeds), so
the control could have failed and did not pass by width. Unplanned corroborating
observation (report-only): at 3 of 4 seats the *disagreeing* pairs are themselves
below their own band (e.g. powerless 0.2298 < 0.2480) — value proximity survives
even type disagreement, the opposite of what colocation-only would produce.

`theater_ratio` has no seat type ⇒ remains EXPLORATORY/OPEN, outside the headline
(resolution is 4/5 fields, per the frozen scope rule).

## Headline 2 — OQ-123 MITIGATED: (a) refuted, (c1) present but not the driver; live remainder (b)-or-(c2)

**(a) imputation/omission-drag is REFUTED** (pre-registered PERSISTENT-DIVERGENCE
branch). The authored-both cell (n=805, POWERED: benchmark Wilson-lo 0.639 > cell
band95 0.288) shows powerless agreement **0.3925** vs unconditioned **0.3937**
(r_ab − r_all = **−0.0012**, far under the frozen +0.10 float margin). The seat's
weak agreement is a property of pairs where BOTH models authored a real victim —
imputation cannot be dragging it (imputed cells hold ≤5 of 960 pairs; recon had
already shown the plan's "imputed cells may swallow most powerless seats" premise
was arithmetically impossible: haiku 5, flash 0 imputed-eligible).

**Partition** (reconciles exactly against per-corpus marginals; row/col sums =
841/5/114 and 877/0/83): authored-both 805, imputed-one 5 (all haiku-omits),
imputed-both 0 (VACUOUS by construction — flash has zero imputation-eligible
stories), residual absent-involved cells 47+67+36.

**(c1) under-authoring exists directionally but does not carry the divergence.**
Secondary omission probe: haiku omits 72 vs flash 36 (n=108, larger-share Wilson-lo
0.573 > 0.5 ⇒ DIRECTIONAL (c1)-signature). Primary (imputed-one) probe: 5 vs 0,
REPORT-ONLY (n<10, frozen guard). But the divergence persists at full strength
inside authored-both, so (c1) is a real generation asymmetry riding alongside, not
the explanation. Sonnet control (ii) — non-blind, disclosed: **INDETERMINATE**, and
directionally ANTI-(c1): sonnet authors victims at 0.781 vs twins 0.876/0.914 —
the twins do NOT under-author relative to the third model.

**Seat gradient is robust:** bootstrap 95% CIs — powerless [0.365, 0.426] sits
wholly below moderate [0.559, 0.621], analytical [0.535, 0.599], institutional
[0.664, 0.721]. The gradient is the finding and it survives resampling.

**Live remainder:** inside authored-both, the powerless seat clears its chance band
(H1-consistent) but stays maximally model-sensitive — **(b) real model divergence
vs (c2) systematically ambiguous authored content**, not separable at the free
tier. **B4 (paired third-model twin) is NOT auto-armed** (frozen: only powered LOW
arms it; this landed PERSISTENT DIVERGENCE). B4 remains an operator-discretion
spend whose role would be the (b)/(c2) split — (c2) recovers authored-cell
agreement under a sonnet paired twin, (b) diverges.

## Witnesses (all pasted/committed in this dir)

1. **Harness regression:** pristine (git HEAD) vs extended harness on identical
   inputs+seed: `twin_comparison.json` and `RESULTS.md` **byte-identical**;
   diff instrument positive-controlled (detects the cross-substrate change vs the
   2026-06-13 file). Deviation from plan doc declared in pre-reg §Deviations-1:
   the 2026-06-13 baseline is at `8126231` on pre-OQ-138 outputs — cross-substrate,
   not a valid regression target.
2. **Unconditioned tables at `bbf5c92`:** `unconditioned_bbf5c92/` (this run's
   H1/H2 reference; the parent audit dir left untouched).
3. **Method witness (Track A):** same id-set hash stamped on observed AND band per
   seat (`twin_conditioned.json`).
4. **Partition reconciliation:** 47+67+36+805+5 = 960; haiku 805+36=841 authored,
   5 imputed, 47+67=114 absent; flash 67+805+5=877 authored, 0 imputed, 47+36=83
   absent — equals the recon marginals.
5. **Load-time spot-checks** (`spotcheck_haiku.log`, `spotcheck_sonnet.log`, both
   exit 0, full logs kept): haiku `usul_al_fiqh_method__hanafi_reading`
   `authored_victim` TRUE; haiku `catastrophe_proxy_sufficiency__simulation_
   fidelity_threshold` victim ABSENT pre-repair → bridge mints `inferred_subject`
   (E=0.62, S=0.48 — matches the Python gate replica) → `authored_victim` FALSE;
   sonnet `animal_status_kernel__property_reading` same shape (E=0.88, S=0.79).
   Flash-side imputed spot-check VACUOUS by construction (marginal 0, disclosed).
   The `d==0.90` gate was dropped at freeze (recon falsified the universal d-tell;
   named imputed id has d=0.95).

## Addendum (2026-07-04, post-review reconciliation)

1. **Disagreeing-below-band disposition: no committed falsifier fired.** The frozen
   control text (`PRE_REGISTRATION.md` §Track A, byte-unchanged since `bc04d809` —
   `git diff bc04d809 HEAD -- PRE_REGISTRATION.md` = empty) commits to *report
   n/band/width/observed* plus the REACH criterion only; "disagreeing pairs NOT
   below band" was never frozen as a falsifier (the plan doc itself had already
   demoted that verdict as trivially passable). The 3/4 below-band disagreeing
   subsets are therefore genuinely unplanned, correctly filed report-only.
2. **Strong-form (a)-kill (derivation from reported aggregates, no new witness).**
   Corpus agrees 378/960, authored-both 316/805 ⇒ the 155 non-authored-both pairs
   agree at 62/155 = **0.4000** vs authored-both **0.3925** — divergence is UNIFORM
   across cells, not compositional. This pins (a) harder than "imputed cells too
   small to drag": there is no depressed non-authored stratum at all.
3. **Institutional margin robustness (post-hoc envelope, not a decision change).**
   N=1000 permutations (stamped in the output). Probe on the harness code path
   reproduces the shipped numbers at the frozen seed (obs 0.0129, band5 0.0188,
   band95 0.0198); across 20 fresh seeds band5 ∈ [0.01883, 0.01891] — spread
   ~8×10⁻⁵, two orders below the 0.006 margin; observed < min(band5) at every seed.
4. **Stamp structure (make-or-break witness, stated precisely).** Each seat carries
   ONE `same_side` block holding {n, idset hash, observed, band5, band95} as sibling
   fields; observed and band are computed from the identical id list in a single
   `_delta_stats` call — there are no two hashes to diff because set divergence is
   structurally impossible at the JSON level (single-source, stronger than
   hash-equality over separately selected sets).
5. **Attribution note:** the DIRECTIONAL (c1) finding rides the deviation-4
   secondary probe (frozen pre-join); the plan-literal primary alone was
   report-only (n=5).

## Files

- `PRE_REGISTRATION.md` — frozen rules + declared deviations (commit `bc04d809`)
- `twin_conditioned.json` / `RESULTS_CONDITIONED.md` — conditioned run output
- `unconditioned_bbf5c92/` — unconditioned H1/H2 tables at the current substrate
- `spotcheck_haiku.log`, `spotcheck_sonnet.log` — swipl witnesses
