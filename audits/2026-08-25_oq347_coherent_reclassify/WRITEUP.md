# WRITEUP — OQ-342 step (3) / OQ-347 steps 2–4: the coherent 19-leg reclassify, the situation-fixed core, and OQ-348

**Executed:** 2026-08-25
**OQ:** OQ-342 step (3); OQ-347 steps 2–4; OQ-348; OQ-345 (shortfall recording)
**Verdict (one line, scoped):** the coherent reclassify at one clean HEAD (`a3966e7`) reproduces
the 15-commit dirty-tree artifacts' `per_constraint` **exactly** on all 17 unchanged legs; the
situation-fixed core is **6 of 871 (0.7%)**, stratum-stable; and OQ-348's permutation landed in
the pre-registered **UNNAMED cell** (ε itself beats the seed-permutation null on all 3
cross-model pairs) — that cell is a ruling for the operator, not resolved here.
**Manifest cite:** all 19 coherent outputs stamp `code_commit_short: a3966e7`,
`code_dirty: False`, `n_constraints` == on-disk count (table in audit_log.md / task
b9de5gvpq output); the superseded artifacts spanned 15 commits, 16/19 dirty (S4/S5).
**Fired:** live — the zero-diff half is pure confirmation, but OQ-348's founding contrast
FLIPPED: authored ε was presumed ≈ marginal chance across models (the "9%" read), and the
permutation shows same-seed ε agreement beats its null on every cross-model pair tested
(0.148 vs 0.067; 0.101 vs 0.023; 0.081 vs 0.024) — the legs share seed-keyed structure at the
INPUT, so the OQ's routing-vs-coarseness contrast does not exist as framed. (Also latent: the
S8 extension — 27 absent-ε records on the canonical artifact, OQ-377.)

## Numbered findings

1. **The dirty artifacts were fine (pre-registered zero-diff CONFIRMED).** Every unchanged leg's
   `per_constraint` is IDENTICAL old-vs-coherent (`n_only_old/n_only_new/n_changed` = 0/0/0 on
   all 17, `oq347_prereg_diff.py` output pasted in session). The 15-commit, 16-dirty artifact
   set was behaviorally identical to clean HEAD — S5/S6's warrant (only
   `giant_component_analysis.pl` differed, and `json_report.pl` does not load it) held, and no
   OQ-112-class non-determinism surfaced anywhere in ~18,800 records. **This result is
   INDEPENDENT of the OQ-348 reframe (findings 8–10): nothing in the null-specification question
   touches per_constraint identity. It stands unqualified.**
2. **The rescued legs changed exactly as pre-registered.** nemotron: nothing vanished, +4 new ids
   (== the driver's provenance-tagged count); nemotron_think: +2 (leg COMPLETE at 1005).
   Pre-existing-id changes confined to 14 corpus-relative keys (arakelov, maxent family,
   wasserstein family, contamination_network, drift_events, diagnostic_verdict,
   signature_pressure — the ensemble-refit channel). **verdict_join: 0 changes; h1_band,
   signature, claimed_type, base_extractiveness: 0 changes.** The OQ-98 shape held: the raw
   `diagnostic_verdict` input moved, the `verdict_join` headline did not. NOTE for the next
   reader: the plan's "n_changed == 0 outside verdict_join" phrasing was written against
   OQ-345's 6-field instrument; under whole-record comparison the ensemble-refit keys
   necessarily move when a corpus grows — the executed check blocked on the four structural
   fields (trigger 2's own list) and reported the refit keys, which is the check the plan's
   intent specifies.
3. **The situation-fixed core exists and is small: 6 of 871 (0.7%).** Seeds on which every one
   of 21 derived same-model pairs agrees on non-null h1_band ∧ verdict ∧ signature:
   `climate_response_imperative__adaptation_priority_reading`,
   `derivative_work_statutory_boundary__enclosure_reading`, `kodashim_corpus__study_as_exercise`,
   `nsl_legal_text__democratic_enclosure_reading`, `quran_hadith_substrate__state_hybrid`,
   `substance_control_legitimacy__prohibition_reading`. agreed-on-null: 0. Excluded: 134 ids
   (listed in `outputs/coherent_a3966e7/situation_fixed_core.json`) — the post-rescue rerun
   (OQ-378) compares against that list rather than recomputing blind. The strictness is real:
   the pair set includes cross-regime (think/no-think) and cross-schema (June-original) pairs.
4. **OQ-348 landed in the UNNAMED cell {h1/verdict beats null, ε beats null} — STOP-AND-ASK.**
   The within-model floors (known-positive arm) all beat the null decisively, so the instrument
   discriminates. On all 3 cross-model pairs, every field INCLUDING authored ε beats the
   seed-permutation null. Per the plan: the legs share seed-keyed structure at the input, so the
   engine-compresses-idiom contrast is unbuildable as framed. Not forced into reading A or B;
   the observed cell is recorded in OQ-348 and the reading is the operator's.
   Artifact: `outputs/coherent_a3966e7/permutation_null_oq348.json` (seed 348, 2000 iters).
5. **The split moves nothing (Phase 8).** Core rate 0.6–0.8% in every stratum with n≥30 (pooled
   0.7%); per-pair agreements within a few points across strata; the stealth2×nemotron
   permutation beats the null in both large nemotron strata separately. Tiny strata (n=2, 27)
   reported with n only. Pooled numbers are both artifact and result.
   Artifact: `outputs/coherent_a3966e7/stratum_split_report.txt`.
6. **Phase 1 sync rescues: 6 of 11 landed.** nemotron 996→1000 (5 declared short, ids +
   failure modes in `leg_shortfall.md`), nemotron_think 1003→1005 (COMPLETE). Tag
   `+seed_rescue1` read back on every landed story. The 257 batch-only seeds are OQ-378.
7. **S8 extension (latent, OQ-377):** absent ε = 0 on every model-leg artifact, but the CANONICAL
   `outputs/pipeline_output.json` carries 27 absent-ε records (raw twin 27, one dead `_head`
   file 11) — a live input for any of the 22 remaining coercion sites that reads the canonical.

## Findings 8–10 — the ruling round (operator, 2026-08-25, same day)

8. **The framing finding (its own altitude): OQ-348's null was MIS-SPECIFIED, not surprised.**
   A marginals-only seed-label permutation asks *is there any seed-keyed dependence* — an answer
   fixed before the run, since every leg is authored from the same 1005-seed pool. ε is a
   property of the authored text and covaries by seed BY CONSTRUCTION; it cannot be the
   negative-control arm the 2×2 was built on, so cell (beats, beats) is what the design should
   have predicted. Recorded in OQ-348 with the original hypothesis left intact; the re-specified
   test minted as child OQ-380. Corollary correction propagated (prior-art sweep, not a note):
   the "9% ε agreement ≈ chance" family — chance is ~2–7%, so 9% was ~4× chance. Corrected at
   KNOWN_STATE 2026-08-22 (stealth first-read line), OQ-343 ("only 12%"), and OQ-348's body.
9. **The re-specified test (OQ-380): the engine-side excess SURVIVES ε-conditioning.**
   Permutation within exact (ε_A, ε_B) strata (invariance control: ε observed == null exactly,
   asserted, passed on all 8 pairs; singleton freezing 5–40% per pair, conservative). All 3
   cross-model pairs still beat the conditioned null on h1_band (p=.006/.002/.028), verdict
   (p=.0005/.033/.0005) and signature (p=.002/.046/.0035) — but the gaps shrink from ~+15–20pp
   (marginals-only) to ~+1–3pp: MOST raw same-seed agreement is input passthrough, and a small
   consistent engine-side excess remains. Scoped: this conditions on ε ONLY — the excess is
   seed-keyed structure beyond exact-ε passthrough, an upper bound on engine amplification
   (other authored fields also covary by seed). Artifact:
   `outputs/coherent_a3966e7/permutation_null_oq348_eps_conditioned.json`.
10. **The core is real structure, not a marginals artifact.** Per-leg seed-label permutation
    (records shuffled as units, 2000 iters, seed 3471) yields a null core of **0 — max 0 across
    all 2000 iterations** vs observed 6 (p=0.0005; reproduction control: recomputed == recorded).
    Independent pairwise rates predict essentially zero joint survivors across 21 pairs, so the
    reportable number is the EXCESS (6 vs ~0), not the bare count. Artifact:
    `outputs/coherent_a3966e7/core_null_report.txt`; tool `python/audits/oq347_core_null.py`.

## Evidence map

| artifact | what it shows |
|---|---|
| `audit_log.md` | HEAD stamps (open `7fc4b8c59`, freeze `a3966e7c6`), sole-writer check, S1–S22 verification incl. S6 control PASSED |
| `preserved_arms.md` | four §9 arms + md5s; §9 pinned to `haiku`@`0f432fb` / `flash`@`2ce8e18` |
| `leg_shortfall.md` | rescue results, shortfall ids, OQ-58 sweeps (1 quarantined edge each), read-backs |
| `outputs/coherent_a3966e7/` | the 19 coherent outputs + `permutation_null_oq348.json` + `situation_fixed_core.json` + `stratum_split_report.txt` (gitignored; manifests cited above) |
| `python/audits/oq347_coherent_reclassify.py` | Phase-4 driver (its verify table = the coherence witness) |
| `python/audits/oq347_prereg_diff.py` | Phase-5 diff (strata + stop fields) |
| `python/audits/situation_fixed_core.py`, `permutation_null.py`, `oq347_stratum_split.py` | Phases 6–8 |

**Prior-art grep per finding:** zero-diff class — KNOWN_STATE 2026-08-23 TRIPWIRE 5 (the
unreconstructable-artifacts finding this closes); ε-agreement premise — OQ-348's own "9%" read
(2026-08-22, pre-permutation); core-absence — OQ-120 prereg
`audits/2026-08-21_oq120_epsilon_boundary/substrate_check.md:36` (population now exists).

**HEAD stamp pair:** open `7fc4b8c59e…` (dirty only at this executor's own RUNS.md append +
one leftover 2026-08-24 manifest, both accounted) → freeze/close `a3966e7c6…` (clean through
Phases 4–8; the close-out commit follows this writeup). The dirty→clean transition between
stamps is this session's own freeze commit, witnessed in audit_log.md.
