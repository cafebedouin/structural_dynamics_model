# OQ-112 close-out — combined witness pass (Part A bite-check + Part B kill-conditions), declare-and-stop

**Date:** 2026-06-23. **Substrate:** `main` @ `5e5e2392`; live `testsets` pinned **LIVE=92**
(manifest `pipeline_run_at=2026-06-23T05:39:44Z code_commit_short=a5593f7 n_constraints=92`).
**Phase:** evidence-gathering + disposition; **no engine `.pl` edits** (no live biter found).
**Pre-registered ruling:** `BITE_RULING.md` (written before any probe ran).

## Headline

Under the pre-registered **field-level** bite-definition: **only item 1 touched live output on
the 92** (13/92 abductive signals `agrees`→`unavailable`, **headline-neutral**); items 2/4/7 are
**witnessed-latent** (void/sentinel/errored arms 0-firing). Part B finds items **3/5/6/8 do NOT
fire as live bites** on 92 — and the cross-corpus check shows this is **structural, not a
92-sparsity accident**: on both denser live twins (`testsets_haiku` 960, `testsets_flash` 960) the
same absence-gates stay masked. **No headline ever flipped on any live leg from any OQ-112 fix.**
Branch → **DECLARE-AND-STOP**; 3/5/6/8 filed witnessed-latent, fix-shapes recorded, not landed.

## Re-pin (prerequisite)

`repin_92.txt`: **LIVE=92**, membership **byte-identical** to `audits/2026-06-22_oq112_round1/pinned_corpus.txt`
(diff empty), 86 distinct `constraint_claim` subjects, 0 non-corpus demos. Negative control
(`repin_negctl.txt`): bad `corpus_path`→`corpus_empty` throw (asserted path honored);
`testsets_haiku` overlay→960 (overlay-took ≠ default-fallback). The 92 is a real load.

## Part A — retrospective bite-check on shipped fixes 1/2/4/7  (`partA_*.txt`)

**Bite = a consumer-read field whose value changed on the live 92 due to the fix (FIELD-level).**
**Positive control (`partA_items12.txt`):** forced an indexed maxent void on a baseline-**green**
constraint (`nicene_creed_...`) → `verdict_join` headline **green→yellow**, flagged by the
comparison. The headline-diff detector is LIVE, so "no headline bite" is falsifiable, not a
clean-grep.

| Fix | Field checked | On-92 result | Bite? |
|-----|---------------|--------------|-------|
| **1 (C4a `:198/:212/:163`)** | abductive signal in diagnostic summary | **13/92** flip `agrees`→`unavailable` (members = the 13 `has_abd=no`) | **YES — field-level, HEADLINE-NEUTRAL** (join verdict identical for all 92) |
| **2 (maxent completion gate)** | `verdict_join` Alerts (maxent_voided) | **0/92** void alerts under normal completed run (classical+indexed run_info present, N=86) | NO — latent (live-unexercised) |
| **4 (A3 maxent-local sentinel)** | maxent profile/LL values | **0** sentinels (all 86 claim constraints carry all metrics; else-branches unreached) | NO — latent. PC: theater-retract → `unknown` sentinel fires (detector live) |
| **7 (wasserstein `wm_token/wm_emit`)** | wasserstein cells in output | **344/344** wm_emit cells genuine float, **0** null/errored arms | NO — latent. (18 top-level nulls = the PRE-EXISTING outer else `:452-454`, transport-profile absent for the 6 claim-less constraints — NOT item-7-produced) |

## Part B — prospective kill-conditions on 3/5/6/8  (`partB*.txt`)

Four items, four distinct controls, no shared generalization. The v1 guard-predicate sweep
(`partB.txt`) over-counted (it counted guard firings, not consumed-output reachability); the v2
reachability pass (`partB_reach.txt`) + the json consumption check (`partB_item6_aggregates.txt`)
resolve each to **not a live bite**:

- **Item 3 (A6 absence-certifies-clean).** Guard fires for the 6 claim-less constraints
  (excess_extraction absent → `excess_extraction_subscore`=1.0), **but the value never reaches a
  consumer**: `purity_score/2` requires `epistemic_access_check(C,true)` first, which is **false**
  for all 6 → short-circuits to the `-1.0` sentinel → json emits `purity_score: null`,
  `purity_band: null`. No corpus-level purity/compliance aggregate counts them. Other subscore
  consumers: `genuine_findings_query` is **not in the pipeline**; `drl_boltzmann_analysis:purity_deficit`
  is not a per-constraint json field. **Live-fire: NO.** Control: a measured-excess constraint →
  EX<1.0 (probe distinguishes measured-clean from absence-clean).
- **Item 5 (C4b blind=stable).** The live corpus is **NOT synchronic** — 1569 `measurement/5`
  facts, 303 series. `metric_trend(base_extractiveness)` fires for 86 constraints (14 stable / 69
  increasing / 3 decreasing). **All 14 `stable` have a succeeding `metric_delta`** (a measured
  small-delta; `metric_delta` requires a non-empty series + T2>T1, so it FAILS on absence rather
  than reading stable). `logical_fingerprint` trend returns `unknown` on <2 points; `pattern_analysis`
  is the Pattern-6-fixed system_gradient (`open(Why)`). **Live-fire: NO blind=stable** (trends are
  measured; absence fails closed). Control: injected rising series → `increasing`.
- **Item 6 (A2 statistic-on-empty→0.0).** Canonical `system_gradient_for(_,_,[],R)` → `open(no_gradient_data)`
  (Pattern-6-fixed); 0 coercion intervals on the live corpus. The cohomology corpus-level
  empty-fallbacks (`grothendieck_cohomology.pl:347/:382`) don't fire (N=92>0). The only consumed
  per-constraint scalar holding `0.0`, `contextuality_fraction` (26 cases), is **measured H¹=0**
  (`constraint_contextuality = H1/6`), not empty-collapse. **Live-fire: NO consumed measured-flat-0.0
  from empty on 92.** Control: mean-of-[]→0.0 vs mean-of-[0.2,0.4,0.6]→0.4 (collapse shape real &
  detectable; the json-scan-then-adjudicate method flagged 26 zeros and each resolved to measured).
  **Coverage boundary (declared, not silent):** the canonical site + the consumed-json scalar
  surface + the cohomology empty-fallbacks were traced; the maxent/`trajectory_mining` internal A2
  sites run over the 86-claim / measured-series (non-empty) populations and are class-inferred
  latent — NOT individually traced.
- **Item 8 (low: C4c/A7/B2).** C4c: the 6 yield `pass(no_extraction_data)`, but
  `boltzmann_compliant` returns **`inconclusive(insufficient_classifications)`** for all 6 (masked
  upstream of the T1–T4 aggregation) — never a false-clean `compliant`. Control:
  `actinide_..._flat_control` → `compliant(0)` (real verdict; probe distinguishes). A7
  (zero-contamination-on-untyped) is flagged in the census as **needing a design ruling** (intended
  semantics vs absorption), not a clear defect. B2 (collectors over build-stage caches) is a defect
  only if the report path runs without the build step — it does not in `run_pipeline`. **Live-fire: NO.**

## Cross-corpus witness — "latent" is structural, not a 92-fact  (`crosscorpus_twin.txt`, `crosscorpus_flash.txt`)

The latent verdict rests on the 92's sparse constraints being pruned by **upstream**
epistemic/sufficiency guards before the Pattern-6 absence-gate can emit. To test whether that is a
92-sparsity accident, the item-3 (A6) and item-8 (C4c) gates were re-checked on **both denser live
twins** (overlay-took witnessed: 960 each, re-key risk 0):

| Corpus | epistemic=true | excess-absent AND epistemic=true (A6 bite) | compliant | compliant AND excess-absent (C4c bite) | measurement/5 |
|--------|---------------|--------------------------------------------|-----------|----------------------------------------|---------------|
| `testsets` (92) | — | **0** | — | **0** | 1569 |
| `testsets_haiku` (960) | 494 | **0** | 43 | **0** | 21165 |
| `testsets_flash` (960) | 748 | **0** | 129 | **0** | 13055 |

**On all three live legs, 0 constraints reach the A6/C4c absence-branch via a consumed path.** The
masking is **structural**: `epistemic_access_check` (and the compliance-sufficiency guard) require
the same metric family that `excess_extraction`/compliance need, so absence of the downstream
datum implies failure of the upstream gate. Same mechanism that makes items 2/4 latent (claim-less
maxent exclusion). **Declared scope boundary:** the **archives** (`kernel_v1`, `original_v6`, …) —
a different, pre-de-leak regime that authors `constraint_classification` facts and carries the
filename≠subject re-key tripwire — were **NOT** swept; they are retrospective-audit breadth
(OQ-89 pattern), out of the live-leg scope for this close. Re-runnable on request.

## Disposition (branch: remainder all-latent → DECLARE-AND-STOP)

**What the arc did:** hardened the codebase against absence-defects across items 1/2/4/7 — one
live-touching but **headline-neutral** (item 1: 13/92 spurious agreements removed), three
latent-hardened (2/4/7, forced-control-witnessed, live-unexercised). Items 3/5/6/8:
**witnessed-latent across all three live legs**, fix-shapes recorded, **declared-not-landed**
pre-rebuild.

**What it did NOT do:** catch a live, user-facing (headline) defect. Under an imminent rebuild,
that is a reasonable outcome to stop on, not a failure to paper over.

**Methodological yield (the genuine catch):** Round-3's read pass falsified 2 of 3 staged writes
before they touched the engine (most-vivid hazard = most-wrong) — already promoted to
`build_discipline.md`. This close adds a second: a guard-predicate count over-reports a Pattern-6
firing; **consumed-output reachability** (does the absence value survive the upstream epistemic
gate into a read field?) is the witness, and it must be checked on more than the sparsest corpus
(the cross-corpus pass is what turned "latent on 92" into "structurally latent").

**Fix-shapes recorded (for the declared-latent items, so each carries its remedy):**
- Item 3 (A6): fail-closed per OQ-44 statute at the 5 sites — return `unknown`/route-to-OPEN on
  absent input rather than the optimistic `1.0`/`0.5`. Latent because the epistemic gate masks all
  live inputs; revisit if a future schema decouples `epistemic_access_check` from the metric family.
- Item 5 (C4b): the live sites already fail-closed (`metric_delta` fails on absence;
  `logical_fingerprint`→`unknown`; `system_gradient`→`open`); residual is documentation that
  `stable` is measured-only. No code change warranted on current evidence.
- Item 6 (A2): carry COVERAGE/N to the read site; return `open`/`unknown` on empty, not `0.0` (the
  `system_gradient` template). Latent because every live A2 input is non-empty (N=92/960).
- Item 8 (C4c): consumers should match `pass(Reason)` not `pass(_)`; A7 needs a design ruling
  (`design_gaps.md`); B2 is benign in the pipeline order.

## Files

- `BITE_RULING.md` — pre-registered field-level bite-definition + branches (written before data).
- `repin_92.txt`, `repin_negctl.txt`, `probe_repin_92.pl`, `probe_repin_negctl.pl` — the pin.
- `partA_items12.txt` + `probe_partA_items12.pl` — items 1/2 + headline-flip control.
- `partA_item4.txt` + `probe_partA_item4.pl` — item-4 sentinel + control.
- `partA_item7.txt` — item-7 wm_emit vs pre-existing outer-else separation.
- `partB.txt`/`probe_partB.pl` (v1 guard sweep), `partB_reach.txt`/`probe_partB_reach.pl` (v2
  reachability), `partB_item6_aggregates.txt` — items 3/5/6/8 + four controls.
- `crosscorpus_twin.txt`/`crosscorpus_flash.txt` + `probe_crosscorpus_*.pl` — both live twins.
