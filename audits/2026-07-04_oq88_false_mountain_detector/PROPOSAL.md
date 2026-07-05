# OQ-88 PROPOSAL — false-mountain as kernel-false-negative detector (pre-registered)

**Date:** 2026-07-04. **Status:** FROZEN before any sweep run. Written after read-only recon
only; no counting, no partition, no classification run has executed at freeze time.

## Question

Is `D = flat-routed ∧ engine-false-mountain` a usable detector for kernel false negatives
(topics the kernel router flattened despite a live necessity-vs-contingency contest)?

**The load-bearing correction (operator, planning session):** D as written does not contain
the term that makes it a kernel signal. A pure authoring flinch (authored `mountain`, computed
`rope`, genuinely settled-as-contingent, no contest) routes flat *correctly* and still fires D.
The detector's validity is therefore an empirical wager: D holds iff flat-routed false-mountains
are dominated by suppressed contestable seats, not flinches. This pass is **Ω_E only** — collect
the evidence, return the pre-registered verdict cell. The Ω_P auto-route seat decision is the
operator's and is NOT resolved here.

## Detector predicate (pinned)

**Layer B — engine false-mountain (per-constraint, from a manifest-bearing pipeline output):**
`claimed_type == "mountain"` ∧ `maxent_top_type == "rope"` ∧ `verdict_join.alerts[]` contains
an alert with `type == "type_1_false_summit"`.

- **Severity is NOT part of the predicate** (recon amendment). In the live regime all nine
  mountain→rope firings are `informational` (the OQ-128 rope/snare severity split,
  `prolog/drl_core.pl` `dr_claim_mismatch/4`), but the archived World3 positive control
  (2026-06-11, pre-split) fires the same alert at `severe`. Pinning `informational` would
  make the positive control un-satisfiable by regime accident. The mountain→snare (severe)
  cases are excluded by the `maxent_top_type == "rope"` conjunct, not by severity.
- **Undetermined-seat bucket:** a mountain→rope entry with `h1_band == null` and no alert
  (recon: `organization_floor_c0`) is reported in its own bucket, folded in neither direction.

**Layer A — routing (from `outputs/kernel_manifests/**/*.manifest.json`):**

- **kernel-routed** ⟺ normalized id present in `build_constraint_map()`
  (`python/migrate_kernel_linkage.py:67` — contested manifests, dict axes with `claim_id`).
- **flat-routed** ⟺ normalized id appears as a plain-string entry in `generation_sequence`
  of a manifest whose `commitment_system_recognition.is_contested_kernel` is falsy.
- **routing-unknown** ⟺ appears in no manifest (pre-router or hand-authored). FAIL-CLOSED:
  never assumed flat; separate bucket.
- **routing-ambiguous** ⟺ normalized id appears in BOTH a flat and a contested manifest
  (stochastic multi-draw topics make this reachable). Reported explicitly, excluded from the
  flat candidate pool with a note — not silently resolved either way.
- **Join normalization:** pipeline ids may carry a compiler suffix (`demographic_skill_mismatch_c0`
  vs manifest `demographic_skill_mismatch`); both sides normalized by stripping `_c<digits>$`.

**D = Layer A (flat) ∧ Layer B.**

## Discriminator (captured ALONGSIDE D, never folded into it)

The candidate missing term: "a contestable regime seat is present." Two instruments:

1. **Regime-omega presence.** Sources (union): (a) `omegas[]` of the constraint's generating
   manifest (id + description text); (b) `omega_variable/3` facts in the constraint's testset
   `.pl` file (name + description) — the only omega source available for kernel_v1 (no
   manifests). Match is case-insensitive regex over id/name + description, two pre-registered
   tiers, each reported separately; "regime-omega-present" for D′ = Tier 1 ∨ Tier 2:
   - **Tier 1 (strict regime contest):**
     `paradigm|regime|belief[ _-]?system|worldview|ideolog|reorganiz|social[ _-]?order`
   - **Tier 2 (necessity-vs-contingency contest):**
     `natural|construct|contingen|inevitab|necessit|irreducib`
   - **Anchor-derivation caveat (pre-registered):** Tier 1 is derived from the World3
     183123-draw fingerprint (`omega_earth4all_paradigm_shift`,
     `omega_belief_system_change_mechanism`); Tier 2 from the necessity-vs-contingency seat
     framing (and recon confirmed the live positive's `natural_vs_constructed_avoidance`
     matches it). The discriminator is therefore IN-SAMPLE on both positives and
     out-of-sample only on the rest of the pool. D′ results are hypothesis-grade, not
     validated-gate-grade, regardless of cell outcome.
2. **Observer H¹ (`h1_band`)** — cross-observer type disagreement, recorded per candidate
   (null = UNDETERMINED per OQ-51, never coerced to 0).

**Pre-registered recovery hypothesis:** `D′ = D ∧ regime-omega-present`.

## The three pins (verbatim from the plan — each names a way the verdict could LOOK witnessed while being inferential)

- **Pin 1 — kernel_v1 is a base-rate measurement, NOT a negative control for D-over-firing.**
  A negative control for a flat-conjunctive predicate needs a labeled true-negative (one
  confirmed flinch: authored mountain, computed rope, flat-routed, genuinely no contestable
  seat). kernel_v1 has no manifests ⇒ Layer A is uncomputable ⇒ every kernel_v1 case is
  routing-unknown, which fail-closes and never assumes flat ⇒ kernel_v1 contributes ZERO to D.
  It measures the Layer-B false-mountain + regime-omega base rate (the flinch-tail density).
  Phase 3 may not quietly stand in for the missing negative control.
- **Pin 2 — positive-control N stated per layer; dispatch exercised on BOTH positives.**
  Full-D N=2: `demographic_skill_mismatch_c0` (live: china 163143 flat manifest ∧ live Layer B)
  and `collapse_mechanism_ambiguity` (World3: 06-08 171605 flat manifest ∧ 06-11 archived
  Layer B). The dispatch control must exercise the World3 join independently — a probe tuned
  to demographic's exact field layout passing PROBE-BROKEN would never prove general Layer-A
  dispatch.
- **Pin 3 — verdict cells 2 and 3 are base-rate-inferred, not witnessed, unless a flinch is
  labeled.** No labeled true-negative exists at freeze time. "Neg over-fires" (cell 2) and
  "D′ recovers" (cell 3) return as inferences from the regime-omega distribution.
  **Witnessability upgrade (offered, non-blocking):** the live flat candidate subset is
  surfaced to the operator with a request for ONE operator-labeled confirmed flinch; if
  supplied it becomes a real negative control (and must itself satisfy D — the
  positive-control-of-the-negative); if not, cells 2/3 ship labeled base-rate-inferred.
  **Cross-draw circularity, recorded:** the regime-omega discriminator was validated on the
  World3 183123 draw; the full-D positive is the 171605 draw (whose manifest omegas recon
  reads as epistemic/mechanism — `omega_regional_aggregation` mentions Earth4All but as
  aggregation-masking). D′-on-World3 is a cross-draw inference; D′ could be NEGATIVE on the
  actual full-D draw via the manifest source. The `.pl`-fact omega source for the World3 story
  (if recoverable from archives) is reported alongside.

## Phases

1. **Live routing partition FIRST** (before any counting): join the live Layer-B set against
   the manifest walk → {flat, kernel-routed (= caught, excluded), routing-unknown
   (fail-closed), routing-ambiguous}. Reconcile N against the live manifest denominator
   (`n_constraints`).
2. **Controls.** (a) Positive: both full-D positives satisfy D — else PROBE BROKEN, halt (fix
   the probe, never recalibrate around a missed positive). (b) Dispatch: reader resolves a
   known mountain→rope; join resolves BOTH known flat manifests incl. `_c0` normalization;
   a known kernel-routed live constraint resolves to the kernel bucket (two-sided). (c)
   Discriminator sanity: the World3 183123 regime-omega trace is capturable — logged as
   discriminator-is-representable, NOT D′-validated-on-World3.
3. **kernel_v1 flinch-tail base rate** (Pin 1). Fresh `classify_corpus('archives/datasets/kernel_v1',
   'pipeline_output_kernel_v1_oq88.json', None)` at current code state — the existing
   `pipeline_output_kernel_v1.json` (commit e8189d1, 2026-07-02) is NOT reused: output-changing
   engine commits landed since (OQ-138 `d248a6b1`/`82aa372e` route+maxent-on-lever; OQ-205
   ε fail-closed `60a69b53`/`e9041905`), and it is not overwritten (distinct output name).
   Measure Layer-B prevalence + per-tier regime-omega distribution over that set (omega source:
   `.pl` `omega_variable/3` facts). **Escalation rule:** extend to original_v5/v6 only if the
   non-regime-omega false-mountain tail is dense enough to warrant it.
4. **Three-cell verdict**, cells graded by evidence:
   `{positive fires: WITNESSED y/n} × {negative over-fires: INFERRED-from-base-rate unless
   flinch labeled} × {D′ recovers: INFERRED, cross-draw on World3}`.
   - pos ✓ ∧ neg clean → D is a gate-signal **candidate** (Ω_P auto-route ruling still the
     operator's; if cell 2 is inferred, say so).
   - pos ✓ ∧ neg over-fires ∧ recovery ✓ → D′ is the refined-gate **candidate**.
   - pos ✓ ∧ neg over-fires ∧ recovery ✗ → demote to **operator-review prompt** (light seat).
   - pos ✗ → **PROBE BROKEN, halt.**
5. **Writeup + substrate:** this directory; OQ-88 status in ISSUES.md; KNOWN_STATE.md;
   regenerate `issues/INDEX.md`.

## Corpora + code state (pinned)

- Live: `outputs/pipeline_output.json`, manifest `pipeline_run_at=2026-07-04T15:30:49Z`,
  `n_constraints=128`, `code_commit_short=23b7faa` (dirty). Prolog delta 23b7faa..HEAD is
  `giant_component_analysis.pl` only (new OQ-193 producer, not in the Layer-B path) ⇒
  comparable regime.
- Archived World3 Layer-B witness: `audits/2026-06-11_oq90_piton_refinement/pipeline_output.preedit.json`
  (run_at 2026-06-11T16:44:05Z, commit 411db0e; the three sibling outputs are consistency
  checks — all four read mountain→rope + `type_1_false_summit`). The OQ-98 baseline
  (07:13, `d9bbdf4`) predates alert wiring into verdict_join (`alerts=[]`) and is NOT used
  for Layer B.
- World3 Layer-A witness: `outputs/kernel_manifests/flat/world3_recalibration_2024_20260608_171605.manifest.json`
  (`is_contested_kernel: null`, plain-string `collapse_mechanism_ambiguity`).
- kernel_v1: `prolog/archives/datasets/kernel_v1/` (~1,106 files; count from glob at run time),
  classified fresh at HEAD `e438723b` (dirty: `signature_detection.pl` explanation-string
  prose only; `validation_suite.pl` auto-generated; `enhanced_report.py` not in this path).
- Sweep script: `python/audits/oq88_false_mountain_detector.py`, archived into this directory.

## Halt conditions

- Either full-D positive fails D → PROBE BROKEN, halt and fix the probe.
- Dispatch control fails (reader misses the known mountain→rope; join misses a known flat
  manifest or the `_c0` normalization; known kernel-routed constraint not in kernel bucket)
  → halt before any counting is reported.
- `classify_corpus` refuses (zero-glob / load-incomplete / raw-stale) → Phase 3 reported as
  NOT RUN, never patched around.
