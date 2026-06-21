# OQ-138 — FSM `resolve_modal_signature_conflict` converted RECLASSIFY → ROUTE/COMMENT

**Date:** 2026-06-21. **Scope:** `false_summit_mountain` clause only (the FSM sub-part of OQ-138).
FCR / constructed / coupling_invariant_rope / false_natural_law remain OPEN with named witnesses.

## What shipped

1. **FSM no longer overwrites `dr_type`.** `config.pl:488` `false_summit_override_target` default
   flipped `tangled_rope → mountain`. The mountain-input clause head binds `ModalType=mountain`, so
   `Result = Target = mountain = ModalType` ⇒ no overwrite, via the *existing* hook (which stays a
   live ablation lever: set `tangled_rope` to restore the legacy v6.9 overwrite). The unknown-input
   clause `Result = tangled_rope → unknown` (OQ-37 honest abstain; **zero live fires — rides OQ-37
   precedent, unverified-in-commit**).

2. **Victim-discriminated severity (the reusable template).** New
   `signature_detection:converted_signature/1` + `signature_diagnostic_severity/3`;
   `signature_grade/2` + `signature_severity/2` rewired so converted signatures grade on their OWN
   discriminant (NOT on a type delta, which is always zero post-revert) — mirroring
   `drl_core:dr_claim_mismatch/4`. `false_summit_mountain ∧ vic>0 → moderate` (concealment floor),
   `∧ vic=0 → informational` (routes; the alert is emitted and visible but raises no floor). Legacy
   (still-overwriting) signatures keep the historical `correction → moderate` mapping **byte-identical**.

3. **Consumer-correctness fix (found by code-read, not in the plan as written).** FSM removed from
   `abductive_helpers:known_override_signature/1` + `override_target/2`. Without this,
   `diagnostic_summary:probe_signature/3` (wired into `collect_signals`) emits a spurious
   `disagrees(override_mismatch(false_summit_mountain, mountain))` post-revert (dr_type now = metric
   type ≠ the stale override target) — an unexplained tension. FSM no longer overrides, so it leaves
   both tables; P1/P7 (`signature_override_artifact` / `pre_post_override_divergence`) then go
   cleanly vacuous.

## Witness (all on current code)

### Step 0 — kill condition runnable (gates the build)
`twin_sweep.pl` on `testsets_flash` (overlay-took-effect: 960 loaded) reproduces the 2026-06-13
breadth result EXACTLY: 40 FSM fires = **18 vic=0 + 22 vic>0**, same 22 named seats. The `vic>0`
falsifier is executable.

### Live corpus grew — the falsifier is now on live main
The live corpus is **92 seats** (was 57 when the plan was written) and now has **3** FSM-cascade
seats, one of them **vic=1** (`protein_anabolic_resistance`) — so the `vic>0` branch is exercised on
live main, not only in the flash overlay. The plan assumed "2 seats, both vic=0."

### Conversion diff (full pipeline, all 92 seats) — `PIPELINE_OLD.txt` vs `PIPELINE_NEW.txt`
**Only the 3 FSM seats change; 89 others byte-identical** (claimed_type, signature, verdict_join,
base, grade). FCR/constructed/CI-rope untouched.

| seat | vic | OLD (pipeline) | NEW (pipeline) |
|---|---|---|---|
| actinide_replenishment_mechanism_flat_control | 0 | yellow / grade=correction | **red** / grade=commentary |
| protein_anabolic_resistance | 1 | yellow / grade=correction | **red** / grade=correction |
| radiative_levitation_stratification | 0 | yellow / grade=correction | **red** / grade=commentary |

`claimed_type=mountain` throughout; `dr_type` reverts `tangled_rope → mountain` (route, not
reclassify). `maxent_top_type=mountain` (maxent now AGREES with the reverted type).

### The surprise: verdict goes RED, not the plan's expected GREEN
OLD decomposition: dirac/cohomology/maxent/context_gap divergences were all *expected_conflicts*
(explained via the override-keyed `signature_override_artifact` / `pre_post_override_divergence`
patterns and `constructed_type(tangled_rope)`); only 1 abductive tension → yellow. The override was
**masking** the structural tensions by setting the type to tangled_rope, where second-class Dirac +
descent failure are "expected." NEW: type reverts to mountain → maxent/context_gap now *agree*
(clean), but dirac (`second_class` vs mountain) + cohomology (`fails_descent`) + abductive
(`hub_conflict`/`classical_oracle_failure`/`epistemic_trap`) surface as genuine raw tensions → red.

**This is honest escalation, not a misfire:** a mountain claim contradicted by second-class
structure + descent failure IS a strong false-summit contradiction. The conversion reveals what the
override hid. Classification is unchanged (`claimed_type=mountain`).

### Operator ruling (2026-06-21)
> *the engine adds commentary, it does not change classifications, and it is OK for diagnostics of
> the engine to render different verdicts.*

⇒ **Position A** (let the subsystems speak; accept red) over Position B (add new expected_conflict
arms to suppress dirac/cohomology and force base green). The victim discriminant correctly lives in
the **commentary layer** (`signature_grade` commentary/correction + alert severity
informational/moderate, both serialized in the join tuple), not forced onto the headline color.

### Why Position A is right — corpus-wide evidence (`fsm_verdict_sweep.pl`)
Across **5 corpora** (live 92, haiku 960, flash 960, kernel_v1 1106, original_v6 3380 ≈ 6,500
stories, multiple generation models): **82 FSM-detected seats, every one carries
cohomology/fails_descent (+ usually dirac) → all yellow/red, 0 where the discriminant would be
headline-visible.** The dirac second-class + cohomology descent-failure are *structural invariants*
of false-summit mountains. Position B would mean blanket-suppressing a signal that fires on 100% of
the population.

### Trap-guard + positive controls (`trapguard.pl`)
- `severity_floor/2` positive control (two-sided): floors severe→red, moderate→yellow; **no** floor
  for informational; **no** catchall.
- Discriminant: `signature_diagnostic_severity(protein, false_summit_mountain, moderate)` (vic>0),
  `(actinide, …, informational)` (vic=0).
- **Trap averted, witnessed:** for all 3 seats metric==dr_type==mountain (type delta = false), yet
  protein grades **correction** (vic>0 discriminant) while the vic=0 pair grade **commentary**. A
  naive type-only revert would have dropped protein to commentary (silently dropping the concealment
  diagnostic). The discriminant does the work — exactly the `dr_claim_mismatch` precedent.

### Engine tests
- `validation_suite` (corpus): **Passed 92, Failed 0, Errors 0.**
- `check_stack`: no new findings vs the 2026-06-04 baseline (pre-existing data_repair/validation_suite warnings only).
- `test_contradiction_signatures`: 5 failed / 12 passed — **identical OLD vs NEW** (confirmed by
  stashed-build run); the failures are in the CS committer-axis fixture (`cs_kernel_axiom_conflict`),
  unrelated to this change.
- Full `run_pipeline.py`: all stages ok.

## Subtlety recorded
`constraint_signature/2` is a cut-terminated priority cascade (FNL > FCR > FSM > NL > CI_rope >
unknown > profile), so it returns exactly ONE signature per seat — the plan's "multi-signature seat"
ordering hazard cannot arise at dispatch (0 live seats yield ≠1 signature). BUT a **bound-arg** query
`constraint_signature(C, false_summit_mountain)` bypasses the higher clauses' cuts via head-unification
and succeeds whenever the FSM *detector* fires, even when FCR/FNL is the true cascade winner. The
build uses the **unbound** form (`constraint_signature(C, Sig), converted_signature(Sig)`), so
FCR/FNL-shadowed seats route through the unchanged legacy path (witnessed:
`quantum_formalism__copenhagen_reading`, `hebrew_living_language__…` in flash — cascade=false_ci_rope,
graded by the legacy FCR path). The `fsm_verdict_sweep.pl` *bound* filter over-included those 2; they
are not FSM conversions.

## Deferred (still OPEN, named witnesses)
- **false_ci_rope** (19 fires) — gather per-seat diff via `fcr_override_enabled` ablation; inspect for diagnostic-vs-non-diagnostic partition.
- **constructed_*** (41+) — pipeline before/after diff + partition inspection.
- **coupling_invariant_rope** (4) — grep `dr_type='rope'` consumers WITH a positive control before routing blind.
- **false_natural_law** — deferred pending OQ-70 (FNL bait-confound).
