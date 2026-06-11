# Step 1 — classifier-sync nb_setval witness (OQ-83 entry item 5)

**Date:** 2026-06-11. **Substrate:** `prolog/archives/datasets/kernel_v2_test` (n=100) — the
prior live corpus archived at `00c639da`; identical files to what the 2026-06-08 V3/V3b census
(`audits/2026-06-08_typea_template_extensibility/BUILD_VERIFICATION.md`) measured as then-live
`testsets/`. **Code:** current worktree HEAD (`6fffa7b4` + this audit dir), i.e. AFTER the OQ-90
piton refinements and OQ-44 fail-closed suppression landed.

**Probes:** `step1_nbsetval_witness.pl` (+ `.out`/`.stderr`), `step1b_challenge_followup.pl`
(+ `.out`/`.stderr`), this dir. Globals cleared before every baseline call; caches cleared via
`cache_registry:clear_all_caches` per call; set-manipulation mirrors
`classify_at_time_with_supp`'s exact `nb_setval` terms (drl_composition.pl:246–251).

## Findings

**1. Hypothesis CONFIRMED at the surviving flagged point.** `clinical_deskilling_automation`
T=0, default context:

```
cleared: snapshot_type=piton  classify_at_time=unknown
set(theater=0.35 eps=0.28): snapshot_type=unknown
VERDICT: mismatch reproduced AND closed by nb-global state -> mechanism CONFIRMED
```

Driver: static `theater_ratio` fact = 0.81 vs temporal-at-T=0 = 0.35. Without the global,
`effective_theater_ratio/3` (drl_core.pl:306) falls back to the static 0.81 inside
`snapshot_type`'s `classify_from_metrics` call; `excess_extraction/2`
(boltzmann_compliance.pl:498) likewise reads static ε (fact ABSENT here → its fallback).
Setting the two globals to exactly what `classify_at_time` threads closes the mismatch.

**2. The second flagged flip GRADUATES CLEAN on current code.** `milblogger_legitimacy_erosion`
T=18: `snapshot_type=piton`, `classify_at_time=piton` — agreement, stable under the
set-manipulation. The 2026-06-08 mismatch at this point no longer exists; the engine moved in
between (OQ-90 capture-keyed piton refinement `64448411`/`fc724ab2`, OQ-44 fail-closed
suppression `966d53c8` — all touch the piton path). The 12→18 flagged flip is therefore no
longer classifier-sensitive at its endpoint.

**3. Census on current code: 2 unique mismatch (C,T) points (was 3 on 2026-06-08).**

```
mismatch: challenge_as_commons_maintenance T=5
mismatch: clinical_deskilling_automation T=0
```

**4. The new point (challenge T=5) is a DIFFERENT cause and touches no counted flip.**
T=5 carries only a suppression measurement (grid misalignment, the OQ-105 pattern; times
[0,2,4,5,6,8,10], ε/theater series skip 5). `classify_at_time` fabricates flagged ε=0.5
(`backed=false`) → `unknown`; `snapshot_type` sources ε via `metric_at`/`safe_metric` →
`scaffold`. Set-manipulation does NOT close it (`snapshot=scaffold` vs `unknown`) — this is
ε-SOURCING divergence, not nb-global state. Type series is scaffold at every other T with
backed endpoints only at T=0/T=10 (both scaffold) → no counted flip exists on this constraint
→ **no new flag enters the offline join**.

**5. Positive controls all PASS** (agreeing points stay agreeing under the same
set-manipulation): milblogger T=0 (`unknown`/`unknown`), clinical T=2
(`tangled_rope`/`tangled_rope`), milblogger T=18.

## Consequences for the flag inheritance (the join OQ)

- `milblogger_legitimacy_erosion` 12→18: **graduates clean** (endpoint no longer
  classifier-sensitive, witnessed on current code).
- `clinical_deskilling_automation` 0→2: stays flagged pending fix-or-document (operator gate);
  mechanism is now VERIFIED, no longer hypothesis.
- `challenge_as_commons_maintenance` T=5: recorded as a known mismatch point of the ε-sourcing
  class; not flagged (no counted flip).

## Latent hazard noted (for the fix-or-document decision)

`classify_at_time` never clears the globals it sets; they key on `tr(C,_)`/`eps(C,_)` — same-C
matches. So `snapshot_type(C,T)` called after any `classify_at_time(C,T')` in one process reads
T'-state for ALL its times (order-dependent, witnessed-class: the set-manipulation above proves
leftover same-C state alters `snapshot_type` output). `degradation_chain/3`
(transition_paths.pl:98–110) is the consumer exposed to this; it is pipeline-unwired today
(unfinished-value per the wire-or-gap adjudication), so no live output is affected.

## Determinism fix (operator ruling 2026-06-11: option 1 — fix order-dependence, document divergence)

Operator ruled per the counterfeit-witness rationale: a threading fix would produce an artifact
that *reads* as sync while the ε-sourcing divergence remains; document-only leaves a live
six-line bug. Fix applied: `snapshot_type/3` sets both nb-globals to `none` at entry
(transition_paths.pl), preserving static-fallback semantics, visibly NOT threaded.

**Witness 1 — unwired-consumer grep (with positive control):** `snapshot_type` callers outside
tests/archives = exactly `degradation_chain` (transition_paths.pl:101); `degradation_chain`
consumers = none beyond its own export (transition_paths.pl:8). Positive control: the same grep
pattern finds `constraint_history`'s real consumer (drl_composition.pl:262). No live output path
touches the edited clause.

**Witness 2 — before/after intervention runs** (`step1c_determinism_fix_witness.pl`, identical
script both runs; `step1c_prefix.out` / `step1c_postfix.out`):

```
PRE-FIX:  stale-read demo: cleared snapshot=piton | classify_at_time=unknown | snapshot-right-after=unknown
          -> ORDER-DEPENDENT (pre-fix bug state)
POST-FIX: stale-read demo: cleared snapshot=piton | classify_at_time=unknown | snapshot-right-after=piton
          -> DETERMINISTIC (post-fix expected state)
```

Post-fix: the clinical T=0 mismatch PERSISTS (`piton` vs `unknown` — the fix does not read as
sync; the documented semantic reason stands), and all three agreeing controls are undisturbed
(clinical T=2 `tangled_rope`/`tangled_rope`, milblogger T=0 `unknown`/`unknown`, T=18
`piton`/`piton`). `run_migration_tests` both tests PASS post-fix; dynamic validation suite on
the live corpus: Warnings 0, "DATA QUALITY: EXCELLENT".

Graduation: `clinical_deskilling_automation` 0→2 enters the join OQ as **documented exclusion**
(truthful label while the ε-sourcing divergence stands); `milblogger_legitimacy_erosion` 12→18
enters **clean**.

## Sync-claim status

Full `classify_at_time ≡ snapshot_type` remains FALSE with TWO witnessed causes:
(a) nb-global temporal threading (closable by threading or clearing), and (b) ε/supp sourcing
divergence at unmeasured times (`metric_at` nearest-fallback vs flagged fabrication) — (b) is
semantic, not state. A threading-only fix does not purchase the V3 must-hold.
