# Recon-2 Pattern Verification — Phase 2 Output

**Status:** Complete.  
**Date:** 2026-05-14  
**Analysis over:** `recon_2_inventory.md` (Phase 1)  
**Granularity rules:** Per `docs/recon_2_scope_v2.md` §5

---

## Pattern 1: Shadow Mode

**Claim.** Stages 7-9 modules don't modify `classify_from_metrics/6` or mutate state it reads.

**Stages 7-9 modules (from inventory):** `boltzmann_compliance.pl`, `purity_scoring.pl`, `drl_purity_network.pl`, `network_dynamics.pl`, `drl_fpn.pl`, `drl_boltzmann_analysis.pl`, `logical_fingerprint.pl`, `giant_component_analysis.pl`, `fpn_report.pl`, `fingerprint_report.pl`.

### Evidence

**boltzmann_compliance.pl — READ-ONLY access:**
- Line 253: `drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type)` — called to collect classifications at multiple observer positions for the independence (factorization) test.
- Lines 37-38: `retractall(cached_classification(_, _, _))`, `retractall(cached_coupling(_, _))` — cache cleanup on `clear_classification_cache/0`.
- Lines 162, 228: `assertz(cached_coupling(C, ComputedScore))`, `assertz(cached_classification(C, Context, ComputedType))` — memoization cache for the Boltzmann test.
- **Assessment:** Reads `classify_from_metrics/6` results; does not mutate predicates that `classify_from_metrics/6` reads. The cached_classification and cached_coupling facts are private to boltzmann_compliance. Read-only access.

**drl_fpn.pl — Iteration state only:**
- Lines 76-80: `retractall(fpn_ep/3)`, `retractall(fpn_type_cache/3)`, `retractall(fpn_neighbors_cache/3)`, `retractall(fpn_iteration_info/4)`, `retractall(fpn_intrinsic/2)` — cleanup of fpn_* dynamic facts.
- Lines 102-165, 274-278: `assertz(fpn_ep/3)`, `assertz(fpn_intrinsic/2)`, `assertz(fpn_type_cache/3)`, `assertz(fpn_neighbors_cache/3)`, `assertz(fpn_iteration_info/4)` — builds and updates iteration state tables.
- **Assessment:** All asserted facts are fpn_* predicates — private to the fixed-point iteration engine. `classify_from_metrics/6` does not read these. No mutation of state that `classify_from_metrics/6` consumes.

**giant_component_analysis.pl — CONFIG MUTATION:**
- Lines 84-96:
  ```prolog
  retract(config:param(network_coupling_threshold, OrigThresh)),
  assertz(config:param(network_coupling_threshold, 0.01)),
  ...
  retract(config:param(network_coupling_threshold, 0.01)),
  assertz(config:param(network_coupling_threshold, OrigThresh))
  ```
- This pattern temporarily replaces `config:param(network_coupling_threshold, _)` with a low value (0.01) during edge computation, then restores it.
- **Does `classify_from_metrics/6` read `network_coupling_threshold`?** Checking `drl_core.pl`: `classify_from_metrics/6` reads `mountain_suppression_ceiling`, `mountain_extractiveness_max`, `piton_epsilon_floor`, `piton_theater_floor`, `snare_chi_floor`, `snare_epsilon_floor`, `snare_suppression_floor`, `scaffold_*`, `rope_*`, `tangled_rope_*`, `piton_*`, `naturalized_*` params — not `network_coupling_threshold`. The param being mutated drives edge construction in the purity network, not metric classification.
- **Assessment:** Runtime mutation of `config:param/2` is a violation of the shadow-mode boundary — `config.pl` is the single source of truth, and a Stage-9+ module modifying it at runtime breaks that invariant, even if the specific param mutated doesn't affect `classify_from_metrics/6` in this instance. The mutation is bounded (restore after use) but is runtime, not initialization-time only.

**All other Stages 7-9 modules:** No assertz/retract calls; no calls to `classify_from_metrics/6`.

### Verdict: **Partial**

Shadow mode mostly holds. Two sub-cases:

1. `boltzmann_compliance.pl` calls `classify_from_metrics/6` (Stage 7) — but read-only. Verdict: **Holds with read-only access** for this module. The architecture requires this: the Boltzmann test needs classifications at multiple positions to test factorization.

2. `giant_component_analysis.pl` mutates `config:param/2` at runtime. The specific param (`network_coupling_threshold`) is not read by `classify_from_metrics/6`, so no classification result changes. However, it modifies the config database — the single source of truth — at runtime, which is a structural violation of the shadow-mode boundary even if the specific effect is currently zero. **Partial violation.**

**Prediction:** "Partial — enforcement was discipline-based, not structural." *Landed.*

---

## Pattern 2: Two-Regime Classification

**Claim.** Classification flows metrics-first via `classify_from_metrics/6`, then signature-override via `structural_signatures:integrate_signature_with_modal/3`.

### Evidence

**Canonical path — `dr_type/3` in `drl_core.pl` (lines 398-414):**
```prolog
dr_type(C, Context, Type) :-
    ...compute BaseEps, Chi, Supp...
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, MetricType),   % Stage 1
    signature_detection:integrate_signature_with_modal(C, MetricType, FinalType),  % Stage 2
    ...
dr_type(_C, _Context, unknown).  % fallback if metrics fail entirely
```
Both stages run on every classification via `dr_type/3`. The canonical two-regime path holds for the primary API.

**First naming issue:** `integrate_signature_with_modal/3` is in `signature_detection.pl` (line 398 of signature_detection.pl). The docs (logic_index.md) attribute this to `structural_signatures.pl`. The import in `drl_core.pl` (line 67) correctly resolves to `signature_detection`:
```prolog
:- use_module(signature_detection, [constraint_signature/2, integrate_signature_with_modal/3]).
```
The facade `structural_signatures.pl` reexports `signature_detection`, so calling via `structural_signatures:integrate_signature_with_modal/3` would work, but `drl_core.pl` bypasses the facade and imports directly. The Stage 2 predicate is in the right place operationally; the doc reference to `structural_signatures.pl` names the facade, not the implementation.

**Bypass — `constraint_instances.pl`:**
Adds clauses to `constraint_indexing:constraint_classification/3` for specific named constraints (Catholic Church 1200, Carbon Tax 2026, Property Rights 2025). These clauses are checked before `dr_type/3` fires. Some clauses:
- Return type atoms directly without calling either stage (e.g., `mountain` for Catholic Church serf perspective, conditioned on `effective_immutability_for_context` check only).
- Others call `drl_core:is_tangled_rope(...)` or `drl_core:is_snare(...)`, which internally call `classify_from_metrics/6` (Stage 1 only; no signature override).

**Scope of bypass:** Three specific historical instances. Not general. These instances were manually constructed to validate specific classification scenarios and predate the two-regime architecture. The bypass is not documented in the code or the docs as a known exception.

**`boltzmann_compliance.pl` classification access:**
Calls `classify_from_metrics/6` directly (line 253) to gather classifications for factorization testing — this is a read-only call to Stage 1 only, for a purpose other than producing a final classification. Not a bypass of the two-regime path.

### Verdict: **Holds with bypasses**

The canonical classification path (`dr_type/3`) runs both stages for all general constraints. A bypass exists in `constraint_instances.pl` for three specific hardcoded instances, where `constraint_classification/3` rules produce or partially produce types without running both stages. The bypass is not documented as intentional. The Stage 2 predicate is correctly identified in code but misattributed in docs (named in `structural_signatures.pl`, implemented in `signature_detection.pl`).

**Prediction:** "Holds, possibly with bypasses for specific signature types." *Landed — bypass exists, for hardcoded instances rather than signature types.*

---

## Pattern 3: Network Contamination One-Hop

**Claim.** Contamination propagation runs one hop only.

### Evidence

**`drl_purity_network.pl` header (lines 43-47):**
```
One-hop only: no transitive propagation (avoids convergence complexity)
```

**`effective_purity/4` implementation (line 170+):**
```prolog
effective_purity(C, Context, EffPurity, ...) :-
    constraint_neighbors(C, Context, Neighbors),
    compute_total_contamination(C, Neighbors, IntrinsicPurityFn, TotalContam, Detail),
    ...
```
`compute_total_contamination` computes contamination from each neighbor using the **neighbor's intrinsic purity** (not its effective purity). Reading intrinsic purity (a static property of each neighbor, not recursively computed) enforces one-hop by code structure: there is no recursive call back into `effective_purity`.

Enforcement is **structural**: the algorithm simply never recurses. No parameter governs hop count.

**`drl_fpn.pl` — explicit multi-hop extension:**
Header (lines, Stage 8b section):
```
Extends Stage 8's one-hop purity propagation to multi-hop convergence.
The single semantic change: neighbor purity reads from the previous
iteration's effective purity (fpn_ep/3) instead of intrinsic purity
```
`drl_fpn.pl` is a documented extension beyond one-hop. It is a separate, opt-in computation (`fpn_run/2`), not a replacement of the one-hop engine. The one-hop enforcement in `drl_purity_network.pl` is unchanged.

**`network_dynamics.pl`:**
`detect_network_drift/3` calls `drl_purity_network:effective_purity/4` (the one-hop version), not `drl_fpn:fpn_effective_purity`. Type 11 network drift uses the one-hop effective purity.

### Verdict: **Structural**

One-hop enforcement is structural: `drl_purity_network.pl` reads neighbor intrinsic purity (a static value), never recursing into effective purity, making multi-hop propagation impossible in this module by code structure. `drl_fpn.pl` extends this to multi-hop but is a separate opt-in module with its own execution entry point (`fpn_run/2`); it does not replace the one-hop engine.

**Prediction:** "Enforced by structure or parameter." *Landed — structural.*

---

## Pattern 4: Priority Ordering in `dr_type/3`

**Claim.** Priority order: Mountain > Piton(dead-coord) > Snare > Scaffold > Rope > Tangled Rope > Piton(fallback) > Naturalized > unknown.

### Evidence

Gate cascade in `classify_from_metrics/6`, `drl_core.pl`:

| Line | Type | Notes |
|------|------|-------|
| 300 | `mountain` | Supp ≤ SuppCeil, BaseEps ≤ MaxX, immutability = mountain |
| 314 | `piton` (dead-coord) | coordination_dead, ε ≥ piton_epsilon_floor, theater ≥ TRFloor — fires before snare |
| 323 | `snare` | Chi ≥ ChiFloor, Eps ≥ EpsFloor, Supp ≥ SuppFloor, snare_immutability_check |
| 333 | `scaffold` | coordination function, Chi ≤ scaffold ceiling, temporality check |
| 341 | `rope` | Chi ≤ RopeCeil, Eps ≤ RopeCeil, immutability = rope OR emerges_naturally |
| 352 | `tangled_rope` | eps and chi in tangled range |
| 366 | `piton` (fallback) | normal piton conditions, no dead-coord requirement |
| 379 | `naturalized` | low-level fallback for naturally-emerged constraints |
| 385 | `unknown` | catch-all |

Documented order: Mountain > Piton(dead-coord) > Snare > Scaffold > Rope > Tangled Rope > Piton(fallback) > Naturalized > unknown

Code order matches documented order exactly.

**`dr_type/3` wrapper (lines 398-414):**
```prolog
dr_type(C, Context, Type) :-
    ...metrics...
    classify_from_metrics(..., MetricType),
    signature_detection:integrate_signature_with_modal(C, MetricType, FinalType), ...
dr_type(_C, _Context, unknown).
```
Priority is implemented as a Prolog clause sequence: the first matching clause fires and cuts. The ordering is enforced by clause order.

**Implementation detail:** The `integrate_signature_with_modal/3` call at Stage 2 can override the MetricType from Stage 1. Signature overrides can promote (rope → CI_Rope), demote (mountain → tangled_rope for FNL), or change type (mountain → constructed_constraint). The priority order applies to the metric-based Stage 1; Stage 2 may alter the final type. This is by design and consistent with the docs.

### Verdict: **Holds**

Gate cascade matches documented priority order exactly. The implementation detail of Stage 2 signature override is consistent with the documented two-regime architecture, not a violation of priority ordering.

**Prediction:** "Holds with implementation details." *Landed.*

---

## Pattern 5: Spec → Registry → Implementation Flow Direction

**Excluded from this audit** per `recon_2_scope_v2.md` §5: "Not testable from a code snapshot. Requires commit-history audit."

**Recon-1 observation (for context):** Recon-1 found that value-level drift was resolved by updating the docs to match code — the backward direction. This observation cannot be confirmed or denied from a code snapshot and is not re-litigated here.

---

## Cross-Pattern Observation

`drl_composition.pl` imports `drl_audit_core` (which is out-of-scope deprecated audit code). This creates a dependency from an in-scope production module to out-of-scope audit tooling. Not a pattern violation per the patterns defined above, but notable as an architectural coupling. `drl_audit_core.pl` exports `effective_extraction/3`, `structural_signature/3`, `ontological_fraud_check/2`, `omega_risk/4` — the last two appear diagnostic. Worth a targeted review if `drl_composition.pl` is ever refactored.
