# Recon-2 Concept Inventory — Phase 3 Output

**Status:** Complete.  
**Date:** 2026-05-14  
**Analysis over:** `recon_2_inventory.md` (Phase 1)  
**Verdict criteria:** Unified (one definition, others import) / Convergent (multiple, same output) / Divergent (multiple, different output)

---

## Concept 1: Purity Zone

**Doc reference:** `logic_extensions.md` §2.3, `logic_thresholds.md` §6a.  
**Known state from recon-1 (C13):** Three divergent implementations — `logical_fingerprint.pl`, `fpn_report.pl`, `giant_component_analysis.pl`.

### Implementation Table

| File | Predicate | Zone names | Thresholds | Authoritative? |
|------|-----------|------------|------------|----------------|
| `logical_fingerprint.pl` (line 607-611) | `purity_zone/2` | pristine / sound / borderline / contaminated / degraded | ≥0.90 / ≥0.70 / ≥0.50 / ≥0.30 / else | NO (fingerprint-scoped) |
| `fpn_report.pl` (line 109-112) | `purity_zone/2` | sound / contested / degraded / critical | ≥0.70 / ≥0.50 / ≥0.30 / else | NO (report-scoped) |
| `giant_component_analysis.pl` (line 576-582) | `purity_zone/2` | sound / borderline / warning / degraded | config `purity_action_sound_floor` / mid / `purity_action_degraded_floor` / else | NO (analysis-scoped) |
| `purity_scoring.pl` (header comment) | none (comment only) | "structurally sound" / "borderline" / "contaminated" | >0.8 / 0.5 / <0.3 | NO (comment, not predicate) |
| `config.pl` (lines 418-422) | none (params) | — | `purity_action_sound_floor`, `purity_action_escalation_floor`, `purity_action_degraded_floor` | Partial (supplies thresholds for gca only) |

**Authoritative:** None. There is no single `purity_zone/2` implementation that other modules import or delegate to. Each implementation is self-contained.

### Verdict: **Divergent — 3 active implementations, 1 informal description**

Three `purity_zone/2` predicate implementations with incompatible zone vocabularies:
- 5-zone vocabulary (logical_fingerprint): pristine, sound, borderline, contaminated, degraded
- 4-zone vocabulary type A (fpn_report): sound, contested, degraded, critical
- 4-zone vocabulary type B (giant_component): sound, borderline, warning, degraded

No two implementations share a complete zone set. "borderline" appears in both logical_fingerprint (zone 3 of 5) and giant_component_analysis (zone 2 of 4) but at different threshold positions. "sound" appears in all three with the same semantic meaning (high purity) but with different hardcoded/parameterized thresholds.

Fourth observation: `purity_scoring.pl` header documents 3 zones by threshold description but does not define a `purity_zone/2` predicate. These informal thresholds differ from all three active implementations.

There is also `signature_detection.pl`'s `structural_purity/2` predicate (lines 943-962), which returns atoms: `inconclusive`, `pure_natural_law`, `pure_coordination`, `pure_scaffold`, `contaminated(Reasons)`. This is a different concept (structural contamination signature, not a purity-score zone bucket), but shares the `contaminated` atom and the word "purity" in the predicate name. It is NOT a `purity_zone/2` implementation; calling it a 4th purity-zone implementation would conflate distinct concepts. Noted for clarity.

**Recon-1 C13:** Confirmed. Current state matches the three-implementation finding. No additional `purity_zone/2` predicate was found beyond the three active implementations. Prediction of "check for fourth+" found none — but the purity_scoring.pl informal description and structural_purity/2 extend the picture.

*Prediction: "Divergent (C13 confirmed)." Landed.*

---

## Concept 2: Structural Signature

**Doc reference:** `logic_extensions.md` §1 (NL/FNL/CI_Rope/FCR), `logic.md` §V.

### Implementation Table

| File | Predicate | Defines? | Delegates to? | Notes |
|------|-----------|----------|---------------|-------|
| `signature_detection.pl` | `constraint_signature/2` | YES (lines 53-113) | None — primary implementation | 9 signature types: natural_law, false_natural_law, coupling_invariant_rope, false_ci_rope, false_summit_mountain, coordination_scaffold, piton_signature, constructed_constraint (3 sub-variants: low/mid/high extraction), ambiguous |
| `signature_detection.pl` | `integrate_signature_with_modal/3` | YES | None — Stage 2 override | Resolves metric type + signature → final type |
| `structural_signatures.pl` | — | NO (facade only) | signature_detection (reexport) | Empty wrapper; zero logic |
| `drl_core.pl` | `dr_signature/2` | YES (line 31) | `constraint_signature/2` | Convenience export: calls `constraint_signature/2` |
| `signature_mapper.pl` | `map_custom_pillar/3` | NO | `signature_detection:constraint_signature/2` | Maps non-standard terms to types via signatures; calls not defines |
| `boltzmann_compliance.pl` | — | NO | Uses signatures indirectly | Supplies Boltzmann results that signature_detection uses |

**Authoritative:** `signature_detection.pl` — `constraint_signature/2` is the sole definition point. All other references either import, delegate to, or wrap it.

### Verdict: **Unified**

`constraint_signature/2` is defined once, in `signature_detection.pl`. All paths that need a signature go through it. The doc attribution to `structural_signatures.pl` names the facade, not the implementation, but the operational unity is intact.

**Naming observation:** The docs name 4 signature types (NL, FNL, CI_Rope, FCR per logic_extensions.md). The implementation has 9 (plus constructed sub-variants and ambiguous). The doc set underdescribes the implementation by roughly 2×. Not a divergence of implementation, but a divergence between the doc's named signature catalog and the code's actual catalog.

*Prediction: "Convergent or unified." Landed — unified.*

---

## Concept 3: Classification Gate

**Doc reference:** `logic.md` §II.B (gate definitions).

### Implementation Table

| File | Predicate | Route | Notes |
|------|-----------|-------|-------|
| `drl_core.pl` | `classify_from_metrics/6` | Canonical — defines all 9 gate clauses | Primary |
| `drl_core.pl` | `dr_type/3` | Calls classify_from_metrics/6 + integrate_signature_with_modal/3 | Primary API |
| `signature_detection.pl` | `integrate_signature_with_modal/3` | Stage 2 override — adjusts MetricType | Supplement, not gate replacement |
| `constraint_instances.pl` | `constraint_indexing:constraint_classification/3` | Hardcoded rules + partial delegation | Bypass for 3 named instances |
| `boltzmann_compliance.pl` | `classify_at_context/3` | Calls `drl_core:classify_from_metrics/6` | Read-only; for Boltzmann test only |
| `drl_audit_core.pl` | `structural_signature/3` | Uses alternative audit-chi path (not sigmoid pipeline) | OUT OF SCOPE; deprecated |
| `data_validation.pl` | `infer_expected_type/2` | Delegates to `drl_core:classify_from_metrics/6` | Out of scope module but routes through canonical |

**Authoritative:** `drl_core.pl:classify_from_metrics/6` — all in-scope production paths route through it.

**Bypass:** `constraint_instances.pl` adds clauses to `constraint_indexing:constraint_classification/3` that can produce constraint types without calling `classify_from_metrics/6`. These cover three specific historical instances and are not marked as bypasses in the code. Some clauses in this file do call `drl_core:is_mountain/3` etc., which do route through `classify_from_metrics/6` — so the bypass is partial even within the bypass file.

### Verdict: **Unified with one undocumented bypass**

All general constraint classification routes through `classify_from_metrics/6`. The `constraint_instances.pl` bypass applies to 3 hardcoded historical examples and is not documented as a known exception to the two-regime pattern.

*Prediction: "Unified via classify_from_metrics/6, possibly with one bypass for naturalized or a similar edge case." Landed — bypass exists but for hardcoded instances, not the naturalized gate specifically.*

---

## Concept 4: Drift Event Type

**Doc reference:** `logic.md` §III, `logic_extensions.md` §4.  
**Known signal from recon-1 (L5):** `drl_lifecycle.pl` header says "Ten drift event types" but 11 are implemented.

### Implementation Table

**Defined in `drift_events.pl`:**

| # | Type atom | Line | Scope |
|---|-----------|------|-------|
| 1 | `metric_substitution` | 186 | context-free |
| 2 | `extraction_accumulation` | 194 | context-free |
| 3 | `coordination_loss` | 202 | context-free |
| 4 | `function_obsolescence` | 212 | context-free |
| 5 | `sunset_violation` | 217 | context-free |
| 6 | `extraction_dried_up` | 223 | context-free |
| 7 | `internalized_piton` | 227 | context-free |
| 8 | `coupling_drift` | 283 | context-free |
| 9 | `boltzmann_floor_drift` | 291 | context-free |
| 10 | `purity_drift` | 349 | context-free |
| 11 | `network_drift` | 393 | context-free (delegated to network_dynamics.pl) |

**Indexed variants also defined in `drift_events.pl`** (not in the 11-count):

| Indexed variant | Line | Notes |
|-----------------|------|-------|
| `extraction_accumulation_indexed` | 234 | context-specific variant |
| `false_mountain_drift` | 242 | indexed: claimed mountain, actual type mismatch |
| `load_bearing_degradation` | 248 | indexed: purity/type degradation |
| `coupling_drift_indexed` | 301 | indexed variant |
| `reform_pressure_detected` | 311 | indexed: reform pressure signal |
| `purity_drift_indexed` | 357 | indexed variant |
| `network_drift_indexed` | 398 | indexed variant |

**Other modules defining or matching on drift type atoms:**

| File | Interaction |
|------|-------------|
| `drl_lifecycle.pl` | Header says "Ten drift event types" — stale (recon-1 L5 confirmed). |
| `network_dynamics.pl` | Implements `detect_network_drift/3`, `network_drift_contagion/3`, `network_drift_velocity/4`, `network_drift_severity/3` — the operative implementation of Type 11 (`network_drift`). `drift_event(C, network_drift, Evidence)` in `drift_events.pl` calls `network_dynamics:detect_network_drift/3`. |
| `drl_composition.pl` | Defines `transformation_type/6` with atoms: `capture`, `obsolescence`, `calcification`, `discovery` — semantic labels for constraint transformations, not drift event types. Distinct concept; not in the drift event type vocabulary. |
| `drift_report.pl` | Out of scope (pure reporter). Enumerates drift event types for reporting — reads them, does not define. |

**Count reconciliation:**
- 11 context-free types: confirmed, matches recon-1 L5 finding.
- 7 additional indexed variants defined in `drift_events.pl`: these are not documented in the doc set's type enumeration.
- `drl_lifecycle.pl` header: still says "Ten" — not updated since the 11th type was added.

### Verdict: **Divergent**

Three sites of divergence:
1. `drl_lifecycle.pl` header vs. actual count: documented as 10, 11 implemented — persistent across recon-1 and recon-2.
2. The doc set (logic.md §III, logic_extensions.md §4) documents a closed set of named types, but `drift_events.pl` implements 7 indexed variants that are not named or described anywhere in the docs.
3. `network_dynamics.pl` contains the operative implementation of Type 11 (`detect_network_drift`) while `drift_events.pl` contains the `drift_event(C, network_drift, _)` declaration that delegates to it — the type is split across two modules.

**Authoritative:** `drift_events.pl` is the primary definition site for the 11 context-free types. `network_dynamics.pl` is authoritative for the Type 11 detection logic.

*Prediction: "Divergent — header says 10 vs 11 actual." Landed. Extended: 7 additional indexed variants also undocumented.*
