# Recon-2 Inventory — Phase 1 Output

**Status:** Complete. Phase 2 (patterns) and Phase 3 (concepts) produced in parallel.  
**Date:** 2026-05-14  
**Scope anchor:** `docs/recon_2_scope_v2.md`, execution prompt Recon-2 Engine Drift Audit  
**Prior audit:** `docs/logic_divergence_audit.md` (recon-1, Feb + Mar 2026)

---

## Pre-Audit Scope Finding

Before extraction began, inspection of the 32 definite in-scope files revealed a **facade chain** not anticipated by the scope:

- `structural_signatures.pl` is an empty convenience wrapper (`:- module(structural_signatures, []).`) over `boltzmann_compliance`, `signature_detection`, `purity_scoring`. Its own header says "NOT the canonical import path."
- `drl_modal_logic.pl` is an empty facade over `drl_composition`, `drl_counterfactual`, `drl_boltzmann_analysis`, `drl_purity_network`, `drl_fpn`. Same header note.
- `drl_lifecycle.pl` is an empty facade over `drift_events`, `transition_paths`, `network_dynamics`, `drift_report`. Same.

The docs (logic.md, logic_index.md) consistently name the facades as implementation locations. The actual logic lives downstream. This is an instance of the label-stable / content-elsewhere drift the audit is hunting — surfaced before extraction ran.

Scope correction applied (per plan): `drl_composition.pl` and `drl_counterfactual.pl` added as in-scope. `drl_boltzmann_analysis.pl` was in the ambiguous set and confirmed in-scope during Pass A.

---

## 1. Primary Inventory Table

### 1a. Core Files — Path, Module, Purpose, Doc References

| File | Module | Purpose | Doc References |
|------|--------|---------|----------------|
| `drl_core.pl` | `drl_core` | Context-indexed classification (v4.0). `classify_from_metrics/6` is the canonical hub; `dr_type/3` is the primary API. Integrates signatures via `signature_detection`. | logic.md (multiple); logic_thresholds.md (§§2-4); logic_index.md |
| `drl_modal_logic.pl` | `drl_modal_logic` | [NO LOGIC] Empty facade; reexports `drl_composition`, `drl_counterfactual`, `drl_boltzmann_analysis`, `drl_purity_network`, `drl_fpn`. | logic_thresholds.md ("reformability_score/3", "purity_adjusted_energy/4"); logic_index.md (listed as implementation) |
| `drl_lifecycle.pl` | `drl_lifecycle` | [NO LOGIC] Empty facade; reexports `drift_events`, `transition_paths`, `network_dynamics`, `drift_report`. | logic_index.md; logic_thresholds.md (contamination/network refs) |
| `structural_signatures.pl` | `structural_signatures` | [NO LOGIC] Empty convenience wrapper; reexports `boltzmann_compliance`, `signature_detection`, `purity_scoring`. Header says "NOT the canonical import path." | logic_index.md ("Signatures: structural_signatures.pl"); logic_thresholds.md (as wrapper) |
| `signature_detection.pl` | `signature_detection` | Structural signature detection (v3.2+). Implements all 4 Boltzmann-derived signatures (NL, FNL, CI_Rope, FCR). Contains `integrate_signature_with_modal/3`, the Stage 2 override predicate. | logic.md (multiple: signature descriptions, shadow mode reminders); logic_extensions.md (§1 code fragments); logic_thresholds.md |
| `signature_mapper.pl` | `signature_mapper` | Maps non-standard terminology to standard constraint types via signatures. | [UNREFERENCED] |
| `signature_config.pl` | [NO MODULE DECL] | Configuration parameters for structural signatures (v3.2). Defines param/2 facts to add to config.pl. | [UNREFERENCED] |
| `constraint_indexing.pl` | `constraint_indexing` | Context-indexed classification. Implements power/scope modifiers, sigmoid, directionality, site contexts. Multifile for `constraint_classification/3`. | logic.md (effective_immutability, valid_context, extractiveness); logic_thresholds.md |
| `constraint_bridge.pl` | `constraint_bridge` | Bridge from constraint classification to diagnostic state and veto actor derivation. | [UNREFERENCED] |
| `constraint_data.pl` | `constraint_data` | Data bridge: centralizes metric access, delegates to `narrative_ontology` via multifile. | [UNREFERENCED] |
| `constraint_instances.pl` | `constraint_instances` | Extends `constraint_indexing:constraint_classification/3` with rules for specific historical instances (Catholic Church 1200, Carbon Tax 2026, Property Rights 2025). | [UNREFERENCED] |
| `drift_events.pl` | `drift_events` | Event detection, severity, velocity for constraint drift. Implements 11 drift event types. | logic_thresholds.md (via drl_lifecycle.pl facade: `detect_coupling_drift/1`, `detect_boltzmann_floor_drift/1`) |
| `type_metadata.pl` | `type_metadata` | Pure data: descriptions, strategies, colors, severity ordering for 6 constraint types. | [UNREFERENCED] |
| `measurement_layer.pl` | `measurement_layer` | Wasserstein L1 (earth mover's) distance between MaxEnt distributions at adjacent observer positions. | [UNREFERENCED] |
| `domain_priors.pl` | `domain_priors` | Domain-specific priors: extractiveness/stakes/suppression/resistance by category. Bridges domain registry with classification. Declares multifile facts in `drl_core` namespace. | [UNREFERENCED by name; implied by classification architecture] |
| `domain_priors_expanded.pl` | `domain_priors_expanded` | Auto-generated corpus-derived defaults from 617 constraints. Category and type profiles. | [UNREFERENCED] |
| `domain_registry.pl` | `domain_registry` | Auto-generated registry: 3,346 constraint IDs → type categories. | [UNREFERENCED] |
| `narrative_ontology.pl` | `narrative_ontology` | Core ontology: entities, intervals, events, constraint claims, metrics, `is_tangled_rope/1`, `has_coordination_function/1`, `has_asymmetric_extraction/1`. | [UNREFERENCED by name; omnipresent as imported module] |
| `drl_composition.pl` | `drl_composition` | Composition rules, transformation tracking, audit utilities. Split from drl_modal_logic.pl (v4.0+). Imports `drl_audit_core`. | [UNREFERENCED — see Exists-but-Unnamed §3] |
| `drl_counterfactual.pl` | `drl_counterfactual` | Context-indexed counterfactual reasoning: simulate cuts, dependency chains, structural coupling, scaffold need assessment. Split from drl_modal_logic.pl (v4.0+). | [UNREFERENCED — see Exists-but-Unnamed §3] |
| `boltzmann_compliance.pl` | `boltzmann_compliance` | Boltzmann compliance engine (v5.0): factorization independence test for NL claims. Calls `drl_core:classify_from_metrics/6` to gather classifications for testing. Uses assertz/retractall for memoization cache. | logic_extensions.md (§1, multiple code fragments); logic_thresholds.md (§5) |
| `purity_scoring.pl` | `purity_scoring` | Combines 4 Boltzmann tests into purity scalar [0,1]. Header defines 3 informal zones (>0.8 sound, 0.5 borderline, <0.3 contaminated) in comments only; no `purity_zone/2` predicate. | [UNREFERENCED by name; "purity scoring" concept referenced in logic_extensions.md] |
| `drl_purity_network.pl` | `drl_purity_network` | Stage 8: One-hop contamination propagation through constraint networks. Explicitly enforces one-hop only ("no transitive propagation, avoids convergence complexity"). | logic_thresholds.md (effective_purity, contamination strength, immunity) |
| `network_dynamics.pl` | `network_dynamics` | Network drift detection (Type 11), contagion, cascades. Bridges Stage 8 topology with drift engine. | [UNREFERENCED — see Exists-but-Unnamed §3] |
| `drl_fpn.pl` | `drl_fpn` | Stage 8b: Fixed-Point Network Iteration. Explicitly extends one-hop propagation to multi-hop convergence (reads fpn_ep from previous iteration instead of intrinsic purity). Uses assertz/retractall for iteration state. | logic_thresholds.md (line 614: implementation reference) |
| `fpn_report.pl` | [NO MODULE DECL] | Standalone script comparing one-hop vs multi-hop effective purity; reports zone migrations. **Implements its own `purity_zone/2` (4 zones: sound/contested/degraded/critical) — diverges from other implementations.** | [UNREFERENCED as module; retained in scope per scope doc: implements purity-zone taxonomy] |
| `logical_fingerprint.pl` | `logical_fingerprint` | 7-dimensional structural fingerprint engine. Dimension 6 (zone) contains `purity_zone/2` (5 zones: pristine/sound/borderline/contaminated/degraded, hardcoded thresholds). | [UNREFERENCED by name; "logical fingerprint" concept implied in architecture] |
| `fingerprint_report.pl` | [NO MODULE DECL] | Standalone script: corpus-wide fingerprint analysis, shift patterns, zone distribution. | [UNREFERENCED] |
| `giant_component_analysis.pl` | `giant_component_analysis` | Erdős-Rényi phase transition investigation. Implements 3rd `purity_zone/2` (4 zones: sound/borderline/warning/degraded, config-driven). **CRITICAL: temporarily mutates `config:param/2` at runtime** (retract/assertz of `network_coupling_threshold`). | [UNREFERENCED] |
| `config.pl` | `config` | Central configuration. 500+ params across 33 sections. Single source of truth per architecture invariants. | logic_thresholds.md (primary; "Source: config.pl"); logic_index.md |
| `config_schema.pl` | `config_schema` | Declarative schema for all param/2 facts: type, constraint, cross-parameter relationship invariants. | [UNREFERENCED by name] |
| `config_validation.pl` | `config_validation` | Schema-driven validation. Critically: validates `drl_purity_network:type_contamination_strength/2` hardcoded facts against config params. | [UNREFERENCED by name] |
| `drl_boltzmann_analysis.pl` | `drl_boltzmann_analysis` | Stages 5–7: reformability scoring, coupling-aware scaffold need, purity reform recommendations, purity-qualified action algebra. Reexported by drl_modal_logic.pl. | logic_thresholds.md (via "drl_modal_logic.pl facade": reformability_score, purity_qualified_action, action_composition_gate, purity_adjusted_energy) |
| `sheaf_analysis.pl` | `sheaf_analysis` | Diagnostic composition of H¹ and Arakelov height into 3-regime partition (genuine_sheaf/fragile_presheaf/manifest_presheaf) and block consistency monitoring. | [UNREFERENCED] |
| `grothendieck_cohomology.pl` | `grothendieck_cohomology` | Čech cohomological invariants (H⁰, H¹), contextuality fraction (Abramsky-Brandenburger), power-chain monotonicity. Uses assertz for cache management. | [UNREFERENCED] |
| `arakelov_height.pl` | `arakelov_height` | Boundary complexity diagnostic: ε × (MaxEnt uncertainty + conditional signature pressure). Identifies constraints where engine cannot decide type. Uses nb_setval/nb_getval for threshold memoization. | [UNREFERENCED] |
| `coercion_projection.pl` | `coercion_projection` | Coercion vector [A, S, U, R], magnitude, gradient, system-level gradient. [NO HEADER beyond first comment]. | [UNREFERENCED] |

### 1b. Pattern Flags (P1–P4)

| File | P1: assertz/retract | P1: calls classify_from_metrics | P2: produces type | P3: contamination | P4: dr_type cascade |
|------|--------------------|---------------------------------|-------------------|-------------------|---------------------|
| drl_core.pl | NO | YES (defines it) | YES | NO | YES (defines dr_type/3) |
| drl_modal_logic.pl | NO | NO | NO | NO | NO |
| drl_lifecycle.pl | NO | NO | NO | NO | NO |
| structural_signatures.pl | NO | NO | NO | NO | NO |
| signature_detection.pl | NO | NO (comment ref only) | YES (via signature override) | NO | NO |
| signature_mapper.pl | NO | NO | YES (via constraint_signature) | NO | NO |
| signature_config.pl | NO | NO | NO | NO | NO |
| constraint_indexing.pl | NO | NO | PARTIAL (indexed wrapper) | NO | NO |
| constraint_bridge.pl | YES (veto_actor) | NO | YES (via dr_type/2) | NO | YES (calls dr_type) |
| constraint_data.pl | NO | NO | NO | NO | NO |
| constraint_instances.pl | NO | NO | YES (hardcoded + delegated) | NO | NO |
| drift_events.pl | NO | NO | NO | NO | YES (calls dr_type) |
| type_metadata.pl | NO | NO | NO | NO | NO |
| measurement_layer.pl | NO | NO | NO | NO | NO |
| domain_priors.pl | NO | NO | NO | NO | NO |
| domain_priors_expanded.pl | NO | NO | NO | NO | NO |
| domain_registry.pl | NO | NO | NO | NO | NO |
| narrative_ontology.pl | NO | NO | NO | NO | NO |
| drl_composition.pl | NO | NO | NO | NO | YES (calls dr_type) |
| drl_counterfactual.pl | NO | NO | NO | NO | YES (calls dr_type, multiple) |
| boltzmann_compliance.pl | YES (cache) | YES (line 253, read-only) | YES (via classify_from_metrics) | NO | NO |
| purity_scoring.pl | NO | NO | NO | NO | NO |
| drl_purity_network.pl | NO | NO | NO | YES (one-hop, structural) | NO |
| network_dynamics.pl | NO | NO | NO | YES (Type 11) | NO |
| drl_fpn.pl | YES (iteration state) | NO | NO | YES (multi-hop) | NO |
| fpn_report.pl | NO | NO | NO | YES (comparison) | NO |
| logical_fingerprint.pl | NO | NO | NO | NO | NO |
| fingerprint_report.pl | NO | NO | NO | NO | NO |
| giant_component_analysis.pl | YES (**config mutation**) | NO | NO | YES (analysis) | NO |
| config.pl | NO | NO | NO | NO | NO |
| config_schema.pl | NO | NO | NO | NO | NO |
| config_validation.pl | NO | NO | NO | NO | NO |
| drl_boltzmann_analysis.pl | NO | NO | NO | NO | NO |
| sheaf_analysis.pl | NO | NO | NO | NO | NO |
| grothendieck_cohomology.pl | YES (cache) | NO | NO | NO | NO |
| arakelov_height.pl | NO (nb_setval) | NO | NO | NO | NO |
| coercion_projection.pl | NO | NO | NO | NO | NO |

### 1c. Concept Flags (C1–C4)

| File | C1: purity zone | C2: struct signature | C3: classification gate | C4: drift event type |
|------|-----------------|----------------------|-------------------------|----------------------|
| drl_core.pl | NO | YES (imports/calls) | YES (defines classify_from_metrics/6) | NO |
| drl_modal_logic.pl | NO | NO | NO | NO |
| drl_lifecycle.pl | NO | NO | NO | NO |
| structural_signatures.pl | NO | NO | NO | NO |
| signature_detection.pl | YES (structural_purity/2) | YES (defines constraint_signature/2, integrate_signature_with_modal/3) | YES (signature override path) | NO |
| signature_mapper.pl | NO | YES (uses constraint_signature/2) | YES (maps to types) | NO |
| signature_config.pl | NO | NO | NO | NO |
| constraint_indexing.pl | NO | NO | NO | NO |
| constraint_bridge.pl | NO | NO | YES (via dr_type/2) | NO |
| constraint_data.pl | NO | NO | NO | NO |
| constraint_instances.pl | NO | NO | YES (direct + delegated) | NO |
| drift_events.pl | YES (critical ref in purity_drift) | NO | NO | YES (defines 11 types) |
| type_metadata.pl | NO | NO | YES (lists types) | NO |
| measurement_layer.pl | NO | NO | YES (operates on type chain) | NO |
| domain_priors.pl | NO | NO | NO | NO |
| domain_priors_expanded.pl | NO | NO | YES (type profiles) | NO |
| domain_registry.pl | NO | NO | YES (registry entries) | NO |
| narrative_ontology.pl | NO | NO | YES (defines constraint_type/1) | NO |
| drl_composition.pl | NO | NO | YES (composition rules on types) | NO |
| drl_counterfactual.pl | NO | NO | YES (counterfactual on types) | NO |
| boltzmann_compliance.pl | NO | YES (uses signatures) | YES (via classify_from_metrics) | NO |
| purity_scoring.pl | YES (comment zones) | YES (indirectly) | NO | NO |
| drl_purity_network.pl | YES (zone-qualified scores) | YES (indirectly) | YES (uses types for immunity) | NO |
| network_dynamics.pl | YES (sound/escalation/degraded floors) | NO | YES (uses types for severity) | YES (Type 11) |
| drl_fpn.pl | YES (effective purity zones) | NO | YES (uses types for immunity) | NO |
| fpn_report.pl | YES (defines purity_zone/2 #2) | NO | NO | NO |
| logical_fingerprint.pl | YES (defines purity_zone/2 #1) | YES (fingerprint_coupling) | YES (uses types) | NO |
| fingerprint_report.pl | NO | YES (indirectly) | NO | NO |
| giant_component_analysis.pl | YES (defines purity_zone/2 #3) | YES (indirectly) | YES (uses types) | NO |
| config.pl | YES (action floors) | YES (signature thresholds) | NO | NO |
| config_schema.pl | YES (schema) | YES (schema) | NO | NO |
| config_validation.pl | YES (validates) | YES (validates) | NO | NO |
| drl_boltzmann_analysis.pl | YES (purity reform targets) | NO | YES (uses types) | NO |
| sheaf_analysis.pl | NO | NO | NO | NO |
| grothendieck_cohomology.pl | NO | NO | NO | NO |
| arakelov_height.pl | NO | NO | NO | NO |
| coercion_projection.pl | NO | NO | NO | NO |

---

## 2. Named-but-Missing

**Prediction: near-empty (docs written from code, not spec).**

**Finding: Zero.** No file named in the doc set is absent from the file system.

*Prediction: landed.*

---

## 3. Exists-but-Unnamed

Files in scope that no doc names directly. Prediction: ≥2.

| File | Situation |
|------|-----------|
| `drl_composition.pl` | Implements composition, transformation, audit utilities. Completely absent from all four docs. Reexported by `drl_modal_logic.pl` facade. |
| `drl_counterfactual.pl` | Implements counterfactual reasoning. Completely absent from all four docs. Reexported by `drl_modal_logic.pl` facade. |
| `network_dynamics.pl` | Implements Type 11 network drift detection and contagion. Not named anywhere in the doc set. Reexported by `drl_lifecycle.pl` facade. |
| `drl_fpn.pl` | Stage 8b fixed-point iteration. Named once in logic_thresholds.md at an implementation reference, but not described in the architecture docs. |
| `coercion_projection.pl` | Coercion vector and gradient computation. Not named anywhere. |
| `sheaf_analysis.pl` | Sheaf/presheaf partition. Not named anywhere. |
| `grothendieck_cohomology.pl` | H⁰/H¹ cohomology. Not named anywhere. |
| `arakelov_height.pl` | Arakelov height diagnostic. Not named anywhere. |
| Multiple others | narrative_ontology.pl, domain_priors.pl, constraint_data.pl, etc. — not named by file but functionally implied. |

Most notable: `drl_composition.pl` and `drl_counterfactual.pl` are the actual implementations behind the facade the docs call "modal logic," yet neither appears by name anywhere in the doc set. Same for `network_dynamics.pl` and the lifecycle facade.

*Prediction: "≥2 entries from the ambiguous-resolution set" — landed. Exceeded: 8 notable entries, not 2.*

---

## 4. Purpose-Mismatch

Files where doc-stated purpose visibly diverges from actual contents. Prediction: ≥3.

| File | Doc-Stated Purpose | Actual Contents | Divergence |
|------|--------------------|-----------------|------------|
| `structural_signatures.pl` | Docs: "Signatures: structural_signatures.pl" (logic_index.md); thresholds doc calls it "the canonical signatures module." | Empty convenience wrapper. Zero logic. | Severe: label points to facade, logic is in `signature_detection.pl`, `boltzmann_compliance.pl`, `purity_scoring.pl`. |
| `drl_modal_logic.pl` | Docs: implementation location for `reformability_score/3`, `purity_adjusted_energy/4` (logic_thresholds.md). | Empty facade. Those predicates are in `drl_boltzmann_analysis.pl`. | Severe: thresholds doc attributes specific predicates to the facade. |
| `drl_lifecycle.pl` | Self-description in header: "Ten drift event types." | Eleven drift event types implemented (confirmed recon-1 finding L5). | Moderate: header comment is wrong by one. |
| `fpn_report.pl` | Named as a reporter; scope doc initially listed as out-of-scope reporter. | Implements `purity_zone/2` with 4-zone taxonomy (sound/contested/degraded/critical) — logic-bearing. | Moderate: file classified by name as report tooling, but contains a logic-bearing concept definition. |

*Prediction: "≥3 purpose-mismatch files" — landed. Found 4.*

---

## 5. Ambiguous-Resolution Decisions

**IN SCOPE** — implements doc-claimed logic:

| File | Decision | Justification |
|------|----------|---------------|
| `sheaf_analysis.pl` | IN | Exports `sheaf_status/2`, `block_consistency/2` — implements the three-regime partition (genuine_sheaf/fragile_presheaf/manifest_presheaf) that is a doc-claimed diagnostic. |
| `grothendieck_cohomology.pl` | IN | Exports `cohomological_obstruction/3`, `descent_status/2`, `contextuality_fraction/1` — implements H⁰/H¹ cohomology, the Abramsky-Brandenburger contextuality fraction, and power-chain monotonicity per the spec. |
| `arakelov_height.pl` | IN | Exports `arakelov_height/2`, `arakelov_threshold/1` — implements the boundary complexity diagnostic and fragile/genuine threshold split, a doc-claimed instrument. |
| `coercion_projection.pl` | IN | Exports coercion vector, magnitude, gradient predicates — implements domain-logic computations for the coercion apparatus. |
| `drl_boltzmann_analysis.pl` | IN | Exports `reformability_score/3`, `purity_qualified_action/4`, `action_composition_gate/3`, `purity_adjusted_energy/4` — these exact predicates are attributed to `drl_modal_logic.pl` in logic_thresholds.md. The file implements them; the facade reexports them. |

**OUT OF SCOPE** — computes analysis on the apparatus from outside:

| File | Decision | Justification |
|------|----------|---------------|
| `covering_analysis.pl` | OUT | Grid redundancy and coverage gap audit: analyzes the 12-point index grid for information-theoretic redundancy. Audit tooling, not logic implementation. |
| `invertibility_analysis.pl` | OUT | Tests whether context-tuple transformations are invertible. Audit tooling: investigates pipeline properties, does not implement pipeline logic. |
| `bifurcation_export.pl` | OUT | Exports classifications as streaming output for diff-based audit. Pure reporting. |
| `drl_audit_core.pl` | OUT | Deprecated audit path using simplified chi computation (not sigmoid pipeline). Explicitly marked DEPRECATED; produces "audit signatures" not real classification outputs. |
| `omega1_audit.pl` | OUT | Multi-phase analysis of constraints unknown at analytical/global level. Audit tooling that profiles classification failures. |
| `post_synthesis.pl` | OUT | T12 post-synthesis divergence trigger: detects diagnostic verdict / abductive trigger disagreement. Canary trigger, not constraint logic. |
| `product_site_export.pl` | OUT | Exports product-site cohomology to JSON. Pure reporting/export. |
| `inferred_coupling_protocol.pl` | OUT | Verifies dormant inferred coupling activation. Verification/measurement protocol, not logic implementation. |
| `gap_diagnostic.pl` | OUT | Characterizes classification gap region; profiles constraints unknown at midpoint d. Diagnostic tooling. |
| `diagnostic_summary.pl` | OUT | Cross-subsystem verdict synthesis: aggregates signals from 12 subsystems, produces traffic-light verdict. Meta-analysis layer. |

**Additional scope note:** `drl_audit_core.pl` is imported by `drl_composition.pl` (line 35: `:- use_module(drl_audit_core)`). Since `drl_audit_core` is OUT and `drl_composition` is IN, this is an inward dependency from in-scope to out-scope code — potential coupling issue, noted for Phase 2/3 analysis.

---

## 6. Summary Counts

- **Definite in-scope:** 32 files (per plan) + 2 scope corrections (drl_composition, drl_counterfactual) = **34**
- **Added from ambiguous:** 5 (sheaf_analysis, grothendieck_cohomology, arakelov_height, coercion_projection, drl_boltzmann_analysis)
- **Total in-scope:** **39 files**
- **Out of scope from ambiguous:** 10 files
- **Named-but-missing:** 0
- **Exists-but-unnamed (notable):** 8
- **Purpose-mismatch:** 4
