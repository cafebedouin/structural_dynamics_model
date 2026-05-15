# Recon-2 Predictions Check

**Status:** Phases 1-3 complete. Phase 4 (side study) pending framework-author review.  
**Date:** 2026-05-14  
**Predictions source:** `docs/recon_2_scope_v2.md` §2

---

## Phase 1 (Module Inventory) Predictions

> **≥3 purpose-mismatch files.**

**Result: 4. Landed (exceeded).**
1. `structural_signatures.pl` — docs say it's the signatures module; it's an empty facade.
2. `drl_modal_logic.pl` — docs attribute specific predicates to it; it's an empty facade; those predicates are in `drl_boltzmann_analysis.pl`.
3. `drl_lifecycle.pl` header — says "Ten drift event types"; 11 implemented.
4. `fpn_report.pl` — labeled a reporter, listed as out-of-scope initially; contains logic-bearing `purity_zone/2` implementation (retained in scope per scope doc note).

---

> **Named-but-missing list will be empty or near-empty.**

**Result: Empty (0 entries). Landed exactly.**

---

> **Exists-but-unnamed list will have at least 2 entries from the ambiguous-resolution set.**

**Result: 8 notable entries. Landed (exceeded).**  
Most significant: `drl_composition.pl` and `drl_counterfactual.pl` implement the actual modal logic content; neither appears by name anywhere in the doc set. `network_dynamics.pl` implements Type 11 drift but is unnamed in docs. Four additional files from the newly-in-scope set (sheaf_analysis, grothendieck_cohomology, arakelov_height, coercion_projection) are also unnamed. Prediction said "≥2 entries from the ambiguous-resolution set" — 5 from that set ended up in-scope and unnamed.

---

## Phase 2 (Architectural Patterns) Predictions

> **Shadow mode: Partial. The pattern's enforcement was never structural — it was discipline-based.**

**Result: Partial. Landed.**  
Two sub-cases:
- `boltzmann_compliance.pl` calls `classify_from_metrics/6` for read-only Boltzmann testing. Verdict for this module: Holds with read-only access.
- `giant_component_analysis.pl` mutates `config:param(network_coupling_threshold, _)` at runtime (retract/assertz pair, lines 84-96). The specific param is not read by `classify_from_metrics/6`, so no classification result changes — but config is the single source of truth and runtime mutation violates the architectural boundary. Partial violation.

Net verdict for shadow mode: **Partial**. Prediction landed.

---

> **Two-regime classification: Holds, possibly with bypasses.**

**Result: Holds with bypasses. Landed.**  
- `dr_type/3` always runs both stages for general constraints.
- `constraint_instances.pl` bypass: adds `constraint_indexing:constraint_classification/3` rules for 3 specific historical instances, some of which produce or partially produce types without running both stages. Not documented as a known exception.
- Stage 2 predicate (`integrate_signature_with_modal/3`) is in `signature_detection.pl`, not `structural_signatures.pl` as docs state.

---

> **Network one-hop: Enforced by parameter or by structural recursion bound; clean finding either way.**

**Result: Structural. Landed.**  
`drl_purity_network.pl` enforces one-hop by reading neighbor intrinsic purity (a static value), never recursing into effective purity. No parameter governs hop count — enforcement is code structure. `drl_fpn.pl` is a documented, opt-in extension to multi-hop (not a replacement).

---

> **Priority ordering: Holds in `drl_core.pl`'s gate cascade, with implementation details that may differ from the docs' compact statement.**

**Result: Holds. Landed.**  
Gate cascade in `classify_from_metrics/6` matches documented order exactly: Mountain (300) > Piton(dead-coord) (314) > Snare (323) > Scaffold (333) > Rope (341) > Tangled Rope (352) > Piton(fallback) (366) > Naturalized (379) > unknown (385). No implementation details diverge from documented order.

---

**Phase 2 net prediction: 1 partial, 3 holds. Result: 1 partial, 3 holds. Landed exactly.**

---

## Phase 3 (Concept Inventory) Predictions

> **Purity zone: Divergent (C13 from recon-1 confirmed; check for fourth+ implementations).**

**Result: Divergent — 3 active purity_zone/2 implementations. No fourth.** Prediction landed.  
Recon-1 C13 confirmed. The three implementations have different zone vocabularies and different thresholds. `purity_scoring.pl` header documents 3 informal zones in comments (no predicate), adding a 4th description site but not a 4th predicate implementation. `structural_purity/2` in `signature_detection.pl` is a distinct concept (not a purity zone bucket), noted to avoid conflation.

---

> **Structural signature: Convergent or unified.**

**Result: Unified. Landed.**  
`constraint_signature/2` defined exactly once, in `signature_detection.pl`. All other references import or delegate. The doc attribution to the facade (`structural_signatures.pl`) is a naming error, not a divergence in implementation. Finding extends the prediction: docs undercount the signature types (4 named vs 9 implemented + constructed sub-variants).

---

> **Classification gate: Unified via `classify_from_metrics/6`, possibly with one bypass.**

**Result: Unified with one undocumented bypass. Landed.**  
`classify_from_metrics/6` is the sole gate implementation. `constraint_instances.pl` bypass exists for 3 hardcoded historical instances. Bypass is not documented as an intentional exception.

---

> **Drift event type: Divergent — header says 10 vs 11 actual, type definitions likely scattered.**

**Result: Divergent. Landed.**  
Primary finding: 11 context-free types in `drift_events.pl`; `drl_lifecycle.pl` header still says 10 (recon-1 L5 persistent). Extended finding: 7 indexed variants in `drift_events.pl` not named in the docs; Type 11 (`network_drift`) detection logic split across `drift_events.pl` and `network_dynamics.pl`.

---

**Phase 3 net prediction: 2 divergent, 1 convergent/unified, 1 unified-with-bypass. Result: 2 divergent, 1 unified, 1 unified-with-bypass. Landed exactly.**

---

## Phase 4 (Side Study) Predictions — PENDING

Phase 4 (abductive subsystem side study) has not run. Predictions carry forward to next session:

> **Same within-subsystem concept-drift profile as constraint logic.**

> **Cleaner config-consumption (abductive_* params read from config.pl, not hardcoded).**

> **Clean boundary to constraint logic (abductive may have its own conventions but the boundary is clean).**

---

## Overall Assessment

All phase 1-3 predictions landed. No prediction was falsified by the data. The findings exceeded predictions in scope (more exists-but-unnamed entries than predicted, more purpose-mismatch files than predicted, more indexed drift variants than documented), but the predicted verdicts were correct in every case.

**Most informative finding (outside prediction space):**  
`giant_component_analysis.pl` mutates `config:param/2` at runtime. This was not predicted and is the most architecturally significant finding: a Stage-9+ module temporarily modifying the config database (the single source of truth) during execution, even if the specific param does not affect `classify_from_metrics/6` in the current corpus. The mutation is bounded (restore after use) but runtime.

**Second-most informative (outside prediction space):**  
The facade chain is deeper than anticipated. Three of the six "core module" names the docs use (`structural_signatures.pl`, `drl_modal_logic.pl`, `drl_lifecycle.pl`) are empty wrappers; the actual logic is in sub-modules, several of which (`drl_composition.pl`, `drl_counterfactual.pl`, `network_dynamics.pl`) are completely unnamed in the doc set.
