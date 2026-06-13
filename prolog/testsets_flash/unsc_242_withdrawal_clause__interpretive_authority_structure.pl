% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Structure
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint describes the contested authority structure for
 *   interpreting the withdrawal clause of UNSC Resolution 242. The
 *   International Court of Justice, drafting states, and the occupying state
 *   each claim primary interpretive authority, leading to a meta-dispute that
 *   prevents definitive resolution of the underlying textual ambiguity. This
 *   meta-dispute itself functions as a snare, allowing parties with political
 *   leverage to benefit from the perpetuated ambiguity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.85).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.75).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.85).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e196cdc9-43c2-4310-bb60-3b74f32bae01').
narrative_ontology:cs_kernel_codification('e196cdc9-43c2-4310-bb60-3b74f32bae01', fixed_text).
narrative_ontology:cs_authority_grounding('e196cdc9-43c2-4310-bb60-3b74f32bae01', distributed).
narrative_ontology:cs_reading_relation('e196cdc9-43c2-4310-bb60-3b74f32bae01', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('e196cdc9-43c2-4310-bb60-3b74f32bae01', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_axiom('e196cdc9-43c2-4310-bb60-3b74f32bae01', foundational, interpretive_authority_is_contested).
narrative_ontology:cs_axiom_status(interpretive_authority_is_contested, holdable).
narrative_ontology:cs_axiom_grounding('e196cdc9-43c2-4310-bb60-3b74f32bae01', interpretive_authority_is_contested, conventional).
narrative_ontology:cs_reference_frame('e196cdc9-43c2-4310-bb60-3b74f32bae01', un_charter_interpretive_principles).
narrative_ontology:cs_drift_state('e196cdc9-43c2-4310-bb60-3b74f32bae01', contemporary_geopolitical_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e196cdc9-43c2-4310-bb60-3b74f32bae01', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_powers).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, parties_seeking_legal_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_system).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the interpretive ambiguity allows the occupying state to retain territory and veto powers to maintain strategic flexibility, at the expense of parties seeking legal closure. Suppression (0.75) is high because the various claims to authority actively suppress any single, definitive interpretation from taking hold, effectively trapping those seeking resolution. The theater ratio (0.4) reflects that while there are genuine legal arguments, a significant portion of the 'interpretive' activity serves to maintain the ambiguity rather than resolve it.
 *
 * PERSPECTIVAL GAP:
 *   The occupying state and veto powers experience this as a beneficial flexibility, allowing them to pursue their interests within a legally ambiguous framework. Parties seeking legal closure and the international legal system experience it as a profound failure of international law, leading to ongoing costs and erosion of legitimacy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and veto powers are beneficiaries (d near 0.0) as they gain from the lack of definitive interpretation. Parties seeking legal closure and the international legal system are victims (d near 1.0) as they bear the costs of unresolved disputes and undermined legal authority. The ICJ and drafting states, while claiming authority, are constrained in their ability to enforce it, placing them closer to the middle, but still subject to the snare's dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to provide a clear mechanism for treaty interpretation. However, it has atrophied into a mechanism for perpetuating ambiguity, serving the interests of powerful actors. The high extractiveness and suppression, coupled with the 'dead' founding problem status, indicate a snare where the coordination function (resolving ambiguity) has been subverted by extraction (benefiting from ambiguity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_interpretive_claims,
    'Which claim to interpretive authority (ICJ, drafting states, customary practice) holds the greatest legitimacy under international law, independent of political power?',
    'A universally accepted, non-politicized advisory opinion from a body like the International Law Commission, or a new, unambiguous UN Security Council resolution that explicitly defines interpretive hierarchy.',
    'If one claim is definitively established as legitimate, the snare would weaken as the ambiguity it thrives on would be reduced, potentially leading to a clearer legal obligation for withdrawal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_interpretive_claims, conceptual, 'The underlying legitimacy of competing claims to interpretive authority.').

omega_variable(
    political_vs_legal_capture,
    'To what extent is the interpretive authority structure captured by political interests (e.g., veto powers) versus being a genuine, albeit complex, legal dispute?',
    'Analysis of voting records, diplomatic cables, and state practice to quantify instances where political interests demonstrably override legal arguments in interpretive debates.',
    'If political capture is dominant, the constraint is more clearly a snare, as its ''legal'' function is merely a cover for power dynamics. If it''s a genuine legal dispute, it might lean more towards a tangled rope with high coordination costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_legal_capture, empirical, 'The degree of political capture in the interpretive authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This constraint describes the meta-dispute over interpretive authority for UNSC Resolution 242's withdrawal clause. It enables the persistence of the 'maximal_withdrawal_reading' and 'partial_withdrawal_reading' by preventing a definitive resolution between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
