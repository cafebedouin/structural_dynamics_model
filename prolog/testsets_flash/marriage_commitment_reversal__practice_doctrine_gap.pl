% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Reversal: Practice-Doctrine Gap
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the period (1890-1904) where a religious
 *   institution publicly suspended its marriage commitment practice (Section
 *   132) due to external pressure, while preserving the principle in doctrine
 *   and allowing some private continuation. This created a structural
 *   ambiguity, enabling institutional survival (coordination) at the cost of
 *   membership clarity and internal coherence (extraction). The constraint is
 *   a 'practice-doctrine gap' reading of the broader 'marriage commitment
 *   reversal' kernel.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Agenda setter (institutional/constrained) — navigated external pressure and internal dissent.
 *   - general_membership: Payer (moderate/constrained) — experienced confusion and betrayal due to doctrinal ambiguity.
 *   - fundamentalist_factions: Payer (organized/identity_locked) — resisted the change, leading to schism.
 *   - federal_government: Observer (institutional/arbitrage) — exerted external pressure, leading to the public suspension.
 *   - institutional_survival: Beneficiary (analytical/civilizational) — the abstract entity that benefited from the ambiguity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.7).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Reversal: Practice-Doctrine Gap").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '8d337407-0323-49bf-9e89-aab5992e6f2c').
narrative_ontology:cs_kernel_codification('8d337407-0323-49bf-9e89-aab5992e6f2c', fixed_text).
narrative_ontology:cs_authority_grounding('8d337407-0323-49bf-9e89-aab5992e6f2c', lineage).
narrative_ontology:cs_interpretation_layer_present('8d337407-0323-49bf-9e89-aab5992e6f2c').
narrative_ontology:cs_reading_relation('8d337407-0323-49bf-9e89-aab5992e6f2c', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d337407-0323-49bf-9e89-aab5992e6f2c', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('8d337407-0323-49bf-9e89-aab5992e6f2c', foundational, doctrinal_integrity_preserved_despite_practice_suspension).
narrative_ontology:cs_axiom_status(doctrinal_integrity_preserved_despite_practice_suspension, holdable).
narrative_ontology:cs_axiom_grounding('8d337407-0323-49bf-9e89-aab5992e6f2c', doctrinal_integrity_preserved_despite_practice_suspension, theological).
narrative_ontology:cs_axiom('8d337407-0323-49bf-9e89-aab5992e6f2c', secondary, institutional_survival_justifies_ambiguity).
narrative_ontology:cs_axiom_status(institutional_survival_justifies_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('8d337407-0323-49bf-9e89-aab5992e6f2c', institutional_survival_justifies_ambiguity, instrumental).
narrative_ontology:cs_reference_frame('8d337407-0323-49bf-9e89-aab5992e6f2c', unambiguous_doctrinal_practice).
narrative_ontology:cs_drift_state('8d337407-0323-49bf-9e89-aab5992e6f2c', post_1890_manifesto, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8d337407-0323-49bf-9e89-aab5992e6f2c', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the ambiguity imposed significant costs on the general membership and fundamentalist factions, who faced cognitive dissonance, loss of clarity, or outright schism. Suppression (0.7) was necessary to manage internal dissent and maintain public compliance. The theater ratio (0.6) is high because the public suspension of practice was largely performative, masking continued private adherence and doctrinal preservation. The period 1890-1904 is chosen as the interval because it marks the initial public declaration of suspension (1890) and the subsequent period of ambiguity before more definitive institutional shifts.
 *
 * PERSPECTIVAL GAP:
 *   Institutional leadership experienced this as a necessary, if painful, coordination to ensure survival, balancing external threats with internal commitments. For the general membership, it was a source of confusion and betrayal, an extraction of their clear understanding of core doctrine. Fundamentalist factions saw it as a direct violation, leading to their exit. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership (agenda_setter) is a beneficiary (d near 0.0) as they successfully navigated a crisis and preserved the institution. General membership and fundamentalist factions (payers) are targets (d near 1.0) as they bore the costs of ambiguity and internal conflict. Institutional survival (an abstract beneficiary) is also near 0.0. The federal government is an external actor whose pressure shaped the constraint but was not directly extracted from or subsidized by it in this specific reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a 'tangled_rope' because it served a genuine coordination function (institutional survival in the face of federal pressure) but did so through asymmetric extraction (the cost of ambiguity borne by the membership). The classification prevents mislabeling it as a 'snare' by acknowledging the real, if costly, coordination, and prevents mislabeling it as a 'rope' by highlighting the significant, asymmetric extraction. The ambiguity itself was the mechanism of both coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily a ''practice-doctrine gap'' reading of the marriage commitment reversal kernel, or is it better understood as an ''exogenous override'' or ''endogenous reinterpretation''?',
    'Analysis of primary source documents (e.g., Woodruff''s private journals vs. public declarations, federal court records) to determine the dominant causal factor for the change in practice and the explicit doctrinal justifications offered at the time.',
    'If ''exogenous override'' is dominant, the constraint''s suppression and extractiveness would be higher, reflecting external coercion. If ''endogenous reinterpretation'' is dominant, the ''theater_ratio'' would be lower, as the doctrinal shift would be more genuine. This ''practice-doctrine gap'' reading emphasizes the ambiguity and internal tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing between different readings of the marriage commitment reversal kernel.').

omega_variable(
    legitimation_dual_track_efficacy,
    'How effective was the dual-track legitimation (public compliance vs. private continuation) in ensuring institutional survival and managing internal dissent?',
    'Quantitative analysis of membership retention, schism rates, and legal challenges during the 1890-1904 period, correlated with the degree of public vs. private adherence to the marriage commitment.',
    'If highly effective, it reinforces the ''tangled_rope'' classification by demonstrating the functional coordination (institutional survival) achieved through asymmetric extraction (membership clarity sacrificed). If ineffective, it suggests a more ''snare''-like quality, where the extraction failed to deliver its promised coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimation_dual_track_efficacy, empirical, 'Assessing the efficacy of the dual-track legitimation strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0, 0.4).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 5, 0.5).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 10, 0.55).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 14, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 14, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 14, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel. This 'practice-doctrine gap' reading focuses on the period of ambiguity and internal tension, distinct from readings emphasizing external coercion or internal reinterpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
