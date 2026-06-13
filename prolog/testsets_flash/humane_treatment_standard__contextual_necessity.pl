% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Humane Treatment Standard (Contextual Necessity Reading)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'contextual necessity' reading of Common
 *   Article 3 of the Geneva Conventions, which posits that while a baseline
 *   of humane treatment is required, national security imperatives can
 *   override certain aspects, making humane treatment context-dependent and
 *   permitting 'enhanced interrogation' techniques. This reading grants
 *   discretion to state security agencies, making detainee protections
 *   conditional and effectively shrinking the victim set to exclude
 *   'high-value targets' in perceived necessity scenarios.
 *
 * KEY AGENTS:
 *   - state_security_agencies: Agenda setter (institutional/arbitrage) — defines and applies 'necessity'
 *   - national_security_decision_makers: Beneficiary (institutional/arbitrage) — benefits from expanded operational latitude
 *   - detainees_of_national_security_interest: Payer (powerless/trapped) — bears the costs of conditional treatment
 *   - human_rights_advocates: Payer (organized/constrained) — bears the cost of defending the absolute standard
 *   - international_legal_bodies: Observer (institutional/analytical) — adjudicates compliance and legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.65).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.75).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.65).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Humane Treatment Standard (Contextual Necessity Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'ff35384e-0686-4782-afdd-78fdb4c5c9d5').
narrative_ontology:cs_kernel_codification('ff35384e-0686-4782-afdd-78fdb4c5c9d5', fixed_text).
narrative_ontology:cs_authority_grounding('ff35384e-0686-4782-afdd-78fdb4c5c9d5', extraction).
narrative_ontology:cs_interpretation_layer_present('ff35384e-0686-4782-afdd-78fdb4c5c9d5').
narrative_ontology:cs_reading_relation('ff35384e-0686-4782-afdd-78fdb4c5c9d5', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('ff35384e-0686-4782-afdd-78fdb4c5c9d5', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('ff35384e-0686-4782-afdd-78fdb4c5c9d5', foundational, national_security_overrides_absolute_prohibition).
narrative_ontology:cs_axiom_status(national_security_overrides_absolute_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('ff35384e-0686-4782-afdd-78fdb4c5c9d5', national_security_overrides_absolute_prohibition, instrumental).
narrative_ontology:cs_axiom('ff35384e-0686-4782-afdd-78fdb4c5c9d5', foundational, humane_treatment_is_context_dependent).
narrative_ontology:cs_axiom_status(humane_treatment_is_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('ff35384e-0686-4782-afdd-78fdb4c5c9d5', humane_treatment_is_context_dependent, conventional).
narrative_ontology:cs_reference_frame('ff35384e-0686-4782-afdd-78fdb4c5c9d5', state_sovereignty_and_security_priority).
narrative_ontology:cs_drift_state('ff35384e-0686-4782-afdd-78fdb4c5c9d5', post_9_11_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ff35384e-0686-4782-afdd-78fdb4c5c9d5', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_security_decision_makers).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_of_national_security_interest).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, human_rights_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading permits actions that would otherwise be prohibited, extracting dignity and rights from detainees. Suppression (0.75) is high due to the state's coercive power and the secrecy surrounding 'enhanced interrogation' practices. Theater ratio (0.4) reflects the ongoing rhetorical efforts to frame these practices as compliant with international law, despite their deviation from an absolute standard. Resistance (0.5) is moderate, coming primarily from human rights organizations and some international bodies, but often overridden by state claims of necessity. Accessibility collapse (0.6) is significant for detainees, as their legal and physical options are severely curtailed once designated a 'national security interest'.
 *
 * PERSPECTIVAL GAP:
 *   State security agencies and national security decision-makers experience this as a necessary flexibility (beneficiary seat), allowing them to protect national interests. Detainees and human rights advocates (payer seats) experience it as a dangerous erosion of fundamental rights, leading to abuse. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State security agencies and national security decision-makers are clear beneficiaries (d near 0.0) as they gain operational discretion and avoid accountability for certain actions. Detainees are direct targets (d near 1.0) as they bear the full cost of conditional treatment. Human rights advocates are also targets (d near 0.7) as their mission is undermined and they expend resources to resist this interpretation. International legal bodies are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling pure extraction as coordination by highlighting the conditional nature of 'humane treatment'. It exposes how a coordination problem (managing conflict) is leveraged to extract from a specific group (detainees) under the guise of 'necessity'. The constraint's mandate (national security) is used to justify practices that erode the very standards it purports to uphold, indicating a drift towards extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine interpretation of Common Article 3, or a re-framing to permit otherwise prohibited actions?',
    'Analysis of international jurisprudence and state practice over time, particularly in non-belligerent contexts.',
    'If a genuine interpretation, it highlights the flexibility of IHL; if a re-framing, it exposes a snare operating under the guise of legal interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''contextual_necessity'' reading of the ''humane_treatment_standard'' kernel.').

omega_variable(
    victim_set_definition,
    'Does the ''contextual necessity'' reading genuinely narrow the victim set, or does it merely provide cover for broader application of enhanced interrogation?',
    'Empirical review of interrogation practices and outcomes, comparing cases where ''necessity'' was invoked versus those where it was not.',
    'If the victim set is genuinely narrowed, the constraint is less extractive than it appears; if not, its extractiveness is higher and more widespread.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_definition, empirical, 'The ''contextual_necessity'' reading shrinks the victim set to exclude high-value targets in necessity scenarios, making detainee protections conditional.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/institutional barriers) or internalized (detainees'' belief in the legitimacy of their treatment)?',
    'Post-release psychological assessment and legal aid access: if suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in detainee treatment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.3).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__contextual_necessity, theater_ratio, 5, 0.35).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__contextual_necessity, theater_ratio, 10, 0.38).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__contextual_necessity, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__contextual_necessity, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__contextual_necessity, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__contextual_necessity, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__contextual_necessity, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__contextual_necessity, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__contextual_necessity, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, state_secrecy_doctrine).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, military_justice_system).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'humane_treatment_standard' kernel (Common Article 3). Its extractiveness and suppression metrics differ significantly from the 'absolute_prohibition' reading, which would show negligible extraction and high suppression of prohibited acts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
