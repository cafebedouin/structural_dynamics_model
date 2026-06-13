% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public Health Primary Vaccine Mandate
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of vaccine
 *   mandates, where collective protection is prioritized over individual
 *   consent when voluntary compliance fails to achieve herd immunity and
 *   vulnerable populations face lethal exposure risk. It posits that the
 *   state has a legitimate interest, and even a duty, to compel vaccination
 *   to protect the most vulnerable and ensure the functioning of public
 *   health infrastructure. The constraint's operation involves active
 *   enforcement mechanisms to ensure compliance, leading to a high degree of
 *   extraction from those whose consent is overridden.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.7).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.8).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Primary Vaccine Mandate").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '335d9884-1605-438d-9c01-eac568e204ae').
narrative_ontology:cs_kernel_codification('335d9884-1605-438d-9c01-eac568e204ae', formalized).
narrative_ontology:cs_authority_grounding('335d9884-1605-438d-9c01-eac568e204ae', lineage).
narrative_ontology:cs_interpretation_layer_present('335d9884-1605-438d-9c01-eac568e204ae').
narrative_ontology:cs_reading_relation('335d9884-1605-438d-9c01-eac568e204ae', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('335d9884-1605-438d-9c01-eac568e204ae', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('335d9884-1605-438d-9c01-eac568e204ae', foundational, collective_protection_supersedes_individual_consent_in_crisis).
narrative_ontology:cs_axiom_status(collective_protection_supersedes_individual_consent_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('335d9884-1605-438d-9c01-eac568e204ae', collective_protection_supersedes_individual_consent_in_crisis, deontological).
narrative_ontology:cs_axiom('335d9884-1605-438d-9c01-eac568e204ae', foundational, state_has_duty_to_protect_public_health).
narrative_ontology:cs_axiom_status(state_has_duty_to_protect_public_health, holdable).
narrative_ontology:cs_axiom_grounding('335d9884-1605-438d-9c01-eac568e204ae', state_has_duty_to_protect_public_health, deontological).
narrative_ontology:cs_reference_frame('335d9884-1605-438d-9c01-eac568e204ae', public_health_emergency_response_framework).
narrative_ontology:cs_drift_state('335d9884-1605-438d-9c01-eac568e204ae', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('335d9884-1605-438d-9c01-eac568e204ae', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, healthcare_systems).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, general_public).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals_coerced).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed_without_mandates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the direct imposition on individual autonomy and the costs associated with compliance (e.g., lost employment, restricted access). Suppression (0.8) is high due to the coercive nature of mandates, which often involve legal penalties, employment termination, or exclusion from public spaces. The theater ratio is low (0.1) because the enforcement is direct and functional, not performative; the mandates genuinely aim to achieve public health outcomes. Resistance is high (0.75) due to strong opposition from individuals asserting bodily autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, this constraint is a necessary coordination mechanism to prevent widespread harm. From the perspective of unvaccinated individuals whose consent is overridden, it is a coercive imposition. The engine's classification will reflect this divergence, likely showing a Rope for beneficiaries and a Snare for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations and healthcare systems are primary beneficiaries (d near 0.0) as they are protected from lethal exposure and collapse. The general public also benefits from herd immunity. Unvaccinated individuals who are coerced into compliance are victims (d near 1.0) as their bodily autonomy is directly overridden. Immunocompromised individuals who would be exposed without mandates are also considered victims in the absence of this constraint, highlighting the trade-off.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate is tied to ongoing public health threats. However, if the underlying public health risk diminishes significantly, the justification for the mandate would erode, and its persistence would then be a candidate for mandatrophy, potentially reclassifying it as a Piton or Snare if enforcement continued without a live problem. The 'founding_problem_status' being 'live' is critical to its current classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a reading of the ''vaccine_mandate_balance'' kernel, specifically ''public_health_primary''?',
    'Analysis of the foundational axioms and their alignment with the ''public_health_primary'' position, contrasting with ''bodily_autonomy_primary'' and ''proportionality_reading''.',
    'If misidentified, the classification of extraction and suppression for affected agents would be inverted or misattributed, leading to incorrect policy recommendations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''vaccine_mandate_balance'' kernel, instantiating the ''public_health_primary'' position.').

omega_variable(
    vulnerable_populations_victim_status,
    'Are immunocompromised and other vulnerable populations victims of the absence of mandates, or beneficiaries of their presence?',
    'Empirical data on infection rates, hospitalization, and mortality in vulnerable groups during periods of high and low vaccine uptake, with and without mandates.',
    'If mandates are absent, vulnerable populations become victims of exposure risk, increasing the effective extraction of the ''bodily_autonomy_primary'' reading. Under this ''public_health_primary'' reading, they are beneficiaries of the mandate''s protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_populations_victim_status, empirical, 'Clarifies the victim/beneficiary status of vulnerable populations depending on mandate presence.').

omega_variable(
    consent_subordination_justification,
    'To what extent is the subordination of individual consent to collective protection justified by the severity of the public health threat and the efficacy of the intervention?',
    'Ongoing ethical and epidemiological review, public discourse, and legal challenges that test the limits of state power versus individual rights in public health crises.',
    'If the justification weakens, the ''public_health_primary'' reading''s legitimacy erodes, increasing the perceived extraction on unvaccinated individuals and potentially shifting the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_subordination_justification, preference, 'Examines the ethical justification for overriding individual consent for collective health.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__public_health_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__public_health_primary, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__public_health_primary, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__public_health_primary, suppression_requirement, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vaccine_mandate_balance' kernel. Each reading represents a distinct structural claim about the balance between individual rights and public health imperatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
