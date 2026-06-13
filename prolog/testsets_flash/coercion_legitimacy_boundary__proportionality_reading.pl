% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint, the 'proportionality_reading' of the
 *   'coercion_legitimacy_boundary' kernel, asserts that the legitimacy of
 *   public health coercion (e.g., vaccine mandates, quarantines) is not
 *   absolute but scales with the severity and transmissibility of the
 *   disease. For highly severe and transmissible diseases like measles,
 *   coercion is deemed legitimate; for less severe ones like seasonal flu, it
 *   is not. This reading attempts to balance collective public health with
 *   individual autonomy through a case-by-case assessment, leading to a
 *   variable victim set depending on the pathogen.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.6).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality Principle for Public Health Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, 'd38f190d-d5e0-4983-869f-176a32fc6437').
narrative_ontology:cs_kernel_codification('d38f190d-d5e0-4983-869f-176a32fc6437', formalized).
narrative_ontology:cs_authority_grounding('d38f190d-d5e0-4983-869f-176a32fc6437', lineage).
narrative_ontology:cs_interpretation_layer_present('d38f190d-d5e0-4983-869f-176a32fc6437').
narrative_ontology:cs_reading_relation('d38f190d-d5e0-4983-869f-176a32fc6437', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('d38f190d-d5e0-4983-869f-176a32fc6437', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('d38f190d-d5e0-4983-869f-176a32fc6437', foundational, coercion_must_be_necessary_and_proportional).
narrative_ontology:cs_axiom_status(coercion_must_be_necessary_and_proportional, holdable).
narrative_ontology:cs_axiom_grounding('d38f190d-d5e0-4983-869f-176a32fc6437', coercion_must_be_necessary_and_proportional, deontological).
narrative_ontology:cs_axiom('d38f190d-d5e0-4983-869f-176a32fc6437', foundational, disease_severity_and_transmissibility_are_relevant_factors).
narrative_ontology:cs_axiom_status(disease_severity_and_transmissibility_are_relevant_factors, holdable).
narrative_ontology:cs_axiom_grounding('d38f190d-d5e0-4983-869f-176a32fc6437', disease_severity_and_transmissibility_are_relevant_factors, empirically_contingent).
narrative_ontology:cs_reference_frame('d38f190d-d5e0-4983-869f-176a32fc6437', constitutional_proportionality_doctrine).
narrative_ontology:cs_drift_state('d38f190d-d5e0-4983-869f-176a32fc6437', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d38f190d-d5e0-4983-869f-176a32fc6437', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, general_public).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate public health crises by establishing a framework for when individual liberties can be curtailed for the greater good, ensuring a baseline of public safety against infectious diseases.
% TRANSFER_FUNCTION: Transfers a portion of individual autonomy and liberty from individuals to the state (public health authorities) in exchange for collective protection from severe infectious diseases. The extent of this transfer is proportional to the disease threat.
% ABSENT_VOICES: Those who advocate for absolute bodily autonomy, regardless of public health risk, are often marginalized in policy discussions that adopt this proportionality framework. They would argue that no disease severity justifies involuntary medical intervention.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, public health responses would either become overly coercive (if public health primary prevailed) or entirely ineffective (if bodily autonomy primary prevailed), leading to a breakdown in disease control or a severe erosion of individual rights. Society would have to re-establish a new balance.
% FOUNDING_PROBLEM: The historical tension between individual liberty and collective safety during infectious disease outbreaks, where unchecked individual action could lead to widespread harm, and unchecked state power could lead to tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, ethicists, and public health historians widely corroborate the enduring nature of this tension. Judicial precedents in constitutional law (e.g., Jacobson v. Massachusetts) and ongoing debates during pandemics attest to the problem's live status, from sources outside the direct beneficiaries of public health mandates.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates collective action to prevent disease spread (benefiting the general public) but does so through asymmetric extraction (mandates on individuals). Extractiveness is moderate (0.45) due to the inherent burden of mandates, but not extreme because the principle aims to limit coercion. Suppression (0.6) is necessary to enforce mandates, but not total, as legal challenges and exemptions exist. The low theater ratio (0.1) reflects that the enforcement is generally functional, not performative, when applied proportionally.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities (agenda-setters) view this as a necessary and just framework for protecting the population, experiencing it as a Rope. Individuals subject to mandates, particularly for diseases they perceive as low risk, experience it as a Snare due to the direct imposition on their autonomy. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the general public are beneficiaries (d near 0.0) as they gain collective protection. Individuals subject to mandates are targets (d near 1.0) as they bear the direct costs of compliance. The proportionality principle aims to modulate this directionality, shifting it closer to symmetric for less severe threats and further towards target for severe ones.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality principle is designed to prevent mandatrophy by ensuring that coercive measures are only applied when the founding problem (severe disease threat) is 'live.' If coercion were applied indiscriminately to low-severity diseases, the constraint would drift towards a Snare, as its justification would become theatrical or purely extractive. The case-by-case adjudication is meant to keep the mandate 'live' only when truly needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine application of proportionality, or a cover for overreach?',
    'Judicial review of specific mandates against established proportionality tests (necessity, suitability, strict sense proportionality).',
    'If genuinely proportional, the constraint functions as a legitimate Tangled Rope balancing rights; if overreach, it shifts towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''proportionality_reading'' of the ''coercion_legitimacy_boundary'' kernel.').

omega_variable(
    severity_threshold_ambiguity,
    'Where is the precise threshold of disease severity and transmissibility that justifies coercion?',
    'Consensus among epidemiologists, ethicists, and legal scholars, potentially codified in legislation or judicial precedent.',
    'A clearer threshold would reduce arbitrary application and increase predictability, potentially lowering perceived extractiveness for those near the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_ambiguity, empirical, 'Ambiguity in defining the ''severity'' and ''transmission'' metrics for proportionality.').

omega_variable(
    victim_set_variability,
    'How does the victim set change with different pathogens, and is this change consistently applied?',
    'Comparative analysis of mandate application across different disease outbreaks and jurisdictions.',
    'Inconsistent application of the proportionality principle would indicate a drift towards arbitrary extraction, potentially reclassifying the constraint as a Snare for certain populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_variability, empirical, 'The victim set (individuals subject to mandates) varies significantly based on the pathogen''s characteristics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'coercion_legitimacy_boundary' kernel, focusing on the proportionality of public health interventions. It is distinct from 'bodily_autonomy_primary' (categorical rejection of coercion) and 'public_health_primary' (categorical prioritization of collective health).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
