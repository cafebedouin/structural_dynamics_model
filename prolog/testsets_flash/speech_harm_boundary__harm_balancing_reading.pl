% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Harm Boundary: Proportionality Balancing Reading
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'harm balancing' reading of the speech
 *   harm boundary kernel. It posits that speech is presumptively protected
 *   but can be restricted when it causes demonstrated harm, subject to a
 *   proportionality test. This reading acknowledges a genuine coordination
 *   problem (balancing rights) but involves active enforcement and
 *   identifiable payers (speakers whose speech is restricted) and
 *   beneficiaries (those protected from harm). The metrics reflect a moderate
 *   level of extraction and suppression, as the balancing act inherently
 *   involves some restriction on speech, but it is not absolute.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.45).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.3).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Harm Boundary: Proportionality Balancing Reading").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '8ceda879-7c3b-4308-88f6-fd5230e04da7').
narrative_ontology:cs_kernel_codification('8ceda879-7c3b-4308-88f6-fd5230e04da7', formalized).
narrative_ontology:cs_authority_grounding('8ceda879-7c3b-4308-88f6-fd5230e04da7', lineage).
narrative_ontology:cs_interpretation_layer_present('8ceda879-7c3b-4308-88f6-fd5230e04da7').
narrative_ontology:cs_reading_relation('8ceda879-7c3b-4308-88f6-fd5230e04da7', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ceda879-7c3b-4308-88f6-fd5230e04da7', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('8ceda879-7c3b-4308-88f6-fd5230e04da7', foundational, speech_presumptively_protected).
narrative_ontology:cs_axiom_status(speech_presumptively_protected, holdable).
narrative_ontology:cs_axiom_grounding('8ceda879-7c3b-4308-88f6-fd5230e04da7', speech_presumptively_protected, deontological).
narrative_ontology:cs_axiom('8ceda879-7c3b-4308-88f6-fd5230e04da7', foundational, demonstrated_harm_justifies_restriction).
narrative_ontology:cs_axiom_status(demonstrated_harm_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('8ceda879-7c3b-4308-88f6-fd5230e04da7', demonstrated_harm_justifies_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('8ceda879-7c3b-4308-88f6-fd5230e04da7', post_wwii_liberal_democracy_framework).
narrative_ontology:cs_drift_state('8ceda879-7c3b-4308-88f6-fd5230e04da7', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8ceda879-7c3b-4308-88f6-fd5230e04da7', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, potential_victims_of_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, social_cohesion).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, advocates_for_unrestricted_speech).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).
:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while speakers bear costs, the restrictions are not arbitrary but tied to a demonstrable harm. Suppression (0.30) is also moderate, as enforcement is active but aims for precision rather than blanket bans. The accessibility collapse (0.60) indicates that while alternatives to harmful speech exist, the legal framework significantly narrows the scope of permissible expression. Resistance (0.40) is present from free speech advocates who contest the boundaries of 'harm'. The claimed type is 'tangled_rope' because it genuinely coordinates the conflicting interests of speakers and potential victims, but does so with asymmetric extraction (speakers pay for restrictions) and requires active enforcement by the judicial system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of potential victims, this constraint is a necessary 'rope' providing protection and fostering a safe environment. From the perspective of speakers whose speech is restricted, it can feel like a 'snare' due to the costs and limitations imposed. The judicial system, as the agenda-setter, views it as a 'tangled_rope' due to the complex balancing act and the need for continuous enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Potential victims of harmful speech are clear beneficiaries (d=0.0-0.2) as the constraint directly protects them. Speakers of harmful speech are targets (d=0.8-1.0) as they bear the costs of restriction. Advocates for unrestricted speech are also targets, bearing the costs of challenging the system. Social cohesion is a diffuse beneficiary. The judicial and legislative systems are agenda-setters, balancing coordination and enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging both the coordination function (balancing rights, protecting from harm) and the extractive component (restricting speech, imposing costs). It avoids the 'snare' label by emphasizing the proportionality and demonstrated harm requirements, which aim to prevent arbitrary extraction. It avoids the 'rope' label by recognizing the active enforcement and costs borne by speakers. The founding problem remains live, indicating no mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrated_harm_threshold,
    'What is the precise, empirically verifiable threshold for ''demonstrated harm'' that justifies speech restriction, and how consistently is it applied across different contexts and types of speech?',
    'Empirical legal studies analyzing judicial decisions and their outcomes, coupled with interdisciplinary research on the causal links between speech and harm.',
    'A clear, consistently applied threshold would reduce extractiveness for speakers and increase the ''rope'' aspect of coordination. An inconsistent or vague threshold would increase extractiveness and suppression, pushing the classification closer to a ''snare'' for speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrated_harm_threshold, empirical, 'Ambiguity in the ''demonstrated harm'' standard and its application.').

omega_variable(
    proportionality_test_objectivity,
    'To what extent is the proportionality balancing test an objective, rule-bound application of principles, versus a subjective, outcome-oriented exercise influenced by judicial or societal biases?',
    'Comparative legal analysis across jurisdictions and time, examining the consistency of outcomes for similar cases, and critical legal studies analyzing the underlying assumptions of the balancing test.',
    'If the test is largely subjective, the constraint''s legitimacy as a fair balancing mechanism is undermined, increasing its perceived extractiveness and suppression for speakers. If objective, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_objectivity, conceptual, 'Objectivity of the proportionality balancing test.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''harm balancing'' reading sufficiently distinct from the ''dignity'' reading, or do they converge in practice when addressing certain categories of speech (e.g., hate speech)?',
    'Detailed comparative case law analysis, examining whether the legal reasoning and outcomes differ significantly when applying a harm balancing test versus a dignity-based test to similar factual scenarios.',
    'If they converge, the conceptual distinction between the readings is weakened, suggesting a single underlying constraint with different justifications. If they diverge, the distinct structural implications of each reading are reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinction between harm balancing and dignity readings in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(spee_be_t1950, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(spee_be_t1970, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1950, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(spee_su_t1970, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2010, 0.29).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_harm_boundary' kernel. It is structurally distinct from the 'absolutist_reading' (speech protection near-absolute) and the 'dignity_reading' (speech protection subordinate to human dignity), which are modeled as separate constraints due to differing epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
