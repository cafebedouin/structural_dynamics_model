% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Speech Protection: Categorical Balancing Reading
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'categorical balancing' reading of First
 *   Amendment speech protection, where the judiciary defines categories of
 *   protected and unprotected speech through a case-by-case balancing of
 *   speech value against potential harm. This approach, exemplified by cases
 *   like Chaplinsky v. New Hampshire (1942) and its progeny, grants
 *   significant interpretive power to the courts. It is one of several
 *   competing readings of the First Amendment's scope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.65).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.7).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Speech Protection: Categorical Balancing Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '7c18743b-a811-4dee-9d57-96f95561e204').
narrative_ontology:cs_kernel_codification('7c18743b-a811-4dee-9d57-96f95561e204', fixed_text).
narrative_ontology:cs_authority_grounding('7c18743b-a811-4dee-9d57-96f95561e204', lineage).
narrative_ontology:cs_interpretation_layer_present('7c18743b-a811-4dee-9d57-96f95561e204').
narrative_ontology:cs_reading_relation('7c18743b-a811-4dee-9d57-96f95561e204', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7c18743b-a811-4dee-9d57-96f95561e204', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('7c18743b-a811-4dee-9d57-96f95561e204', foundational, speech_value_is_context_dependent).
narrative_ontology:cs_axiom_status(speech_value_is_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('7c18743b-a811-4dee-9d57-96f95561e204', speech_value_is_context_dependent, conventional).
narrative_ontology:cs_axiom('7c18743b-a811-4dee-9d57-96f95561e204', foundational, judicial_balancing_is_necessary).
narrative_ontology:cs_axiom_status(judicial_balancing_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('7c18743b-a811-4dee-9d57-96f95561e204', judicial_balancing_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('7c18743b-a811-4dee-9d57-96f95561e204', chaplinsky_balancing_framework).
narrative_ontology:cs_drift_state('7c18743b-a811-4dee-9d57-96f95561e204', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7c18743b-a811-4dee-9d57-96f95561e204', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, legal_profession).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, speakers_of_unpopular_speech).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the judicial process of defining categories and balancing interests imposes significant costs on speakers, particularly those whose speech is unpopular or from marginalized groups. Suppression (0.70) is also high, as the evolving and often unpredictable nature of these categories leads to self-censorship and active enforcement against speech deemed 'low-value.' The theater ratio (0.40) reflects that while the stated goal is to protect valuable speech, a substantial portion of the judicial activity is dedicated to maintaining interpretive control and managing the political fallout of controversial speech decisions, rather than purely facilitating expression.
 *
 * PERSPECTIVAL GAP:
 *   The institutional judiciary and legal profession experience this constraint as a necessary and legitimate mechanism for managing complex societal interests, reinforcing their authority and expertise. For minority speakers and those expressing unpopular views, it is experienced as an unpredictable and often suppressive force that limits their ability to engage in public discourse, with high costs for challenging restrictions.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary and legal profession are clear beneficiaries, gaining authority, complexity, and revenue from the system. Minority speakers and those of unpopular speech are victims, bearing the costs of suppression and unpredictability. Legal predictability itself is a victim, as the case-by-case approach inherently undermines clear rules. Public discourse is a mixed beneficiary, theoretically purified but practically narrowed by judicial gatekeeping.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (balancing speech and harm) is still live, but its operation has accumulated substantial extraction and suppression. The 'tangled_rope' classification reflects that while there's a genuine coordination function (defining speech boundaries), it's intertwined with asymmetric extraction of interpretive authority and legal fees, sustained by active enforcement and the suppression of alternative readings. The rising extractiveness and suppression over time suggest a drift towards a more extractive, less coordinative function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_predictability,
    'To what extent does judicial discretion in defining speech categories genuinely serve societal interests versus merely reinforcing judicial power and legal complexity?',
    'Comparative analysis of speech outcomes in jurisdictions with more absolutist or harm-limited approaches; empirical study of chilling effects on different speaker groups under the categorical balancing regime.',
    'If discretion primarily serves power, the constraint''s extractiveness is higher than measured, and its coordination function is more theatrical. If it genuinely serves societal interests, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_predictability, conceptual, 'Ambiguity in the purpose and effect of judicial discretion in speech categories.').

omega_variable(
    unprotected_speech_categories_stability,
    'Are the categories of ''unprotected'' speech (e.g., obscenity, incitement, true threats) stable and consistently applied, or do they shift with political and social pressures?',
    'Longitudinal legal analysis of case law evolution across different eras and political climates; content analysis of judicial opinions for shifts in definitional criteria.',
    'If categories are unstable, legal predictability is lower, and the effective suppression on speakers is higher due to increased uncertainty. This would push the constraint closer to a ''snare'' for speakers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unprotected_speech_categories_stability, empirical, 'Stability and consistency of ''unprotected'' speech categories.').

omega_variable(
    minority_speech_impact_disparity,
    'Does the categorical balancing approach disproportionately impact the speech of minority groups or those with less social power, even when their speech does not fall into traditionally ''unprotected'' categories?',
    'Empirical studies on the application of speech restrictions across different demographic and social groups; analysis of legal aid access and litigation success rates for marginalized speakers.',
    'If there is a disproportionate impact, the effective extractiveness and suppression for these groups are significantly higher than the aggregate measures suggest, indicating a more severe ''snare'' for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_speech_impact_disparity, empirical, 'Disparate impact of categorical balancing on minority speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1940, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(firs_tr_t1960, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(firs_tr_t1980, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(firs_be_t1940, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1940, 0.4).
narrative_ontology:measurement(firs_be_t1960, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(firs_be_t1980, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1940, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1940, 0.5).
narrative_ontology:measurement(firs_su_t1960, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(firs_su_t1980, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2000, 0.67).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'first_amendment_speech_protection' kernel. Its structural properties and metrics are distinct from the 'absolutist_reading' and 'harm_limited_reading' siblings, which are modeled as separate constraints due to differing ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
