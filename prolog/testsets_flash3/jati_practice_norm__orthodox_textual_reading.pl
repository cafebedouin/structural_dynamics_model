% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Jati Boundaries as Fixed Scriptural Varna Framework (Orthodox Textual Reading)
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'orthodox textual reading' of jati
 *   boundaries, where social hierarchy and occupational roles are seen as
 *   divinely ordained by fixed scriptural varna frameworks. Deviation from
 *   these roles is considered ritual pollution. This reading emphasizes the
 *   immutability and sacred nature of the system, justifying high extraction
 *   and severe suppression of lower-jati and Dalit communities. The claimed
 *   type is 'snare' because the coordination story (divine order, ritual
 *   purity) serves as cover for a system of pure extraction and enforced
 *   immobility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.9).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.95).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Boundaries as Fixed Scriptural Varna Framework (Orthodox Textual Reading)").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '4effb73c-c98c-4f3a-9c99-b84fa6e3303e').
narrative_ontology:cs_kernel_codification('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', fixed_text).
narrative_ontology:cs_authority_grounding('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', lineage).
narrative_ontology:cs_interpretation_layer_present('4effb73c-c98c-4f3a-9c99-b84fa6e3303e').
narrative_ontology:cs_reading_relation('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', foundational, varna_is_divinely_ordained_and_immutable).
narrative_ontology:cs_axiom_status(varna_is_divinely_ordained_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', varna_is_divinely_ordained_and_immutable, theological).
narrative_ontology:cs_axiom('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', foundational, deviation_from_jati_duty_is_ritual_pollution).
narrative_ontology:cs_axiom_status(deviation_from_jati_duty_is_ritual_pollution, holdable).
narrative_ontology:cs_axiom_grounding('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', deviation_from_jati_duty_is_ritual_pollution, deontological).
narrative_ontology:cs_reference_frame('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', ancient_vedic_social_order).
narrative_ontology:cs_drift_state('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', contemporary_india, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4effb73c-c98c-4f3a-9c99-b84fa6e3303e', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, upper_jati_elites).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, lower_jati_communities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, dalit_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces scriptural injunctions regarding varna and jati, deriving authority and social status from maintaining the purity and hierarchy of the system. Benefits from offerings and deference.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_priesthood, agenda_setter,
    institutional, generational, arbitrage, regional).

% Benefit from the social and economic privileges conferred by their high ritual status, including access to resources, education, and political power. Their position is legitimized by the scriptural framework.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, upper_jati_elites, beneficiary,
    powerful, generational, mobile, regional).

% Are assigned specific, often ritually 'polluting' occupations, limiting their social mobility and economic opportunities. They are bound by social norms and fear of ostracization or divine retribution for deviation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, lower_jati_communities, payer,
    powerless, generational, identity_locked, local).

% Are considered outside the varna system, facing extreme social exclusion, discrimination, and violence. Their labor is exploited, and their attempts at upward mobility are met with severe resistance. Exit is virtually impossible within the traditional social structure.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dalit_communities, payer,
    powerless, generational, trapped, local).

% Advocate for the abolition of caste discrimination and the dismantling of the varna system, often challenging the scriptural interpretations that legitimize it. They face significant social and political opposition.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, social_reformers, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rigid social order and division of labor, ensuring specific tasks are performed and maintaining ritual purity through prescribed roles and interactions.
% TRANSFER_FUNCTION: Transfers social status, economic resources, and ritual authority from lower-jati and Dalit communities to upper-jati elites and the priesthood, in exchange for a perceived stable social order.
% ABSENT_VOICES: Historical and contemporary voices from lower-jati and Dalit communities, who have consistently challenged the legitimacy and fairness of the system, are often silenced or dismissed as illegitimate interpretations of scripture.
% DISAPPEARANCE_RATIONALE: If the scripturally-derived jati framework vanished overnight, the social hierarchy, economic distribution, and ritual practices would undergo profound and rapid reorganization. The power and privilege of upper jatis would collapse, and lower jatis would gain unprecedented mobility and autonomy, leading to widespread social upheaval and redefinition of identity.
% FOUNDING_PROBLEM: To establish a divinely ordained social order, maintain ritual purity, and ensure the performance of essential societal functions through a hierarchical division of labor.
% FOUNDING_PROBLEM_CORROBORATION: The Brahmin priesthood and upper-jati elites assert the founding problem of maintaining dharma and social order is still live. Social reformers and lower-jati movements, corroborated by historical evidence of exploitation and discrimination, argue that the 'problem' was always a justification for extraction, and the system's persistence serves only to maintain power imbalances.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) due to the systematic deprivation of resources, opportunities, and dignity from lower-status groups. Suppression is extremely high (0.95) because the system relies on social ostracization, economic coercion, and even violence to prevent mobility and enforce ritual purity. The theater ratio is low (0.1) as the ritual practices are deeply integrated into daily life and serve a direct function in maintaining the social order, rather than being purely performative. Resistance is high (0.7) reflecting ongoing struggles by marginalized communities against the system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Brahmin priesthood and upper-jati elites, this system is a divinely sanctioned order that ensures social harmony and ritual purity. From the perspective of lower-jati and Dalit communities, it is a brutal system of oppression and exploitation. The engine's classification will highlight this divergence, showing a snare from the victims' seats and a perceived rope or mountain from the beneficiaries' seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priesthood and upper-jati elites are clear beneficiaries, deriving immense social, economic, and ritual power from this interpretation. Lower-jati and Dalit communities are the primary victims, trapped in a system that assigns them polluting occupations and denies them basic rights and mobility. Social reformers act as observers, analyzing and challenging the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_interpretation_ambiguity,
    'To what extent is the ''fixed scriptural varna framework'' an immutable divine command versus a historically contingent interpretation by the Brahmin priesthood?',
    'Comparative textual analysis across different historical periods and regional traditions, alongside archaeological and anthropological evidence of pre-scriptural social organization.',
    'If the framework is shown to be a contingent interpretation, it undermines the ''mountain'' claim of divine ordination, reclassifying it more firmly as a constructed snare. If truly immutable, it strengthens the claim of naturalness, though the extraction would still be present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_interpretation_ambiguity, empirical, 'Ambiguity in the immutability of scriptural interpretation.').

omega_variable(
    internalized_suppression_vs_structural,
    'What proportion of the measured suppression is due to internalized beliefs (fear of pollution, karma) versus external structural barriers (violence, economic exclusion)?',
    'Longitudinal studies of communities after external structural barriers are removed (e.g., legal protections, economic uplift programs): if suppression persists, it indicates internalized components.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher and more resilient to external reforms, requiring different intervention strategies. If primarily structural, legal and economic reforms would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_vs_structural, empirical, 'Structural vs. internalized suppression mechanism in jati system.').

omega_variable(
    coordination_function_legitimacy,
    'Does the ''coordination function'' of maintaining social order genuinely benefit society as a whole, or is it primarily a justification for the beneficiaries'' extractive practices?',
    'Analysis of social welfare outcomes (health, education, economic stability) across different social systems, comparing societies with rigid caste systems to those with more fluid social structures.',
    'If the coordination function is found to be a cover, the constraint is a pure snare. If it genuinely provides some societal benefit, it might lean towards a tangled rope, though the high extraction and suppression would still dominate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_legitimacy, conceptual, 'Whether the coordination function is legitimate or a cover for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__orthodox_textual_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__orthodox_textual_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(jati_tr_t80, jati_practice_norm__orthodox_textual_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__orthodox_textual_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 60, 0.9).
narrative_ontology:measurement(jati_be_t80, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 80, 0.9).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 100, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.92).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 40, 0.93).
narrative_ontology:measurement(jati_su_t60, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 60, 0.94).
narrative_ontology:measurement(jati_su_t80, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 80, 0.95).
narrative_ontology:measurement(jati_su_t100, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
