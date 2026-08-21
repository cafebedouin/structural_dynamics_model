% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership (Social Role Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint describes gendered category membership as grounded in
 *   sustained social performance and recognition by others. It is one reading
 *   of the broader 'gendered_category_membership' kernel. Under this reading,
 *   individuals gain or lose membership in gendered categories based on how
 *   they present themselves and how that presentation is interpreted and
 *   validated by their social environment. This creates a coordination
 *   function for social interaction but also imposes significant performance
 *   costs and gatekeeping mechanisms.
 *
 * KEY AGENTS:
 *   - social_gatekeepers: Agenda-setter (institutional/constrained) — enforce norms, grant/deny recognition
 *   - cis_gender_individuals: Beneficiary (powerful/mobile) — benefit from stable categories, generally recognized
 *   - trans_gender_individuals: Payer (powerless/identity_locked) — bear performance costs, risk exclusion
 *   - non_conforming_individuals: Payer (powerless/constrained) — bear social costs for non-adherence
 *   - gender_theorists: Observer (analytical/analytical) — analyze the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.45).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.6).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership (Social Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, 'e6d0c293-cc32-4a84-8e09-05a8654d923a').
narrative_ontology:cs_kernel_codification('e6d0c293-cc32-4a84-8e09-05a8654d923a', implicit).
narrative_ontology:cs_authority_grounding('e6d0c293-cc32-4a84-8e09-05a8654d923a', practice).
narrative_ontology:cs_interpretation_layer_present('e6d0c293-cc32-4a84-8e09-05a8654d923a').
narrative_ontology:cs_reading_relation('e6d0c293-cc32-4a84-8e09-05a8654d923a', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6d0c293-cc32-4a84-8e09-05a8654d923a', gendered_category_membership__gender_identity_reading, influences).
narrative_ontology:cs_axiom('e6d0c293-cc32-4a84-8e09-05a8654d923a', foundational, social_performance_is_constitutive_of_gender).
narrative_ontology:cs_axiom_status(social_performance_is_constitutive_of_gender, holdable).
narrative_ontology:cs_axiom_grounding('e6d0c293-cc32-4a84-8e09-05a8654d923a', social_performance_is_constitutive_of_gender, conventional).
narrative_ontology:cs_axiom('e6d0c293-cc32-4a84-8e09-05a8654d923a', foundational, social_recognition_is_necessary_for_membership).
narrative_ontology:cs_axiom_status(social_recognition_is_necessary_for_membership, holdable).
narrative_ontology:cs_axiom_grounding('e6d0c293-cc32-4a84-8e09-05a8654d923a', social_recognition_is_necessary_for_membership, conventional).
narrative_ontology:cs_reference_frame('e6d0c293-cc32-4a84-8e09-05a8654d923a', traditional_gender_roles_and_expectations).
narrative_ontology:cs_drift_state('e6d0c293-cc32-4a84-8e09-05a8654d923a', contemporary_gender_fluidity_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6d0c293-cc32-4a84-8e09-05a8654d923a', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_gender_individuals).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, social_gatekeepers).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_gender_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, non_conforming_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and institutions who actively define, monitor, and enforce social norms of gender performance and recognition, determining who 'counts' as a member of a gendered category. They benefit from the stability and predictability of these categories.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_gatekeepers, agenda_setter,
    institutional, generational, constrained, global).

% Individuals whose gender identity aligns with their sex assigned at birth and whose social performance of gender is generally recognized without question. They benefit from the clarity and stability of gendered categories and the exclusion of those who do not conform.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_gender_individuals, beneficiary,
    powerful, biographical, mobile, global).

% Individuals whose gender identity differs from their sex assigned at birth. They bear the significant social and emotional costs of performing gender roles to gain recognition and acceptance, often facing gatekeeping and exclusion if their performance is deemed insufficient or inauthentic.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_gender_individuals, payer,
    powerless, biographical, identity_locked, global).

% Individuals who may identify with a gender but do not adhere to traditional gender roles or expressions. They bear social costs, stigma, and potential exclusion for failing to perform gender in ways that are readily recognized by social gatekeepers.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, non_conforming_individuals, payer,
    powerless, biographical, constrained, global).

% Academics and researchers who analyze the social construction, performance, and recognition of gender, often critiquing its mechanisms and effects on individuals and society.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared social categories for gender, enabling predictable social interactions, roles, and access to gender-segregated spaces based on observable performance and recognition by others.
% TRANSFER_FUNCTION: Transfers social legitimacy, access, and recognition to those who successfully perform and are recognized as members of a gendered category, while extracting performance labor, conformity, and potential exclusion from those who do not or cannot.
% ABSENT_VOICES: Those who reject the premise of gender as a social performance, or those whose performance is consistently unrecognized, are often excluded from the discourse about the criteria for gendered category membership. Their experiences of exclusion are often framed as individual failures rather than systemic issues.
% DISAPPEARANCE_RATIONALE: If gendered category membership based on social role and recognition vanished overnight, social interactions would lose a fundamental organizing principle. Roles, expectations, and access to gender-segregated spaces (e.g., bathrooms, sports, social clubs) would become profoundly ambiguous, leading to widespread social reorganization.
% FOUNDING_PROBLEM: The need for stable, recognizable social categories to organize human interaction, division of labor, and social hierarchy, providing a framework for social roles and expectations.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists, anthropologists, and historians attest to the pervasive and enduring role of gendered social categories in organizing human societies across cultures and time. While the specific performances and recognition criteria evolve, the underlying function of social categorization remains.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).
:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45, rising to 0.52) due to the ongoing performance demands and the social costs of non-recognition. Suppression is moderate-high (0.60, rising to 0.66) because social gatekeeping and the pressure to conform are pervasive and actively enforced through social sanction and exclusion. Theater ratio is moderate (0.40, rising to 0.45) as the performance of gender is a genuine social function, but increasingly self-conscious and subject to scrutiny, with some enforcement activity focused on maintaining the 'performance' itself rather than core social coordination. The metrics show a gradual increase over time, reflecting growing contestation and explicit enforcement of gender norms in response to challenges to traditional categories.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social gatekeepers and many cisgender individuals, this constraint provides a natural and necessary order for social life. From the perspective of transgender and non-conforming individuals, it operates as a demanding and often arbitrary system of performance and recognition that can lead to significant harm and exclusion. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Social gatekeepers and cisgender individuals are structural beneficiaries, as they benefit from the stability and predictability of gendered categories and often face fewer performance demands. Transgender and non-conforming individuals are targets, bearing the costs of performance, potential misrecognition, and exclusion. Their 'identity_locked' exit option reflects the deep personal stakes in gender identity, making exit from the system of recognition profoundly difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's core mandate of organizing social interaction through gendered categories remains 'live'. However, the specific mechanisms of 'sustained social performance and recognition' are increasingly contested. The rising extractiveness and suppression indicate that while the mandate persists, the constraint's operation has become more burdensome and coercive for certain groups, suggesting a drift towards a more extractive form of coordination rather than a complete atrophy of function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinction,
    'This constraint is one reading of the ''gendered_category_membership'' kernel. How does this ''social_role_reading'' differ structurally from the ''biological_sex_reading'' and ''gender_identity_reading''?',
    'Comparative analysis of the core axioms, beneficiary/victim sets, and enforcement mechanisms across all three readings.',
    'Clarifies the specific structural claims and consequences of each reading, preventing conflation and enabling precise classification of each distinct constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing this reading from other interpretations of gendered category membership.').

omega_variable(
    social_recognition_criteria_ambiguity,
    'What constitutes ''recognition'' in practice, and whose recognition holds authority? Is it a subjective feeling, a community consensus, or institutional validation?',
    'Empirical sociological studies of how gender is recognized in different social contexts and the power dynamics involved in granting or withholding recognition.',
    'If recognition is primarily institutional, the constraint''s ''suppression'' and ''extractiveness'' are higher due to centralized gatekeeping. If it''s diffuse, the ''theater_ratio'' might be higher due to constant, uncodified performance demands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_recognition_criteria_ambiguity, empirical, 'Ambiguity in the criteria and authority of social recognition for gender.').

omega_variable(
    performance_authenticity_vs_coercion,
    'To what extent is the ''sustained social performance'' of gender an authentic expression of self, versus a coerced act to gain social acceptance and avoid harm?',
    'Qualitative research and lived experience narratives from individuals navigating gendered social expectations, particularly those who face high stakes for non-conformity.',
    'If performance is largely coerced, the ''extractiveness'' and ''suppression'' metrics are higher than currently estimated, reflecting the hidden costs of forced conformity. If it''s primarily authentic, the ''theater_ratio'' would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_authenticity_vs_coercion, empirical, 'The balance between authentic expression and coerced conformity in gender performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1950, gendered_category_membership__social_role_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(gend_tr_t1965, gendered_category_membership__social_role_reading, theater_ratio, 1965, 0.33).
narrative_ontology:measurement(gend_tr_t1980, gendered_category_membership__social_role_reading, theater_ratio, 1980, 0.36).
narrative_ontology:measurement(gend_tr_t1995, gendered_category_membership__social_role_reading, theater_ratio, 1995, 0.39).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__social_role_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(gend_tr_t2025, gendered_category_membership__social_role_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(gend_be_t1950, gendered_category_membership__social_role_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(gend_be_t1965, gendered_category_membership__social_role_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(gend_be_t1980, gendered_category_membership__social_role_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(gend_be_t1995, gendered_category_membership__social_role_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__social_role_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(gend_be_t2025, gendered_category_membership__social_role_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1950, gendered_category_membership__social_role_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(gend_su_t1965, gendered_category_membership__social_role_reading, suppression_requirement, 1965, 0.53).
narrative_ontology:measurement(gend_su_t1980, gendered_category_membership__social_role_reading, suppression_requirement, 1980, 0.57).
narrative_ontology:measurement(gend_su_t1995, gendered_category_membership__social_role_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__social_role_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(gend_su_t2025, gendered_category_membership__social_role_reading, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_access_to_spaces).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_labor_roles).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'gendered_category_membership' kernel, each with its own structural properties and classification. This reading focuses on social performance and recognition, influencing and coexisting with biological and identity-based readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
