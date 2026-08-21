% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Sex/Gender Category Membership (Medical Gatekeeping Model)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid reading' of sex/gender category
 *   membership, where recognition is determined by a combination of
 *   biological factors and medical/social transition, enforced by medical
 *   gatekeeping. It is one reading of the broader 'sex_gender_category'
 *   kernel. The constraint is claimed as a Tangled Rope, reflecting its dual
 *   function of providing a pathway for transition while imposing significant
 *   costs and exclusions. The metrics reflect high extraction and suppression
 *   due to the gatekeeping model.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.75).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.8).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership (Medical Gatekeeping Model)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'a8007992-9271-4f15-9670-2fe65d544cc4').
narrative_ontology:cs_kernel_codification('a8007992-9271-4f15-9670-2fe65d544cc4', formalized).
narrative_ontology:cs_authority_grounding('a8007992-9271-4f15-9670-2fe65d544cc4', expertise).
narrative_ontology:cs_interpretation_layer_present('a8007992-9271-4f15-9670-2fe65d544cc4').
narrative_ontology:cs_reading_relation('a8007992-9271-4f15-9670-2fe65d544cc4', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8007992-9271-4f15-9670-2fe65d544cc4', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_axiom('a8007992-9271-4f15-9670-2fe65d544cc4', foundational, sex_is_medically_mutable).
narrative_ontology:cs_axiom_status(sex_is_medically_mutable, holdable).
narrative_ontology:cs_axiom_grounding('a8007992-9271-4f15-9670-2fe65d544cc4', sex_is_medically_mutable, empirically_contingent).
narrative_ontology:cs_axiom('a8007992-9271-4f15-9670-2fe65d544cc4', foundational, medical_gatekeeping_ensures_category_integrity).
narrative_ontology:cs_axiom_status(medical_gatekeeping_ensures_category_integrity, holdable).
narrative_ontology:cs_axiom_grounding('a8007992-9271-4f15-9670-2fe65d544cc4', medical_gatekeeping_ensures_category_integrity, conventional).
narrative_ontology:cs_reference_frame('a8007992-9271-4f15-9670-2fe65d544cc4', binary_medical_transition_pathway).
narrative_ontology:cs_drift_state('a8007992-9271-4f15-9670-2fe65d544cc4', contemporary_identity_politics_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a8007992-9271-4f15-9670-2fe65d544cc4', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transgender_individuals_seeking_recognition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_binary_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the medical and psychological criteria for gender transition and legal sex change. Benefits from the authority and funding associated with this gatekeeping role. Justifies its role as ensuring appropriate care and maintaining social coherence.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Must navigate complex, costly, and often lengthy medical and psychological processes to gain recognition in their affirmed gender. Bears significant financial, emotional, and temporal burdens. Exit means abandoning legal and social recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, transgender_individuals_seeking_recognition, payer,
    powerless, biographical, trapped, global).

% Are often excluded or poorly served by a binary medical gatekeeping model that primarily recognizes male-to-female or female-to-male transitions. Their identities are not easily accommodated, leading to lack of recognition and care.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_binary_individuals, excluded,
    powerless, biographical, identity_locked, global).

% Benefit from the perceived stability and clarity of gender categories, particularly in sex-segregated spaces and legal contexts. The gatekeeping model reinforces traditional understandings of sex and gender, which aligns with their existing social structures.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_individuals, beneficiary,
    powerful, generational, mobile, global).

% Advocate for self-identification and reduced medical gatekeeping, arguing that the current model is overly burdensome and infringes on bodily autonomy. They challenge the authority of medical institutions in defining gender.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gender_identity_advocates, observer,
    organized, biographical, constrained, global).

% Advocate for a strict biological definition of sex, often viewing medical transition as insufficient to change fundamental sex categories. They may see the hybrid model as already too permissive.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, biological_sex_advocates, observer,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, medically-mediated pathway for individuals to align their legal and social gender with their internal identity, aiming for social coherence and access to gender-segregated spaces/resources.
% TRANSFER_FUNCTION: Transfers authority over gender category recognition from individuals to medical and legal institutions. Transfers significant financial, emotional, and temporal costs to transitioning individuals.
% ABSENT_VOICES: Non-transitioning trans individuals, non-binary individuals who do not fit binary medical models, and those advocating for self-identification without medical gatekeeping. They would argue for bodily autonomy and self-determination.
% DISAPPEARANCE_RATIONALE: If medical gatekeeping vanished, gender categories would immediately become more fluid, self-identification would become the de facto standard, and medical institutions would lose their gatekeeping power. Social norms and legal frameworks would have to rapidly adapt to a new paradigm of gender recognition.
% FOUNDING_PROBLEM: To manage the social and legal implications of gender transition, providing a recognized pathway for individuals to change legal sex while maintaining some perceived stability in sex-segregated categories.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and some cisgender groups attest the problem is still live, citing concerns about category coherence and appropriate medical care. Trans advocates and some legal scholars attest the founding problem has shifted from managing transition to gatekeeping identity, and the current model creates more problems than it solves; this is supported by patient advocacy reports and legal challenges.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high due to the financial, emotional, and physical costs imposed on individuals seeking recognition through medical transition. Suppression is also high, as the model actively excludes those who do not or cannot medically transition from full category recognition, and it suppresses alternative, less gatekept pathways. The theater ratio is moderate, acknowledging genuine medical care while also noting the performative aspects of 'proving' one's gender to gatekeepers. The increasing extractiveness and theater ratio over time reflect growing scrutiny and resistance to the gatekeeping model, suggesting its function is becoming more about maintaining institutional authority than pure coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of medical institutions, this is a necessary coordination mechanism for managing complex transitions and maintaining social order. From the perspective of transgender and non-binary individuals, it is an extractive and suppressive system that imposes undue burdens and denies self-determination. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical institutions are beneficiaries and agenda-setters, controlling the process and deriving authority. Cisgender individuals are beneficiaries, as the model reinforces categories that benefit them. Transgender individuals seeking recognition are targets/payers, bearing the costs of the process. Non-binary individuals are largely excluded, as the model often fails to accommodate their identities. The structural relationships directly inform these directionalities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_gatekeeping,
    'To what extent are the medical interventions and gatekeeping processes genuinely necessary for the well-being of transitioning individuals, versus serving to maintain institutional authority over gender categories?',
    'Longitudinal studies comparing health outcomes and social integration for individuals transitioning under different models (e.g., informed consent vs. gatekeeping), and analysis of the historical evolution of medical protocols.',
    'If gatekeeping is found to be largely for institutional control, the constraint''s extractiveness and suppression would be re-evaluated as higher, and its coordination function as lower. If medically essential, the coordination function would be stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_gatekeeping, empirical, 'Distinguishing medical necessity from institutional control in gender transition pathways.').

omega_variable(
    category_coherence_definition,
    'What constitutes ''category coherence'' in the context of sex and gender, and whose definition of coherence is being enforced by this constraint?',
    'Sociological and philosophical analysis of competing definitions of sex/gender categories, and examination of the power dynamics underlying the adoption of specific definitions in legal and medical frameworks.',
    'If ''coherence'' is found to be a proxy for maintaining traditional gender norms, the constraint''s justification as a coordination mechanism would weaken, and its extractive nature would be amplified. If a broad consensus on coherence exists, the coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_coherence_definition, conceptual, 'Ambiguity in the definition and enforcement of sex/gender category coherence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., legal barriers, lack of access to care) or internalized (e.g., self-censorship, fear of social rejection) for transgender and non-binary individuals?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., in jurisdictions with self-ID), reclassify as partially internalized. Qualitative studies on lived experience.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint''s impact more pervasive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gender category recognition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t2000, sex_gender_category__hybrid_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(sex__tr_t2005, sex_gender_category__hybrid_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(sex__tr_t2010, sex_gender_category__hybrid_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(sex__tr_t2015, sex_gender_category__hybrid_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(sex__tr_t2020, sex_gender_category__hybrid_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(sex__tr_t2025, sex_gender_category__hybrid_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(sex__be_t2000, sex_gender_category__hybrid_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(sex__be_t2005, sex_gender_category__hybrid_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(sex__be_t2010, sex_gender_category__hybrid_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(sex__be_t2015, sex_gender_category__hybrid_reading, base_extractiveness, 2015, 0.73).
narrative_ontology:measurement(sex__be_t2020, sex_gender_category__hybrid_reading, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement(sex__be_t2025, sex_gender_category__hybrid_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t2000, sex_gender_category__hybrid_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(sex__su_t2005, sex_gender_category__hybrid_reading, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement(sex__su_t2010, sex_gender_category__hybrid_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(sex__su_t2015, sex_gender_category__hybrid_reading, suppression_requirement, 2015, 0.79).
narrative_ontology:measurement(sex__su_t2020, sex_gender_category__hybrid_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(sex__su_t2025, sex_gender_category__hybrid_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, gender_segregated_spaces_access).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, legal_sex_recognition).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (hybrid_reading) of the 'sex_gender_category' kernel, which also includes a biology-only reading and an identity-only reading. Each reading instantiates a distinct constraint with its own ε and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
