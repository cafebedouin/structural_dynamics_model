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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Sex/Gender Category Membership (Hybrid Medical Gatekeeping Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid' reading of sex/gender category
 *   membership, where recognition for transgender individuals is conditional
 *   on meeting specific medical and social transition criteria, often
 *   enforced by medical gatekeeping. It attempts to bridge biological and
 *   identity-based understandings but imposes significant costs on those
 *   seeking recognition, particularly excluding non-transitioning
 *   individuals. The constraint is claimed as a Tangled Rope due to its dual
 *   function of coordinating categories while extracting costs through
 *   gatekeeping.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.65).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.75).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership (Hybrid Medical Gatekeeping Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'e304cc96-a77d-4f72-95c2-27f176e492b3').
narrative_ontology:cs_kernel_codification('e304cc96-a77d-4f72-95c2-27f176e492b3', formalized).
narrative_ontology:cs_authority_grounding('e304cc96-a77d-4f72-95c2-27f176e492b3', expertise).
narrative_ontology:cs_interpretation_layer_present('e304cc96-a77d-4f72-95c2-27f176e492b3').
narrative_ontology:cs_reading_relation('e304cc96-a77d-4f72-95c2-27f176e492b3', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('e304cc96-a77d-4f72-95c2-27f176e492b3', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('e304cc96-a77d-4f72-95c2-27f176e492b3', foundational, gender_identity_requires_medical_validation).
narrative_ontology:cs_axiom_status(gender_identity_requires_medical_validation, holdable).
narrative_ontology:cs_axiom_grounding('e304cc96-a77d-4f72-95c2-27f176e492b3', gender_identity_requires_medical_validation, conventional).
narrative_ontology:cs_axiom('e304cc96-a77d-4f72-95c2-27f176e492b3', foundational, biological_sex_is_foundational_but_mutable).
narrative_ontology:cs_axiom_status(biological_sex_is_foundational_but_mutable, holdable).
narrative_ontology:cs_axiom_grounding('e304cc96-a77d-4f72-95c2-27f176e492b3', biological_sex_is_foundational_but_mutable, empirically_contingent).
narrative_ontology:cs_reference_frame('e304cc96-a77d-4f72-95c2-27f176e492b3', medically_mediated_gender_recognition).
narrative_ontology:cs_drift_state('e304cc96-a77d-4f72-95c2-27f176e492b3', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e304cc96-a77d-4f72-95c2-27f176e492b3', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_professionals).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_women_advocates).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transgender_individuals_seeking_recognition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_transgender_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the medical criteria for gender transition and subsequent category recognition. They benefit from their epistemic authority and control over access to medical interventions, which are prerequisites for recognition under this reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_professionals, agenda_setter,
    institutional, generational, constrained, national).

% Must undergo medical evaluation and often physical transition to gain recognition in their affirmed gender. They bear significant financial, emotional, and physical costs, and their access to social and legal categories is conditional on medical gatekeeping.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, transgender_individuals_seeking_recognition, payer,
    powerless, biographical, identity_locked, national).

% Benefit from the perceived maintenance of sex-segregated spaces and categories, which they argue protects their rights and safety. This reading offers a compromise that includes some trans women while maintaining a biological component.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_women_advocates, beneficiary,
    organized, generational, mobile, national).

% Are largely excluded from recognition in their affirmed gender under this reading, as they do not meet the medical transition criteria. They bear the cost of non-recognition without access to the conditional benefits.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_transgender_individuals, excluded,
    powerless, biographical, identity_locked, national).

% Codify and enforce the legal definitions of sex and gender, often incorporating medical criteria for changes to legal documents. They provide the framework within which medical gatekeeping operates.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social and legal categories of sex/gender by providing a defined, medically-mediated pathway for transgender individuals to gain recognition, balancing biological considerations with social identity.
% TRANSFER_FUNCTION: Transfers social and legal recognition (and associated rights/access) from a default biological assignment to an affirmed gender, conditional on medical gatekeeping. It transfers authority over identity claims to medical institutions.
% ABSENT_VOICES: Non-transitioning transgender individuals and those who advocate for self-identification are largely absent from the decision-making process that defines this hybrid model. They would argue for broader, less conditional recognition.
% DISAPPEARANCE_RATIONALE: If this hybrid model vanished, the legal and social landscape for transgender individuals would immediately shift. Either a more purely biological or purely identity-based model would likely emerge, or a chaotic period of undefined categories would ensue, forcing a rapid reorganization of legal and social norms.
% FOUNDING_PROBLEM: The problem of how to integrate transgender identities into existing sex/gender categories while addressing concerns about biological sex differences and maintaining social order.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested as live by medical bodies, legal scholars, and various advocacy groups, though their proposed solutions differ. The ongoing societal debate and legislative efforts corroborate the persistence of this foundational tension.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).

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
 *   Extractiveness is high (0.65) due to the significant costs (financial, psychological, time) imposed by medical gatekeeping and the conditional nature of recognition. Suppression is also high (0.75) as alternatives to medical transition for recognition are actively suppressed by legal and institutional frameworks. The theater ratio is low (0.20) because the medical and legal processes, while costly, are genuinely functional in determining and enforcing category boundaries under this model. The increasing extractiveness and suppression over time reflect the hardening of these gatekeeping mechanisms in response to broader societal debates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of medical institutions and some cisgender women advocates, this model is a necessary coordination mechanism that balances competing interests and ensures 'appropriate' categorization. From the perspective of transgender individuals, particularly those who are non-transitioning or advocate for self-identification, it is an extractive and suppressive system that imposes undue burdens and denies fundamental recognition.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical professionals and legal systems act as agenda-setters and beneficiaries, deriving authority and control from their role in defining and enforcing the criteria. Cisgender women advocates also benefit from the perceived protection of sex-segregated spaces. Transgender individuals seeking recognition are payers, bearing the direct costs of transition and conditional recognition. Non-transitioning transgender individuals are largely excluded, bearing the costs of non-recognition without the conditional benefits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_gatekeeping,
    'To what extent are the medical criteria for transition and recognition genuinely necessary for well-being and social integration, versus serving as a gatekeeping mechanism to limit access to categories?',
    'Longitudinal studies comparing outcomes for individuals under different models of care (e.g., informed consent vs. gatekeeping) and cross-cultural analysis of legal recognition without medical requirements.',
    'If criteria are primarily gatekeeping, the extractiveness and suppression are higher than currently measured, indicating a Snare. If genuinely necessary, the coordination function is stronger, supporting a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_gatekeeping, empirical, 'Distinguishing medical necessity from social gatekeeping in transition requirements.').

omega_variable(
    scope_of_category_protection,
    'Whose interests are primarily protected by this hybrid model: cisgender women''s sex-segregated spaces, or the integrity and clarity of sex/gender categories themselves?',
    'Analysis of legal challenges and policy debates: which arguments are prioritized, and what are the stated and unstated goals of legislative efforts to codify this model.',
    'If primarily protecting cisgender women''s spaces, the beneficiary structure is more concentrated and the constraint leans more extractive. If protecting abstract category integrity, the coordination function is more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_category_protection, conceptual, 'Clarifying the primary beneficiaries and protective function of the hybrid category model.').

omega_variable(
    internalized_suppression_trans_individuals,
    'Is the suppression experienced by transgender individuals primarily structural (legal/medical barriers) or internalized (self-censorship, shame, identity-fusion with the medical pathway)?',
    'Post-exit suppression trajectory: if suppression persists after legal/medical barriers are removed (e.g., in jurisdictions with self-ID), reclassify as partially internalized. Qualitative studies on lived experience.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit less effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_trans_individuals, empirical, 'Structural vs. internalized suppression mechanism for transgender individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t1990, sex_gender_category__hybrid_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(sex__tr_t2000, sex_gender_category__hybrid_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(sex__tr_t2010, sex_gender_category__hybrid_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(sex__tr_t2024, sex_gender_category__hybrid_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sex__be_t1990, sex_gender_category__hybrid_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(sex__be_t2000, sex_gender_category__hybrid_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(sex__be_t2010, sex_gender_category__hybrid_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(sex__be_t2024, sex_gender_category__hybrid_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t1990, sex_gender_category__hybrid_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(sex__su_t2000, sex_gender_category__hybrid_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(sex__su_t2010, sex_gender_category__hybrid_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(sex__su_t2024, sex_gender_category__hybrid_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sex_gender_category' kernel, alongside 'biology_reading' and 'identity_reading'. Each reading defines category membership differently and has distinct structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
