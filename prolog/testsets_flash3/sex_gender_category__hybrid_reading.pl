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
 *   human_readable: Sex/Gender Category Membership (Hybrid Medical Gatekeeping Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid' reading of sex/gender category
 *   membership, where recognition of gender identity (e.g., as a woman) is
 *   contingent on both an internal sense of identity and undergoing specific
 *   medical transition processes. It is a medical gatekeeping model that aims
 *   to bridge biological sex and gender identity. This reading conditionally
 *   includes trans women after medical transition, but at a high cost, and
 *   excludes non-transitioning trans individuals. The authority is
 *   concentrated in medical institutions and legal systems that codify these
 *   requirements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.75).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership (Hybrid Medical Gatekeeping Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '3bd3e7cf-0a3e-441d-b8cf-2f097972ec91').
narrative_ontology:cs_kernel_codification('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', formalized).
narrative_ontology:cs_authority_grounding('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', expertise).
narrative_ontology:cs_interpretation_layer_present('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91').
narrative_ontology:cs_reading_relation('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', foundational, medical_transition_validates_gender_identity).
narrative_ontology:cs_axiom_status(medical_transition_validates_gender_identity, holdable).
narrative_ontology:cs_axiom_grounding('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', medical_transition_validates_gender_identity, conventional).
narrative_ontology:cs_axiom('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', foundational, biological_sex_retains_some_relevance).
narrative_ontology:cs_axiom_status(biological_sex_retains_some_relevance, holdable).
narrative_ontology:cs_axiom_grounding('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', biological_sex_retains_some_relevance, empirically_contingent).
narrative_ontology:cs_reference_frame('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', medically_mediated_gender_recognition).
narrative_ontology:cs_drift_state('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3bd3e7cf-0a3e-441d-b8cf-2f097972ec91', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_women_who_accept_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transgender_women_seeking_recognition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_transgender_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define and administer the criteria for medical transition, acting as gatekeepers for legal and social recognition of gender identity. They benefit from the demand for their services and the authority derived from their expertise.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_institutions, agenda_setter,
    institutional, generational, mobile, national).

% These individuals must undergo medical transition processes (hormone therapy, surgeries) to gain recognition under this framework. They bear significant financial, emotional, and physical costs, and their identity is deeply tied to achieving this recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, transgender_women_seeking_recognition, payer,
    powerless, biographical, identity_locked, local).

% These individuals benefit from a framework that maintains a distinction based on biological sex while allowing for the inclusion of trans women who have undergone medical transition, often seeing it as a compromise that protects certain sex-based spaces or categories.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_women_who_accept_transition, beneficiary,
    moderate, biographical, mobile, local).

% These individuals are largely excluded from recognition under this framework, as their gender identity is not validated by medical transition. They bear the cost of non-recognition and social marginalization, with no clear path to inclusion within this specific constraint.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_transgender_individuals, excluded,
    powerless, biographical, identity_locked, local).

% Legal systems codify and enforce the medical gatekeeping model, translating medical criteria into legal recognition for gender markers on documents. They provide the formal structure that gives the medical institutions their power in this context.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, albeit costly, pathway for transgender individuals to gain social and legal recognition, aiming to reconcile biological sex with gender identity through medical intervention, thereby coordinating social categories.
% TRANSFER_FUNCTION: Transfers social and legal recognition (and associated rights/access) to transgender individuals who undergo medical transition, in exchange for significant personal cost and adherence to medical protocols. It also transfers authority and resources to medical institutions.
% ABSENT_VOICES: Non-transitioning transgender individuals and those who advocate for self-identification without medical gatekeeping are largely excluded. They would argue for a more inclusive and less burdensome path to recognition, challenging the necessity of medical intervention for identity validation.
% DISAPPEARANCE_RATIONALE: If this medical gatekeeping model vanished, the legal and social landscape for gender recognition would immediately shift. Transgender individuals would likely seek recognition based on self-identification, medical institutions would lose a significant source of authority and revenue in this domain, and the debate over category membership would intensify, reorganizing around new criteria.
% FOUNDING_PROBLEM: The problem of how to reconcile traditional binary sex categories with emerging understandings of gender identity, particularly for individuals whose gender identity does not align with their sex assigned at birth, while maintaining some form of biological referent.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and some cisgender women attest that the problem is still live, arguing for the need for clear, medically-defined criteria. Transgender advocates and some legal scholars contest the 'problem' as framed, arguing the constraint creates more problems than it solves; independent sociological research highlights the ongoing social friction.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is high due to the significant financial, physical, and emotional costs imposed on individuals seeking recognition through medical transition. Suppression (0.75) is also high, as the framework actively excludes alternative pathways to recognition (e.g., self-identification without medical intervention) and enforces adherence to medical protocols. The theater ratio (0.20) is relatively low, as the medical and legal processes are genuinely functional in administering the gatekeeping, though critics argue the 'necessity' of some interventions is performative. The claimed type is 'tangled_rope' because it offers a coordination function (a pathway to recognition) but with significant asymmetric extraction and active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of medical institutions, this is a necessary coordination mechanism for managing complex social categories. From the perspective of transgender individuals, it is an extractive and suppressive gatekeeping system that imposes undue burdens for basic recognition. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical institutions and legal systems act as agenda-setters and beneficiaries, gaining authority and resources from administering the transition process. Transgender women seeking recognition are primary payers, bearing the costs of transition. Cisgender women who accept this model are beneficiaries, as it maintains a form of sex-based distinction while allowing for conditional inclusion. Non-transitioning transgender individuals are excluded, bearing the costs of non-recognition without a pathway to benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_social_construction,
    'To what extent are the medical transition requirements genuinely necessary for ''sex change'' or ''gender affirmation'' as opposed to being socially constructed gatekeeping mechanisms?',
    'Longitudinal studies on the efficacy and necessity of various medical interventions for well-being and social integration, alongside cross-cultural comparisons of gender recognition models.',
    'If medical necessity is low, the extractiveness and suppression of this constraint are higher than currently measured, indicating a greater degree of arbitrary gatekeeping. If high, the costs are more justifiable as inherent to the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_social_construction, empirical, 'Ambiguity regarding the true necessity of medical interventions for gender recognition.').

omega_variable(
    exclusion_of_non_transitioning_trans_individuals,
    'Is the exclusion of non-transitioning transgender individuals from this framework a necessary boundary for the ''hybrid'' model, or an arbitrary limitation that could be relaxed without undermining the core coordination?',
    'Policy experiments in jurisdictions that adopt more inclusive hybrid models, observing the impact on social cohesion and category clarity.',
    'If the exclusion is arbitrary, the constraint''s suppression is higher than necessary, and its coordination function is less inclusive than it could be, pushing it closer to a Snare for the excluded group. If necessary, the boundary is a core feature of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_of_non_transitioning_trans_individuals, conceptual, 'Whether the exclusion of non-transitioning trans individuals is a structural necessity or an arbitrary choice.').

omega_variable(
    hybrid_vs_identity_framing,
    'Is the ''hybrid'' model a stable compromise between biology and identity, or an unstable intermediate state that will inevitably drift towards either the ''biology_reading'' or ''identity_reading''?',
    'Analysis of legal and social trends over the next 10-20 years in jurisdictions adopting this model, observing shifts in policy and public discourse.',
    'If unstable, the current classification as Tangled Rope might be temporary, with future reclassification towards a more extractive Snare (if it drifts to biology_reading) or a more coordinative Rope (if it drifts to identity_reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_vs_identity_framing, empirical, 'Stability of the hybrid model as a distinct category framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__hybrid_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__hybrid_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__hybrid_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__hybrid_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__hybrid_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__hybrid_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__hybrid_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__hybrid_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__hybrid_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__hybrid_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sex_gender_category' kernel. This 'hybrid_reading' attempts to reconcile biological sex with gender identity through medical gatekeeping, distinct from the 'biology_reading' (immutable biology) and 'identity_reading' (self-identification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
