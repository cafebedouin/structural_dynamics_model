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
 *   This constraint describes the 'hybrid reading' of sex/gender category
 *   membership, where recognition of a transgender individual's affirmed
 *   gender is contingent on meeting specific medical and social transition
 *   criteria, often enforced by medical institutions and legal systems. It
 *   aims to bridge biological and identity-based understandings but results
 *   in significant gatekeeping and exclusion for those who do not or cannot
 *   medically transition. This reading is one of three competing
 *   interpretations of the 'sex_gender_category' kernel.
 *
 * KEY AGENTS:
 *   - medical_institutions: Agenda-setter (institutional/arbitrage) — defines and enforces transition criteria.
 *   - transgender_individuals_seeking_transition: Payer (powerless/identity_locked) — bears costs of gatekeeping for recognition.
 *   - cisgender_women_advocates: Beneficiary (organized/mobile) — benefits from perceived category clarity.
 *   - non_transitioning_transgender_individuals: Excluded (powerless/trapped) — excluded from recognition under this model.
 *   - legal_systems: Agenda-setter (institutional/constrained) — codifies medical gatekeeping into law.
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
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership (Hybrid Medical Gatekeeping Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'e3433bae-c163-4eef-9c32-c3b0888218d3').
narrative_ontology:cs_kernel_codification('e3433bae-c163-4eef-9c32-c3b0888218d3', formalized).
narrative_ontology:cs_authority_grounding('e3433bae-c163-4eef-9c32-c3b0888218d3', expertise).
narrative_ontology:cs_interpretation_layer_present('e3433bae-c163-4eef-9c32-c3b0888218d3').
narrative_ontology:cs_reading_relation('e3433bae-c163-4eef-9c32-c3b0888218d3', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3433bae-c163-4eef-9c32-c3b0888218d3', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('e3433bae-c163-4eef-9c32-c3b0888218d3', foundational, gender_identity_requires_medical_validation).
narrative_ontology:cs_axiom_status(gender_identity_requires_medical_validation, holdable).
narrative_ontology:cs_axiom_grounding('e3433bae-c163-4eef-9c32-c3b0888218d3', gender_identity_requires_medical_validation, conventional).
narrative_ontology:cs_axiom('e3433bae-c163-4eef-9c32-c3b0888218d3', foundational, sex_categories_are_medically_mediated).
narrative_ontology:cs_axiom_status(sex_categories_are_medically_mediated, holdable).
narrative_ontology:cs_axiom_grounding('e3433bae-c163-4eef-9c32-c3b0888218d3', sex_categories_are_medically_mediated, empirically_contingent).
narrative_ontology:cs_reference_frame('e3433bae-c163-4eef-9c32-c3b0888218d3', medically_governed_gender_transition).
narrative_ontology:cs_drift_state('e3433bae-c163-4eef-9c32-c3b0888218d3', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e3433bae-c163-4eef-9c32-c3b0888218d3', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_women_advocates).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transgender_individuals_seeking_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_transgender_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the criteria for gender transition, including diagnostic protocols, hormonal treatments, and surgical interventions. They benefit from the demand for these services and their gatekeeping role in legal gender recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Must navigate complex, often lengthy, and expensive medical pathways to align their legal and social gender with their identity. They bear the costs of medical procedures, therapy, and the emotional burden of gatekeeping, but are identity-locked into seeking these services for recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, transgender_individuals_seeking_transition, payer,
    powerless, biographical, identity_locked, local).

% Advocate for sex-based categories that include trans women only after significant medical transition, viewing this as a necessary boundary for women's spaces and rights. They benefit from the perceived clarity and protection this model offers to their existing categories.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_women_advocates, beneficiary,
    organized, generational, mobile, national).

% Are largely excluded from legal and social recognition within their affirmed gender under this model, as they do not meet the medical transition criteria. They bear the social and legal costs of non-recognition without a clear pathway to inclusion.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_transgender_individuals, excluded,
    powerless, biographical, trapped, local).

% Codify and enforce the medical gatekeeping model into law, determining who can legally change their gender markers on official documents. They rely on medical expertise to define eligibility, creating a bureaucratic layer of control.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, medically-defined pathway for gender transition and legal recognition, aiming to coordinate social and legal categories with medical interventions.
% TRANSFER_FUNCTION: Transfers authority over gender identity recognition from individuals to medical and legal institutions, along with significant financial and emotional costs from transgender individuals to these institutions.
% ABSENT_VOICES: Non-transitioning transgender individuals are largely absent from the policy-making process, as their experiences are not fully accommodated by this model. They would advocate for identity-based recognition without medical prerequisites.
% DISAPPEARANCE_RATIONALE: If this medical gatekeeping model vanished, legal and social recognition of gender identity would likely shift towards either a purely biological or purely identity-based model, leading to significant reorganization of legal frameworks, medical practices, and social norms around gender.
% FOUNDING_PROBLEM: The need to reconcile evolving understandings of gender identity with existing binary sex categories in legal and social systems, while providing a structured process for individuals seeking to transition.
% FOUNDING_PROBLEM_CORROBORATION: Medical professionals attest to the ongoing need for structured care pathways. Advocates for sex-based rights attest to the need for clear, verifiable criteria for category membership. Transgender advocacy groups, while critical of gatekeeping, acknowledge the historical role of medical transition in gaining recognition.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is high due to the significant financial, emotional, and temporal costs imposed on transgender individuals by the gatekeeping process. Suppression (0.75) is also high, as alternatives to medicalized pathways for legal recognition are actively suppressed by institutional and legal structures. The theater ratio (0.20) is relatively low, as the medical and legal processes involved are genuinely functional in their gatekeeping role, though their justification is contested. Accessibility collapse (0.60) reflects that while some pathways exist, they are heavily constrained. Resistance (0.50) is moderate, with ongoing advocacy for less gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   Medical institutions and cisgender women advocates perceive this as a necessary, coordinated approach to category definition, balancing different interests. Transgender individuals, particularly those who are non-transitioning or face barriers to transition, experience it as a highly extractive and suppressive system that denies their self-identified gender unless they conform to external medical criteria.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical institutions are clear beneficiaries and agenda-setters, profiting from and controlling the process. Transgender individuals seeking transition are payers, bearing the costs and subject to the constraint's rules. Cisgender women advocates benefit from the perceived stability of categories. Non-transitioning transgender individuals are excluded, bearing costs without benefit. Legal systems act as agenda-setters, codifying and enforcing the medical model.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it attempts to coordinate social categories and medical pathways (beneficiaries: medical institutions, cisgender women advocates) but does so through asymmetric extraction from transgender individuals (victims: transgender individuals seeking transition, non-transitioning transgender individuals) and requires active enforcement by medical and legal institutions. The mandate is still live, but its implementation is contested, preventing it from being a Piton. It is not a Snare because there is a genuine, albeit contested, coordination function (reconciling identity with existing categories).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_gatekeeping,
    'To what extent are the medical requirements for gender recognition genuinely necessary for well-being and social integration, versus serving as gatekeeping mechanisms for existing social categories?',
    'Longitudinal studies on the outcomes of different models of gender recognition (e.g., informed consent vs. gatekeeping) across various jurisdictions, assessing mental health, social integration, and access to care.',
    'If medical requirements are primarily gatekeeping, the constraint''s extractiveness and suppression are higher than currently estimated, and its coordination function is weaker. If genuinely necessary, the costs are more justifiable as part of a coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_gatekeeping, empirical, 'Distinguishing between medical necessity and social gatekeeping in transition pathways.').

omega_variable(
    exclusion_of_non_transitioning_trans_individuals,
    'Is the exclusion of non-transitioning transgender individuals from full recognition an inherent feature of this hybrid model, or an unintended consequence that could be mitigated?',
    'Policy analysis of legal reforms that attempt to integrate non-transitioning trans individuals within a hybrid framework, and their practical outcomes.',
    'If inherent, the victim set is structurally fixed. If mitigable, the constraint could evolve towards a more inclusive form, reducing its overall suppression and extractiveness for this group.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_of_non_transitioning_trans_individuals, conceptual, 'Assessing the structural necessity of excluding non-transitioning trans individuals.').

omega_variable(
    hybrid_model_legitimacy,
    'Is the hybrid model a stable and legitimate compromise between competing views, or an unstable equilibrium that will inevitably drift towards either a purely biological or purely identity-based model?',
    'Analysis of legal challenges, public discourse, and legislative trends over the next decade in jurisdictions that have adopted hybrid models.',
    'If unstable, the current classification as Tangled Rope may be temporary, with a future reclassification towards Snare (if it drifts to biological exclusion) or Rope (if it drifts to identity-based inclusion with minimal gatekeeping).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_model_legitimacy, preference, 'Stability and long-term trajectory of the hybrid model in the face of competing claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t1980, sex_gender_category__hybrid_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(sex__tr_t1990, sex_gender_category__hybrid_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(sex__tr_t2000, sex_gender_category__hybrid_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(sex__tr_t2010, sex_gender_category__hybrid_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(sex__tr_t2024, sex_gender_category__hybrid_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sex__be_t1980, sex_gender_category__hybrid_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(sex__be_t1990, sex_gender_category__hybrid_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(sex__be_t2000, sex_gender_category__hybrid_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(sex__be_t2010, sex_gender_category__hybrid_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(sex__be_t2024, sex_gender_category__hybrid_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t1980, sex_gender_category__hybrid_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(sex__su_t1990, sex_gender_category__hybrid_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(sex__su_t2000, sex_gender_category__hybrid_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(sex__su_t2010, sex_gender_category__hybrid_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(sex__su_t2024, sex_gender_category__hybrid_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
