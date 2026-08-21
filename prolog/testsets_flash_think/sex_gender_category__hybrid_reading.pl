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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Sex/Gender Category: Hybrid Medical Gatekeeping Model
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid' model for sex/gender category
 *   membership, where an individual's legal gender is determined by a
 *   combination of their biological sex at birth and a process of social and
 *   often medical transition, typically overseen by medical institutions. It
 *   functions as a gatekeeping mechanism, allowing conditional inclusion for
 *   trans individuals who meet specific criteria, while excluding others. The
 *   high extractiveness reflects the significant costs borne by trans
 *   individuals, and high suppression reflects the institutional power to
 *   deny recognition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.78).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.85).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category: Hybrid Medical Gatekeeping Model").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '8ff51ca6-d479-4a07-a1e9-3c573ce8c871').
narrative_ontology:cs_kernel_codification('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', formalized).
narrative_ontology:cs_authority_grounding('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', expertise).
narrative_ontology:cs_interpretation_layer_present('8ff51ca6-d479-4a07-a1e9-3c573ce8c871').
narrative_ontology:cs_reading_relation('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', sex_gender_category__biology_reading, influences).
narrative_ontology:cs_reading_relation('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_axiom('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', foundational, medical_oversight_for_gender_recognition).
narrative_ontology:cs_axiom_status(medical_oversight_for_gender_recognition, holdable).
narrative_ontology:cs_axiom_grounding('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', medical_oversight_for_gender_recognition, empirically_contingent).
narrative_ontology:cs_axiom('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', foundational, biological_sex_as_foundational_but_mutable).
narrative_ontology:cs_axiom_status(biological_sex_as_foundational_but_mutable, holdable).
narrative_ontology:cs_axiom_grounding('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', biological_sex_as_foundational_but_mutable, conventional).
narrative_ontology:cs_reference_frame('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', medically_supervised_transition_pathway).
narrative_ontology:cs_drift_state('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', contemporary_self_id_advocacy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8ff51ca6-d479-4a07-a1e9-3c573ce8c871', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_institutions_professionals).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, legal_systems_upholding_model).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_individuals_upholding_boundaries).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_individuals_seeking_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the medical criteria for gender transition and legal recognition. Benefits from fees for assessments, treatments, and gatekeeping services. Justifies its role as ensuring medical safety and appropriate care.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_institutions_professionals, agenda_setter,
    institutional, generational, arbitrage, global).

% Codifies and enforces the legal requirements for gender recognition, often deferring to medical assessments. Benefits from maintaining a structured, albeit complex, system for legal classification and identity documents.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legal_systems_upholding_model, agenda_setter,
    institutional, generational, constrained, national).

% Must navigate complex, costly, and often lengthy medical pathways to access gender-affirming care and legal recognition. Bears significant financial, emotional, and temporal costs. Exit means abandoning legal recognition or medical care.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_individuals_seeking_transition, payer,
    powerless, biographical, identity_locked, local).

% Are largely excluded from legal gender recognition under this model if they do not undergo medical transition, regardless of their gender identity. Bears the cost of non-recognition and social marginalization without a clear pathway to inclusion.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer).

% Benefit from the perceived stability and clarity of gender categories maintained by the medical gatekeeping model, which aligns with traditional understandings of sex and gender. They are not directly involved in the gatekeeping process but support its existence.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_individuals_upholding_boundaries, beneficiary,
    organized, generational, mobile, national).

% Actively challenge the medical gatekeeping model, advocating for self-identification and reduced barriers to care. Bear the costs of advocacy, legal challenges, and supporting affected individuals.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_rights_advocates, observer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, trans_rights_advocates, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, medical_institutions_professionals).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, medically-supervised pathway for individuals seeking gender transition, aiming to ensure medical safety, consistency in care, and a recognized process for legal gender change.
% TRANSFER_FUNCTION: Transfers significant financial resources, personal autonomy, and time from trans individuals to medical and legal institutions in exchange for access to gender-affirming care and legal recognition of their gender.
% ABSENT_VOICES: Trans individuals who cannot or choose not to medically transition, intersex individuals whose experiences are often marginalized by binary medical models, and advocates for self-identification models are largely excluded from shaping this constraint.
% DISAPPEARANCE_RATIONALE: If this medical gatekeeping model vanished overnight, the legal and social landscape for gender recognition would be profoundly altered. Many jurisdictions would likely shift towards self-identification, medical institutions would lose a significant revenue stream and regulatory power, and trans individuals would face a dramatically different, potentially less restrictive, path to recognition.
% FOUNDING_PROBLEM: To establish a medically and legally sanctioned framework for individuals seeking to change their legal sex or gender, in response to early medical interventions and the need for legal clarity in identity documents.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and some legal bodies argue the problem of ensuring appropriate medical care and legal clarity is still live. Trans rights advocates and human rights organizations, from outside the benefiting parties, contend that the original problem has largely shifted to one of gatekeeping and control, rather than genuine medical necessity or coordination.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it offers a pathway (coordination function) for gender recognition and medical care, but this pathway is heavily gated and involves substantial extraction from trans individuals. The high extractiveness (0.78) is driven by the financial, emotional, and temporal costs of navigating medical and legal systems. Suppression (0.85) is high due to the institutional power of medical and legal bodies to grant or deny recognition, effectively limiting alternatives. The theater ratio (0.40) indicates that while genuine medical assessment occurs, a significant portion of the process serves to maintain institutional control over category boundaries rather than purely medical necessity. Accessibility collapse is high (0.70) as for many, this is the only legally recognized path.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of medical institutions, this model provides necessary structure and safety. From the perspective of trans individuals, it is an extractive and suppressive gatekeeping mechanism. The engine's computation of per-seat classification will highlight this divergence, showing a beneficial 'rope' for agenda-setters and a 'snare' or 'tangled_rope' for payers/victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical institutions and legal systems are clear beneficiaries and agenda-setters, controlling the process and deriving resources and authority. Cisgender individuals who uphold traditional boundaries also benefit from the perceived stability of categories. Trans individuals seeking transition are primary payers and victims, bearing the costs and navigating the gates. Non-transitioning trans individuals are largely excluded and victimized by the lack of a pathway for their recognition. Trans rights advocates act as observers and bear costs through their efforts to challenge the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_social_control,
    'To what extent is the medical gatekeeping model driven by genuine medical necessity and safety concerns, versus a desire to maintain traditional social categories and institutional control over identity?',
    'Comparative analysis of health outcomes and social integration in jurisdictions with different models (e.g., self-identification vs. medical gatekeeping), alongside independent ethical review of medical protocols.',
    'If primarily social control, the extractiveness and suppression metrics are more accurately attributed to rent-seeking and boundary enforcement; if primarily medical necessity, a portion of these metrics reflects legitimate coordination costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_social_control, conceptual, 'Distinguishing medical necessity from social control in gatekeeping.').

omega_variable(
    cost_of_gatekeeping_vs_care,
    'What is the actual cost of the gatekeeping process (assessments, bureaucratic hurdles, delays) compared to the direct costs of gender-affirming medical care itself?',
    'Detailed economic analysis of healthcare systems, disaggregating costs associated with diagnostic processes, psychological evaluations, and administrative overhead from the costs of hormones, surgeries, and ongoing medical support.',
    'A high ratio of gatekeeping costs to direct care costs would strongly support the high extractiveness metric and indicate a system designed for control rather than efficient care delivery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_of_gatekeeping_vs_care, empirical, 'Quantifying the economic burden of gatekeeping vs. care.').

omega_variable(
    exclusion_of_non_transitioning_trans_individuals,
    'Is the exclusion of non-transitioning trans individuals from legal recognition an inherent feature of the ''hybrid'' model, or a remediable flaw?',
    'Legal reforms in jurisdictions adopting hybrid models that explicitly create pathways for non-transitioning trans individuals, and subsequent analysis of their impact on the model''s coherence and social acceptance.',
    'If inherent, the model''s victim set is structurally fixed; if remediable, the model could evolve to be less extractive and suppressive for this group without fundamentally altering its ''hybrid'' nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_of_non_transitioning_trans_individuals, preference, 'Structural vs. remediable exclusion of non-transitioning trans individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t1980, sex_gender_category__hybrid_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(sex__tr_t1990, sex_gender_category__hybrid_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(sex__tr_t2000, sex_gender_category__hybrid_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(sex__tr_t2010, sex_gender_category__hybrid_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(sex__tr_t2020, sex_gender_category__hybrid_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(sex__tr_t2024, sex_gender_category__hybrid_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(sex__be_t1980, sex_gender_category__hybrid_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(sex__be_t1990, sex_gender_category__hybrid_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(sex__be_t2000, sex_gender_category__hybrid_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(sex__be_t2010, sex_gender_category__hybrid_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(sex__be_t2020, sex_gender_category__hybrid_reading, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(sex__be_t2024, sex_gender_category__hybrid_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t1980, sex_gender_category__hybrid_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(sex__su_t1990, sex_gender_category__hybrid_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(sex__su_t2000, sex_gender_category__hybrid_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(sex__su_t2010, sex_gender_category__hybrid_reading, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement(sex__su_t2020, sex_gender_category__hybrid_reading, suppression_requirement, 2020, 0.84).
narrative_ontology:measurement(sex__su_t2024, sex_gender_category__hybrid_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sex_gender_category' kernel, focusing on the medical gatekeeping model. It is structurally distinct from the 'biology_reading' (sex_gender_category__biology_reading) and the 'identity_reading' (sex_gender_category__identity_reading), which represent alternative frameworks for defining gender category membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
