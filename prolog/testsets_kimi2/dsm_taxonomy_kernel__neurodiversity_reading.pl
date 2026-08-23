% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Taxonomy Pathologization of Neurodivergence
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction
 *
 * SUMMARY:
 *   The Diagnostic and Statistical Manual of Mental Disorders (DSM) operates
 *   as the dominant psychiatric taxonomy in the United States with global
 *   influence. From the neurodiversity reading, its categories do not map
 *   neutral biological facts but instead pathologize natural neurological
 *   variationâparticularly autism, ADHD, and related neurotypesâwhen that
 *   variation conflicts with the behavioral norms demanded by schools,
 *   workplaces, and carceral systems. The constraint extracts through
 *   coercive normalization, denial of self-determination, and the channeling
 *   of accommodation costs onto diagnosed individuals rather than
 *   institutional environments. This story instantiates the neurodiversity
 *   reading of the DSM taxonomy kernel; sibling readings (biomedical,
 *   critical psychiatry) model the same text as biological correlation or
 *   pharmaceutical market construction respectively.
 *
 * KEY AGENTS:
 *   - neurodivergent_individuals: Primary target (powerless/constrained) â bear pathologization, masking demands, and loss of self-determination
 *   - american_psychiatric_association: Agenda-setter (institutional/constrained) â authors and maintains taxonomy, collects authority and revenue
 *   - norm_enforcing_institutions: Primary beneficiary (organized/constrained) â schools, employers, and carceral systems use DSM to justify conformity demands and avoid accommodation
 *   - psychiatric_clinicians: Secondary beneficiary/enforcer (organized/constrained) â administer diagnoses and depend on DSM for professional authority and reimbursement
 *   - neurodivergent_self_advocates: Excluded voice (moderate/mobile) â would contest pathologization if included in revision processes
 *   - disability_rights_legal_advocates: Analytical observer (institutional/analytical) â challenge DSM-based exclusion through litigation and policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.82).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy Pathologization of Neurodivergence").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'e43978a5-fb9a-46a4-8c82-5257df9d082e').
narrative_ontology:cs_kernel_codification('e43978a5-fb9a-46a4-8c82-5257df9d082e', formalized).
narrative_ontology:cs_authority_grounding('e43978a5-fb9a-46a4-8c82-5257df9d082e', extraction).
narrative_ontology:cs_interpretation_layer_present('e43978a5-fb9a-46a4-8c82-5257df9d082e').
narrative_ontology:cs_reading_relation('e43978a5-fb9a-46a4-8c82-5257df9d082e', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e43978a5-fb9a-46a4-8c82-5257df9d082e', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('e43978a5-fb9a-46a4-8c82-5257df9d082e', foundational, neurological_variation_is_intrinsic_diversity).
narrative_ontology:cs_axiom_status(neurological_variation_is_intrinsic_diversity, holdable).
narrative_ontology:cs_axiom_grounding('e43978a5-fb9a-46a4-8c82-5257df9d082e', neurological_variation_is_intrinsic_diversity, empirically_contingent).
narrative_ontology:cs_axiom('e43978a5-fb9a-46a4-8c82-5257df9d082e', foundational, pathologization_constitutes_harm).
narrative_ontology:cs_axiom_status(pathologization_constitutes_harm, holdable).
narrative_ontology:cs_axiom_grounding('e43978a5-fb9a-46a4-8c82-5257df9d082e', pathologization_constitutes_harm, deontological).
narrative_ontology:cs_reference_frame('e43978a5-fb9a-46a4-8c82-5257df9d082e', norm_enforcement_apparatus).
narrative_ontology:cs_drift_state('e43978a5-fb9a-46a4-8c82-5257df9d082e', contemporary_neurodiversity_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e43978a5-fb9a-46a4-8c82-5257df9d082e', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, norm_enforcing_institutions).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_clinicians).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, american_psychiatric_association).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of pathologization through coercive normalization demands, behavioral therapies aimed at masking, and denial of self-determination. Must often accept a diagnostic label to access educational or workplace accommodations, even when the label frames their natural neurological variation as disorder. Exit is constrained because rejecting the taxonomy typically means losing services or facing institutional exclusion.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, constrained, global).

% Authors and revises the DSM through expert committees, controlling the boundaries of mental disorder and neurological pathology. Derives institutional authority, publication revenue, and disciplinary centrality from the taxonomy's status as the standard reference. Constrained exit because abandoning the medical-model framework would dissolve its foundational legitimacy.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, american_psychiatric_association, agenda_setter,
    institutional, generational, constrained, global).

% Schools, employers, and carceral systems rely on DSM categories to classify individuals, justify discipline or exclusion, and transfer the burden of accommodation from environmental modification to individual medical treatment. Benefit from a medically legitimated rationale for demanding behavioral conformity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, norm_enforcing_institutions, beneficiary,
    organized, biographical, constrained, national).

% Administer assessments, deliver diagnoses, and provide treatments normed to DSM categories. Professional training, licensure, and reimbursement depend on DSM literacy, creating structural alignment with the taxonomy's authority even when individual clinicians hold neurodiversity-affirming views.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_clinicians, beneficiary,
    organized, biographical, constrained, national).

% Would contest the pathologization of natural neurological variation if included in taxonomy revision processes. Currently excluded from APA committee deliberations and formal nosological authority, though active in public discourse and community organizing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_self_advocates, excluded,
    moderate, biographical, mobile, global).

% Challenge DSM-based exclusion in courts and policy forums. They observe the structural extraction but operate through slow legal remediation and administrative complaints rather than direct taxonomy revision.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, disability_rights_legal_advocates, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, diffuse).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic language that coordinates clinical communication, insurance reimbursement, educational eligibility determination, and research aggregation across disparate medical and institutional settings.
% TRANSFER_FUNCTION: Moves authority to define legitimate neurological difference from neurodivergent individuals and their communities to medical institutions and norm-enforcing systems, transferring the costs of non-conformity onto diagnosed individuals while allowing institutions to avoid environmental accommodation.
% ABSENT_VOICES: Neurodivergent self-advocates and neurodiversity scholars who argue that neurological difference requires accommodation and rights rather than medical intervention are structurally excluded from DSM revision processes and institutional governance.
% DISAPPEARANCE_RATIONALE: If DSM categories vanished, schools and employers would lose their primary medicalized justification for demanding conformity; access to accommodations would need to be reconceptualized outside diagnosis; psychiatric research, insurance coding, and cross-institutional communication would require reorganization around non-pathologizing frameworks.
% FOUNDING_PROBLEM: Unreliable and inconsistent classification of mental distress and behavioral difference across clinicians and institutions, leading to failed communication, arbitrary treatment decisions, and inability to aggregate knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Psychiatric historians and sociologists of medicine attest that classification inconsistency was the original problem. Neurodiversity advocates and critical disability scholars attest that the current arrangement has outlived this problem and now functions primarily to enforce conformity. Independent anthropological and disability-studies research from outside the benefiting parties supports the shifted-function reading.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.82 over the interval because the taxonomy's initial function of reducing diagnostic inconsistency has been progressively colonized by norm-enforcement demands; by the end-state, pathologization operates as a primary mechanism for institutional cost-shifting. Suppression rises from 0.50 to 0.78 as gatekeeping around diagnoses tightened and alternatives (self-identification, non-medical accommodation) were systematically excluded from institutional recognition. Theater ratio rises from 0.15 to 0.48 because an increasing share of DSM maintenance activity performs medical objectivity while actually managing behavioral conformity. Accessibility collapse is high (0.72) because once an individual or institution adopts the DSM framework, non-diagnostic routes to support nearly disappear. Resistance is substantial (0.68) due to the growing neurodiversity movement. All measurement series share one time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (APA, norm-enforcing institutions, clinicians) experience the constraint as necessary coordination infrastructure that reduces arbitrariness and justifies resource allocation. The payer seat (neurodivergent individuals) experiences the same structure as enforced extraction that denies self-determination. The engine computes this divergence from the structural data: identical taxonomy, opposite directionalities, different effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The APA and psychiatric clinicians sit near the beneficiary end because the constraint subsidizes their authority and revenue. Norm-enforcing institutions sit near the beneficiary end because the constraint reduces their accommodation burden. Neurodivergent individuals sit near the full-target end because the constraint extracts self-determination and imposes normalization costs; their constrained exit (diagnosis-required for services) amplifies effective extraction. Self-advocates are excluded from the authority structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the genuine coordination function (shared diagnostic language enabling cross-institutional communication) as pure extraction, while also preventing the medical model from claiming pure coordination status. The rising theater ratio and extraction trajectory over the interval signal that the coordination component is atrophying relative to the extraction component, but has not fully collapsed into pure snare. If the founding problem (classification inconsistency) were genuinely still live, extraction would not trend upward while theater increased; the divergence between founding_problem_status (contested) and the extractive trajectory is exactly the mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_kernel_reading_incommensurability,
    'Does the neurodiversity reading of the DSM taxonomy share a common referent with the biomedical reading, or do the readings track entirely different constraints (diagnostic communication versus enforced conformity)?',
    'Comparative structural analysis of the same DSM edition across readings to determine if metric profiles are epsilon-invariant or require full kernel decomposition.',
    'If readings track different constraints, the kernel should be split into separate stories with no shared epsilon; if shared, the divergence is perspectival only and the kernel remains unified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dsm_kernel_reading_incommensurability, conceptual, 'Whether sibling readings address the same structural constraint or different ones').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of neurodivergent self-determination primarily structural (institutional gatekeeping, diagnosis-required access) or internalized (self-pathologization, identity fusion with diagnostic labels)?',
    'Longitudinal study of neurodivergent individuals who exit diagnostic frameworks: if accommodation access remains blocked and self-concept remains medicalized after structural exit, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because targets carry the suppression with them after exit, increasing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    taxonomy_coordination_genuineness,
    'Does the DSM retain any non-extractable coordination function that would persist if the pathologizing frame were removed?',
    'Analyze whether descriptive phenotyping for accommodation and support purposes could occur without the medical-model framing and gatekeeping architecture.',
    'If a separable coordination function exists, the constraint remains tangled rope; if the coordination is inseparable from the pathologization, the constraint collapses to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taxonomy_coordination_genuineness, conceptual, 'Whether diagnostic coordination is separable from pathologization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dsm__tr_t5, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(dsm__tr_t15, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(dsm__tr_t25, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(dsm__tr_t35, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 35, 0.46).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm__be_t5, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(dsm__be_t15, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(dsm__be_t25, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(dsm__be_t35, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 35, 0.79).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dsm__su_t5, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(dsm__su_t15, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(dsm__su_t25, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(dsm__su_t35, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the DSM taxonomy kernel, instantiating the neurodiversity critique. Sibling constraints model the same kernel from different structural framings. The epsilon values diverge because the referent differs: this reading tracks the norm-enforcement function, while siblings track the biological-correspondence and pharmaceutical-market functions respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
