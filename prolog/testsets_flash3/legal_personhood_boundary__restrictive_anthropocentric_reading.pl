% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary (Restrictive Anthropocentric Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint defines legal personhood as limited to born humans with
 *   demonstrated cognitive capacity. It is a reading of the broader
 *   'legal_personhood_boundary' kernel, emphasizing human exceptionalism and
 *   individual autonomy. This reading structurally excludes fetuses,
 *   ecosystems, and artificial intelligences from legal personhood, thereby
 *   maximizing the autonomy of pregnant persons and minimizing state
 *   intervention in areas like reproduction and environmental law. The
 *   metrics reflect a stable, low-extraction 'mountain' from the perspective
 *   of those who benefit from this definition, as it is presented as a
 *   natural and necessary feature of the legal landscape.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.15).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.2).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, mountain).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary (Restrictive Anthropocentric Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:emerges_naturally(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '9b73ced2-ea1b-4d3d-a46f-5e769c855b70').
narrative_ontology:cs_kernel_codification('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', formalized).
narrative_ontology:cs_authority_grounding('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', lineage).
narrative_ontology:cs_interpretation_layer_present('9b73ced2-ea1b-4d3d-a46f-5e769c855b70').
narrative_ontology:cs_reading_relation('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', foundational, personhood_requires_born_human_cognitive_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_born_human_cognitive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', personhood_requires_born_human_cognitive_capacity, conventional).
narrative_ontology:cs_axiom('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', foundational, human_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(human_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', human_autonomy_is_paramount, deontological).
narrative_ontology:cs_reference_frame('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', post_enlightenment_liberal_legal_tradition).
narrative_ontology:cs_drift_state('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', contemporary_rights_expansion_debates, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9b73ced2-ea1b-4d3d-a46f-5e769c855b70', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans_with_cognitive_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, individual_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the sole recognized legal persons, they benefit from the full suite of rights and protections, and their autonomy (especially reproductive autonomy) is prioritized. This group defines the legal and moral landscape.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans_with_cognitive_capacity, beneficiary,
    institutional, generational, analytical, universal).

% Denied legal personhood, they are not rights-bearers and their interests are mediated through the pregnant person. Their legal status is contingent and dependent.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses, excluded,
    powerless, immediate, trapped, local).

% Denied legal personhood, they cannot assert rights in their own name and are protected only instrumentally, through human-centric environmental regulations. Their intrinsic value is not legally recognized.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems, excluded,
    powerless, civilizational, trapped, global).

% Denied legal personhood, they are treated as property or tools, regardless of their potential for advanced cognition or sentience. Their development is governed by human-defined parameters.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences, excluded,
    powerless, generational, trapped, global).

% Their autonomy over their bodies and reproductive choices is maximized due to the non-personhood status of fetuses. They are the primary decision-makers regarding pregnancy outcomes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    powerful, biographical, mobile, national).

% Interpret and apply the legal framework that defines personhood. They are the custodians of the existing legal tradition and shape its evolution through precedent and doctrine.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_scholars_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, stable boundary for legal rights and responsibilities, ensuring that the legal system operates with a defined set of subjects and objects, minimizing ambiguity in rights allocation.
% TRANSFER_FUNCTION: Transfers the full scope of legal rights, protections, and autonomy exclusively to born humans with cognitive capacity, while denying these to other entities.
% ABSENT_VOICES: Fetuses, ecosystems, and advanced artificial intelligences are structurally excluded from the conversation; if present, they would argue for their own recognition as rights-bearers, challenging the anthropocentric and capacity-based limitations.
% DISAPPEARANCE_RATIONALE: If this restrictive definition of personhood vanished, the entire legal and ethical framework would collapse. Rights, responsibilities, and moral obligations would need to be radically redefined, leading to profound shifts in reproductive law, environmental protection, and the regulation of advanced AI.
% FOUNDING_PROBLEM: To establish a clear, administrable basis for legal rights and duties, ensuring social order and protecting human autonomy within a framework comprehensible to human society.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and practitioners attest that the need for a clear personhood boundary remains live for the functioning of the legal system. Philosophers and ethicists outside the immediate legal beneficiaries corroborate the historical need for such a boundary, even while contesting its current scope.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, ExtMetricName, E),
    domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legal_personhood_boundary__restrictive_anthropocentric_reading),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a 'mountain' because, from the perspective of this reading, the definition of personhood is treated as a fundamental, almost natural, limit of the legal system. Extractiveness is low (0.15) because it primarily defines who is 'in' the system, rather than extracting from those within it. Suppression is low (0.2) as the excluded entities are not seen as 'suppressed' but rather as outside the scope of personhood. Accessibility collapse is high (0.88) because, within this framework, there are almost no recognized alternatives for personhood. Resistance is low (0.1) from within the legal system, though external philosophical and ethical challenges exist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of born humans with cognitive capacity, this constraint is a natural and just ordering of the legal world. From the perspective of those excluded (fetuses, ecosystems, AIs), it is a fundamental denial of rights and a source of profound extraction, though their 'voice' is not recognized within this legal framework. The engine's per-seat classification would highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans with cognitive capacity are the primary beneficiaries (d=0.0), as they are the exclusive holders of rights. Pregnant persons also benefit from enhanced autonomy. Fetuses, ecosystems, and AIs are structurally excluded (d=1.0), bearing the full cost of non-recognition. Legal scholars and judges act as agenda-setters, maintaining and interpreting this boundary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_boundary,
    'Is the limitation of personhood to born, cognitively capable humans a natural, inevitable feature of legal systems, or a socially constructed boundary that benefits identifiable groups?',
    'Comparative legal anthropology and historical analysis of personhood concepts across diverse cultures and eras, alongside philosophical arguments for alternative personhood criteria.',
    'If found to be a social construct, the ''mountain'' classification would be challenged, potentially reclassifying it as a ''snare'' or ''tangled_rope'' for excluded entities, with higher extractiveness and suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_boundary, conceptual, 'Ambiguity regarding the inherent ''naturalness'' of the personhood boundary.').

omega_variable(
    cognitive_capacity_threshold_ambiguity,
    'What is the precise threshold for ''cognitive capacity'' required for personhood, and how is it empirically measured and legally applied?',
    'Interdisciplinary consensus from neuroscience, philosophy of mind, and legal precedent on the minimum criteria for sentience, self-awareness, or rationality, and its consistent application in law.',
    'Ambiguity in this threshold could lead to arbitrary exclusion or inclusion, creating a ''tangled_rope'' for those near the boundary, or a ''snare'' if the threshold is manipulated for extractive purposes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_capacity_threshold_ambiguity, empirical, 'Uncertainty in the definition and measurement of cognitive capacity for personhood.').

omega_variable(
    future_ai_personhood_challenge,
    'How would the emergence of highly advanced, sentient AI challenge or force a re-evaluation of this restrictive anthropocentric personhood boundary?',
    'The actual development of AI demonstrating advanced cognitive and emotional capacities, leading to legal and philosophical debates, and potential legislative or judicial re-evaluation of personhood criteria.',
    'Such a development would likely force a reclassification of this constraint from a ''mountain'' to a ''tangled_rope'' or ''snare'', as the exclusion of AI would become a source of active contestation and potential extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_ai_personhood_challenge, empirical, 'Potential future challenge to anthropocentric personhood from advanced AI.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_protection_frameworks).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_liability_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legal_personhood_boundary' kernel. Its restrictive definition influences and is influenced by laws concerning reproductive autonomy, environmental protection, and AI regulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
