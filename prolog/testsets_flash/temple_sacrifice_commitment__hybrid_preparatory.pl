% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment (Hybrid Preparatory Reading)
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the Halakhic tradition's approach to the laws
 *   of Temple sacrifice in the absence of a functioning Temple. It is a
 *   'hybrid preparatory' reading, asserting that the study of these laws is
 *   neither a full occupation of the commandment (as if the Temple existed)
 *   nor a mere archiving of defunct knowledge, but an active preparation for
 *   a future messianic restoration. This reading extracts cognitive and
 *   financial resources from the community to maintain a commitment in a
 *   suspended state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.45).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.3).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment (Hybrid Preparatory Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '83cd465c-270b-46d7-a3cc-50e4e7d9b8fb').
narrative_ontology:cs_kernel_codification('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', fixed_text).
narrative_ontology:cs_authority_grounding('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', lineage).
narrative_ontology:cs_interpretation_layer_present('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb').
narrative_ontology:cs_reading_relation('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', foundational, study_as_preparation_for_future_performance).
narrative_ontology:cs_axiom_status(study_as_preparation_for_future_performance, holdable).
narrative_ontology:cs_axiom_grounding('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', study_as_preparation_for_future_performance, theological).
narrative_ontology:cs_axiom('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', secondary, material_performance_is_ultimate_form).
narrative_ontology:cs_axiom_status(material_performance_is_ultimate_form, holdable).
narrative_ontology:cs_axiom_grounding('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', material_performance_is_ultimate_form, deontological).
narrative_ontology:cs_reference_frame('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', halakhic_continuity_in_suspension).
narrative_ontology:cs_drift_state('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('83cd465c-270b-46d7-a3cc-50e4e7d9b8fb', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_donors).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the Halakha, including the laws of Temple sacrifice. They define the scope and method of study, asserting its preparatory value for a future messianic era. Their careers and social standing are tied to the continuity of this tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Are the primary sites for the study of Halakha, including sacrifice laws. They receive funding and students based on the perceived importance of this study, maintaining a dedicated infrastructure for its perpetuation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Provide financial support for yeshivas and scholars, believing in the spiritual and preparatory value of studying sacrifice laws. Their resources are directed towards maintaining a commitment to a practice that is currently non-performable.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_donors, payer,
    moderate, biographical, constrained, local).

% Dedicate significant cognitive and temporal resources to studying complex sacrifice laws, often with limited direct application in contemporary practice. They gain spiritual merit and intellectual discipline, but bear the cost of engaging with non-performable law.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha, beneficiary).

% Analyze the historical evolution of Halakhic practice and the social functions of religious institutions. They observe the commitment to sacrifice study as a cultural and institutional phenomenon, without participating in its normative claims.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, secular_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective commitment of a religious community to a future messianic restoration of Temple sacrifice, ensuring the continuity of knowledge and readiness for its re-institution.
% TRANSFER_FUNCTION: Transfers cognitive resources (time, intellectual effort) from students and financial resources from donors to rabbinic scholars and yeshiva institutions, in exchange for maintaining the tradition and preparing for a future religious practice.
% ABSENT_VOICES: Those who believe the commitment has been symbolically transformed or is purely archival would object, arguing that resources could be better allocated to currently performable mitzvot or social welfare. They are absent from the interpretive discourse that defines the 'preparatory' nature of the study.
% DISAPPEARANCE_RATIONALE: If the commitment to study sacrifice laws as a preparatory exercise vanished, a significant portion of traditional Jewish scholarship and institutional life would lose its central organizing principle. Yeshivas would need to redefine their curricula and mission, and the community's orientation towards messianic redemption would fundamentally shift.
% FOUNDING_PROBLEM: The destruction of the Second Temple left a void in Jewish religious practice, as central commandments related to sacrifice became impossible to perform, threatening the continuity of the Halakhic tradition.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and the vast majority of Orthodox Jewish communities attest that the problem of non-performable sacrifice laws remains live, as the Temple has not been rebuilt. While secular historians might frame it as a historical adaptation, within the religious framework, the problem persists until the messianic era.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).
:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while the study provides spiritual and intellectual benefits, it diverts resources to a practice that cannot be materially performed, creating an opportunity cost. Suppression (0.3) is low, as participation is largely voluntary, but social and identity pressures within the religious community encourage adherence. Theater ratio (0.2) is also low, as the commitment is genuinely held, but some aspects of the 'preparatory' framing might be seen as performative maintenance of a suspended tradition. The metrics reflect a long-term, slowly accumulating extraction of resources for a deferred benefit.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars, this is a vital act of faith and continuity, a 'rope' binding the community to its future. From the perspective of some community donors or students, it might feel more like a 'tangled rope' or even a 'snare,' where resources are extracted for a benefit that is uncertain or could be achieved through less resource-intensive means. The engine's classification as 'tangled_rope' reflects this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and yeshiva institutions are beneficiaries and agenda-setters, as they define, transmit, and are supported by this interpretive framework. Community donors and students are payers, contributing resources for a future-oriented, currently non-performable practice. The 'identity_locked' exit option for scholars and students reflects the deep integration of this commitment into their religious and professional identities.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids full mandatrophy by framing the study as 'preparatory' for a future event, rather than a direct performance of the commandment. This allows the mandate to remain 'live' even in suspension. However, the moderate extractiveness and the 'contested' status of the founding problem suggest a potential for mandatrophy if the preparatory function is perceived to diminish or if alternative interpretations gain traction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preparatory_efficacy_ambiguity,
    'Is the study of sacrifice laws genuinely effective as a ''preparatory exercise'' for a future Temple, or is it primarily a mechanism for maintaining institutional continuity and identity?',
    'Theological or philosophical arguments regarding the nature of preparation and the efficacy of study, potentially informed by historical analysis of similar ''suspended'' religious practices.',
    'If primarily institutional maintenance, the ''coordination'' aspect of the constraint diminishes, increasing its effective extractiveness and pushing it closer to a Snare. If genuinely preparatory, the coordination function is strong, justifying the resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparatory_efficacy_ambiguity, conceptual, 'Ambiguity regarding the true function of ''preparatory'' study.').

omega_variable(
    resource_allocation_efficiency,
    'Are the resources (cognitive, financial) allocated to the study of non-performable sacrifice laws optimally used for the community''s overall religious and social goals?',
    'Comparative analysis of resource allocation in communities with different interpretive approaches to sacrifice laws, or internal community discussions on priorities for religious education and social welfare.',
    'If resources are found to be inefficiently allocated, the effective extractiveness for payers (donors, students) would be higher, as their contributions yield suboptimal returns. This could lead to increased resistance or calls for re-prioritization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, preference, 'Efficiency of resource allocation for non-performable religious practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 100, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 100, 0.1).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 500, 0.12).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(temp_tr_t2024, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 100, 0.3).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(temp_be_t2024, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 500, 0.22).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 1000, 0.25).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 1500, 0.27).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(temp_su_t2024, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'temple_sacrifice_commitment' kernel, each representing a distinct structural claim about the nature of the commandment in the absence of the Temple. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
