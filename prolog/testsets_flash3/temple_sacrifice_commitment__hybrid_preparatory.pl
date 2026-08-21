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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment: Hybrid Preparatory Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid preparatory' reading of the Temple
 *   sacrifice commitment within Halakhic tradition. It posits that the study
 *   of sacrifice laws, while not a direct performance, is a vital preparatory
 *   exercise for a future messianic restoration, maintaining the commitment
 *   in a suspended state. This reading navigates between full occupation and
 *   mere archiving, extracting cognitive and financial resources for a
 *   deferred, yet actively anticipated, practice. This is one reading of the
 *   'temple_sacrifice_commitment' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.45).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.2).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, scaffold).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment: Hybrid Preparatory Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, 'ebb7111b-55b4-4c55-93ba-c932d6066df7').
narrative_ontology:cs_kernel_codification('ebb7111b-55b4-4c55-93ba-c932d6066df7', fixed_text).
narrative_ontology:cs_authority_grounding('ebb7111b-55b4-4c55-93ba-c932d6066df7', lineage).
narrative_ontology:cs_interpretation_layer_present('ebb7111b-55b4-4c55-93ba-c932d6066df7').
narrative_ontology:cs_reading_relation('ebb7111b-55b4-4c55-93ba-c932d6066df7', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('ebb7111b-55b4-4c55-93ba-c932d6066df7', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('ebb7111b-55b4-4c55-93ba-c932d6066df7', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('ebb7111b-55b4-4c55-93ba-c932d6066df7', foundational, study_as_preparation_for_future_performance).
narrative_ontology:cs_axiom_status(study_as_preparation_for_future_performance, holdable).
narrative_ontology:cs_axiom_grounding('ebb7111b-55b4-4c55-93ba-c932d6066df7', study_as_preparation_for_future_performance, theological).
narrative_ontology:cs_axiom('ebb7111b-55b4-4c55-93ba-c932d6066df7', foundational, commitment_maintained_in_suspended_state).
narrative_ontology:cs_axiom_status(commitment_maintained_in_suspended_state, holdable).
narrative_ontology:cs_axiom_grounding('ebb7111b-55b4-4c55-93ba-c932d6066df7', commitment_maintained_in_suspended_state, theological).
narrative_ontology:cs_reference_frame('ebb7111b-55b4-4c55-93ba-c932d6066df7', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('ebb7111b-55b4-4c55-93ba-c932d6066df7', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ebb7111b-55b4-4c55-93ba-c932d6066df7', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, pious_community_members).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_funders).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students_of_non_performable_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the Halakhic tradition, including laws of Temple sacrifice. They maintain the 'suspended state' doctrine and guide the community in preparatory study, benefiting from the intellectual and spiritual engagement it provides.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Engage in the study of sacrifice laws as a spiritual discipline, believing it contributes to the messianic restoration. They derive spiritual benefit and a sense of active participation in the tradition's future, but also contribute resources to support this study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, pious_community_members, beneficiary,
    moderate, biographical, constrained, local).

% Provide financial support for institutions and scholars dedicated to the study of sacrifice laws. While they may share the messianic hope, their resources are directed towards a practice that is currently non-performable, representing a diversion of funds from other communal needs.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_funders, payer,
    powerful, biographical, mobile, local).

% Dedicate significant cognitive and temporal resources to studying intricate laws that cannot be practically applied in the present. This commitment is driven by religious identity and communal expectation, but represents a substantial investment in a deferred practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students_of_non_performable_law, payer,
    powerless, biographical, identity_locked, local).

% Analyze the historical evolution of religious practices and their interpretations. They observe the maintenance of the commitment to sacrifice laws as a cultural and intellectual phenomenon, without participating in its normative claims.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, secular_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective spiritual and intellectual efforts of a community towards a shared messianic future, ensuring the preservation and readiness of a complex religious practice for its eventual restoration.
% TRANSFER_FUNCTION: Transfers cognitive resources (study, intellectual engagement) and financial resources (funding for scholars and institutions) from the broader community to the maintenance and preparation for a future, currently non-performable, religious practice.
% ABSENT_VOICES: Those who prioritize immediate, performable religious obligations or alternative communal investments might object, arguing that resources are misallocated to a deferred practice. They are often marginalized by the dominant narrative of messianic anticipation.
% DISAPPEARANCE_RATIONALE: If the commitment to preparatory study of Temple sacrifice vanished, a significant portion of rabbinic scholarship and communal spiritual life would lose its central organizing principle. Educational curricula, communal funding priorities, and individual spiritual practices would undergo substantial reorganization, reflecting a profound shift in religious identity and eschatological focus.
% FOUNDING_PROBLEM: The destruction of the Second Temple left a void in Jewish religious practice, as central sacrificial rituals could no longer be performed, threatening the continuity of divine command and communal identity.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by rabbinic authorities and the pious community, who view the Temple's absence as an ongoing spiritual challenge. Secular historians corroborate the historical reality of the Temple's destruction and the subsequent reorientation of Jewish practice, confirming the problem's historical genesis, though not its 'live' spiritual status.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) is moderate because resources are diverted to a non-performable practice, creating a cost for funders and students, but it is not pure extraction due to the genuine spiritual and communal benefits derived by participants. Suppression (0.2) is low, as participation is largely voluntary, driven by identity and belief rather than coercion. Theater ratio (0.1) is low, as the study is considered genuinely functional for its stated preparatory goal, not merely performative. The claimed type is 'scaffold' because it is a temporary support structure for a future state, though without a formal sunset clause, its temporality is eschatological.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars and pious community members, this constraint is a vital 'scaffold' for future restoration, providing spiritual benefit and continuity. From the perspective of community funders and students, it functions as a 'tangled rope' or 'snare', extracting resources for an uncertain future benefit, with the coordination function (preserving tradition) intertwined with the cost of deferral.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and pious community members are beneficiaries, as they derive spiritual and intellectual gains from maintaining the tradition and preparing for restoration. Community funders and students of non-performable law are payers, as they invest significant resources (financial and cognitive) into a practice that cannot be immediately realized. The 'identity_locked' exit option for scholars and students reflects the deep integration of this commitment into their religious identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as 'scaffold' prevents mislabeling it as a 'snare' by acknowledging the genuine coordination function of preserving tradition and preparing for a future state, even if the benefits are deferred. However, the moderate extractiveness and the lack of a formal sunset clause (its temporality is eschatological) highlight the potential for mandatrophy, where the 'preparatory' function could drift into indefinite, self-sustaining extraction without a clear path to resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_certainty,
    'What is the certainty and timeframe of the messianic restoration, and how does this impact the ''preparatory'' justification?',
    'Theological consensus or empirical events (e.g., Temple rebuilding).',
    'If the timeline is highly uncertain or indefinitely deferred, the ''preparatory'' function weakens, increasing the effective extractiveness and potentially reclassifying the constraint towards a ''snare'' or ''piton'' due to indefinite resource allocation for a non-realizable goal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timeline_certainty, conceptual, 'Uncertainty of the messianic timeline affects the justification for preparatory study.').

omega_variable(
    resource_allocation_efficiency,
    'Are the resources (cognitive, financial) allocated to preparatory study the most efficient means to achieve the stated goal of messianic restoration, or could they be better utilized elsewhere?',
    'Comparative analysis of resource allocation in other religious or communal endeavors, or internal theological debate on priorities.',
    'If alternative allocations are demonstrably more efficient for the ultimate goal, the current arrangement''s extractiveness increases, as it represents a suboptimal use of communal resources, potentially shifting classification towards ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation for preparatory study.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''hybrid preparatory'' reading, or is it better understood as a ''performance_only'' or ''study_as_exercise'' reading with different implications for extractiveness?',
    'Analysis of authoritative rabbinic texts and communal practice, focusing on explicit statements regarding the nature of study and its relationship to future performance.',
    'If reclassified as ''performance_only'', extractiveness would be higher (study is mere archiving). If reclassified as ''study_as_exercise'', extractiveness would be lower (study is full performance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the precise nature of the commitment to Temple sacrifice laws.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.09).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.1).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 60, 0.09).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 80, 0.11).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 80, 0.46).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.19).
narrative_ontology:measurement(temp_su_t40, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(temp_su_t60, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 60, 0.19).
narrative_ontology:measurement(temp_su_t80, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 80, 0.21).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
