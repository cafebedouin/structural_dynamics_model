% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Obligation to Study Suspended Sacrificial Laws (Performance-Only Reading)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The performance-only reading of kodashim commandment status holds that
 *   without a Temple and altar, the positive commandments of sacrifice are
 *   suspended — they are husks (kelipot) of their former operative selves.
 *   Yet the halakhic world continues to devote massive intellectual and
 *   material resources to their study. This constraint story models the
 *   institutional practice of mandatory kodashim study as a constraint on the
 *   Orthodox Jewish scholarly community. The reading claims the commandment
 *   is suspended; the constraint is the social-institutional demand that it
 *   be studied anyway.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.78).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.62).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.54).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, snare).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Obligation to Study Suspended Sacrificial Laws (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, 'b615abff-2ae3-4f66-ab48-47c0ca3afb76').
narrative_ontology:cs_kernel_codification('b615abff-2ae3-4f66-ab48-47c0ca3afb76', fixed_text).
narrative_ontology:cs_authority_grounding('b615abff-2ae3-4f66-ab48-47c0ca3afb76', lineage).
narrative_ontology:cs_interpretation_layer_present('b615abff-2ae3-4f66-ab48-47c0ca3afb76').
narrative_ontology:cs_reading_relation('b615abff-2ae3-4f66-ab48-47c0ca3afb76', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('b615abff-2ae3-4f66-ab48-47c0ca3afb76', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('b615abff-2ae3-4f66-ab48-47c0ca3afb76', foundational, commandment_suspended_without_temple).
narrative_ontology:cs_axiom_status(commandment_suspended_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('b615abff-2ae3-4f66-ab48-47c0ca3afb76', commandment_suspended_without_temple, conventional).
narrative_ontology:cs_axiom('b615abff-2ae3-4f66-ab48-47c0ca3afb76', foundational, study_of_suspended_commandment_is_not_fulfillment).
narrative_ontology:cs_axiom_status(study_of_suspended_commandment_is_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('b615abff-2ae3-4f66-ab48-47c0ca3afb76', study_of_suspended_commandment_is_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('b615abff-2ae3-4f66-ab48-47c0ca3afb76', suspended_commandment_status).
narrative_ontology:cs_drift_state('b615abff-2ae3-4f66-ab48-47c0ca3afb76', post_talmudic_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b615abff-2ae3-4f66-ab48-47c0ca3afb76', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, kodashim_specialist_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, halakhic_authorities_maintaining_curriculum).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, general_torah_scholars).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, community_resource_pool).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, torah_study_as_intrinsic_merit).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, preservation_of_oral_tradition).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, halakhic_continuity_through_study).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set curriculum requiring extensive kodashim study; control ordination and funding; benefit from prestige and donor support tied to maintaining full traditional curriculum including suspended commandments.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Build careers on expertise in suspended laws; publish, teach, and hold positions that depend on the continued centrality of kodashim in the curriculum; their professional identity is fused with this specialization.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, kodashim_specialist_scholars, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, kodashim_specialist_scholars, agenda_setter).

% Issue rulings that maintain kodashim study as obligatory; derive authority from being the guardians of the full tradition; face pressure not to appear to diminish any part of Torah.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_authorities_maintaining_curriculum, agenda_setter,
    institutional, generational, constrained, global).

% Invest years of prime intellectual energy mastering laws they are taught are currently inoperative; cannot easily redirect because their communal belonging, marriage prospects, and identity are tied to completing the traditional curriculum.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_students, payer,
    moderate, biographical, identity_locked, global).

% Compete for limited communal resources (funding, attention, positions) that are diverted to maintaining kodashim infrastructure; their work in practically applicable areas is subordinated.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, general_torah_scholars, payer,
    moderate, biographical, constrained, global).

% Communal funds support yeshivas and kollels; a significant portion goes to sustaining study of suspended laws rather than social services, education in practical halakha, or poverty relief.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, community_resource_pool, payer,
    powerless, generational, trapped, global).

% Argue that resources should be redirected to living halakha; are structurally excluded from halakhic decision-making bodies and yeshiva governance; their objections are dismissed as outside the tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, reform_and_academic_critics, excluded,
    moderate, biographical, mobile, global).

% Observes the constraint from outside the commitment system; sees the structural divergence between the performance-only reading's claim (commandment suspended) and the institutional practice (intensive study continues).
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the textual and conceptual integrity of the sacrificial corpus against total oblivion; maintains a scholarly class capable of theoretical reconstruction if the Temple were rebuilt.
% TRANSFER_FUNCTION: Moves scholarly labor, communal funding, and curricular space from practically applicable halakha (e.g., civil law, medical ethics, agricultural law) to the study of inoperative sacrificial procedures.
% ABSENT_VOICES: Working-class community members who fund yeshivas but have no say in curriculum; women scholars historically excluded from kodashim study; potential scholars who would enter Torah study if the curriculum were more practically relevant.
% DISAPPEARANCE_RATIONALE: If the obligation to study kodashim as a core curriculum requirement vanished, yeshiva curricula would shift dramatically toward practical halakha and aggada; specialist scholars would need to retrain or lose positions; communal funds would be freed for other priorities; the halakhic authority structure would lose a key pillar of its claim to total传承.
% FOUNDING_PROBLEM: After the Temple's destruction, how to prevent the sacrificial laws from being forgotten entirely, and how to maintain a halakhic system that claims continuity with the biblical sacrificial cult.
% FOUNDING_PROBLEM_CORROBORATION: Traditional historians (e.g., R. Sherira Gaon, Rambam's Introduction to Mishnah) attest the founding problem was preservation against forgetting. Contemporary scholars outside the beneficiary set (e.g., Haym Soloveitchik, Vered Noam) argue the founding problem was largely solved by the Mishnah's redaction; the current intensity of study exceeds preservation needs and serves institutional interests.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the curriculum mandate diverts prime scholarly years and communal funds to laws the reading itself declares inoperative. Theater ratio is high (0.71) because much study is performative — maintaining the appearance of comprehensive tradition rather than engaging with living practice. Suppression is moderate (0.62): enforcement is social (shidduch prospects, communal standing, funding) not physical. Accessibility collapse (0.54) reflects that alternatives exist (e.g., Daf Yomi, practical halakha focus) but are structurally marginalized. Resistance (0.38) is low because dissenters exit the system rather than reform it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a Rope: coordinating tradition-preservation across generations. From the student/payer seat, it is a Snare: extracting labor for a suspended commandment. The engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda setters (yeshiva heads, halakhic authorities) are structural beneficiaries: they control the curriculum and derive authority from its comprehensiveness. Specialist scholars are beneficiaries with constrained exit — their identity is fused to the specialty. Students are payers with identity-locked exit: leaving means abandoning their communal identity. Community resource pool is a powerless payer. Excluded critics have mobile exit but no voice inside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preservation against forgetting) was substantially solved by the Mishnah and Talmud's redaction. The constraint persists because the institutions that grew around the solution now depend on the problem's continuation for their legitimacy and resource flow. The mandate has atrophied into a self-justifying structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the performance-only reading''s claim that the commandment is suspended interact with the institutional practice that mandates its intensive study?',
    'Compare the reading''s explicit halakhic claim (suspended) with the sociological reality (mandatory study); measure resource allocation to kodashim vs. practical halakha in yeshiva budgets and curricula.',
    'If the gap is structural and persistent, the constraint is a Snare (extraction without coordination). If the gap is narrowing (curricula shifting), the constraint may be transitioning toward Scaffold or Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, empirical, 'Commitment-system framing: this reading instantiates a constraint that the reading itself declares suspended.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative curricula structural (funding rules, ordination requirements) or internalized (students believe kodashim study is intrinsically superior)?',
    'Survey yeshiva students and faculty on their stated reasons for prioritizing kodashim; track curriculum changes when external funding conditions shift.',
    'If internalized, the constraint''s effective suppression is higher than institutional rules suggest — the constraint travels with the agent even after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in a religious commitment system.').

omega_variable(
    preservation_vs_extraction_boundary,
    'At what point does preservation of a suspended legal corpus become extractive rather than coordinative?',
    'Measure the marginal scholarly output of kodashim study against the marginal cost in diverted resources; assess whether new insights are generated or only repetitive commentary.',
    'If marginal output is near zero, the coordination function is exhausted and the constraint is pure extraction (Snare). If significant theoretical work continues, a Tangled Rope classification may be warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preservation_vs_extraction_boundary, conceptual, 'The coordination-extraction boundary for suspended legal corpora.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_perf_tr_t70, kodashim_commandment_status__performance_only, theater_ratio, 70, 0.25).
narrative_ontology:measurement(kodashim_perf_tr_t500, kodashim_commandment_status__performance_only, theater_ratio, 500, 0.35).
narrative_ontology:measurement(kodashim_perf_tr_t1000, kodashim_commandment_status__performance_only, theater_ratio, 1000, 0.48).
narrative_ontology:measurement(kodashim_perf_tr_t1500, kodashim_commandment_status__performance_only, theater_ratio, 1500, 0.58).
narrative_ontology:measurement(kodashim_perf_tr_t1800, kodashim_commandment_status__performance_only, theater_ratio, 1800, 0.66).
narrative_ontology:measurement(kodashim_perf_tr_t2024, kodashim_commandment_status__performance_only, theater_ratio, 2024, 0.71).

% Extraction over time
narrative_ontology:measurement(kodashim_perf_be_t70, kodashim_commandment_status__performance_only, base_extractiveness, 70, 0.35).
narrative_ontology:measurement(kodashim_perf_be_t500, kodashim_commandment_status__performance_only, base_extractiveness, 500, 0.45).
narrative_ontology:measurement(kodashim_perf_be_t1000, kodashim_commandment_status__performance_only, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(kodashim_perf_be_t1500, kodashim_commandment_status__performance_only, base_extractiveness, 1500, 0.65).
narrative_ontology:measurement(kodashim_perf_be_t1800, kodashim_commandment_status__performance_only, base_extractiveness, 1800, 0.72).
narrative_ontology:measurement(kodashim_perf_be_t2024, kodashim_commandment_status__performance_only, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_perf_su_t70, kodashim_commandment_status__performance_only, suppression_requirement, 70, 0.3).
narrative_ontology:measurement(kodashim_perf_su_t500, kodashim_commandment_status__performance_only, suppression_requirement, 500, 0.4).
narrative_ontology:measurement(kodashim_perf_su_t1000, kodashim_commandment_status__performance_only, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(kodashim_perf_su_t1500, kodashim_commandment_status__performance_only, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(kodashim_perf_su_t1800, kodashim_commandment_status__performance_only, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(kodashim_perf_su_t2024, kodashim_commandment_status__performance_only, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__performance_only, 0.08).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, yeshiva_curriculum_mandate).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, halakhic_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kodashim_commandment_status kernel. The performance-only reading treats the commandment as suspended and the study obligation as institutional accretion. The messianic_deferral reading treats study as readiness-maintenance. The study_as_performance reading treats study as fulfillment. They form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, moderate, 0.85).
constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
