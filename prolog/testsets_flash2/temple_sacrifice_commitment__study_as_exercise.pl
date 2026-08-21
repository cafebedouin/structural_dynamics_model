% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Temple Sacrifice Commitment: Study as Exercise of Divine Command
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'study as exercise' reading of the Temple
 *   sacrifice commitment, where intellectual engagement with sacrifice law is
 *   considered a direct fulfillment of the divine command, particularly in
 *   the absence of material conditions for performance. This reading posits
 *   zero extractiveness, as study is intrinsically valuable and a direct
 *   expression of covenant fidelity, benefiting the studying community. It is
 *   claimed as a Mountain due to its perceived natural emergence from the
 *   theological necessity of maintaining divine command in changed
 *   circumstances, and its persistence as a foundational aspect of religious
 *   life.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Temple Sacrifice Commitment: Study as Exercise of Divine Command").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '26257676-592a-491b-8cd4-cfd0dd5a7407').
narrative_ontology:cs_kernel_codification('26257676-592a-491b-8cd4-cfd0dd5a7407', fixed_text).
narrative_ontology:cs_authority_grounding('26257676-592a-491b-8cd4-cfd0dd5a7407', lineage).
narrative_ontology:cs_interpretation_layer_present('26257676-592a-491b-8cd4-cfd0dd5a7407').
narrative_ontology:cs_reading_relation('26257676-592a-491b-8cd4-cfd0dd5a7407', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('26257676-592a-491b-8cd4-cfd0dd5a7407', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('26257676-592a-491b-8cd4-cfd0dd5a7407', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('26257676-592a-491b-8cd4-cfd0dd5a7407', foundational, intellectual_engagement_is_performance).
narrative_ontology:cs_axiom_status(intellectual_engagement_is_performance, holdable).
narrative_ontology:cs_axiom_grounding('26257676-592a-491b-8cd4-cfd0dd5a7407', intellectual_engagement_is_performance, theological).
narrative_ontology:cs_reference_frame('26257676-592a-491b-8cd4-cfd0dd5a7407', post_temple_rabbinic_tradition).
narrative_ontology:cs_drift_state('26257676-592a-491b-8cd4-cfd0dd5a7407', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('26257676-592a-491b-8cd4-cfd0dd5a7407', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community of scholars and adherents who engage in the intellectual study of sacrifice law. They derive spiritual and communal benefit from this practice, seeing it as a direct fulfillment of divine will and a means of maintaining covenant fidelity. Their identity is deeply intertwined with this commitment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, identity_locked, global).

% The abstract concept of faithfulness to the divine covenant. This reading posits that intellectual engagement with the laws of sacrifice directly contributes to the maintenance of this fidelity, even in the absence of material conditions for performance. It is a vindicated proposition, not an active agent.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity).

% The ultimate source of the commandment to perform sacrifices. This reading interprets the command broadly to include intellectual engagement as a form of performance, thereby maintaining its live status even when material performance is impossible.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, divine_command, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, divine_command).

% The hope for the restoration of the Temple and the resumption of material sacrifices. While not directly opposed, this reading's emphasis on study as full performance can be seen as de-emphasizing the urgency of material restoration, which some adherents might find problematic.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, messianic_aspirations, excluded,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, messianic_aspirations).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing spiritual and intellectual life of a dispersed community around a central, unperformable divine command, ensuring continuity of tradition and shared purpose through study.
% TRANSFER_FUNCTION: Transfers spiritual merit and communal identity to the studying community by transforming intellectual engagement into a form of divine service, in lieu of material sacrifice.
% ABSENT_VOICES: Those who hold a 'performance_only' reading would argue that study, while valuable, cannot fully occupy the divine command in the absence of material conditions, and that this reading risks spiritual complacency regarding the need for restoration. They are excluded from this reading's core premise.
% DISAPPEARANCE_RATIONALE: If the understanding of 'study as exercise' vanished, the studying community would lose a primary mode of covenant fulfillment and spiritual engagement. The commitment to sacrifice law would become purely archival or preparatory, fundamentally altering the religious practice and identity of the community.
% FOUNDING_PROBLEM: The historical destruction of the Temple and the subsequent inability to perform material sacrifices, creating a crisis of how to fulfill a central divine command.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic literature and theological discourse from the post-Temple era universally attest to the problem of fulfilling sacrifice commands in exile. The 'study as exercise' interpretation emerged as a widely accepted, though not universally exclusive, solution within the tradition itself.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.0, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the core premise of this reading: study is not a substitute or a lesser form of performance, but a full and intrinsically valuable exercise of the divine command. Therefore, there is no extraction (0.0), no suppression (0.0), and no theater (0.0). Accessibility collapse is high (0.95) because, within this framework, the alternative of 'not studying' is seen as a collapse of commitment itself. Resistance is low (0.05) because this reading is widely accepted within the tradition as a legitimate mode of engagement.
 *
 * PERSPECTIVAL GAP:
 *   Other readings, particularly 'performance_only', would experience this constraint very differently, seeing it as a form of spiritual compromise or even evasion. However, this story focuses solely on the 'study as exercise' reading, which views itself as a direct and uncompromised fulfillment.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community is the primary beneficiary, gaining spiritual merit and maintaining identity. Covenant fidelity is also a beneficiary (as a vindicated proposition). The divine command is the agenda-setter, as its interpretation drives the practice. Messianic aspirations are 'excluded' in the sense that this reading, by fully occupying the commitment through study, may reduce the perceived urgency of material restoration, though it does not deny its ultimate importance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity,
    'Is this constraint a genuine ''study as exercise'' reading, or is it a ''hybrid preparatory'' reading that overstates the completeness of study as performance?',
    'Analysis of rabbinic responsa and theological texts concerning the spiritual efficacy of study versus material performance, particularly in contexts where material performance is theoretically possible but deferred.',
    'If reclassified as ''hybrid preparatory'', extractiveness might rise slightly (as study is then seen as an incomplete substitute), and the claimed type might shift from Mountain to Rope or Tangled Rope, reflecting a more conditional or transitional form of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity, conceptual, 'Distinguishing between study as full performance versus study as preparation for future performance.').

omega_variable(
    natural_law_vs_theological_construct,
    'Is the ''study as exercise'' interpretation an emergent natural law of theological necessity, or a constructed theological response to historical conditions?',
    'Comparative theological analysis across traditions facing similar crises of unperformable commands; philosophical inquiry into the nature of divine command and human intellectual engagement.',
    'If a constructed response, the ''emerges_naturally'' flag would be false, and the constraint would be re-evaluated as a Rope or Tangled Rope, reflecting a human-designed coordination mechanism rather than an irreducible theological truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_theological_construct, conceptual, 'Ambiguity between a natural theological principle and a historically contingent theological construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.0).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
