% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhism Partition (Life-Cycle vs Afterlife)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint describes the 'partition' reading of Shinto-Buddhism
 *   relations in Japan, where the two traditions functionally coexist by
 *   occupying distinct domains (Shinto for life-cycle events, Buddhism for
 *   death/afterlife) without requiring deep ontological integration. This
 *   reading emphasizes functional separation and practitioner autonomy over
 *   doctrinal unity. The constraint is claimed as a Mountain due to its deep
 *   historical entrenchment and perceived naturalness within Japanese
 *   religious practice, despite the presence of beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.05).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, mountain).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhism Partition (Life-Cycle vs Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:emerges_naturally(shinbutsu_ontological_commitment__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '1f61eff1-efa9-4a40-9e9f-f8d45dfabab4').
narrative_ontology:cs_kernel_codification('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', implicit).
narrative_ontology:cs_authority_grounding('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', practice).
narrative_ontology:cs_interpretation_layer_present('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4').
narrative_ontology:cs_reading_relation('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', foundational, functional_domain_autonomy).
narrative_ontology:cs_axiom_status(functional_domain_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', functional_domain_autonomy, conventional).
narrative_ontology:cs_axiom('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', foundational, ontological_non_integration_is_default).
narrative_ontology:cs_axiom_status(ontological_non_integration_is_default, holdable).
narrative_ontology:cs_axiom_grounding('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', ontological_non_integration_is_default, conventional).
narrative_ontology:cs_reference_frame('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', functional_separation_of_domains).
narrative_ontology:cs_drift_state('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', contemporary_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1f61eff1-efa9-4a40-9e9f-f8d45dfabab4', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_monks).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, japanese_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer Shinto rituals for life-cycle events (birth, marriage, festivals) without needing to reconcile with Buddhist doctrines. Their authority is preserved within their domain.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_priests, beneficiary,
    organized, generational, constrained, local).

% Administer Buddhist rituals for death, funerals, and ancestral rites without needing to integrate Shinto kami ontologically. Their authority is preserved within their domain.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_monks, beneficiary,
    organized, generational, constrained, local).

% Navigate life-cycle events and death rituals by engaging both Shinto shrines and Buddhist temples as needed, without perceiving a contradiction or requiring deep theological integration. This functional division simplifies religious practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, japanese_households, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the historical and theological development of Shinto-Buddhism relations, identifying this partition as a distinct interpretive framework for understanding their coexistence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, religious_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious practice by assigning distinct, non-overlapping domains (life-cycle events to Shinto, death/afterlife to Buddhism), allowing practitioners to engage both traditions without requiring ontological synthesis.
% TRANSFER_FUNCTION: Transfers ritual responsibilities and associated social functions to distinct religious institutions, preventing conflict over domain authority and allowing each to specialize.
% ABSENT_VOICES: Theological purists from either tradition who might demand exclusive ontological claims or a unified doctrinal system are largely absent from the mainstream practice this partition describes, or their voices are marginalized by the functional convenience of the arrangement.
% DISAPPEARANCE_RATIONALE: If this functional partition vanished, Japanese religious practice would face immediate ontological and practical contradictions. Households would struggle to navigate life and death rituals, and religious institutions would enter into direct competition or be forced into explicit syncretism, fundamentally altering the landscape of Japanese religiosity.
% FOUNDING_PROBLEM: The historical challenge of integrating indigenous kami worship with the imported, highly systematized Buddhist cosmology without either tradition losing its distinct identity or functional role.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and ethnographers attest to the ongoing functional division in contemporary Japanese religious practice, corroborating that the problem of coexistence without full integration remains a live, practical solution for many households and institutions.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, ExtMetricName, E),
    domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(shinbutsu_ontological_commitment__partition_reading),
    narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this partition primarily facilitates coexistence rather than extracting resources. Suppression is minimal (0.05) as the arrangement is largely self-enforcing through cultural practice, not coercion. Theater ratio is low (0.05) as the functional division is genuine. Accessibility collapse is high (0.88) because for many practitioners, this functional division is the 'natural' way to engage with both traditions, making alternatives (like strict monotheism or full syncretic integration) less accessible or desirable. Resistance is low (0.02) as this reading represents a widely accepted, stable mode of religious practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Japanese households, this partition is a natural and convenient way to engage with religious life. From a theological perspective, the lack of ontological integration might be seen as a conceptual challenge, but for the purposes of this reading, the functional separation is the dominant feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priests and Buddhist monks benefit from clear, non-overlapping domains of authority. Japanese households benefit from a simplified approach to religious practice. There are no clear victims, as the arrangement is seen as mutually beneficial or at least benign. The 'emerges_naturally: true' flag, combined with beneficiaries, triggers False Summit Mountain detection, prompting analysis of whether this 'natural' partition is also a constructed arrangement that benefits specific groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (facilitating coexistence) remains live. The classification as a Mountain (with FSM potential) helps prevent mislabeling a deeply ingrained cultural pattern as pure extraction, while still flagging the presence of beneficiaries for scrutiny. The low extractiveness and suppression suggest it functions more as a stable cultural norm than a coercive structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_partition,
    'Is this functional partition a genuine ''natural law'' of Japanese religious practice, or a historically constructed arrangement that benefits identifiable agents?',
    'Historical analysis of the political and social forces that shaped the division of religious labor, and comparative studies with other cultures'' religious syntheses.',
    'If primarily constructed, the ''mountain'' classification would be challenged, potentially reclassifying it as a ''rope'' or ''tangled_rope'' if active enforcement or asymmetric benefits are identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_partition, conceptual, 'Ambiguity between natural cultural evolution and deliberate institutional shaping of religious domains.').

omega_variable(
    ontological_integration_demand,
    'To what extent do practitioners or theologians genuinely demand ontological integration between Shinto and Buddhism, rather than merely functional coexistence?',
    'Surveys of contemporary religious practitioners and textual analysis of theological discourse to gauge the prevalence and intensity of calls for deeper integration.',
    'If demand for integration is high and suppressed, the ''suppression'' metric might be understated, and the ''partition_reading'' might be seen as an enforced rather than natural state, shifting its classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_integration_demand, empirical, 'The actual demand for ontological integration versus the acceptance of functional partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 150, 0.05).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(shin_be_t50, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 150, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(shin_su_t50, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 100, 0.05).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 150, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_commitment' kernel, focusing on the functional partition between Shinto and Buddhism. The other readings (syncretic, incoherence) offer alternative interpretations of their historical and ontological relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
