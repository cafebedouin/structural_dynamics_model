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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhism Partition (Life-Cycle vs. Afterlife)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint describes the 'partition reading' of the historical and
 *   contemporary relationship between Shinto and Buddhism in Japan. It posits
 *   that the two traditions functionally coexist by occupying separate
 *   domains of practice (Shinto for life-cycle events and local kami,
 *   Buddhism for death and the afterlife) without requiring deep ontological
 *   integration. This reading emphasizes the practical, non-conflicting
 *   nature of their interaction, where practitioners fluidly move between
 *   traditions based on ritual need. The constraint is claimed as a Mountain
 *   due to its perceived natural emergence from cultural practice and its low
 *   extractiveness, but with beneficiaries declared to trigger False Summit
 *   Mountain (FSM) evaluation, as the 'naturalness' benefits identifiable
 *   institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.1).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.05).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, mountain).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhism Partition (Life-Cycle vs. Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:emerges_naturally(shinbutsu_ontological_commitment__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '5a123ac9-c677-4cff-bbf8-5b15981cd9a6').
narrative_ontology:cs_kernel_codification('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', implicit).
narrative_ontology:cs_authority_grounding('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', practice).
narrative_ontology:cs_interpretation_layer_present('5a123ac9-c677-4cff-bbf8-5b15981cd9a6').
narrative_ontology:cs_reading_relation('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', foundational, functional_differentiation_is_optimal).
narrative_ontology:cs_axiom_status(functional_differentiation_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', functional_differentiation_is_optimal, conventional).
narrative_ontology:cs_axiom('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', foundational, ontological_integration_is_unnecessary).
narrative_ontology:cs_axiom_status(ontological_integration_is_unnecessary, holdable).
narrative_ontology:cs_axiom_grounding('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', ontological_integration_is_unnecessary, deontological).
narrative_ontology:cs_reference_frame('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', functional_coexistence_framework).
narrative_ontology:cs_drift_state('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', contemporary_religious_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5a123ac9-c677-4cff-bbf8-5b15981cd9a6', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, japanese_households).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, functional_differentiation_of_religious_practice).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, ontological_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear domain of practice (life-cycle rituals, local kami worship) that avoids direct competition or doctrinal conflict with Buddhism, allowing for stable institutional operation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_shrines, beneficiary,
    institutional, generational, mobile, local).

% Benefit from a clear domain of practice (funerary rites, ancestor veneration, afterlife concerns) that avoids direct competition or doctrinal conflict with Shinto, allowing for stable institutional operation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_temples, beneficiary,
    institutional, generational, mobile, local).

% Benefit from a clear division of religious labor, allowing them to engage with both traditions for different life events without perceived contradiction or the need for complex theological reconciliation. They access Shinto for births, weddings, and local festivals, and Buddhism for funerals and ancestor care.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, japanese_households, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the historical and contemporary relationship between Shinto and Buddhism, observing the functional partition and its implications for Japanese religious identity and practice. Their analysis informs the understanding of this constraint.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, religious_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the functional division of religious labor in Japanese society, allowing Shinto and Buddhist institutions to coexist and serve distinct ritual and spiritual needs without direct competition or doctrinal integration.
% TRANSFER_FUNCTION: Transfers ritual responsibilities and spiritual authority for life-cycle events to Shinto, and for death/afterlife events to Buddhism, from a unified, syncretic religious authority to functionally specialized institutions.
% ABSENT_VOICES: Theological purists from either tradition who might argue for exclusive ontological claims or a unified, integrated cosmology; their voices are marginalized by the practical efficacy of the partition.
% DISAPPEARANCE_RATIONALE: If the functional partition vanished, Japanese religious practice would face significant disruption. Households would lose a clear framework for engaging with life and death rituals, and Shinto and Buddhist institutions would enter direct competition, potentially leading to doctrinal conflicts or a forced syncretism that many practitioners do not currently require.
% FOUNDING_PROBLEM: The historical challenge of integrating indigenous kami worship with the imported Buddhist cosmology and institutional structures without either tradition losing its distinct identity or functional role.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and practitioners widely attest that the functional partition remains a live solution to managing the coexistence of Shinto and Buddhism, even after the Meiji-era separation policies. Ethnographic studies and historical analyses from outside the immediate religious institutions corroborate this ongoing functional differentiation.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).

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
 *   Extractiveness is low (0.1) because the partition primarily facilitates coexistence rather than extracting resources. Suppression is low (0.05) as the partition is maintained by cultural practice and mutual benefit, not active coercion. Theater ratio is zero, as the functional division is genuine and not performative. Accessibility collapse is high (0.9) because, from a practitioner's perspective, the alternatives to this functional division (e.g., a single, integrated religious system) are largely absent or culturally unappealing. Resistance is low (0.05) because the arrangement is widely accepted and beneficial to most stakeholders.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Japanese households, this partition is a natural and convenient way to navigate religious life. From the perspective of religious scholars, it is a historically contingent but highly stable cultural arrangement. The engine's FSM evaluation will test whether this 'naturalness' masks an underlying constructedness that benefits the religious institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto shrines and Buddhist temples are beneficiaries, as the partition allows them to maintain distinct institutional identities and revenue streams without direct competition. Japanese households are also beneficiaries, as they gain a clear and convenient framework for religious practice. Religious scholars are observers, analyzing the system without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (facilitating coexistence) remains live, as evidenced by the 'live' status of the founding problem. The low extractiveness and suppression, combined with the high accessibility collapse, suggest a stable, self-reinforcing cultural pattern rather than a decaying or actively extractive one. The FSM evaluation will be crucial to confirm if the 'mountain' claim holds under scrutiny of its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_partition,
    'Is the functional partition between Shinto and Buddhism a naturally emergent cultural pattern, or a historically constructed arrangement that benefits identifiable religious institutions?',
    'Comparative historical analysis of other cultures'' religious syncretism, or counterfactual modeling of Japanese religious history without the partition. If similar partitions are rare or unstable elsewhere, it suggests a constructed element.',
    'If primarily constructed, the constraint''s ''mountain'' classification would be re-evaluated towards a ''rope'' or ''tangled_rope'', acknowledging the active maintenance and beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_partition, conceptual, 'Ambiguity between natural cultural evolution and institutional construction.').

omega_variable(
    doctrinal_integration_potential,
    'To what extent is deeper ontological or doctrinal integration between Shinto and Buddhism genuinely impossible or undesirable for practitioners, versus merely unattempted due to the stability of the partition?',
    'Ethnographic studies exploring practitioner desires for integration, or analysis of historical periods where integration was more actively pursued (e.g., honji-suijaku) to assess its viability and perceived benefits/costs.',
    'If integration is genuinely undesirable, the partition is more robust. If merely unattempted, the ''accessibility_collapse'' might be overstated, and the constraint could be more easily altered.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_integration_potential, empirical, 'The true potential and desirability of deeper doctrinal integration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 1600, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1600, 0.0).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1700, 0.0).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(shin_tr_t1900, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(shin_tr_t2020, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 2020, 0.0).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1600, 0.1).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1700, 0.1).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(shin_be_t1900, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(shin_be_t2020, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 2020, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(shin_su_t1900, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(shin_su_t2020, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 2020, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_commitment' kernel. This 'partition_reading' emphasizes functional coexistence without deep integration, distinct from the 'syncretic_reading' (unified cosmology) and 'incoherence_reading' (institutional tolerance of disunity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
