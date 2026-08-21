% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Declaratory Theory of Statehood (Montevideo Convention)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the declaratory theory of statehood, as
 *   codified in the Montevideo Convention (1933). Under this reading,
 *   statehood is an objective legal fact established by meeting four
 *   criteria: a permanent population, a defined territory, government, and
 *   capacity to enter into relations with other states. Recognition by other
 *   states is merely 'declaratory' of an existing fact, not 'constitutive' of
 *   statehood itself. This reading is presented as a Mountain due to its
 *   claim of objective, fact-based legal status, though its beneficiaries (de
 *   facto authorities) trigger FSM evaluation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.25).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.15).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, mountain).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Declaratory Theory of Statehood (Montevideo Convention)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '76689214-601f-4a93-b481-a9ea54947ada').
narrative_ontology:cs_kernel_codification('76689214-601f-4a93-b481-a9ea54947ada', formalized).
narrative_ontology:cs_authority_grounding('76689214-601f-4a93-b481-a9ea54947ada', lineage).
narrative_ontology:cs_interpretation_layer_present('76689214-601f-4a93-b481-a9ea54947ada').
narrative_ontology:cs_reading_relation('76689214-601f-4a93-b481-a9ea54947ada', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_reading_relation('76689214-601f-4a93-b481-a9ea54947ada', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('76689214-601f-4a93-b481-a9ea54947ada', foundational, statehood_is_objective_fact).
narrative_ontology:cs_axiom_status(statehood_is_objective_fact, holdable).
narrative_ontology:cs_axiom_grounding('76689214-601f-4a93-b481-a9ea54947ada', statehood_is_objective_fact, deontological).
narrative_ontology:cs_axiom('76689214-601f-4a93-b481-a9ea54947ada', foundational, recognition_is_declaratory_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_declaratory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('76689214-601f-4a93-b481-a9ea54947ada', recognition_is_declaratory_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('76689214-601f-4a93-b481-a9ea54947ada', montevideo_convention_1933).
narrative_ontology:cs_drift_state('76689214-601f-4a93-b481-a9ea54947ada', contemporary_international_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('76689214-601f-4a93-b481-a9ea54947ada', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, international_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, existing_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, self_determination_principle).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, legal_positivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the authorities that have established effective control over a territory and population, meeting the Montevideo criteria. Under the declaratory reading, their statehood is a legal fact, not dependent on external recognition. They benefit from this legal status, even if recognition is withheld.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities, beneficiary,
    organized, biographical, constrained, local).

% Existing states are legally bound to acknowledge the statehood of entities meeting the criteria, even if they prefer not to. This limits their political leverage to condition recognition on other factors, which can be seen as a cost to their diplomatic flexibility.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_states, payer,
    institutional, generational, constrained, global).

% The international legal system benefits from a clear, objective standard for statehood, which reduces ambiguity and potential for political manipulation. It reinforces the idea of international law as a self-executing system based on facts, rather than political consensus.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_legal_system, beneficiary,
    institutional, civilizational, analytical, universal).

% States from which new entities claim independence. Under the declaratory reading, their ability to deny the legal fact of statehood to a secessionist entity that meets the criteria is diminished, reducing their leverage in disputes.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states, payer,
    institutional, generational, constrained, national).

% Scholars and practitioners who adhere to the constitutive theory of statehood, believing that recognition by existing states is essential. Their perspective is excluded from the declaratory reading's legal framework, as it prioritizes objective facts over political acts.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, constitutive_theorists, excluded,
    analytical, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an objective, fact-based standard for determining when a political entity qualifies as a state, thereby coordinating international legal and diplomatic interactions around a common understanding of sovereignty.
% TRANSFER_FUNCTION: Transfers legal status and associated rights/duties from the realm of political discretion to objective fact, from existing states (who might withhold recognition) to new entities (who meet the criteria).
% ABSENT_VOICES: Proponents of the constitutive theory of statehood are structurally excluded from this reading's framework, as their core premise (recognition as a prerequisite) is denied. They would argue that statehood is a political act, not merely a factual one.
% DISAPPEARANCE_RATIONALE: If the declaratory theory vanished, statehood would revert to a purely political act of recognition, leading to increased instability, arbitrary denials of sovereignty, and a less predictable international legal order. De facto authorities would lose their legal standing, and existing states would gain unchecked power to grant or deny statehood.
% FOUNDING_PROBLEM: The problem of arbitrary and politically motivated recognition of new states, leading to instability and lack of clarity in international relations.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and many emerging states corroborate that the problem of arbitrary recognition remains live, and the declaratory theory provides a crucial counter-balance. Some established states, however, contest its full applicability in practice.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, ExtMetricName, E),
    domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily establishes a legal fact rather than extracting resources. Any 'extraction' is from existing states losing political leverage. Suppression is low (0.15) as it's a legal principle, not actively enforced coercion, though it suppresses the political discretion of existing states. Theater ratio is low (0.1) as the criteria are generally applied in good faith, though political non-recognition can introduce performative elements. Accessibility collapse is high (0.8) because if an entity meets the criteria, its statehood is legally established, collapsing alternatives to that status. Resistance is low (0.1) as the principle is widely accepted in international law, though often challenged in practice by states preferring the constitutive view.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of de facto authorities, this is a Mountain that grants them inherent rights. From the perspective of existing states, it's a Rope that binds their political actions. The engine's FSM will evaluate the Mountain claim against the presence of beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities meeting the criteria are beneficiaries, gaining legal status. The international legal system benefits from clarity. Existing states and especially parent states are payers, as their political discretion to deny statehood is constrained. Constitutive theorists are excluded, as their core premise is rejected by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The declaratory theory's mandate remains live: it continues to address the problem of arbitrary recognition. Its persistence is not due to atrophy but to its ongoing function in international law, despite challenges from the constitutive view. The low theater ratio reflects this functional persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_principle,
    'Is the declaratory theory of statehood a genuine natural law (a structural feature of international reality) or a constructed legal principle that benefits identifiable agents?',
    'Analysis of historical state formation patterns independent of legal codification, and the degree to which states consistently adhere to the principle even when politically inconvenient.',
    'If a genuine natural law, its classification as Mountain is robust. If a constructed principle, the presence of beneficiaries (de_facto_authorities) would push it towards a Tangled Rope or Snare, reflecting its role in distributing power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_principle, conceptual, 'Ambiguity between inherent legal fact and politically beneficial construct.').

omega_variable(
    recognition_practice_drift,
    'To what extent does the actual practice of state recognition by existing states diverge from the declaratory principle, and is this divergence acknowledged?',
    'Empirical study of recognition patterns, particularly in contested cases (e.g., Kosovo, Palestine), and analysis of official statements by states regarding their recognition policies.',
    'Significant unacknowledged drift towards constitutive practice would indicate a higher theater ratio and potentially a reclassification towards Piton or Tangled Rope, as the stated principle becomes performative cover for political discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_practice_drift, empirical, 'Gap between declaratory theory and actual state practice in recognition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.2).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1990, 0.24).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1990, 0.14).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
