% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology: Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'immutable diagnostic' reading of the
 *   Deferential Realism ontology, asserting that the constraint typology is
 *   an objective observational instrument with fixed referents. Mountains are
 *   physical invariants, snares are measurable extraction mechanisms, and
 *   misclassification is an error correctable through better observation.
 *   This reading emphasizes the discoverable nature of constraint types and
 *   the framework's role in providing an objective, stable diagnostic tool.
 *   It is a 'mountain' in its own self-conception, claiming to describe an
 *   inherent structure of reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.1).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.7).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, mountain).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology: Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'd4e4eb50-fc60-407d-bdb2-ccbe88002071').
narrative_ontology:cs_kernel_codification('d4e4eb50-fc60-407d-bdb2-ccbe88002071', formalized).
narrative_ontology:cs_authority_grounding('d4e4eb50-fc60-407d-bdb2-ccbe88002071', expertise).
narrative_ontology:cs_interpretation_layer_present('d4e4eb50-fc60-407d-bdb2-ccbe88002071').
narrative_ontology:cs_reading_relation('d4e4eb50-fc60-407d-bdb2-ccbe88002071', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('d4e4eb50-fc60-407d-bdb2-ccbe88002071', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('d4e4eb50-fc60-407d-bdb2-ccbe88002071', foundational, typology_has_fixed_referents).
narrative_ontology:cs_axiom_status(typology_has_fixed_referents, holdable).
narrative_ontology:cs_axiom_grounding('d4e4eb50-fc60-407d-bdb2-ccbe88002071', typology_has_fixed_referents, deontological).
narrative_ontology:cs_axiom('d4e4eb50-fc60-407d-bdb2-ccbe88002071', secondary, misclassification_is_observational_error).
narrative_ontology:cs_axiom_status(misclassification_is_observational_error, holdable).
narrative_ontology:cs_axiom_grounding('d4e4eb50-fc60-407d-bdb2-ccbe88002071', misclassification_is_observational_error, empirically_contingent).
narrative_ontology:cs_reference_frame('d4e4eb50-fc60-407d-bdb2-ccbe88002071', objective_diagnostic_instrument).
narrative_ontology:cs_drift_state('d4e4eb50-fc60-407d-bdb2-ccbe88002071', contemporary_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d4e4eb50-fc60-407d-bdb2-ccbe88002071', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, analytical_observers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, constraint_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a stable, objective framework for classifying constraints, allowing for consistent analysis and comparison across diverse domains. Views misclassification as an error in observation or measurement, not a flaw in the framework itself.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, analytical_observers, beneficiary,
    analytical, generational, analytical, universal).

% The framework itself, as an abstract entity, benefits from its principles being treated as immutable and universally applicable. Its persistence relies on the belief that its categories correspond to fixed referents in reality.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_framework, agenda_setter,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_framework).

% Would argue that the typology is primarily a normative tool for critique, where 'snare' is a declaration based on values, not a discovery. Their perspective is suppressed by the immutable diagnostic reading's insistence on objective referents.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_proponents, excluded,
    moderate, biographical, constrained, global).

% Would argue for a mixed view, where some categories are fixed but others are subject to normative judgment. Their nuanced position is foreclosed by the immutable diagnostic reading's insistence on universal objectivity.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_proponents, excluded,
    moderate, biographical, constrained, global).

% Individuals or groups subject to constraints classified by the framework. They are indirectly affected by the framework's interpretation, as an 'immutable diagnostic' classification might legitimize their situation as an objective fact rather than a remediable construct.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constraint_targets, payer,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, objective language and methodology for identifying and classifying constraints across diverse domains, enabling consistent analysis and communication among researchers and practitioners.
% TRANSFER_FUNCTION: Transfers epistemic authority and analytical rigor to the framework and its users, by asserting that its classifications are discoverable facts about the world, rather than subjective interpretations.
% ABSENT_VOICES: Proponents of more constructivist or pragmatic views of the typology are excluded. They would argue that the framework's categories are not purely objective but are shaped by normative choices and rhetorical aims. Their voices are suppressed by the dominant 'immutable diagnostic' framing.
% DISAPPEARANCE_RATIONALE: If this reading of the DR ontology vanished, the entire analytical project of Deferential Realism would lose its foundational claim to objectivity and fixed referents. Classification disputes would become purely normative or rhetorical, and the framework's utility as a diagnostic instrument would collapse, forcing a fundamental re-evaluation of its purpose and methods.
% FOUNDING_PROBLEM: The problem of inconsistent and subjective classification of constraints, leading to analytical ambiguity and an inability to compare different constraint mechanisms on a common, objective basis.
% FOUNDING_PROBLEM_CORROBORATION: The analytical community within Deferential Realism, as well as external observers seeking objective tools for institutional analysis, corroborate the ongoing need for a consistent and objective classification system. This corroboration comes from outside the immediate beneficiaries of the 'immutable diagnostic' reading, as it addresses a general epistemic problem.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, ExtMetricName, E),
    domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because this reading primarily extracts epistemic authority and analytical consistency, not material resources. Suppression is high (0.7) because this reading actively suppresses alternative framings that challenge its claim to objective, fixed referents. The 'emerges_naturally: true' flag reflects this reading's core claim that the typology describes natural or inherent structures. The claimed type is 'mountain' because this reading asserts its own foundational, immutable nature.
 *
 * PERSPECTIVAL GAP:
 *   Analytical observers, as beneficiaries, experience this reading as a robust, stable foundation for their work. Proponents of alternative readings (rhetorical scaffold, hybrid pragmatic) are excluded and experience this reading as a suppressive force that forecloses their interpretive flexibility. The framework itself, as an abstract agenda-setter, benefits from its own reification as an objective truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Analytical observers and the DR framework itself are beneficiaries, as they gain epistemic authority and stability. Proponents of alternative readings are effectively targets, as their perspectives are suppressed. Constraint targets are indirectly payers, as the 'objective' classification of their situation can limit avenues for critique or change.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading, by asserting its own immutable and diagnostic nature, inherently resists mandatrophy. Its mandate is to provide objective classification, a problem it claims is always 'live'. The high suppression of alternative framings is precisely what prevents its function from being re-evaluated or declared obsolete by external criteria. The challenge of mandatrophy for this reading is whether its 'objective' referents are truly immutable or are themselves subject to conceptual drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_conceptual_choice,
    'Is the fixed referent claim of the DR typology a discovery of natural law, or a conceptual choice made to stabilize the analytical framework?',
    'Analysis of the historical evolution of the typology''s categories and their responsiveness to shifts in normative theory or empirical observation. If categories shift in response to conceptual rather than empirical pressures, it suggests a conceptual choice.',
    'If a conceptual choice, the ''emerges_naturally'' claim would be reclassified as ''false'', and the constraint would likely shift from Mountain to a more constructed type (e.g., Rope or Tangled Rope), with higher extractiveness from those whose alternative framings are suppressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_conceptual_choice, conceptual, 'Ambiguity between discovered natural law and constructed conceptual framework.').

omega_variable(
    suppression_of_alternative_framings,
    'To what extent does the ''immutable diagnostic'' reading actively suppress alternative framings of the typology, rather than merely being a more accurate description?',
    'Content analysis of academic discourse, funding patterns for research on the typology, and institutional responses to challenges to the ''immutable diagnostic'' view. Evidence of active exclusion or marginalization would indicate suppression.',
    'If active suppression is high, the ''suppression'' metric would be confirmed, and the classification might lean towards a more extractive type (e.g., Snare or Tangled Rope) if the suppression serves to maintain the epistemic authority of the beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_framings, empirical, 'Distinguishing between accurate description and active suppression of alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.1).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Deferential Realism Ontology' kernel. This 'immutable diagnostic' reading asserts fixed referents, while the 'rhetorical scaffold' reading views the typology as a normative critique tool, and the 'hybrid pragmatic' reading sees a mix of fixed and contested categories. All three are structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
