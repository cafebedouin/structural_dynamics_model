% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Rational Impossibility of Nuclear Victory
 *   domain: strategic_studies/international_relations/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This constraint describes the rational-choice limit imposed by nuclear
 *   weapons: while military victory might be structurally conceivable, the
 *   costs associated with achieving it through nuclear conflict fundamentally
 *   outweigh any potential benefits, rendering such a path irrational for any
 *   state actor. It is a reading of the 'nuclear impossibility kernel' that
 *   emphasizes the rational calculus rather than physical impossibility or
 *   credibility paradoxes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.1).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.9).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Rational Impossibility of Nuclear Victory").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations/nuclear_deterrence_theory").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'effddc94-d488-440a-aa69-1bc811343ca7').
narrative_ontology:cs_kernel_codification('effddc94-d488-440a-aa69-1bc811343ca7', implicit).
narrative_ontology:cs_authority_grounding('effddc94-d488-440a-aa69-1bc811343ca7', self_enforcing).
narrative_ontology:cs_reading_relation('effddc94-d488-440a-aa69-1bc811343ca7', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('effddc94-d488-440a-aa69-1bc811343ca7', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('effddc94-d488-440a-aa69-1bc811343ca7', foundational, rational_actors_avoid_unacceptable_costs).
narrative_ontology:cs_axiom_status(rational_actors_avoid_unacceptable_costs, holdable).
narrative_ontology:cs_axiom_grounding('effddc94-d488-440a-aa69-1bc811343ca7', rational_actors_avoid_unacceptable_costs, empirically_contingent).
narrative_ontology:cs_axiom('effddc94-d488-440a-aa69-1bc811343ca7', foundational, nuclear_war_costs_exceed_benefits).
narrative_ontology:cs_axiom_status(nuclear_war_costs_exceed_benefits, holdable).
narrative_ontology:cs_axiom_grounding('effddc94-d488-440a-aa69-1bc811343ca7', nuclear_war_costs_exceed_benefits, empirically_contingent).
narrative_ontology:cs_reference_frame('effddc94-d488-440a-aa69-1bc811343ca7', cold_war_deterrence_logic).
narrative_ontology:cs_drift_state('effddc94-d488-440a-aa69-1bc811343ca7', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('effddc94-d488-440a-aa69-1bc811343ca7', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, global_population).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and maintain nuclear arsenals, thereby creating and upholding the conditions for this rational constraint. They benefit from avoiding direct conflict but bear the costs and risks of maintaining deterrence.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the absence of large-scale, existential wars between great powers, which this constraint helps prevent. They have limited agency over the constraint itself.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, beneficiary,
    organized, biographical, constrained, global).

% The ultimate beneficiary of avoiding nuclear war, as their survival is at stake. They are trapped by the existence of nuclear weapons but benefit from the rational constraint on their use.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, global_population, beneficiary,
    powerless, civilizational, trapped, universal).

% Study and interpret the implications of nuclear weapons on international relations, contributing to the understanding and reinforcement of this rational constraint through academic discourse and policy advice.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, diffuse).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates states to avoid direct military conflict as a rational policy option, by making the costs of such conflict exceed any conceivable benefit.
% TRANSFER_FUNCTION: Transfers the perceived rationality of achieving military victory through large-scale conflict from states, in exchange for avoiding catastrophic destruction.
% ABSENT_VOICES: Future generations (who would inherit a post-nuclear world) and hypothetical irrational actors (whose behavior is not accounted for by the constraint's premise).
% DISAPPEARANCE_RATIONALE: If the rational constraint vanished (e.g., through a technological breakthrough rendering nuclear weapons harmless, or a fundamental shift in human rationality), the calculus of war would be entirely rewritten, leading to a massive and unpredictable geopolitical rearrangement.
% FOUNDING_PROBLEM: Preventing large-scale, existential wars between great powers in an era of weapons of mass destruction.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing maintenance of nuclear arsenals, active deterrence postures, international treaties (e.g., NPT), and a broad academic consensus in strategic studies all corroborate the continued relevance of this founding problem.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint primarily prevents a highly destructive outcome, rather than actively extracting from agents. Suppression is high because the alternative of rational nuclear war is almost completely foreclosed. Theater ratio is moderate, reflecting the ongoing need for signaling, posturing, and modernization to maintain deterrence credibility, even if the underlying rational constraint is stable. Accessibility collapse is very high as the rational path to victory is effectively closed. Resistance is low because rational actors generally accept the premise of nuclear irrationality, though they may seek to circumvent its implications through other means.
 *
 * PERSPECTIVAL GAP:
 *   While nuclear powers and strategic analysts generally agree on the rational impossibility of nuclear victory, there can be differences in how the 'costs' are weighed against 'benefits,' or the degree to which 'victory' is truly structurally possible. However, the core premise of prohibitive costs remains widely accepted across rational actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are both the primary agents maintaining the constraint (through deterrence) and beneficiaries of the stability it provides. Non-nuclear states and the global population are beneficiaries, as the constraint prevents existential threats. There are no direct 'victims' of this constraint, as its function is to prevent catastrophic victimhood.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint—preventing existential war—remains acutely live. There is no evidence of mandatrophy; the constraint's function is as critical today as it was at its inception.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is the primary constraint imposed by nuclear weapons a rational-choice limit, a physical impossibility, or a credibility paradox?',
    'Conceptual analysis and empirical observation of state behavior and strategic discourse: if states consistently act as if victory is physically impossible, it supports the structural contraction reading; if they struggle with credible threats, it supports the credibility paradox reading.',
    'If the constraint is primarily a physical impossibility, its ''mountain'' nature is more absolute; if it''s a credibility paradox, it might be a ''tangled_rope'' of signaling and bluffing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the fundamental nature of the nuclear impossibility constraint.').

omega_variable(
    rational_actor_assumption_validity,
    'To what extent can all state actors be reliably assumed to be rational in their nuclear decision-making?',
    'Empirical study of historical crises, psychological analysis of decision-making under stress, and assessment of non-state actor proliferation risks.',
    'If the rational actor assumption is frequently violated, the constraint''s effectiveness as a ''mountain'' of rational choice is undermined, potentially shifting it towards a ''snare'' for those trapped by irrationality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption_validity, empirical, 'The reliability of the rational actor assumption in nuclear deterrence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(nucl_tr_t1960, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(nucl_tr_t2000, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(nucl_be_t1960, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(nucl_be_t2000, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(nucl_su_t1960, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1960, 0.85).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1980, 0.9).
narrative_ontology:measurement(nucl_su_t2000, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_proliferation_regime).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, conventional_arms_control_treaties).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, great_power_competition_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nuclear_impossibility_kernel', focusing on the rational-choice aspect. It coexists with the 'structural_contraction_reading' (physical impossibility) and the 'credibility_paradox_reading' (inherent incredibility of the threat).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
