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
 *   human_readable: Nuclear Impossibility: Rational Dropout (Costs Exceed Benefits)
 *   domain: strategic_studies/international_relations/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This constraint describes the 'rational dropout' reading of the nuclear
 *   impossibility kernel: nuclear weapons have created a situation where,
 *   while military victory might be theoretically conceivable, the costs
 *   associated with achieving it (mutual destruction) far exceed any rational
 *   benefit. This makes full-scale nuclear war an irrational choice,
 *   effectively removing it from the set of viable strategic options for
 *   rational actors. This reading emphasizes the cost-benefit calculation and
 *   the continued existence of war as a 'reachable' but 'unchoosable' state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.05).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.95).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Impossibility: Rational Dropout (Costs Exceed Benefits)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations/nuclear_deterrence_theory").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'a8e24569-91e7-4970-873c-a53fb5e8aee2').
narrative_ontology:cs_kernel_codification('a8e24569-91e7-4970-873c-a53fb5e8aee2', implicit).
narrative_ontology:cs_authority_grounding('a8e24569-91e7-4970-873c-a53fb5e8aee2', diffuse_epistemic).
narrative_ontology:cs_reading_relation('a8e24569-91e7-4970-873c-a53fb5e8aee2', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8e24569-91e7-4970-873c-a53fb5e8aee2', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('a8e24569-91e7-4970-873c-a53fb5e8aee2', foundational, nuclear_war_costs_exceed_benefits).
narrative_ontology:cs_axiom_status(nuclear_war_costs_exceed_benefits, holdable).
narrative_ontology:cs_axiom_grounding('a8e24569-91e7-4970-873c-a53fb5e8aee2', nuclear_war_costs_exceed_benefits, empirically_contingent).
narrative_ontology:cs_axiom('a8e24569-91e7-4970-873c-a53fb5e8aee2', foundational, actors_are_rational_utility_maximizers).
narrative_ontology:cs_axiom_status(actors_are_rational_utility_maximizers, holdable).
narrative_ontology:cs_axiom_grounding('a8e24569-91e7-4970-873c-a53fb5e8aee2', actors_are_rational_utility_maximizers, conventional).
narrative_ontology:cs_reference_frame('a8e24569-91e7-4970-873c-a53fb5e8aee2', rational_deterrence_equilibrium).
narrative_ontology:cs_drift_state('a8e24569-91e7-4970-873c-a53fb5e8aee2', contemporary_strategic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a8e24569-91e7-4970-873c-a53fb5e8aee2', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, global_stability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, mutually_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the stability of nuclear deterrence, which prevents large-scale conventional wars between them. However, they are also constrained by the existential risk of their own arsenals, making direct conflict irrational. Their identity as nuclear powers is tied to maintaining this balance.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers, beneficiary,
    institutional, generational, identity_locked, global).

% Bear the diffuse costs of living under the nuclear shadow, including the risk of accidental war or proliferation. They are also subject to the strategic calculations of nuclear powers, limiting their foreign policy options. Their ability to exit this system is limited by their lack of nuclear weapons.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, global).

% Study the dynamics of nuclear deterrence, rational choice, and conflict escalation. They provide theoretical frameworks and policy recommendations, but do not directly control the constraint. Their role is to understand and articulate the structural limits.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_analysts, observer,
    analytical, civilizational, analytical, universal).

% Advocate for arms control, non-proliferation, and de-escalation, benefiting from the perceived stability that nuclear deterrence provides, even while seeking to mitigate its risks. They operate within the framework of nuclear reality, seeking to manage its consequences.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, global_stability_advocates, beneficiary,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding among nuclear-armed states that the costs of full-scale nuclear war far outweigh any potential benefits, thereby coordinating their behavior away from direct military confrontation.
% TRANSFER_FUNCTION: Transfers the option of large-scale conventional war between nuclear powers from the realm of rational choice to the realm of unthinkable catastrophe, effectively 'transferring' the cost of such a war into a deterrent effect.
% ABSENT_VOICES: Historical military strategists who believed in the possibility of 'winning' a major war, or those who advocate for pre-emptive strikes based on a belief in survivability, are absent from contemporary rational strategic discourse. Their views are foreclosed by the cost-benefit analysis.
% DISAPPEARANCE_RATIONALE: If the rational impossibility of nuclear victory vanished (e.g., through a technological breakthrough rendering nuclear weapons obsolete or survivable), the strategic landscape would fundamentally rearrange. Conventional war between major powers would become a rational option again, leading to a massive re-militarization and potential for direct conflict.
% FOUNDING_PROBLEM: The problem of preventing large-scale, existential conflict between great powers in an era of increasingly destructive conventional weaponry.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing absence of direct military conflict between major nuclear powers, despite geopolitical tensions, corroborates the live status of this problem. International relations scholars and defense strategists outside of any single nuclear power's direct benefit attest to this.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Mountain because the fundamental cost-benefit calculation, given the destructive power of nuclear weapons, is seen as an unchangeable structural feature of the international system. Extractiveness is very low (0.05) because no party actively 'extracts' from this constraint; rather, it imposes a universal cost of irrationality. Suppression is high (0.95) because the sheer destructive power of nuclear weapons effectively suppresses any rational alternative to avoiding direct conflict. Accessibility collapse is high (0.9) as the option of 'winning' a nuclear war has largely collapsed from rational strategic thought. Resistance is low (0.05) because the constraint is widely accepted by rational actors as an immutable fact of the nuclear age. Theater ratio is low (0.1) as the constraint's operation is primarily functional deterrence, not performative.
 *
 * PERSPECTIVAL GAP:
 *   While all rational actors acknowledge the constraint, nuclear powers might perceive a subtle benefit in the stability it provides, whereas non-nuclear states might perceive only the existential risk. However, the core rational calculation remains consistent across these perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are beneficiaries in that the constraint prevents large-scale conventional wars between them, but they are also targets of the constraint's logic, as it limits their strategic options. Non-nuclear states are payers, bearing the diffuse risk and strategic limitations. Strategic analysts are observers, seeking to understand the constraint's implications.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing existential war) is still very much live. There is no evidence of mandatrophy; the constraint continues to function as intended by its underlying logic. The classification as a Mountain reflects its perceived natural-law-like status in strategic thought.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_assumption_robustness,
    'How robust is the assumption of rational actors in a crisis scenario, and could non-rational factors lead to nuclear use despite the cost-benefit calculation?',
    'Historical analysis of near-misses, psychological studies of decision-making under extreme stress, and theoretical modeling of non-rational escalation pathways.',
    'If rationality is found to be fragile, the constraint''s effective suppression might be lower than perceived, and its classification could shift towards a Snare (if a party could exploit irrationality) or a Tangled Rope (if coordination mechanisms are needed to manage irrationality).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_assumption_robustness, empirical, 'Uncertainty regarding the persistence of rational decision-making under extreme nuclear crisis conditions.').

omega_variable(
    cost_benefit_threshold_drift,
    'Could technological advancements (e.g., missile defense, space-based weapons) or changes in strategic doctrine alter the perceived cost-benefit threshold for nuclear war, making ''victory'' seem less costly?',
    'Ongoing technological assessment, wargaming simulations, and analysis of evolving strategic doctrines among nuclear powers.',
    'If the perceived costs decrease significantly, the constraint''s extractiveness could rise (as the ''cost'' of avoiding war diminishes, making war more thinkable), and its classification could shift away from a Mountain towards a more constructed type like a Tangled Rope or even a Snare if a party believes they can ''win''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_benefit_threshold_drift, empirical, 'Uncertainty about whether technological or doctrinal shifts could change the rational cost-benefit calculation of nuclear war.').

omega_variable(
    reading_distinction_clarity,
    'Is the distinction between ''rational dropout'' (costs exceed benefits) and ''structural contraction'' (physical impossibility of victory) sufficiently clear, or do they represent points on a continuum?',
    'Further conceptual analysis and formal modeling to precisely define the boundary conditions where ''irrationality'' transitions into ''impossibility''.',
    'If the distinction blurs, the ''rational dropout'' reading might be subsumed into the ''structural contraction'' reading, potentially leading to a re-evaluation of the Mountain classification if the ''physical impossibility'' claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinction_clarity, conceptual, 'Conceptual clarity between the rational-choice and physical-impossibility readings of nuclear deterrence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1991, 0.08).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.04).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1991, 0.03).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2010, 0.04).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.9).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1980, 0.95).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1991, 0.9).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, conventional_arms_race_constraint).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, non_proliferation_treaty).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nuclear impossibility kernel'. This 'rational dropout' reading emphasizes the cost-benefit calculation, while the 'structural contraction' reading focuses on physical impossibility, and the 'credibility paradox' reading on the inherent incredibility of nuclear threats. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
