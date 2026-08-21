% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Deterrence Equilibrium for Total War
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence equilibrium' reading of the
 *   total war possibility space. It posits that total war remains a
 *   strategically viable option, but its initiation is deterred by the
 *   mutually assured destruction (MAD) framework. The constraint is
 *   maintained by continuous investment in nuclear capabilities and strategic
 *   planning, which, while costly, prevents the greater cost of actual total
 *   war. This reading emphasizes the rational calculation of costs and
 *   benefits, and the ongoing need for vigilance and capability maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.3).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.7).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence Equilibrium for Total War").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, 'fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6').
narrative_ontology:cs_kernel_codification('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', formalized).
narrative_ontology:cs_authority_grounding('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', practice).
narrative_ontology:cs_interpretation_layer_present('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6').
narrative_ontology:cs_reading_relation('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', foundational, rational_actor_cost_benefit_calculation).
narrative_ontology:cs_axiom_status(rational_actor_cost_benefit_calculation, holdable).
narrative_ontology:cs_axiom_grounding('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', rational_actor_cost_benefit_calculation, empirically_contingent).
narrative_ontology:cs_axiom('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', foundational, mutual_vulnerability_ensures_deterrence).
narrative_ontology:cs_axiom_status(mutual_vulnerability_ensures_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', mutual_vulnerability_ensures_deterrence, empirically_contingent).
narrative_ontology:cs_reference_frame('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', cold_war_mad_doctrine).
narrative_ontology:cs_drift_state('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', contemporary_multi_polar_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fd7197b9-7ff1-4aa5-8f3e-a0d68044c8e6', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, global_stability).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, military_budgets).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, military_strategists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, develop strategic doctrines, and engage in continuous signaling to ensure mutual vulnerability. They benefit from the deterrence stability but bear the immense cost and risk of maintaining the capability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the absence of total war between major powers, which reduces global conflict risk. However, they are also potential targets or collateral damage in any escalation, and their security is dependent on the stability of the nuclear balance.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, beneficiary,
    moderate, biographical, constrained, global).

% Are tasked with continuously theorizing, planning, and training for total war scenarios, including counterforce strikes and escalation management. Their professional identity is tied to maintaining the strategic option, even if it's never exercised. This requires significant intellectual and resource investment.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, military_strategists, payer,
    organized, biographical, identity_locked, global).

% Bear the ultimate risk of total war, including existential threat. They also pay for the maintenance of nuclear arsenals through taxes and live under the psychological burden of potential annihilation. They have no direct exit from this vulnerability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations, payer,
    powerless, immediate, trapped, global).

% Argue for disarmament and the elimination of nuclear weapons, believing the current equilibrium is too fragile. Their proposals are often dismissed by nuclear powers as undermining deterrence, effectively excluding them from the core strategic conversation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the strategic behavior of nuclear powers by establishing a shared understanding that total war, while possible, carries unacceptable costs, thereby deterring its initiation.
% TRANSFER_FUNCTION: Transfers the risk of total war from active conflict to a state of perpetual readiness and mutual vulnerability, with associated costs in military budgets and psychological burden on populations.
% ABSENT_VOICES: Advocates for nuclear disarmament and those who believe total war is no longer a 'thinkable' option are largely excluded from the strategic planning that maintains the deterrence equilibrium. They would argue for alternative security frameworks.
% DISAPPEARANCE_RATIONALE: If the deterrence equilibrium vanished (e.g., through a breakthrough in missile defense or a collapse of command and control), the strategic landscape would immediately destabilize, likely leading to a rapid arms race, pre-emptive strikes, or conventional conflicts escalating to total war. The world's security architecture is built upon this constraint.
% FOUNDING_PROBLEM: The problem of preventing large-scale, existential conflict between great powers in an era of unprecedented destructive capability.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and national security establishments across nuclear powers consistently attest that the problem of preventing total war remains live and that deterrence is the primary mechanism. Independent international relations scholars also largely corroborate the ongoing relevance of deterrence theory, even while debating its specific mechanisms.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).
:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) reflects the significant, ongoing costs of maintaining nuclear arsenals and strategic readiness, which are extracted from national budgets and populations. Suppression (0.7) is high because the strategic environment actively suppresses any state's ability to escape mutual vulnerability without risking catastrophic instability. Theater ratio is low (0.1) because the threat of total war, while never exercised, is considered genuinely real and the capabilities are functional, not merely performative. The claimed type is 'rope' because, from this reading, it's a coordination mechanism that prevents a worse outcome, despite its costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers and strategists, this is a necessary, albeit costly, rope. From the perspective of civilian populations and arms control advocates, it might appear more like a snare, trapping humanity in a perpetual state of existential risk and resource drain. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are agenda-setters and beneficiaries, as they manage the deterrence and benefit from the resulting stability, despite the costs. Non-nuclear states are beneficiaries of global stability but remain vulnerable. Military strategists are payers, as their profession is dedicated to maintaining the 'thinkable' space of total war. Civilian populations are ultimate payers, bearing the risk and financial burden. Arms control advocates are excluded, as their proposals challenge the core premise of this equilibrium.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_assumption_validity,
    'Is the assumption of rational state actors, central to deterrence theory, consistently valid across all potential decision-makers and crisis scenarios?',
    'Empirical analysis of historical crisis decision-making, psychological studies of high-stress leadership, and game-theoretic modeling of irrational actors.',
    'If rationality is not consistently valid, the deterrence equilibrium is far more fragile than assumed, increasing the effective risk and extractiveness, potentially reclassifying it towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_assumption_validity, empirical, 'Uncertainty regarding the foundational assumption of rational actors in deterrence theory.').

omega_variable(
    technological_stability_of_mad,
    'Will future technological advancements (e.g., advanced missile defense, cyber warfare, AI in command and control) fundamentally destabilize the mutual vulnerability underpinning deterrence?',
    'Ongoing technological development and strategic analysis; wargaming and simulations of future conflict scenarios.',
    'If MAD is destabilized, the constraint either collapses into active total war (if space contraction is false) or transforms into a new, potentially more extractive, security regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_stability_of_mad, empirical, 'Uncertainty about the long-term technological viability of mutual assured destruction.').

omega_variable(
    deterrence_vs_taboo_causality,
    'Is the absence of total war primarily due to the material deterrence equilibrium (this reading) or a constructed normative taboo against nuclear use (nuclear_taboo_reading)?',
    'Historical counterfactual analysis, comparative studies of state behavior in crises, and analysis of public and elite discourse on nuclear weapons.',
    'If the nuclear taboo is the primary driver, this constraint''s extractiveness (costs of maintaining arsenals) is less justified by its coordination function, potentially reclassifying it as a piton or snare. If deterrence is primary, the costs are justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_vs_taboo_causality, conceptual, 'Ambiguity regarding the causal mechanism preventing total war: material deterrence vs. normative taboo.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_proliferation_treaty).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, conventional_arms_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. This 'deterrence_equilibrium_reading' emphasizes the rational calculation of costs and benefits, distinct from the 'space_contraction_reading' (total war unthinkable) and 'nuclear_taboo_reading' (total war normatively prohibited).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
