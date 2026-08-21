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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This constraint describes the strategic reality where total war remains a
 *   theoretical possibility, but its actualization is deterred by the
 *   catastrophic costs of mutual nuclear vulnerability. It is a 'rope' in the
 *   sense that it coordinates state behavior around a shared, albeit
 *   terrifying, understanding of consequences. The constraint requires
 *   continuous, active enforcement through the maintenance of credible
 *   nuclear arsenals and the articulation of strategic doctrines. This is one
 *   reading of the 'total_war_possibility_space' kernel, emphasizing material
 *   capabilities and rational calculation.
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
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence Equilibrium for Total War").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, 'b48d51f4-2252-46b4-830d-847e9c3d6272').
narrative_ontology:cs_kernel_codification('b48d51f4-2252-46b4-830d-847e9c3d6272', formalized).
narrative_ontology:cs_authority_grounding('b48d51f4-2252-46b4-830d-847e9c3d6272', practice).
narrative_ontology:cs_interpretation_layer_present('b48d51f4-2252-46b4-830d-847e9c3d6272').
narrative_ontology:cs_reading_relation('b48d51f4-2252-46b4-830d-847e9c3d6272', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('b48d51f4-2252-46b4-830d-847e9c3d6272', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('b48d51f4-2252-46b4-830d-847e9c3d6272', foundational, rational_actor_calculus).
narrative_ontology:cs_axiom_status(rational_actor_calculus, holdable).
narrative_ontology:cs_axiom_grounding('b48d51f4-2252-46b4-830d-847e9c3d6272', rational_actor_calculus, empirically_contingent).
narrative_ontology:cs_axiom('b48d51f4-2252-46b4-830d-847e9c3d6272', foundational, mutual_assured_destruction_credibility).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_credibility, holdable).
narrative_ontology:cs_axiom_grounding('b48d51f4-2252-46b4-830d-847e9c3d6272', mutual_assured_destruction_credibility, empirically_contingent).
narrative_ontology:cs_reference_frame('b48d51f4-2252-46b4-830d-847e9c3d6272', cold_war_strategic_stability).
narrative_ontology:cs_drift_state('b48d51f4-2252-46b4-830d-847e9c3d6272', contemporary_multi_polar_era, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('b48d51f4-2252-46b4-830d-847e9c3d6272', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, global_stability).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, military_planners).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, develop strategic doctrines, and engage in signaling to ensure mutual vulnerability. They benefit from the deterrence stability but bear the immense cost and risk of maintaining the capability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the absence of total war between major powers, which reduces global instability and the risk of conventional conflict escalation. They have limited direct influence on the deterrence dynamic but are deeply affected by its success or failure.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, beneficiary,
    moderate, biographical, constrained, global).

% Are tasked with continuously theorizing, planning, and preparing for total war scenarios, including counterforce strikes and escalation ladders, despite the high costs and low probability of execution. Their professional identity is tied to maintaining strategic readiness.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, military_planners, payer,
    organized, biographical, identity_locked, national).

% Bear the financial burden of maintaining vast nuclear arsenals and the associated strategic infrastructure, through taxes. They have little direct say in strategic policy but fund its existence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, taxpayers, payer,
    powerless, immediate, trapped, national).

% The abstract outcome of the deterrence equilibrium, representing the avoidance of catastrophic global conflict. It is a beneficiary in an analytical sense, as it 'receives' the benefit of non-war.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, global_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(total_war_possibility_space__deterrence_equilibrium_reading, global_stability).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the strategic behavior of nuclear-armed states by establishing a shared understanding that initiating total war would result in unacceptable mutual destruction, thereby preventing its occurrence.
% TRANSFER_FUNCTION: Transfers resources (trillions of dollars, scientific talent, political capital) from national economies to military-industrial complexes for the maintenance of nuclear arsenals and strategic planning, in exchange for the perceived security of deterrence.
% ABSENT_VOICES: Future generations, who bear the long-term risks of nuclear proliferation and accidental war, are absent. They would likely advocate for disarmament or alternative security architectures, but their interests are not directly represented in current strategic calculus.
% DISAPPEARANCE_RATIONALE: If the deterrence equilibrium vanished overnight (e.g., through a technological breakthrough rendering nuclear weapons obsolete or a sudden loss of mutual vulnerability), the strategic landscape would fundamentally shift. States would likely re-evaluate their security postures, potentially leading to a new arms race, conventional conflicts, or a scramble for new deterrents, fundamentally reorganizing global power dynamics.
% FOUNDING_PROBLEM: The problem of preventing catastrophic, civilization-ending conflict in an era where nation-states possess the capability to wage total war.
% FOUNDING_PROBLEM_CORROBORATION: Strategic analysts, international relations scholars, and government defense ministries across multiple nuclear and non-nuclear states corroborate that the threat of total war remains a live problem, and deterrence is the primary mechanism preventing it. This is attested through academic publications, defense white papers, and international security dialogues.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.3) reflecting the immense financial and opportunity costs of maintaining nuclear deterrence, but it's not 'pure extraction' as it provides a perceived benefit of preventing total war. Suppression is high (0.7) because the strategic environment actively suppresses any deviation from the deterrence logic, punishing states that fail to maintain credible deterrents or that challenge the equilibrium. Theater ratio is low (0.1) because the threat is largely real and the capabilities are functional, though some aspects of strategic signaling might be performative. Accessibility collapse is moderate (0.6) as alternatives to deterrence (e.g., disarmament, global governance) are theoretically available but practically difficult to achieve given state sovereignty and trust issues. Resistance is low (0.15) because while there are anti-nuclear movements, the core logic of deterrence is widely accepted by state actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers, the deterrence equilibrium is a necessary, albeit costly, mechanism for global stability. From the perspective of military planners, it's a constant, high-stakes intellectual and logistical challenge. From the perspective of taxpayers, it's a massive, unavoidable expenditure. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are agenda-setters and beneficiaries, as they control the deterrence mechanism and benefit from the stability it provides, despite the costs. Non-nuclear states are beneficiaries, as they gain from the absence of total war without direct cost. Military planners and taxpayers are payers, bearing the direct costs of maintaining the deterrent. Global stability is an analytical beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing total war) is still very much live, so mandatrophy is not resolved. The continuous investment in war-fighting capability, even if never used, is seen as essential to maintaining the deterrence, preventing the constraint from atrophying into a piton. The high suppression and moderate extractiveness indicate it is actively maintained, not merely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_of_actors,
    'Does the deterrence equilibrium rely on perfectly rational actors, and what happens if rationality breaks down?',
    'Empirical studies of decision-making under extreme stress, historical analysis of near-misses, and psychological modeling of leadership behavior in crisis.',
    'If actors are consistently less rational than assumed, the ''rope'' classification might be too optimistic, potentially shifting towards a ''snare'' due to inherent instability and risk, or even a ''mountain'' if the breakdown is an irreducible feature of human nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_of_actors, empirical, 'Uncertainty about the degree of rationality in state actors under nuclear threat.').

omega_variable(
    technological_destabilization,
    'Could emerging technologies (e.g., AI in command and control, hypersonic weapons, advanced missile defense) fundamentally destabilize the mutual vulnerability that underpins deterrence?',
    'Ongoing technological development and strategic analysis; wargaming and simulations of future conflict scenarios.',
    'If mutual vulnerability is eroded, the deterrence equilibrium could collapse, leading to a ''snare'' (if one side gains a first-strike advantage) or a ''piton'' (if the system becomes purely theatrical without real deterrent effect).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_destabilization, empirical, 'Uncertainty regarding the long-term stability of deterrence in the face of technological change.').

omega_variable(
    deterrence_vs_taboo_causality,
    'Is the absence of total war primarily due to the material deterrence equilibrium, or to a normative ''nuclear taboo'' that has emerged independently?',
    'Historical counterfactual analysis, sociological studies of norm emergence in international relations, and comparative analysis of state behavior in crises where material deterrence was ambiguous.',
    'If the nuclear taboo is the primary driver, the ''deterrence_equilibrium_reading'' might be overstating the role of material factors, and the constraint might be better classified under the ''nuclear_taboo_reading'' as a ''rope'' or even a ''mountain'' of social construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_taboo_causality, conceptual, 'Ambiguity in the causal mechanism preventing total war: material deterrence vs. normative taboo.').


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
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_proliferation_treaty).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, conventional_arms_control_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
