% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Extraction Cycle
 *   domain: legal_anthropology/political_systems/history
 *
 * SUMMARY:
 *   This constraint story analyzes blood-feud obligations as a destructive
 *   extraction cycle, depleting societal resources and hindering the
 *   development of centralized political authority. It is one reading of the
 *   'feud_obligation_kernel', focusing on the negative economic and political
 *   consequences rather than its coordination function or moral status. The
 *   high extractiveness and suppression reflect the continuous drain on human
 *   and material capital, enforced by social custom and the threat of
 *   retaliation.
 *
 * KEY AGENTS:
 *   - feud_participants: Primary target (moderate/identity_locked) — bears extraction, trapped by honor.
 *   - peasantry: Secondary target (powerless/trapped) — bears diffuse costs of instability.
 *   - royal_authority: Primary beneficiary/agenda_setter (institutional/arbitrage) — benefits from the delegitimization of private violence.
 *   - ecclesiastical_authorities: Secondary agenda_setter/observer (institutional/constrained) — attempts to pacify, but often ineffective.
 *   - analytical_historians: Analytical observer (analytical/analytical) — provides the framework for this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.85).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.78).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/political_systems/history").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '054c3a05-aaeb-4f14-a7d2-80ae6a96f533').
narrative_ontology:cs_kernel_codification('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', implicit).
narrative_ontology:cs_authority_grounding('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', practice).
narrative_ontology:cs_reading_relation('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', foundational, feud_depletes_productive_capacity).
narrative_ontology:cs_axiom_status(feud_depletes_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', feud_depletes_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', foundational, feud_prevents_state_consolidation).
narrative_ontology:cs_axiom_status(feud_prevents_state_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', feud_prevents_state_consolidation, empirically_contingent).
narrative_ontology:cs_reference_frame('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', pre_state_anarchy).
narrative_ontology:cs_drift_state('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', rise_of_centralized_states, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('054c3a05-aaeb-4f14-a7d2-80ae6a96f533', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and kinship groups bound by honor and custom to avenge wrongs, leading to cycles of violence, resource depletion, and mortality. Exit is perceived as dishonorable and dangerous, leading to social ostracization or further victimization.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_participants, payer,
    moderate, biographical, identity_locked, local).

% Caught in the crossfire of feuds, suffering property damage, loss of life, and disruption to agricultural production. They have no means to escape the territorial violence and are often forced to support one side or another.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Benefits from the delegitimization of private violence, which strengthens its claim to a monopoly on violence. This monopoly, in turn, legitimizes its right to tax and govern, enabling territorial consolidation and state-building. Actively seeks to suppress feuds through law and force.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, royal_authority, agenda_setter).

% Condemn feuds as sinful and work to promote peace through religious injunctions, truces, and arbitration. Their efforts provide an alternative framework for conflict resolution, but often lack the coercive power to enforce it against entrenched custom.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authorities, observer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authorities, agenda_setter).

% Study the historical and sociological dynamics of blood feuds, analyzing their causes, consequences, and role in state formation. They provide the analytical framework for understanding the feud as an extraction cycle.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of centralized justice, feuds provide a mechanism for aggrieved parties to seek redress and deter future offenses, albeit through destructive means. However, this reading emphasizes the breakdown of coordination into a net-negative cycle.
% TRANSFER_FUNCTION: Transfers productive capacity (labor, resources, lives) from feud participants and the peasantry into a cycle of vengeance and destruction. It also transfers legitimacy and power to nascent royal authorities by demonstrating the failure of private justice.
% ABSENT_VOICES: The voices of those who suffer most from the feuds – the non-combatant peasantry, women, and children – are largely absent from the customary and legal discourse surrounding feuds. They would advocate for peace and stability above honor.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight, the social and political landscape would fundamentally rearrange. Productive capacity would be freed, population growth might accelerate, and centralized state authority would face less resistance in consolidating power and establishing a monopoly on justice.
% FOUNDING_PROBLEM: The absence of a reliable, centralized authority to adjudicate disputes and enforce justice, leading individuals and kin groups to resort to self-help mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Analytical historians and royal chronicles attest that while the problem of centralized justice was once live, the persistence of feuds in later periods, despite the rise of state institutions, indicates the problem is largely 'dead' in terms of its original justification. The feud persists as a custom, not a functional solution to a live problem, as corroborated by economic data on resource depletion and demographic records of mortality.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the cycle of vengeance continuously consumes lives, labor, and property without generating equivalent value. Suppression (0.78) is high due to the powerful social and cultural norms that compel participation, making exit (e.g., refusing to avenge a kin's death) extremely difficult and costly (identity_locked). Theater ratio is low (0.1) because the feuds are genuinely destructive, not performative; their function is to inflict harm and maintain honor, not to signal. Accessibility collapse is high (0.7) for individuals and communities trapped in the cycle, as alternatives (like state justice) are either absent or too weak to offer a viable exit. Resistance (0.45) is moderate, primarily from nascent state powers and ecclesiastical authorities, but diffuse and often ineffective at the local level.
 *
 * PERSPECTIVAL GAP:
 *   Feud participants often perceive their actions as upholding honor and justice, a necessary (if costly) form of self-help in a stateless society. This reading, however, frames it as a net-negative extraction cycle from an analytical, macro-historical perspective. Royal authority views the feud as an obstacle to be overcome, while ecclesiastical authorities see it as a moral failing. The engine's classification will highlight this divergence, showing the participants as victims of a snare, while the royal authority benefits from its eventual suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Feud participants are targets (high d) as they bear the direct costs of violence and resource depletion, often compelled by identity-locked social norms. The peasantry are also targets (high d) as they suffer collateral damage with no agency. Royal authority is a beneficiary (low d) because the destructive nature of feuds creates a strong societal demand for a centralized monopoly on violence, which the crown can then provide and legitimize its taxation and governance. Ecclesiastical authorities are observers/agenda-setters, attempting to shift the underlying norms.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the feud as a 'rope' (stateless coordination) by emphasizing its destructive, extractive nature and the identifiable victims. While a 'stateless_coordination_reading' might argue for a coordination function, this 'extraction_cycle_reading' highlights that any coordination is overshadowed by the net-negative transfer and suppression. The persistence of feuds even as state alternatives emerge indicates a mandatrophy of its original 'justice' function, becoming a self-perpetuating snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'How does this ''extraction_cycle_reading'' of the ''feud_obligation_kernel'' structurally differ from its sibling readings?',
    'Comparative analysis of the beneficiary/victim sets and extractiveness metrics across all readings of the ''feud_obligation_kernel''.',
    'This reading places feud participants in the victim set and royal authority in the beneficiary set, leading to a Snare classification. The ''stateless_coordination_reading'' would likely show participants as beneficiaries of a Rope, and the ''christianized_pacification_reading'' would focus on moral transgression rather than economic extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Clarifies the specific structural interpretation of the feud kernel.').

omega_variable(
    coordination_vs_extraction_balance,
    'To what extent did blood feuds provide a genuine, albeit costly, coordination function (e.g., deterrence, justice) in the absence of state power, versus being a purely extractive cycle?',
    'Detailed historical-sociological studies comparing regions with and without feuds, or with different forms of customary justice, to quantify net societal welfare outcomes.',
    'If a significant coordination function is proven, the base extractiveness might be slightly lower, potentially shifting the classification closer to a Tangled Rope, acknowledging a dual function. If purely extractive, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_balance, empirical, 'Assesses the balance between coordination and extraction in feuds.').

omega_variable(
    identity_lock_strength,
    'What is the precise strength of the ''identity_locked'' exit option for feud participants? How easily could individuals or groups opt out without complete social ruin?',
    'Anthropological case studies of historical or contemporary societies with similar customary justice systems, examining the social costs of non-participation.',
    'If the identity lock is weaker than assessed, the suppression metric might be slightly lower, and exit options might shift to ''constrained'', potentially altering the effective extraction calculation for participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Quantifies the social cost of exiting feud obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 1000, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1000, 0.75).
narrative_ontology:measurement(feud_be_t1100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1100, 0.8).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1200, 0.83).
narrative_ontology:measurement(feud_be_t1300, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1300, 0.85).
narrative_ontology:measurement(feud_be_t1400, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1400, 0.84).
narrative_ontology:measurement(feud_be_t1500, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1500, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(feud_su_t1100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1100, 0.7).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1200, 0.75).
narrative_ontology:measurement(feud_su_t1300, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1300, 0.78).
narrative_ontology:measurement(feud_su_t1400, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1400, 0.77).
narrative_ontology:measurement(feud_su_t1500, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1500, 0.78).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1000, tn=1500
narrative_ontology:measurement(feud_grid_01, feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse(class), 1000, 0.75).
narrative_ontology:measurement(feud_grid_02, feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse(class), 1500, 0.8).
narrative_ontology:measurement(feud_grid_03, feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse(individual), 1000, 0.7).
narrative_ontology:measurement(feud_grid_04, feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse(individual), 1500, 0.75).
narrative_ontology:measurement(feud_grid_05, feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse(organizational), 1000, 0.6).
narrative_ontology:measurement(feud_grid_06, feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse(organizational), 1500, 0.65).
narrative_ontology:measurement(feud_grid_07, feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse(structural), 1000, 0.5).
narrative_ontology:measurement(feud_grid_08, feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse(structural), 1500, 0.4).
narrative_ontology:measurement(feud_grid_09, feud_obligation_kernel__extraction_cycle_reading, resistance(class), 1000, 0.2).
narrative_ontology:measurement(feud_grid_10, feud_obligation_kernel__extraction_cycle_reading, resistance(class), 1500, 0.15).
narrative_ontology:measurement(feud_grid_11, feud_obligation_kernel__extraction_cycle_reading, resistance(individual), 1000, 0.4).
narrative_ontology:measurement(feud_grid_12, feud_obligation_kernel__extraction_cycle_reading, resistance(individual), 1500, 0.35).
narrative_ontology:measurement(feud_grid_13, feud_obligation_kernel__extraction_cycle_reading, resistance(organizational), 1000, 0.3).
narrative_ontology:measurement(feud_grid_14, feud_obligation_kernel__extraction_cycle_reading, resistance(organizational), 1500, 0.25).
narrative_ontology:measurement(feud_grid_15, feud_obligation_kernel__extraction_cycle_reading, resistance(structural), 1000, 0.5).
narrative_ontology:measurement(feud_grid_16, feud_obligation_kernel__extraction_cycle_reading, resistance(structural), 1500, 0.6).
narrative_ontology:measurement(feud_grid_17, feud_obligation_kernel__extraction_cycle_reading, stakes_inflation(class), 1000, 0.85).
narrative_ontology:measurement(feud_grid_18, feud_obligation_kernel__extraction_cycle_reading, stakes_inflation(class), 1500, 0.9).
narrative_ontology:measurement(feud_grid_19, feud_obligation_kernel__extraction_cycle_reading, stakes_inflation(individual), 1000, 0.8).
narrative_ontology:measurement(feud_grid_20, feud_obligation_kernel__extraction_cycle_reading, stakes_inflation(individual), 1500, 0.85).
narrative_ontology:measurement(feud_grid_21, feud_obligation_kernel__extraction_cycle_reading, stakes_inflation(organizational), 1000, 0.7).
narrative_ontology:measurement(feud_grid_22, feud_obligation_kernel__extraction_cycle_reading, stakes_inflation(organizational), 1500, 0.75).
narrative_ontology:measurement(feud_grid_23, feud_obligation_kernel__extraction_cycle_reading, stakes_inflation(structural), 1000, 0.6).
narrative_ontology:measurement(feud_grid_24, feud_obligation_kernel__extraction_cycle_reading, stakes_inflation(structural), 1500, 0.55).
narrative_ontology:measurement(feud_grid_25, feud_obligation_kernel__extraction_cycle_reading, suppression(class), 1000, 0.75).
narrative_ontology:measurement(feud_grid_26, feud_obligation_kernel__extraction_cycle_reading, suppression(class), 1500, 0.8).
narrative_ontology:measurement(feud_grid_27, feud_obligation_kernel__extraction_cycle_reading, suppression(individual), 1000, 0.7).
narrative_ontology:measurement(feud_grid_28, feud_obligation_kernel__extraction_cycle_reading, suppression(individual), 1500, 0.75).
narrative_ontology:measurement(feud_grid_29, feud_obligation_kernel__extraction_cycle_reading, suppression(organizational), 1000, 0.6).
narrative_ontology:measurement(feud_grid_30, feud_obligation_kernel__extraction_cycle_reading, suppression(organizational), 1500, 0.68).
narrative_ontology:measurement(feud_grid_31, feud_obligation_kernel__extraction_cycle_reading, suppression(structural), 1000, 0.5).
narrative_ontology:measurement(feud_grid_32, feud_obligation_kernel__extraction_cycle_reading, suppression(structural), 1500, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, territorial_consolidation_constraint).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, state_taxation_legitimacy).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feud_obligation_kernel', each focusing on different structural aspects and yielding different classifications. This reading emphasizes the destructive, extractive cycle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
