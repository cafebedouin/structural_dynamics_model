% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Removed from Strategic Possibility Space (Space Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the space-contraction reading of the
 *   contested kernel 'total_war_possibility_space'. The reading asserts that
 *   nuclear weapons removed total war from the strategically thinkable — not
 *   by making it prohibitively costly (deterrence reading) or normatively
 *   taboo (taboo reading), but by making it logically impossible to plan for
 *   or execute. The material capability to annihilate human civilization
 *   structures the possibility space itself: total war cannot be thought as a
 *   rational strategic option because it has become a non-option in the logic
 *   of mutual vulnerability. The constraint is claimed as a genuine natural
 *   law (mountain) — an emergent property of the physics of nuclear weapons
 *   and the logic of mutually assured destruction. The measurement series
 *   track rising suppression_requirement over time, indicating that
 *   maintaining the constraint requires increasing institutional work to
 *   suppress (or re-suppress) the idea that total war might be thinkable.
 *
 * KEY AGENTS:
 *   - nuclear_armed_great_powers: institutional agenda-setters, trapped by their own weaponry
 *   - populations_in_nuclear_armed_states: powerless beneficiaries, relieved of total-war vulnerability
 *   - military_planning_establishments: institutional observers and beneficiaries, confined to sub-strategic planning
 *   - non_nuclear_armed_states: moderate-power beneficiaries, insulated from great-power annihilation scenarios
 *   - deterrence_equilibrium_advocates: excluded analytical seat, holds competing reading of same kernel
 *   - nuclear_taboo_advocates: excluded analytical seat, holds competing reading of same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.15).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.08).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Removed from Strategic Possibility Space (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '470e186d-de8e-442c-ac65-b2279ee523e0').
narrative_ontology:cs_kernel_codification('470e186d-de8e-442c-ac65-b2279ee523e0', implicit).
narrative_ontology:cs_authority_grounding('470e186d-de8e-442c-ac65-b2279ee523e0', expertise).
narrative_ontology:cs_interpretation_layer_present('470e186d-de8e-442c-ac65-b2279ee523e0').
narrative_ontology:cs_reading_relation('470e186d-de8e-442c-ac65-b2279ee523e0', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('470e186d-de8e-442c-ac65-b2279ee523e0', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('470e186d-de8e-442c-ac65-b2279ee523e0', foundational, mutual_vulnerability_eliminates_strategic_rationality_for_total_war).
narrative_ontology:cs_axiom_status(mutual_vulnerability_eliminates_strategic_rationality_for_total_war, holdable).
narrative_ontology:cs_axiom_grounding('470e186d-de8e-442c-ac65-b2279ee523e0', mutual_vulnerability_eliminates_strategic_rationality_for_total_war, empirically_contingent).
narrative_ontology:cs_axiom('470e186d-de8e-442c-ac65-b2279ee523e0', foundational, capability_structures_cognition_not_norm_or_cost).
narrative_ontology:cs_axiom_status(capability_structures_cognition_not_norm_or_cost, holdable).
narrative_ontology:cs_axiom_grounding('470e186d-de8e-442c-ac65-b2279ee523e0', capability_structures_cognition_not_norm_or_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('470e186d-de8e-442c-ac65-b2279ee523e0', material_impossibility_of_total_war).
narrative_ontology:cs_drift_state('470e186d-de8e-442c-ac65-b2279ee523e0', contemporary_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('470e186d-de8e-442c-ac65-b2279ee523e0', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, populations_in_nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, military_planning_establishments).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because no party actively collects from the constraint — it is a structural fact, not a mechanism of transfer. Suppression is very low (0.08) at baseline because the constraint is self-enforcing through material logic: no one needs to suppress the idea that total war is thinkable, because the material structure makes it unthinkable. However, the measurement series shows suppression_requirement rising over 60 years (0.02 → 0.09), indicating that institutional effort to suppress the competing readings (deterrence and taboo) has been necessary to maintain consensus that the space-contraction reading is the operative one. Theater_ratio is low (0.12) because the strategic planning system is genuinely responsive to the constraint — military doctrine, weapons procurement, escalation scenarios are not performative; they are operationally constrained by the assumption that total war is off the table. The accessibility_collapse is very high (0.92) because once the logic of nuclear weapons is understood, no rational actor can plan for total war — alternatives simply do not exist within strategic rationality. Resistance is negligible (0.05) because no major military establishment argues for total-war planning; that option has been removed from serious strategic discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of nuclear-armed great powers, the constraint is self-evidently a structural fact — total war is not reachable because the mathematics of mutual vulnerability make it a non-option. From the seat of non-nuclear-armed states, the constraint appears as a gift: they benefit from the fact that their more powerful neighbors cannot mobilize total annihilation. From the analytical seat of deterrence advocates, the constraint is mischaracterized: total war remains reachable but is deterred by costs and rationality. The space-contraction reading computes as a mountain from every institutional seat; it computes as a mountain (not a snare, not a rope) because the material structure itself enforces the boundary. The perceptual divergence is NOT about whether the constraint is real — it is about whether total war is structurally impossible or merely very costly and rationally avoided. The engine should compute mountain-type classification from the measurement profile (low extractiveness, very high accessibility_collapse, very low resistance, emerges_naturally: true) regardless of the competing readings.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are needed. The populations and military establishments are beneficiaries (they benefit from the removal of total war from possibility space) but do not directly collect extraction. Great powers are trapped by the constraint but also benefit (they avoid the alternative: mutual annihilation). No party extracts value by maintaining the constraint — it maintains itself through physics. The beneficiary/victim structure is degenerate because this is a genuine natural law constraint, not a constructed mechanism of transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits signs of mandatrophy in the institutional layer: the founding problem (how to manage great-power conflict without total mobilization) is 'live' in abstract terms, but the problem is no longer solved by intentional action — it is solved by material fact. Military establishments maintain strategic doctrine that accommodates the constraint (no-first-use, escalation dominance in conventional ranges), but these are not solutions to the founding problem; they are adaptations to a constraint that has solved the problem for them. The rising suppression_requirement suggests that maintaining consensus about the space-contraction reading requires increasing institutional effort to suppress the competing readings. If the suppression_requirement continues rising, it could indicate that the constraint is becoming a piton — maintained by institutional inertia and professional consensus rather than by the material logic that created it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    space_contraction_vs_deterrence_mechanism,
    'Is total war removed from strategic possibility because nuclear weapons make it materially/logically impossible, or because nuclear weapons make it so costly that rationality deters it?',
    'Thought experiments and game-theoretic analysis: if rational actors in a situation of mutual vulnerability could somehow ''restart'' at a pre-decision point with full information about the costs, would they still choose total war escalation? Space contraction says no (it is logically impossible); deterrence says it depends on the cost structure and risk tolerance of the actor.',
    'If the mechanism is deterrence (not space contraction), the constraint is unstable: a sufficiently desperate or irrational actor could decide the costs are acceptable and choose total war. If the mechanism is space contraction, total war is non-negotiable — no cost structure makes it thinkable. The two readings produce different implications for stability and the durability of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(space_contraction_vs_deterrence_mechanism, conceptual, 'Whether the constraint operates through material logic (impossibility) or cost-based rationality (deterrence).').

omega_variable(
    institutional_suppression_ratchet,
    'Is the rising suppression_requirement over the interval an indicator that the constraint is becoming a piton (maintained by institutional consensus rather than material logic), or is it a sign of institutional deepening (greater sophistication in understanding and managing the constraint)?',
    'Track the composition of suppression_requirement: is it applied against rival readings (deterrence and taboo advocates are marginalized in strategy departments)? Against alternative weapons programs that might bypass the constraint? Against war-gaming exercises that violate the boundary? Rising suppression directed at rival readings would indicate piton dynamics; rising suppression directed at weapons or doctrines would indicate institutional deepening.',
    'If piton dynamics are present, the constraint may be vulnerable to institutional change: a shift in strategic culture or a new generation of leaders could restore total-war planning. If institutional deepening is occurring, the constraint is consolidating and becoming more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_ratchet, empirical, 'Whether suppression_requirement increase indicates institutional piton dynamics or constraint consolidation.').

omega_variable(
    false_summit_natural_law_status,
    'Is the claim that total war is structurally removed from possibility space a genuine natural law (emergent from physics and logic), or a constructed constraint that benefits the great powers and the strategic studies discipline?',
    'Check whether beneficiary presence (populations, military establishments, deterrence system) indicates captured natural law. If the constraint benefits identifiable actors who have institutional stake in its maintenance, the ''natural law'' claim becomes suspect. Compare with genuine natural laws (entropy, gravitational limits) that benefit no one specifically.',
    'If the constraint is a false summit (constructed but presented as natural), it becomes vulnerable to deconstruction: actors could challenge the claim that total war is impossible and propose alternative strategic pathways. If it is a genuine natural law, deconstruction fails because the boundary is materially real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Whether the space-contraction claim is a genuine natural law or a false summit masking constructed power.').

omega_variable(
    kernel_reading_non_equivalence,
    'Can the space-contraction, deterrence-equilibrium, and nuclear-taboo readings coexist in a single strategic doctrine, or are they mutually exclusive commitments?',
    'Examine actual military doctrine from nuclear-armed states: does doctrine treat total war as impossible (space contraction), very costly but possible (deterrence), or prohibited by norm (taboo)? Can elements of all three appear in the same doctrine without contradiction?',
    'If readings are mutually exclusive, one must eventually dominate — doctrine will commit to one reading. If readings can coexist (as different layers of constraint), the kernel admits multiple simultaneous readings. The stability of the nuclear constraint may depend on this pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_non_equivalence, empirical, 'Whether the kernel''s multiple readings are compatible or strictly exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__space_contraction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tota_tr_t15, total_war_possibility_space__space_contraction_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(tota_tr_t30, total_war_possibility_space__space_contraction_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(tota_tr_t45, total_war_possibility_space__space_contraction_reading, theater_ratio, 45, 0.12).
narrative_ontology:measurement(tota_tr_t60, total_war_possibility_space__space_contraction_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(tota_tr_t75, total_war_possibility_space__space_contraction_reading, theater_ratio, 75, 0.12).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__space_contraction_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(tota_be_t15, total_war_possibility_space__space_contraction_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(tota_be_t30, total_war_possibility_space__space_contraction_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(tota_be_t45, total_war_possibility_space__space_contraction_reading, base_extractiveness, 45, 0.15).
narrative_ontology:measurement(tota_be_t60, total_war_possibility_space__space_contraction_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(tota_be_t75, total_war_possibility_space__space_contraction_reading, base_extractiveness, 75, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__space_contraction_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(tota_su_t15, total_war_possibility_space__space_contraction_reading, suppression_requirement, 15, 0.03).
narrative_ontology:measurement(tota_su_t30, total_war_possibility_space__space_contraction_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(tota_su_t45, total_war_possibility_space__space_contraction_reading, suppression_requirement, 45, 0.07).
narrative_ontology:measurement(tota_su_t60, total_war_possibility_space__space_contraction_reading, suppression_requirement, 60, 0.09).
narrative_ontology:measurement(tota_su_t75, total_war_possibility_space__space_contraction_reading, suppression_requirement, 75, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.08).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_strategy_doctrine_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, great_power_war_prevention_institution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The sibling readings (deterrence_equilibrium_reading and nuclear_taboo_reading) are SEPARATE constraint stories with DIFFERENT epsilon values and DIFFERENT structural data. The three stories share the same kernel commitment but instantiate it differently. This story (space_contraction_reading) claims total war is materially impossible; the deterrence reading claims it is materially possible but rationally deterred; the taboo reading claims it is materially possible but normatively prohibited. Decomposition is necessary (ε-invariance principle) because the empirical status, mechanism, and vulnerability conditions differ across readings. Link via network.affects_constraints to indicate kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
