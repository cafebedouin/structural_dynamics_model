% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Total War Winnability (Structural Contraction Reading)
 *   domain: international_relations_theory/strategic_studies/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'structural contraction' reading of the
 *   post-1945 total war winnability kernel. It posits that the advent of
 *   nuclear weapons fundamentally altered the physical possibility of
 *   achieving strategic objectives through total war, rendering it unwinnable
 *   and thus structurally removed from the realm of rational state action.
 *   This is not a social choice or a normative agreement, but a physical
 *   limit imposed by the destructive power of nuclear arsenals. The
 *   constraint is a Mountain because it is an unchangeable physical reality,
 *   not dependent on human enforcement or belief.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: Agenda-setter (their arsenals create the constraint, but they cannot 'uncreate' it) / Institutional / Arbitrage
 *   - non_nuclear_states: Beneficiary (protected from total war by the constraint) / Organized / Constrained
 *   - populations_in_counterfactual_nuclear_exchange: Victim (hypothetical targets of a total war that is now impossible) / Powerless / Trapped
 *   - strategic_theorists: Observer (analyze the implications of this structural change) / Analytical / Analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.0).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Total War Winnability (Structural Contraction Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations_theory/strategic_studies/commitment_system_analysis").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, 'b212ec00-2674-4fda-b85a-9460d7f1b318').
narrative_ontology:cs_kernel_codification('b212ec00-2674-4fda-b85a-9460d7f1b318', implicit).
narrative_ontology:cs_authority_grounding('b212ec00-2674-4fda-b85a-9460d7f1b318', self_enforcing).
narrative_ontology:cs_reading_relation('b212ec00-2674-4fda-b85a-9460d7f1b318', total_war_winnability_post1945__normative_reading_drop, influences).
narrative_ontology:cs_reading_relation('b212ec00-2674-4fda-b85a-9460d7f1b318', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('b212ec00-2674-4fda-b85a-9460d7f1b318', foundational, nuclear_weapons_alter_physical_reality_of_war).
narrative_ontology:cs_axiom_status(nuclear_weapons_alter_physical_reality_of_war, holdable).
narrative_ontology:cs_axiom_grounding('b212ec00-2674-4fda-b85a-9460d7f1b318', nuclear_weapons_alter_physical_reality_of_war, empirically_contingent).
narrative_ontology:cs_axiom('b212ec00-2674-4fda-b85a-9460d7f1b318', foundational, total_victory_is_physically_impossible_in_nuclear_exchange).
narrative_ontology:cs_axiom_status(total_victory_is_physically_impossible_in_nuclear_exchange, holdable).
narrative_ontology:cs_axiom_grounding('b212ec00-2674-4fda-b85a-9460d7f1b318', total_victory_is_physically_impossible_in_nuclear_exchange, empirically_contingent).
narrative_ontology:cs_reference_frame('b212ec00-2674-4fda-b85a-9460d7f1b318', pre_nuclear_strategic_paradigm).
narrative_ontology:cs_drift_state('b212ec00-2674-4fda-b85a-9460d7f1b318', post_nuclear_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b212ec00-2674-4fda-b85a-9460d7f1b318', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_winnability_post1945__structural_contraction_reading, populations_in_counterfactual_nuclear_exchange).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the arsenals that create the physical conditions for this constraint. While they cannot 'uncreate' the constraint, their strategic doctrines and force postures influence its stability. They are simultaneously constrained by the impossibility of total victory.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_armed_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the structural impossibility of total war, as it reduces the existential threat to their populations and sovereignty. They do not directly contribute to maintaining the constraint but are shaped by its existence.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states, beneficiary,
    organized, generational, constrained, global).

% Represent the hypothetical victims of a total war that, due to this constraint, is no longer a winnable option. Their 'cost' is in the potential for annihilation that the constraint prevents.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, populations_in_counterfactual_nuclear_exchange, payer,
    powerless, immediate, trapped, universal).

% Analyze and interpret the implications of nuclear weapons for international relations, including the structural impossibility of total war. Their work shapes understanding but does not directly alter the physical constraint.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, by making total war unwinnable, implicitly coordinates state behavior away from existential conflict, forcing alternative means of competition and dispute resolution.
% TRANSFER_FUNCTION: It transfers the 'cost' of total war (annihilation) from actual populations to a hypothetical, counterfactual space, effectively 'paying' for global security with the threat of mutual destruction.
% ABSENT_VOICES: Any historical or future actors who might believe in the possibility or desirability of total victory in a nuclear age are structurally absent from the decision-making space, as the physical reality forecloses their strategic options.
% DISAPPEARANCE_RATIONALE: If the structural impossibility of total war vanished (e.g., through a technological breakthrough rendering nuclear weapons obsolete or ineffective), global strategic calculations would fundamentally rearrange. States would immediately re-evaluate the utility of large-scale conventional conflict, potentially leading to a return to pre-nuclear patterns of warfare.
% FOUNDING_PROBLEM: The problem of preventing existential, civilization-ending conflict in an era of unprecedented destructive power.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, as attested by ongoing nuclear deterrence theory, arms control efforts, and the continued existence of nuclear arsenals. International security experts and policymakers universally corroborate the continued salience of preventing nuclear war, even if they disagree on the precise mechanism of its prevention.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness, suppression, and theater_ratio are all near zero because this is a physical constraint, not a human-constructed one. It doesn't extract rents, suppress alternatives (it removes the alternative of total war itself), or require theatrical maintenance. Accessibility collapse is high (0.95) because the option of 'winning' a total war has been almost entirely removed from the decision space. Resistance is low (0.05) because, while some actors might theoretically desire total victory, the physical reality makes active resistance against this constraint futile.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear-armed states, the constraint is a self-imposed limitation that paradoxically enhances their security by preventing existential conflict. From non-nuclear states, it's a beneficial external reality. The victims are hypothetical populations who would suffer in a total war, but their 'victimhood' is in the counterfactual absence of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states are effectively agenda-setters in that their actions (developing nuclear weapons) created the condition, but they are also subject to its physical limits (d=0.5). Non-nuclear states are beneficiaries (d=0.1) as they are protected from total war without bearing direct costs. Hypothetical populations are victims (d=0.9) of the counterfactual total war, but the constraint itself prevents this outcome. Strategic theorists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a Mountain, is not subject to mandatrophy in the conventional sense, as its function is a physical reality rather than a human mandate. The question of 'mandatrophy' here would be whether the physical reality itself has changed, which is not the case. The classification prevents mislabeling a physical impossibility as a social choice or a policy outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is the impossibility of total war a structural consequence of nuclear weapons (this reading), a normative shift, or a change in strategic culture?',
    'Empirical analysis of state behavior in crises, counterfactual historical analysis, and examination of military doctrine for evidence of ''winnable'' total war planning post-1945.',
    'If this structural contraction reading is correct, the constraint is a Mountain. If the normative or strategic culture readings are correct, the constraint is a Rope or Tangled Rope, with different mechanisms of persistence and potential for erosion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishes the structural impossibility of total war from normative or ideational shifts.').

omega_variable(
    beneficiary_ambiguity_mountain,
    'Does the structural impossibility of total war benefit identifiable agents, or is it a universal condition?',
    'Analysis of which state actors or populations gain a relative security advantage from the absence of total war, even if it is a universal condition.',
    'If identifiable beneficiaries exist, this Mountain is a False Summit, reclassifying to a Tangled Rope to reflect the potential for extraction from the ''peace dividend'' or security dilemma.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_ambiguity_mountain, empirical, 'Examines whether the ''natural'' constraint of total war impossibility has identifiable beneficiaries, potentially indicating a False Summit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(tota_tr_t15, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 15, 0.0).
narrative_ontology:measurement(tota_tr_t30, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 30, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(tota_be_t15, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 15, 0.0).
narrative_ontology:measurement(tota_be_t30, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 30, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(tota_su_t15, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 15, 0.0).
narrative_ontology:measurement(tota_su_t30, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 30, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This 'structural contraction' reading posits a physical impossibility, while sibling readings focus on normative or ideational shifts. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
