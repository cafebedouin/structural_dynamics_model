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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rational Dropout Constraint
 *   domain: strategic_studies/international_relations/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This constraint describes the strategic reality imposed by nuclear
 *   weapons, where the costs of large-scale conflict between nuclear-armed
 *   states fundamentally outweigh any conceivable benefits, leading to a
 *   'rational dropout' from such conflicts. It is one reading of the 'nuclear
 *   impossibility kernel,' focusing on the rational-choice aspect rather than
 *   physical impossibility or credibility paradoxes. The constraint is
 *   actively maintained through deterrence postures and arms races,
 *   extracting strategic freedom while coordinating global avoidance of
 *   direct great-power war.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.85).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.92).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational Dropout Constraint").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations/nuclear_deterrence_theory").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, '0b237b85-a08b-4a47-9c9b-c682db98a7ae').
narrative_ontology:cs_kernel_codification('0b237b85-a08b-4a47-9c9b-c682db98a7ae', formalized).
narrative_ontology:cs_authority_grounding('0b237b85-a08b-4a47-9c9b-c682db98a7ae', practice).
narrative_ontology:cs_interpretation_layer_present('0b237b85-a08b-4a47-9c9b-c682db98a7ae').
narrative_ontology:cs_reading_relation('0b237b85-a08b-4a47-9c9b-c682db98a7ae', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('0b237b85-a08b-4a47-9c9b-c682db98a7ae', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('0b237b85-a08b-4a47-9c9b-c682db98a7ae', foundational, cost_benefit_analysis_governs_strategic_choice).
narrative_ontology:cs_axiom_status(cost_benefit_analysis_governs_strategic_choice, holdable).
narrative_ontology:cs_axiom_grounding('0b237b85-a08b-4a47-9c9b-c682db98a7ae', cost_benefit_analysis_governs_strategic_choice, empirically_contingent).
narrative_ontology:cs_axiom('0b237b85-a08b-4a47-9c9b-c682db98a7ae', secondary, mutual_assured_destruction_is_unacceptable).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('0b237b85-a08b-4a47-9c9b-c682db98a7ae', mutual_assured_destruction_is_unacceptable, deontological).
narrative_ontology:cs_reference_frame('0b237b85-a08b-4a47-9c9b-c682db98a7ae', cold_war_deterrence_logic).
narrative_ontology:cs_drift_state('0b237b85-a08b-4a47-9c9b-c682db98a7ae', post_cold_war_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0b237b85-a08b-4a47-9c9b-c682db98a7ae', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, global_population).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, military_planners).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, global_population).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and shape deterrence doctrines. They benefit from avoiding direct great-power war but pay the cost of maintaining expensive arsenals and having their strategic options severely curtailed by the threat of mutual destruction. Their identity as great powers is tied to this capability.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers, payer).

% Benefit from the absence of large-scale great-power conflict, which would have devastating global consequences. However, their strategic autonomy is constrained by the nuclear umbrella or the threat of nuclear escalation, limiting their ability to pursue certain foreign policy objectives.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, payer).

% Tasked with developing and maintaining credible deterrence, they face the paradox of planning for a war that cannot be won. Their professional identity is deeply intertwined with the nuclear enterprise, despite the inherent futility of its ultimate use.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, military_planners, payer,
    organized, immediate, identity_locked, national).

% Benefits from the avoidance of global nuclear war, ensuring continued existence. However, they live under the constant existential threat of nuclear annihilation, a diffuse but profound cost that shapes collective psychology and political discourse.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, global_population, beneficiary,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, global_population, payer).

% Analyze the dynamics of nuclear deterrence, its stability, and its implications for international relations. They are outside the direct operational loop but their analysis influences policy and public understanding.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates nuclear-armed states to avoid direct military conflict by making the costs of victory exceed any conceivable benefit, thereby preventing large-scale conventional or nuclear war.
% TRANSFER_FUNCTION: Transfers the freedom of strategic action and the possibility of traditional military victory from nuclear-armed states (and by extension, all states) to the imperative of existential survival.
% ABSENT_VOICES: Future generations, who bear the existential risk without having consented to the initial creation or ongoing maintenance of nuclear arsenals. Also, non-state actors who might acquire nuclear capabilities and operate outside the rational-choice framework.
% DISAPPEARANCE_RATIONALE: If nuclear weapons vanished overnight, the fundamental calculus of great-power competition would revert to a pre-nuclear state, potentially leading to a resurgence of large-scale conventional conflicts as the ultimate deterrent is removed. Global security architecture would need complete re-evaluation.
% FOUNDING_PROBLEM: Preventing a repeat of the devastating global conventional wars of the early 20th century, and specifically deterring aggression between great powers.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars, historical analysis of the Cold War, ongoing geopolitical tensions, and the continued existence of large conventional militaries all corroborate that the problem of great-power conflict remains live, and nuclear weapons are seen as a primary deterrent by many outside the nuclear powers themselves.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint fundamentally removes the option of traditional military victory for nuclear-armed states, extracting strategic flexibility. Suppression is very high (0.92) as the threat of mutual annihilation effectively suppresses large-scale conventional or nuclear war. Theater ratio is moderate (0.45) reflecting the significant signaling, posturing, and arms race dynamics that are performative aspects of deterrence, alongside genuine maintenance. Accessibility collapse is very high (0.95) as the alternative of winning a great-power war is almost entirely foreclosed. Resistance is low (0.15) because the core premise (unacceptable costs) is widely accepted, though there is resistance to proliferation or specific deterrence doctrines.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers experience this as a necessary evil, a burden that ensures their survival but limits their options. Non-nuclear states often see it as a source of both security (from great-power war) and vulnerability (to nuclear blackmail or accidental escalation). Military planners grapple with the inherent contradiction of preparing for an unwinnable war. The global population experiences it as an existential threat that coordinates their survival.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are both beneficiaries (avoiding direct war) and victims (constrained strategic options, cost of arsenals), leading to a complex directionality. Non-nuclear states are beneficiaries of global stability but victims of curtailed autonomy. The global population is a beneficiary of survival but a victim of existential dread. The constraint coordinates avoidance but extracts heavily from all parties' strategic freedom.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_of_actors,
    'Is the ''rational dropout'' premise robust against non-rational actors, miscalculation, or leadership changes?',
    'Historical analysis of near-misses, psychological studies of decision-making under extreme stress, and observation of state behavior in crises.',
    'If rationality is less robust than assumed, the constraint''s stability is lower, and the risk of escalation is higher, potentially reclassifying it closer to a Snare due to inherent instability and victimhood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_of_actors, empirical, 'Uncertainty about the consistent rationality of actors in nuclear crises.').

omega_variable(
    technological_drift_impact,
    'Do emerging technologies (e.g., AI in command and control, hypersonic weapons, space-based defenses) fundamentally alter the cost-benefit calculus or the possibility of victory?',
    'Ongoing strategic analysis, wargaming, and empirical observation of military doctrine evolution and technological deployment.',
    'If new technologies create perceived pathways to victory or significantly reduce costs, the ''rational dropout'' premise weakens, potentially shifting the constraint towards a more active Snare or even a degraded Piton if deterrence becomes theatrical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_drift_impact, empirical, 'Impact of technological advancements on the nuclear strategic calculus.').

omega_variable(
    distinction_from_physical_impossibility,
    'Is the distinction between ''victory is too costly'' (rational dropout) and ''victory is physically impossible'' (structural contraction) genuinely robust, or does the former collapse into the latter under extreme conditions?',
    'Conceptual analysis of strategic theory, and thought experiments on the limits of ''rationality'' in existential scenarios.',
    'If the distinction collapses, this reading would be subsumed by the ''structural_contraction_reading,'' implying a more Mountain-like constraint where human choice is irrelevant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distinction_from_physical_impossibility, conceptual, 'Conceptual boundary between rational choice and physical impossibility in nuclear war.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.4).
narrative_ontology:measurement(nucl_tr_t1989, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1989, 0.55).
narrative_ontology:measurement(nucl_tr_t2001, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(nucl_tr_t2015, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.85).
narrative_ontology:measurement(nucl_be_t1989, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1989, 0.88).
narrative_ontology:measurement(nucl_be_t2001, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2001, 0.82).
narrative_ontology:measurement(nucl_be_t2015, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2015, 0.86).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.9).
narrative_ontology:measurement(nucl_su_t1989, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1989, 0.95).
narrative_ontology:measurement(nucl_su_t2001, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2001, 0.88).
narrative_ontology:measurement(nucl_su_t2015, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2015, 0.93).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nuclear_impossibility_kernel,' focusing on the rational-choice aspect. It is linked to its sibling readings, 'structural_contraction_reading' and 'credibility_paradox_reading,' which offer alternative interpretations of the nuclear reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
