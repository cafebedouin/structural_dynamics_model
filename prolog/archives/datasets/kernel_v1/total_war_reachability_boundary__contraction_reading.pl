% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Boundary (Contraction Reading: Physical Impossibility)
 *   domain: international_relations/nuclear_deterrence/strategic_theory
 *
 * SUMMARY:
 *   The contraction reading interprets nuclear weapons as fundamentally
 *   restructuring the strategic space available to rational state actors.
 *   Where pre-nuclear strategic theory treated total war — unlimited
 *   mobilization, unconditional conquest, annihilation of the enemy — as the
 *   ultimate strategic objective and a feasible (if catastrophic) option, the
 *   contraction reading argues that nuclear weapons have made this objective
 *   physically impossible to achieve. The constraint is not that states
 *   choose not to escalate (dropping reading) or that escalation capacity
 *   could be restored by technology (contingent reachability reading), but
 *   that escalation to total war is now structurally unreachable — equivalent
 *   to 2+2=5 or perpetual motion. The measurement trajectory shows
 *   extractiveness collapsing from 1.0 (1945: nuclear weapons are a
 *   war-winning technology) through 0.95 (1962: Cuban Missile Crisis reveals
 *   mutual vulnerability) to 0.05-0.02 (contemporary: total war is understood
 *   as mutually suicidal and therefore not a strategic option). Theater ratio
 *   remains low throughout, indicating that the constraint is primarily a
 *   structural feature, not a performative or ritualistic mechanism. The
 *   contraction reading positions nuclear deterrence as a discovered natural
 *   law of strategic competition, not as an institutional arrangement or
 *   behavioral equilibrium.
 *
 * KEY AGENTS:
 *   - Nuclear-armed states: Institutional actors (all power levels relative to non-nuclear states) who occupy the strategic space; trapped by the contraction that makes total war impossible regardless of military capability
 *   - Non-nuclear states: Constrained by the contraction; cannot aspire to total war even asymmetrically; victim of the reachability boundary that leaves only limited war options
 *   - Humanity (species-level collective): Powerless victim; the reachability boundary is enforced not by any institutional actor but by physics itself; no escape, no negotiation, no exit
 *   - Analytical observer (strategic studies community): Witnesses the contraction; the reading provides a framework for understanding why total war has ceased to be a live strategic option
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.02).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Boundary (Contraction Reading: Physical Impossibility)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/nuclear_deterrence/strategic_theory").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '7cbe8fcd-6954-4174-b515-9a71c95cb831').
narrative_ontology:cs_kernel_codification('7cbe8fcd-6954-4174-b515-9a71c95cb831', fixed_text).
narrative_ontology:cs_authority_grounding('7cbe8fcd-6954-4174-b515-9a71c95cb831', lineage).
narrative_ontology:cs_interpretation_layer_present('7cbe8fcd-6954-4174-b515-9a71c95cb831').
narrative_ontology:cs_reading_relation('7cbe8fcd-6954-4174-b515-9a71c95cb831', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('7cbe8fcd-6954-4174-b515-9a71c95cb831', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('7cbe8fcd-6954-4174-b515-9a71c95cb831', foundational, nuclear_exchange_irreversibility).
narrative_ontology:cs_axiom_status(nuclear_exchange_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('7cbe8fcd-6954-4174-b515-9a71c95cb831', nuclear_exchange_irreversibility, empirically_contingent).
narrative_ontology:cs_axiom('7cbe8fcd-6954-4174-b515-9a71c95cb831', foundational, reachability_boundary_permanence).
narrative_ontology:cs_axiom_status(reachability_boundary_permanence, holdable).
narrative_ontology:cs_axiom_grounding('7cbe8fcd-6954-4174-b515-9a71c95cb831', reachability_boundary_permanence, deontological).
narrative_ontology:cs_reference_frame('7cbe8fcd-6954-4174-b515-9a71c95cb831', strategic_space_post_nuclear_contraction).
narrative_ontology:cs_drift_state('7cbe8fcd-6954-4174-b515-9a71c95cb831', contemporary_deterrence_stability, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7cbe8fcd-6954-4174-b515-9a71c95cb831', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HUMANITY UNDER MUTUAL ASSURED DESTRUCTION (MOUNTAIN) — The reachability boundary is a physical constraint: total war with nuclear weapons is not a policy choice or coordination problem, but a physical impossibility within the contraction reading. No escape from the constraint exists. The entire species is trapped by the structural reality that escalation to mutual annihilation is irreversible. This is not suppression — it is physical law.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contraction_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: NUCLEAR POWERS UNDER CONTRACTION READING (MOUNTAIN) — Even institutional actors with maximum power and the most agency (arbitrage options) cannot escape the reachability boundary. The constraint is not enforceable by any external party — it emerges from physics itself. Nuclear powers cannot 'win' total war, therefore the option does not exist in their feasible strategy set. This is not a coordination mechanism or an extraction; it is an immutable physical limit.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contraction_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (CONTRACTION READING) (MOUNTAIN) — From the civilizational, universal analytical perspective, the contraction reading instantiates a genuine natural law: nuclear escalation dynamics produce a reachability boundary beyond which no state can operate. The contraction is a fundamental structural feature of nuclear strategy, not a contingent institutional arrangement or a technological artifact. The boundary is discovered, not constructed.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(total_war_reachability_boundary__contraction_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(total_war_reachability_boundary__contraction_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.02): Asymptotically near zero. The contraction reading treats total war reachability as a natural law — analogous to the speed of light limit in relativity or the Halting Problem in computation. There is no extraction because no agent benefits from or enforces the constraint. The constraint emerges from the physics of nuclear exchange (irreversible species-level destruction), not from institutional mechanisms or policy choices. The residual 0.02 accounts for measurement uncertainty and the possibility that some actors attempt to reframe the boundary (e.g., nuclear powers claiming they retain 'usable' nuclear options), but the contraction reading interprets such reframings as strategic theater rather than structural reality. SUPPRESSION (0.00): Zero by definition. A mountain constraint has no suppression — it is not enforced by coercion or prevented by barriers. The boundary exists whether states wish it or not. THEATER RATIO (0.05): Very low. The constraint exhibits minimal performative content because the boundary is not maintained by ritual, institutional inertia, or symbolic activity. The small residual reflects institutional discourse around nuclear strategy (deterrence theory, arms control negotiations) that may obscure but does not constitute the underlying constraint. ACCESSIBILITY COLLAPSE (0.92): Very high. Within the contraction frame, there is virtually no accessible alternative to accepting that total war is off the strategic table. States cannot choose otherwise; no technological or institutional pathway restores total war to feasibility without first overcoming the fundamental physics of nuclear exchange. RESISTANCE (0.08): Very low. There is minimal resistance to recognizing the constraint because the constraint is recognized as natural law. The small residual accounts for strategic actors who deny or downplay the contraction (e.g., nuclear powers claiming first-strike viability), but such denial is understood as strategic posturing, not substantive resistance to the boundary itself.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives (powerless humanity, institutional nuclear powers, analytical observer) converge on the MOUNTAIN classification in the contraction reading. This convergence is itself diagnostic: a genuine natural law produces invariant classification across all observational positions. The gap that would normally differentiate perspectives (beneficiary vs victim, arbitrary vs trapped exit) does not apply here because the constraint is not enforced by any agent. The contraction reading predicts no perspectival gap; the dropping reading and contingent_reachability reading predict significant gaps (nuclear powers would see rope/arbitrage, non-nuclear states would see snare/trapped). The absence of a perspectival gap is the contraction reading's empirical signature.
 *
 * DIRECTIONALITY LOGIC:
 *   In the contraction reading, directionality does not apply in the usual sense because there is no extraction flow and no beneficiary-victim structure. The constraint is not maintained by any actor; it emerges from physics. All agents (powerless, institutional, analytical) experience the same structural reality: total war is unreachable. This uniform experience across all (P,T,E,S) contexts is diagnostic of a genuine mountain (natural law). There are no beneficiaries and no victims in the sense of the extraction framework — the constraint is not imposed on anyone, it is discovered by everyone. The contraction reading therefore declares zero beneficiaries and zero victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading resolves the mandatrophy by positioning total war as a MOUNTAIN: a discovered physical limit, not a constructed extraction mechanism or coordination equilibrium. Mandatrophy does not apply to natural laws — there is no tension between coordination function and asymmetric extraction because neither exists. The constraint is purely a limit on the feasible strategy set. The contraction reading therefore explicitly rejects mandatrophy resolution as inapplicable; the constraint is not a hybrid form but a pure limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_dropping_observability,
    'Is the reachability boundary a genuine structural contraction (total war physically impossible), or merely a behavioral dropping (actors choose not to escalate while preserving technical capability)?',
    'Structural analysis: Does the boundary arise from physics of nuclear exchange (irreversible species-level extinction), or from institutional equilibrium incentives (mutual deterrence)? Observationally: Can a state implement technological or strategic change to restore total war to its feasible set?',
    'If contraction: mountain classification holds (natural law, accessibility_collapse ≥ 0.85). If dropping: constraint is rope or tangled_rope (actors choose not to escalate; boundary is maintained by coordination, not physics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contraction_vs_dropping_observability, conceptual, 'Whether reachability boundary is a genuine structural contraction or behavioral dropping').

omega_variable(
    technological_reversibility_of_contraction,
    'Could technological advancement (defenses against nuclear exchange, precision weapons, space-based systems) restore total war to the feasible set, or is the contraction irreversible given the current technological baseline?',
    'Counterfactual analysis: What technological changes would be required to make total war winnable again? Feasibility assessment: Are those changes within the trajectory of plausible development? Expert elicitation from strategic studies, physics, and engineering communities.',
    'If reversible: the contraction reading is context-dependent and could transition to contingent_reachability_reading under different technology regimes. If irreversible: the contraction is a fundamental structural feature, supporting the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_reversibility_of_contraction, empirical, 'Whether technological change could restore total war to feasible set').

omega_variable(
    species_extinction_vs_state_extinction,
    'Does the contraction render total war physically impossible, or merely render it mutually suicidal (state extinction is certain, species extinction is probable but not guaranteed)?',
    'Game-theoretic and nuclear effects analysis: Under escalation dynamics, what is the probability of species-level extinction vs regional or global state collapse? Uncertainty in nuclear winter modeling, population survival, institutional recovery.',
    'If species extinction certain: mountain gate requirement (accessibility_collapse = 1.0, no alternatives exist) is fully satisfied. If state extinction certain but species extinction probable: the constraint is still mountain-like (no state chooses it) but the ''natural law'' framing is softened to probabilistic irreversibility rather than deterministic physical law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(species_extinction_vs_state_extinction, empirical, 'Whether escalation produces species-level extinction or state-level extinction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_hiroshima_1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(theater_cuban_missile_crisis_1962, total_war_reachability_boundary__contraction_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(theater_deterrence_maturation_1980, total_war_reachability_boundary__contraction_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(theater_post_cold_war_2000, total_war_reachability_boundary__contraction_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(theater_contemporary_2020, total_war_reachability_boundary__contraction_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(extractiveness_hiroshima_1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 1.0).
narrative_ontology:measurement(extractiveness_cuban_missile_crisis_1962, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1962, 0.95).
narrative_ontology:measurement(extractiveness_deterrence_maturation_1980, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(extractiveness_post_cold_war_2000, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(extractiveness_contemporary_2020, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2020, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contraction_reading, 0.0).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel is instantiated by three structurally distinct constraint stories with different ε values and different classification profiles. The contraction_reading interprets the boundary as a physical impossibility (ε=0.02, mountain). The dropping_reading interprets it as behavioral choice within a maintained technical capability (ε≈0.40, rope/tangled_rope). The contingent_reachability_reading interprets it as technology-dependent and reversible (ε≈0.30, piton). Each reading is a complete constraint story linked to the others via this network; they are not observational variants of a single constraint but structurally distinct claims about the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
