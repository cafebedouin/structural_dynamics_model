% ============================================================================
% CONSTRAINT STORY: cognitive_efficiency_epistemic_cost
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_efficiency_epistemic_cost, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cognitive_efficiency_epistemic_cost
 *   human_readable: Cognitive Efficiency-Epistemic Cost Trade-off
 *   domain: epistemology/cognitive_science/philosophy_of_mind
 *
 * SUMMARY:
 *   The cognitive efficiency-epistemic cost trade-off is a structural
 *   constraint arising from the computational architecture of bounded
 *   rational agents. Human cognition evolved under severe resource
 *   constraints: limited working memory, finite processing time, metabolic
 *   costs of neural computation. The resulting architecture prioritizes speed
 *   and efficiency through pattern recognition, schema formation, and
 *   heuristic processing. These same mechanisms systematically generate
 *   epistemic errors: false pattern detection, schema-driven false memories,
 *   availability and representativeness heuristics that violate probability
 *   theory. This is not a design flaw but a necessary consequence of
 *   optimization under constraints. Any physical system performing real-time
 *   inference with finite resources must trade accuracy for speed. The
 *   constraint is observable across multiple empirical domains
 *   (speed-accuracy curves in decision tasks, schema-driven memory
 *   distortions, heuristic bias strength) and all observables converge on the
 *   same structural limit. This is a genuine mountain: the constraint emerges
 *   naturally from computational resource bounds, exhibits maximum
 *   accessibility collapse (no agent can access an alternative cognitive
 *   architecture), and shows maximum resistance to modification (training can
 *   shift error rates but cannot eliminate the trade-off).
 *
 * KEY AGENTS:
 *   - Unreflective Cognizer: Any agent operating with default heuristic processing (powerless/trapped) — experiences the constraint as immediate and unchangeable
 *   - Trained Reasoner: Agent with metacognitive awareness and deliberative training (moderate/constrained) — can recognize and sometimes correct heuristic errors but cannot exit the underlying architecture
 *   - Cognitive Science Research Community: Institutional researchers studying the constraint (institutional/arbitrage) — can characterize the trade-off and design mitigations but cannot alter the fundamental limit
 *   - Debiasing Intervention Coalition: Organized efforts to reduce cognitive bias (organized/mobile) — can shift error distributions but cannot eliminate the speed-accuracy trade-off
 *   - Analytical Observer: Civilizational perspective recognizing the constraint as a mathematical property of bounded optimization (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_efficiency_epistemic_cost, 0.08).
domain_priors:suppression_score(cognitive_efficiency_epistemic_cost, 0.02).
domain_priors:theater_ratio(cognitive_efficiency_epistemic_cost, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_efficiency_epistemic_cost, extractiveness, 0.08).
narrative_ontology:constraint_metric(cognitive_efficiency_epistemic_cost, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(cognitive_efficiency_epistemic_cost, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cognitive_efficiency_epistemic_cost, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(cognitive_efficiency_epistemic_cost, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_efficiency_epistemic_cost, mountain).
narrative_ontology:human_readable(cognitive_efficiency_epistemic_cost, "Cognitive Efficiency-Epistemic Cost Trade-off").
narrative_ontology:topic_domain(cognitive_efficiency_epistemic_cost, "epistemology/cognitive_science/philosophy_of_mind").

domain_priors:emerges_naturally(cognitive_efficiency_epistemic_cost).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREFLECTIVE COGNIZER (MOUNTAIN) — Cannot exit the speed-accuracy trade-off. Pattern recognition and heuristic processing are hardwired into neural architecture. The constraint appears as an unchangeable feature of cognition itself.
constraint_indexing:constraint_classification(cognitive_efficiency_epistemic_cost, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: TRAINED REASONER (MOUNTAIN) — Can learn to recognize heuristic errors and apply deliberative correction, but cannot eliminate the underlying architecture. Training reduces error rates but does not remove the fundamental trade-off between speed and accuracy. The constraint remains immutable even with metacognitive awareness.
constraint_indexing:constraint_classification(cognitive_efficiency_epistemic_cost, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: COGNITIVE SCIENCE RESEARCH COMMUNITY (MOUNTAIN) — Studies the constraint across populations and contexts. Can design interventions that mitigate specific errors but cannot alter the fundamental computational architecture. The trade-off is a structural feature of bounded rationality, not a contingent institutional arrangement.
constraint_indexing:constraint_classification(cognitive_efficiency_epistemic_cost, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Recognizes the constraint as a necessary consequence of computational resource limits. Any physical system performing real-time inference under resource constraints must trade accuracy for speed. This is not a contingent feature of human cognition but a mathematical property of bounded optimization. The constraint is invariant across all observables: speed-accuracy curves, schema-driven errors, and heuristic biases all reflect the same underlying computational limit.
constraint_indexing:constraint_classification(cognitive_efficiency_epistemic_cost, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: DEBIASING INTERVENTION COALITION (MOUNTAIN) — Organized efforts to reduce cognitive bias (educational programs, decision support systems, nudge architectures) can shift error rates but cannot eliminate the speed-accuracy trade-off. Even with optimal training and environmental design, the fundamental constraint remains: faster processing yields more errors, slower processing yields fewer errors. The coalition sees the constraint as immutable at the architectural level, though specific error manifestations are malleable.
constraint_indexing:constraint_classification(cognitive_efficiency_epistemic_cost, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_efficiency_epistemic_cost_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cognitive_efficiency_epistemic_cost, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_efficiency_epistemic_cost, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cognitive_efficiency_epistemic_cost, ExtMetricName, E),
    domain_priors:suppression_score(cognitive_efficiency_epistemic_cost, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cognitive_efficiency_epistemic_cost),
    narrative_ontology:constraint_metric(cognitive_efficiency_epistemic_cost, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cognitive_efficiency_epistemic_cost, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cognitive_efficiency_epistemic_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint extracts epistemic cost (systematic errors) but this is not asymmetric extraction benefiting one agent at another's expense. All agents bear the cost equally as a consequence of their shared computational architecture. The small non-zero value reflects that the constraint does impose real costs (false beliefs, suboptimal decisions) but these are not extractive in the sense of benefiting an extractor. Suppression (0.02): Minimal. Agents can learn about the constraint, study it, and develop partial mitigations. No active enforcement prevents understanding or intervention. The constraint persists not through suppression of alternatives but through the absence of alternatives given physical and computational limits. Theater ratio (0.05): Minimal. There is no performative layer obscuring the constraint's function. The trade-off is directly observable in experimental data and subjective experience. Accessibility collapse (0.92): Very high. No agent can access an alternative cognitive architecture that eliminates the speed-accuracy trade-off without ceasing to be a bounded rational agent. Resistance (0.08): Very low. The constraint is not maintained by active resistance to change but by the mathematical structure of bounded optimization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap in classification type — all perspectives classify as mountain. The gap is in the *depth of understanding* of why the constraint is immutable. The unreflective cognizer experiences the constraint as an opaque fact (I make errors). The trained reasoner understands specific error mechanisms (I know I'm vulnerable to availability bias). The research community characterizes the trade-off quantitatively (speed-accuracy curves, bias strength measurements). The analytical observer recognizes the constraint as a necessary consequence of computational resource bounds (any bounded optimizer must trade speed for accuracy). But all perspectives agree on the classification: this is an unchangeable structural limit, not a contingent arrangement that could be otherwise.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a uniform-type mountain constraint with no beneficiaries or victims. All agents experience the same computational limit. There is no extraction flow because the epistemic cost is not transferred between agents but borne by each agent as a consequence of their own architecture. Directionality values are not applicable — the constraint is symmetric across all perspectives. The mountain classification is invariant across power levels, time horizons, exit options, and spatial scopes because the constraint is a mathematical property of bounded rationality, not a contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating a genuine mountain: a structural limit that is not a naturalized extraction mechanism. The key diagnostic is the absence of beneficiaries. Mountains that are actually snares or tangled ropes in disguise always have hidden beneficiaries — agents who benefit from the constraint being perceived as immutable. Here, no agent benefits from the speed-accuracy trade-off. All agents would prefer to have both speed and accuracy if it were computationally possible. The constraint persists not because it serves anyone's interests but because it is a mathematical property of bounded optimization. The analytical observer's mountain classification is not a false summit — it is a correct identification of a natural law. The constraint is invariant across all observables (speed-accuracy tasks, schema errors, heuristic biases) because they all measure the same underlying computational limit. This is what a real mountain looks like: no extraction, no suppression, no theater, maximum accessibility collapse, and invariance across measurement methodologies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_efficiency_epistemic_cost, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_efficiency_epistemic_cost, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a single structural claim with a single stable epsilon value across all observables. Speed-accuracy trade-offs, schema-driven errors, and heuristic biases are different manifestations of the same computational limit, not separate constraints requiring decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
