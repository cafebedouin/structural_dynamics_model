% ============================================================================
% CONSTRAINT STORY: conways_game_of_life_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conways_game_of_life_dynamics, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conways_game_of_life_dynamics
 *   human_readable: Conway's Game of Life Dynamics
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   Conway's Game of Life is a mathematical constraint system so fundamental
 *   that it transcends observer perspective. The four simple local rules — a
 *   cell survives if it has 2-3 neighbors, is born if it has exactly 3
 *   neighbors, and dies otherwise — together with the 2D grid topology,
 *   generate an inexorable logical necessity. Complex patterns emerge
 *   (blinkers, gliders, Gosper guns) not from intention or design, but from
 *   the inevitable unfolding of those rules. No agent — computational,
 *   mathematical, or otherwise — can negotiate with or extract value from
 *   logical necessity. The constraint is the same from every vantage point:
 *   the rules are immutable, the consequences are determined, the boundary
 *   between decidable and undecidable dynamics is fixed. This is the
 *   canonical exemplar of a Mountain constraint: zero degrees of freedom,
 *   universal accessibility collapse (agents cannot evade the rules even with
 *   unlimited resources), minimal resistance to verification (the rules can
 *   be stated in one paragraph), and natural emergence (they follow from the
 *   axioms without additional apparatus).
 *
 * KEY AGENTS:
 *   - Mathematical Observer: Analytical position (analytical/analytical) — sees the logical structure directly; no beneficiary/victim distinction applies
 *   - Computational Agent within Life: Embedded position (powerless/trapped) — inhabits the rule system; cannot escape the constraints
 *   - Computer Science Community: Institutional position (institutional/analytical) — studies Life as a computational model; discovers but cannot change its properties
 *   - Systems Designer: Designer position (powerful/mobile) — can choose initial conditions and rule variants, but cannot modify the fundamental dynamics once chosen
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conways_game_of_life_dynamics, 0.08).
domain_priors:suppression_score(conways_game_of_life_dynamics, 0.02).
domain_priors:theater_ratio(conways_game_of_life_dynamics, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, extractiveness, 0.08).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conways_game_of_life_dynamics, mountain).
narrative_ontology:human_readable(conways_game_of_life_dynamics, "Conway's Game of Life Dynamics").
narrative_ontology:topic_domain(conways_game_of_life_dynamics, "mathematical/computational").

domain_priors:emerges_naturally(conways_game_of_life_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL OBSERVER (MOUNTAIN) — From the mathematical standpoint, Conway's Game of Life dynamics are an irreducible logical consequence of the four local rules and the 2D grid topology. The emergence of stable patterns (blinkers, beacons), oscillators, and computational universality flows necessarily from these axioms. No agent can negotiate with or extract from the logical structure. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. The classification is stable across all mathematical observers regardless of their computational resources or theoretical framework.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL AGENT WITHIN LIFE (MOUNTAIN) — An observer embedded within a Game of Life universe cannot escape or modify the rules; they can only learn to navigate them. The birth/survival/death rules are constraints as immutable as physical laws would be to a being trapped in a physics simulation. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.11. Powerlessness does not change the classification; the constraint is mountain for all positions because it is logically irreducible.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: COMPUTER SCIENCE COMMUNITY (MOUNTAIN) — The computational properties of Life (Turing completeness, undecidability of long-term behavior) are structural facts about the system, not negotiable institutional arrangements. Researchers can study Life, build patterns, but cannot change its fundamental dynamics. The constraint manifests as a permanent boundary on what is computable within the Life universe. d≈0.60, f(d)≈0.75, σ=1.0 → χ≈0.06. Even institutional agents with collective resources face an immutable logical wall.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: SYSTEMS DESIGNER / PRAGMATIC VIEW (MOUNTAIN) — Even a designer with total freedom in the immediate present, choosing local rules and initial conditions, cannot escape the fact that once rules are set, the dynamics are determined. The constraint is the logical chain from axioms to consequences. Mobility and power do not bypass mathematical necessity. d≈0.48, f(d)≈0.60, σ=0.8 → χ≈0.05. The mountain is invariant under observer capability.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conways_game_of_life_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(conways_game_of_life_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conways_game_of_life_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(conways_game_of_life_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(conways_game_of_life_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(conways_game_of_life_dynamics),
    narrative_ontology:constraint_metric(conways_game_of_life_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(conways_game_of_life_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(conways_game_of_life_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Game of Life is pure logical consequence with no distributional asymmetry. No agent gains resource advantage from the rules themselves; all agents face identical logical structure. The small non-zero value (0.08 rather than 0.00) reflects that initial condition choice creates a narrow window of control before the rules take over — but this is design flexibility, not extraction. Suppression (0.02): Negligible. The rules are transparent, fully specified, and verifiable by running the automaton. There are no hidden mechanisms, opaque institutions, or coercive dependencies. Verification of Life dynamics requires only a computer and the rule definition. Theater ratio (0.05): Negligible. The system is purely functional with no performative component. The rules do exactly what they appear to do; there is no gap between stated function and actual operation. Accessibility collapse (0.92): Very high. No agent, regardless of power, resources, or exit options, can negotiate with logical necessity. Even omnipotent designers cannot make 2+2 equal 5 within a consistent mathematical system. Resistance (0.08): Very low. The rules are easily verified through simulation; the logical structure is transparent; there are no barriers to understanding.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap. All four perspectives (analytical, powerless, institutional, powerful) arrive at the same classification: Mountain. This is the hallmark of a truly natural law. The mathematical observer, the embedded computational agent, the institutional research community, and the pragmatic systems designer all face identical irreducibility. The gap would only emerge if one perspective could evade the constraint differently than another — but logical necessity admits no such escape. This uniformity is not a weakness in the analysis; it is confirmation of the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is analytically vacuous for this constraint because there are no beneficiaries or victims. Game of Life dynamics are not extracted from any agent to benefit another. The rule structure is symmetric: every cell is subject to identical rules. No agent occupies a structural position of benefit relative to another. The derivation chain expects beneficiaries and victims; this constraint has neither. The directional tuple (P,T,E,S) varies across perspectives, but d is constant across all positions because the underlying constraint is logically invariant. This is why Mountain constraints need not declare beneficiaries/victims — they have no distributional asymmetry to encode.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    undecidability_boundary,
    'Where exactly does the boundary between decidable and undecidable questions about Life dynamics lie, and does this boundary shift under alternative rule sets?',
    'Formal proof work (Turing reduction chains) establishing decidability status for specific pattern classes and rule variants; empirical exploration of the decision boundary across Rule Space',
    'If most questions are decidable: Life is closer to a controlled mathematical system (Mountain status confirmed). If undecidability is ubiquitous: the practical unpredictability is more fundamental, shifting perspective slightly toward irreducible complexity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(undecidability_boundary, empirical, 'Decidability boundary for pattern classes in Life and rule variants').

omega_variable(
    emergent_complexity_origin,
    'Does the complexity of stable patterns and emergent structures arise purely from the local rules, or does it require a critical mass of initial complexity?',
    'Systematic exploration of minimal initial conditions that generate gliders, blinkers, and other structures; information-theoretic analysis of pattern complexity vs rule complexity',
    'If purely from rules: the constraint is purely structural (Mountain confirmed). If critical mass is needed: the observation site influences the outcome, suggesting a weaker form of emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergent_complexity_origin, empirical, 'Source of emergent complexity in Life dynamics').

omega_variable(
    rule_space_universality,
    'Are Turing-complete rule sets common or rare within the space of all possible cellular automata rules? Does universality depend on specific numerical parameters?',
    'Exhaustive computational census of small rule spaces (Wolfram''s characterization extended); parameter sensitivity analysis for universality',
    'If universality is common: the mountain is robust across rule variations. If rare: Life''s universality is a contingent property of its specific rules, weakening the mountain framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rule_space_universality, empirical, 'Prevalence of Turing completeness in cellular automata rule space').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conways_game_of_life_dynamics, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgol_tr_t0, conways_game_of_life_dynamics, theater_ratio, 0, 0.02).
narrative_ontology:measurement(cgol_tr_t25, conways_game_of_life_dynamics, theater_ratio, 25, 0.04).
narrative_ontology:measurement(cgol_tr_t50, conways_game_of_life_dynamics, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(cgol_be_t0, conways_game_of_life_dynamics, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cgol_be_t25, conways_game_of_life_dynamics, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(cgol_be_t50, conways_game_of_life_dynamics, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conways_game_of_life_dynamics, information_standard).
narrative_ontology:affects_constraint(conways_game_of_life_dynamics, cellular_automaton_universality).
narrative_ontology:affects_constraint(conways_game_of_life_dynamics, computable_function_limits).

% DUAL FORMULATION NOTE:
% Conway's Game of Life is the canonical example of a cellular automaton, upstream of a family of automata-based constraints. Its universality (Turing completeness) affects the classification of other automata rule sets; its undecidability boundaries affect constraints on computation and information processing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
