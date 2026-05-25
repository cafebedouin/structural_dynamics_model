% ============================================================================
% CONSTRAINT STORY: nash_equilibrium_existence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nash_equilibrium_existence, []).

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
 *   constraint_id: nash_equilibrium_existence
 *   human_readable: Nash Equilibrium Existence (Brouwer-Fixed Point Foundation)
 *   domain: mathematics/game_theory/topology
 *
 * SUMMARY:
 *   Nash equilibrium existence is a theorem in game theory asserting that
 *   every finite, normal-form game with mixed-strategy players admits at
 *   least one Nash equilibrium — a configuration of strategies where no
 *   player has unilateral incentive to deviate. The theorem rests on
 *   Brouwer's fixed-point theorem: the best-response correspondence from the
 *   simplex to itself is continuous and compact, guaranteeing a fixed point.
 *   This constraint is classically invariant — it holds identically across
 *   all observables, all agent populations, and all game structures. The
 *   mathematical structure is immutable. However, the theater_ratio (0.15)
 *   captures a modest and growing gap between the formal guarantee and
 *   empirical salience: as game complexity increases (information
 *   asymmetries, dynamic structures, equilibrium multiplicity), the existence
 *   guarantee becomes less informative about which equilibrium will be
 *   selected or what agents will predict. The guarantee is logically
 *   unchanged; its empirical content has diminished slightly over the past
 *   century of game-theoretic research.
 *
 * KEY AGENTS:
 *   - Any Player in Strategic Interaction: Subject to the constraint (powerless/trapped) — cannot avoid the existence guarantee; forced to inhabit an equilibrium
 *   - Game Designer / Mechanism Designer: Institutional actor (organized/constrained) — cannot eliminate equilibria; can only select or influence which equilibrium is selected
 *   - Analyst / Theorist: Observer (analytical/analytical) — recognizes the constraint as a topological necessity flowing from Brouwer's theorem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nash_equilibrium_existence, 0.08).
domain_priors:suppression_score(nash_equilibrium_existence, 0.02).
domain_priors:theater_ratio(nash_equilibrium_existence, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nash_equilibrium_existence, extractiveness, 0.08).
narrative_ontology:constraint_metric(nash_equilibrium_existence, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(nash_equilibrium_existence, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nash_equilibrium_existence, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nash_equilibrium_existence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nash_equilibrium_existence, mountain).
narrative_ontology:human_readable(nash_equilibrium_existence, "Nash Equilibrium Existence (Brouwer-Fixed Point Foundation)").
narrative_ontology:topic_domain(nash_equilibrium_existence, "mathematics/game_theory/topology").

domain_priors:emerges_naturally(nash_equilibrium_existence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGENT IN STRATEGIC INTERACTION (MOUNTAIN) — Any player in a finite, normal-form game faces the invariant constraint: a Nash equilibrium exists, guaranteed by the topological structure. This is not avoidable through strategy, coalition, or exit. The constraint is inescapable — a mathematical fact independent of the player's preferences, power, or willingness to exit.
constraint_indexing:constraint_classification(nash_equilibrium_existence, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: GAME DESIGNER / POLICY MAKER (MOUNTAIN) — Even institutional actors designing games or mechanisms cannot escape the existence guarantee. They cannot design a game where no Nash equilibrium exists. The constraint is inescapable even from a position of institutional power — one can choose which equilibrium emerges (through equilibrium selection), but one cannot eliminate equilibria. The existence guarantee is a fixed law governing all game structures.
constraint_indexing:constraint_classification(nash_equilibrium_existence, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of formal logic and topology, Nash equilibrium existence follows from Brouwer's fixed-point theorem applied to the best-response correspondence. The existence is a pure mathematical consequence of finiteness and convexity. No empirical data, no institutional arrangement, no strategic behavior can change this. The constraint is logically and mathematically immutable across all possible games and all possible agent behaviors.
constraint_indexing:constraint_classification(nash_equilibrium_existence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nash_equilibrium_existence_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(nash_equilibrium_existence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nash_equilibrium_existence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nash_equilibrium_existence, ExtMetricName, E),
    domain_priors:suppression_score(nash_equilibrium_existence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nash_equilibrium_existence),
    narrative_ontology:constraint_metric(nash_equilibrium_existence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nash_equilibrium_existence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nash_equilibrium_existence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The Nash equilibrium existence guarantee does not extract value from any agent; it is a pure mathematical fact. The small positive value reflects the theater ratio — the guarantee is not itself a constraint but a meta-statement about the space of constraints (games). Base extractiveness remains near zero because no agent benefits while others pay cost. Suppression (0.02): Minimal. The constraint does not suppress alternatives; it asserts that alternatives do not exist mathematically. There are no barriers to exit because the constraint is not an institutional arrangement but a topological theorem. Theater ratio (0.15): Low and slowly increasing. Historically, Nash equilibrium existence was presented as a foundational guarantee — the proof of existence was the primary contribution (Nash, 1950). In modern game theory, the existence theorem is pedagogically and formally central but empirically less salient because: (1) games are often infinite or have multiple equilibria, reducing the informativeness of mere existence; (2) equilibrium selection is the harder problem; (3) dynamic and information-asymmetric games have different solution concepts. The theater has increased (from ~0.05 in 1950 to ~0.15 in 2026) as the field has matured and recognized that existence is necessary but not sufficient. The ratio remains low because the mathematical content is genuine and non-performative.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All three perspectives produce mountain classifications because the constraint is mathematically universal. The existence guarantee holds identically for all agents, all game structures, and all strategic contexts within the finite, normal-form domain. The analytical observer, the beneficiary, and the victim all experience the same immutable topological fact. The universality of the classification is diagnostic of a true natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not meaningfully computed for mountain constraints. There is no extraction flow because there are no beneficiaries or victims. The existence guarantee simply is — it does not advantage some agents over others, it does not create asymmetric costs, and it is not a constraint that could be removed to benefit one party. The d value would be undefined in the directionality framework because the constraint has zero asymmetry.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infinite_game_extension,
    'Does Nash equilibrium existence extend to infinite games, and if so, under what closure conditions?',
    'Literature review of infinite game theory (compact metric spaces, weak convergence, measure-theoretic foundations); proof techniques and failure cases',
    'If existence holds universally: constraint remains mountain. If existence fails for some infinite game structures: constraint is mountain only for finite games, snare for infinite games with special structure. The constraint''s universality scope shrinks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infinite_game_extension, conceptual, 'Scope of Nash equilibrium existence to infinite games').

omega_variable(
    approximate_vs_exact_equilibrium,
    'Is the constraint modeling exact Nash equilibria or epsilon-Nash approximations, and does the distinction affect the classification?',
    'Formal definition of equilibrium concept in use; comparison of existence proofs for exact vs approximate; empirical observability gap',
    'If exact: mountain classification is precise. If approximate: the ''existence'' is a statement about approximate structures, which may have different topological properties. Approximate equilibria may exhibit different universality profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(approximate_vs_exact_equilibrium, conceptual, 'Exact versus approximate Nash equilibrium semantics').

omega_variable(
    mixed_strategy_interpretation,
    'Is the existence guarantee meaningful for the empirical interpretation of mixed-strategy equilibria, or is it a mathematical artifact of the probability simplex?',
    'Philosophical analysis of mixed-strategy interpretation; experimental evidence on whether agents empirically play mixed strategies vs pure-strategy distributions; observability in real games',
    'If mixed strategies are empirically meaningful: existence guarantee has predictive content. If mixed strategies are mathematical convenience: existence is a topological theorem with limited empirical application. Epistemological status of the constraint shifts from empirical law to formal convenience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mixed_strategy_interpretation, preference, 'Empirical meaningfulness of mixed-strategy Nash equilibria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nash_equilibrium_existence, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nash_tr_t0, nash_equilibrium_existence, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nash_tr_t50, nash_equilibrium_existence, theater_ratio, 50, 0.12).
narrative_ontology:measurement(nash_tr_t100, nash_equilibrium_existence, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(nash_be_t0, nash_equilibrium_existence, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(nash_be_t50, nash_equilibrium_existence, base_extractiveness, 50, 0.07).
narrative_ontology:measurement(nash_be_t100, nash_equilibrium_existence, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nash_equilibrium_existence, information_standard).
narrative_ontology:affects_constraint(nash_equilibrium_existence, equilibrium_selection_problem).
narrative_ontology:affects_constraint(nash_equilibrium_existence, mixed_strategy_empirical_validity).
narrative_ontology:affects_constraint(nash_equilibrium_existence, game_theoretic_solution_concept_foundedness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
