% ============================================================================
% CONSTRAINT STORY: monty_hall_conditional_probability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_monty_hall_conditional_probability, []).

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
    domain_priors:emerges_naturally/1,
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monty_hall_conditional_probability
 *   human_readable: The Monty Hall Problem (Conditional Probability)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Monty Hall problem is a counter-intuitive probability puzzle that
 *   demonstrates how new information (a host revealing a non-winning option)
 *   alters the probability space. The core constraint is the mathematical
 *   law of conditional probability itself, which dictates that switching doors
 *   doubles the chance of winning (from 1/3 to 2/3). This logical fact is
 *   unchangeable, classifying it as a Mountain. Human intuition often fails
 *   to grasp this, leading to suboptimal choices, but this is a consequence
 *   of interacting with the Mountain, not an extractive property of the
 *   constraint itself.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Contestant: Agent confronting the mathematical reality (powerless/trapped).
 *   - The Educator: Agent using the puzzle to demonstrate the principle (institutional/arbitrage).
 *   - The Intuitive Skeptic: Agent whose "common sense" (50/50 odds) conflicts with the mathematical law.
 *   - The Analytical Observer: Agent who sees the full probability space and its logical consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The mathematical law itself is not extractive. The "cost" is the
% cognitive effort to understand it, which is near zero. The perceived 33%
% loss from not switching is a consequence of choice, not an extraction by
% the constraint. ε must be <= 0.25 for a Mountain.
domain_priors:base_extractiveness(monty_hall_conditional_probability, 0.02).

% Rationale: The law of probability does not suppress alternatives; it simply
% renders them mathematically incorrect. There is no active coercion. The
% suppression score must be <= 0.05 for a Mountain.
domain_priors:suppression_score(monty_hall_conditional_probability, 0.01).

% Rationale: The constraint is a formal mathematical truth with no performative
% or theatrical component.
domain_priors:theater_ratio(monty_hall_conditional_probability, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monty_hall_conditional_probability, extractiveness, 0.02).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility Collapse: The laws of probability are absolute; alternatives are logically incoherent.
narrative_ontology:constraint_metric(monty_hall_conditional_probability, accessibility_collapse, 1.0).
% Resistance: One can argue against the conclusion, but cannot structurally resist the mathematical law itself.
narrative_ontology:constraint_metric(monty_hall_conditional_probability, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(monty_hall_conditional_probability, mountain).
narrative_ontology:human_readable(monty_hall_conditional_probability, "The Monty Hall Problem (Conditional Probability)").

% --- Binary flags ---
% No active enforcement is needed; the constraint is a law of logic.

% --- Emergence flag (required for mountain constraints) ---
% Mathematical laws are discovered, not invented, and emerge from logical axioms.
domain_priors:emerges_naturally(monty_hall_conditional_probability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (a mathematical law), this constraint
% does not have structural beneficiaries or victims. The effects are uniform
% consequences of interacting with an unchangeable logical reality.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE CONTESTANT (MOUNTAIN)
% The agent facing the choice. From this perspective, the odds are a fixed,
% unchangeable feature of the environment. The feeling of being "tricked" is
% a psychological reaction to confronting this logical Mountain.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE EDUCATOR (MOUNTAIN)
% The agent using the problem as a teaching tool. The educator is not creating
% a coordination device (Rope) but is revealing a pre-existing, unchangeable
% logical structure (Mountain) to coordinate understanding.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context. An observer with full knowledge of probability
% theory sees the constraint as a direct and simple consequence of mathematical
% axioms—a pure Mountain.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monty_hall_conditional_probability_tests).

test(type_invariance) :-
    % Verify that the constraint is a Mountain from all key perspectives,
    % demonstrating its status as a uniform-type natural law.
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify that the base metrics are within the allowed range for a Mountain.
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, extractiveness, E),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(monty_hall_conditional_probability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is the law of conditional probability, which has near-zero
 *   base extractiveness (ε=0.02) and suppression (0.01). The perceived 33%
 *   "extraction" from a contestant who doesn't switch is a consequence of
 *   their choice relative to an unchangeable fact, not an act of extraction
 *   by the constraint. The metrics correctly reflect a Mountain. The addition
 *   of the Natural Law (NL) profile metrics (accessibility_collapse=1.0,
 *   resistance=0.01) and the emerges_naturally flag ensures the engine's
 *   natural law certification chain will pass, correctly identifying this
 *   as a mathematical law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a uniform-type constraint that
 *   classifies as a Mountain from all perspectives. The contestant, the
 *   educator, and the analyst all confront the same unyielding logical
 *   structure. Their psychological or utilitarian relationship to it differs,
 *   but its fundamental type does not change.
 *
 * MANDATROPHY ANALYSIS:
 *   By correctly identifying the mathematical law as a Mountain, this analysis
 *   avoids mislabeling a natural law as a Snare or Tangled Rope. The framework
 *   correctly distinguishes between the properties of the constraint itself
 *   (the math) and the outcomes for agents who interact with it (winning or
 *   losing the car). This prevents the "mandatrophy" of seeing social
 *   extraction where there is only logical consequence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_monty_hall,
    'Is the Mountain stable if the host is not an ideal, rule-bound actor (e.g., the host only offers the switch when the contestant has already chosen the car)?',
    'Analysis of the game rules and host intent. If the host has agency and can act strategically, the constraint is no longer a pure mathematical law.',
    'If the host is malicious/strategic, the constraint ceases to be a Mountain and becomes a Snare (a rigged game). If the host is rule-bound, it remains a Mountain.',
    confidence_without_resolution(high) % High confidence in the standard, idealized model.
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(monty_hall_conditional_probability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for this constraint because its base
% extractiveness (0.02) is below the 0.46 threshold for mandatory tracking.
% As a mathematical law, its properties are considered static over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No network relationships are declared for this fundamental mathematical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a Mountain, the constraint does not have
% beneficiaries or victims, so directionality is not a primary factor in its
% classification.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */