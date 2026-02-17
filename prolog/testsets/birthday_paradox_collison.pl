% ============================================================================
% CONSTRAINT STORY: birthday_paradox_collision
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_birthday_paradox_collision, []).

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
 *   constraint_id: birthday_paradox_collision
 *   human_readable: The Birthday Paradox (Collision Probability)
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   The Birthday Paradox is a mathematical principle demonstrating that in a
 *   set of n randomly chosen items, the probability of a collision (a shared
 *   property, like a birthday) reaches 50% with only sqrt(N) items, where N is
 *   the total number of possibilities. For 365 days, this is 23 people. This
 *   functions as a fundamental, unchangeable constraint of probability theory,
 *   affecting domains from cryptography (hash collisions) to biology (genetic
 *   uniqueness). This story models the mathematical principle itself, which is
 *   a pure Mountain. Its *application* in other systems (e.g., weak hashing)
 *   would constitute separate, downstream constraints (Snares or Scaffolds).
 *
 * KEY AGENTS (by structural relationship):
 *   - Hash Function Implementers: (powerless/trapped) — The logic is bound by this mathematical law.
 *   - Security Architects: (institutional/mobile) — Must design systems that account for this law.
 *   - Analytical Observer: (analytical/analytical) — Perceives the mathematical structure directly.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birthday_paradox_collision, 0.05).
domain_priors:suppression_score(birthday_paradox_collision, 0.0).
domain_priors:theater_ratio(birthday_paradox_collision, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birthday_paradox_collision, extractiveness, 0.05).
narrative_ontology:constraint_metric(birthday_paradox_collision, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(birthday_paradox_collision, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% accessibility_collapse=1.0: It is logically impossible to access an alternative to this mathematical law.
% resistance=0.0: One cannot meaningfully "resist" a theorem of probability.
narrative_ontology:constraint_metric(birthday_paradox_collision, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(birthday_paradox_collision, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(birthday_paradox_collision, mountain).
narrative_ontology:human_readable(birthday_paradox_collision, "The Birthday Paradox (Collision Probability)").

% --- Emergence flag (required for mountain constraints) ---
% This is a mathematical truth that emerges from the structure of probability
% without human design or enforcement. Required for the mountain metric gate.
domain_priors:emerges_naturally(birthday_paradox_collision).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain (a law of mathematics), this constraint has no structural
% beneficiaries or victims. Agents may benefit or suffer from its *consequences*
% in other systems, but the principle itself is symmetric and non-extractive.
% No enrichment needed.

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

% PERSPECTIVE 1: THE HASH FUNCTION IMPLEMENTER (MOUNTAIN)
% For any system operating within a finite state space, the probability of
% collision is an unchangeable mathematical law.
constraint_indexing:constraint_classification(birthday_paradox_collision, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE SECURITY ARCHITECT (MOUNTAIN)
% An institutional actor cannot change the law, only account for it. They
% "exit" the negative consequences by choosing a larger state space (e.g.,
% moving from SHA-1 to SHA-256), but the underlying mathematical constraint
% remains a Mountain.
constraint_indexing:constraint_classification(birthday_paradox_collision, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context perceives the constraint as a fundamental
% and invariant feature of probability theory.
constraint_indexing:constraint_classification(birthday_paradox_collision, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birthday_paradox_collision_tests).

test(invariance_across_perspectives) :-
    % Verify that the constraint is a Mountain from all key perspectives.
    constraint_indexing:constraint_classification(birthday_paradox_collision, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birthday_paradox_collision, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(birthday_paradox_collision, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == mountain,
    TypeAnalytical == mountain.

test(mountain_threshold_adherence) :-
    % Verify metrics are within the canonical Mountain range.
    narrative_ontology:constraint_metric(birthday_paradox_collision, extractiveness, E),
    narrative_ontology:constraint_metric(birthday_paradox_collision, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(birthday_paradox_collision_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This regeneration corrects a structural error in the original file. The
 *   original file conflated a mathematical principle (a Mountain) with its
 *   application in technology (which can create Snares). Per the ε-invariance
 *   principle, these are distinct constraints. This file now models only the
 *   mathematical law.
 *
 *   - Base Extractiveness (ε=0.05): A mathematical truth does not extract
 *     value; it simply describes reality. The low value reflects its status as
 *     a non-extractive, structural fact. The original value of 0.4 incorrectly
 *     attributed the "extraction" of security from weak systems to the law
 *     itself, rather than to the design of those systems.
 *   - Suppression Score (S=0.0): The paradox does not suppress alternatives;
 *     it is a logical inevitability. The fact that it is counter-intuitive to
 *     humans is a cognitive bias, not structural suppression.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a pure Mountain, the constraint is
 *   classified identically from all perspectives. The original file's attempt
 *   to classify it as a Rope or Snare was a category error. An agent can *use*
 *   a mountain as a tool (Rope) or to set a trap (Snare), but the mountain
 *   itself remains a mountain. The design of a cryptographic system that
 *   *leverages* this principle would be a separate constraint story.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, there are no structural beneficiaries or victims. The
 *   declarations have been removed to reflect this.
 *
 * MANDATROPHY ANALYSIS:
 *   By correctly identifying the mathematical law as a Mountain with ε≈0, this
 *   classification avoids mislabeling a law of nature as an extractive Snare.
 *   It forces a clear distinction between the principle and its application,
 *   preventing the false naturalization of vulnerabilities in man-made systems.
 *   A separate constraint, `weak_hash_vulnerability`, would correctly be
 *   classified as a Snare, with this Mountain as a causal dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_birthday_paradox_collision_1,
    'How does the collision probability shift if the underlying distribution is non-uniform (e.g., seasonal birthdays)?',
    'Compare collision rates in datasets with uniform distributions versus real-world demographic data with known spikes.',
    'If non-uniformity significantly accelerates collisions, then the "safe" input size (e.g., 23 people) is an overestimate, making downstream Snares (like weak hashes) even more dangerous than standard models predict.',
    confidence_without_resolution(high)
).

omega_variable(
    omega_birthday_paradox_collision_2,
    'Does a practical quantum algorithm (e.g., Grover''s) fundamentally change the sqrt(N) nature of the collision problem?',
    'Theoretical analysis and simulation of quantum collision-finding algorithms against classical models.',
    'If a quantum algorithm offers a speedup beyond sqrt(N), it would not change this constraint (which is classical) but would create a new, more severe quantum constraint that renders downstream security systems (Scaffolds) obsolete faster.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(birthday_paradox_collision, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. As a Mountain with base_extractiveness <= 0.46, this
% constraint does not exhibit lifecycle drift. Its properties are static.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% This mathematical principle is a foundational Mountain that affects the
% viability of many cryptographic and data-integrity constraints.
narrative_ontology:affects_constraint(birthday_paradox_collision, weak_hash_vulnerability).
narrative_ontology:affects_constraint(birthday_paradox_collision, uuid_uniqueness_guarantee).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. As a Mountain, there are no beneficiaries or victims,
% so directionality derivation is not used.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */