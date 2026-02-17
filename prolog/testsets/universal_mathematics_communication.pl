% ============================================================================
% CONSTRAINT STORY: universal_mathematics_communication
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_universal_mathematics_communication, []).

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: universal_mathematics_communication
 *   human_readable: Mathematics as a Universal Communication Constraint
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   This constraint posits that mathematics is not a human invention but a
 *   fundamental "language of the universe". It acts as a bridge for
 *   communication between species with "alien" minds—such as humans and bees,
 *   or humans and extraterrestrials—by providing a shared framework of logic
 *   and quantity. The core structure is a natural law (Mountain), but its
 *   application as a communication tool is a form of coordination (Rope).
 *
 * KEY AGENTS (by structural relationship):
 *   - Honeybees: Primary target (powerless/trapped) — experiences mathematics as an immutable law of nature they must navigate to get rewards.
 *   - Human Scientists (SETI/NASA): Primary beneficiary (institutional/mobile) — uses mathematics as a coordination tool (Rope) to attempt interstellar communication.
 *   - Extraterrestrials: Hypothetical beneficiary (organized/mobile) — would rely on this shared framework for first contact.
 *   - Analytical Observer: Analytical observer — sees the full dual structure of a natural law being used as a coordination device.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Mathematics is non-extractive; it is a shared discovery that
% enables cooperation rather than exploitation. ε is low, consistent with Mountain.
domain_priors:base_extractiveness(universal_mathematics_communication, 0.10).

% Rationale: The core logical structure of mathematics is non-suppressible.
% While different "dialects" or notations exist, the underlying truths are fixed.
% Score must be <= 0.05 for Mountain classification.
domain_priors:suppression_score(universal_mathematics_communication, 0.05).

% Rationale: This is a purely functional, technical constraint with minimal
% performative theater.
domain_priors:theater_ratio(universal_mathematics_communication, 0.04).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(universal_mathematics_communication, extractiveness, 0.10).
narrative_ontology:constraint_metric(universal_mathematics_communication, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(universal_mathematics_communication, theater_ratio, 0.04).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Accessibility Collapse: The laws of mathematics are inescapable; no coherent
% alternative is structurally accessible.
narrative_ontology:constraint_metric(universal_mathematics_communication, accessibility_collapse, 0.98).
% Resistance: Meaningful resistance to mathematical truth is incoherent.
narrative_ontology:constraint_metric(universal_mathematics_communication, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(universal_mathematics_communication, mountain).
narrative_ontology:human_readable(universal_mathematics_communication, "Mathematics as a Universal Communication Constraint").

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally as a consequence of intelligence and physical reality.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(universal_mathematics_communication).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% While this is a Mountain, the beneficiary declaration is needed to derive
% the has_coordination_function/1 predicate, which allows the institutional
% perspective to correctly classify it as a Rope (using a Mountain as a tool).
narrative_ontology:constraint_beneficiary(universal_mathematics_communication, intelligent_life).

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

% PERSPECTIVE 1: THE HONEYBEE (POWERLESS SUBJECT) - MOUNTAIN
% For the bee, the logic of addition and subtraction is an immutable feature
% of the world they must navigate to survive (or get sugar). They cannot change
% the rules of "odd/even" or the existence of "zero"; they can only discover
% and adapt to them. It is a natural law.
constraint_indexing:constraint_classification(universal_mathematics_communication, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: NASA/SETI SCIENTIST (INSTITUTIONAL) - ROPE
% For institutions, mathematics is a coordination mechanism (Rope). It is
% functional and changeable in its *expression* (e.g., binary vs. prime sequences)
% to facilitate the goal of interstellar conversation. They are using a
% Mountain as a tool for coordination.
constraint_indexing:constraint_classification(universal_mathematics_communication, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER - MOUNTAIN
% The observer sees the deepest structure. Math is a Rope because different
% species may develop "different approaches... akin to dialects," but ultimately
% it is a Mountain because it is an unavoidable "consequence of intelligence"
% and the structure of reality.
constraint_indexing:constraint_classification(universal_mathematics_communication, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(universal_mathematics_communication_tests).

test(perspectival_gap) :-
    % Demonstrates that the Bee/Analyst sees a Mountain (nature) while the
    % Scientist sees a Rope (tool).
    constraint_indexing:constraint_classification(universal_mathematics_communication, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(universal_mathematics_communication, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(universal_mathematics_communication, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_threshold_validation) :-
    % Confirms that the base metrics are consistent with a Mountain classification.
    narrative_ontology:constraint_metric(universal_mathematics_communication, extractiveness, E),
    narrative_ontology:constraint_metric(universal_mathematics_communication, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    % Confirms the natural law profile metrics are present and valid.
    narrative_ontology:constraint_metric(universal_mathematics_communication, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(universal_mathematics_communication, resistance, R),
    domain_priors:emerges_naturally(universal_mathematics_communication),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(universal_mathematics_communication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core insight is that a fundamental Mountain (the laws of mathematics)
 *   can be used as a pure coordination device (Rope) by a sufficiently
 *   advanced agent. The base metrics (ε=0.10, suppression=0.05) and the
 *   `emerges_naturally` flag firmly establish the constraint's identity as a
 *   Mountain. The Natural Law profile metrics (accessibility_collapse=0.98,
 *   resistance=0.01) were added to pass the structural linter and correctly
 *   reflect the non-negotiable nature of mathematical truth.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between ontology and application.
 *   - The Honeybee (powerless) experiences the raw ontology: math is an
 *     unbreakable law of its environment, hence a Mountain.
 *   - The Scientist (institutional) experiences the application: math is a
 *     tool, a language whose notation can be chosen (binary, primes) to
 *     coordinate with a hypothetical other, hence a Rope.
 *   - The Analyst sees both: the underlying structure is a Mountain, which is
 *     what makes it a reliable candidate for use as a Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is fundamentally symmetric. The `constraint_beneficiary`
 *   is declared as `intelligent_life` because the coordination function of
 *   mathematics benefits any species capable of using it. There are no victims.
 *   This declaration is what allows the engine to derive `has_coordination_function`,
 *   enabling the Rope classification from the institutional perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is the inverse of Mandatrophy. Instead of mislabeling extraction
 *   as coordination, it shows how pure coordination can be built upon a
 *   foundation that is not coordination at all, but a natural law. It highlights
 *   the system's ability to distinguish between a constraint's fundamental
 *   nature (Mountain) and its indexed application (Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_math_ontological_status,
    'Is mathematics a human construct (Rope/Scaffold) or a universal consequence of intelligence (Mountain)?',
    'Comparison with verified extraterrestrial mathematical systems.',
    'If Mountain: Interstellar communication is guaranteed to be possible. If Rope: Contact may be impossible without shared culture.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_math_ontological_status, conceptual, 'The ontological status of mathematics (discovered vs. invented).').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(universal_mathematics_communication, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for this constraint as base_extractiveness (0.10) is below
% the threshold of 0.46.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No network relationships or Boltzmann data declared for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary status and
% exit options correctly models the perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */