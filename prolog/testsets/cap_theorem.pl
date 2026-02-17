% ============================================================================
% CONSTRAINT STORY: cap_theorem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_cap_theorem, []).

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
 *   constraint_id: cap_theorem
 *   human_readable: CAP Theorem (Brewer's Theorem)
 *   domain: technological
 *
 * SUMMARY:
 *   The CAP theorem states that any distributed data store can only provide two
 *   of three guarantees: Consistency (every read receives the most recent write),
 *   Availability (every request receives a response), and Partition Tolerance
 *   (the system continues to operate despite network failures). It is a
 *   fundamental, unchangeable limit in distributed computing, analogous to a
 *   law of physics. As such, it classifies as a Mountain from all perspectives,
 *   defining the landscape upon which other constraints (Ropes, Snares) are built.
 *
 * KEY AGENTS (by structural relationship):
 *   - The On-Call Engineer: Experiences the consequences of the theorem during a failure ([powerless]/[trapped]).
 *   - The Cloud Infrastructure Provider: Builds systems within the theorem's limits ([institutional]/[arbitrage]).
 *   - The Distributed Systems Architect: Analytical observer mapping the trade-offs ([analytical]/[analytical]).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: A logical theorem does not "extract" value; it defines the space
% of possibility. The cost of trade-offs is a consequence of reality, not an
% extraction by the theorem itself. ε is therefore near zero.
domain_priors:base_extractiveness(cap_theorem, 0.05).

% Rationale: The alternative (a CAP-complete system) is logically impossible.
% Its suppression is absolute and requires no active enforcement, hence a
% near-zero score.
domain_priors:suppression_score(cap_theorem, 0.01).
domain_priors:theater_ratio(cap_theorem, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cap_theorem, extractiveness, 0.05).
narrative_ontology:constraint_metric(cap_theorem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(cap_theorem, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
%
% Accessibility Collapse: Alternatives are logically impossible.
narrative_ontology:constraint_metric(cap_theorem, accessibility_collapse, 1.0).
% Resistance: Meaningful resistance is incoherent.
narrative_ontology:constraint_metric(cap_theorem, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cap_theorem, mountain).
narrative_ontology:human_readable(cap_theorem, "CAP Theorem (Brewer's Theorem)").

% --- Emergence flag (required for mountain constraints) ---
% The theorem is a discovered property of distributed systems, not a human construct.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(cap_theorem).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain (a logical law), the CAP theorem has no beneficiaries or
% victims in a structural sense. It is an inert feature of the environment.
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

% PERSPECTIVE 1: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The computer scientist sees CAP as a fundamental, unchangeable feature of
% the computational universe. It is a pure Mountain.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 2: THE INFRASTRUCTURE ARCHITECT (MOUNTAIN)
% For the CTO or architect, CAP is the terrain upon which they build Ropes
% (e.g., choosing an AP or CP system). The theorem itself is not the Rope;
% it is the unchangeable Mountain that necessitates the Rope's design.
% Their ability to choose between database types (arbitrage) does not
% change the nature of the underlying theorem.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ON-CALL ENGINEER (MOUNTAIN)
% During a network partition, the engineer is trapped in a situation that
% feels like a Snare. However, the theorem is not the Snare. The Snare is
% the specific system's failure mode, which is a consequence of the design
% choices made in the context of the CAP Mountain. The theorem itself remains
% an inert, unchangeable fact of the situation.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cap_theorem_tests).

test(perspectival_invariance) :-
    % Verify that as a fundamental law, it is a Mountain from all perspectives.
    constraint_indexing:constraint_classification(cap_theorem, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(cap_theorem, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cap_theorem, mountain, context(agent_power(powerless), _, _, _)).

test(threshold_validation) :-
    % Verify metrics are within Mountain thresholds.
    narrative_ontology:constraint_metric(cap_theorem, extractiveness, E),
    narrative_ontology:constraint_metric(cap_theorem, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify the constraint has the required profile for natural law certification.
    domain_priors:emerges_naturally(cap_theorem),
    narrative_ontology:constraint_metric(cap_theorem, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(cap_theorem, resistance, R), R =< 0.15.

:- end_tests(cap_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The CAP theorem is a formal, proven limit in distributed computing. Its
 *   metrics reflect this: base extractiveness (ε=0.05) is minimal, as the
 *   theorem itself doesn't extract value but defines the problem space.
 *   Suppression (0.01) is also minimal, as the alternative (a CAP-complete
 *   system) is logically impossible and requires no active suppression. The
 *   addition of the Natural Law profile metrics (accessibility_collapse=1.0,
 *   resistance=0.0, emerges_naturally=true) formally certifies its Mountain
 *   status, ensuring it passes the engine's structural checks for natural laws.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The theorem is a Mountain for all observers.
 *   The initial intuition that it is a Rope for an architect or a Snare for an
 *   engineer is a category error. Those agents are interacting with systems
 *   (Ropes, Snares) that are *built in response to* the Mountain. The framework
 *   correctly distinguishes the fundamental constraint (the theorem) from the
 *   contingent systems designed around it.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, the constraint is inert and has no directionality. It does
 *   not have structural beneficiaries or victims. It is a feature of the
 *   environment that affects all actors, much like gravity.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the CAP theorem as a Mountain, we avoid mislabeling a
 *   fundamental limit as a form of social or economic extraction. This prevents
 *   the false conclusion that the "costs" imposed by the theorem are the result
 *   of a policy that could be changed, rather than a logical necessity that must
 *   be engineered around.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cap_theorem,
    'Is the CAP theorem a true logical primitive, or could a novel computational model (e.g., quantum networking) create a context where the trade-offs are bypassed, revealing it as a Scaffold of classical computing?',
    'Formal proof of a distributed model that provides C, A, and P simultaneously under a non-classical paradigm.',
    'If true, the constraint would be reclassified from Mountain to Scaffold, representing a temporary limit of a specific technological era. If false, its Mountain status is further confirmed.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cap_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.05) is below the 0.46 threshold for
% mandatory temporal tracking. As a logical law, its metrics are static.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(cap_theorem, [type]).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(cap_theorem, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(cap_theorem, pacelc_theorem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a Mountain, the constraint is inert and has no
% inherent directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */