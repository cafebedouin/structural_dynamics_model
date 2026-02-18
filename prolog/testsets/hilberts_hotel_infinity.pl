% ============================================================================
% CONSTRAINT STORY: hilberts_hotel_infinity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_hilberts_hotel, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hilberts_hotel_infinity
 *   human_readable: Hilbert's Paradox of the Grand Hotel
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Hilbert's Hotel is a thought experiment illustrating the counter-intuitive
 *   properties of infinite sets. It describes a hotel with infinitely many
 *   rooms, all occupied. Despite being "full," the hotel can still accommodate
 *   new guests by shifting existing guests according to mathematical rules
 *   (e.g., Guest n moves to Room n+1). This constraint represents the
 *   unyielding logic of transfinite arithmetic, which is fixed and unchangeable.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Guest (powerless/trapped): An agent whose position is dictated by
 *     the mapping function f(n), with no ability to resist the logic.
 *   - The Mathematician/Educator (institutional/mobile): An agent who uses
 *     the paradox as a tool to coordinate understanding of transfinite numbers.
 *   - Agents relying on finite intuition (powerless/trapped): Those whose
 *     understanding of concepts like "fullness" is invalidated by the constraint.
 *   - The Analytical Observer (analytical/analytical): Sees the complete,
 *     unchangeable logical structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Low extraction. The paradox "extracts" the intuitive human sense
% of "fullness" and "location," but does not extract material value.
domain_priors:base_extractiveness(hilberts_hotel_infinity, 0.15).
% Rationale: Very low suppression. As a logical truth, it doesn't suppress
% alternatives; it simply defines the behavior of infinite sets within its
% axiomatic system. Set to meet the Mountain threshold.
domain_priors:suppression_score(hilberts_hotel_infinity, 0.05).
% Rationale: Zero theater. The paradox is a pure, functional logical concept.
domain_priors:theater_ratio(hilberts_hotel_infinity, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hilberts_hotel_infinity, extractiveness, 0.15).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Rationale: Within ZFC set theory, no alternative is conceivable.
narrative_ontology:constraint_metric(hilberts_hotel_infinity, accessibility_collapse, 1.0).
% Rationale: Resistance is incoherent; one cannot "resist" a mathematical proof.
narrative_ontology:constraint_metric(hilberts_hotel_infinity, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hilberts_hotel_infinity, mountain).
narrative_ontology:human_readable(hilberts_hotel_infinity, "Hilbert's Paradox of the Grand Hotel").
narrative_ontology:topic_domain(hilberts_hotel_infinity, "mathematical/logical").

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the axioms of Set Theory (ZFC).
domain_priors:emerges_naturally(hilberts_hotel_infinity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% While the core constraint is a Mountain, its application in pedagogy has a
% coordination function, justifying these declarations for the Rope perspective.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hilberts_hotel_infinity, mathematicians).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hilberts_hotel_infinity, agents_relying_on_finite_intuition).

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

% PERSPECTIVE 1: THE HOTEL GUEST (MOUNTAIN)
% For the guest, the rule that "there is always room" is a natural law of
% their universe. They have no agency to resist the shifting rule. The
% Mountain of transfinite arithmetic is unyielding.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MATHEMATICS TEACHER (ROPE)
% For the educator, Hilbert's Hotel is a "Rope"—a functional coordination
% mechanism. It allows them to pull students away from the confusion of
% finite intuition and toward a standard understanding of set theory.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical observer sees the constraint for what it is: a fixed,
% unchangeable logical consequence of the axioms of infinity.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hilberts_hotel_tests).

test(perspectival_gap) :-
    % Verify the gap between the guest's experience and the teacher's application.
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, rope,
        context(agent_power(institutional), _, _, _)).

test(analytical_view_is_mountain) :-
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
        context(agent_power(analytical), _, _, _)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(hilberts_hotel_infinity).

:- end_tests(hilberts_hotel_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core of this constraint is a logical truth, making its fundamental
 *   classification a Mountain (ε=0.15, suppression=0.05). The extractiveness
 *   score of 0.15 reflects the "cost" of abandoning finite intuition, a
 *   non-material extraction. The suppression is set to the Mountain ceiling
 *   (0.05) as the logic does not suppress alternatives but merely defines its
 *   domain. The NL profile metrics (accessibility_collapse=1.0, resistance=0.0)
 *   are set to their logical extremes, as is appropriate for a mathematical axiom.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the perception of the constraint as an unchangeable
 *   law of reality (Mountain, for the guest and analyst) and its use as a
 *   functional tool for coordinating knowledge (Rope, for the educator).
 *   The educator benefits from this structure, using it to teach, while the
 *   guest (or anyone trapped by finite intuition) simply experiences its
 *   immutable effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are 'mathematicians' who gain a powerful tool for reasoning
 *   about infinity. Victims are 'agents_relying_on_finite_intuition', as
 *   their conceptual models are invalidated. This victimhood is abstract,
 *   leading to a low base extraction score.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain from the analytical perspective prevents
 *   misinterpreting the pedagogical "Rope" as the entire story. It correctly
 *   identifies the underlying structure as a fixed, non-negotiable property
 *   of a logical system, not a socially constructed coordination device.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_hilberts_hotel,
    "Is the 'extraction' from finite intuition a meaningful cost, or simply the price of knowledge?",
    "Philosophical analysis of cognitive frameworks and the value of counter-intuitive truths.",
    "If it's a true cost, the extraction score could be higher. If it's just learning, the score is appropriate.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_hilberts_hotel, conceptual,
    "Whether the invalidation of finite intuition constitutes genuine extraction.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hilberts_hotel_infinity, 1924, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.15) is below the
% 0.46 threshold. The logical constraint is static and does not drift.

/*
narrative_ontology:measurement(hilberts_hotel_infinity_tr_t0, hilberts_hotel_infinity, theater_ratio, 0, 0.0).
narrative_ontology:measurement(hilberts_hotel_infinity_tr_t5, hilberts_hotel_infinity, theater_ratio, 5, 0.0).
narrative_ontology:measurement(hilberts_hotel_infinity_tr_t10, hilberts_hotel_infinity, theater_ratio, 10, 0.0).

narrative_ontology:measurement(hilberts_hotel_infinity_ex_t0, hilberts_hotel_infinity, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hilberts_hotel_infinity_ex_t5, hilberts_hotel_infinity, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(hilberts_hotel_infinity_ex_t10, hilberts_hotel_infinity, base_extractiveness, 10, 0.15).
*/

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The pedagogical use of the paradox is an information standard.
% narrative_ontology:coordination_type(hilberts_hotel_infinity, information_standard).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(hilberts_hotel_infinity, other_constraint_id).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options is sufficient to model the perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */