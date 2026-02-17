% ============================================================================
% CONSTRAINT STORY: burali_forti_paradox
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_burali_forti_paradox, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burali_forti_paradox
 *   human_readable: Burali-Forti Paradox
 *   domain: technological/mathematics
 *
 * SUMMARY:
 *   The Burali-Forti paradox demonstrates that the collection of all ordinal
 *   numbers cannot itself be a set. If it were, it would have an ordinal
 *   number greater than every ordinal in the collection, leading to a logical
 *   contradiction. It represents a fundamental limit on "totalities" in
 *   naive set theory, forcing the development of more constrained axiomatic
 *   systems like ZFC.
 *
 * KEY AGENTS (by structural relationship):
 *   - Naive comprehension proponents: Primary target (powerless/trapped) — their foundational system is collapsed by the paradox.
 *   - Axiomatic set theorists: Primary beneficiary (institutional/arbitrage) — use the paradox to justify and enforce distinctions (e.g., sets vs. proper classes) that ensure consistency.
 *   - Analytical observer: Sees the paradox as a fundamental, unchangeable feature of logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burali_forti_paradox, 0.20).
domain_priors:suppression_score(burali_forti_paradox, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(burali_forti_paradox, 0.02).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burali_forti_paradox, extractiveness, 0.20).
narrative_ontology:constraint_metric(burali_forti_paradox, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(burali_forti_paradox, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(burali_forti_paradox, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(burali_forti_paradox, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(burali_forti_paradox, mountain).
narrative_ontology:human_readable(burali_forti_paradox, "Burali-Forti Paradox").

% --- Emergence flag (required for mountain constraints) ---
% The paradox emerges naturally from the definitions of ordinals and well-ordering.
domain_priors:emerges_naturally(burali_forti_paradox).

% --- Structural relationships (REQUIRED for non-mountain perspectives) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(burali_forti_paradox, axiomatic_set_theorists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(burali_forti_paradox, naive_comprehension_proponents).

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

% PERSPECTIVE 1: THE NAIVE MATHEMATICIAN (SNARE)
% Agent attempting to use unrestricted comprehension. The paradox acts as a
% trap that collapses their foundational system into contradiction.
constraint_indexing:constraint_classification(burali_forti_paradox, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE AXIOMATIC SYSTEM (ZFC) (ROPE)
% For a formal system like ZFC, the paradox is a coordination mechanism. It
% forces the distinction between "Sets" and "Proper Classes," a functional
% tool that prevents the system from collapsing.
constraint_indexing:constraint_classification(burali_forti_paradox, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% To the logician or set theorist, the paradox is an unchangeable feature of
% the mathematical landscape. It is a fundamental limit that cannot be
% altered, only navigated around.
constraint_indexing:constraint_classification(burali_forti_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burali_forti_paradox_tests).

test(perspectival_gap) :-
    % Verify the classification changes across key perspectives.
    constraint_indexing:constraint_classification(burali_forti_paradox, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(burali_forti_paradox, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(burali_forti_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    TypeAnalytical == mountain,
    TypeInstitutional == rope,
    TypePowerless == snare,
    TypeAnalytical \= TypeInstitutional,
    TypeInstitutional \= TypePowerless.

test(analytical_is_mountain) :-
    constraint_indexing:constraint_classification(burali_forti_paradox, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_thresholds_adherence) :-
    % Verify the base metrics align with the Mountain classification.
    narrative_ontology:constraint_metric(burali_forti_paradox, extractiveness, E),
    narrative_ontology:constraint_metric(burali_forti_paradox, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(burali_forti_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.20) is low, as the "extraction" is purely
 *   semantic: the loss of unrestricted comprehension. It does not extract
 *   material resources. The suppression score (0.05) is very low because
 *   while alternative axiomatic systems (like NF) exist, they do not
 *   "suppress" the paradox itself; they are merely different ways of avoiding
 *   it. The paradox is a feature of logic, not a policy choice. The NL
 *   profile metrics (collapse=1.0, resistance=0.0) reflect its status as an
 *   absolute logical limit.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a naive actor, the paradox is a Snare that destroys
 *   their system. For an institutional system (ZFC), it's a Rope used to
 *   coordinate behavior (distinguishing sets from classes). For the analytical
 *   observer, it is a Mountain—an immutable law of formal systems. This
 *   Mountain -> Rope -> Snare transition is a classic signature of a
 *   fundamental limit being managed by an institutional framework.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `axiomatic_set_theorists` who build and maintain consistent
 *     formal systems. The paradox provides the justification for the rules
 *     (like the Axiom of Regularity) that give their systems power and stability.
 *   - Victims: `naive_comprehension_proponents` who believe any describable
 *     collection can form a set. The paradox directly targets and invalidates
 *     this belief.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the core constraint as a Mountain
 *   of logic. The Rope and Snare classifications are emergent properties of
 *   how different agents interact with this Mountain. An analysis that only
 *   saw the Snare (the contradiction) or the Rope (the ZFC rules) would miss
 *   the underlying, unchangeable reality that necessitates them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_burali_forti_paradox,
    "Are 'Proper Classes' a real mathematical landscape (Mountain) or merely a linguistic scaffold (Rope) to avoid the Snare?",
    "Development of a grand unified theory of categories vs sets.",
    "If Mountain: The hierarchy of size is a fundamental limit. If Rope: It's a bug in our current notation.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_burali_forti_paradox, conceptual, "Are 'Proper Classes' a real feature of mathematics or a notational patch?").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(burali_forti_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This is a fundamental logical constraint, not a designed coordination
% mechanism, so Boltzmann/Network data is not applicable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% The standard derivation from beneficiary/victim status and exit options
% accurately models the directionality for all agents. No overrides needed.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */