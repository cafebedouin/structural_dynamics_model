% ============================================================================
% CONSTRAINT STORY: pareto_principle
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_pareto_principle, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pareto_principle
 *   human_readable: The Pareto Principle (80/20 Rule)
 *   domain: statistical/economic/social
 *
 * SUMMARY:
 *   The Pareto Principle is a statistical observation that for many outcomes,
 *   roughly 80% of consequences come from 20% of causes. This power-law
 *   distribution is a recurring pattern in systems with feedback loops and
 *   cumulative advantage (e.g., wealth distribution, software bugs, city sizes).
 *   This story models the principle itself as a statistical law of nature,
 *   not the social policies that might result from it. As a statistical
 *   regularity, it is an unchangeable feature of the landscape.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Long-Tail Producer (powerless/trapped): An actor whose output falls in the "trivial many" 80%.
 *   - The System Optimizer (moderate/mobile): An actor who uses knowledge of the principle to leverage the "vital few" 20%.
 *   - The Economic Analyst (analytical/analytical): An observer modeling the distribution as a fundamental property of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% As a statistical observation, the principle itself has near-zero extractiveness.
% The extraction occurs in downstream systems that operate according to this principle.
domain_priors:base_extractiveness(pareto_principle, 0.05).
% The principle cannot be suppressed; it is an emergent property of complex systems.
domain_priors:suppression_score(pareto_principle, 0.02).
% The principle is a raw statistical fact with no performative aspect.
domain_priors:theater_ratio(pareto_principle, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pareto_principle, extractiveness, 0.05).
narrative_ontology:constraint_metric(pareto_principle, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(pareto_principle, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
%
% In systems with preferential attachment, alternatives to power-law distributions are structurally inaccessible.
narrative_ontology:constraint_metric(pareto_principle, accessibility_collapse, 0.95).
% One cannot meaningfully "resist" a statistical pattern, only its effects. Resistance is incoherent.
narrative_ontology:constraint_metric(pareto_principle, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pareto_principle, mountain).
narrative_ontology:human_readable(pareto_principle, "The Pareto Principle (80/20 Rule)").
narrative_ontology:topic_domain(pareto_principle, "statistical/economic/social").

% --- Emergence flag (required for mountain constraints) ---
% The distribution emerges naturally from preferential attachment and power laws.
domain_priors:emerges_naturally(pareto_principle).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), the Pareto Principle does
% not have designed beneficiaries or victims. The distribution it describes
% creates winners and losers, but those are features of a downstream constraint
% (e.g., 'market_dynamics'), not this one.

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

% PERSPECTIVE 1: THE LONG-TAIL PRODUCER (MOUNTAIN)
% For an actor in the "trivial many," the principle is an unchangeable,
% harsh reality of the market landscape they inhabit. It is a feature of
% the terrain, not a rule someone imposed.
constraint_indexing:constraint_classification(pareto_principle, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SYSTEM OPTIMIZER (MOUNTAIN)
% For a business manager or engineer, the principle is an immutable law of
% systems that they must work with. It's like gravity; you don't fight it,
% you design around it to achieve leverage.
constraint_indexing:constraint_classification(pareto_principle, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ECONOMIC ANALYST (MOUNTAIN)
% For a policy maker or analyst, the principle is a fundamental 'Mountain' – an
% immutable statistical reality they must plan around for long-term policy
% (e.g., taxation, social welfare). It is an unchangeable input to their models.
constraint_indexing:constraint_classification(pareto_principle, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context confirms the classification is invariant.
constraint_indexing:constraint_classification(pareto_principle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pareto_principle_tests).

test(perspectival_invariance) :-
    % Verify that as a natural law, it is classified as Mountain from all key perspectives.
    constraint_indexing:constraint_classification(pareto_principle, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    config:param(mountain_extractiveness_max, MountainEpsMax),
    config:param(mountain_suppression_ceiling, MountainSuppMax),
    narrative_ontology:constraint_metric(pareto_principle, ExtMetricName, E),
    narrative_ontology:constraint_metric(pareto_principle, SuppMetricName, S),
    E =< MountainEpsMax,
    S =< MountainSuppMax.

:- end_tests(pareto_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Pareto Principle is modeled as a Mountain because it is a statistical
 *   observation about the universe, akin to the law of large numbers or the
 *   central limit theorem. It is not a human-designed rule system. Its base
 *   extractiveness and suppression are near-zero because the principle itself
 *   does not act; it merely describes a pattern that emerges naturally in
 *   systems with cumulative advantage. The necessary Natural Law (NL) profile
 *   metrics (accessibility_collapse, resistance, emerges_naturally) have been
 *   added to ensure it passes the NL certification chain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a true Mountain, the classification is
 *   invariant across all observer indices. While the *consequences* of the
 *   principle are felt very differently by those in the "vital few" versus the
 *   "trivial many," the principle itself remains an unchangeable feature of
 *   the landscape for everyone. The negative experience of someone in the
 *   long tail is a result of a downstream constraint (e.g., `market_competition`)
 *   that is affected by the Pareto Principle, not the principle itself.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, beneficiary and victim declarations are not applicable.
 *   The constraint is a symmetric feature of reality.
 *
 * MANDATROPHY ANALYSIS:
 *   Modeling this as a Mountain correctly separates the descriptive statistical
 *   law from the prescriptive social or economic systems that operate within it.
 *   An incorrect analysis might conflate the principle with its outcomes (e.g.,
 *   wealth inequality) and misclassify it as a Snare. This would be a category
 *   error, attributing agency and enforcement to a statistical pattern. The
 *   framework correctly identifies the principle as part of the immutable
 *   background, while a separate constraint story would be needed to model the
 *   (potentially Snare-like) rules of a specific market.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_pareto_principle,
    'Is the Pareto distribution a truly fundamental law of complex systems, or is it contingent on specific initial conditions (e.g., scarcity, competition) that could theoretically be altered?',
    'Identifying or creating a large-scale complex adaptive system with cumulative advantage that reliably produces a non-power-law distribution of outcomes.',
    'If fundamental (Mountain), policy should focus on mitigating its effects (e.g., redistribution). If contingent (Tangled Rope), policy could focus on changing the system rules to prevent its emergence.',
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_pareto_principle, conceptual, 'Is the Pareto distribution a fundamental law or a contingent outcome of system rules?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pareto_principle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.05) is below the 0.46 threshold for
% mandatory lifecycle drift tracking. As a statistical law, its properties
% are considered time-invariant.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% The Pareto Principle is a foundational statistical constraint that influences
% the structure of many economic and social constraints.
% narrative_ontology:affects_constraint(pareto_principle, wealth_inequality).
% narrative_ontology:affects_constraint(pareto_principle, market_concentration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */