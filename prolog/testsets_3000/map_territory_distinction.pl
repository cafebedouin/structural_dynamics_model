% ============================================================================
% CONSTRAINT STORY: map_territory_distinction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_map_territory_distinction, []).

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
 *   constraint_id: map_territory_distinction
 *   human_readable: The Map-Territory Distinction
 *   domain: epistemology/logic/semantics
 *
 * SUMMARY:
 *   The map-territory distinction is a fundamental epistemological
 *   constraint: any representation of reality differs from reality itself in
 *   ways that cannot be eliminated, only managed. This constraint operates at
 *   the intersection of logic, semantics, and pragmatics. Maps compress,
 *   abstract, distort, and simplify territory. Maps introduce new properties
 *   (color, grid lines, legends) that territory lacks. Maps operate under
 *   different logical and physical rules than territory. No agent — whether
 *   powerless navigator, moderate planner, institutional actor, or analytical
 *   observer — can escape this distinction. The constraint is not contingent
 *   on technology, intelligence, or resources. It follows from the basic
 *   asymmetry between representation (discrete, finite, intentional,
 *   symbolic) and referent (continuous, boundless, indifferent, material).
 *   This distinguishes it sharply from institutional barriers or coordination
 *   problems that might be ameliorated. The constraint is natural law.
 *
 * KEY AGENTS:
 *   - The Agent Without Language: Experiences the distinction as brute fact — territory exists whether or not mapped
 *   - The Practical Navigator: Relies on maps for coordination but remains bound by the distinction — maps guide but cannot replace navigation
 *   - The Institutional Knowledge Keeper: Maintains maps (policies, models, documentation) that diverge from territory (outcomes, experiences)
 *   - The Analytical Observer: Recognizes the distinction as logically necessary, not contingent on scale or detail
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(map_territory_distinction, 0.08).
domain_priors:suppression_score(map_territory_distinction, 0.03).
domain_priors:theater_ratio(map_territory_distinction, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(map_territory_distinction, extractiveness, 0.08).
narrative_ontology:constraint_metric(map_territory_distinction, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(map_territory_distinction, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(map_territory_distinction, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(map_territory_distinction, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(map_territory_distinction, mountain).
narrative_ontology:human_readable(map_territory_distinction, "The Map-Territory Distinction").
narrative_ontology:topic_domain(map_territory_distinction, "epistemology/logic/semantics").

domain_priors:emerges_naturally(map_territory_distinction).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGENT WITHOUT LANGUAGE (MOUNTAIN) — An agent with no representational capacity experiences the constraint as absolute: the territory exists independent of any map. No exit from this fundamental distinction — representation always differs from referent. Even the pre-linguistic organism is bound by this asymmetry.
constraint_indexing:constraint_classification(map_territory_distinction, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PRACTICAL NAVIGATOR (MOUNTAIN) — Even with maps and territory both available, the distinction remains binding: a map of terrain cannot replace walking the terrain. The constraint holds regardless of how sophisticated the map becomes. No exit from the asymmetry — better maps do not eliminate the need to navigate reality.
constraint_indexing:constraint_classification(map_territory_distinction, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: INSTITUTIONAL KEEPER (MOUNTAIN) — Organizations maintain maps (models, policies, documentation) that must be distinguished from territory (actual outcomes, lived experiences). The constraint binds institutions regardless of their complexity or resources. Even the most sophisticated bureaucracy cannot eliminate the gap between policy and reality.
constraint_indexing:constraint_classification(map_territory_distinction, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of formal logic and semantics, the distinction is irreducible: any finite representation of territory necessarily excludes information, introduces distortion, and operates under different logical rules than the territory itself. This is not a contingent property of current maps but a structural consequence of representation itself. Zero degrees of freedom.
constraint_indexing:constraint_classification(map_territory_distinction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(map_territory_distinction_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(map_territory_distinction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(map_territory_distinction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(map_territory_distinction, ExtMetricName, E),
    domain_priors:suppression_score(map_territory_distinction, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(map_territory_distinction),
    narrative_ontology:constraint_metric(map_territory_distinction, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(map_territory_distinction, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(map_territory_distinction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. No agent profits from the map-territory distinction itself — it is not an extraction mechanism but a structural property of representation. The distinction does not create asymmetry in access or benefit; it constrains everyone equally. Suppression (0.03): Minimal. The distinction is not suppressed by lack of alternatives or coercive enforcement. Agents are aware of it and acknowledge it constantly ('the map is not the territory'). Theater ratio (0.12): Minimal. The distinction requires no performative maintenance or theatrical enforcement. It holds regardless of whether any agent invokes it. Accessibility collapse (0.92): Very high. There are no meaningful alternatives to accepting this distinction — all representation systems must grapple with it. No exit exists through better maps, simulations, or knowledge. Resistance (0.08): Very low. The distinction faces no structural opposition. Even those who contest specific applications of it must use representations to do so, thereby instantiating the very distinction they contest.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on the same classification (mountain). The distinction is invariant across power level, time horizon, exit options, and spatial scope. The powerless agent, the moderate planner, the institutional keeper, and the analytical observer all experience the same immutable constraint. This convergence is itself a signature of natural law status — the constraint is not observable-relative or context-dependent. No perspectival gap emerges because the constraint is not about relationships between agents but about the fundamental structure of representation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to this constraint. There are no beneficiaries or victims — the distinction does not create extraction. There is no asymmetry in who bears the cost or who benefits. All agents, regardless of power or position, experience the map-territory distinction equally. This absence of directionality is itself a diagnostic signature of mountain status.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    representation_completeness,
    'Can a map ever be sufficiently detailed that the distinction between map and territory becomes meaningless?',
    'Analysis of infinite-detail mapping (territory at scale 1:1); examination of whether perfect fidelity requires the map to instantiate all properties of the territory, collapsing the distinction into identity',
    'If impossible: confirms mountain status. If achievable: the distinction is contingent on resolution/scale, potentially weakening the mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(representation_completeness, conceptual, 'Whether infinite-fidelity representation collapses the map-territory distinction').

omega_variable(
    territory_independence,
    'Does territory exist independent of representation, or is all territory necessarily known-through-representation?',
    'Philosophical analysis of realism vs idealism; epistemological investigation of whether territory-in-itself is coherent without an observer or representational framework',
    'If territory is independent: mountain status confirmed. If all territory is conceptually dependent on representation: the distinction may be less absolute than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territory_independence, conceptual, 'Whether territory has ontological independence from representation').

omega_variable(
    pragmatic_collapse,
    'In practical contexts (AI training, simulation, virtual environments), can a map become functionally isomorphic to territory for purposes of action and prediction?',
    'Empirical testing in domains where agents act based on maps (video game worlds, simulations, digital twins); measurement of whether prediction/action error rates converge to zero in closed systems',
    'If maps become functionally indistinguishable: suppression value may be lower than 0.03 in those domains, and the constraint''s universality weakens. If permanent gap persists: mountain status confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatic_collapse, empirical, 'Whether maps can become functionally equivalent to territory in closed systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(map_territory_distinction, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(map_terr_tr_t0, map_territory_distinction, theater_ratio, 0, 0.12).
narrative_ontology:measurement(map_terr_tr_t50, map_territory_distinction, theater_ratio, 50, 0.12).
narrative_ontology:measurement(map_terr_tr_t100, map_territory_distinction, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(map_terr_be_t0, map_territory_distinction, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(map_terr_be_t50, map_territory_distinction, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(map_terr_be_t100, map_territory_distinction, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(map_territory_distinction, information_standard).
narrative_ontology:affects_constraint(map_territory_distinction, goodharts_law).
narrative_ontology:affects_constraint(map_territory_distinction, lossy_abstraction_principle).
narrative_ontology:affects_constraint(map_territory_distinction, symbol_grounding_problem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
