% ============================================================================
% CONSTRAINT STORY: exit_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exit_cost_asymmetry, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exit_cost_asymmetry
 *   human_readable: Exit Cost Asymmetry as Structural Constraint
 *   domain: political_philosophy/organizational_theory/ethics_of_agency
 *
 * SUMMARY:
 *   Exit cost asymmetry is the structural fact that leaving a situation
 *   requires upfront costs (financial, social, psychological, logistical)
 *   while remaining does not. This asymmetry is not an artifact of any
 *   particular institutional arrangement but a consequence of how commitment,
 *   coordination, and path dependence work in social systems. The constraint
 *   is scale-invariant: individuals face it in employment decisions, families
 *   face it in housing decisions, organizations face it in strategic
 *   decisions, and nations face it in alliance decisions. The asymmetry
 *   creates a bias toward status quo even when the status quo is
 *   deteriorating, because exit requires crossing a cost threshold that
 *   inaction does not. This is a genuine mountain — a structural feature of
 *   agency under uncertainty — not a policy choice that could be eliminated
 *   through institutional reform. The constraint can be measured via
 *   debt-to-income ratios (financial exit costs), contract penalty clauses
 *   (legal exit costs), social network density (social capital exit costs),
 *   and geographic mobility constraints (logistical exit costs). All agents
 *   across all power levels and time horizons perceive this as an immutable
 *   constraint, though its magnitude varies with resources and context.
 *
 * KEY AGENTS:
 *   - Trapped Agent: Powerless/trapped — faces maximum magnitude exit costs with no resources to absorb them; experiences the constraint as an absolute barrier
 *   - Constrained Planner: Moderate/constrained — can plan to reduce exit costs over biographical time but cannot eliminate the asymmetry
 *   - Mobile Professional: Powerful/mobile — faces reduced magnitude exit costs but still experiences the structural asymmetry
 *   - Institutional Actor: Institutional/arbitrage — faces organizational inertia and path dependence as manifestations of exit cost asymmetry
 *   - Organized Coalition: Organized/constrained — can pool resources to reduce individual exit costs but faces collective exit cost asymmetry
 *   - Analytical Observer: Analytical/analytical — recognizes exit cost asymmetry as a mathematical property of decision spaces under commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exit_cost_asymmetry, 0.08).
domain_priors:suppression_score(exit_cost_asymmetry, 0.03).
domain_priors:theater_ratio(exit_cost_asymmetry, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exit_cost_asymmetry, extractiveness, 0.08).
narrative_ontology:constraint_metric(exit_cost_asymmetry, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(exit_cost_asymmetry, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exit_cost_asymmetry, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(exit_cost_asymmetry, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exit_cost_asymmetry, mountain).
narrative_ontology:human_readable(exit_cost_asymmetry, "Exit Cost Asymmetry as Structural Constraint").
narrative_ontology:topic_domain(exit_cost_asymmetry, "political_philosophy/organizational_theory/ethics_of_agency").

domain_priors:emerges_naturally(exit_cost_asymmetry).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED AGENT (MOUNTAIN) — Exit costs are an immutable structural fact at immediate time horizons. The debt-to-income ratio, contract penalties, and geographic constraints are not negotiable or changeable through individual action. The asymmetry between staying and leaving is a fixed parameter of the decision space.
constraint_indexing:constraint_classification(exit_cost_asymmetry, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED PLANNER (MOUNTAIN) — Even with moderate resources and biographical time horizons, exit cost asymmetry remains a structural constraint. The agent can plan around it, save to reduce debt ratios, or negotiate contract terms, but cannot eliminate the fundamental asymmetry that exit requires upfront costs while staying does not. The constraint is changeable in magnitude but not in kind.
constraint_indexing:constraint_classification(exit_cost_asymmetry, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MOBILE PROFESSIONAL (MOUNTAIN) — Powerful agents with mobility still face exit cost asymmetry, though at lower magnitude. Relocation costs, social capital loss, and opportunity costs of transition are structural features of any exit decision. The asymmetry is reduced but not eliminated by resources.
constraint_indexing:constraint_classification(exit_cost_asymmetry, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL ACTOR (MOUNTAIN) — Institutions face exit cost asymmetry in organizational decisions: divesting from a market, relocating operations, or dissolving partnerships all require upfront costs that maintaining status quo does not. The asymmetry is a structural feature of organizational inertia and path dependence.
constraint_indexing:constraint_classification(exit_cost_asymmetry, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — Exit cost asymmetry is a mathematical property of decision spaces under uncertainty. The asymmetry derives from the irreversibility of time and the sunk cost structure of social arrangements. It is not a policy choice or institutional artifact but a consequence of how commitment and coordination work in any social system. This is a genuine mountain, not a false summit — the constraint emerges from the structure of agency itself.
constraint_indexing:constraint_classification(exit_cost_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ORGANIZED COALITION (MOUNTAIN) — Collective action can reduce exit costs for individual members (mutual aid funds, relocation assistance, shared housing) but cannot eliminate the structural asymmetry. The coalition faces its own exit cost asymmetry in deciding whether to dissolve or persist. The constraint is scale-invariant.
constraint_indexing:constraint_classification(exit_cost_asymmetry, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exit_cost_asymmetry_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(exit_cost_asymmetry, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exit_cost_asymmetry, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(exit_cost_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(exit_cost_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(exit_cost_asymmetry),
    narrative_ontology:constraint_metric(exit_cost_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(exit_cost_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(exit_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. Exit cost asymmetry does not extract from agents — it is a structural feature of the decision space, not a mechanism that transfers value. The small non-zero value reflects that the asymmetry creates friction costs (agents may remain in suboptimal situations longer than they would in a frictionless world), but these are coordination costs, not extraction. Suppression (0.03): Very low. The constraint does not suppress alternatives through coercion. Agents are free to exit; the asymmetry is a cost structure, not a prohibition. The small non-zero value reflects that high exit costs can function as de facto barriers, but this is a side effect of the cost structure, not active suppression. Theater ratio (0.05): Very low. There is no performative component to exit cost asymmetry. The costs are real and measurable. Accessibility collapse (0.92): Very high. Exit cost asymmetry is universally accessible to all agents — everyone who faces an exit decision perceives the cost differential. Resistance (0.08): Very low. The constraint is not contested. No agent claims that exit costs are symmetric or that the asymmetry is eliminable through policy. The resistance value reflects only that agents may dispute the magnitude of specific exit costs, not the existence of the asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this constraint story. All agents across all power levels, time horizons, exit options, and spatial scopes classify exit cost asymmetry as a mountain. The constraint is a uniform-type mountain — a genuine natural law of social coordination. The magnitude of experienced exit costs varies with resources (powerless agents face higher costs than powerful agents), but the structural asymmetry itself is invariant. This is the diagnostic signature of a true mountain: no agent, regardless of position, perceives the constraint as changeable or eliminable. The constraint is not naturalized (a false summit) but genuinely structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Exit cost asymmetry has no beneficiaries or victims in the structural sense. It is not a mechanism that transfers value between agents but a property of decision spaces. All agents experience it as a constraint on their action space, but none benefit from its existence. The constraint is symmetric in the sense that it applies to all agents, though asymmetric in magnitude (powerless agents face higher exit costs than powerful agents). Because there are no beneficiaries or victims, directionality values are not derived from structural relationships but from the canonical fallback for each power atom. All perspectives classify as mountain because the constraint is genuinely immutable at all time horizons and power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   Exit cost asymmetry resolves the mandatrophy by being a genuine coordination constraint with no extractive component. The asymmetry is not a mechanism that benefits some agents at the expense of others but a structural feature of how commitment and path dependence work in any social system. The constraint creates friction costs (agents remain in suboptimal situations longer than they would in a frictionless world), but these are coordination costs inherent to the decision problem, not extraction. The low extractiveness (0.08) reflects this: the constraint imposes costs on agents, but those costs are not transferred to beneficiaries — they are dissipated as friction. This is the paradigm case of a mountain that is not a snare in disguise: the constraint is immutable, universally experienced, and non-extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exit_cost_asymmetry, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exit_cost_asymmetry, resource_allocation).

% DUAL FORMULATION NOTE:
% Exit cost asymmetry is a foundational constraint that appears as a component in many other constraints (debt traps, non-compete clauses, immigration restrictions, alliance commitments). Those constraints are distinct stories with their own extractiveness values reflecting the specific institutional arrangements that amplify or exploit the underlying asymmetry. This story models the asymmetry itself, not its institutional manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
