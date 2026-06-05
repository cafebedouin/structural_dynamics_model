% ============================================================================
% CONSTRAINT STORY: law_of_diminishing_returns
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_law_of_diminishing_returns, []).

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
 *   constraint_id: law_of_diminishing_returns
 *   human_readable: The Law of Diminishing Returns
 *   domain: economic/production_theory
 *
 * SUMMARY:
 *   The law of diminishing returns is one of the oldest and most fundamental
 *   principles in economic thought, dating to 19th-century agricultural
 *   economics and formalized in 20th-century production theory. It states
 *   that holding all factors of production constant except one, incrementally
 *   increasing the variable factor will eventually yield declining marginal
 *   returns. This constraint is classified as a Mountain across all
 *   perspectives because it emerges from the mathematical structure of
 *   production functions, not from institutional arrangements, market power,
 *   or policy choices. The constraint has zero degrees of freedom: no agent
 *   can escape it, no innovation can permanently nullify it (though
 *   technological progress can shift the boundary), and no coordination
 *   mechanism can transform it into a different type. Unlike snares or
 *   tangled ropes that depend on specific beneficiaries and victims,
 *   diminishing returns operates uniformly across all production
 *   systems—medieval agriculture, industrial manufacturing, software
 *   development, service delivery. The mathematical necessity of the
 *   relationship (marginal product must decline if one factor is fixed) makes
 *   it invariant across observables and perspectives.
 *
 * KEY AGENTS:
 *   - Mathematical Economist: Analytical observer (analytical/analytical) — perceives the constraint as a logical necessity inherent in production function structure
 *   - Industrial Producer: Powerful decision-maker (powerful/mobile) — experiences diminishing returns as a fixed limit on capital investment returns, regardless of market context
 *   - Development Bank: Institutional actor (institutional/arbitrage) — encounters diminishing returns as an invariant constraint on all agricultural and industrial productivity interventions
 *   - Subsistence Farmer: Powerless agent (powerless/trapped) — directly experiences diminishing returns as the fixed boundary within which survival labor is conducted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(law_of_diminishing_returns, 0.12).
domain_priors:suppression_score(law_of_diminishing_returns, 0.03).
domain_priors:theater_ratio(law_of_diminishing_returns, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(law_of_diminishing_returns, extractiveness, 0.12).
narrative_ontology:constraint_metric(law_of_diminishing_returns, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(law_of_diminishing_returns, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(law_of_diminishing_returns, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(law_of_diminishing_returns, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(law_of_diminishing_returns, mountain).
narrative_ontology:human_readable(law_of_diminishing_returns, "The Law of Diminishing Returns").
narrative_ontology:topic_domain(law_of_diminishing_returns, "economic/production_theory").

domain_priors:emerges_naturally(law_of_diminishing_returns).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL ECONOMIST (MOUNTAIN) — From a formal economic analysis perspective, diminishing returns emerges as a mathematical consequence of production function structure. Given a Cobb-Douglas or CES production function with fixed complementary factors, the marginal product of any single input must eventually decline. This is a logical necessity, not a contingent institutional arrangement. ε=0.12, suppression=0.03, accessibility_collapse=0.92. Universal scope: the pattern holds across all commodity production systems.
constraint_indexing:constraint_classification(law_of_diminishing_returns, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: INDUSTRIAL PRODUCER (MOUNTAIN) — A large-scale manufacturer planning 20-year capital investment sees diminishing returns as an immutable constraint on productivity scaling. They can exit by relocating production or adopting different techniques, but they cannot escape the mathematical relationship: doubling fertilizer while holding land constant will not double yields indefinitely. The constraint is visible regardless of market conditions, labor costs, or technology choices. This is the lived experience of the constraint for those making production decisions: it is as fixed as gravity.
constraint_indexing:constraint_classification(law_of_diminishing_returns, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPMENT BANK (MOUNTAIN) — A financial institution evaluating agricultural productivity investments across regions encounters diminishing returns as a fundamental economic law. Whether intervening in subsistence farming or industrial agriculture, the constraint is invariant: returns per unit of capital decline predictably. The bank cannot arbitrage away from this law — it must instead incorporate it into risk assessment. No institutional workaround bypasses the underlying mathematics.
constraint_indexing:constraint_classification(law_of_diminishing_returns, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: SUBSISTENCE FARMER (MOUNTAIN) — A farmer with fixed land, working at the productivity frontier with no access to capital or technology, directly experiences diminishing returns as an immutable limit on output. Adding more hours of labor, more seed, or more tools yields less additional output as marginal product declines. This is not extractive — it is a natural limit encoded in the production process itself. No amount of effort overcomes it; the farmer experiences it as the fixed structure within which survival decisions occur.
constraint_indexing:constraint_classification(law_of_diminishing_returns, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(law_of_diminishing_returns_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(law_of_diminishing_returns, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(law_of_diminishing_returns, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(law_of_diminishing_returns, ExtMetricName, E),
    domain_priors:suppression_score(law_of_diminishing_returns, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(law_of_diminishing_returns),
    narrative_ontology:constraint_metric(law_of_diminishing_returns, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(law_of_diminishing_returns, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(law_of_diminishing_returns_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Extremely low. Diminishing returns is not an extraction mechanism — it does not transfer value from one agent to another. It is a physical/mathematical constraint on how much output can be produced from given inputs. The low ε reflects that the 'loss' in marginal productivity is not captured by any beneficiary; it simply dissipates as unrealized output. Suppression (0.03): Negligible. There is no coercion involved. Agents are not prevented from applying additional factor inputs — they simply observe declining returns when they do. The low suppression reflects complete transparency: the constraint is visible in every production accounting system. Theater ratio (0.15): Very low. No performative activity is required to maintain the constraint. It is not a ritual or an institutional fiction; it is a straightforward mathematical relationship. The minimal theater reflects that no organizational effort is needed to keep the law 'in force' — it operates through production function structure alone. Accessibility collapse (0.92): Very high. The constraint is completely inaccessible to violation or exemption. No producer, no matter how powerful, can achieve indefinitely increasing marginal returns while holding complementary factors constant. The near-certainty of the boundary crossing makes accessibility collapse near-unity. Resistance (0.08): Minimal. No organized effort opposes the constraint, because opposition would be futile. The low resistance reflects that the constraint is accepted universally as a fact rather than resisted as an imposition.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on Mountain classification. The mathematical economist sees logical necessity. The industrial producer sees invariant constraint across investment cycles. The development bank sees universal limit on all interventions. The subsistence farmer sees fixed boundary on labor productivity. There is no perspectival gap because the constraint has zero degrees of freedom — all observers, regardless of power or position, encounter the same mathematical relationship. This uniformity is the defining characteristic of a Mountain: the classification is independent of (Power, Time, Exit, Scope). This distinguishes DR from constraints like verification bottleneck (which exhibits six different types across perspectives) or regulatory capture (which exhibits different types for regulator vs. regulated industry).
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality computation is needed for Mountains. The constraint has no beneficiary and no victim. No agent extracts value from other agents through this constraint. The 'loss' is not transferred — it is simply unrealized output. The mathematical structure ensures that all agents experience the constraint identically, relative to their own production decisions. A farmer cannot exit by bribing another farmer to take the diminishing returns for them, because the constraint is not a transfer mechanism. It is a property of the production process itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_substitution_boundary,
    'At what point does technological innovation in complementary factors nullify the diminishing returns law for the original factor?',
    'Historical analysis of agricultural yields: comparison of marginal product curves before and after introduction of tractors, synthetic fertilizers, irrigation, and genetic improvement. Quantification of the shift in the production function.',
    'If substitution is continuous and unbounded: the law is a temporary constraint within each technological regime, not a universal mountain. If substitution has limits: the mountain classification holds universally. The distinction determines whether DR is a natural law or a regime-specific feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_substitution_boundary, empirical, 'Whether technological innovation permanently nullifies diminishing returns or merely shifts the boundary').

omega_variable(
    factor_complementarity_definition,
    'Is the ''ceteris paribus'' assumption (holding other factors constant) a description of physical reality or a modeling choice that obscures factor substitution?',
    'Philosophical analysis of production function specification; examination of cases where apparent ''other factors'' are actually substitutable (e.g., labor hours vs. machine hours, fertilizer vs. water). Identification of which factors are truly fixed vs. contractually held constant.',
    'If ceteris paribus is strictly physical (factors truly cannot be varied): mountain. If ceteris paribus is a modeling assumption: the law describes a artifact of the model, not a natural law. DR would shift to Tangled Rope (coordination of factor prices obscures substitution options).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(factor_complementarity_definition, conceptual, 'Whether ceteris paribus is a physical reality or a modeling assumption').

omega_variable(
    production_boundary_conditions,
    'Does diminishing returns persist at all scales and all production volumes, or does it emerge only in a specific midrange operating region?',
    'Production function analysis across full domain: examination of behavior at zero input, optimal scale, and extreme over-application (e.g., zero fertilizer = zero yield; doubling optimal fertilizer = declining yield; 100x fertilizer = toxicity/death). Identification of regions where relationship is linear, concave, or S-shaped.',
    'If diminishing returns is universal across all input levels: strong mountain. If limited to a specific operating range: constraint is a regime-specific feature, not a universal law. May suggest Scaffold classification (a temporary, bounded constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(production_boundary_conditions, empirical, 'Whether diminishing returns operates at all production scales or only in midrange').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(law_of_diminishing_returns, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(law_of_diminishing_returns, resource_allocation).
narrative_ontology:affects_constraint(law_of_diminishing_returns, optimal_factor_mix).
narrative_ontology:affects_constraint(law_of_diminishing_returns, scale_economies_vs_diseconomies).
narrative_ontology:affects_constraint(law_of_diminishing_returns, agricultural_productivity_limits).

% DUAL FORMULATION NOTE:
% Diminishing returns is a upstream constraint that affects all downstream productivity claims. Specific constraints about agricultural yields, manufacturing scale, or capital productivity operate within the boundary set by diminishing returns. The law does not cause those constraints; it establishes the mathematical framework within which they operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
