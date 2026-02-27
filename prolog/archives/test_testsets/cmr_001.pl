% ============================================================================
% CONSTRAINT STORY: cmr_001
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cmr_001, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cmr_001
 *   human_readable: Critical Minerals Reserve
 *   domain: economic
 *
 * SUMMARY:
 *   The US government establishes a $12 billion critical minerals reserve to
 *   reduce reliance on China and other nations for strategic resources. This
 *   initiative aims to bolster domestic mining capabilities, secure supply
 *   chains, and mitigate economic vulnerabilities. The reserve is a strategic
 *   tool to enhance national security and economic resilience.
 *
 * KEY AGENTS:
 *   - US Government: Primary beneficiary (institutional/arbitrage) – gains control over mineral supplies.
 *   - Domestic Mining Companies: Secondary beneficiary (powerful/mobile) – benefit from government contracts and demand.
 *   - Taxpayers: Primary victim (powerless/trapped) – bear the financial burden of the reserve.
 *   - Foreign Mineral Suppliers: Secondary victim (moderate/constrained) – face reduced demand and market share.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cmr_001, 0.5).
domain_priors:suppression_score(cmr_001, 0.4).
domain_priors:theater_ratio(cmr_001, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cmr_001, extractiveness, 0.5).
narrative_ontology:constraint_metric(cmr_001, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(cmr_001, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cmr_001, tangled_rope).
narrative_ontology:human_readable(cmr_001, "Critical Minerals Reserve").
narrative_ontology:topic_domain(cmr_001, "economic").

domain_priors:requires_active_enforcement(cmr_001).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cmr_001, domestic_mining_companies).
narrative_ontology:constraint_beneficiary(cmr_001, us_government).
narrative_ontology:constraint_victim(cmr_001, foreign_mineral_suppliers).
narrative_ontology:constraint_victim(cmr_001, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Taxpayers bear the cost of the reserve, with limited direct benefit. The funds are allocated, and they cannot easily exit the tax system. d = 0.95
constraint_indexing:constraint_classification(cmr_001, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The US government benefits from the reserve, gaining greater control over critical mineral supplies and reducing reliance on foreign nations. Exit through policy change.
constraint_indexing:constraint_classification(cmr_001, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical perspective sees the reserve as a tangled rope, balancing the benefits of reduced foreign reliance with the costs to taxpayers and potential distortions in the mineral market. Long-term benefits are uncertain.
constraint_indexing:constraint_classification(cmr_001, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Domestic mining companies benefit from government contracts and increased demand, but are also constrained by environmental regulations and market volatility.
constraint_indexing:constraint_classification(cmr_001, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Foreign mineral suppliers, particularly those from nations the US seeks to reduce reliance on, are negatively impacted by the reserve, facing reduced demand and market share.
constraint_indexing:constraint_classification(cmr_001, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cmr_001_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cmr_001, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cmr_001, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cmr_001, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cmr_001_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.50 – Moderate extraction as taxpayers fund the reserve with potential benefits to domestic mining companies. Suppression: 0.40 – Moderate suppression due to reduced reliance on foreign suppliers. Theater ratio: 0.20 – Low theater as the reserve is primarily a functional economic policy.
 *
 * PERSPECTIVAL GAP:
 *   The government sees it as a necessary strategic move (rope), while taxpayers may view it as a costly burden (snare). Domestic mining companies see opportunity (tangled_rope), while foreign suppliers perceive reduced market access (snare).
 *
 * DIRECTIONALITY LOGIC:
 *   US Government: Beneficiary + arbitrage → d ≈ 0.05. Domestic Mining Companies: Beneficiary + mobile → d ≈ 0.15. Taxpayers: Victim + trapped → d ≈ 0.95. Foreign Mineral Suppliers: Victim + constrained → d ≈ 0.85.
 *
 * MANDATROPHY ANALYSIS:
 *   The reserve is primarily a strategic economic policy, designed to shift dependence away from foreign nations that the US has identified as having conflicting interests. This goal is achieved through protectionist measures. While the intent is understandable, the long term effects of these actions are more easily described and classified as a tangled rope than as a type of rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_distortion,
    'To what extent does the reserve distort the global market for critical minerals?',
    'Economic analysis of price fluctuations and supply chain shifts.',
    'Significant distortion could lead to retaliatory measures and reduced global supply chain efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_distortion, empirical, 'The impact of the reserve on global mineral markets').

omega_variable(
    strategic_effectiveness,
    'How effectively does the reserve reduce US vulnerability to supply chain disruptions?',
    'Scenario planning and simulations of potential disruptions.',
    'Low effectiveness would undermine the primary rationale for the reserve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_effectiveness, empirical, 'The effectiveness of the reserve in mitigating supply chain risks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cmr_001, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmr__tr_t0, cmr_001, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cmr__tr_t5, cmr_001, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cmr__tr_t10, cmr_001, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(cmr__be_t0, cmr_001, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cmr__be_t5, cmr_001, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cmr__be_t10, cmr_001, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cmr_001, resource_allocation).
narrative_ontology:affects_constraint(cmr_001, trade_agreements).
narrative_ontology:affects_constraint(cmr_001, geopolitical_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
