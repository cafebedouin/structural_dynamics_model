% ============================================================================
% CONSTRAINT STORY: cmr_001
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
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
 *   initiative aims to secure the supply chain for key industries, including
 *   defense, technology, and renewable energy. However, it also raises
 *   concerns about market distortion, trade retaliation, and the potential
 *   for inefficient resource allocation.
 *
 * KEY AGENTS:
 *   - US Mining Companies: Beneficiaries (institutional/arbitrage)
 *   - US Manufacturing Sector: Beneficiaries (institutional/constrained)
 *   - Chinese Mining Companies: Victims (moderate/constrained)
 *   - Non-US Allied Mining Companies: Victims (moderate/constrained)
 *   - US Taxpayers: Victims (powerless/trapped)
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
narrative_ontology:constraint_beneficiary(cmr_001, us_mining_companies).
narrative_ontology:constraint_beneficiary(cmr_001, us_manufacturing_sector).
narrative_ontology:constraint_victim(cmr_001, chinese_mining_companies).
narrative_ontology:constraint_victim(cmr_001, non_us_allied_mining_companies).
narrative_ontology:constraint_victim(cmr_001, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% US Taxpayers bear the cost of establishing and maintaining the reserve, with limited direct benefit or exit options.
constraint_indexing:constraint_classification(cmr_001, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% US Mining Companies benefit from government subsidies and reduced competition, enabling arbitrage opportunities.
constraint_indexing:constraint_classification(cmr_001, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer sees the reserve as a tangled rope, balancing strategic security with market distortion and potential inefficiency.
constraint_indexing:constraint_classification(cmr_001, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Chinese Mining Companies face restricted access to the US market and reduced global market share due to the reserve's influence.
constraint_indexing:constraint_classification(cmr_001, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% US Manufacturing sector benefits from a secure and stable supply of critical minerals at potentially lower prices.
constraint_indexing:constraint_classification(cmr_001, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

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
 *   Extractiveness (0.50): Moderate. The reserve extracts value from taxpayers and foreign competitors, while providing benefits to domestic industries. Suppression (0.40): Moderate. The reserve suppresses competition from foreign suppliers and may limit innovation in alternative materials.
 *
 * PERSPECTIVAL GAP:
 *   The US mining companies see the reserve as a rope, facilitating coordination and access to resources. Chinese mining companies perceive it as a snare, restricting their market access. US taxpayers bear the cost with limited direct benefit. The analytical observer sees the reserve as a tangled rope, balancing strategic security with market distortion.
 *
 * DIRECTIONALITY LOGIC:
 *   US mining companies benefit from subsidies and reduced competition, giving them arbitrage opportunities. Chinese mining companies face restricted market access. US taxpayers bear the cost of funding the reserve. The US manufacturing sector benefits from a more stable domestic supply of critical minerals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_size_impact,
    'What is the optimal size of the reserve to balance strategic security with market distortion?',
    'Economic modeling and geopolitical risk assessment',
    'Smaller reserve may not provide sufficient security; larger reserve may significantly distort markets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_size_impact, empirical, 'Optimal size of the critical minerals reserve').

omega_variable(
    substitution_potential,
    'How quickly can substitutes be developed for the targeted critical minerals?',
    'Materials science research and market analysis',
    'Faster substitution reduces the long-term value of the reserve; slower substitution increases its strategic importance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_potential, empirical, 'Potential for substitution of critical minerals').


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
narrative_ontology:affects_constraint(cmr_001, semiconductor_supply).
narrative_ontology:affects_constraint(cmr_001, rare_earth_dependency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
