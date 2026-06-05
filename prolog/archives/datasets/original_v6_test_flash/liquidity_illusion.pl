% ============================================================================
% CONSTRAINT STORY: liquidity_illusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liquidity_illusion, []).

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
 *   constraint_id: liquidity_illusion
 *   human_readable: The Exit Door Mirage
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Exit Door Mirage describes a situation where a financial or digital
 *   market creates the appearance of high liquidity during normal times,
 *   enticing participants with easy entry and exit. However, during periods
 *   of stress or market downturns, this liquidity vanishes, leaving many
 *   trapped. This illusion is created and maintained by a combination of
 *   factors, including market maker incentives, regulatory oversight
 *   failures, and investor behavior.
 *
 * KEY AGENTS:
 *   - Market Makers: Primary beneficiary (institutional/arbitrage) - profit from providing liquidity and often have early access to information.
 *   - Early Investors: Beneficiary (powerful/mobile) - can exit before the liquidity dries up.
 *   - Late Entrants: Primary victim (powerless/trapped) - enter the market believing the liquidity promise, only to find themselves trapped during downturns.
 *   - Retail Investors: Secondary victim (moderate/constrained) - often lack sophisticated tools and knowledge, making them susceptible to the illusion.
 *   - Regulators: Observer (institutional/constrained) - attempt to manage the risks associated with the illusion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liquidity_illusion, 0.55).
domain_priors:suppression_score(liquidity_illusion, 0.45).
domain_priors:theater_ratio(liquidity_illusion, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liquidity_illusion, extractiveness, 0.55).
narrative_ontology:constraint_metric(liquidity_illusion, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(liquidity_illusion, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liquidity_illusion, tangled_rope).
narrative_ontology:human_readable(liquidity_illusion, "The Exit Door Mirage").
narrative_ontology:topic_domain(liquidity_illusion, "economic/technological").

domain_priors:requires_active_enforcement(liquidity_illusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liquidity_illusion, market_makers).
narrative_ontology:constraint_beneficiary(liquidity_illusion, early_investors).
narrative_ontology:constraint_victim(liquidity_illusion, late_entrants).
narrative_ontology:constraint_victim(liquidity_illusion, retail_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of retail investors who enter the market near its peak, believing in the high liquidity promised. When a downturn occurs, they find themselves trapped with depreciating assets and limited exit options. They experience the full force of the mirage.
constraint_indexing:constraint_classification(liquidity_illusion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective of institutional investors who are large enough to move markets but also have fiduciary duties to manage risk. They are constrained by the need to balance rapid exit with market impact. They benefit from superior information but still bear costs during rapid downturns.
constraint_indexing:constraint_classification(liquidity_illusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of market makers who profit from providing liquidity. In normal market conditions, they benefit from the spread between bid and ask prices, making them beneficiaries of the liquidity illusion. Their ability to arbitrage allows them to benefit from imbalances.
constraint_indexing:constraint_classification(liquidity_illusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of an analytical observer who understands the structural incentives and feedback loops that create the illusion. They recognize the inherent instability and potential for cascading failures. The system appears as a tangled rope, balancing coordination (liquidity provision) with extraction (advantage for informed traders).
constraint_indexing:constraint_classification(liquidity_illusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liquidity_illusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liquidity_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liquidity_illusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liquidity_illusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liquidity_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55. The illusion extracts value from late entrants and retail investors, transferring it to early investors and market makers. Suppression: 0.45. The illusion suppresses alternative investment strategies by promoting the belief in constant liquidity and easy exit. Theater Ratio: 0.30. While there are regulatory efforts to monitor liquidity, they are not always effective in preventing the mirage from forming.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap arises because market makers and early investors genuinely experience high liquidity as a benefit, while late entrants and retail investors only realize the illusion when it is too late. The analytical observer sees the systemic risks and potential for cascading failures that these different perspectives miss.
 *
 * DIRECTIONALITY LOGIC:
 *   Market makers benefit from providing liquidity in normal times and often have the ability to exit before the liquidity vanishes. Retail investors, on the other hand, are often trapped with depreciating assets and limited exit options. The directional derivation reflects this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that it contains elements of both coordination and extraction. The market makers provide a genuine coordination function by offering liquidity, but this function also facilitates extraction from less informed investors. The tangled rope classification captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_identification,
    'What specific indicators reliably signal the transition from high liquidity to a liquidity crunch?',
    'Statistical analysis of market microstructure data, including bid-ask spreads, order book depth, and volatility clustering.',
    'Improved early warning systems for systemic risk; better calibration of regulatory interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_identification, empirical, 'Identification of liquidity tipping points.').

omega_variable(
    market_maker_incentives,
    'How can market maker incentives be aligned with overall market stability to discourage the creation or maintenance of liquidity illusions?',
    'Agent-based modeling of market dynamics under different regulatory regimes; empirical studies of the impact of market maker obligations on liquidity provision during stress events.',
    'More robust market infrastructure; reduced probability of liquidity cascades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_maker_incentives, preference, 'Incentive alignment for market makers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liquidity_illusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liqu_tr_t0, liquidity_illusion, theater_ratio, 0, 0.1).
narrative_ontology:measurement(liqu_tr_t5, liquidity_illusion, theater_ratio, 5, 0.2).
narrative_ontology:measurement(liqu_tr_t10, liquidity_illusion, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(liqu_be_t0, liquidity_illusion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liqu_be_t5, liquidity_illusion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(liqu_be_t10, liquidity_illusion, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liquidity_illusion, resource_allocation).
narrative_ontology:affects_constraint(liquidity_illusion, moral_hazard).
narrative_ontology:affects_constraint(liquidity_illusion, systemic_risk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
