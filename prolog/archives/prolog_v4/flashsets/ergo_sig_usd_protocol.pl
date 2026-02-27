% ============================================================================
% CONSTRAINT STORY: ergo_sig_usd_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_sig_usd_protocol, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergo_sig_usd_protocol
 *   human_readable: SigUSD Stability Mechanism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   SigUSD is an algorithmic stablecoin on the Ergo blockchain, designed to
 *   maintain a value of $1 USD through a reserve-backed system. Ergo holders
 *   can provide ERG to the reserve in exchange for SigUSD, and vice versa.
 *   The system relies on oracles to determine the ERG/USD exchange rate. If
 *   the reserve ratio falls below a certain threshold, minting of new SigUSD
 *   is halted, and arbitrageurs are incentivized to restore the balance by
 *   providing ERG or redeeming SigUSD. This protocol has multiple impacts on
 *   different agents within the Ergo ecosystem. The potential for oracle
 *   manipulation is a central risk.
 *
 * KEY AGENTS:
 *   - SigUSD Holders (Late Adopters): Powerless/Trapped - Those who bought SigUSD at high prices and could suffer significant losses during a depeg.
 *   - SigUSD Stabilizers (ERGO Holders): Moderate/Constrained - Those who arbitrage SigUSD to maintain peg.
 *   - Ergo Miners: Institutional/Arbitrage - Miners benefit from the increased activity on Ergo blockchain.
 *   - Ergo Holders (Early Adopters): Powerful/Mobile - Early adopters who are more resilient to price fluctuations.
 *   - Systemic Stability of Ergo: Powerless/Trapped - The Ergo ecosystem could be compromised with a SigUSD catastrophic failure.
 *   - The Analytical Observer: Analytical/Analytical - Can see and analyze the full structural properties of the protocol.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_sig_usd_protocol, 0.55).
domain_priors:suppression_score(ergo_sig_usd_protocol, 0.45).
domain_priors:theater_ratio(ergo_sig_usd_protocol, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_sig_usd_protocol, extractiveness, 0.55).
narrative_ontology:constraint_metric(ergo_sig_usd_protocol, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ergo_sig_usd_protocol, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_sig_usd_protocol, tangled_rope).
narrative_ontology:human_readable(ergo_sig_usd_protocol, "SigUSD Stability Mechanism").
narrative_ontology:topic_domain(ergo_sig_usd_protocol, "economic/technological").

domain_priors:requires_active_enforcement(ergo_sig_usd_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_sig_usd_protocol, ergo_miners).
narrative_ontology:constraint_beneficiary(ergo_sig_usd_protocol, ergo_holders_early_adopters).
narrative_ontology:constraint_victim(ergo_sig_usd_protocol, sigusd_holders_late_adopters).
narrative_ontology:constraint_victim(ergo_sig_usd_protocol, systemic_stability_of_ergo).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: SigUSD holders who bought in near a peak and are now underwater (powerless/trapped). They are trapped because exiting would realize losses and they may believe in a recovery. They bear the cost of any instability in the system. High extraction.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Ergo holders who participate in stabilizing the SigUSD price through arbitrage and providing liquidity (moderate/constrained). They have some ability to exit, but doing so impacts their returns. They both benefit from the stability mechanism and bear some risk. Medium extraction.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective 3: Ergo miners who earn transaction fees and block rewards related to SigUSD transactions (institutional/arbitrage). They are beneficiaries because the stablecoin increases demand for Ergo and its blockchain. Low extraction, primarily coordination.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Perspective 4: Ergo holders who adopted Ergo early on and benefit from the increased value and stability brought by SigUSD. They have the power to influence the system's governance and development. They benefit significantly but also risk reputational damage if the system fails. This is why they are considered powerful and mobile. Low extraction from their perspective.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective 5: The systemic stability of the Ergo blockchain itself (powerless/trapped). If SigUSD fails catastrophically, it could damage the entire Ergo ecosystem's reputation and long-term viability. This is a trapped situation because the blockchain cannot simply exit or reorganize. High extraction.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Perspective 6: Analytical observer analyzing the long-term economic impact of the SigUSD protocol (analytical/analytical). The observer sees the mixed benefits and risks to various participants and the broader Ergo ecosystem. The observer also accounts for the possible network effects to make the type tangled_rope.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_sig_usd_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_sig_usd_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_sig_usd_protocol, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_sig_usd_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ergo_sig_usd_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The system extracts value from SigUSD holders if it fails to maintain its peg. Late adopters are especially vulnerable. Suppression (0.45): Users face some barriers to exit during extreme market conditions due to slippage and potential network congestion. However, arbitrageurs are incentivized to maintain the peg, mitigating some of the suppression. Theater Ratio (0.30): The bonding curve mechanism and oracle reliance introduce a degree of performative action, but it is primarily functional. The protocol has seen usage, but there have been volatility events.
 *
 * PERSPECTIVAL GAP:
 *   Different participants experience the SigUSD protocol differently. Late adopters may perceive it as a snare, while early adopters see it as a rope. The analytical observer recognizes the tangled rope nature due to the coordination mechanism and potential extractive properties. These are legitimate perspectives on the complex system.
 *
 * DIRECTIONALITY LOGIC:
 *   SigUSD holders bear the cost of instability (high directionality), while Ergo miners benefit from increased transaction volume (low directionality). Early adopters benefit from increased adoption. The core stability mechanism extracts value from late adopters and provides stability benefits to miners.
 *
 * MANDATROPHY ANALYSIS:
 *   The protocol's classification as Tangled Rope resolves the Mandatrophy problem by recognizing the coordination aspect (providing a stablecoin) and the potential extractive element if the system depegs. It's not a pure coordination mechanism (Rope) or pure extraction (Snare), but a hybrid that depends on the participants position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_manipulation_risk,
    'How vulnerable are the price oracles used by the protocol to manipulation?',
    'Rigorous security audits of the oracle implementations, stress testing under various market conditions, and diversification of oracle sources.',
    'High vulnerability: potential for catastrophic depegging and loss of funds. Low vulnerability: increased confidence in system stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_manipulation_risk, empirical, 'Risk of price oracle manipulation').

omega_variable(
    black_swan_resilience,
    'How well does the protocol handle extreme market volatility (''black swan'' events)?',
    'Simulations using historical market data, analysis of the protocol''s bonding curve mechanics, and emergency shutdown procedures.',
    'Low resilience: potential for runaway inflation or collapse of the stablecoin. High resilience: increased confidence in long-term viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_swan_resilience, empirical, 'Resilience to extreme market events').

omega_variable(
    bonding_curve_parameters,
    'Are the parameters of the bonding curve (used for minting and redeeming SigUSD) optimally tuned?',
    'Economic modeling and simulations to determine the optimal parameters for different market conditions.',
    'Suboptimal parameters: potential for inefficient capital utilization or instability. Optimal parameters: improved stability and capital efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bonding_curve_parameters, conceptual, 'Optimization of bonding curve parameters').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_sig_usd_protocol, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_tr_t0, ergo_sig_usd_protocol, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ergo_tr_t6, ergo_sig_usd_protocol, theater_ratio, 6, 0.2).
narrative_ontology:measurement(ergo_tr_t12, ergo_sig_usd_protocol, theater_ratio, 12, 0.3).

% Extraction over time
narrative_ontology:measurement(ergo_be_t0, ergo_sig_usd_protocol, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ergo_be_t6, ergo_sig_usd_protocol, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(ergo_be_t12, ergo_sig_usd_protocol, base_extractiveness, 12, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_sig_usd_protocol, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
