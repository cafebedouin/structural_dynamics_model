% ============================================================================
% CONSTRAINT STORY: tail_risk_compression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tail_risk_compression, []).

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
 *   constraint_id: tail_risk_compression
 *   human_readable: The Volatility Suppression Trap
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Volatility Suppression Trap describes a scenario where systemic risks
 *   are artificially suppressed to maintain short-term stability, effectively
 *   'compressing' volatility into a hidden but catastrophic tail-end event.
 *   This is achieved through a combination of regulatory policies, market
 *   incentives, and behavioral biases that discourage risk assessment and
 *   encourage excessive leverage. The constraint creates a situation where
 *   short-term gains are prioritized over long-term stability, leading to an
 *   accumulation of systemic risk that is eventually unleashed in a sudden
 *   and devastating manner.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) - bear the long-term consequences of compressed tail risk.
 *   - Long-Term Investors: Secondary victim (moderate/constrained) - caught in a system that rewards short-term gains but exposes them to eventual tail risk.
 *   - Short-Term Investors: Primary beneficiary (institutional/arbitrage) - benefit from artificially suppressed volatility, allowing them to generate higher returns in the short run.
 *   - Political Incumbents: Beneficiary (powerful/mobile) - gain from appearing stable economy.
 *   - Regulatory Bodies: Complex actor (institutional/constrained) - may contribute to the problem through well-intentioned but flawed policies or capture.
 *   - Analytical Observer: (analytical/analytical) - sees the full picture of volatility suppression and the eventual catastrophic risk.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tail_risk_compression, 0.75).
domain_priors:suppression_score(tail_risk_compression, 0.8).
domain_priors:theater_ratio(tail_risk_compression, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tail_risk_compression, extractiveness, 0.75).
narrative_ontology:constraint_metric(tail_risk_compression, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(tail_risk_compression, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tail_risk_compression, snare).
narrative_ontology:human_readable(tail_risk_compression, "The Volatility Suppression Trap").
narrative_ontology:topic_domain(tail_risk_compression, "economic/technological").

domain_priors:requires_active_enforcement(tail_risk_compression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tail_risk_compression, short_term_investors).
narrative_ontology:constraint_beneficiary(tail_risk_compression, political_incumbents).
narrative_ontology:constraint_victim(tail_risk_compression, long_term_investors).
narrative_ontology:constraint_victim(tail_risk_compression, future_generations).
narrative_ontology:constraint_victim(tail_risk_compression, financial_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations bear the brunt of compressed tail risk when catastrophic events occur. They have no ability to exit or influence current decisions, making them completely vulnerable.
constraint_indexing:constraint_classification(tail_risk_compression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Long-term investors are caught in a system that rewards short-term gains but exposes them to the eventual tail risk. They are constrained by market forces and regulations, but can partially mitigate risk through diversification.
constraint_indexing:constraint_classification(tail_risk_compression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Short-term investors benefit from the artificially suppressed volatility, allowing them to generate higher returns in the short run. They can arbitrage by exiting the market before the tail risk materializes.
constraint_indexing:constraint_classification(tail_risk_compression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Regulatory bodies, while intended to ensure stability, can become complicit in suppressing volatility due to political pressures or flawed models. They are constrained by existing frameworks and often react slowly to emerging risks, leading to a piton-like state where their actions are more performative than functional.
constraint_indexing:constraint_classification(tail_risk_compression, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Political incumbents benefit from the appearance of stability and growth during their tenure, even if it comes at the expense of increased tail risk. They are powerful actors who can influence policies to maintain the status quo, but they are also mobile as they can leave office before the consequences become apparent. The increased extraction from the other agents is a benefit to them as their policies are favored.
constraint_indexing:constraint_classification(tail_risk_compression, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer sees the full picture of volatility suppression and the eventual catastrophic risk. They understand the incentives driving the behavior and the long-term consequences for the system.
constraint_indexing:constraint_classification(tail_risk_compression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tail_risk_compression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tail_risk_compression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tail_risk_compression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tail_risk_compression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tail_risk_compression, TR),
    TR >= 0.70.

:- end_tests(tail_risk_compression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The system extracts wealth from long-term stability and transfers it to short-term gains. Future generations are forced to bear the costs of current actions. Suppression (0.80): High. Active suppression of volatility hides indicators. Models may be flawed and provide a false sense of security. Political forces may also hide risks from the public. Theater Ratio (0.75): High. While regulatory bodies may claim to be reducing risk, the suppression and extraction levels indicate that the bodies only make small impacts, and that they are mostly performative. Short term stability for votes is favored over a complex plan.
 *
 * PERSPECTIVAL GAP:
 *   The main perspectival gap arises from differing time horizons and exit options. Short-term investors and political incumbents benefit from the illusion of stability, while long-term investors and future generations bear the costs of the eventual collapse. Regulatory bodies may genuinely believe they are reducing risk, but their actions can inadvertently exacerbate the problem. The analytical observer can see the entire system and recognize the inherent instability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the ability to benefit from or avoid the eventual tail risk event. Short-term investors have arbitrage options and can exit before the collapse. Future generations have no exit options and bear the full cost. Long-term investors are constrained, while regulatory bodies are mobile because they can shift between being genuine and performative.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandated trophy is resolved by the stark contrast between short-term gains and long-term costs. While short-term investors may see the situation as a rope or scaffold (coordination), the perspective of future generations reveals it to be a snare (extraction). The key is to recognize that the apparent stability is an illusion created by the active suppression of volatility, which is fundamentally unsustainable. The analytical observer sees the long term effect, validating the extraction from the powerless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systemic_risk_threshold,
    'What level of interconnectedness and leverage creates a tipping point for systemic risk?',
    'Network analysis of financial institutions and stress testing of their balance sheets.',
    'If threshold is too low: overregulation stifles economic growth. If threshold is too high: financial system is vulnerable to collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_risk_threshold, empirical, 'The level of systemic risk that triggers a catastrophic event.').

omega_variable(
    regulatory_capture_influence,
    'To what extent are regulatory bodies influenced by the industries they regulate?',
    'Analysis of lobbying efforts, campaign contributions, and revolving door employment between regulators and industry.',
    'If high influence: regulatory bodies are ineffective at preventing tail risk. If low influence: regulatory bodies can effectively mitigate tail risk, but there will be higher suppression. This would create an environment for active regulatory innovation to occur, and new methods that can effectively mitigate the risk and provide utility to the population as a whole can emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_influence, empirical, 'The degree to which regulatory bodies are captured.').

omega_variable(
    behavioral_bias_persistence,
    'How long does the belief in sustained low volatility persist among market participants?',
    'Surveys of investor sentiment, analysis of trading patterns, and tracking of risk premiums.',
    'If long persistence: tail risk is further compressed due to increased leverage and complacency. If short persistence: market self-corrects before tail risk becomes catastrophic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_bias_persistence, empirical, 'How long market participants underestimate risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tail_risk_compression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tail_tr_t0, tail_risk_compression, theater_ratio, 0, 0.3).
narrative_ontology:measurement(tail_tr_t5, tail_risk_compression, theater_ratio, 5, 0.7).
narrative_ontology:measurement(tail_tr_t10, tail_risk_compression, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(tail_be_t0, tail_risk_compression, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(tail_be_t5, tail_risk_compression, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(tail_be_t10, tail_risk_compression, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tail_risk_compression, enforcement_mechanism).
narrative_ontology:affects_constraint(tail_risk_compression, moral_hazard_banking).
narrative_ontology:affects_constraint(tail_risk_compression, too_big_to_fail).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
