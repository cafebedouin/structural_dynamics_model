% ============================================================================
% CONSTRAINT STORY: tail_risk_compression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
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
 *   The Volatility Suppression Trap represents a systemic economic constraint
 *   where repeated interventions to stabilize markets create a hidden but
 *   growing tail-risk burden. Central banks and financial regulators have
 *   deployed quantitative easing, repo market backstops, circuit breakers,
 *   and emergency lending facilities to prevent market volatility spikes.
 *   These tools succeed in their immediate goal: volatility indices remain
 *   suppressed, panic cascades are averted, and the financial system appears
 *   stable. However, the constraint operates through a structural mechanism:
 *   suppressing volatility does not eliminate it; it compresses it into
 *   larger, less frequent tail-end events. As time in the suppression regime
 *   extends, the accumulated hidden risk grows, while market participants
 *   rationally adjust expectations to match observed (suppressed) volatility.
 *   This creates a trap: investors remove hedges, institutions reduce
 *   reserves, and volatility expectations recalibrate downward. When the
 *   regime inevitably transitions—whether through policy choice, unexpected
 *   shock, or crowded positioning—the full magnitude of accumulated tail risk
 *   reappears suddenly. The victims are those without information about or
 *   control over the regime transition (retail investors, unhedged
 *   counterparties, future market participants), while the immediate
 *   beneficiaries are those who captured the low-volatility era without
 *   bearing its tail-end costs. The constraint exhibits all six
 *   classification types depending on perspective, making it a critical case
 *   study for how indexical classification disambiguates seemingly
 *   contradictory narratives about systemic risk.
 *
 * KEY AGENTS:
 *   - Central Banks / Regulatory Stabilizers: Primary architects (institutional/arbitrage) — deploy suppression tools, capture information rent about regime transitions, but become structurally dependent on continued intervention
 *   - Retail Investors: Primary victims (powerless/trapped) — locked into normalized volatility expectations, cannot exit without losses, rebalance portfolios based on suppressed regime
 *   - Unhedged Counterparties: Secondary victims (moderate/constrained) — removed hedges due to years of low volatility, face massive repricing costs if regime shifts, constrained by reputation and capital requirements
 *   - Financial Institutions / Large Asset Managers: Ambiguous position (institutional/arbitrage to constrained) — both beneficiaries (can reduce risk capital in low-volatility regime, concentrate on high-probability trades) and trapped victims (depend on continued low rates and vol suppression for profitability)
 *   - Systemic Financial Stability Coalition: Organized network (organized/constrained) — regulators, central banks, large institutions coordinating volatility management; benefits from stability coordination but extracts information rent
 *   - Future Market Participants: Absent victims (powerless/trapped) — not yet present in markets, will inherit the regime transition and tail-risk realization without having participated in suppression benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tail_risk_compression, 0.58).
domain_priors:suppression_score(tail_risk_compression, 0.72).
domain_priors:theater_ratio(tail_risk_compression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tail_risk_compression, extractiveness, 0.58).
narrative_ontology:constraint_metric(tail_risk_compression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tail_risk_compression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tail_risk_compression, snare).
narrative_ontology:human_readable(tail_risk_compression, "The Volatility Suppression Trap").
narrative_ontology:topic_domain(tail_risk_compression, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_victim(tail_risk_compression, retail_investors).
narrative_ontology:constraint_victim(tail_risk_compression, unhedged_counterparties).
narrative_ontology:constraint_victim(tail_risk_compression, future_market_participants).
narrative_ontology:constraint_victim(tail_risk_compression, systemic_financial_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Locked into normalized volatility expectations by years of suppressed tail risk. Cannot exit markets without realizing losses or abandoning retirement savings. Believes volatility has structurally declined due to market efficiency. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.95.
constraint_indexing:constraint_classification(tail_risk_compression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNHEDGED COUNTERPARTY (SNARE) — Financial institutions, pension funds, and corporate treasuries that removed hedges due to years of volatility suppression. Cannot suddenly re-hedge without massive costs and reputational damage from admitting prior risk misjudgment. d≈0.85, f(d)≈1.18, σ=1.2 → χ≈0.81.
constraint_indexing:constraint_classification(tail_risk_compression, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CENTRAL BANK / REGULATORY STABILIZER (SNARE WITH INVERSION) — Primary architect of volatility suppression through repeated interventions (quantitative easing, repo market support, circuit breakers, emergency lending). Experiences the constraint as beneficiary with arbitrage exit (can pause interventions, shift policy), but classification remains Snare because the institution itself becomes trapped by market dependency on continued suppression. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01 as direct beneficiary, but the deeper structural analysis reveals institutional entrapment: cannot exit intervention without triggering the very instability it suppressed.
constraint_indexing:constraint_classification(tail_risk_compression, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SYSTEMIC STABILITY COALITION (TANGLED ROPE) — Policymakers, regulators, and financial institutions collectively benefit from the coordination function of volatility suppression (prevents panic cascades, maintains lending, enables long-term planning). But the mechanism has asymmetric extraction costs: retail investors and unhedged players bear tail-end losses while suppressors gain information rent (knowing regime change timing before public). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(tail_risk_compression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL RISK MANAGEMENT ESTABLISHMENT (PITON) — Value-at-Risk models, stress testing protocols, and Basel framework metrics all encode assumptions about volatility distributions calibrated to suppressed-regime data. These models are performative risk measures: they correctly predict risk within the suppression regime but fail catastrophically when the regime collapses. theater_ratio≈0.65 reflects that risk management ritual (quarterly stress tests, VaR calculations, risk committees) provides false security. Persists due to regulatory inertia and lack of better alternatives, despite known methodological failures.
constraint_indexing:constraint_classification(tail_risk_compression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a mathematical/physical perspective on stochastic processes, tail events are irreducible features of nonlinear systems. You cannot indefinitely suppress downside tail probability without it accumulating; it is a conservation law for risk in complex systems. Volatility suppression at t=0 to t=T mathematically guarantees larger volatility at t>T. This perspective risks false naturalization: the structural constraint (ε=0.58, suppression=0.72) is a policy choice, not a physical law. The engine will compute this as a false summit, revealing that 'volatility is conserved' conflates a mathematical property of unsuppressed systems with the claim that suppression itself is natural/inevitable.
constraint_indexing:constraint_classification(tail_risk_compression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tail_risk_compression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tail_risk_compression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tail_risk_compression, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The suppression regime extracts from retail and unhedged players through information asymmetry (insiders know regime fragility), portfolio rebalancing (victims reduce hedges, increase leverage), and regime transition timing (insiders can position for transition, outsiders cannot). Not as extreme as pure fraud (0.75+) because volatility suppression does provide real coordination benefits: prevents panic cascades, enables long-term planning, maintains credit flow. The extraction is structural, not intentional. Suppression (0.72): High. Multiple barriers prevent exit: psychological (normalized volatility expectations), institutional (regulations penalizing frequent hedging changes), economic (hedging costs accumulate in low-volatility regime, making removal rational), and informational (asymmetric knowledge about regime fragility). Retail investors cannot exit without abandoning retirement security. Institutions cannot exit without admitting prior risk misjudgment. Central banks cannot exit without triggering the very instability they suppressed. Theater ratio (0.65): Moderate-high. Risk management practices (VaR modeling, stress testing, risk committees) become performative in suppressed regimes. Models calibrated to suppressed data predict risk correctly within that regime but fail catastrophically outside it. The theater has increased over the 20-year interval as risk managers have grown confident in low-volatility normality, making their models increasingly theatrical rather than predictive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates fundamental perspectival divergence. Central banks experience volatility suppression as successful coordination and stabilization (Rope or Tangled Rope from their position with arbitrage exits and information advantages). Retail investors experience the same suppression as a trap (Snare). The systemic stability coalition sees coordination benefits (Tangled Rope). The unhedged counterparties see mixed extraction and coordination (Tangled Rope). Risk management establishments see performative safety (Piton). The analytical observer risks naturalizing the trap as an irreducible property of markets (false Mountain). The largest perspectival gap is between stabilizers (who can potentially exit through policy change) and victims (who cannot exit without catastrophic portfolio losses). The suppression that stabilizers experience as a policy tool appears to victims as an immutable constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail investors: Victim + trapped → d≈0.92, f(d)≈1.40. Maximal extraction. Cannot exit volatility suppression regime without abandoning long-term wealth accumulation; rebalance portfolios rationally into the trap. Unhedged counterparties: Victim + constrained → d≈0.85, f(d)≈1.18. High extraction. Can theoretically re-hedge, but costs are prohibitive after years of suppressed volatility; reputation and shareholder pressure prevent sudden defensive moves. Stabilizers: Beneficiary + arbitrage nominally → d≈0.15, f(d)≈-0.01, but deeper analysis reveals institutional entrapment — cannot exit suppression without destroying the stability they built. Structural d is actually higher (≈0.35-0.45) when accounting for institutional inertia and policy credibility constraints. Systemic stability coalition: Both beneficiary (stabilization coordination) and partial victim (trapped by dependency) → d≈0.50, f(d)≈0.65. Mixed position. Unhedged counterparties benefit from the coordination aspect but suffer extraction through reduced reserves and hedging. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; the engine detects as false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (χ=0.58 < 0.70, so mandatrophy_resolved not required, but analysis provided): The volatility suppression trap resolves the mandatrophy by demonstrating that Snare classification is correct despite surface-level coordination benefits. The temptation is to classify as Tangled Rope: 'Suppression provides coordination benefit (prevents panic cascades) AND extracts from victims (redistributes tail risk). Therefore, hybrid Tangled Rope.' This conflates two structural features: (1) the genuine coordination benefit of preventing panic cascades, and (2) the extraction mechanism of compressing tail risk. However, the Snare classification is more accurate because: (a) the coordination function (crisis prevention) is NOT achieved through extraction; it is achieved through temporary suppression. (b) The extraction (accumulated tail risk) is NOT necessary for coordination; it is a side effect of the suppression mechanism itself. (c) The beneficiaries of suppression (stabilizers, short-term investors) do NOT depend on continued extraction of tail risk; they benefit from the low-volatility environment, and would be happier if tail risk could be eliminated rather than compressed. Therefore, the Snare classification applies because the extraction (tail-risk compression) does NOT serve a coordination function — it is a byproduct of a mechanism that serves coordination but extracts as a side effect. This is distinct from Tangled Rope, where extraction is integral to the coordination mechanism. If volatility could be stabilized without compressing tail risk (e.g., through better information flow, improved circuit breakers that prevent cascades without creating hidden tails), the system would still maintain coordination benefits but lose the extraction component. The fact that removal of the extraction mechanism would preserve the coordination benefit indicates the extraction is parasitic, not symbiotic — confirming Snare rather than Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_efficacy_threshold,
    'What proportion of tail-end risk can be suppressed before the accumulated hidden risk creates catastrophic feedback loops?',
    'Historical analysis of volatility regimes; measurement of actual tail-risk accumulation vs suppression intensity; network stress testing of counterparty exposure',
    'If threshold < 40% suppressed: current regime is sustainable. If threshold > 70% suppressed: regime collapse is inevitable within 5-10 years. Classification shifts from Snare (temporary extraction) to Snare (inevitable default).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_efficacy_threshold, empirical, 'Threshold at which accumulated tail risk creates unstoppable feedback').

omega_variable(
    policy_circuit_breaker_credibility,
    'Can central banks actually execute a graceful volatility regime transition without triggering the panic cascade they are trying to prevent?',
    'Game-theoretic analysis of commitment devices; historical precedent for successful monetary policy regime shifts; measurement of market expectations for policy credibility',
    'If credible: stabilizers have genuine arbitrage exit. If not credible: stabilizers are also trapped (central institution moves from Snare beneficiary to Snare victim). Transforms the classification from ''stabilizers suppress; victims trapped'' to ''everyone trapped.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_circuit_breaker_credibility, conceptual, 'Whether policy can execute graceful regime transition without panic').

omega_variable(
    information_asymmetry_persistence,
    'Do insiders (central banks, large institutions, derivatives traders) actually have predictive information about when suppression will fail, or do they face the same uncertainty as retail investors?',
    'Analysis of insider trading patterns around volatility spikes; correlation between central bank communication timing and market regime changes; examination of derivatives positioning before crises',
    'If insiders have information: suppression is pure extraction (Snare confirmed). If insiders lack information: suppression is coordination failure with symmetric uncertainty (Rope or Tangled Rope). Changes directionality for stabilizers from low d to moderate d.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Whether insiders have predictive information about regime collapse').

omega_variable(
    nonlinear_tail_accumulation,
    'Does tail risk accumulate linearly with suppression intensity, or does it exhibit nonlinear (threshold or exponential) growth?',
    'Mathematical modeling of volatility processes under suppression regimes; empirical measurement of tail-end VaR vs time-in-suppression; analysis of past regime-change events',
    'If linear: trajectory is predictable and gradual (Piton degradation model fits). If nonlinear: critical transition is sudden (Snare classification is correct; Piton is dangerous misclassification). Affects measurement trajectories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nonlinear_tail_accumulation, empirical, 'Growth dynamics of compressed tail risk over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tail_risk_compression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tailrisk_tr_t0, tail_risk_compression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tailrisk_tr_t10, tail_risk_compression, theater_ratio, 10, 0.5).
narrative_ontology:measurement(tailrisk_tr_t20, tail_risk_compression, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(tailrisk_be_t0, tail_risk_compression, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tailrisk_be_t10, tail_risk_compression, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(tailrisk_be_t20, tail_risk_compression, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tail_risk_compression, enforcement_mechanism).
narrative_ontology:affects_constraint(tail_risk_compression, liquidity_mirage).
narrative_ontology:affects_constraint(tail_risk_compression, zero_rate_floor_trap).
narrative_ontology:affects_constraint(tail_risk_compression, systemic_leverage_accumulation).

% DUAL FORMULATION NOTE:
% The volatility suppression trap is downstream of monetary policy choices (quantitative easing, repo backstops) and upstream of leverage accumulation in asset markets. Separate constraint stories exist for the policy mechanisms (zero_rate_floor_trap, ε≈0.35) and the market-level leverage consequences (systemic_leverage_accumulation, ε≈0.52). The suppression trap (ε=0.58) represents the emergent constraint at the system level where all three mechanisms interact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tail_risk_compression, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
