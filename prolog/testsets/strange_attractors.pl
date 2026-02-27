% ============================================================================
% CONSTRAINT STORY: strange_attractors
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strange_attractors, []).

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
 *   constraint_id: strange_attractors
 *   human_readable: Systemic Risk Amplification via Strange Attractor Dynamics
 *   domain: economic/financial_stability
 *
 * SUMMARY:
 *   Systemic risk amplification via strange attractor dynamics describes the
 *   emergence of nonlinear market regimes where feedback loops between
 *   leverage, collateral availability, counterparty interconnection, and
 *   behavioral herding create self-reinforcing volatility patterns. These
 *   attractors concentrate risk in predictable but complex ways, enabling
 *   high-frequency traders and systemically important institutions (SIIs) to
 *   profit from the dynamics while extracting wealth from retail investors
 *   and small businesses trapped in the system. The constraint exhibits
 *   classical tangled rope structure: it provides genuine coordination
 *   benefits (liquidity provision, price discovery) while simultaneously
 *   enabling asymmetric extraction through information advantage and
 *   institutional arbitrage capacity. The extractiveness has increased over
 *   the interval (0.35 → 0.58) as financial engineering has become more
 *   sophisticated in exploiting attractor structure, while regulatory theater
 *   (0.42 → 0.64) has escalated as post-2008 reforms have become
 *   performative. From multiple perspectives, this constraint appears
 *   simultaneously as coordination (rope), temporary policy failure
 *   (scaffold), degraded regulatory theater (piton), pure extraction (snare),
 *   and natural mathematical inevitability (mountain). The mandatrophy is
 *   resolved by showing that the constraint's classification depends entirely
 *   on the observer's structural position, exit capacity, and time horizon —
 *   the same market mechanism is genuinely coordinating for those with
 *   arbitrage options and purely extractive for those trapped without exit.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — bear portfolio volatility cost without predictive capacity or hedging access
 *   - Small Businesses: Secondary victims (powerless/trapped) — dependent on credit and supplier availability, which are entrained by attractor dynamics
 *   - Systemically Important Institutions (SIIs): Primary beneficiaries (institutional/arbitrage) — profit from volatility prediction and leverage arbitrage; can hedge or exit local stress
 *   - High-Frequency Trading Firms: Primary beneficiaries (organized/arbitrage) — extract value from volatility patterns and information advantage during attractor transitions
 *   - Central Banks and Financial Regulators: Organized defenders (organized/constrained) — attempt macroprudential management but are themselves entrained by the dynamics
 *   - Decentralized Finance Protocols: Emerging alternative (organized/constrained) — building parallel market infrastructure with different attractor topology
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as mathematical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strange_attractors, 0.58).
domain_priors:suppression_score(strange_attractors, 0.68).
domain_priors:theater_ratio(strange_attractors, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strange_attractors, extractiveness, 0.58).
narrative_ontology:constraint_metric(strange_attractors, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(strange_attractors, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strange_attractors, tangled_rope).
narrative_ontology:human_readable(strange_attractors, "Systemic Risk Amplification via Strange Attractor Dynamics").
narrative_ontology:topic_domain(strange_attractors, "economic/financial_stability").

domain_priors:requires_active_enforcement(strange_attractors).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strange_attractors, systemically_important_institutions).
narrative_ontology:constraint_beneficiary(strange_attractors, high_frequency_traders).
narrative_ontology:constraint_beneficiary(strange_attractors, large_financial_conglomerates).
narrative_ontology:constraint_victim(strange_attractors, retail_investors).
narrative_ontology:constraint_victim(strange_attractors, small_businesses).
narrative_ontology:constraint_victim(strange_attractors, systemic_stability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped in global capital markets with no exit option short of complete divestment (economically unrealistic). Experiences maximum extraction as portfolio volatility spikes during strange attractor cascades. No capacity to predict or hedge against nonlinear regime shifts. Information asymmetry ensures they buy high during euphoric approaches and sell low during collapse phases.
constraint_indexing:constraint_classification(strange_attractors, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (SNARE) — Credit availability, input costs, and revenue streams are all entrained by strange attractor dynamics in upstream financial markets. When the attractor shifts, credit dries up and suppliers fail simultaneously. No exit option except bankruptcy. Suppression is extreme: no hedging instruments available at retail scale; no predictive capacity; no policy recourse during cascade.
constraint_indexing:constraint_classification(strange_attractors, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEMIC STABILITY COMMONS (TANGLED ROPE) — Can be modeled as an organized agent (central banks, regulators, international coordination bodies) with constrained exit options. Benefits from periods of stability and liquidity coordination enabled by the same interconnected market structure. But bears catastrophic extraction cost during cascade phases. Active enforcement (macroprudential regulation, circuit breakers, capital requirements) attempts to manage the strange attractor but is itself entrained by the dynamics — enforcement mechanisms degrade in crisis.
constraint_indexing:constraint_classification(strange_attractors, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SYSTEMICALLY IMPORTANT INSTITUTIONS (ROPE) — Primary beneficiary. For large financial conglomerates and high-frequency trading operations, the strange attractor is a coordination mechanism: it concentrates volatility in predictable patterns, enabling profitable positioning before regime shifts. Arbitrage options (leverage, derivatives, counterparty substitution, regulatory arbitrage across jurisdictions) allow them to exit local stress and transfer it downstream. The constraint functions as a wealth transfer mechanism in their favor — extraction runs toward this agent.
constraint_indexing:constraint_classification(strange_attractors, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY RESPONSE APPARATUS (PITON) — Post-2008, macroprudential regulation (stress testing, capital buffers, circuit breakers, derivatives clearing) was designed to prevent strange attractor formation and manage cascades. In practice, these mechanisms are substantially performative: they create the appearance of control while the underlying nonlinear dynamics persist. Stress test thresholds are calibrated to historical volatility distributions that no longer apply. Circuit breakers prevent trades for seconds and resume at the same prices. Capital requirements are gamed through structured products. The regulatory theater persists through institutional inertia — replaced only marginally by actual risk prevention. Theater ratio high because the enforcement mechanisms are ceremonial risk management rather than functional elimination of strange attractors.
constraint_indexing:constraint_classification(strange_attractors, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DECENTRALIZED FINANCE MOVEMENT (SCAFFOLD) — Blockchain-based market infrastructure aims to replace concentrated financial intermediaries with distributed protocols, theoretically eliminating systemically important institutions. DeFi experiences its own strange attractors (liquidity cascades, liquidation spirals) but operates on a sunset logic: as decentralization tools mature and custody solutions improve, the economic incentive to route assets through traditional SIIs declines. This is an active displacement strategy with a visible exit path — institutional decentralization — though it retains uncertainty about whether it prevents strange attractors or redistributes them.
constraint_indexing:constraint_classification(strange_attractors, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL CHAOS VIEW (MOUNTAIN) — From a mathematical/systems perspective, strange attractors in any sufficiently complex feedback system are structurally inevitable. Markets have endogenous liquidity demand, collateral chains, leverage feedback, and herding dynamics — these create nonlinear coupling that inevitably produces attractors. No institutional design can eliminate them; only management is possible. However, this perspective risks naturalizing what is partially contingent: the severity and topology of the strange attractor depend on policy choices (leverage caps, transparency mandates, counterparty consolidation limits). The mountain classification is a false summit — it mistakes the invariance of 'some attractor exists' with the contingence of 'this specific attractor exists with this topology.'
constraint_indexing:constraint_classification(strange_attractors, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strange_attractors_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strange_attractors, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strange_attractors, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strange_attractors, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strange_attractors, TR),
    TR >= 0.70.

:- end_tests(strange_attractors_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint enables asymmetric wealth transfer during volatility cascades. SIIs and high-frequency traders extract value from the predictable structure of strange attractor transitions. However, extractiveness is not at maximum (0.72+) because some coordination benefit is genuine — the same interconnected markets that create attractors also enable efficient capital allocation during normal phases. The extractiveness reflects the mixture of coordination (enabling) and extraction (distorting). Suppression (0.68): High. Retail investors face extreme barriers to escape: no real-time attractor monitoring, no affordable hedging instruments, no policy recourse during cascades. Small businesses cannot forecast supplier/credit availability changes driven by upstream attractor shifts. Informational opacity is structural — even regulators cannot identify attractor topology in real time. Theater ratio (0.64): Moderate-high. Post-2008 macroprudential regulation (stress testing, capital buffers, circuit breakers, derivatives clearing) creates the appearance of systemic control while the underlying nonlinear dynamics persist. Stress tests are calibrated to historical distributions that no longer apply. Circuit breakers create brief pauses before resumption at similar prices. Capital requirements are gamed through structured products. The regulatory theater has increased as regulatory complexity has grown without corresponding reduction in systemic vulnerability.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range of tangled rope classification. Retail investors see pure extraction (Snare) — they have no capacity to profit from or escape volatility cascades. SIIs see coordination (Rope) — the same markets that create attractors enable their profitable positioning. Small businesses see extraction (Snare) — credit and supplier dependencies are exogenously determined by dynamics they cannot predict. Central banks see themselves as managing a temporary coordination problem (Scaffold) with sunset logic: macroprudential tools will eventually stabilize the system. Regulators see their own mechanisms as degraded (Piton) — they maintain the appearance of control through performative supervision. The analytical observer risks seeing this as mathematically inevitable (Mountain) — complex feedback systems always produce attractors — but the structural data reveals this as a false summit: the severity, topology, and exploitability of specific attractors depend on policy choices about leverage, transparency, and institutional consolidation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is derived from their structural position relative to the strange attractor: (1) Power level determines their capacity to anticipate and hedge against regime shifts. Powerless agents (retail, small business) have d near 1.0 (full targets). Institutional agents with analytical capacity have d near 0.0 (beneficiaries). (2) Exit options determine their flexibility to escape cascade phases. Trapped agents (retail, small business) have high d. Arbitrage-capable agents (SIIs, hedge funds) have low d. (3) Beneficiary/victim status is structural: those who profit from volatility are beneficiaries (low d); those who suffer losses are victims (high d). The sigmoid function f(d) converts structural position into experienced extractiveness. Beneficiaries with low d experience negative χ (they are subsidized by the constraint). Victims with high d experience amplified χ (they bear extraction). Constrained agents with moderate exit capacity experience moderate χ. This derivation is standard; the mandatrophy insight is that all six types are legitimate and simultaneously true from different observational positions.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL DECOMPOSITION: The mandatrophy 'Is this pure coordination or pure extraction?' is resolved by showing that the answer is 'both, depending on who you are.' The same strange attractor structure that enables efficient price discovery and liquidity provision (coordination benefit) also enables high-frequency trading, leverage-driven volatility, and information-asymmetric wealth transfer (extraction mechanism). These are not two separate constraints; they are two aspects of a single tangled structure. The constraint classifies as Tangled Rope from the analytical perspective (ε=0.58, suppression=0.68, 0.40 ≤ χ ≤ 0.90) because it exhibits both genuine coordination function AND asymmetric extraction with active enforcement mechanisms. From the beneficiary perspective (SIIs with arbitrage options), it classifies as Rope — they experience the coordination benefits and have exit options. From the victim perspective (retail investors trapped without exit), it classifies as Snare — they experience pure extraction. The Scaffold perspective (decentralized finance movement) suggests that alternative infrastructure could reduce attractor severity with a generational sunset. The Piton perspective (regulatory theater) shows that enforcement mechanisms are degraded and performative. The Mountain perspective (analytical/natural chaos) is a false summit — it mistakes the universality of 'some attractor exists' with the contingence of 'this specific attractor is unmanageable.' The constraint resolves mandatrophy by being honestly tangled: coordination enables extraction; extraction is real but not total; reform is possible but constrained by the nonlinear structure itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attractor_topology_identification,
    'Can regulatory authorities identify the current strange attractor''s basin of attraction and critical bifurcation thresholds in real time?',
    'Real-time estimation of phase-space structure via high-dimensional variance decomposition; prospective prediction of regime shifts before they occur; comparison of predicted vs actual bifurcation timing in stress scenarios',
    'If topology is identifiable: macroprudential policy can be anticipatory (Scaffold confidence increases). If topology is obscured: policy is reactive theater (Piton confidence increases). If topology changes faster than detection: attractor is truly unmanageable (Mountain confidence increases).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attractor_topology_identification, empirical, 'Real-time identifiability of strange attractor topology and bifurcation thresholds').

omega_variable(
    leverage_constraint_sufficiency,
    'Do leverage caps and counterparty exposure limits actually reduce strange attractor severity, or do they merely displace the nonlinear dynamics into off-balance-sheet derivatives and shadow banking?',
    'Comparison of systemic risk concentration metrics before and after leverage constraint implementation; measurement of off-balance-sheet leverage relative to regulated leverage; tracking of cascade initiation points in constrained vs unconstrained market segments',
    'If leverage constraints are sufficient: extractiveness drops, enforcement becomes more functional (Piton → Rope transition possible). If displacement occurs: extractiveness persists hidden (constraint becomes Snare disguised as controlled system).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leverage_constraint_sufficiency, empirical, 'Whether leverage constraints reduce or displace systemic nonlinear dynamics').

omega_variable(
    decentralized_attractor_substitution,
    'Does replacing concentrated financial intermediaries with decentralized protocols eliminate strange attractors or merely create new attractors with different topology (flash crashes, liquidation spirals, MEV extraction)?',
    'Comparative analysis of volatility clustering, bifurcation points, and cascade triggers in DeFi vs traditional markets; measurement of whether crisis correlation patterns are reduced or merely reorganized; assessment of whether decentralized attractors are more or less exploitable by informed actors',
    'If DeFi attractors are weaker: scaffold sunset is real and structural (confidence high). If DeFi attractors are equivalent or worse: decentralization merely redistributes extraction (scaffold confidence drops to medium/low).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_attractor_substitution, empirical, 'Whether decentralized finance eliminates or redistributes strange attractor dynamics').

omega_variable(
    behavioral_vs_structural_origin,
    'Are strange attractors primarily generated by feedback loops in the financial structure (leverage, collateral chains, counterparty networks) or by behavioral biases and herding in investor decision-making?',
    'Agent-based models isolating behavioral vs structural contributions; empirical decomposition of volatility clustering into herding components vs nonlinear mechanical feedback; cross-market comparison of attractor properties in human-dominated vs automated trading',
    'If primarily structural: policy must redesign market infrastructure (tangled_rope → scaffold trajectory possible). If primarily behavioral: education and circuit breakers suffice (extraction severity assessment changes). If mixed and inseparable: mandatrophy is genuine and unresolvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_structural_origin, empirical, 'Decomposition of strange attractor origins into behavioral vs structural mechanisms').

omega_variable(
    extraction_flow_reversal_possibility,
    'Can systemic risk events be engineered to reliably transfer wealth from large financial institutions to small actors, reversing the normal extraction direction?',
    'Analysis of whether short-volatility strategies, tail-hedging derivatives, or organized retail investor coordination can create profitable positions during cascade phases; historical tracking of winners and losers in recent cascade events; feasibility assessment of retail-scale instruments with high payoff-to-cost ratios in chaos regimes',
    'If reversal is possible: extraction becomes exploitable, not inevitable (tangled_rope → rope transition possible). If reversal is structurally blocked: extraction is asymmetric by institutional design (snare classification for powerless agents is confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_flow_reversal_possibility, empirical, 'Whether extraction direction can be reversed through coordinated strategies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strange_attractors, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stattr_tr_t0, strange_attractors, theater_ratio, 0, 0.42).
narrative_ontology:measurement(stattr_tr_t10, strange_attractors, theater_ratio, 10, 0.55).
narrative_ontology:measurement(stattr_tr_t20, strange_attractors, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(stattr_be_t0, strange_attractors, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stattr_be_t10, strange_attractors, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(stattr_be_t20, strange_attractors, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strange_attractors, resource_allocation).
narrative_ontology:affects_constraint(strange_attractors, credit_channel_procyclicality).
narrative_ontology:affects_constraint(strange_attractors, herding_amplification_feedback).
narrative_ontology:affects_constraint(strange_attractors, leverage_constraint_binding).

% DUAL FORMULATION NOTE:
% This constraint decomposes into three related but structurally distinct claims: (1) The credit channel couples real economy to financial markets through collateral feedback (ε=0.42, tangled_rope). (2) Behavioral herding amplifies exogenous shocks through imitative trading (ε=0.35, rope with theater components). (3) Leverage constraints are binding and effective in reducing attractor severity (ε=0.28, scaffold with sunset), or they are permeable and displacement-prone (ε=0.52, snare). Each has different ε and different management implications. Together, they form a constraint family where the aggregate strange attractor emerges from their interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strange_attractors, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
