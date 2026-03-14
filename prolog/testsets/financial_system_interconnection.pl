% ============================================================================
% CONSTRAINT STORY: financial_system_interconnection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financial_system_interconnection, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: financial_system_interconnection
 *   human_readable: Financial System Interconnection and Systemic Risk
 *   domain: economic/financial/regulatory
 *
 * SUMMARY:
 *   Financial system interconnection is a structural constraint that
 *   simultaneously enables modern economies to function at scale and
 *   concentrates contagion risk in ways that systematically extract value
 *   from retail depositors, small businesses, and taxpayers. The constraint
 *   exhibits the full tension of a Tangled Rope: genuine coordination
 *   function (credit allocation, payment systems, liquidity provision) is
 *   inseparable from asymmetric extraction (concentration of gains in
 *   systemically important institutions, concentration of losses among
 *   powerless agents, moral hazard from too-big-to-fail). Active enforcement
 *   through regulation maintains the hybrid structure but cannot fully
 *   eliminate either function. The theater ratio has increased over the
 *   20-year interval as regulatory compliance has grown more complex while
 *   predictive accuracy has stagnated — Basel III was in place before the
 *   2020 COVID-driven credit freeze, suggesting degradation of the regulatory
 *   information function. The extractiveness metric shows steady increase,
 *   reflecting both real structural tightening and capture dynamics where
 *   regulatory frameworks increasingly codify advantages for large
 *   institutions.
 *
 * KEY AGENTS:
 *   - Retail Depositors: Primary victim (powerless/trapped) — no viable exit from system; absorb losses through crises and inflation despite insurance theater
 *   - Small Business Sector: Secondary victim (moderate/constrained) — dependent on credit access; extraction through rationing and rate spikes during crises
 *   - Financial Stability (Collective Good): Structural victim (powerless/trapped) — cannot organize; contagion cascades routinely exploit interconnection topology
 *   - Systemically Important Banks (SIFIs): Primary beneficiary (institutional/constrained) — benefit from coordination function but constrained by systemic criticality and regulation; implicit TBTF guarantee provides hidden subsidy
 *   - Large Investment Funds: Secondary beneficiary (organized/arbitrage) — high exit capacity; benefit from liquidity, leverage, and ability to exit before contagion propagates
 *   - Central Bank: Tertiary beneficiary (institutional/arbitrage) — maximum policy arbitrage; controls liquidity injection and policy rates; can insulate itself from contagion
 *   - Regulatory Apparatus: Tertiary actor (institutional/constrained) — maintains compliance theater but has limited predictive power; Piton classification reflects post-hoc validation rather than prospective risk detection
 *   - Alternative Finance Ecosystems: Analytical observer (analytical/mobile) — represent genuine alternative pathways with sunset logic; currently marginal but growing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financial_system_interconnection, 0.58).
domain_priors:suppression_score(financial_system_interconnection, 0.68).
domain_priors:theater_ratio(financial_system_interconnection, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financial_system_interconnection, extractiveness, 0.58).
narrative_ontology:constraint_metric(financial_system_interconnection, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(financial_system_interconnection, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financial_system_interconnection, tangled_rope).
narrative_ontology:human_readable(financial_system_interconnection, "Financial System Interconnection and Systemic Risk").
narrative_ontology:topic_domain(financial_system_interconnection, "economic/financial/regulatory").

domain_priors:requires_active_enforcement(financial_system_interconnection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financial_system_interconnection, systemically_important_institutions).
narrative_ontology:constraint_beneficiary(financial_system_interconnection, central_banks).
narrative_ontology:constraint_beneficiary(financial_system_interconnection, large_investment_funds).
narrative_ontology:constraint_victim(financial_system_interconnection, retail_depositors).
narrative_ontology:constraint_victim(financial_system_interconnection, small_businesses).
narrative_ontology:constraint_victim(financial_system_interconnection, taxpayers).
narrative_ontology:constraint_victim(financial_system_interconnection, financial_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL DEPOSITOR (SNARE) — Structurally trapped in the system with no viable exit. Cannot move deposits to alternative financial infrastructure. Absorbs losses through bank failures, currency devaluation, and inflation despite deposit insurance theaters. Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(financial_system_interconnection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FINANCIAL STABILITY AS COLLECTIVE GOOD (SNARE) — Abstract structural good that cannot organize or exit. Contagion cascades routinely destroy systemic stability; no agent advocates for it; interconnection creates moral hazard where institutions are too-big-to-fail, guaranteeing extraction of taxpayer resources during crisis periods. The collective good bears pure extraction with no mitigation mechanism.
constraint_indexing:constraint_classification(financial_system_interconnection, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL BUSINESS SECTOR (TANGLED ROPE) — Experiences genuine coordination: access to credit lines, payment processing, working capital financing enables business operations. Also experiences extraction: credit rationing during crises, interest rate spikes, sudden collateral calls. Moderate power but constrained exit — cannot maintain operations without financial system access, but system failures directly threaten survival. Mixed function and extraction.
constraint_indexing:constraint_classification(financial_system_interconnection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE INVESTMENT FUNDS (ROPE) — Organized institutional actors with arbitrage options. Benefits from interconnection through access to diverse counterparties, leverage, and systemic liquidity. Can shift capital globally and move out of distressed assets during stress periods. Experiences interconnection as coordination: the system enables their core operations. Net beneficiaries with institutional exit capacity.
constraint_indexing:constraint_classification(financial_system_interconnection, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SYSTEMICALLY IMPORTANT BANK (SNARE→TANGLED ROPE) — Institutional actor constrained by systemic role. The SIFI experiences genuine coordination function: interconnection enables payment processing, lending, market-making at scale. But also experiences extraction through regulation, capital requirements, too-big-to-fail stigma, and implicit government guarantee that enables risk-taking by competitors. Active enforcement (regulation) maintains both coordination and extraction. Constrained exit due to systemic criticality — cannot shrink without systemic cost.
constraint_indexing:constraint_classification(financial_system_interconnection, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CENTRAL BANK (ROPE) — Institutional actor with high arbitrage capacity (ability to inject liquidity, set policy rates, manage currency). Experiences interconnection as coordination mechanism: systemic stability is their primary mandate. Can exit through policy tools (quantitative easing, rate manipulation) that insulate them from contagion. Benefits from interconnection through operational leverage and policy effectiveness. Net beneficiary.
constraint_indexing:constraint_classification(financial_system_interconnection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY COMPLIANCE APPARATUS (PITON) — Basel III, stress testing, macroprudential frameworks are partially degraded — they perform compliance verification but have limited predictive power for systemic crises. The 2008 crisis occurred despite all major institutions passing stress tests in 2007. Theater ratio reflects that much regulatory activity is post-hoc risk theater rather than prospective risk mitigation. Regulatory structures persist through institutional inertia and mandate, not proven effectiveness.
constraint_indexing:constraint_classification(financial_system_interconnection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ALTERNATIVE FINANCE ECOSYSTEMS (SCAFFOLD) — Blockchain-based finance, peer-to-peer lending, and decentralized exchanges represent genuine alternatives with sunset logic. These alternatives reduce extraction pressure on traditional finance by offering exit routes for retail actors. Mobile agent perspective because alternatives can expand/contract. Theater ratio in alternative systems is lower (direct settlement, transparent smart contracts) but nascent. Real sunset potential: if alternatives scale, traditional interconnection's extraction burden declines.
constraint_indexing:constraint_classification(financial_system_interconnection, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational view recognizes that modern economies require interconnection (coordination function) to operate at current scale and complexity. But interconnection also concentrates contagion risk (extraction function). The constraint is neither pure coordination nor pure extraction — it is genuinely hybrid. Active enforcement (regulation) is necessary but insufficient. Analytical perspective captures the irreducible tension: you cannot have low-cost financial intermediation without systemic risk, and you cannot have financial stability without accepting some coordination cost.
constraint_indexing:constraint_classification(financial_system_interconnection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_system_interconnection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financial_system_interconnection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_system_interconnection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financial_system_interconnection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(financial_system_interconnection, TR),
    TR >= 0.70.

:- end_tests(financial_system_interconnection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system does provide genuine coordination function (credit, payments, liquidity) worth approximately 0.20-0.25 of base extractiveness. But the distribution of benefits and costs is severely asymmetric. Retail depositors receive payment services and nominal interest while absorbing contagion losses; small businesses receive credit conditional on excessive collateral and rates that spike in crises; large institutions receive access to leverage and implicit TBTF guarantees. The net extraction (benefit - cost) is positive for top 5% of financial institutions and negative for bottom 80% of depositors. Suppression (0.68): High. Significant barriers to exit include: (1) physical necessity — modern economies require financial intermediation; (2) regulatory barriers — cannot operate without banking system participation; (3) information asymmetry — retail actors cannot effectively assess counterparty risk; (4) collective action failure — individual exit impossible without coordinating millions of depositors; (5) property rights constraints — your deposits are bank liabilities, giving banks claim priority in insolvency. Theater ratio (0.55): Moderate. Regulatory stress testing, capital adequacy frameworks, and macroprudential oversight are partially theater — they assess plausibility but have poor forward predictive power. However, unlike the verification bottleneck example, the theater serves a secondary function (post-hoc legitimization) rather than being the primary mechanism. The theater has increased over the interval as Basel III became more complex.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the powerless depositor's Snare and the central bank's Rope is the fundamental insight: the same structural constraint produces opposite classifications for agents with different power levels and exit capacity. This is the mechanism by which interconnection sustains extraction — the beneficiaries see beneficial coordination while the targets are sufficiently disorganized and exit-constrained that they cannot resist. Breaking the gap requires either (1) enabling exit for trapped agents (alternative finance scaling), (2) organizing trapped agents into collective power (deposit insurance reform, mutual banks), or (3) reallocation of extraction gains (wealth redistribution through taxation). Current regulatory approaches attempt (3) through redistribution of some crisis costs, but retain the underlying Tangled Rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by structural position: beneficiary status, victim status, and exit capacity. Retail depositors with trapped exit experience maximum d (0.95+) — they are structurally pure targets of extraction. Small businesses with constrained exit experience high-moderate d (0.70-0.80) — they benefit from credit but cannot exit and face extraction through rationing. Large institutions with arbitrage options experience low d (0.10-0.20) — they are structurally beneficiaries. Central banks with analytical exit capacity experience low d (near 0) — they control the system. The SIFI perspective (constrained institutional) experiences moderate d (0.40-0.55) — the institution benefits from interconnection but is constrained by systemic criticality. The functional form χ = ε × f(d) × σ(S) produces high chi for powerless/trapped agents and low/negative chi for institutional/arbitrage agents, which accurately reflects the asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by showing that financial interconnection is genuinely hybrid — it cannot be classified as pure coordination (Rope) because extraction is real and severe for powerless agents; it cannot be classified as pure extraction (Snare) because coordination function is real and necessary. The Tangled Rope classification is the minimal accuracy representation. Attempted misclassifications: (1) Naive Rope classification ignores distributional asymmetry and moral hazard. (2) Naive Snare classification ignores that deposit insurance and payment systems do provide real coordination value. (3) Piton classification would suggest the system is vestigial and could be removed — but systemic importance proves the coordination function is irreplaceable. (4) Scaffold classification is premature — alternative finance is not yet scaled enough to be the sunset path, though it represents the potential path. The Tangled Rope classification forces acknowledgment that both functions are real and that eliminating extraction requires either eliminating coordination (impossible and catastrophic) or transforming the institutional structure (difficult but possible through alternative finance scaling or institutional reform).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contagion_mechanism_precision,
    'What fraction of financial crisis propagation is deterministic contagion (interconnection topology) vs stochastic panic (behavioral feedback)?',
    'Historical crisis data: network reconstruction of actual exposures during 2008, 2020, Eurozone crises; comparison of exposure-based contagion models to realized propagation patterns; behavioral analysis of panic selling phases',
    'If predominantly deterministic: interconnection is the binding constraint (Snare classification likely correct). If predominantly panic: interconnection is secondary to confidence collapse (classification shifts to behavioral/psychological constraints). Impact on policy: topology redesign vs confidence management strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contagion_mechanism_precision, empirical, 'Fraction of crisis propagation from network topology vs behavioral panic').

omega_variable(
    too_big_to_fail_moral_hazard,
    'Does implicit TBTF guarantee increase risk-taking by large institutions, thereby increasing systemic fragility?',
    'Comparative analysis of risk metrics (leverage, equity ratios, concentration) pre- and post-crisis for institutions known to be TBTF; correlation of TBTF status with subsequent risk-taking behavior; econometric isolation of TBTF cost of capital benefit vs risk increase',
    'If moral hazard > TBTF benefit: regulation should break up large institutions (architectural change). If TBTF benefit > moral hazard: interconnection benefits dominate extraction cost (reframe as Rope). If roughly balanced: confirms Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(too_big_to_fail_moral_hazard, empirical, 'Moral hazard amplification from TBTF implicit guarantees').

omega_variable(
    alternative_finance_scalability,
    'Can decentralized/blockchain finance scale to handle systemic financial flows without recreating the interconnection constraint?',
    'Stress testing of alternative finance systems at progressively larger scale; analysis of whether defi protocols recreate liquidity fragility under stress; study of whether decentralization sacrifices efficiency necessary for real economy function',
    'If scalable without recreation: genuine sunset clause for traditional interconnection (Scaffold classification confirmed). If scalability impossible: alternatives are permanently marginal and extraction persists indefinitely (Tangled Rope persists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_finance_scalability, empirical, 'Whether alternative finance can scale without recreating systemic interconnection').

omega_variable(
    regulatory_capture_feedback,
    'Do large institutions actively capture regulators to undermine interconnection constraints, or are regulators independently constrained by complexity?',
    'Documented revolving-door analysis (regulator career paths before/after agency tenure); textual analysis of regulatory guidance and industry commentary for alignment/divergence; historical comparison of regulations vs industry lobbying positions',
    'If high capture: extraction increases over time despite regulatory theater (Snare classification escalates). If low capture: regulatory degradation is complexity-driven and potentially remediable (Piton classification more accurate than degraded Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_feedback, empirical, 'Degree of regulatory capture by large financial institutions').

omega_variable(
    distributional_extractiveness,
    'Is financial system interconnection''s extraction distributed across all agent types or concentrated on particular vulnerable populations?',
    'Wealth/income impact analysis by decile during crisis periods (2008, 2020, regional crises); tracking of deposit losses, unemployment, asset value collapse across income groups; analysis of whose debts are socialized and whose assets are protected by bailouts',
    'If highly concentrated on poor/middle: extraction is severe and deliberately enforced (Snare classification higher confidence). If distributed: interconnection is more balanced mixed function (Rope classification higher confidence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_extractiveness, empirical, 'Distribution of financial crisis costs across income/wealth deciles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financial_system_interconnection, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fsi_tr_t0, financial_system_interconnection, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fsi_tr_t5, financial_system_interconnection, theater_ratio, 5, 0.48).
narrative_ontology:measurement(fsi_tr_t10, financial_system_interconnection, theater_ratio, 10, 0.55).
narrative_ontology:measurement(fsi_tr_t15, financial_system_interconnection, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(fsi_be_t0, financial_system_interconnection, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fsi_be_t5, financial_system_interconnection, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fsi_be_t10, financial_system_interconnection, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(fsi_be_t15, financial_system_interconnection, base_extractiveness, 15, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financial_system_interconnection, resource_allocation).
narrative_ontology:boltzmann_floor_override(financial_system_interconnection, 0.2).
narrative_ontology:affects_constraint(financial_system_interconnection, too_big_to_fail_moral_hazard).
narrative_ontology:affects_constraint(financial_system_interconnection, deposit_insurance_credibility).
narrative_ontology:affects_constraint(financial_system_interconnection, regulatory_arbitrage_jurisdictional).
narrative_ontology:affects_constraint(financial_system_interconnection, shadow_banking_parallel_extraction).

% DUAL FORMULATION NOTE:
% Financial system interconnection decomposes into multiple structurally distinct constraints: (1) technical interconnection (payment system topology) with low extractiveness (0.15-0.20), (2) regulatory interconnection (capital and liquidity requirements) with moderate extractiveness (0.35-0.45), (3) moral hazard from TBTF (implicit guarantee structure) with high extractiveness (0.65-0.75). The aggregate constraint shown here (ε=0.58) represents the sum of these mechanisms. Each can be addressed through different architectural or policy interventions, but together they form the integrated financial system interconnection constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financial_system_interconnection, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
