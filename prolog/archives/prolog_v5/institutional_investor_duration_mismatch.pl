% ============================================================================
% CONSTRAINT STORY: institutional_investor_duration_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_investor_duration_mismatch, []).

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
 *   constraint_id: institutional_investor_duration_mismatch
 *   human_readable: Institutional Investor Duration Mismatch
 *   domain: financial_markets/asset_management
 *
 * SUMMARY:
 *   Institutional investors (pension funds, endowments, insurance companies)
 *   face a structural constraint: their liabilities have fixed payment
 *   schedules (retiree benefits, policy payouts) while their assets have
 *   uncertain returns and lifespans. This duration mismatch creates a
 *   coordination problem — how to align asset maturity profiles with
 *   liability schedules — but the mechanism by which financial intermediaries
 *   solve this problem has become extraction-rich. Asset managers charge fees
 *   (0.50-1.50% annually) justified as duration management services, but much
 *   of the fee extraction is enabled by opacity: beneficiaries cannot observe
 *   their actual duration exposure, cannot exit misaligned portfolios without
 *   incurring transaction costs, and cannot accurately price the risk they
 *   are bearing. The constraint exhibits high suppression (0.65) because the
 *   mechanism operates across multiple layers of intermediation (asset
 *   managers → pension fund trustees → beneficiaries) with information
 *   asymmetry at each boundary. Theater ratio (0.48) is moderate, indicating
 *   that while quarterly performance reporting creates performative
 *   rebalancing pressure, the constraint is not purely theatrical — duration
 *   management has a real coordination function. The extractiveness
 *   trajectory (0.35 → 0.58 over 15 years) reflects that duration mismatches
 *   have grown as interest rate volatility increased and as asset management
 *   competition drove fee expansion rather than fee compression. Regulatory
 *   reforms (Solvency II in Europe, stress-testing requirements post-2008)
 *   are building institutional capacity for algorithmic, transparent duration
 *   matching, creating a potential sunset mechanism that would convert the
 *   constraint from tangled_rope to scaffold.
 *
 * KEY AGENTS:
 *   - Asset Managers: Primary beneficiary (institutional/arbitrage) — extract management fees justified by duration matching services; arbitrage option available through client switching or strategy changes
 *   - Pension Fund Beneficiaries: Primary victim (powerless/trapped) — retirement savings locked into portfolios they cannot observe; bear full cost of rebalancing-induced volatility; information asymmetry prevents exit
 *   - Pension Fund Trustees: Secondary actor (organized/constrained) — fiduciary duty creates genuine coordination problem; also participate in extraction through performance fee structure; constrained by regulatory capital requirements
 *   - Financial Intermediaries: Beneficiary (institutional/arbitrage) — custodians, brokers, clearing houses extract transaction fees from forced rebalancing cycles
 *   - Retail Investors: Secondary victim (moderate/constrained) — duration mismatch embedded in target-date retirement funds; bear costs of volatility amplification; can exit through fund switching
 *   - Regulatory Authorities: Powerful reform agent (powerful/mobile) — building alternative architectures (stress testing, real-time monitoring, duration caps) that could sunset private extraction mechanism
 *   - Systemic Financial Stability: Tertiary victim (analytical/analytical) — pro-cyclical forced rebalancing during market stress amplifies volatility and systemic risk; externality not internalized by extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_investor_duration_mismatch, 0.58).
domain_priors:suppression_score(institutional_investor_duration_mismatch, 0.65).
domain_priors:theater_ratio(institutional_investor_duration_mismatch, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_investor_duration_mismatch, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_investor_duration_mismatch, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_investor_duration_mismatch, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_investor_duration_mismatch, tangled_rope).
narrative_ontology:human_readable(institutional_investor_duration_mismatch, "Institutional Investor Duration Mismatch").
narrative_ontology:topic_domain(institutional_investor_duration_mismatch, "financial_markets/asset_management").

domain_priors:requires_active_enforcement(institutional_investor_duration_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_investor_duration_mismatch, asset_managers).
narrative_ontology:constraint_beneficiary(institutional_investor_duration_mismatch, financial_intermediaries).
narrative_ontology:constraint_victim(institutional_investor_duration_mismatch, pension_fund_beneficiaries).
narrative_ontology:constraint_victim(institutional_investor_duration_mismatch, retail_investors).
narrative_ontology:constraint_victim(institutional_investor_duration_mismatch, systemic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PENSION FUND BENEFICIARY (SNARE) — Structurally trapped. The beneficiary's retirement savings are locked into duration mismatches they cannot observe or exit. They bear the full cost of volatility spikes when short-term liability schedules force asset liquidations at unfavorable prices. No alternative portfolio available; no information asymmetry remedy within the constraint structure. Maximum suppression — the mechanism itself is obscured by layers of intermediaries.
constraint_indexing:constraint_classification(institutional_investor_duration_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PENSION FUND TRUSTEE (TANGLED ROPE) — Constrained by fiduciary duty, regulatory capital requirements, and liability schedules. Faces genuine coordination problem: matching asset duration to liability duration is critical for solvency. But the constraint also enables extraction: opportunity for performance fee arbitrage when rebalancing forced by mismatch. Active enforcement through regulatory capital gates (Basel III, Solvency II) is required to maintain the coordination function while containing extraction.
constraint_indexing:constraint_classification(institutional_investor_duration_mismatch, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ASSET MANAGER (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: managing duration mismatches through active trading and rebalancing generates management fees (0.50-1.50% annually) without fiduciary liability. Arbitrage option available: can exit client relationships or shift strategies if constraints tighten. Net positive: extraction flows toward this agent while they solve the coordination problem.
constraint_indexing:constraint_classification(institutional_investor_duration_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM MOVEMENT (SCAFFOLD) — Powerful agents (central banks, macroprudential authorities) increasingly see duration mismatch as a systemic risk requiring coordinated management. Stress-testing regimes, duration caps, and liquidity risk frameworks are building alternative architectures (real-time monitoring, automated rebalancing, duration-matched bonds). These tools are designed to sunset the private extraction mechanism by making it transparently costly. Theater ratio low because reform mechanisms are functional rather than performative — regulatory sandboxes and algorithmic rebalancing test actual risk reduction.
constraint_indexing:constraint_classification(institutional_investor_duration_mismatch, scaffold,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: QUARTERLY REPORTING RITUAL (PITON) — Pension funds and asset managers are required to produce quarterly performance reports and valuations, creating pressure for smooth earnings and penalty avoidance. This ritual was designed to enable transparency but has become performative: fund managers engage in strategic rebalancing ahead of reporting periods to show stable returns, precisely the behavior that amplifies duration mismatches between reporting cycles. Theater ratio high because the reporting mechanism produces the opposite of its intended function.
constraint_indexing:constraint_classification(institutional_investor_duration_mismatch, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a timeless/universal perspective, duration mismatch is an inherent feature of forward-looking financial systems: assets have uncertain lifespans while liabilities have fixed schedules. Some gap is inescapable and irreducible. However, the structural data contradicts this — the measured 0.65 suppression and 0.58 extractiveness indicate human agency and contingent institutional choices (fee structures, capital standards, reporting requirements), not natural law. The engine's false summit detector will identify this as naturalization of regulatory artifact.
constraint_indexing:constraint_classification(institutional_investor_duration_mismatch, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_investor_duration_mismatch_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_investor_duration_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_investor_duration_mismatch, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_investor_duration_mismatch, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_investor_duration_mismatch, TR),
    TR >= 0.70.

:- end_tests(institutional_investor_duration_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Duration mismatch creates genuine coordination value — asset managers solving the problem through active rebalancing and derivative hedging provide real service. But the fee structure (0.50-1.50% annually) captures more value than coordination cost alone would justify. Historical benchmarks suggest equivalent algorithmic duration matching costs 0.10-0.20% annually; the premium reflects extraction. The trajectory from 0.35 to 0.58 reflects both absolute growth in mismatches (interest rate volatility increased post-2008, regulatory arbitrage expanded) and expansion of extracted fee space as asset management competition failed to drive fees downward. Suppression (0.65): High. Multiple layers prevent beneficiaries from exiting or observing: (1) Information asymmetry — pension fund beneficiaries do not receive portfolio duration reports; (2) Intermediation opacity — trustees do not fully disclose asset manager fee structures; (3) Transaction costs — switching asset managers triggers capital gains taxes and rebalancing costs; (4) Regulatory barriers — pension fund trustees cannot easily shift to algorithmic management due to fiduciary duty standards that prefer human oversight. Theater ratio (0.48): Moderate. The constraint combines functional and performative elements. Functional: duration matching is real problem requiring real solution. Performative: quarterly reporting cycles create rebalancing pressure independent of actual duration drift, and performance metrics reward smooth returns rather than risk-adjusted outcomes. Strategic rebalancing ahead of reporting dates amplifies volatility between quarters, paradoxically worsening the very mismatch the mechanism claims to solve.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the power of indexed classification to capture structural misalignment within a single institutional mechanism. The asset manager's perspective (Rope, immediate/arbitrage) sees coordination: managing duration mismatches is solving a legitimate problem, generating value through active trading and hedging. The trustee's perspective (Tangled Rope, generational/constrained) is mixed: they coordinate on behalf of beneficiaries but also extract through fee arbitrage and performance incentives. The beneficiary's perspective (Snare, biographical/trapped) is pure extraction: the benefit of coordination is completely overwhelmed by the cost of opacity and volatility. The regulatory reform perspective (Scaffold, civilizational/mobile) sees a temporary problem with sunset: regulatory stress testing and algorithmic rebalancing are building architectures that would make the private extraction mechanism transparently costly and therefore obsolete. The quarterly reporting perspective (Piton, immediate/constrained) shows degradation: the ritual designed to enable transparency now produces the opposite effect through strategic rebalancing pressure. The analytical 'natural law' perspective risks misclassification: treating duration mismatch as an inherent feature of finance rather than as a contingent institutional arrangement maintained by fee incentives and regulatory design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the extraction flow. Asset managers (institutional/arbitrage) have d ≈ 0.15: they are beneficiaries with exit options, experiencing low or negative effective extraction. The sigmoid f(d) ≈ -0.01 means their experienced χ is dampened — they perceive the constraint as coordination (Rope). Pension fund trustees (organized/constrained) have d ≈ 0.40: they benefit from fee income but are constrained by fiduciary duty and regulatory capital requirements, experiencing moderate extraction. Pension fund beneficiaries (powerless/trapped) have d ≈ 0.95: they are pure victims with no exit, experiencing maximum f(d) ≈ 1.42 — their perceived χ is amplified, making the snare classification inevitable from their perspective. Scope modifier σ(national) = 1.0 does not adjust χ further, but if this constraint scaled to continental/global scope (σ > 1.0), effective extractiveness would be amplified by verification difficulty across jurisdictions. The gradient in directionality (from asset manager through trustee to beneficiary) directly produces the perspectival spread from Rope through Tangled Rope to Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the asset manager's coordination value is real but separable from the extraction mechanism. The constraint could remain as pure coordination (Rope) if fees reflected actual transaction and hedging costs (~0.15-0.20% annually). The fact that fees average 0.75-1.50% indicates extraction above coordination cost. The tangled_rope classification correctly captures both functions: beneficiaries (institutional/beneficiary) truly solve the coordination problem (why it's not pure snare), while extracting beyond coordination value (why it's not pure rope). Mandatrophy is resolved by the structural data: the presence of both beneficiaries and victims with active enforcement requirement validates tangled_rope gates. The scaffold perspective provides the potential exit: if regulatory reform successfully deploys algorithmic duration matching with transparent costs, the extraction mechanism loses justification and the constraint converts to a temporary coordination tool with a sunset. The piton perspective indicates degradation risk: if quarterly reporting pressure continues to distort rebalancing timing, the coordination function becomes increasingly performative and theater_ratio rises, potentially degrading tangled_rope toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_transaction_cost,
    'Is the measured extractiveness (0.58) genuine rent extraction or legitimate transaction cost of managing duration mismatch?',
    'Comparative analysis: cross-country asset management fee structures; correlation between fee levels and actual duration mismatch reduction; counterfactual cost analysis of algorithmic automated rebalancing',
    'If genuine extraction: snare classification appropriate for beneficiaries. If transaction cost: rope classification dominates. Methodological threshold: fees below 0.30% annually indicate cost recovery; above 0.75% indicate rent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_transaction_cost, empirical, 'Whether high management fees reflect extraction or legitimate coordination cost').

omega_variable(
    systemic_stability_role,
    'Does institutional investor duration management contribute to systemic stability or destabilize it through forced pro-cyclical rebalancing?',
    'Analysis of aggregate portfolio flows during market stress periods; measurement of volatility correlation between duration-mismatched portfolios and market crashes; regulatory stress-test data revealing forced liquidations',
    'If stabilizing: constraint is pure coordination (Rope dominates). If destabilizing: constraint is extraction mechanism amplifying systemic risk, and victims include systemic financial stability (add to victims list, upgrade snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_stability_role, empirical, 'Whether duration mismatch management is systemic stabilizer or destabilizer').

omega_variable(
    beneficiary_diversity_and_extraction_concentration,
    'Do all asset managers extract equally, or does extractive capacity concentrate among dominant players with scale advantages?',
    'Distribution analysis of management fees across asset manager size classes; market concentration metrics; correlation between concentration and extraction levels',
    'If extraction concentrated: snare classification shifts to organized victim capacity (coalition power dynamics). If distributed: snare classification holds for atomized beneficiaries, explaining why powerless agents cannot mobilize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_diversity_and_extraction_concentration, empirical, 'Whether extraction is distributed or concentrated among dominant asset managers').

omega_variable(
    regulatory_sunset_credibility,
    'Are regulatory reforms (Solvency II, stress testing, duration caps) actually reducing extractive space or creating new opacity through regulatory complexity?',
    'Longitudinal comparison of extracted fees pre- and post-regulation; measurement of compliance cost vs extraction reduction; analysis of regulatory arbitrage exploiting new rules',
    'If sunset credible: scaffold classification confirmed and extraction should decline over time. If regulatory complexity creates new extraction: scaffold is false, constraint persists as tangled_rope with higher theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_credibility, empirical, 'Whether regulatory reform represents genuine sunset or new complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_investor_duration_mismatch, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iidm_tr_t0, institutional_investor_duration_mismatch, theater_ratio, 0, 0.32).
narrative_ontology:measurement(iidm_tr_t5, institutional_investor_duration_mismatch, theater_ratio, 5, 0.4).
narrative_ontology:measurement(iidm_tr_t10, institutional_investor_duration_mismatch, theater_ratio, 10, 0.48).
narrative_ontology:measurement(iidm_tr_t15, institutional_investor_duration_mismatch, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(iidm_be_t0, institutional_investor_duration_mismatch, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iidm_be_t5, institutional_investor_duration_mismatch, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(iidm_be_t10, institutional_investor_duration_mismatch, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(iidm_be_t15, institutional_investor_duration_mismatch, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_investor_duration_mismatch, resource_allocation).
narrative_ontology:boltzmann_floor_override(institutional_investor_duration_mismatch, 0.18).
narrative_ontology:affects_constraint(institutional_investor_duration_mismatch, pension_fund_solvency_risk).
narrative_ontology:affects_constraint(institutional_investor_duration_mismatch, pro_cyclical_market_volatility).
narrative_ontology:affects_constraint(institutional_investor_duration_mismatch, fee_compression_resistance).

% DUAL FORMULATION NOTE:
% Duration mismatch decomposes into multiple structurally distinct constraints: (1) pension_fund_solvency_risk (ε≈0.25, coordination problem with mountain features), (2) institutional_investor_duration_mismatch (ε≈0.58, extraction mechanism), (3) pro_cyclical_market_volatility (ε≈0.62, systemic risk amplification). This story focuses on the extraction mechanism. Upstream constraint is solvency risk; downstream constraints are market volatility and fee structure resistance to compression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_investor_duration_mismatch, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
