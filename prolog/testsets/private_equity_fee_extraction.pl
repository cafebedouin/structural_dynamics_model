% ============================================================================
% CONSTRAINT STORY: private_equity_fee_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_private_equity_fee_extraction, []).

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
 *   constraint_id: private_equity_fee_extraction
 *   human_readable: Private Equity Fee Extraction Mechanism
 *   domain: financial/corporate_governance
 *
 * SUMMARY:
 *   The private equity fee extraction mechanism operates at the intersection
 *   of capital allocation, financial engineering, and institutional power. A
 *   PE fund acquires a company using leveraged capital (typically 60-70%
 *   debt, 30-40% equity), implements operational changes over a 5-7 year
 *   holding period, and exits via sale or IPO. During this period, managers
 *   extract fees that are largely decoupled from performance or actual value
 *   creation: a 2% annual management fee on committed capital generates
 *   hundreds of millions of dollars annually regardless of returns, and 20%
 *   carried interest on profits aligns manager incentives with financial
 *   outcomes but not with worker welfare, creditor risk, or long-term company
 *   sustainability. The constraint exhibits snare characteristics from the
 *   perspective of workers and creditors (trapped with no exit, bearing
 *   suppression through information asymmetry and legal subordination) but
 *   appears as pure coordination (rope) from the manager's perspective and
 *   tangled rope from the institutional investor's perspective (genuine
 *   capital allocation function alongside asymmetric fee extraction). The
 *   theater ratio (0.58) reflects that much of the justification for PE fee
 *   structures relies on narrative about 'value creation' and 'operational
 *   discipline' that has become increasingly performative as financial
 *   engineering and asset stripping have displaced genuine operational
 *   improvement.
 *
 * KEY AGENTS:
 *   - Private Equity Managers: Primary beneficiaries (institutional/arbitrage) — extract fees regardless of performance; full exit optionality via secondary markets and new fund launches
 *   - Portfolio Company Workers: Primary victims (powerless/trapped) — face job cuts, wage suppression, and asset stripping with no exit options; geographic and skill-specific barriers to mobility
 *   - Company Creditors: Secondary victims (moderate/constrained) — subordinated in capital stack; absorb default risk while PE managers extract fees first; constrained by credit market frictions
 *   - Institutional Investors (Limited Partners): Beneficiaries with constraints (powerful/arbitrage) — receive market-rate returns on capital but pay layered fees that reduce net returns; fiduciary duty lock-in creates suppression
 *   - Regulatory Reform Coalition: Organized agents (organized/constrained) — labor advocates, state regulators, reform legislators building fee transparency and clawback mechanisms with genuine sunset logic
 *   - Long-Term Value Creation: Victim (powerless/trapped) — abstract collective good; productive companies degraded through financial engineering; no mechanism for exit or resistance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals extraction mechanism independent of narrative justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_equity_fee_extraction, 0.68).
domain_priors:suppression_score(private_equity_fee_extraction, 0.65).
domain_priors:theater_ratio(private_equity_fee_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_equity_fee_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(private_equity_fee_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(private_equity_fee_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(private_equity_fee_extraction, snare).
narrative_ontology:human_readable(private_equity_fee_extraction, "Private Equity Fee Extraction Mechanism").
narrative_ontology:topic_domain(private_equity_fee_extraction, "financial/corporate_governance").

domain_priors:requires_active_enforcement(private_equity_fee_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_equity_fee_extraction, private_equity_managers).
narrative_ontology:constraint_victim(private_equity_fee_extraction, portfolio_companies).
narrative_ontology:constraint_victim(private_equity_fee_extraction, workers).
narrative_ontology:constraint_victim(private_equity_fee_extraction, creditors).
narrative_ontology:constraint_victim(private_equity_fee_extraction, long_term_value_creation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PORTFOLIO COMPANY WORKFORCE (SNARE) — Workers in acquired companies face severe extraction with minimal exit options. Job cuts, wage suppression, and asset stripping are structural features, not side effects. Trapped by geographic location, skill specificity, and labor market frictions. No ability to negotiate fee structures or capital allocation decisions. Experience maximum extraction during holding period.
constraint_indexing:constraint_classification(private_equity_fee_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPANY CREDITORS (SNARE) — Bondholders and lenders in leveraged buyout structures bear subordination risk while PE managers extract fees regardless of performance. Constrained exit through credit markets; high costs to refinance or restructure. Extraction flow is asymmetric: managers paid first from cash flow, creditors absorb losses. Suppression through information asymmetry and structural subordination in capital stack.
constraint_indexing:constraint_classification(private_equity_fee_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTOR / LIMITED PARTNER (TANGLED ROPE) — Pension funds and endowments benefit from PE returns but are partially locked into fee structures. Genuine coordination function: PE provides capital, market discipline, and operational improvement to underperforming companies. But layered fees (management fees, carried interest, transaction fees, monitoring fees) extract 6-8% annually before any performance is realized. Beneficiaries of the coordination function but also bear extraction costs. Arbitrage exit through secondary market sales, but at significant discounts. High organized power but constrained by regulatory fiduciary duties to maintain PE allocation.
constraint_indexing:constraint_classification(private_equity_fee_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVATE EQUITY MANAGER (ROPE) — Sees the constraint as pure coordination: raising capital pools, identifying undervalued companies, implementing operational improvements, and distributing returns is a genuine market function. Asymmetric compensation (2% management fee + 20% carried interest) appears as earned incentive alignment. Full arbitrage exit via secondary markets, fund liquidation, or launching new funds. Maximum beneficiary position. Extraction runs entirely toward this agent; they experience the constraint as enabling mechanism.
constraint_indexing:constraint_classification(private_equity_fee_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — State regulators, labor advocates, and reform-minded legislators are building alternative oversight mechanisms: fee transparency requirements, clawback provisions, pension fund divestment pressure, and carried-interest taxation. These constraints have sunset logic — if implemented, they reduce the extractiveness of the fee mechanism significantly. Constrained by incumbent industry influence and regulatory capture, but organized coalition with real policy leverage. Theater persists in regulatory capture dynamics but reforms are gradually increasing structural friction.
constraint_indexing:constraint_classification(private_equity_fee_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE 'VALUE CREATION' NARRATIVE (PITON) — The theoretical justification for PE fee structures is that managers add genuine value through operational improvements, strategic repositioning, and disciplined capital allocation. This narrative is increasingly performative: many acquisitions show value extraction rather than creation (asset stripping, financial engineering, dividend recapitalizations). The narrative persists through institutional inertia — PE retains legitimacy from historical cases where discipline worked (1980s-2000s) despite declining functional evidence. Theater ratio reflects that fee justifications are maintained through narrative rather than empirical performance demonstration.
constraint_indexing:constraint_classification(private_equity_fee_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the PE fee structure is a snare that extracts value from the real economy (workers, suppliers, creditors) and transfers it to financial intermediaries regardless of actual performance or value creation. The suppression mechanisms are structural: information asymmetry about actual returns, regulatory capture preventing fee transparency, and lock-in through pension fund fiduciary duties. No beneficial coordination function emerges at this level — the fees are pure extraction layered onto capital allocation. The analytical observer sees the constraint as a sophisticated rent-seeking mechanism sustained by narrative and institutional power.
constraint_indexing:constraint_classification(private_equity_fee_extraction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_equity_fee_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(private_equity_fee_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(private_equity_fee_extraction, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(private_equity_fee_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(private_equity_fee_extraction, TR),
    TR >= 0.70.

:- end_tests(private_equity_fee_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, with upward drift over the interval. The base extractiveness reflects that PE managers extract 2% annual fees plus 20% carried interest, transaction fees, monitoring fees, and distribution fees. These layers compound — a typical fund structure extracts 6-8% annually from portfolio assets before any performance is realized. The extractiveness increases over the interval (0.45 → 0.68) because: (1) fee-based models have systematized over time, (2) fund sizes have grown, concentrating fees, (3) regulatory clarity has not increased (no downward pressure), (4) carried interest taxation debates have not resulted in meaningful rate changes. Suppression (0.65): Moderate-high. Suppression mechanisms include: (1) information asymmetry about actual returns (institutional investors receive limited transparency on portfolio company operations), (2) regulatory capture (PE industry prevents fee transparency requirements), (3) legal subordination (creditors are structurally junior to fee claims), (4) lock-in effects (pension funds face fiduciary duty constraints that prevent exit even if fee structures are unfavorable), (5) narrative power (the 'value creation' story justifies extraction). Suppression is not absolute — some institutional investors can exit via secondary markets, and regulatory reform is building pressure — but structural barriers are substantial. Theater ratio (0.58): Moderate-high and increasing. The justification for PE fee structures centers on operational improvement and capital discipline. But empirical evidence shows declining alignment: (1) financial engineering (dividend recapitalizations, asset sales) has become more common than operational investment, (2) worker outcomes have deteriorated in many PE acquisitions (wages, benefits, safety violations increased), (3) leverage cycles show that PE relies on credit availability rather than genuine value creation, (4) recession and credit tightening trigger defaults that expose debt unsustainability. The theater ratio increases over the interval because the narrative of 'value creation' persists despite declining functional evidence — the institutional legitimacy of PE relies increasingly on performative justification rather than demonstrable operational improvement.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap separates managers' experience (rope/pure coordination) from workers' experience (snare/pure extraction). This gap emerges because: (1) Managers have institutional power, arbitrage exit, and align with the formal framing of the constraint ('capital allocation is necessary'), resulting in a perception of the fee structure as enabling market function. (2) Workers have powerless position, trapped exit, and bear all extraction costs with no decision-making power, resulting in perception of the fee structure as coercive rent extraction. (3) The same structural mechanism (leveraged acquisition, fee extraction, asset optimization) produces opposite valuations depending on position. (4) Information asymmetry suppresses workers' and creditors' ability to contest the structure — they see job cuts and wage suppression but not the fee architecture that drove these decisions. The institutional investor perspective (tangled rope) reveals the hybrid nature: genuine capital allocation function (operational improvement, discipline, strategic repositioning) coexists with extraction (layered fees, carried interest concentration, leverage subordination). This mixed experience is the analytical signal that the constraint contains both coordination and extraction components — it's not pure snare, but it's also not pure rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from structural relationships to the fee extraction flow. PE managers (institutional, arbitrage, beneficiary) have d ≈ 0.08 — full beneficiary with exit optionality, resulting in negative effective extraction f(d) ≈ -0.12. Institutional investors (powerful, arbitrage, mixed beneficiary/victim) have d ≈ 0.48 — they benefit from capital allocation but bear suppressed costs through fees, resulting in moderate f(d) ≈ 0.60. Portfolio company workers (powerless, trapped, victim) have d ≈ 0.95 — maximum subordination with no exit, resulting in maximum f(d) ≈ 1.42. Company creditors (moderate, constrained, victim) have d ≈ 0.75 — significant subordination but with some exit via credit market refinancing, resulting in f(d) ≈ 1.15. Regulatory reformers (organized, constrained, partial victim with policy leverage) have d ≈ 0.40 — organized power but constrained by incumbent capture, resulting in f(d) ≈ 0.40. The scope modifier σ(S) is global (σ=1.2) because PE operates at planetary scale and capital flows globally. The final effective extraction χ = ε × f(d) × σ(S) varies by perspective: for workers, χ ≈ 0.68 × 1.42 × 1.2 ≈ 1.16 (capped at 1.0 for reporting); for managers, χ ≈ 0.68 × (-0.12) × 1.2 ≈ -0.10 (negative extraction, pure benefit). This perspectival divergence is the diagnostic signature of a snare viewed from multiple positions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The private equity fee extraction constraint is classified as snare (extractiveness > 0.70, suppression > 0.60, χ > 0.66 from multiple perspectives), and mandatrophy is resolved by demonstrating that the snare classification does not collapse into 'all institutional extraction is snare.' Instead, the snare classification is perspectival and justified: from the worker's perspective, it is genuinely snare (no exit, maximum extraction, no coordination benefit). From the institutional investor's perspective, it is tangled rope (genuine coordination function + extraction). From the manager's perspective, it is rope (pure coordination). The mandatrophy is resolved by showing that the same constraint produces different classifications from different perspectives, and this perspectival divergence is not a classification error but a structural feature of how extraction operates when information asymmetry and power differentials are high. The snare classification identifies which agents bear the extraction burden and have no exit — not that the constraint is universally extractive. Regulatory reform (scaffold perspective) offers a real sunset mechanism: fee transparency, clawback provisions, and carried-interest taxation would reduce the snare's extractiveness significantly. The piton classification shows that the 'value creation' narrative is increasingly performative. The analytical observer's snare classification reveals the extraction mechanism independent of narrative justification. All these perspectives together resolve the mandatrophy: the constraint is snare for those who bear the extraction (workers, creditors), rope for those who benefit (managers), and tangled rope for institutional investors (mixed experience). No single classification is 'the truth' — the perspectival structure itself is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_improvement_attribution,
    'How much of PE portfolio company value increase is attributable to genuine operational improvements versus financial engineering and asset stripping?',
    'Longitudinal comparison of operational metrics (productivity, R&D investment, capacity utilization, worker productivity) for PE-acquired vs non-acquired peer companies; analysis of asset valuations before/after PE ownership',
    'If operational improvement dominates (>60%): tangled_rope classification gains strength — real coordination function exists alongside extraction. If financial engineering dominates (>60%): snare classification confirmed — no coordination function, pure extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_improvement_attribution, empirical, 'Attribution of value change to operational improvement vs financial engineering').

omega_variable(
    fee_performance_correlation,
    'Is there correlation between PE managers'' fee extraction rates and actual investor returns? Do higher fee structures predict higher or lower net returns to limited partners?',
    'Cross-fund performance analysis controlling for fund size, vintage year, and sector; regression of fee structure (2/20 vs 1.5/20 vs 2/25) against net IRR and MOIC; historical trend analysis of fees vs returns over time',
    'If high correlation with better returns: fee structure is justified incentive alignment. If no correlation or negative correlation: fees are extraction independent of performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fee_performance_correlation, empirical, 'Correlation between fee structure and net investor returns').

omega_variable(
    leverage_sustainability,
    'What proportion of PE portfolio company leverage is sustainable long-term vs creates default risk that creditors bear?',
    'Debt-to-cash-flow ratio analysis of PE-acquired companies at exit vs acquisition; default rate analysis during recession/credit tightening; creditor loss data from portfolio company failures',
    'If most leverage is sustainable: creditor suppression is moderate, extraction is bounded. If substantial leverage creates hidden default risk: creditor subordination is extraction mechanism, suppression is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leverage_sustainability, empirical, 'Sustainability of leverage in PE capital structures').

omega_variable(
    carried_interest_distribution,
    'Is carried interest accrual concentrated among senior partners/founders or distributed to junior investment professionals? Does the concentration measure affect experienced extractiveness?',
    'Fund partnership documentation analysis; carried interest distribution data from regulatory filings or insider reporting; comparison across firm size and maturity',
    'If concentrated at top: extraction mechanism is more pronounced and visible (concentrated rents). If distributed: rents appear as earned incentive compensation, suppression via narrative credibility is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carried_interest_distribution, empirical, 'Distribution of carried interest concentration within PE firms').

omega_variable(
    alternative_capital_structures,
    'Do alternative capital structures (mutual ownership, worker ownership, strategic buyer operations) achieve comparable returns to PE while reducing worker/creditor extraction?',
    'Comparative performance analysis of PE vs employee stock ownership plans (ESOPs) vs strategic buyers in same sectors; analysis of wage, employment, and debt outcomes',
    'If alternatives achieve comparable returns: PE fee extraction is not functionally necessary. If PE substantially outperforms: fee structure is partially justified by superior capital allocation despite extraction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capital_structures, empirical, 'Comparative performance of alternative capital structures').

omega_variable(
    institutional_investor_substitution_threat,
    'If fee transparency and clawback provisions were mandated, would institutional investors exit PE allocation significantly, or would PE market remain stable at lower fee structures?',
    'Institutional investor preference surveys; historical case studies of funds adopting fee transparency/clawbacks; stress test modeling of capital availability at different fee levels',
    'If investors would exit substantially: regulatory reform creates real market friction (scaffold sunset is real). If investors remain: fee structures are pure extraction not necessary for capital formation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_investor_substitution_threat, preference, 'Institutional investor response to fee regulation scenarios').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_equity_fee_extraction, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pefee_tr_t0, private_equity_fee_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pefee_tr_t5, private_equity_fee_extraction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pefee_tr_t10, private_equity_fee_extraction, theater_ratio, 10, 0.58).
narrative_ontology:measurement(pefee_tr_t15, private_equity_fee_extraction, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(pefee_be_t0, private_equity_fee_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pefee_be_t5, private_equity_fee_extraction, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(pefee_be_t10, private_equity_fee_extraction, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(pefee_be_t15, private_equity_fee_extraction, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_equity_fee_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(private_equity_fee_extraction, 0.18).
narrative_ontology:affects_constraint(private_equity_fee_extraction, leveraged_buyout_capital_structure).
narrative_ontology:affects_constraint(private_equity_fee_extraction, financial_engineering_incentives).
narrative_ontology:affects_constraint(private_equity_fee_extraction, worker_bargaining_power_erosion).

% DUAL FORMULATION NOTE:
% The private equity fee extraction constraint is upstream of specific acquisition outcomes (job cuts, asset stripping, debt defaults). The fee architecture creates systematic incentives for financial optimization over operational investment. The constraint family includes: (1) fee_extraction (this story, ε=0.68, snare), (2) operational_discipline_function (ε=0.35, rope when genuine, piton when performative — decompose), (3) leveraged_capital_structure (ε=0.55, tangled rope — genuine leverage benefits + default risk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(private_equity_fee_extraction, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
