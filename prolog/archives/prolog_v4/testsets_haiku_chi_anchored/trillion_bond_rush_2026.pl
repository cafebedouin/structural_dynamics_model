% ============================================================================
% CONSTRAINT STORY: trillion_bond_rush_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trillion_bond_rush_2026, []).

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
 *   constraint_id: trillion_bond_rush_2026
 *   human_readable: Global $1 Trillion Bond Issuance Record (2026)
 *   domain: economic/financial
 *
 * SUMMARY:
 *   Global bond issuance reached $1 trillion on February 2, 2026, marking the
 *   fastest pace in financial history. This constraint represents a
 *   structural tension between legitimate financial coordination (sovereigns
 *   and corporations locking in favorable borrowing terms during a low-rate
 *   environment) and extraction mechanisms (future debt servicing burdens,
 *   destruction of retail savings yields, and concentration of fiscal risk).
 *   The issuance surge emerges from a decade of monetary suppression
 *   (near-zero rates justified as financial stability) that simultaneously
 *   creates incentives for massive borrowing. Central banks explicitly
 *   designed this window as temporary (forward guidance signals rate
 *   normalization), but market participants are rushing to issue before rates
 *   rise. The constraint exhibits all characteristics of a Tangled Rope:
 *   genuine coordination function (efficient capital mobilization during
 *   favorable window) coupled with asymmetric extraction (future taxpayers
 *   and retail savers bear costs; present-day issuers and financial
 *   intermediaries capture benefits). The theater ratio (0.58) reflects that
 *   credit rating agencies continue to publish assessments despite model
 *   obsolescence—their ratings are largely performative, providing
 *   institutional legitimacy for issuance rather than meaningful risk
 *   differentiation. From the perspective of future taxpayers and retail
 *   savers, the constraint functions as a Snare: trapped by decisions made in
 *   preceding period, no exit capacity, compounding obligations. From the
 *   perspective of investment banks and primary issuers, it functions as a
 *   Rope: pure coordination mechanism enabling efficient market access. The
 *   analytical observer sees the constraint as Tangled Rope: coordinating
 *   legitimate borrowing needs while simultaneously enabling unsustainable
 *   debt accumulation.
 *
 * KEY AGENTS:
 *   - Primary Bond Issuers (large sovereigns, investment-grade corporates): Institutional/arbitrage — capture low borrowing costs and funding access during favorable window
 *   - Investment Banks & Underwriters: Institutional/arbitrage — gatekeepers and transaction processors; capture spreads, fees, advisory mandates
 *   - Retail Savers: Powerless/trapped — suffer yield suppression and forced migration to riskier assets; cannot exit low-yield trap
 *   - Future Taxpayers (Fiscal Commons): Powerless/trapped — inherit debt servicing obligations from present issuance surge; no exit from compounding fiscal constraints
 *   - Mid-Tier Sovereigns: Organized/constrained — benefit from temporary access window but subordinated to larger issuers in rate competition
 *   - Central Banks: Organized/constrained — designed the near-zero rate environment as temporary coordination device; attempting to engineer exit via forward guidance
 *   - Credit Rating Agencies: Institutional/arbitrage — maintain institutional legitimacy for issuance despite degraded analytical capacity
 *   - Analytical Observer: Analytical/analytical — sees both coordination value and extraction dynamics; classifies as Tangled Rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trillion_bond_rush_2026, 0.52).
domain_priors:suppression_score(trillion_bond_rush_2026, 0.65).
domain_priors:theater_ratio(trillion_bond_rush_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trillion_bond_rush_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(trillion_bond_rush_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(trillion_bond_rush_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trillion_bond_rush_2026, tangled_rope).
narrative_ontology:human_readable(trillion_bond_rush_2026, "Global $1 Trillion Bond Issuance Record (2026)").
narrative_ontology:topic_domain(trillion_bond_rush_2026, "economic/financial").

domain_priors:requires_active_enforcement(trillion_bond_rush_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trillion_bond_rush_2026, primary_bond_issuers).
narrative_ontology:constraint_beneficiary(trillion_bond_rush_2026, investment_banks).
narrative_ontology:constraint_beneficiary(trillion_bond_rush_2026, wealth_concentration_actors).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, future_debt_servicing_capacity).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, sovereign_fiscal_space).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, retail_savers_low_yield_trap).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISCAL COMMONS (SNARE) — Future sovereigns are trapped by debt accumulation. Governments racing to issue while rates are favorable (perceived as temporary) lock in obligations that constrain fiscal capacity for a generation. No exit: taxpayers born into debt servicing requirements. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.77.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RETAIL SAVERS (SNARE) — Trapped in yield scarcity. Central bank rate suppression (justified as financial stability) makes safe savings impossible; savers forced into riskier assets or inflation loss. Bond issuance surge absorbs available capital into institutional channels, further constraining retail alternatives. d≈0.88, f(d)≈1.35, σ=1.1 → χ≈0.78.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SOVEREIGN DEBT ISSUERS (TANGLED ROPE) — Smaller sovereigns experience mixed coordination and extraction. The issuance surge creates window of favorable access (coordination benefit), but also creates crowding effects and rate competition. Constrained by fiscal needs and inability to issue outside this window. Beneficiary from temporary coordination window, victim of subordination to larger issuers. d≈0.58, f(d)≈0.68, σ=1.2 → χ≈0.44.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIMARY BOND ISSUERS (ROPE) — Large sovereigns (US, EU, Japan) see the issuance surge as pure coordination: locking in favorable borrowing terms while rates are low is legitimate fiscal optimization. Full exit optionality: can issue or wait. Arbitrage position: can access multiple markets. Benefits from the coordination mechanism (efficient capital mobilization). d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INVESTMENT BANKS (ROPE) — Gatekeepers capture transaction fees and commissions. The surge is pure benefit: underwriting spreads, advisory mandates, capital markets activity. Full exit and arbitrage: can route to equity, derivatives, or forex markets. See the constraint as optimal coordination: matching borrowers to investors efficiently. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CENTRAL BANKS (SCAFFOLD) — Organized actors see the issuance surge as a temporary problem requiring sunset intervention. Forward guidance rate hikes and quantitative tightening are designed to exit from near-zero rates, reducing the incentive for this surge. The suppression (near-zero rates forcing issuance) has a designed exit: rates normalize, issuance slows. Constrained by inflation mandate and financial stability requirements. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.26.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CREDIT RATING AGENCIES (PITON) — Degraded institutional actors performing theater. Ratings persist despite obsolescence: agency models cannot distinguish between coordinated issuance and unsustainable debt accumulation. Theater_ratio=0.58 (59% performative content): publish ratings that issuers shop for, revise slowly, and fail to predict defaults. Persist through institutional inertia (market convention) and regulatory mandate (Basel III requires ratings). d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the issuance surge exhibits both coordination (efficient capital mobilization during favorable window) and extraction (concentration of debt burden on future fiscal capacity, retail savings destruction). The constraint is neither pure law nor pure contingency: financial markets require some debt issuance coordination, but the current pace embeds asymmetric extraction. d≈0.65, f(d)≈0.95, σ=1.2 → χ≈0.59.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trillion_bond_rush_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trillion_bond_rush_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trillion_bond_rush_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trillion_bond_rush_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trillion_bond_rush_2026, TR),
    TR >= 0.70.

:- end_tests(trillion_bond_rush_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The surge exhibits substantial extraction: future fiscal capacity is mortgaged to present borrowing rates; retail savings yields are permanently suppressed; debt servicing obligations compound across generations. However, extractiveness is not maximal (≤0.66 snare threshold) because part of the issuance reflects legitimate rate-locking during a genuine rate window, not pure predation. The trajectory shows increase from 0.28 (2022: normal issuance pace) to 0.52 (2026: panic rush). Suppression (0.65): High. Near-zero central bank rates force issuance as only available return on capital; retail savers have no legitimate alternatives; sovereigns face competitive pressure to issue before rate normalization; credit rating opacity suppresses transparency. However, suppression is not absolute—some market participants (wealthy investors with alternative access, corporate treasuries with cash flows) retain optionality. Theater ratio (0.58): Moderate-high. Credit rating agencies provide institutional legitimacy for issuance despite models that cannot distinguish between sustainable and unsustainable debt. Financial media narratives emphasize 'locking in favorable rates' (performative framing) rather than examining whether accumulated debt is serviceable. However, some genuine structural analysis occurs: fiscal sustainability discussions, debt trajectory modeling, so theater is not dominant (≥0.70 piton threshold).
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence. Primary issuers and investment banks see pure Rope (coordination mechanism). Future taxpayers see pure Snare (extraction trap). Retail savers see Snare (low-yield constraint). Mid-tier sovereigns see Tangled Rope (mixed coordination and subordination). Central banks see Scaffold (temporary intervention with designed sunset via rate normalization). Credit rating agencies see no constraint at all—theater is so dominant they perceive only normal market function. The analytical observer sees Tangled Rope: genuine coordination value (rate-locking during favorable window) coupled with extraction mechanism (unsustainable debt accumulation). The gap reflects structural reality: different agents occupy incompatible positions relative to the constraint. For issuers, the constraint solves a problem (accessing capital at favorable rates). For savers and future taxpayers, the constraint creates a problem (yield scarcity, debt servicing burden).
 *
 * DIRECTIONALITY LOGIC:
 *   Primary issuers & investment banks: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.09. Net beneficiaries; capture value from spread and financing access. Retail savers: Victim + trapped → d≈0.88, f(d)≈1.35. Maximum extraction; cannot exit yield suppression. Future taxpayers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; born into debt obligations. Mid-tier sovereigns: Mixed (beneficiary from access + victim from subordination) + constrained → d≈0.58, f(d)≈0.68. Moderate extraction; some benefit from window, significant cost from being subordinated. Central banks: Organized + constrained → d≈0.42, f(d)≈0.42. Low extraction; designed the constraint as temporary intervention. Credit rating agencies: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification derives from theater gate, not directionality. Analytical observer: analytical → d≈0.65, f(d)≈0.95. Captures coordination value and extraction symmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint exhibits both genuine coordination function AND asymmetric extraction. Coordination function: sovereigns and corporations are legitimately optimizing fiscal positions during a favorable rate window; this solves a real collective action problem (how to mobilize capital efficiently). Asymmetric extraction: the coordination window is artificially created by central bank rate suppression; when rates normalize (designed exit), future borrowers will face higher costs; retail savers bear the cost of yield suppression; future taxpayers inherit debt service burdens. The Tangled Rope classification prevents two mislabelings: (1) classifying as pure Rope by ignoring extraction dynamics and future fiscal risk, and (2) classifying as pure Snare by ignoring that some issuance reflects legitimate rate optimization. The mandatrophy is resolved by acknowledging that the constraint is genuinely mixed: it coordinates capital efficiently while simultaneously enabling unsustainable debt accumulation. Central banks attempted to manage this by designing a sunset (forward guidance toward rate normalization), creating a Scaffold perspective; if central banks fail to deliver on this exit, the constraint becomes pure Tangled Rope indefinitely, and extraction increases significantly. The analytical observer's Tangled Rope classification is the binding constraint—it captures the structural reality that cannot be escaped by focusing on either the coordination or extraction dimensions alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rate_normalization_timeline,
    'What is the actual path for interest rate normalization, and at what threshold does the issuance rush collapse?',
    'Central bank forward guidance credibility; market expectations data; actual rate trajectory relative to consensus forecasts',
    'If rates normalize within 18 months: Scaffold sunset is real, issuers face higher debt servicing costs immediately. If rates remain near-zero beyond 2027: Snare dynamics deepen as fiscal space constraints accumulate without relief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rate_normalization_timeline, empirical, 'Timeline and trajectory of interest rate normalization').

omega_variable(
    fiscal_sustainability_threshold,
    'At what debt-to-GDP ratio do fiscal constraints become binding, and for which sovereigns first?',
    'Debt trajectory modeling; fiscal space assessments from IMF/World Bank; market-implied probability of fiscal stress from credit default swap spreads',
    'If threshold breached within 3 years: Tangled Rope and Snare classifications will intensify; Future Taxpayers perspective shifts from trapped to economically devastated. If threshold holds beyond 5 years: extraction may be sustainable in structural terms (though normatively problematic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_threshold, empirical, 'Fiscal sustainability threshold and timeline to binding constraints').

omega_variable(
    retail_savings_alternatives_emergence,
    'Do alternative savings mechanisms (crypto, real assets, non-yield stores of value) provide genuine exit from the low-yield trap, or are they higher-risk substitutes?',
    'Empirical tracking of retail capital flows; correlation of retail asset allocations with yield suppression; volatility and drawdown analysis of alternatives',
    'If alternatives are genuine exits: Retail Savers move from Snare to Tangled Rope (constrained but with some agency). If alternatives collapse or are sufficiently risky: Retail Savers remain in Snare, may shift to Powerless from Moderate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retail_savings_alternatives_emergence, empirical, 'Whether non-traditional savings vehicles provide effective exit from yield suppression').

omega_variable(
    issuance_coordination_vs_panic,
    'Is the $1 trillion surge rational coordinated borrowing (locking favorable rates), or is it panic-driven competitive rushing (tragedy of the commons)?',
    'Motivation analysis of issuer statements; correlation of issuance timing with rate expectations; comparison to historical rate-locking episodes',
    'If rational coordination: classification remains Tangled Rope (some coordination value). If panic rush: classification shifts toward Snare across more perspectives; extraction increases as coordination function disappears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(issuance_coordination_vs_panic, conceptual, 'Whether issuance surge reflects rational coordination or panic-driven collective action problem').

omega_variable(
    central_bank_credibility_collapse,
    'If central banks fail to deliver on forward guidance (maintain low rates despite commitment to hikes), does the Scaffold sunset clause fail?',
    'Central bank action vs forward guidance correlation; market expectations of follow-through; policy reversals or extensions',
    'If credibility collapses: Scaffold perspective invalidates; constraint becomes Tangled Rope indefinitely. Future Taxpayers shift from generational to civilizational horizon. Extraction deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_bank_credibility_collapse, empirical, 'Central bank credibility on interest rate normalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trillion_bond_rush_2026, 2022, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tbr_tr_t0, trillion_bond_rush_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tbr_tr_t4, trillion_bond_rush_2026, theater_ratio, 4, 0.5).
narrative_ontology:measurement(tbr_tr_t8, trillion_bond_rush_2026, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(tbr_be_t0, trillion_bond_rush_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tbr_be_t4, trillion_bond_rush_2026, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(tbr_be_t8, trillion_bond_rush_2026, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trillion_bond_rush_2026, resource_allocation).
narrative_ontology:affects_constraint(trillion_bond_rush_2026, central_bank_rate_suppression).
narrative_ontology:affects_constraint(trillion_bond_rush_2026, sovereign_fiscal_space_contraction).
narrative_ontology:affects_constraint(trillion_bond_rush_2026, retail_yield_scarcity_trap).

% DUAL FORMULATION NOTE:
% The $1 trillion bond rush is downstream of central bank monetary policy (near-zero rate regime) but represents a distinct constraint with its own ε. The issuance surge has ε=0.52 (moderate-high extraction); the underlying rate suppression has ε≈0.35-0.45 (coordination with extraction side effects). These are linked: rate suppression creates issuance incentives, issuance surge locks in future debt obligations, which constrains fiscal space. Network decomposition captures this dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trillion_bond_rush_2026, powerless, 0.92).
constraint_indexing:directionality_override(trillion_bond_rush_2026, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
