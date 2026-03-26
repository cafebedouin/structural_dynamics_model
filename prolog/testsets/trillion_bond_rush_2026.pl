% ============================================================================
% CONSTRAINT STORY: trillion_bond_rush_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The $1 trillion bond issuance milestone in February 2026 represents a
 *   structural constraint that combines coordination (capital market access)
 *   with extraction (financial repression of savers, intergenerational debt
 *   transfer). The rapid pace reflects both genuine funding needs and
 *   policy-driven interest rate suppression that channels savings into
 *   government bonds at negative real yields. The constraint exhibits
 *   multiple classification types depending on observer position: pure
 *   extraction (snare) from the powerless saver or future taxpayer
 *   perspective; coordinating mechanism (rope) from the investment banker
 *   perspective; mixed coordination and enforcement (tangled rope) from the
 *   sovereign issuer perspective; performative surveillance (piton) from the
 *   ratings infrastructure; temporary support (scaffold) from the central
 *   bank perspective; and false naturalization (mountain) from analysts who
 *   frame the surge as inevitable rather than policy-contingent. The theater
 *   ratio (0.68) reflects that bond market surveillance and risk
 *   infrastructure provide continuous metrics and analysis but minimal
 *   predictive warning before credit events, while the extractiveness (0.58)
 *   captures the real transfer of purchasing power and intergenerational
 *   fiscal burden.
 *
 * KEY AGENTS:
 *   - Retail Savers: Primary victims (powerless/trapped) — forced into negative real yields; no exit from bond markets without accepting uncompensated risk
 *   - Future Taxpayers: Primary victims (powerless/trapped) — bear cost of sovereign debt accumulation via future austerity, inflation, or default
 *   - Investment Banking Syndicate: Primary beneficiary (institutional/arbitrage) — captures underwriting fees, trading spreads, and liquidity provision; can exit to other markets
 *   - Sovereign Fiscal Authorities: Mixed beneficiary/victim (organized/constrained) — benefit from capital access; constrained by roll-over risk and future liabilities; requires active enforcement of investor confidence
 *   - Central Banks: Policy actor (institutional/arbitrage) — provide temporary liquidity support and rate suppression; maintain exit option through tightening
 *   - Credit Rating Infrastructure: Institutional observer (institutional/arbitrage) — maintains performative risk assessment; surveillance persists through regulatory requirement despite predictive failures
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy-contingent arrangement as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trillion_bond_rush_2026, 0.58).
domain_priors:suppression_score(trillion_bond_rush_2026, 0.62).
domain_priors:theater_ratio(trillion_bond_rush_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trillion_bond_rush_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(trillion_bond_rush_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(trillion_bond_rush_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trillion_bond_rush_2026, snare).
narrative_ontology:human_readable(trillion_bond_rush_2026, "Global $1 Trillion Bond Issuance Record (2026)").
narrative_ontology:topic_domain(trillion_bond_rush_2026, "economic/financial").

domain_priors:requires_active_enforcement(trillion_bond_rush_2026).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trillion_bond_rush_2026, investment_banks).
narrative_ontology:constraint_beneficiary(trillion_bond_rush_2026, sovereign_issuers_credit_access).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, retail_savers).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, future_taxpayers).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, currency_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL SAVER (SNARE) — Trapped in negative real rates (yields below inflation). Cannot exit bond markets without accepting equity volatility or currency risk. Central bank suppression of rates creates extraction mechanism: purchasing power erodes while institutional investors arbitrage the liquidity glut. No alternative for capital preservation. Maximum experienced extraction.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE TAXPAYER (SNARE) — Trapped by sovereign debt accumulation. Rapid issuance accelerates debt service burden. No exit option — citizenship and tax liability are the constraints. Current political actors capture benefits (fiscal stimulus); future cohorts bear extraction through austerity, inflation, or default risk. Generational extraction.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INVESTMENT BANKING SYNDICATE (ROPE) — Benefits from underwriting fees, trading spreads, and liquidity provision. Experiences constraint as coordination mechanism: rapid issuance creates market standardization, settlement efficiency, and information discovery. Fee capture during issuance surge. Arbitrage exit — can shift to other markets or securities if returns compress. Net beneficiary.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FISCAL AUTHORITY (TANGLED ROPE) — Benefits from capital access and delayed fiscal adjustment. Constrained by roll-over risk, funding costs, and international scrutiny. Requires active enforcement of creditworthiness and investor confidence. The rapid issuance pace is both coordinating solution (market capacity to absorb supply) and extraction mechanism (negative carry on surplus fiscal needs). Mixed benefit and burden — coordination function (finding capital) overlaps with asymmetric extraction (future debt burden).
constraint_indexing:constraint_classification(trillion_bond_rush_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDIT RATING INFRASTRUCTURE (PITON) — Rating agencies and financial surveillance maintain performative risk assessment. Records $1T issuance with minimal downgrade activity — theater of risk management. Infrastructure persists through inertia (regulatory requirement for ratings) despite documented failures to predict crisis risk. High theater ratio: metrics, stress tests, and ratings published continuously but with minimal predictive power during surge events. Extraction mechanism is weak because enforcement is largely theatrical.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CENTRAL BANK LIQUIDITY FRAMEWORK (SCAFFOLD) — Provides temporary coordination through expanded balance sheets and forward guidance. Suppression is active (rate suppression via policy) but explicitly conditional: central banks signal tapering and eventual tightening. Sunset clause embedded: policy frameworks describe rate-normalization pathways. Theater ratio modest — substantive policy changes announced regularly. Coordination function is genuine (capital market stabilization); extraction is acknowledged as temporary. Scaffolding structure: support decreases as market capacity matures.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN — FALSE SUMMIT) — Some analysts naturalize the bond surge as 'inevitable given fiscal needs' or 'equilibrium of global capital supply and demand.' This perspective risks false summit: the issuance surge is not an immutable feature of economics but a contingent outcome of policy choices (rate suppression, fiscal expansion, capital flow patterns). The constraint's extractiveness is institutional, not natural. Engine should flag this as naturalization of a contingent arrangement.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trillion_bond_rush_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trillion_bond_rush_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trillion_bond_rush_2026, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Base extractiveness (0.58): Moderate-high. The constraint transfers purchasing power from savers to fiscal authorities and banks through rate suppression, fee capture, and intergenerational debt burden. Savers trapped in negative real yields experience direct extraction; future taxpayers face implicit extraction via debt accumulation. The extraction is not total (markets still function, capital still flows) but significant and structural. Suppression (0.62): Moderate-high. Central bank rate suppression creates the mechanism; regulatory framework channels savings into bonds; information asymmetries between issuers and retail savers limit exit options; currency risk deters capital flight. Theater ratio (0.68): High-moderate. Rating agencies publish continuous assessments, stress tests, and surveillance metrics. The issuance pace is accompanied by detailed financial communications. Yet predictive value is low — ratings remain investment-grade even as debt service ratios deteriorate; surveillance infrastructure produces output without proportional risk detection. The theater has increased as issuance volume has accelerated: more metrics, more analysis, less warning.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (investment banks, fiscal authorities with access) perceive the constraint as rope (coordination) or tangled rope (mixed) — they see capital market efficiency and beneficial access. Victims (savers, future taxpayers) perceive snare — extraction without exit. The central bank sees its own framework as temporary support (scaffold) with sunset pathways through normalization; markets increasingly doubt the sunset timeline. The ratings infrastructure sees its own role as piton — the surveillance ritual persists despite documented failure to predict credit events (false positive on creditworthiness during surge). The analytical observer risks seeing mountain (natural law of debt dynamics) when the reality is institutional choice (rate suppression policy). The perspectival gap reveals that the same $1T issuance event is simultaneously: a genuine coordination solution (capital for fiscal needs), an extraction mechanism (real rates negative for savers), a temporary support structure (central bank liquidity) with uncertain sunset, and a policy choice (rate suppression) that could be altered.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position. Savers with trapped exit (cannot leave bond markets; currency risk, volatility risk) and victim status (negative real yields) derive high d (~0.90-0.95) → high f(d) → high experienced extraction. Future taxpayers trapped by tax obligation and bearing intergenerational cost derive high d. Investment bankers with arbitrage exit (can shift to equities, alternative assets) and beneficiary status (fee capture) derive low d (~0.10-0.15) → negative f(d) → net positive experience despite formal extraction. Sovereign issuers with constrained exit (cannot default without cost but have fiscal options) and mixed beneficiary/victim status (capital access but future burden) derive moderate-high d (~0.55-0.65) → moderate f(d) → moderate extraction experienced. Central banks with policy arbitrage and temporary-support intent derive d override: canonical institutional (0.00) adjusted upward to ~0.25 to reflect that central bank rate suppression actively creates the extraction mechanism (not passive beneficiary; not pure victim; designer of constraint). This override captures the paradox: central banks appear neutral but actively suppress real rates.
 *
 * MANDATROPHY ANALYSIS:
 *   The $1T bond surge tests the mandatrophy framework by asking: is this constraint pure coordination (rope), temporary support (scaffold), mixed (tangled rope), or pure extraction (snare)? The answer varies by observer and resolves the mandatrophy through perspectival decomposition. From the saver's view: snare (pure extraction, trapped, no beneficiary role). From the bank's view: rope (coordination of capital flows). From the fiscal authority's view: tangled rope (coordination function of accessing capital + extraction function of future burden). From the central bank's view: scaffold (temporary support with sunset). From the analytical observer's view: the constraint is policy-contingent, not natural — the 'mountain' framing is a false summit to be rejected. The mandatrophy is resolved not by choosing one type but by recognizing that all are structurally valid from different indices, and the presheaf over those indices IS the complete description of the constraint. The theatrical aspect (continuous surveillance, detailed metrics, low warning power) is the signature of institutional legitimation masking underlying extraction — classic Piton mechanism wearing the costume of analytical rigor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rate_normalization_timeline,
    'When will central banks normalize rates sufficiently to eliminate negative real rates and restore saver returns?',
    'Central bank communications, inflation dynamics, labor market tightening. Observable: when nominal yields sustainably exceed inflation + risk premium.',
    'If normalization occurs within 2-3 years: snare classification is temporary (scaffold framework validates). If delayed beyond 5 years: snare extraction becomes entrenched (institutional preference for financial repression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rate_normalization_timeline, empirical, 'Timeline for interest rate normalization and real return restoration').

omega_variable(
    debt_sustainability_threshold,
    'What sovereign debt-to-GDP level triggers fiscal restructuring or austerity in major economies?',
    'Bond market pricing (spreads, duration), credit rating actions, fiscal policy shifts. Observable: when government borrowing costs exceed nominal GDP growth.',
    'If threshold is very high (>150% debt/GDP): future taxpayer extraction is delayed, snare classification softens. If threshold is moderate (~100-120%): extraction accelerates within current generational cohort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability_threshold, empirical, 'Sovereign debt sustainability threshold before fiscal crisis').

omega_variable(
    alternative_financing_capacity,
    'Can fiscal authorities shift to other financing mechanisms (MMT, asset taxes, currency debasement) that bypass bond markets entirely?',
    'Policy experimentation in specific jurisdictions, inflation outcomes, currency stability. Observable: whether alternative financing mechanisms scale without triggering currency or inflation crises.',
    'If alternatives scale: investment banking beneficiary role diminishes, snare structure weakens. If alternatives fail or trigger crises: bond issuance remains the enforced mechanism, snare deepens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_financing_capacity, conceptual, 'Whether alternative fiscal financing mechanisms can bypass bond markets').

omega_variable(
    private_demand_sustainability,
    'Is the $1T/year issuance pace sustainable by private investor demand, or does it rely on central bank balance sheet expansion and financial repression of rates?',
    'Central bank holdings as % of outstanding bonds, private investor survey data, real yield levels. Observable: when private demand alone cannot absorb issuance at market rates.',
    'If demand is private and genuine: rope classification (coordination) is primary. If demand is suppressed-rate-driven (savers forced into bonds): snare classification is primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_demand_sustainability, empirical, 'Whether bond issuance pace is sustainable by private investor demand alone').

omega_variable(
    currency_reserve_concentration,
    'Does the $1T issuance increase global reliance on USD and USD-bond holdings as reserve assets, or does it distribute across currencies?',
    'Currency composition of central bank reserves, yield curves across currencies, trade finance patterns. Observable: when non-USD bonds capture >30% of new issuance.',
    'If USD-concentrated: currency stability constraint becomes entrenched (seigniorage extraction). If diversified: currency risk is distributed, snare mechanism weakens globally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(currency_reserve_concentration, empirical, 'Currency reserve concentration and seigniorage effects of bond issuance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trillion_bond_rush_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tbr_tr_t0, trillion_bond_rush_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(tbr_tr_t6, trillion_bond_rush_2026, theater_ratio, 6, 0.64).
narrative_ontology:measurement(tbr_tr_t12, trillion_bond_rush_2026, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(tbr_be_t0, trillion_bond_rush_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tbr_be_t6, trillion_bond_rush_2026, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(tbr_be_t12, trillion_bond_rush_2026, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trillion_bond_rush_2026, resource_allocation).
narrative_ontology:affects_constraint(trillion_bond_rush_2026, negative_real_yields_entrapment).
narrative_ontology:affects_constraint(trillion_bond_rush_2026, sovereign_debt_sustainability_cliff).
narrative_ontology:affects_constraint(trillion_bond_rush_2026, currency_reserve_concentration_risk).

% DUAL FORMULATION NOTE:
% The $1T bond surge decomoses into three structurally distinct constraints: (1) negative real yields (direct saver extraction, ε~0.65, snare), (2) sovereign debt accumulation (intergenerational extraction, ε~0.52, tangled rope), (3) currency concentration (systemic extraction, ε~0.48, tangled rope). This constraint story models the issuance event itself (ε=0.58, snare primary) and its effects on capital flows. Upstream: central bank rate suppression policy (ε~0.45, scaffold). Downstream: inflation dynamics and fiscal crisis thresholds (ε varies by scenario). Each story has distinct beneficiaries, victims, and empirical signatures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trillion_bond_rush_2026, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
