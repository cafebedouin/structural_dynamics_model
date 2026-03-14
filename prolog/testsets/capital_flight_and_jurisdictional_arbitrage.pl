% ============================================================================
% CONSTRAINT STORY: capital_flight_and_jurisdictional_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capital_flight_and_jurisdictional_arbitrage, []).

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
 *   constraint_id: capital_flight_and_jurisdictional_arbitrage
 *   human_readable: Capital Flight and Jurisdictional Arbitrage
 *   domain: economic/political/regulatory
 *
 * SUMMARY:
 *   Capital flight and jurisdictional arbitrage represent a structural
 *   asymmetry in the modern global economy: capital can migrate across
 *   borders seeking tax advantage, but labor and immobile public asset
 *   holders cannot. This constraint generates a race-to-the-bottom in
 *   corporate tax rates, erodes fiscal sovereignty, and shifts tax burden
 *   from capital to labor. The constraint exhibits all six DR types across
 *   different perspectives. From the perspective of mobile capital holders
 *   and multinational corporations, the system functions as pure coordination
 *   (Rope) — jurisdictions compete to attract investment through tax
 *   incentives, and capital flows to high-return locations. From the
 *   perspective of welfare states seeking to fund public services, it is a
 *   tangled hybrid of coordination (attracting investment is genuinely
 *   necessary) and extraction (capital mobility undermines the fiscal base).
 *   From the perspective of immobile populations dependent on public
 *   services, it is pure extraction (Snare) with trapped exit options. Global
 *   tax cooperation coalitions (OECD, EU) see this as a temporary
 *   coordination failure with a sunset clause (Scaffold) — minimum tax
 *   agreements and country-by-country reporting are building enforcement
 *   mechanisms. The national tax state itself represents a piton (Piton) —
 *   the institution persists through inertia while its primary function
 *   (taxing capital) has atrophied. An analytical observer at civilizational
 *   timescale risks naturalizing tax competition as an immutable law of
 *   economics when it is actually a contingent institutional arrangement.
 *
 * KEY AGENTS:
 *   - Mobile Capital Holders and Multinationals: Primary beneficiary (institutional/arbitrage) — capture arbitrage returns and benefit from tax competition. Net positive directionality.
 *   - Immobile Labor Force: Primary victim (powerless/trapped) — cannot exit jurisdictions and bear wage pressure and tax burden shift. Maximum extraction experienced.
 *   - Public Services Dependent Populations: Secondary victim (powerless/trapped) — retirees, disabled, children depend on tax-financed services that erode as capital flees. Generational snare.
 *   - High-Tax Welfare States: Intermediate victim (organized/constrained) — face coordination necessity (attracting investment) alongside extraction cost (capital mobility limits fiscal tools). Cannot win tax competition.
 *   - Low-Tax Jurisdictions and Tax Havens: Secondary beneficiary (organized/mobile) — benefit from capital attraction and tax revenue concentration. Experience rope classification.
 *   - Small and Medium Enterprises: Tertiary victim (moderate/constrained) — experience both coordination benefit and extraction cost; cannot easily relocate but face competitiveness pressure from multinational tax avoidance.
 *   - Global Tax Coalitions: Organized agents (organized/constrained) — OECD, EU, developing-country coalitions building alternative enforcement mechanisms with sunset logic.
 *   - National Tax Administrations: Institutional actor (institutional/analytical) — maintain degraded institutions whose primary function (capital taxation) has atrophied. Theater persists through inertia.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable economic laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_flight_and_jurisdictional_arbitrage, 0.58).
domain_priors:suppression_score(capital_flight_and_jurisdictional_arbitrage, 0.68).
domain_priors:theater_ratio(capital_flight_and_jurisdictional_arbitrage, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_flight_and_jurisdictional_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(capital_flight_and_jurisdictional_arbitrage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(capital_flight_and_jurisdictional_arbitrage, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_flight_and_jurisdictional_arbitrage, tangled_rope).
narrative_ontology:human_readable(capital_flight_and_jurisdictional_arbitrage, "Capital Flight and Jurisdictional Arbitrage").
narrative_ontology:topic_domain(capital_flight_and_jurisdictional_arbitrage, "economic/political/regulatory").

domain_priors:requires_active_enforcement(capital_flight_and_jurisdictional_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_flight_and_jurisdictional_arbitrage, mobile_capital_holders).
narrative_ontology:constraint_beneficiary(capital_flight_and_jurisdictional_arbitrage, low_tax_jurisdictions).
narrative_ontology:constraint_victim(capital_flight_and_jurisdictional_arbitrage, high_tax_jurisdictions).
narrative_ontology:constraint_victim(capital_flight_and_jurisdictional_arbitrage, immobile_labor_force).
narrative_ontology:constraint_victim(capital_flight_and_jurisdictional_arbitrage, public_services_dependent_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILE LABOR FORCE (SNARE) — Workers and residents cannot exit jurisdictions seeking relocation due to family ties, housing constraints, language barriers, or immigration restrictions. They bear the full cost of capital flight through reduced public services, wage pressure, and tax burden shift to labor. No coordination benefit — pure extraction with maximum suppression.
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SERVICES DEPENDENT POPULATIONS (SNARE) — Social safety net beneficiaries, retirees, disabled persons, and children depend on tax-financed public services. Capital flight erodes the tax base without reducing service demand. They face trapped exit (cannot relocate to follow jobs or services) and high suppression (no alternatives to public provision). Pure extraction across generational timescale.
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL AND MEDIUM ENTERPRISES (TANGLED ROPE) — SMEs experience both coordination benefit (access to EU single market, tax treaties) and extraction cost (they cannot easily migrate to low-tax jurisdictions but face competitiveness pressure from multinational tax avoidance). Constrained exit due to operational embeddedness and supply chain ties. Active enforcement (transfer pricing rules, BEPS minimum tax) is required; suppression is moderate (some relocation possible but costly).
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: MOBILE CAPITAL HOLDERS AND MULTINATIONALS (ROPE) — Capital-rich actors experience the constraint as pure coordination: jurisdictions compete to attract capital through low tax rates, creating a coordination mechanism that allocates capital to high-return locations. Effective exit options (relocate operations, shift profits, establish subsidiaries in low-tax zones) create low d. Benefits from the constraint flow toward this agent. No victim status — perceives coordination benefit.
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-TAX JURISDICTIONS (TANGLED ROPE) — Welfare states derive 35-45% of revenue from capital and corporate taxes. They have genuine coordination interest (attracting investment for growth) but face asymmetric extraction (capital mobility undermines fiscal sovereignty). Constrained exit: cannot fully abandon tax competition without losing investment but cannot win the race. Active enforcement required (OECD BEPS, digital tax, country-by-country reporting). Suppression is high (jurisdictions have limited tools to prevent arbitrage without driving capital away).
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LOW-TAX JURISDICTIONS AND TAX HAVENS (ROPE) — For these actors, the constraint is pure coordination: they compete on tax arbitrage and capital capture with genuine mutual benefit (Luxembourg, Ireland, Singapore capture mobile capital and create employment). No victim status — experience low extraction and high coordination benefit. Mobile exit options (can shift tax policy) create moderate d, producing rope classification.
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: GLOBAL TAX COOPERATION COALITIONS (SCAFFOLD) — OECD, EU, and nascent developing-country coalitions (India-led South Center) see capital flight as a temporary coordination failure with a sunset clause. Mechanisms like BEPS, digital tax agreements, minimum corporate tax (Pillar 2), and country-by-country reporting are building alternative verification pathways. Exit is constrained (coalitions require consensus) but the perspective sees declining theater as enforcement mechanisms mature. Sunset clause is real: 15% minimum tax (2024 agreement) and unilateral digital taxes reduce arbitrage opportunities.
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: NATIONAL TAX SYSTEMS (PITON) — Individual nation-states maintain tax administrations and corporate tax structures that are largely degraded — their primary function (capturing capital gains) has atrophied as capital became mobile. The theater persists through institutional inertia (bureaucracies, legal precedent, electoral symbolism) despite reduced capacity to collect from mobile capital. Taxation of capital is performative in many jurisdictions. Only labor and immobile assets bear real tax burden.
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some capital mobility is inherent to modern finance: differential tax rates across jurisdictions will always create arbitrage incentives, and any regulatory system faces irreducible enforcement limits at scale. This perspective risks naturalizing what is actually a contingent institutional arrangement (nation-state control of capital taxation) as an immutable law. The engine's false summit detector will flag this as naturalization rather than genuine NL constraint.
constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_flight_and_jurisdictional_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_flight_and_jurisdictional_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_flight_and_jurisdictional_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capital_flight_and_jurisdictional_arbitrage, TR),
    TR >= 0.70.

:- end_tests(capital_flight_and_jurisdictional_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The measurement trajectory shows acceleration from 1980 (0.12) through 2010 (0.52) as capital mobility increased, financial engineering matured, and digital commerce created new arbitrage vectors. The 2024 value (0.54) reflects slight decline from 2020 peak due to OECD minimum tax and digital tax agreements taking effect, but the constraint remains substantially extractive. The core extraction mechanism is profit-shifting and capital relocation by multinationals, estimated at 4-13% of global corporate tax revenue annually. Suppression (0.68): High. Trapped exit options for labor and immobile populations mean suppression is structural and severe. High-tax jurisdictions face institutional suppression (limited tools without losing capital). Only mobile capital holders have low suppression (arbitrage exit options). Theater ratio (0.55): Moderate. Some performative elements: national tax codes maintain symbolic corporate tax rates while real effective rates are negotiated through transfer pricing and tax planning. Enforcement theater is increasing (country-by-country reporting, minimum tax compliance boards) but enforcement capacity remains below necessity. The theater is functional rather than purely decorative — it coordinates minimum standards — unlike the pure theater of a piton.
 *
 * PERSPECTIVAL GAP:
 *   The constraint simultaneously classifies as Snare (from immobile labor perspective), Tangled Rope (from SME and welfare-state perspectives), Rope (from mobile capital perspective), Scaffold (from global coalition perspective), and Piton (from national tax system perspective). This perspectival multiplicity reflects genuine structural differences in agent relationships to the constraint, not merely disagreement. The bundling of beneficiaries (mobile capital, low-tax jurisdictions) and victims (immobile labor, welfare states, SMEs) in a single constraint is the tangled rope signature — both coordination and extraction exist in the same mechanism. No single perspective collapses the others; each reveals a real structural dimension.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. Mobile capital holders are beneficiaries with arbitrage exit options → low d (approximately 0.15) → negative f(d) → negative effective extraction (they experience benefits flowing toward them). Immobile labor force are victims with trapped exit → high d (approximately 0.95) → high f(d) ≈ 1.42 → multiplied effective extraction (maximum experienced extraction). High-tax jurisdictions are victims but with constrained (not trapped) exit and some beneficiary function (capital attraction) → moderate d (approximately 0.60) → moderate f(d) ≈ 0.80 → moderate experienced extraction. SMEs are victims with constrained exit but some beneficiary function (market access, trade treaties) → moderate-high d (approximately 0.70) → moderate-high f(d) ≈ 1.00 → moderate-high experienced extraction. Scope modifier σ(S) applies: this is a global constraint (σ = 1.2), amplifying all χ values by 20%. Global-scale enforcement ambiguity increases effective extraction pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE MANDATROPHY: Does capital flight and tax arbitrage coordinate capital allocation or merely extract from immobile populations? The constraint resolves the mandatrophy by showing that BOTH are true depending on the observer's position. For mobile capital holders, the system coordinates capital allocation efficiently toward high-return jurisdictions — this is the real coordination function. For immobile populations and dependent jurisdictions, the system extracts without coordination — they bear costs while seeing no reciprocal benefit. The analytical requirement is to measure the ratio of coordination function to extraction overhead. If capital actually flows to higher-productivity locations, some coordination function exists. If capital flows primarily to lower-tax locations regardless of productivity, the coordination function is minimal and the constraint is mostly extraction. Current empirical evidence suggests mixed: some correlation between tax rates and genuine productivity differences (coordination signal) but substantial profit-shifting independent of real economic activity (extraction signal). The Tangled Rope classification reflects this genuine mixture — not confusion, but structural truth: the mechanism both coordinates capital AND extracts from non-mobile populations. The constraint would be misclassified as pure Rope (coordination) if we focused only on capital-holder perspective; equally misclassified as pure Snare (extraction) if we focused only on labor perspective. The mandatrophy is resolved by multi-perspectival measurement and acceptance that the structural reality is hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_of_capital_flight_magnitude,
    'What is the true magnitude of annual capital flight and profit shifting? Tax agency estimates range from 4-13% of global corporate tax revenue.',
    'Cross-national data harmonization (IMF, OECD, World Bank); tracing of illicit financial flows; satellite data on foreign direct investment patterns; discrepancy analysis between source-country and destination-country tax records',
    'If lower bound (4%): constraint is moderate (Rope from coalition perspective). If upper bound (13%): constraint is severe (Snare from victim perspective). Measurement ambiguity enables both cost-benefit arguing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_capital_flight_magnitude, empirical, 'True magnitude of annual capital flight and profit shifting').

omega_variable(
    causal_attribution_tax_competition,
    'How much of the race-to-the-bottom in corporate tax rates is driven by genuine capital mobility vs. policy ideology and corporate lobbying?',
    'Counterfactual analysis: jurisdictions that resisted tax competition (e.g., Denmark, Nordic countries) showed comparable or superior investment outcomes; controlled experiments with tax treaty harmonization; analysis of lobbying expenditure vs. actual capital flows by sector',
    'If driven primarily by capital mobility: constraint is structural (Mountain or Rope). If driven primarily by ideology: constraint is institutional (Tangled Rope or Piton). If mixed: requires joint optimization analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_tax_competition, conceptual, 'Causal drivers of tax competition race-to-bottom').

omega_variable(
    enforcement_mechanism_saturation,
    'Can global minimum tax (Pillar 2), country-by-country reporting, and digital tax agreements actually close the arbitrage gap, or do they face structural non-compliance at scale?',
    'Post-2024 implementation monitoring: IRS audits comparing reported income across jurisdictions; OECD compliance tracking; analysis of new arbitrage mechanisms emerging after minimum tax implementation; cost-benefit analysis of enforcement vs. avoidance innovation arms race',
    'If enforcement mechanisms work: scaffold sunset is real, classification shifts from Tangled Rope to temporary Scaffold within 10 years. If non-compliance continues: classification remains Tangled Rope, minimum tax becomes another piton (performative ritual).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_saturation, empirical, 'Whether global minimum tax enforcement can close arbitrage gap').

omega_variable(
    labor_productivity_vs_capital_extraction,
    'How much of the correlation between capital flight and reduced public services is genuine causation vs. confounded with productivity shocks and demographic change?',
    'Synthetic control analysis: jurisdictions with identical labor productivity and demographics but different capital flight exposure; time-series decomposition of tax revenue changes; controlled comparison of public service outcomes across low-mobility-loss vs. high-mobility-loss regions',
    'If capital flight is primary driver: victim classification is accurate (Snare for trapped populations). If confounded: extraction magnitude is lower, classification may shift toward Rope. Changes how victim agent power is understood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_productivity_vs_capital_extraction, empirical, 'Causal attribution of public service decline to capital flight vs. other factors').

omega_variable(
    non_state_actors_exit,
    'For SMEs and non-multinational corporations, is constrained exit actually accurate, or can small firms also achieve tax optimization through transfer pricing and subsidiary structures?',
    'Comparative study of tax avoidance strategy availability by firm size; audit rates and penalties by firm size; accessibility of tax planning services and expertise to SMEs; measured difference in effective tax rates between SMEs and multinationals in same sector',
    'If SMEs can access similar avoidance: classification should upgrade from Tangled Rope (victims) to Rope (coordinating peers). If constrained: current classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_state_actors_exit, empirical, 'Whether SMEs have meaningfully different tax avoidance capacity than multinationals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_flight_and_jurisdictional_arbitrage, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capflight_tr_t1980, capital_flight_and_jurisdictional_arbitrage, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(capflight_tr_t2000, capital_flight_and_jurisdictional_arbitrage, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(capflight_tr_t2010, capital_flight_and_jurisdictional_arbitrage, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(capflight_tr_t2020, capital_flight_and_jurisdictional_arbitrage, theater_ratio, 2020, 0.58).
narrative_ontology:measurement(capflight_tr_t2024, capital_flight_and_jurisdictional_arbitrage, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(capflight_be_t1980, capital_flight_and_jurisdictional_arbitrage, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(capflight_be_t2000, capital_flight_and_jurisdictional_arbitrage, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(capflight_be_t2010, capital_flight_and_jurisdictional_arbitrage, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(capflight_be_t2020, capital_flight_and_jurisdictional_arbitrage, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(capflight_be_t2024, capital_flight_and_jurisdictional_arbitrage, base_extractiveness, 2024, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_flight_and_jurisdictional_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(capital_flight_and_jurisdictional_arbitrage, fiscal_sovereignty_erosion).
narrative_ontology:affects_constraint(capital_flight_and_jurisdictional_arbitrage, tax_base_labor_shift).
narrative_ontology:affects_constraint(capital_flight_and_jurisdictional_arbitrage, regulatory_arbitrage).
narrative_ontology:affects_constraint(capital_flight_and_jurisdictional_arbitrage, corporate_profit_shifting).

% DUAL FORMULATION NOTE:
% Capital flight decomposes into three structurally distinct constraints: (1) Corporate Profit Shifting (ε≈0.65, Snare) — accounting and transfer pricing manipulation with minimal coordination function; (2) Regulatory Arbitrage (ε≈0.48, Tangled Rope) — genuine coordination of capital allocation mixed with tax-driven distortion; (3) Fiscal Sovereignty Erosion (ε≈0.55, Tangled Rope) — welfare-state coordination necessity compromised by capital mobility. These stories are linked because corporate profit shifting creates the arbitrage opportunity that drives regulatory arbitrage, which in turn erodes fiscal sovereignty of high-tax jurisdictions. The family exhibits different extractiveness values and different beneficiary/victim profiles per component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capital_flight_and_jurisdictional_arbitrage, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
