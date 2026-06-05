% ============================================================================
% CONSTRAINT STORY: land_improvement_conflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_land_improvement_conflation, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: land_improvement_conflation
 *   human_readable: Land Improvement Conflation in Housing Markets
 *   domain: political_economy/housing/institutional_analysis
 *
 * SUMMARY:
 *   The land improvement conflation treats housing prices as determined by a
 *   natural law of supply and demand, when they are actually constructed
 *   through zoning restrictions, lending standards, tax policy, and appraisal
 *   practices that collectively restrict supply and amplify land rents. The
 *   constraint operates by naturalizing what is an institutional arrangement:
 *   incumbent landowners and lenders benefit from scarcity-induced price
 *   inflation, while renters and first-time buyers bear the cost of
 *   permanently high prices relative to income. The constraint exhibits both
 *   genuine coordination functions (zoning prevents incompatible land uses;
 *   lending standards match borrowers to sustainable payment obligations) and
 *   asymmetric extraction (supply restrictions that benefit incumbents;
 *   lending standards that amplify collateral inflation). The theater ratio
 *   (0.58) reflects that appraisal processes employ methodologies (comparable
 *   sales) that circularly perpetuate previous valuations, and that 'market
 *   clearing' language obscures institutional mediation. The metrics show
 *   acceleration: extractiveness and suppression both rose from 1975-2020 as
 *   zoning tightened, lending leverage increased, and the income-to-price
 *   ratio deteriorated. The constraint is actively enforced: repeated reform
 *   attempts (upzoning, by-right development, land value tax pilots) have
 *   been blocked or rolled back through incumbent-owner political influence,
 *   zoning board capture, and financing complications. This is not a static
 *   equilibrium — it is an maintained allocation with identifiable
 *   beneficiaries and victims.
 *
 * KEY AGENTS:
 *   - Incumbent Landowners: Primary beneficiary (institutional/arbitrage) — capture housing inflation through appreciation; benefit from zoning scarcity rents; have high political influence through voter concentration and campaign contributions
 *   - Renters and First-Time Buyers: Primary victims (powerless/trapped) — structurally trapped by leverage that exceeds their capital; cannot arbitrage to alternative housing markets; bear permanent rent extraction and opportunity cost of non-ownership
 *   - Mortgage Lenders: Secondary beneficiary (powerful/constrained) — amplify extraction through leverage and collateral inflation; origination fees scale with higher prices; deposit-taking creates incentive to support zoning restrictions that inflate collateral values
 *   - Real Estate Intermediaries: Secondary beneficiary (institutional/arbitrage) — commission structures scale with transaction values; appraisal business models depend on comparable-sales methodology that perpetuates price inflation
 *   - Municipal Zoning Authorities: Institutional beneficiary (organized/constrained) — property tax revenue depends on land valuations; zoning restrictions are functionally supply reduction that increases municipal tax base; NIMBYist constituency enforces zoning restrictions
 *   - Real Estate Appraisers: Institutional performer (moderate/constrained) — maintain the theater of 'market valuation' through comparable-sales methodology; appraisals are legally required for lending but have degraded epistemic function (circularly perpetuate previous prices)
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing constructed institutional arrangements as inevitable economic laws; frame determines whether the constraint is seen as mountain vs. tangled_rope vs. snare
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(land_improvement_conflation, 0.62).
domain_priors:suppression_score(land_improvement_conflation, 0.68).
domain_priors:theater_ratio(land_improvement_conflation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(land_improvement_conflation, extractiveness, 0.62).
narrative_ontology:constraint_metric(land_improvement_conflation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(land_improvement_conflation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(land_improvement_conflation, tangled_rope).
narrative_ontology:human_readable(land_improvement_conflation, "Land Improvement Conflation in Housing Markets").
narrative_ontology:topic_domain(land_improvement_conflation, "political_economy/housing/institutional_analysis").

domain_priors:requires_active_enforcement(land_improvement_conflation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(land_improvement_conflation, 'fc6e0c55-d3c0-4a58-8a80-dbc938fb767a').
narrative_ontology:cs_kernel_codification('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', distributed).
narrative_ontology:cs_authority_grounding('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', extraction).
narrative_ontology:cs_interpretation_layer_present('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a').
narrative_ontology:cs_reading_relation('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', land_improvement_conflation__housing_supply_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', land_improvement_conflation__zoning_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', land_improvement_conflation__regulatory_failure_reading, influences).
narrative_ontology:cs_axiom('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', foundational, prices_reflect_natural_equilibrium).
narrative_ontology:cs_axiom_status(prices_reflect_natural_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', prices_reflect_natural_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', secondary, zoning_prevents_sprawl_and_conflict).
narrative_ontology:cs_axiom_status(zoning_prevents_sprawl_and_conflict, holdable).
narrative_ontology:cs_axiom_grounding('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', zoning_prevents_sprawl_and_conflict, instrumental).
narrative_ontology:cs_reference_frame('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', competitive_housing_market_equilibrium).
narrative_ontology:cs_drift_state('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', contemporary_housing_affordability_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fc6e0c55-d3c0-4a58-8a80-dbc938fb767a', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(land_improvement_conflation, incumbent_landowners).
narrative_ontology:constraint_beneficiary(land_improvement_conflation, mortgage_lenders).
narrative_ontology:constraint_beneficiary(land_improvement_conflation, real_estate_intermediaries).
narrative_ontology:constraint_beneficiary(land_improvement_conflation, municipal_zoning_authorities).
narrative_ontology:constraint_victim(land_improvement_conflation, renters).
narrative_ontology:constraint_victim(land_improvement_conflation, first_time_buyers).
narrative_ontology:constraint_victim(land_improvement_conflation, younger_cohorts).
narrative_ontology:constraint_victim(land_improvement_conflation, housing_price_formation_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG RENTER OR FIRST-TIME BUYER (SNARE) — Structurally trapped by accumulated leverage that prices them out of ownership. High effective extraction. Cannot arbitrage to alternative housing markets (job, family, climate all constrain location). Cannot exit the rental market — must occupy housing. The constraint operates as pure extraction: incumbent owners and lenders capture housing inflation; renters bear the cost through opportunity cost (capital that could build wealth goes to rent) and the identity cost of permanent non-ownership. No genuine coordination function perceived from this position.
constraint_indexing:constraint_classification(land_improvement_conflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT LANDOWNER (ROPE) — Sees the constraint as natural coordination: supply is limited, demand is rising, prices equilibrate. Experiences the constraint as positive alignment of their interests with 'the market.' High exit optionality (can sell, refinance, arbitrage to other assets). Net beneficiary. The constraint from this perspective solves a genuine coordination problem: how to allocate scarce housing across competing users. The beneficiary role makes extraction appear as fair market clearing.
constraint_indexing:constraint_classification(land_improvement_conflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MORTGAGE LENDER AND REAL ESTATE INTERMEDIARY (TANGLED ROPE) — Coordinates capital allocation (lending standards, loan duration, collateral valuation) AND extracts through origination fees, servicing spreads, and valuation capture. Constrained by regulatory requirement to maintain lending standards and reserve ratios, but strategically benefits from price inflation (higher collateral values = more lending capacity). Experiences genuine coordination problem (matching borrowers to appropriate loan products) layered with extraction mechanism (valuation inflation amplifies origination fees). High agency and high extraction simultaneously.
constraint_indexing:constraint_classification(land_improvement_conflation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MUNICIPAL ZONING AUTHORITY (TANGLED ROPE) — Coordinates land use (residential density, commercial mixing, infrastructure) AND extracts through property tax revenue that depends on land valuations. Genuine coordination function (preventing incompatible uses) layered with extraction mechanism (zoning restrictions that reduce supply amplify land rents, which inflate municipal tax base). Constrained by state enabling statutes and NIMBYist voters, but benefits from artificial scarcity. The constraint appears as natural when framed as 'zoning prevents sprawl and maintains neighborhood character,' obscuring that the same rules restrict supply and inflate prices.
constraint_indexing:constraint_classification(land_improvement_conflation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REAL ESTATE AGENT AND APPRAISER (PITON) — Maintains the performative structure of 'market valuation' while the underlying function (matching supply to demand) has atrophied into a pure intermediary toll. Theater ratio is high: appraisals employ comparable sales methodology which circularly reproduces the previous valuation (if comparables were artificially inflated, the appraisal perpetuates inflation). The agent role persists through institutional inertia (required by law for lending) despite degraded epistemic function. Low theater ratio would require transparent cost modeling; appraisals instead maintain opaque justification that naturalizes constructed prices.
constraint_indexing:constraint_classification(land_improvement_conflation, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: NATURALIST / ECONOMICS TEXTBOOK (MOUNTAIN) — Treats housing prices as inevitable equilibrium between supply and demand. Sees no beneficiaries or enforcement — prices 'clear the market' as a natural law equivalent to gravity. From this perspective, extraction and suppression are immeasurable (zero degrees of freedom). The constraint is presented as emergent from individual rational choice with no institutional mediator. However, the structural data contradicts this: the constraint declares beneficiaries, victims, and enforcement requirements. The engine will classify this as a false summit — the naturalist view naturalizes what is actually a constructed institutional arrangement.
constraint_indexing:constraint_classification(land_improvement_conflation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(land_improvement_conflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(land_improvement_conflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(land_improvement_conflation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(land_improvement_conflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(land_improvement_conflation, TR),
    TR >= 0.70.

:- end_tests(land_improvement_conflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high. The constraint benefits incumbent owners and lenders through price appreciation that exceeds income growth, while extracting from renters through permanent rent flows and capital opportunity costs. The extractiveness is not maximal (snare maximum ~0.85) because genuine coordination functions exist (zoning prevents land-use conflicts; lending standards prevent over-leverage) alongside the extraction. The measurement trajectory shows acceleration: extractiveness was lower in 1975 (0.35) when income-to-price ratios were healthier and zoning was less restrictive, rising to 0.62 by 2020 as zoning tightened, lending leverage increased through securitization, and policy reforms failed to materialize. This acceleration indicates that the constraint is not a stable equilibrium but an actively maintained extraction with rising intensity. Suppression (0.68): Moderately high. Barriers to alternative housing arrangements include zoning restrictions (legal/structural), lending standards that require down payments (capital barrier), appraiser bias toward incumbent prices (epistemic barrier), renter political weakness (organizational barrier), and transience of renter populations (social barrier). These are not insurmountable but they are substantial and actively maintained. The suppression trajectory also shows acceleration: municipal zoning codes have become more restrictive, lending standards tightened post-2008, and appraisal culture hardened around comparable-sales circularity. Theater ratio (0.58): Moderately high. The appraisal process employs comparable-sales methodology that appears scientific but actually circularly perpetuates previous valuations — if comparables were artificially inflated, the appraisal reproduces the inflation as 'market evidence.' The term 'market clearing' obscures that prices are set through institutional policy (zoning density, lending LTV ratios, tax policy) rather than discovered through neutral supply-demand mechanics. The theater has increased as appraisal standardization hardened and as 'supply and demand' rhetoric became more naturalized. A lower theater ratio would require transparent cost-based pricing (construction cost + land acquisition cost + financing cost = price) rather than backward-looking comparable-sales methodology.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the fundamental perspectival divide between beneficiaries and victims. The incumbent landowner sees Rope: genuine coordination (zoning prevents sprawl), fair market clearing, alignment of their interests with natural economic law. The young renter sees Snare: extraction with no coordination benefit, trapped by leverage, no exit options. The lender sees Tangled Rope: genuine coordination (matching borrowers to appropriate loans) layered with extraction (collateral inflation increases lending capacity). The municipality sees Tangled Rope: genuine coordination (preventing incompatible land uses) layered with extraction (zoning scarcity increases tax base). The appraiser sees Piton: maintaining a performative valuation ritual whose epistemic function has degraded. The analytical observer at civilizational scope risks seeing Mountain: an inevitable natural law of supply and demand. The perspectival gap reveals that there is no single answer — the classification depends on where the observer sits. However, the structural data (beneficiaries, victims, enforcement) is observer-independent: beneficiaries exist, victims exist, the constraint requires active enforcement through zoning boards and lending standards. The mountain perspective is therefore a false summit — it naturalizes what the structural data reveals as constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the constraint — 0.0 = full beneficiary (constraint subsidizes them), 0.5 = symmetric (costs ≈ benefits), 1.0 = full target (constraint extracts from them). The incumbent landowner has d ≈ 0.1 (high beneficiary, arbitrage exit options = negative effective extraction — they benefit). The renter has d ≈ 0.95 (full target, trapped exit = maximum experienced extraction). The lender has d ≈ 0.35 (net beneficiary but constrained exit = moderate effective extraction in their favor). The municipality has d ≈ 0.2 (beneficiary through tax base, but constrained by state law and voter coalitions). The appraiser has d ≈ 0.45 (symmetric — benefits from price inflation through higher appraisals, but constrained by regulatory requirements and comparable-sales methodology). These directional positions are structural facts derived from power, exit options, and beneficiary/victim status. They are NOT tuned to produce a desired classification — they follow from the analytical description of who benefits and who bears costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: zoning was originally mandated to solve a genuine coordination problem (prevent noxious industrial land uses adjacent to residential areas, avoid sprawl, preserve neighborhood character). The mandate was legitimate at the time zoning was introduced (1920s-1930s). But over time (1975-2020 interval in the measurements), the same zoning rules began to serve a different function: restricting supply and capturing rents for incumbent owners rather than preventing land-use conflicts. The original mandate (coordination) has outlived its utility; the constraint now primarily functions as extraction. A true mandate resolution would require examining whether zoning still primarily solves the coordination problem (which it does, to some degree — zoning does prevent some incompatible uses) or whether the extraction function has become dominant (which the measurements suggest — extractiveness acceleration indicates rising rent capture relative to coordination value). The measurement trajectory (extractiveness 0.35→0.62, theater_ratio 0.42→0.61) indicates that the constraint is drifting from mixed coordination-extraction (balanced tangled rope) toward pure extraction (snare-like). This drift is not accidental — it reflects active beneficiary capture of the zoning system (incumbent owners gaining planning board representation, zoning becoming more restrictive, reforms being blocked). The constraint has not resolved mandatrophy; it has instead shifted its function from coordination to extraction while maintaining the coordination rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_price,
    'Does ''housing price'' name a natural equilibrium or a constructed institutional allocation?',
    'Comparative institutional analysis: Counterfactual modeling of price formation under alternative zoning, lending, and tax regimes. Historical analysis of price acceleration before and after specific policy changes (e.g., post-war suburban zoning, post-1980 securitization, post-2008 QE). Decomposition of price into construction cost + land rent; analysis of how much of land rent is location-specific vs. policy-generated.',
    'If natural: mountain classification holds; renter extraction is unavoidable coordination cost. If constructed: tangled_rope or snare; extraction is policy-contingent and potentially reversible. The designation shifts the explanatory burden from ''supply and demand'' to ''which institutions and policy regimes?''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_price, empirical, 'Whether housing prices reflect natural market clearing or institutional construction').

omega_variable(
    supply_elasticity_under_reform,
    'If zoning restrictions were substantially relaxed, would housing supply respond elastically and prices fall, or would other bottlenecks (labor, materials, financing) prevent supply response?',
    'Natural experiment analysis of deregulated zoning jurisdictions (e.g., Japan''s minimal zoning); econometric modeling of supply elasticity conditional on financing availability; case studies of countries with strong supply-side response (e.g., post-1990 Ireland) vs. weak response (e.g., UK with planning constraints).',
    'If elastic response: supply is policy-constrained; the constraint is a regulatory snare masquerading as market clearing. If inelastic: supply has hard limits; prices reflect genuine scarcity; the constraint contains more coordination function than extraction. Classification shifts from snare toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_elasticity_under_reform, empirical, 'Housing supply elasticity when zoning constraints are relaxed').

omega_variable(
    beneficiary_capture_of_reform,
    'Why have multiple housing supply-side reforms (density bonuses, by-right zoning, land value tax pilots) failed to materialize or been rolled back when they would benefit renters and first-time buyers?',
    'Political economy analysis of zoning reform campaigns; mapping of incumbent-owner voter concentration and political influence; analysis of revolving-door relationships between real estate industries and municipal planning departments; comparison of reform success rates in jurisdictions with strong campaign finance restrictions vs. weak restrictions.',
    'If beneficiary capture explains reform failure: the constraint is actively enforced (snare or tangled_rope). If path dependence and coordination failure explain it: the constraint is a equilibrium trap that would shift if enough agents simultaneously shifted expectations (rope or scaffold with long sunset). The mechanism of persistence is diagnostic for extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_reform, empirical, 'Why housing supply reforms are blocked or rolled back').

omega_variable(
    lending_amplification_mechanism,
    'Does mortgage availability (leverage, loan duration, LTV ratios) amplify land prices, or do land prices drive mortgage demand?',
    'Causal inference using monetary policy shocks as exogenous lending-cycle drivers (e.g., Fed rate changes, quantitative easing). Time-series analysis of the lead/lag relationship between credit availability and price changes. Cross-country comparison of price-to-income ratios in high-leverage regimes (US, UK) vs. low-leverage regimes (Switzerland, Germany with stricter lending standards).',
    'If lending amplifies: the constraint contains a financial extraction layer (lenders systematically inflate collateral values to increase lending); extractiveness is higher than zoning alone would produce. If prices drive lending: financial markets are responsive rather than constitutive; the lender beneficiary role is secondary. Classification implications: lending-driven implies snare or tangled_rope with financial capture; price-driven implies more purely zoning-mediated constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lending_amplification_mechanism, empirical, 'Causal direction between lending availability and land prices').

omega_variable(
    renter_coalition_formation_barrier,
    'Why have renters not formed a political coalition powerful enough to overcome incumbent-owner opposition to supply-side reform, despite numerical majority and demonstrable economic harm?',
    'Political organization theory analysis: measuring renter population concentration (scattered vs. clustered geography), voter registration rates, electoral turnout, campaign contributions. Comparing success of renter advocacy in contexts with renter supermajorities vs. mixed owner-renter populations. Analyzing institutional barriers to renter political voice (transience reduces local engagement; renters'' legal insecurity reduces risk tolerance for organizing).',
    'If organizational barriers are primary: the constraint is a coordination trap that could shift with institutional change (e.g., allowing non-citizen voting, reducing voter transience through housing stability). If renter political power is genuinely insufficient: the constraint is more durable; extraction persists through superior incumbent organization. Classification stability and sunset probability both hinge on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renter_coalition_formation_barrier, empirical, 'Barriers to renter political mobilization against incumbent-owner beneficiaries').

omega_variable(
    naturalization_rhetorical_durability,
    'Does the ''natural law of supply and demand'' framing actively prevent reform (by delegitimizing redistribution as ''fighting nature'') or is it merely epiphenomenal to the actual institutional enforcement (zoning, lending, tax policy)?',
    'Discourse analysis of reform campaigns: tracking how often the ''supply and demand'' frame is invoked by reform opponents vs. other arguments (neighborhood character, infrastructure capacity). Comparative rhetoric in jurisdictions where reform succeeded vs. failed. Polling on public support for specific reforms vs. support for ''ending artificial scarcity'' (testing whether rhetorical framing shifts opinion).',
    'If naturalization framing is causally efficacious: it is part of the suppression mechanism; removing it (cognitive reframing, education, explicit institutional analysis) could weaken the constraint. If epiphenomenal: the constraint persists purely through institutional enforcement; rhetoric is theater. Classification of the theater_ratio hinges on whether rhetorical naturalization is part of the suppressive infrastructure or merely reflects it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_rhetorical_durability, conceptual, 'Whether naturalization framing causally sustains the constraint or merely reflects it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(land_improvement_conflation, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(landimpr_tr_t0, land_improvement_conflation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(landimpr_tr_t15, land_improvement_conflation, theater_ratio, 15, 0.5).
narrative_ontology:measurement(landimpr_tr_t30, land_improvement_conflation, theater_ratio, 30, 0.58).
narrative_ontology:measurement(landimpr_tr_t45, land_improvement_conflation, theater_ratio, 45, 0.61).

% Extraction over time
narrative_ontology:measurement(landimpr_be_t0, land_improvement_conflation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(landimpr_be_t15, land_improvement_conflation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(landimpr_be_t30, land_improvement_conflation, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(landimpr_be_t45, land_improvement_conflation, base_extractiveness, 45, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(landimpr_su_t0, land_improvement_conflation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(landimpr_su_t15, land_improvement_conflation, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(landimpr_su_t30, land_improvement_conflation, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(landimpr_su_t45, land_improvement_conflation, suppression_requirement, 45, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(land_improvement_conflation, resource_allocation).
narrative_ontology:affects_constraint(land_improvement_conflation, zoning_restriction_supply_function).
narrative_ontology:affects_constraint(land_improvement_conflation, mortgage_leverage_amplification).
narrative_ontology:affects_constraint(land_improvement_conflation, appraisal_circularity).
narrative_ontology:affects_constraint(land_improvement_conflation, owner_occupied_housing_subsidy).
narrative_ontology:affects_constraint(land_improvement_conflation, renter_political_organization_failure).

% DUAL FORMULATION NOTE:
% The land improvement conflation decomposes into at least five structurally distinct constraints: (1) zoning as supply restriction (has coordination + extraction functions), (2) lending leverage as collateral amplification (has coordination + extraction functions), (3) appraisal methodology as price circularization (pure extraction with performative coordination), (4) tax policy as owner-occupied subsidy (extraction enforced through IRS), (5) renter political weakness (structural suppression). Each has different ε values and different actor perspectives. The integrated 'housing price' constraint is the combined effect of these five mechanisms operating simultaneously. Network decomposition would map each mechanism separately, but they are sufficiently coupled (zoning restricts supply which justifies lending leverage which requires appraisal circularization which is defended by owner-occupied subsidy which is protected by renter political weakness) that they form a single integrated constraint in actual political economy. The constraint family metaphor applies: these are sibling mechanisms that reinforce each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(land_improvement_conflation, institutional, 0.25).
constraint_indexing:directionality_override(land_improvement_conflation, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
