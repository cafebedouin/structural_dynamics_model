% ============================================================================
% CONSTRAINT STORY: help_to_buy_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_help_to_buy_uk, []).

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
 *   constraint_id: help_to_buy_uk
 *   human_readable: UK 'Help to Buy' Equity Loan Scheme (2013-2023)
 *   domain: economic/housing/fiscal_policy
 *
 * SUMMARY:
 *   The UK's Help to Buy scheme (2013-2023) exemplifies a policy constraint
 *   that combines genuine coordination benefits with asymmetric extraction,
 *   obscured by increasing performativity over time. Launched in response to
 *   post-2008 fiscal contraction and weak first-time buyer demand, the scheme
 *   offered government equity loans (up to 20% of purchase price for new
 *   homes, 15% for existing) to reduce required mortgage deposits. It was
 *   explicitly temporary — a demand-support bridge while housing supply
 *   expanded. In practice, HTB triggered a decade of price inflation that
 *   locked out excluded buyers, captured value for developers and lenders,
 *   and left government with £19.5 billion in subsidy and significant
 *   negative equity exposure. The constraint exhibits all six classifications
 *   depending on perspective: snare for those excluded from the scheme who
 *   faced higher entry prices; tangled rope for eligible buyers who received
 *   access but bore price inflation and debt risk; rope for developers and
 *   lenders who captured coordinated demand stimulus; scaffold from policy
 *   reformers who see a temporary support structure with sunset logic; piton
 *   from the housing supply system which experienced performative demand
 *   stimulus without supply expansion; tangled rope from government as both
 *   enforcer and victim; and (falsely) mountain from analysts who naturalize
 *   demand-side subsidy failures as immutable economic law.
 *
 * KEY AGENTS:
 *   - Eligible First-Time Buyers: Primary intended beneficiaries (moderate/mobile) — gained homeownership access but at higher entry prices; bore equity loan risk
 *   - Property Developers: Primary structural beneficiaries (institutional/arbitrage) — captured price increases, de-risked market, increased origination volume
 *   - Mortgage Lenders: Primary structural beneficiaries (institutional/arbitrage) — expanded loan origination, reduced default risk through equity cushion, captured margin increase
 *   - Locked-Out Renters (Post-Scheme): Primary victims (powerless/trapped) — faced higher entry prices without scheme access; delayed or impossible homeownership
 *   - Rental Market Affordability: Diffuse victim (powerless/trapped) — scheme stimulated purchase demand, reducing rental supply and increasing rents for those excluded from purchase
 *   - Government/Public Fiscal Authority: Organized actor (organized/constrained) — enforcer with dual benefit/cost: fiscal stimulus and employment support offset by £19.5 billion subsidy and negative equity risk
 *   - Housing Supply System: Institutional actor (institutional/arbitrage) — experienced scheme as performative theater: demand stimulus without capacity to respond, resulting in price capture rather than access improvement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(help_to_buy_uk, 0.58).
domain_priors:suppression_score(help_to_buy_uk, 0.65).
domain_priors:theater_ratio(help_to_buy_uk, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(help_to_buy_uk, extractiveness, 0.58).
narrative_ontology:constraint_metric(help_to_buy_uk, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(help_to_buy_uk, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(help_to_buy_uk, tangled_rope).
narrative_ontology:human_readable(help_to_buy_uk, "UK 'Help to Buy' Equity Loan Scheme (2013-2023)").
narrative_ontology:topic_domain(help_to_buy_uk, "economic/housing/fiscal_policy").

domain_priors:requires_active_enforcement(help_to_buy_uk).
narrative_ontology:has_sunset_clause(help_to_buy_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(help_to_buy_uk, property_developers).
narrative_ontology:constraint_beneficiary(help_to_buy_uk, mortgage_lenders).
narrative_ontology:constraint_beneficiary(help_to_buy_uk, government_fiscal_short_term).
narrative_ontology:constraint_victim(help_to_buy_uk, future_homebuyers_priced_out).
narrative_ontology:constraint_victim(help_to_buy_uk, rental_market_affordability).
narrative_ontology:constraint_victim(help_to_buy_uk, public_fiscal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-OUT RENTER POST-SCHEME (SNARE) — First-time buyers excluded from HTB face a housing ladder where prices have been artificially elevated by the scheme's demand stimulus. Bears the extraction (higher entry prices) with no coordination benefit. No arbitrage — locked out entirely from homeownership tier. Experiences maximum suppression: structural barriers to exit housing precarity.
constraint_indexing:constraint_classification(help_to_buy_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HTB-ELIGIBLE FIRST-TIME BUYER (TANGLED ROPE) — Receives genuine coordination benefit (access to homeownership that would otherwise be delayed or impossible). Also bears extraction: higher purchase prices than counterfactual market, equity loan structured as debt with negative equity risk, and participation locks buyer into specific property types/price bands. Mobile exit options (can decline scheme, seek rental, move regions) but real costs to exercising exit. Mixed experience: coordination benefit + asymmetric extraction.
constraint_indexing:constraint_classification(help_to_buy_uk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: DEVELOPERS AND LENDERS (ROPE) — Primary beneficiaries. HTB stimulates demand, justifies higher pricing, expands mortgage origination volume. Experience the constraint as pure coordination mechanism: scheme de-risks their market (government guarantees demand), reduces inventory overhang, enables higher margins through price capture. Arbitrage options: can ignore scheme and serve non-HTB buyers, but scheme makes their preferred market segment much more profitable. Extraction runs toward these agents.
constraint_indexing:constraint_classification(help_to_buy_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL STABILIZATION COALITION (SCAFFOLD) — Government bodies, housing charities, and policy reformers viewing HTB as temporary demand-support mechanism with explicit sunset (scheme ended 2023). See the scheme as a transitional coordination failure: inflated prices are temporary theater; real solution is supply-side reform (build more houses). Sunset clause is genuine — scheme was always intended to bridge a temporary affordability gap while supply caught up. Low effective extraction from this perspective because the coalition has agency and sees the exit path (supply expansion, scheme termination).
constraint_indexing:constraint_classification(help_to_buy_uk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HOUSING SUPPLY SYSTEM (PITON) — The scheme's primary function was demand stimulus, but it operated in a housing market where supply constraints (planning restrictions, construction capacity, land availability) were immutable over the scheme's 10-year horizon. The constraint became increasingly performative: HTB's demand-side stimulus could not increase supply, so it inflated prices instead. Theater ratio increased over time as the gap widened between scheme intent (improve first-time buyer access) and actual effect (price capture). The supply system sees HTB as an inertial policy: maintained because alternatives weren't ready, but degraded in function relative to its stated purpose.
constraint_indexing:constraint_classification(help_to_buy_uk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC FISCAL AUTHORITY (TANGLED ROPE) — Government as both enforcer and victim. Received coordination benefit: HTB subsidized demand during fiscal contraction (post-2008), supported construction employment, maintained housing market confidence. Also bore extraction: £19.5 billion net government spend, negative equity risk on equity loans, opportunity cost (capital not available for supply-side investment or other programs). Constrained exit (politically committed to the scheme once launched; ending it was controversial) combined with both benefits and costs. Active enforcement required (loan administration, subsidy processing, default management).
constraint_indexing:constraint_classification(help_to_buy_uk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a long-horizon universal perspective, demand-side subsidies without supply expansion are inherently self-defeating: they stimulate prices, not access. This is treated as an immutable property of housing economics. However, the structural data contradicts the mountain classification — the scheme had real coordination benefits (for eligible buyers, for developers, for fiscal stability), and was explicitly sunset. The 'immutable natural law' framing naturalizes what is actually a reversible policy choice and a real (if temporary and limited) coordination mechanism. Engine will flag this as a false summit.
constraint_indexing:constraint_classification(help_to_buy_uk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(help_to_buy_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(help_to_buy_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(help_to_buy_uk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(help_to_buy_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(help_to_buy_uk, TR),
    TR >= 0.70.

:- end_tests(help_to_buy_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing over time. Base extractiveness starts at 0.35 (scheme genuinely improves access for eligible buyers, provides fiscal stimulus) but rises to 0.58 by end as: (1) price inflation captures most scheme subsidy, reducing real access gain; (2) excluded buyers face permanently higher prices; (3) government negative equity exposure crystallizes. The trajectory reflects Goodhart drift — as the scheme persists, its stated purpose (first-time buyer access) decouples from its actual effect (price inflation capture). Suppression (0.65): Moderate-high and structural. Significant barriers to exit extraction: eligible buyers cannot reject scheme subsidy and face lower deposit requirements without incurring opportunity cost; developers cannot ignore scheme demand stimulus; lenders cannot forego expanded origination; government faces political cost of scheme termination. Planning restrictions, land scarcity, and construction constraints prevent supply-side response. But suppression is not maximal — buyers have some mobility (can rent, move regions, use alternative financing), developers can serve non-HTB segments, and government did end the scheme. Theater ratio (0.68): High and increasing. Scheme's stated purpose was to 'help' first-time buyers access homeownership. Actual effect was to stimulate demand-side demand while supply constraints converted subsidy into price capture. By 2023, theater had increased substantially as the gap widened between 'help' narrative and price-inflation reality. Scheme persistence despite degraded function (Piton perspective) contributed to theater ratio rise.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces sharp perspectival divergence. The eligible buyer sees mixed benefit-and-cost (Tangled Rope) — genuine access gain but at higher prices. The developer sees pure coordination (Rope) — scheme solves their demand problem. The locked-out renter sees extraction with no coordination benefit (Snare) — higher prices, no access. The fiscal authority sees mixed coordination-and-cost (Tangled Rope) — demand stimulus and employment offset by subsidy and risk. The supply system sees performative theater (Piton) — scheme stimulates demand it cannot fulfill, creating price inflation instead of access. Policy reformers see temporary support with sunset logic (Scaffold) — real problem (supply shortage) requires supply-side solutions, scheme bridges the gap. The analytical observer risks seeing immutable economic law (Mountain) — demand subsidies without supply expansion must inflate prices — but this naturalizes a contingent policy design choice. The perspectival gaps derive from different structural positions (beneficiary vs victim) combined with different exit options and time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position in the extraction flow. Eligible buyers (moderate power, mobile exit) derive low d from beneficiary status + exit options, but d is raised by victim status (price inflation, equity loan risk) and constrained exit (scheme participation locks buyer into specific property types). Developers and lenders (institutional power, arbitrage options) derive very low d (close to 0.0) from beneficiary status + arbitrage exit — they experience the constraint as pure coordination gain. Locked-out buyers (powerless, trapped exit) derive high d (close to 1.0) from victim status + no exit — they bear full extraction cost. Government (organized power, constrained exit) derives moderate d from dual benefit/victim status: fiscal stimulus and employment support (low d) are offset by subsidy costs and negative equity risk (high d). The housing supply system (institutional, arbitrage) derives low d from beneficiary perspective (demand stimulus is beneficial in shortage) but faces degradation of this benefit as theater increases and supply remains constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   HTB resolves the mandatrophy by demonstrating that tangled rope classification is correct: the scheme genuinely coordinates demand (first-time buyers get access, developers get market stimulus, lenders get origination volume) AND simultaneously extracts through price capture (eligible buyers bear inflation, locked-out buyers bear pricing out, government bears fiscal cost). The coordination function is real but limited — HTB does enable homeownership for eligible cohorts that would otherwise have delayed or been denied access. The extraction is also real — price inflation captures most of the subsidy value, transferring it to property owners and lenders. Neither pure coordination (Rope) nor pure extraction (Snare) describes the full constraint. The mandatrophy is resolved by the explicit sunset clause (scheme was always intended to end, indicating temporary structure rather than permanent extraction mechanism) combined with the temporal measurements showing rising extractiveness and rising theater — these are the signatures of a Tangled Rope degrading toward Piton (performative theater without functional coordination). The analytical observer's mountain classification is a false summit: 'demand subsidies inflate prices' is true within institutional constraints (planning restrictions, supply inelasticity) but those constraints are not immutable — supply-side interventions or planning reform could make demand stimulus productive. Naturalizing the constraint as immutable law obscures the policy choices embedded in it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_household_formation,
    'How many additional households formed as a direct result of HTB access vs. would have formed through alternative financing or delayed purchase?',
    'Longitudinal household formation data pre/post-HTB; comparison with regions/cohorts excluded from scheme; hazard analysis of time-to-purchase for eligible vs. ineligible buyers',
    'If high contribution: HTB was genuine coordination mechanism (Rope/TangledRope justified). If low: demand simply substituted for other financing; scheme was primarily price-capture mechanism (Snare justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_household_formation, empirical, 'Causal impact of HTB on household formation rates').

omega_variable(
    housing_supply_elasticity_threshold,
    'At what supply elasticity would HTB have improved access rather than inflated prices? Was UK supply elasticity ever in that range?',
    'Econometric analysis of regional price responses to HTB takeup vs. regional supply growth; estimation of supply elasticity by region and period',
    'If UK elasticity was always low: scheme was structurally extractive from first-time buyers from inception (Snare). If elasticity varied by region/period: scheme was coordinating in some contexts, extractive in others (Tangled Rope justified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(housing_supply_elasticity_threshold, empirical, 'Supply elasticity threshold for HTB effectiveness').

omega_variable(
    fiscal_opportunity_cost_alternative_uses,
    'What housing outcomes would £19.5 billion have produced if deployed as supply-side subsidy (land purchase, planning reform, construction support) rather than demand-side subsidy?',
    'Comparative housing policy analysis; cost-per-unit analysis of supply vs. demand-side interventions; modeling of counterfactual programs',
    'If supply-side deployment would have been dramatically more effective: HTB was a fiscal trap (Snare from public perspective). If roughly equivalent: HTB''s temporal urgency during fiscal contraction was justified (Scaffold/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_opportunity_cost_alternative_uses, empirical, 'Alternative uses of HTB fiscal allocation').

omega_variable(
    negative_equity_crystallization_rate,
    'What fraction of HTB loans entered negative equity (property value < loan + mortgage) by scheme end? What is the actual or projected default and recovery rate?',
    'Government housing agency data on HTB loan performance; property price changes by cohort and region; actual default and recovery rates; projections based on post-2023 market dynamics',
    'If high crystallization and poor recovery: government bore massive extraction cost; fiscal impact was worse than declared (strengthens Snare classification from fiscal perspective). If low: risk was manageable; scheme was actually contained extraction (strengthens Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(negative_equity_crystallization_rate, empirical, 'Negative equity crystallization and recovery rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(help_to_buy_uk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htb_tr_t0, help_to_buy_uk, theater_ratio, 0, 0.42).
narrative_ontology:measurement(htb_tr_t5, help_to_buy_uk, theater_ratio, 5, 0.58).
narrative_ontology:measurement(htb_tr_t10, help_to_buy_uk, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(htb_be_t0, help_to_buy_uk, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(htb_be_t5, help_to_buy_uk, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(htb_be_t10, help_to_buy_uk, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(help_to_buy_uk, resource_allocation).
narrative_ontology:affects_constraint(help_to_buy_uk, uk_housing_supply_constraint).
narrative_ontology:affects_constraint(help_to_buy_uk, mortgage_market_concentration).
narrative_ontology:affects_constraint(help_to_buy_uk, developer_market_power).

% DUAL FORMULATION NOTE:
% HTB is downstream of the fundamental UK housing supply shortage (planning restrictions, land scarcity, construction capacity limits). The scheme's extractiveness depends critically on supply elasticity: in a supply-elastic market, HTB would be pure coordination (Rope); in a supply-inelastic market, it becomes extraction mechanism (Snare/Tangled Rope). The three network-affected constraints capture the upstream structural conditions that convert demand stimulus into price capture rather than access improvement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(help_to_buy_uk, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
