% ============================================================================
% CONSTRAINT STORY: pe_rental_market_lockin
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pe_rental_market_lockin, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pe_rental_market_lockin
 *   human_readable: Private Equity Lock-in of Single-Family Rental Market
 *   domain: economic/housing/real_estate
 *
 * SUMMARY:
 *   Following the 2008 financial crisis, large private equity firms
 *   (Blackstone, Invitation Homes, American Homes 4 Rent, etc.)
 *   systematically acquired vast portfolios of single-family homes,
 *   converting them from owner-occupied to institutional rental assets. This
 *   constraint captures the structural extraction mechanism that emerged as
 *   PE firms bundled homes into securitized rental income streams, optimizing
 *   for capital returns rather than affordability or community stability. The
 *   constraint exhibits multiple DR types across different perspectives: for
 *   prospective homebuyers, it is a snare (locked out of ownership, no exit);
 *   for PE firms, it is a rope (solves aggregation and liquidity problems);
 *   for municipalities, it is tangled rope (mixed coordination and
 *   enforcement burdens); for organized tenant movements, it is a scaffold
 *   (temporary, with a sunset pathway via regulation and alternative
 *   ownership models); for the owner-occupancy norm, it is a piton (atrophied
 *   but rhetorically maintained); for the analytical observer, it risks
 *   appearing as an immutable law of capitalism (false summit).
 *
 * KEY AGENTS:
 *   - Private Equity Firms (Blackstone/Invitation Homes/American Homes 4 Rent): Primary beneficiary (institutional/arbitrage) — capture capital returns from bundled rental portfolios; high exit optionality via secondary markets and portfolio rotation
 *   - Prospective Homebuyers: Primary victim (moderate/constrained) — locked out of ownership via inflated purchase prices; constrained exit (accept rentership or relocate)
 *   - Rental Tenants: Primary victim (powerless/trapped) — subject to rising rents and maintenance cost-cutting; zero exit options within market area
 *   - Institutional Investors: Secondary beneficiary (institutional/arbitrage) — access liquid residential real estate securities; solve capital allocation problem
 *   - Local/State Government: Organized actor (organized/constrained) — regulatory power but constrained by PE capital mobility; mixed coordination and enforcement
 *   - Tenant Organizing Coalitions: Organized actors (organized/constrained) — building alternative pathways (CLTs, rent control, just-cause eviction); see sunset mechanisms
 *   - Owner-Occupancy Norm: Structural feature (institutional/arbitrage) — persists through inertia and political rhetoric; functionally atrophied as wealth-building mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing financialization as inevitable; engine detects false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pe_rental_market_lockin, 0.58).
domain_priors:suppression_score(pe_rental_market_lockin, 0.68).
domain_priors:theater_ratio(pe_rental_market_lockin, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pe_rental_market_lockin, extractiveness, 0.58).
narrative_ontology:constraint_metric(pe_rental_market_lockin, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pe_rental_market_lockin, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pe_rental_market_lockin, tangled_rope).
narrative_ontology:human_readable(pe_rental_market_lockin, "Private Equity Lock-in of Single-Family Rental Market").
narrative_ontology:topic_domain(pe_rental_market_lockin, "economic/housing/real_estate").

domain_priors:requires_active_enforcement(pe_rental_market_lockin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pe_rental_market_lockin, private_equity_firms).
narrative_ontology:constraint_beneficiary(pe_rental_market_lockin, institutional_investors).
narrative_ontology:constraint_beneficiary(pe_rental_market_lockin, capital_markets).
narrative_ontology:constraint_victim(pe_rental_market_lockin, prospective_homebuyers).
narrative_ontology:constraint_victim(pe_rental_market_lockin, rental_tenants).
narrative_ontology:constraint_victim(pe_rental_market_lockin, local_housing_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TENANT (SNARE) — Trapped in rental cycle with rising rents, deteriorating maintenance, and no pathway to ownership. Bears full extraction from PE firm's capital-return optimization. Zero exit options: cannot save sufficient down payment while rents increase faster than wages. Geographic lock-in via family/employment ties.
constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROSPECTIVE HOMEBUYER (SNARE) — Locked out of ownership via purchase prices inflated by PE bulk acquisition. Constrained exit: must either accept perpetual rentership or relocate to market without PE penetration. Bears cost of wealth transfer from homeownership to PE capital returns.
constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL HOUSING MARKET (TANGLED ROPE) — Benefits from PE capital inflow (inventory conversion, investment in stock, rental supply), but suffers from coordination failure: market transitions from owner-occupied (decentralized decision-making) to institutional landlord (centralized extraction optimization). Market has agency (zoning changes, tenant protection laws) but faces coordinated PE opposition. Mixed coordination and extraction.
constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIVATE EQUITY FIRM (ROPE) — Solves liquidity and portfolio aggregation problem for distributed single-family homes. Captures scale economies in maintenance, property management, and capital access. Experiences the constraint as coordination: bundling homes into tradeable securities unlocks value that was trapped in fragmented ownership. Net beneficiary with high arbitrage optionality.
constraint_indexing:constraint_classification(pe_rental_market_lockin, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL INVESTOR (ROPE) — Benefits from liquid residential real estate securities created by PE aggregation. Solves the problem of capital allocation into long-duration housing assets with stable cash flows. Low experienced extraction relative to extraction others bear; arbitrage optionality (exit via secondary markets) is high. Coordination function: creates price discovery and capital efficiency in housing markets.
constraint_indexing:constraint_classification(pe_rental_market_lockin, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MUNICIPAL/STATE GOVERNMENT (TANGLED ROPE) — Organized actor with regulatory power (zoning, rent control, property tax), but constrained by capital mobility (PE threatens to withdraw inventory if regulations tighten). Coordination function: manages housing supply and affordability. But active enforcement: property tax revenue, zoning compliance, rental licensing create ongoing regulatory burden that PE firms lobby to minimize. Asymmetric extraction: PE firms capture returns while governments bear service costs.
constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: TENANT ORGANIZING COALITION (SCAFFOLD) — Organized response to PE lock-in via rent stabilization campaigns, community land trusts, and regulatory pushback. Sees the constraint as temporary and solvable: sunset mechanisms include state-level rent control, acquisition restrictions, and community ownership models. Low effective extraction because the coalition has built alternative pathways (CLTs, just-cause eviction, rent caps). Suppression is high but declining as coalition power grows.
constraint_indexing:constraint_classification(pe_rental_market_lockin, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: OWNER-OCCUPANCY NORM (PITON) — The historical norm of single-family home ownership as wealth-building vehicle and community stability anchor persists theoretically but has atrophied functionally. PE lock-in represents degradation of this norm: home ownership is no longer accessible as a wealth-building mechanism for middle-income households. Theater ratio reflects nostalgic political rhetoric about 'affordable housing' while structural incentives remain firmly extractive. The norm persists through inertia and political language, not functional reality.
constraint_indexing:constraint_classification(pe_rental_market_lockin, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational perspective, capital concentration and the financialization of housing may appear as inevitable features of late capitalism: wealth accumulation drives consolidation, which drives institutional ownership, which drives extraction. This perspective risks naturalizing PE lock-in as a law of economics. However, the structural data contradicts the mountain classification — the constraint is contingent on specific regulatory gaps (FIRPTA exemptions, non-recourse financing, carried-interest tax treatment) and market conditions (post-2008 credit collapse, low interest rates). Engine will identify this as a false summit.
constraint_indexing:constraint_classification(pe_rental_market_lockin, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pe_rental_market_lockin_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pe_rental_market_lockin, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pe_rental_market_lockin, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pe_rental_market_lockin, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pe_rental_market_lockin, TR),
    TR >= 0.70.

:- end_tests(pe_rental_market_lockin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. PE firms extract significant returns through: (1) rent optimization (raising rents faster than inflation), (2) maintenance cost-cutting (reducing property upkeep below owner-occupied standards), (3) capital arbitrage (buying distressed post-2008, selling into securitization premium), and (4) wealth transfer (capturing owner-occupancy returns and converting to institutional yields). The value reflects sustained extraction but not maximum (0.75+) because PE firms must maintain some competitive service level to retain tenants and investor confidence; their extraction is constrained by reputational risk and regulatory exposure. Suppression (0.68): High. Multiple barriers prevent exit from the constraint: (a) geographic lock-in via family/employment, (b) credit constraints preventing down-payment accumulation while rents rise, (c) regulatory vacuum (FIRPTA exemptions, state preemption of local rent control in many states), (d) organized PE opposition to regulations, (e) information asymmetries in lease terms and maintenance obligations. Theater ratio (0.35): Low. Unlike many constraints, PE lock-in is not theatrical — it is a straightforward capital extraction mechanism with minimal performative content. PE firms manage properties functionally (though poorly) and charge rents directly; the constraint operates through transparent market mechanisms (rental prices, lease terms), not ritual or legitimation theater. This low theater distinguishes it from piton-degraded systems.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the PE firm (rope/coordination) and the prospective homebuyer (snare/extraction). From the PE perspective, the constraint solves a real problem: single-family homes were fragmented, illiquid assets; bundling them creates efficient capital markets. From the homebuyer perspective, the same bundling transfers ownership from distributed individuals to consolidated capital, pricing them out permanently. The gap is not about disagreement on facts but about opposite structural positions relative to extraction flow: what enables the PE firm is what traps the homebuyer. This is the hallmark of a tangled rope: both coordination and extraction are structurally present, but different agents experience opposite vectors.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness depends on their structural position via the directionality chain. PE firms (beneficiary + arbitrage) derive d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ. Prospective homebuyers (victim + constrained exit) derive d ≈ 0.85 → f(d) ≈ 1.15 → high χ. Tenants (victim + trapped exit) derive d ≈ 0.95 → f(d) ≈ 1.42 → very high χ. Local government (organized + constrained) derives d ≈ 0.55 → f(d) ≈ 0.75 → moderate-high χ, but organizational power modulates this upward. Tenant coalitions (organized + constrained + visible exit paths) derive d ≈ 0.40 → f(d) ≈ 0.40 → moderate χ with coalition scaling. The analytical observer at civilizational scope risks d ≈ 0.72 (false summit), naturalizing the contingent institutional arrangement (FIRPTA, non-recourse financing, carried-interest tax treatment) as inevitable capital dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CHECK: This constraint is at the tangled rope boundary (ε=0.58, approaching snare threshold ε≥0.66). The classification as tangled_rope requires demonstrating genuine coordination function alongside asymmetric extraction. COORDINATION FUNCTION: PE aggregation of single-family homes does solve a real coordination problem — fragmented ownership made capital markets impossible; bundling enabled securitization, unlocked liquidity, enabled professional management at scale, lowered information barriers for institutional capital. These are not fiction. ASYMMETRIC EXTRACTION: Simultaneously, the constraint extracts from prospective homebuyers (priced out) and tenants (rising rents, reduced maintenance). PE firms and institutional investors capture returns that would otherwise distribute as owner-occupancy equity. This extraction is also not fiction. ACTIVE ENFORCEMENT: The constraint requires active enforcement via: (1) regulatory vacuum (FIRPTA exemptions, state preemption of local rent control), (2) organized PE opposition to community ownership and rent control, (3) financial engineering (non-recourse financing, carried-interest tax treatment), (4) capital markets infrastructure (residential MBS, securitization rating). The constraint does not self-sustain; it requires continuous policy and market infrastructure support. Therefore, tangled_rope is the correct classification. If extraction escalates beyond χ≥0.66 (effective chi, including scope scaling), the classification would degrade toward snare. Measurement shows extractiveness rising from 0.25 (2008) to 0.58 (2023), suggesting the constraint is trending toward snare classification. At ε=0.75 (high snare threshold), the classification would flip and mandatrophy would require resolution. Current status: tangled_rope, NOT YET RESOLVED, but degrading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_intervention_threshold,
    'At what point do state/local rent control, acquisition restrictions, or community ownership mandates become binding constraints on PE market participation?',
    'Comparative analysis of jurisdictions with aggressive rent regulation (CA, NY, OR) vs permissive jurisdictions (TX, FL); tracking of PE portfolio growth rates and withdrawal decisions in response to regulatory changes',
    'If threshold is crossed: scaffold perspective confirmed, PE lock-in is temporary. If threshold cannot be crossed (capital mobility defeats regulation): snare perspective confirmed, lock-in is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention_threshold, empirical, 'Threshold at which regulation binds PE market participation').

omega_variable(
    tenant_coalition_power_ceiling,
    'Can organized tenant movements and community land trusts generate sufficient countervailing power to establish alternative ownership pathways, or does PE capital consolidation exceed their organizational capacity?',
    'Measurement of CLT acquisition pace vs PE acquisition pace; tracking of rent stabilization coverage expansion; analysis of coalition funding sources and political leverage',
    'If coalition power grows: scaffold sunset is real. If PE consolidation outpaces coalition growth: tangled rope will degrade to snare for prospective homebuyers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenant_coalition_power_ceiling, empirical, 'Capacity of tenant coalitions to build countervailing alternatives').

omega_variable(
    capital_market_arbitrage_stability,
    'Does the PE residential real estate securitization market require continuous PE inventory growth to maintain returns, or can it stabilize at current portfolio levels?',
    'Analysis of securitization spreads, refinancing requirements, and exit timelines for mature PE portfolios; tracking of capital inflow/outflow from residential MBS markets',
    'If market requires growth: extraction will accelerate (snare). If market stabilizes: extraction may plateau (tangled rope). If market reverses: exits may trigger forced sales, potentially opening ownership windows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_market_arbitrage_stability, empirical, 'Stability requirements of PE residential real estate securitization').

omega_variable(
    intergenerational_wealth_transfer_collapse,
    'Does PE lock-in of rental housing represent a permanent break in intergenerational wealth accumulation for middle-income households, or will alternative mechanisms (inheritance, state housing assistance) compensate?',
    'Longitudinal tracking of wealth inequality between owner and renter cohorts; measurement of intergenerational wealth transfer rates; policy analysis of alternative state-sponsored wealth-building mechanisms',
    'If permanent break: captures a fundamental shift in capitalism (wealth concentration via institutional ownership). If compensated: snare is mitigated by secondary pathways.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_wealth_transfer_collapse, preference, 'Whether PE lock-in breaks intergenerational wealth accumulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pe_rental_market_lockin, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pe_rental_tr_t0, pe_rental_market_lockin, theater_ratio, 0, 0.28).
narrative_ontology:measurement(pe_rental_tr_t8, pe_rental_market_lockin, theater_ratio, 8, 0.32).
narrative_ontology:measurement(pe_rental_tr_t15, pe_rental_market_lockin, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(pe_rental_be_t0, pe_rental_market_lockin, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(pe_rental_be_t8, pe_rental_market_lockin, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(pe_rental_be_t15, pe_rental_market_lockin, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pe_rental_market_lockin, resource_allocation).
narrative_ontology:boltzmann_floor_override(pe_rental_market_lockin, 0.35).
narrative_ontology:affects_constraint(pe_rental_market_lockin, owner_occupancy_wealth_gap).
narrative_ontology:affects_constraint(pe_rental_market_lockin, residential_real_estate_securitization).
narrative_ontology:affects_constraint(pe_rental_market_lockin, single_family_rental_maintenance_standards).
narrative_ontology:affects_constraint(pe_rental_market_lockin, community_land_trust_capacity).

% DUAL FORMULATION NOTE:
% PE lock-in decomposes into three structurally distinct constraints: (1) capital aggregation coordination (rope/tangled rope) — solved by PE securitization; (2) ownership access barrier (snare) — created by PE pricing; (3) tenant extraction mechanism (snare) — via rent optimization. These are linked: capital aggregation enables the access barrier and extraction mechanism. Separate constraint stories for (2) and (3) would capture their empirical distinctiveness; this story captures the hybrid coordination-extraction nature of the whole system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
