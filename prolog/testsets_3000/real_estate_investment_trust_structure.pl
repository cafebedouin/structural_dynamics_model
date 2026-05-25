% ============================================================================
% CONSTRAINT STORY: real_estate_investment_trust_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_real_estate_investment_trust_structure, []).

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
 *   constraint_id: real_estate_investment_trust_structure
 *   human_readable: Real Estate Investment Trust Structure
 *   domain: finance/real_estate/regulatory_framework
 *
 * SUMMARY:
 *   Real Estate Investment Trusts (REITs) are a statutory framework enabling
 *   institutional capital concentration in real property. Originating in 1960
 *   as a mechanism to democratize real estate investment, REITs evolved into
 *   vehicles for massive institutional capital deployment, portfolio
 *   consolidation, and professional rent optimization. The structure creates
 *   a fundamental tension: it genuinely solves the coordination problem of
 *   providing liquidity and economies of scale for real estate capital, yet
 *   simultaneously enables systematic extraction from tenants and
 *   displacement of small property owners. The constraint exhibits all six DR
 *   types from different institutional positions, revealing how a technically
 *   rational coordination mechanism can embed extraction at scale. Theater
 *   has increased as the original regulatory intent (preventing corporate tax
 *   avoidance, enabling small-investor access) has been circumvented through
 *   complex ownership structures, leverage, and opacity.
 *
 * KEY AGENTS:
 *   - Residential Tenants: Primary victims (powerless/trapped) — bear extraction via rent increases driven by REIT capital redeployment and management optimization
 *   - Small Property Owners: Secondary victims (moderate/constrained) — face market consolidation and are forced to sell to REITs, reinforcing concentration
 *   - Asset Management Firms: Primary beneficiaries (institutional/arbitrage) — capture management fees, depreciation benefits, and optimization spreads; have arbitrage options to reallocate capital
 *   - Pension Funds & Long-term Investors: Secondary beneficiaries (institutional/arbitrage) — benefit from low-cost real estate exposure and tax advantages; have alternative investment options
 *   - Housing Policy Coalition: Organized agents (organized/constrained) — regulate and constrain REIT expansion through zoning, rent control, and tax policy; building alternative structures (CLTs, cooperatives)
 *   - Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains statutory REIT structure through inertia; original coordination function has been overshadowed by extraction mechanisms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing property concentration as inherent to capitalism rather than a policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(real_estate_investment_trust_structure, 0.58).
domain_priors:suppression_score(real_estate_investment_trust_structure, 0.65).
domain_priors:theater_ratio(real_estate_investment_trust_structure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(real_estate_investment_trust_structure, extractiveness, 0.58).
narrative_ontology:constraint_metric(real_estate_investment_trust_structure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(real_estate_investment_trust_structure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(real_estate_investment_trust_structure, tangled_rope).
narrative_ontology:human_readable(real_estate_investment_trust_structure, "Real Estate Investment Trust Structure").
narrative_ontology:topic_domain(real_estate_investment_trust_structure, "finance/real_estate/regulatory_framework").

domain_priors:requires_active_enforcement(real_estate_investment_trust_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(real_estate_investment_trust_structure, institutional_capital_holders).
narrative_ontology:constraint_beneficiary(real_estate_investment_trust_structure, reit_management_companies).
narrative_ontology:constraint_beneficiary(real_estate_investment_trust_structure, large_landlords).
narrative_ontology:constraint_victim(real_estate_investment_trust_structure, tenant_populations).
narrative_ontology:constraint_victim(real_estate_investment_trust_structure, small_property_owners).
narrative_ontology:constraint_victim(real_estate_investment_trust_structure, housing_affordability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESIDENTIAL TENANT (SNARE) — Trapped by housing scarcity and geographic immobility. REITs consolidate properties into portfolios optimized for institutional investor returns, driving rent increases via professional management and capital redeployment. Tenants cannot exit without material cost (relocation, search time, loss of social bonds). Maximum extraction experienced.
constraint_indexing:constraint_classification(real_estate_investment_trust_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL PROPERTY OWNER (SNARE) — Constrained by capital requirements and market consolidation. REITs aggregate properties and achieve economies of scale that individual owners cannot match. Property values inflate due to REIT demand, making acquisition unaffordable for small operators. Exit requires selling to REIT, reinforcing consolidation. Significant extraction with limited alternatives.
constraint_indexing:constraint_classification(real_estate_investment_trust_structure, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ASSET MANAGEMENT FIRM (TANGLED ROPE) — Primary beneficiary with arbitrage options (can deploy capital across real estate, equities, bonds). Benefits from tax-deferred REIT structure enabling capital concentration and portfolio optimization. Genuine coordination function: REITs solve the liquidity problem for property investors by securitizing real estate. Simultaneously extracts via management fees, depreciation capture, and rent optimization that concentrates returns. Moderate to high effective extraction but with real coordination benefit.
constraint_indexing:constraint_classification(real_estate_investment_trust_structure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PENSION FUND INVESTOR (ROPE) — Benefits from REIT structure as pure coordination mechanism: enables large-scale real estate exposure without operational burden. Tax advantages and liquidity solve genuine problems for long-term capital allocation. Experiences constraint as cooperative framework with minimal extraction — low-cost capital deployed into productive assets. Some benefit reallocates to management layer (fee extraction), but net experience is coordination.
constraint_indexing:constraint_classification(real_estate_investment_trust_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HOUSING POLICY COALITION (SCAFFOLD) — Organized actors (affordable housing advocates, regulatory bodies, tenant unions) perceive REIT structure as a temporary problem solvable through policy: Community Land Trusts, inclusionary zoning requirements, rent stabilization, and capital gains taxation are sunset mechanisms that could redistribute REIT returns toward affordable housing. Theater ratio declining as policy alternatives gain legitimacy. Sunset logic: if implemented, these constraints redirect REIT extraction toward public benefit rather than eliminating REITs entirely.
constraint_indexing:constraint_classification(real_estate_investment_trust_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — REIT regulations (1960 requirement for qualified real estate focus, 90% dividend payout rules, prohibition of REIT-to-REIT ownership) are largely theatrical: they were designed to prevent corporate tax avoidance but now function as institutional inertia. REITs have evolved into complex financial vehicles with leverage, derivatives, and opaque management structures that escape the original regulatory intent. Theater ratio high because regulatory compliance is performative — the original coordination function (enabling small-investor real estate exposure) has been overwhelmed by institutional consolidation. Maintained through regulatory habit rather than functional necessity.
constraint_indexing:constraint_classification(real_estate_investment_trust_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capital concentration in real estate is inherent to capitalist dynamics: wherever control over essential resources (shelter) can be disaggregated from use, extractive structures emerge. REITs are merely the institutional form that capitalizes this inevitability. The constraint appears unchangeable because the underlying property rights structure is immutable. However, this naturalizes what is actually a policy choice: property taxation, rent control, tenant protections, and public ownership are all policy mechanisms that could be structured differently. The mountain classification is a false summit.
constraint_indexing:constraint_classification(real_estate_investment_trust_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(real_estate_investment_trust_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(real_estate_investment_trust_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(real_estate_investment_trust_structure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(real_estate_investment_trust_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(real_estate_investment_trust_structure, TR),
    TR >= 0.70.

:- end_tests(real_estate_investment_trust_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the REIT's dual nature. The structure provides genuine coordination (capital liquidity, operational efficiency, risk diversification) but embeds systematic extraction through management fees (~1% AUM), depreciation recapture, and rent optimization practices. The extraction has grown over the 20-year interval (0.32 → 0.58) as consolidation increased and management practices evolved toward active optimization rather than passive holding. The trajectory reflects rent-seeking layered onto an initially neutral coordination mechanism. Suppression (0.65): Moderate-high. Tenants face multiple barriers: housing scarcity limits exit options, lease terms are standardized contracts with minimal negotiation capacity, and information asymmetry prevents visibility into REIT ownership and management practices. Rent increases are framed as 'market dynamics' rather than deliberate extraction. However, suppression is not total — tenant organizing, legal challenges to evictions, and regulatory pushback are possible. Theater ratio (0.68): Moderate-high. Significant theatrical elements: regulatory compliance theater (original intent circumvented), investor relations theater (professional management framed as beneficial), and market-efficiency theater ('market rent' framing conceals discretionary pricing). Theater has increased as regulatory structure became vestigial but was maintained for legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the fundamental asymmetry in the REIT structure. Beneficiaries perceive pure coordination (Rope) or beneficial arrangement (Tangled Rope with net gains) — they see liquidity and efficiency. The asset management layer captures this benefit and experiences it as low extraction. Tenants perceive extraction with minimal coordination benefit (Snare) — the efficiency gains accrue entirely to capital, not to housing occupancy or affordability. Small property owners perceive displacement through consolidation (Snare) — they are being eliminated as competitors. The organized policy coalition perceives a solvable problem with exit mechanisms (Scaffold) — regulatory tools can redirect REIT returns. The regulatory framework perceives its own degradation (Piton) — original protections have become vestigial. The civilizational analytical observer risks seeing property concentration as natural law (Mountain), but the perspectival gap itself reveals this is a policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural position relative to extraction flow. Asset management firms are primary beneficiaries with arbitrage options (low d ~0.15) — they can reallocate capital and experience the constraint as favorable coordination. Pension funds have similar benefits but slightly higher dependency (d ~0.20) — they benefit from low-cost access but have fewer exit options than active asset managers. Tenants are trapped (d ~0.95) — no exit without material cost, bear full extraction. Small property owners are constrained but vulnerable to consolidation (d ~0.80) — they face high-cost exit (must sell to REIT or accept declining competitiveness). Regulatory framework has captured benefits from the tax structure (d ~0.25) — maintains it despite drift from original intent. Housing policy coalition is organized with some constraint (d ~0.55) — can regulate and constrain but cannot eliminate the structure without major policy change.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The six-type range resolves the fundamental mandate conflict in REIT regulation. The original mandate (1960) was to enable small-investor real estate access — this would be Rope classification. The evolved mandate (effective 2000+) is to enable large-scale institutional capital deployment — this is Tangled Rope or Snare depending on tenant impact. The constraint has drifted from its stated purpose because the coordination function (capital liquidity) and the extraction function (rent optimization) are not separable in the current structure. Resolving the mandatrophy requires either: (1) sunset the REIT structure and rebuild with affordability constraints (Scaffold logic), (2) regulate REITs toward genuine coordination (move from Snare toward Rope), or (3) acknowledge that capital concentration is the actual function and reframe the mandate accordingly. The analytical observer's mountain classification is false — property concentration is not a natural law but a policy choice embedded in tax structure, regulatory design, and property rights frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tax_haven_opacity,
    'To what extent do opaque capital structures (offshore ownership, layered management companies, complex derivatives) allow REITs to extract without full regulatory visibility?',
    'Beneficial ownership disclosure requirements; cross-border capital flow tracking; shadow REIT analysis comparing regulatory-visible to economically-controlled properties',
    'If opacity is significant (>30% of extraction hidden): true extractiveness is higher than measured. Suppression increases because tenants and regulators lack information to organize countermeasures. Classification may shift from Tangled Rope to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_haven_opacity, empirical, 'Opacity of capital structures enabling hidden extraction').

omega_variable(
    coordination_function_necessity,
    'Is the REIT structure still necessary to solve the liquidity problem for real estate investment, or have alternatives (crowdfunding, tokenization, community ownership) rendered the tax-advantaged REIT structure redundant?',
    'Comparative analysis of capital access costs before/after REIT alternatives emerge; measurement of whether REITs still provide lowest-cost capital compared to CLTs, housing cooperatives, and equity crowdfunding platforms',
    'If alternatives are equally or more efficient: REIT claim to coordination function is theater (Piton classification strengthens). If REITs remain lowest-cost: coordination function is genuine (Tangled Rope confirmed). Impacts scaffold sunset feasibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether REIT structure remains necessary for real estate liquidity').

omega_variable(
    rent_increase_causation,
    'How much of observed rent increases in REIT-acquired properties is driven by the REIT''s active optimization versus market appreciation that would occur regardless of ownership structure?',
    'Matched comparison: rent trajectories in REIT-acquired vs non-REIT-acquired properties in the same markets, controlling for vintage, class, and local demand. Measurement of active management effects (occupancy optimization, amenity capture) versus passive appreciation.',
    'If REIT effect is <10% of observed increases: tenants'' snare experience is partly attribution error; underlying constraint is housing scarcity (not REIT-specific). If REIT effect >30%: extraction mechanism is active and deliberate, not incidental. Affects suppression magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_increase_causation, empirical, 'REIT-driven rent increases versus market appreciation').

omega_variable(
    consolidation_threshold,
    'At what market concentration level (% of regional housing stock held by top-N REITs) do economies of scale tip into monopolistic extraction?',
    'Regional housing market concentration analysis (Herfindahl index); correlation between REIT market share and rent volatility, vacancy rates, tenant turnover, and management fee escalation',
    'If threshold < 15% concentration: REITs extract monopolistic rents even at modest scale. If threshold > 40%: market competition constrains extraction until very high concentration. Affects whether constraint is inherently extractive or only problematic at scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consolidation_threshold, empirical, 'Market concentration threshold for monopolistic extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(real_estate_investment_trust_structure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reit_tr_t0, real_estate_investment_trust_structure, theater_ratio, 0, 0.52).
narrative_ontology:measurement(reit_tr_t10, real_estate_investment_trust_structure, theater_ratio, 10, 0.6).
narrative_ontology:measurement(reit_tr_t20, real_estate_investment_trust_structure, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(reit_be_t0, real_estate_investment_trust_structure, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(reit_be_t10, real_estate_investment_trust_structure, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(reit_be_t20, real_estate_investment_trust_structure, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(real_estate_investment_trust_structure, resource_allocation).
narrative_ontology:affects_constraint(real_estate_investment_trust_structure, housing_affordability_crisis).
narrative_ontology:affects_constraint(real_estate_investment_trust_structure, tenant_displacement_dynamics).
narrative_ontology:affects_constraint(real_estate_investment_trust_structure, small_landlord_consolidation).

% DUAL FORMULATION NOTE:
% REIT structure decomposes into coordination and extraction functions. The pure coordination aspect (enabling real estate liquidity) has ε ~0.05 (Rope). The extraction mechanism (rent optimization and capital concentration) has ε ~0.65 (Snare). The combined structure averages to ε ~0.58 (Tangled Rope). All three should be modeled separately: reit_liquidity_coordination (ε=0.05), reit_rent_optimization_extraction (ε=0.65), and reit_capital_concentration (ε=0.52). This story represents the integrated institutional arrangement. Decomposition enables precise targeting of policy interventions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(real_estate_investment_trust_structure, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
