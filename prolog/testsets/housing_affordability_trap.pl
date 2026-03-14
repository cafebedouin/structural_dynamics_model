% ============================================================================
% CONSTRAINT STORY: housing_affordability_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_housing_affordability_trap, []).

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
 *   constraint_id: housing_affordability_trap
 *   human_readable: Housing Affordability Trap
 *   domain: economic/urban_policy/social
 *
 * SUMMARY:
 *   The housing affordability trap is a structural constraint where
 *   coordination mechanisms for housing allocation (mortgage markets,
 *   property development, rental systems) have become overlaid with
 *   extractive mechanisms (speculative capital, zoning restrictions, wage
 *   suppression, financialization) that concentrate benefits on existing
 *   property owners and financial intermediaries while imposing escalating
 *   costs on renters and aspiring homebuyers. The constraint exhibits both
 *   genuine coordination (housing must be allocated somewhere; mortgage
 *   lending enables access) and asymmetric extraction (property appreciation
 *   captures value from future market entrants without proportional labor or
 *   capital contribution). The rising extractiveness (0.28 → 0.58 over 20
 *   years) reflects financialization of housing markets and decoupling of
 *   housing prices from local wages. The low theater ratio (0.45) indicates
 *   the extraction is relatively direct and transparent, not hidden behind
 *   performative mechanisms. This distinguishes it from constraints like
 *   securitized mortgage infrastructure (piton-adjacent), which use rating
 *   agency and regulatory theater to obscure risk. The housing affordability
 *   trap is tangled rope at the analytical level because it genuinely
 *   coordinates housing allocation while simultaneously extracting from
 *   economically powerless agents (renters, first-time buyers) through
 *   mechanisms that beneficiaries experience as pure coordination.
 *
 * KEY AGENTS:
 *   - Low-Income Renters: Primary victims (powerless/trapped) — economically immobilized by wage insufficiency, no capital for exit, geographic lock to employment
 *   - First-Time Buyers: Secondary victims (moderate/constrained) — face down payment barriers, mortgage qualification restrictions, wage stagnation relative to prices; some agency through dual-income or inheritance but high cost
 *   - Landlord Capital & Development Finance: Primary beneficiaries (institutional/arbitrage) — capture appreciation gains, mortgage spreads, rental income; full exit optionality; experience the constraint as pure coordination
 *   - Existing Homeowners (Asset-Rich): Mixed position (powerful/mobile) — benefit from appreciation; indirectly extract from younger generations; some suppression from property tax and rate risk but high exit optionality
 *   - Housing Reform Coalition: Organized actors (organized/constrained) — advocate for zoning reform, public housing, rent stabilization, down-payment assistance; see the trap as policy-solvable with sunset timeline
 *   - REIT & Securitization Infrastructure: Institutional actors (institutional/arbitrage) — maintain extraction through opacity and complexity; substantially performative (rating agencies, regulatory theater); persist through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees tangled rope classification; genuine coordination layered with contingent extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(housing_affordability_trap, 0.58).
domain_priors:suppression_score(housing_affordability_trap, 0.68).
domain_priors:theater_ratio(housing_affordability_trap, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(housing_affordability_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(housing_affordability_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(housing_affordability_trap, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(housing_affordability_trap, tangled_rope).
narrative_ontology:human_readable(housing_affordability_trap, "Housing Affordability Trap").
narrative_ontology:topic_domain(housing_affordability_trap, "economic/urban_policy/social").

domain_priors:requires_active_enforcement(housing_affordability_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(housing_affordability_trap, landlord_capital).
narrative_ontology:constraint_beneficiary(housing_affordability_trap, development_finance).
narrative_ontology:constraint_beneficiary(housing_affordability_trap, property_speculation).
narrative_ontology:constraint_victim(housing_affordability_trap, low_income_renters).
narrative_ontology:constraint_victim(housing_affordability_trap, first_time_buyers).
narrative_ontology:constraint_victim(housing_affordability_trap, younger_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RENTER HOUSEHOLD (SNARE) — Trapped by structural factors: inadequate wages relative to rent burden, no capital for down payment, credit barriers, and geographic immobility (employment is local). Cannot exit the rental market or access homeownership. Bears maximum extraction through rent capture with no meaningful coordination benefit. Theater is low here — the extraction is straightforward, not performative.
constraint_indexing:constraint_classification(housing_affordability_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-TIME HOMEBUYER (TANGLED ROPE) — Constrained by down payment requirements, mortgage qualification criteria, and wage stagnation relative to property price escalation. Experiences genuine coordination function (mortgage lending enables access to housing) alongside asymmetric extraction (borrower captures leverage; lender captures most of the total gain). Higher agency than trapped renters but significant extraction and suppression of alternatives. Can theoretically exit through inheritance, dual-income, or geographic relocation, but costs are substantial.
constraint_indexing:constraint_classification(housing_affordability_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEVELOPMENT FINANCE & LANDLORD CAPITAL (ROPE) — Primary beneficiaries. Experience the housing constraint as pure coordination: aggregating capital for development, standardizing mortgage terms, enabling property markets. The constraint solves a genuine coordination problem (matching renters with housing supply). These actors have full exit optionality — capital arbitrages to other markets if housing returns deteriorate. Net beneficiary position. The constraint appears as Rope from their perspective because they genuinely coordinate housing allocation.
constraint_indexing:constraint_classification(housing_affordability_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXISTING HOMEOWNER / ASSET-RICH (TANGLED ROPE) — Mixed position. Benefits from property appreciation (asset capture). Also bears costs through property tax, maintenance, and exposure to rate shocks. Has meaningful exit optionality (can sell, relocate, downsize). Experiences the constraint as mixed coordination-extraction: property markets coordinate housing allocation, but pricing dynamics extract from new market entrants and renters, which indirectly extracts from this agent's future heirs and from community members they know. Perspectival gap arises from temporal asymmetry: current benefit, future intergenerational cost.
constraint_indexing:constraint_classification(housing_affordability_trap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HOUSING POLICY REFORM COALITION (SCAFFOLD) — Organized agents (advocacy groups, progressive legislators, housing rights movements) see the affordability trap as a temporary institutional failure with policy sunset: zoning reform, public housing expansion, rent stabilization, down-payment assistance, and inclusionary zoning are building alternative housing pathways. This perspective sees low effective extraction because the coalition has agency and identifies clear policy interventions with sunsets. Theater is low (direct policy mechanisms, not performative). Suppression is high (regulatory and incumbent resistance), but declining over time as public pressure builds.
constraint_indexing:constraint_classification(housing_affordability_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REIT INFRASTRUCTURE & SECURITIZATION (PITON) — Institutional apparatus (mortgage-backed securities, REITs, rating agencies) maintains the housing extraction through performative stability claims. The apparatus is substantially degraded: it relies on rating agency theater (AAA ratings on mortgage pools with embedded risk), securitization complexity that obscures actual risk, and regulatory theater (stress tests that don't stress test). The mechanism persists through inertia despite low functional purpose — it primarily transfers housing from an asset to an investment product, enabling extraction without coordination function.
constraint_indexing:constraint_classification(housing_affordability_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, housing coordination is both genuine and extractive. Genuine coordination: markets allocate housing to where demand is highest, creating price signals that guide investment. Extractive layering: speculative dynamics, zoning constraints, financialization, and wage suppression generate extraction above coordination cost. The chi formula captures this mix. Theater is moderate (policy theater around 'market efficiency,' 'supply-side solutions' that don't increase actual supply). The analytical perspective sees tangled rope, not mountain, because the extraction is structurally contingent, not natural law.
constraint_indexing:constraint_classification(housing_affordability_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(housing_affordability_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(housing_affordability_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(housing_affordability_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(housing_affordability_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(housing_affordability_trap, TR),
    TR >= 0.70.

:- end_tests(housing_affordability_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the measurement interval. At t=0 (approximately 2004, post-housing bubble but pre-financial crisis), housing extraction was 0.28 — primarily wage-based (affordable rents for those employed in growing metros). By t=20 (approximately 2024), extraction has escalated to 0.58 through three mechanisms: (1) financialization and REIT capital seeking housing returns independent of local fundamentals; (2) zoning restrictions and NIMBY coalitions restricting supply, creating scarcity rents; (3) wage stagnation in many sectors despite productivity gains, reducing renter purchasing power. Suppression (0.68): High and structural. Renters face multiple suppression mechanisms: lack of capital for down payment (no exit path to ownership), inadequate wages (no exit through income growth), employment concentration (no exit through relocation), and credit barriers. First-time buyers face mortgage qualification criteria, down payment requirements, and wage inadequacy. Existing homeowners have some suppression (property tax, rate risk) but high exit optionality. Theater ratio (0.45): Moderate and stable. Housing extraction is relatively transparent — price escalation is observable, wage stagnation is documented, zoning restrictions are explicit policy. There is policy theater around 'market efficiency' and 'supply-side solutions,' but the core mechanisms are not hidden. Compare to securitized mortgage theater (ratings, complexity), which would produce theater_ratio > 0.70.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Snare (powerless/trapped) and Rope (institutional/arbitrage) is the maximal possible gap (opposite classifications from the same constraint). This reveals that the constraint's classification depends entirely on structural position — there is no objective classification independent of perspective. The gap arises from (1) opposite beneficiary/victim status, (2) opposite exit optionality (trapped vs arbitrage), and (3) opposite power levels (powerless vs institutional). Each of these independently would produce different classifications; together they produce maximum divergence. The analytical observer's Tangled Rope classification is the key diagnostic: it says 'this is not Rope everywhere and Snare nowhere; it is genuinely both, with the split determined by power and exit.' The false summit risk is high: someone might claim the constraint is 'really' Rope because markets do coordinate, or 'really' Snare because extraction is severe for renters. The truth is both. The constraint's function depends on which agent you ask.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) follows from structural position, not abstract power. Low-income renters: beneficiary status is absent, victim status is high, exit is trapped → d ≈ 0.95 (full target). Landlord capital: beneficiary status is high, victim status absent, exit is arbitrage → d ≈ 0.10 (full beneficiary). First-time buyers: mixed (both victim from price barriers and beneficiary if they eventually own) + constrained exit → d ≈ 0.55 (symmetric). Existing homeowners: beneficiary from appreciation + some victim from future cost + mobile exit → d ≈ 0.45 (slight beneficiary). The sigmoid f(d) converts these to experienced extractiveness amplification: trapped agents face f(d) ≈ 1.42 (max amplification); arbitrage actors face f(d) ≈ -0.12 (negative, subsidy); symmetric agents face f(d) ≈ 0.65 (moderate). Scope modifier σ(S) at national scope is 1.0 (no amplification; national markets are standard scope). The chi formula produces: trapped renters experience χ ≈ 0.58 × 1.42 × 1.0 = 0.82 (high effective extraction); landlords experience χ ≈ 0.58 × (-0.12) × 1.0 = -0.07 (subsidy/benefit); first-time buyers experience χ ≈ 0.58 × 0.65 × 1.0 = 0.38 (moderate extraction). This directionality structure explains why trapped agents see Snare while beneficiaries see Rope: the same base extractiveness (0.58) produces radically different experienced extraction (0.82 vs -0.07) depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The housing affordability trap resolves the mandatrophy by demonstrating that Tangled Rope is the correct analytical classification and that the perspectival gap (Snare vs Rope) is diagnostic information, not a mistake. The constraint genuinely coordinates housing allocation (Rope function: matching supply to demand, enabling access through lending). The constraint also genuinely extracts (Snare function: prices rise above coordination cost due to scarcity, speculation, wage suppression). The mandatrophy asks: 'Is this extraction or coordination?' Answer: 'Both, with the ratio determined by structural position.' Low-income renters experience extraction because they cannot exit; landlord capital experiences coordination because it can arbitrage. Neither is wrong. The engine should classify this as Tangled Rope, not as a single type viewed from different angles. The scaff perspective (policy reform coalition) is key to mandatrophy resolution: it identifies that the current extraction overlay (financialization, zoning restriction) is policy-contingent, not fundamental. If zoning were liberalized and public housing were expanded, the chi value could shift from 0.58 to 0.30 (lower extractiveness while maintaining coordination). This proves the extraction is not natural law; it is institutional choice. The analytical observer's classification as Tangled Rope (not Mountain) confirms this: the constraint is tangled rope because it remains policy-solvable, not a mountain because it is not fundamental limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_productivity_decoupling,
    'Is the housing affordability crisis primarily a wage stagnation problem (wages decoupled from productivity gains) or a housing supply/zoning problem (restricted supply driving price escalation)?',
    'Cross-national comparison: countries with high productivity growth but also high wage growth (Germany, Nordic states) show different affordability trajectories than US-style wage stagnation despite productivity growth. Decompose housing cost inflation into supply-driven vs demand-driven components via econometric analysis.',
    'If wage-driven: extraction model shifts upstream to labor market constraints (separate constraint story). If supply-driven: zoning and NIMBY coalitions are primary extractors. If both: tangled rope classification holds; neither alone explains the trap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_productivity_decoupling, empirical, 'Wage stagnation vs supply constraint as root cause').

omega_variable(
    speculation_vs_fundamentals,
    'How much of housing price escalation is speculative capital seeking returns vs fundamentals (population growth, construction costs, land scarcity)?',
    'Empirical analysis of price-to-rent ratios across markets; correlation between institutional capital inflows (REIT growth, foreign investment) and price acceleration relative to local fundamentals; historical comparison of pre-financialization (1970s-1990s) vs post-financialization (2000s+) housing price dynamics.',
    'If primarily speculative: REIT infrastructure perspective (piton) is key extraction mechanism; policy can constrain financialization. If primarily fundamental: extraction is coordination artifact, not exploitative overlay; policy options limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculation_vs_fundamentals, empirical, 'Speculative capital vs fundamental drivers of price escalation').

omega_variable(
    zoning_reform_feasibility,
    'Can zoning reform (upzoning, eliminating single-family zoning) produce affordability gains, or does capital simply capture the value created by density allowances?',
    'Natural experiments: Minneapolis zoning reform (eliminated single-family zoning 2019), Oregon statewide reform (2019), California duplex legalization (2022). Track housing supply growth, price impacts, affordability outcomes 5-10 years post-reform. Compare to cities that did not reform.',
    'If zoning reform works: scaffold perspective is substantive; affordability trap has policy sunset. If capital captures zoning value: zoning reform reduces one suppression mechanism but leaves extraction intact; deeper intervention (public housing, rent control, land value taxation) required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zoning_reform_feasibility, empirical, 'Whether zoning reform produces durable affordability gains').

omega_variable(
    intergenerational_wealth_transfer,
    'Is the housing trap primarily extractive (current generations extracting from younger generations) or a coordination mechanism that distributes the wealth transfer problem across time?',
    'Historical asset value analysis: what fraction of current real estate value is attributable to scarcity/regulation vs genuine improvements? Intergenerational wealth transfer modeling: does early homeownership enable wealth accumulation that benefits children, or does it concentrate wealth among lucky cohorts (those who bought before price escalation)?',
    'If extractive transfer: the constraint is snare-like from younger generations'' perspective; policy must address asset redistribution. If coordination artifact: the constraint is rope-like; policy can work within it by enabling earlier access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_wealth_transfer, conceptual, 'Whether housing is extractive transfer or wealth coordination').

omega_variable(
    public_housing_viability,
    'Is large-scale public housing production capable of delivering affordability at scale without degrading to low-quality, segregated provision?',
    'Historical review of public housing provision in different regimes: Vienna social housing (30% of city supply, mixed-income integration), Singapore HDB (80% of population, high quality), post-WWII UK council housing, US public housing failures. Isolate factors driving success vs degradation.',
    'If viable: scaffold sunset is real; policy can exit the trap through institutional alternatives. If not viable: trap is structurally deeper; market provision may be irreplaceable despite its extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_housing_viability, empirical, 'Whether public housing can deliver affordability at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(housing_affordability_trap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(haf_tr_t0, housing_affordability_trap, theater_ratio, 0, 0.3).
narrative_ontology:measurement(haf_tr_t10, housing_affordability_trap, theater_ratio, 10, 0.38).
narrative_ontology:measurement(haf_tr_t20, housing_affordability_trap, theater_ratio, 20, 0.45).
narrative_ontology:measurement(haf_tr_t5, housing_affordability_trap, theater_ratio, 5, 0.33).

% Extraction over time
narrative_ontology:measurement(haf_be_t0, housing_affordability_trap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(haf_be_t10, housing_affordability_trap, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(haf_be_t20, housing_affordability_trap, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(haf_be_t5, housing_affordability_trap, base_extractiveness, 5, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(housing_affordability_trap, resource_allocation).
narrative_ontology:affects_constraint(housing_affordability_trap, wage_stagnation_labor_market).
narrative_ontology:affects_constraint(housing_affordability_trap, zoning_supply_restriction).
narrative_ontology:affects_constraint(housing_affordability_trap, mortgage_securitization_financialization).

% DUAL FORMULATION NOTE:
% Housing affordability is downstream of multiple structural constraints: wage stagnation in labor markets sets renter/buyer purchasing power; zoning and NIMBY coalitions restrict supply; mortgage securitization and REIT capital enable speculative overlays. The housing affordability trap story captures the hybrid constraint that emerges when all three upstream constraints are active simultaneously. Decomposition into separate upstream stories (one per mechanism) would produce ε values ranging from 0.25 (zoning as pure coordination failure) to 0.45 (wage stagnation) to 0.65 (securitization financialization). The housing story captures the combined effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(housing_affordability_trap, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
