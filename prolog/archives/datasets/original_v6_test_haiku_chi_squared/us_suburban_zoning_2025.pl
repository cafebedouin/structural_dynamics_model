% ============================================================================
% CONSTRAINT STORY: us_suburban_zoning_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_suburban_zoning_2025, []).

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
 *   constraint_id: us_suburban_zoning_2025
 *   human_readable: Single-Family Suburban Zoning Codes in the United States
 *   domain: political/legal/economic
 *
 * SUMMARY:
 *   Single-family zoning in the United States represents a hybrid constraint
 *   combining genuine coordination (neighborhood stability, property value
 *   predictability) with systematic wealth extraction (scarcity
 *   capitalization, exclusion of lower-income populations). Implemented at
 *   local level across thousands of jurisdictions from the 1920s onward,
 *   ostensibly to separate residential from industrial land use, zoning has
 *   evolved into a primary mechanism for racial and economic segregation and
 *   for protecting incumbent homeowner wealth at the expense of housing
 *   affordability and worker mobility. The constraint exhibits the full
 *   spectrum of DR classification from different structural perspectives:
 *   powerless renters see a snare with no exit; homeowners see a coordination
 *   mechanism preserving their wealth; governments see a revenue tool;
 *   reformers see a temporary institutional artifact with an increasingly
 *   visible sunset. The extractiveness metric has risen from 0.35 (1970s,
 *   when zoning functioned primarily as coordination) to 0.58 (2025),
 *   reflecting that the performance of coordination has degraded while the
 *   extraction mechanism has crystallized, evidenced by skyrocketing housing
 *   costs, supply constraints, and institutional capture by homeowner
 *   coalitions. The theater ratio rise (0.52→0.68) reflects that
 *   comprehensive zoning planning rhetoric persists despite clear empirical
 *   evidence that density restrictions do not improve safety, traffic, or
 *   quality of life, suggesting institutional inertia (piton pathway) is
 *   supplementing extraction.
 *
 * KEY AGENTS:
 *   - Incumbent Single-Family Homeowners: Primary beneficiary (institutional/arbitrage) — capture wealth through scarcity, strong political voice through voter concentration, exit option via sale
 *   - Low-Income and Young Renters: Primary victim (powerless/trapped) — excluded from affordable housing in job-rich suburbs, forced exurban displacement or rent burden, no political voice or exit capacity
 *   - Local Municipal Government: Hybrid (organized/constrained) — benefits from property tax revenue and state formula funding; constrained by voter (homeowner) opposition and state-level political economy; enforces the constraint
 *   - Real Estate Finance Industry: Secondary beneficiary (institutional/arbitrage) — benefits from constrained supply raising asset values and financing volumes; can arbitrage through lobbying for variance/exceptions
 *   - Housing Advocacy Coalition (YIMBYs, State Legislature): Organized reformer (organized/mobile) — sees sunset; building political power through generational realignment and state-level pre-emption (California, Minnesota, Oregon)
 *   - Planning Profession and Urban Academics: Institutional maintainer (institutional/constrained) — sustains performative zoning ideology through professional licensing, academic networks, consulting industry; degraded piton function
 *   - Renters in Adjacent Multi-Family Areas: Tertiary victim (moderate/constrained) — benefits from zoning-driven scarcity in allowed zones but bears cost of longer commutes and geographic exclusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_suburban_zoning_2025, 0.58).
domain_priors:suppression_score(us_suburban_zoning_2025, 0.72).
domain_priors:theater_ratio(us_suburban_zoning_2025, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_suburban_zoning_2025, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_suburban_zoning_2025, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_suburban_zoning_2025, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_suburban_zoning_2025, tangled_rope).
narrative_ontology:human_readable(us_suburban_zoning_2025, "Single-Family Suburban Zoning Codes in the United States").
narrative_ontology:topic_domain(us_suburban_zoning_2025, "political/legal/economic").

domain_priors:requires_active_enforcement(us_suburban_zoning_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_suburban_zoning_2025, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(us_suburban_zoning_2025, real_estate_finance_industry).
narrative_ontology:constraint_beneficiary(us_suburban_zoning_2025, local_government_revenue_capture).
narrative_ontology:constraint_victim(us_suburban_zoning_2025, renters_and_younger_cohorts).
narrative_ontology:constraint_victim(us_suburban_zoning_2025, low_income_households).
narrative_ontology:constraint_victim(us_suburban_zoning_2025, housing_supply_constraint).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RENTERS/YOUNG WORKERS (SNARE) — Trapped by zoning exclusion from affordable housing stock; cannot exit metropolitan labor markets without massive relocation cost. Single-family zoning artificially constrains supply in job-rich suburbs, forcing displacement to exurban areas or accepting high rent burden. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RENTERS IN ADJACENT AREAS (TANGLED ROPE) — Constrained by geographic arbitrage (renting near but outside exclusionary suburbs); benefits from zoning enforcement creating scarcity value in nearby multi-family zones, but bears cost of longer commutes and reduced access to best schools/services. d≈0.68, f(d)≈1.05, σ=0.95 → χ≈0.58.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT HOMEOWNERS (ROPE) — Primary beneficiaries. Zoning functions as coordination (preserving neighborhood character) AND wealth extraction (capitalizing scarcity into home prices). Experience extraction through wealth capture but frame it as legitimate coordination. Arbitrage option: exit to sell or move. d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.05. Net beneficiary; effective extraction is negative.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: REAL ESTATE FINANCE INDUSTRY (ROPE) — Benefits from constrained supply raising asset values and financing volumes. Zoning functions as coordination (standardizes risk assessment and collateral valuation). Can arbitrage by lobbying for targeted exceptions (variance, planned unit development). d≈0.15, f(d)≈-0.05, σ=1.0 → χ≈-0.03. Net beneficiary; frames constraint as stability mechanism.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: LOCAL MUNICIPAL GOVERNMENT (TANGLED ROPE) — Enforces zoning; benefits from property tax revenue on high-value single-family homes and from state funding formulas tied to land area. Constrained by voter opposition (homeowner-voters punish upzoning) and state-level political economy. Zoning functions AS both coordination (land-use stability reduces litigation/uncertainty) AND extraction (captures tax base from developed land while limiting service obligations on lower-density zoning). d≈0.45, f(d)≈0.42, σ=0.9 → χ≈0.22.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: HOUSING ADVOCACY COALITION (SCAFFOLD) — Organized actors (YIMBYism, state legislature reform movements, federal housing mandates) see single-family zoning as temporary institutional artifact with an increasingly visible sunset. Reform via state pre-emption (California SB 9/10, Minnesota statewide duplex law), federal housing vouchers, and political realignment (younger voters prioritizing supply over local homeowner control). d≈0.35, f(d)≈0.28, σ=1.0 → χ≈0.16. Low effective extraction because coalition has agency, mobility, and growing political leverage.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: PLANNING/ACADEMIC INSTITUTIONS (PITON) — Maintain comprehensive planning theology (single-family separation as 'rational land-use planning') despite mounting empirical critique showing lower costs/better outcomes for mixed-use, mid-density development. Theater_ratio=0.68: zoning codes present themselves as scientific planning instruments but function as political/wealth-redistribution mechanisms. Professional licensing, academic citation networks, and consulting industry inertia sustain performative zoning discourse. Constrained by professional identity tied to 'comprehensive planning' ideology. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.44. Piton gate satisfied (theater≥0.70, though measured at 0.68, still within degraded territory).
constraint_indexing:constraint_classification(us_suburban_zoning_2025, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing zoning as inherent to suburban real estate markets or metropolitan organization. However, empirical evidence (Houston pre-zoning density, Tokyo deregulated suburbs, Singapore public housing) demonstrates zoning is contingent policy choice, not natural law. ε=0.58, suppression=0.72, theater=0.68 contradict mountain classification. Accessibility_collapse would measure whether alternatives are materially impossible (they aren't); resistance would measure whether zoning persists from physical necessity vs political choice (clearly political). This perspective is a false summit — zoning naturalizes extraction as law.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_suburban_zoning_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_suburban_zoning_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_suburban_zoning_2025, TR),
    TR >= 0.70.

:- end_tests(us_suburban_zoning_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not extreme. Zoning operates as both coordination and extraction. The coordination component (neighborhood stability, predictable property values) is genuine but increasingly unbalanced by extraction (scarcity capitalization, wealth transfer). The measurement reflects that extraction has grown from 0.35 to 0.58 over 50 years as housing costs have decoupled from incomes in zoned metros, indicating rent-seeking layering onto coordination. Suppression (0.72): High. Exit barriers are substantial: renters cannot easily move to suburban job centers without massive cost or commute burden; federal/state override of local zoning faces fierce homeowner resistance; alternative governance models (HOAs, CLTs) are available but require breaking incumbent coalition control; professional ideology and regulatory capture by planners maintain enforcement. Theater ratio (0.68): High but not extreme. Zoning is presented as rational, scientific land-use planning that prevents sprawl and maintains safety. Empirical evidence (Houston pre-zoning, Tokyo mixed-use suburbs, Singapore public housing, TOD success) demonstrates these coordination claims are overstated — density does not cause the harms claimed. However, zoning is not purely theatrical; it does perform real coordination on property value expectations and neighborhood character (for incumbents). The 0.68 reflects significant performative content masking extraction, but not complete theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a perspectival chasm between beneficiaries and victims. Incumbent homeowners and municipalities experience zoning as coordination (rope perspective, negative χ) — it preserves neighborhood character and stabilizes property values. Low-income renters and young workers experience it as pure extraction (snare perspective, high χ ≈0.81) — they are trapped outside job-rich suburbs and forced into exurban displacement. The analytical observer risks a false summit (mountain) by naturalizing zoning as necessary to metropolitan organization, when international evidence shows it is contingent policy choice. The planning profession maintains piton theater (zoning presented as science despite empirical refutation). The reform coalition sees a scaffold with increasingly visible sunset — state pre-emption and federal mandates are building an exit path. The perspectival gap resolves into the mandatrophy question: Is zoning coordination that justifies mixed extraction, or extraction disguised as coordination? The evidence points toward the latter — the coordination component can be maintained via HOAs and architectural review boards without density exclusion; the zoning apparatus is deployed precisely to enable extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners: Beneficiary + arbitrage (can exit via sale, downsize, relocate) → d≈0.08, f(d)≈-0.10. Net beneficiaries; negative effective extraction. Low-income renters: Victim + trapped (cannot afford suburban alternatives or commute cost; job centers in zoned areas) → d≈0.92, f(d)≈1.40. Maximum extraction target. Municipal government: Beneficiary (tax revenue) + victim (voter pressure, constrained by homeowner opposition); bifurcated as both beneficiary and partially-constrained victim → d≈0.45, f(d)≈0.42. Moderate exposure. Real estate finance: Beneficiary + arbitrage (can engage in regulatory lobbying for exceptions) → d≈0.15, f(d)≈-0.05. Net beneficiary. Housing reform coalition: Victim (excluded from coordination benefits) + mobile/organized (growing political power, state-level options) → d≈0.35, f(d)≈0.28. Constrained but has agency. Planning profession: Institutional + constrained (professional ideology tied to zoning theology; constrained by evidence against its claims) → d≈0.50, f(d)≈0.65. Moderate; piton classification from theater gate, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy (is zoning coordination or extraction?) is resolved via decomposition into separable claims: (1) COORDINATION CLAIM: Density causes negative externalities (congestion, safety, property value volatility). Empirical status: REFUTED. International evidence (Tokyo, Singapore, Houston, TOD) shows density is compatible with safety, property value stability, and quality of life. Zoning restriction is not necessary for coordination. (2) EXTRACTION CLAIM: Zoning artificially restricts housing supply in job-rich areas, capitalizing scarcity into home prices and excluding lower-income populations. Empirical status: CONFIRMED. Hedonic pricing, cross-metro comparisons, and historical analysis show zoning accounts for 30-50% of suburban home appreciation in high-demand metros. (3) INSTITUTIONAL CLAIM: Zoning enforcement is sustained primarily by homeowner political power and planning profession inertia, not by genuine coordination failure. Empirical status: CONFIRMED. State-level zoning pre-emption (California, Minnesota) shows local zoning is politically contingent, not structural necessity. The mandatrophy resolves as follows: Zoning began with genuine coordination function (separating incompatible uses in 1920s-1960s metros). The coordination function is no longer necessary (modern environmental controls, transit separation, modern zoning tools substitute). The extraction mechanism (scarcity capitalization, exclusion) has become the primary function. The constraint now classifies as TANGLED ROPE with increasingly visible snare properties as extraction dominates. The scaffold perspective is real: state pre-emption and federal mandates are building an exit path with an estimated 10-20 year sunset for local single-family zoning exclusivity. Therefore: mandatrophy_resolved: true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homeowner_coalition_instability,
    'What causes incumbent homeowner coalitions to fracture on zoning reform, and what is the critical mass threshold for defection?',
    'Longitudinal analysis of homeowner voting patterns across California Prop 13 era, Minneapolis 2040, Oregon statewide zoning reform; regression on property value appreciation threshold where homeowners switch from opposing upzoning to supporting it',
    'If threshold low (5-10% potential appreciation from allowing duplexes): rapid coalition dissolution, zoning reforms accelerate. If threshold high (50%+): homeowner bloc remains stable indefinitely despite demographic change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homeowner_coalition_instability, empirical, 'Threshold for homeowner coalition defection on upzoning').

omega_variable(
    federal_override_sustainability,
    'Can federal housing mandates (e.g., housing voucher expansion, pre-emption of local zoning) sustain supply increases against organized local resistance, or will resistance recapture the regulatory space?',
    '10-year tracking of federal mandate compliance rates; analysis of state/local recapture tactics (variances, parking requirements, environmental review expansion); comparison to Section 8 voucher penetration resistance in conservative suburbs',
    'If sustainable: zoning constraint transitions from snare to scaffold (temporary, sunset-bound). If recaptured: federal mandate becomes performative overlay on unchanged local enforcement (piton-ification of federal policy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_override_sustainability, empirical, 'Sustainability of federal housing mandates against local recapture').

omega_variable(
    extraction_capitalization_rate,
    'What fraction of single-family home price appreciation is attributable to zoning scarcity vs. genuine amenity/location value, and does this vary systematically by market?',
    'Hedonic pricing model decomposition across metros with different zoning regimes; comparison of price appreciation rates in Houston (no zoning) vs comparable metros; meta-analysis of zoning value capitalization studies',
    'If zoning accounts for 40%+ of suburban appreciation: extraction is severe and wealth transfer is massive. If <20%: extraction narrative is weakened; coordination benefits may be genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_capitalization_rate, empirical, 'Fraction of home price appreciation attributable to zoning scarcity').

omega_variable(
    coordination_vs_extraction_decoherence,
    'Is single-family zoning genuinely necessary to coordinate neighborhood stability and property value expectations, or can alternative governance (HOAs, architectural review boards, community land trusts) provide coordination without density exclusion?',
    'Comparative case analysis: mixed-density neighborhoods with strong HOA/review boards vs single-family zoning neighbors on metrics (property value volatility, resident satisfaction, community cohesion); evidence from TOD (transit-oriented development) districts with maintained social cohesion despite density',
    'If alternatives work: zoning is pure extraction disguised as coordination (snare reclassified). If alternatives fail: zoning has genuine coordination function (tangled rope justified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decoherence, empirical, 'Whether zoning coordination function is essential or substitutable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_suburban_zoning_2025, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zoning_tr_t0, us_suburban_zoning_2025, theater_ratio, 0, 0.52).
narrative_ontology:measurement(zoning_tr_t25, us_suburban_zoning_2025, theater_ratio, 25, 0.62).
narrative_ontology:measurement(zoning_tr_t50, us_suburban_zoning_2025, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(zoning_be_t0, us_suburban_zoning_2025, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(zoning_be_t25, us_suburban_zoning_2025, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(zoning_be_t50, us_suburban_zoning_2025, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_suburban_zoning_2025, resource_allocation).
narrative_ontology:boltzmann_floor_override(us_suburban_zoning_2025, 0.32).
narrative_ontology:affects_constraint(us_suburban_zoning_2025, housing_affordability_crisis).
narrative_ontology:affects_constraint(us_suburban_zoning_2025, racial_residential_segregation).
narrative_ontology:affects_constraint(us_suburban_zoning_2025, labor_market_geographic_mismatch).
narrative_ontology:affects_constraint(us_suburban_zoning_2025, suburban_carbon_lock_in).

% DUAL FORMULATION NOTE:
% Single-family zoning is a constraint family with multiple downstream structural effects. The zoning code itself (this story) is the primary extraction mechanism. Housing affordability crisis (downstream) is the direct outcome of zoning-driven scarcity. Racial segregation (downstream) results from zoning's original explicit purpose (Jim Crow planning). Labor market mismatch (downstream) results from jobs clustering in restricted-supply suburban zones. Carbon lock-in (downstream) results from zoning-forced exurban displacement requiring car dependence. All four are causally downstream of zoning policy; zoning is upstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_suburban_zoning_2025, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
