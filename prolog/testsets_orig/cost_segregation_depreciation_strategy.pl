% ============================================================================
% CONSTRAINT STORY: cost_segregation_depreciation_strategy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cost_segregation_depreciation_strategy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cost_segregation_depreciation_strategy
 *   human_readable: Cost Segregation Depreciation Strategy in Real Estate Investment
 *   domain: tax_policy/real_estate_finance
 *
 * SUMMARY:
 *   Cost segregation depreciation strategy represents a hybrid
 *   coordination-extraction constraint within real estate taxation. The
 *   constraint simultaneously coordinates legitimate economic depreciation
 *   (different asset classes have different useful lives and should be
 *   depreciated accordingly) while enabling asymmetric extraction through
 *   scale-dependent access barriers. Large institutional investors benefit
 *   from accelerated depreciation timing while small property owners lack
 *   capital to access specialized consulting required to deploy the strategy.
 *   The federal tax system experiences this as both coordination (more
 *   accurate asset classification) and extraction (accelerated revenue loss).
 *   The constraint's theater ratio reflects that cost segregation studies
 *   produce detailed technical allocations that appear rigorous and
 *   defensible while incorporating subjective assumptions that drive tax
 *   optimization outcomes. Over the 20-year interval, both extractiveness and
 *   theater ratio have increased as the strategy has matured from niche tax
 *   optimization technique to standardized practice in large real estate
 *   deals, with consulting methodologies becoming more sophisticated and cost
 *   segregation becoming expected practice for institutional investors.
 *
 * KEY AGENTS:
 *   - Large Real Estate Funds: Primary beneficiary (institutional/arbitrage) — deploy cost segregation across portfolios with economies of scale; negotiate bulk consulting rates; capture accelerated depreciation benefits
 *   - Cost Segregation Consulting Firms: Primary beneficiary (institutional/arbitrage) — earn consulting fees; coordinate legitimate asset allocation; benefit from information asymmetry and technical expertise barriers
 *   - Federal Tax Authority (IRS): Mixed victim/beneficiary (institutional/constrained) — loses revenue through accelerated depreciation but benefits from more accurate asset classification; faces capacity constraints in enforcement
 *   - Small Property Owners: Primary victim (powerless/trapped) — cannot afford consulting costs; stuck in standard depreciation; face competitive disadvantage against larger investors; cannot exit the disadvantage
 *   - Mid-Sized Real Estate Companies: Secondary victim (moderate/constrained) — can theoretically access strategy but face high capital barriers; high consulting costs reduce strategy ROI; constrained by capital limitations
 *   - Rental Tenants: Secondary victim (powerless/trapped) — indirectly affected through higher property prices and rents driven by investor demand for large assets with cost segregation potential; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cost_segregation_depreciation_strategy, 0.58).
domain_priors:suppression_score(cost_segregation_depreciation_strategy, 0.65).
domain_priors:theater_ratio(cost_segregation_depreciation_strategy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cost_segregation_depreciation_strategy, extractiveness, 0.58).
narrative_ontology:constraint_metric(cost_segregation_depreciation_strategy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cost_segregation_depreciation_strategy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cost_segregation_depreciation_strategy, tangled_rope).
narrative_ontology:human_readable(cost_segregation_depreciation_strategy, "Cost Segregation Depreciation Strategy in Real Estate Investment").
narrative_ontology:topic_domain(cost_segregation_depreciation_strategy, "tax_policy/real_estate_finance").

domain_priors:requires_active_enforcement(cost_segregation_depreciation_strategy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cost_segregation_depreciation_strategy, large_real_estate_investors).
narrative_ontology:constraint_beneficiary(cost_segregation_depreciation_strategy, cost_segregation_consulting_firms).
narrative_ontology:constraint_beneficiary(cost_segregation_depreciation_strategy, property_developers).
narrative_ontology:constraint_victim(cost_segregation_depreciation_strategy, federal_tax_revenue).
narrative_ontology:constraint_victim(cost_segregation_depreciation_strategy, small_property_owners).
narrative_ontology:constraint_victim(cost_segregation_depreciation_strategy, rental_tenants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL PROPERTY OWNER (SNARE) — Cannot access cost segregation benefits due to lack of capital to hire specialized consultants ($15,000-$50,000 per engagement). Trapped in standard depreciation schedules. Extraction occurs through tax code asymmetry: large investors use accelerated depreciation while small owners cannot, creating competitive disadvantage in property acquisition and financing. No meaningful exit option.
constraint_indexing:constraint_classification(cost_segregation_depreciation_strategy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-SIZED REAL ESTATE COMPANY (TANGLED ROPE) — Faces high barrier to cost segregation access ($30,000-$50,000 consulting fees reduces ROI significantly). Can access strategy but with substantial cost burden. Constrained by capital requirements and need for specialized expertise. Experiences both coordination benefit (ability to optimize tax position through better asset classification) and extraction (must share substantial fees with consultants; loses competitive advantage to larger firms that negotiate bulk consulting rates). Mixed position.
constraint_indexing:constraint_classification(cost_segregation_depreciation_strategy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE REAL ESTATE FUND (ROPE) — Institutional actor with scale enabling direct deployment of cost segregation across portfolio. Negotiates consulting fees at bulk rates (10-30% of typical per-property costs). Benefits from coordination function: cost segregation organizes capital recovery timing to match actual asset economic lives. Primary beneficiary experiencing constraint as enabling mechanism rather than extractive. Arbitrage optionality across multiple properties and jurisdictions.
constraint_indexing:constraint_classification(cost_segregation_depreciation_strategy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COST SEGREGATION CONSULTING FIRM (ROPE) — Benefits from structural asymmetry in information and technical expertise. Coordinates legitimate asset classification and accelerated depreciation timing. Primary income stream flows from extracting consulting fees, but the service provides real value: accurate asset classification, defensible IRS positions, optimized depreciation schedules. Arbitrage optionality across industries (real estate, manufacturing, hospitality). Pure beneficiary with low suppression experienced by this agent.
constraint_indexing:constraint_classification(cost_segregation_depreciation_strategy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL TAX AUTHORITY (TANGLED ROPE) — Faces genuine coordination problem: assets DO have different useful lives (HVAC systems 10-15 years, structural components 40+ years), and depreciation schedules should reflect economic reality. Cost segregation solves this coordination problem through more accurate asset classification. However, the IRS simultaneously experiences extraction: intentional acceleration of depreciation timing shifts revenue forward, and the gap between intended useful lives and aggressive cost segregation estimates reduces effective tax collection. The constraint requires active enforcement (IRS audits, regulations) to maintain. Institutional beneficiary that also bears costs through lost revenue.
constraint_indexing:constraint_classification(cost_segregation_depreciation_strategy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGISLATIVE FRAMEWORK (PITON) — The MACRS depreciation system (Modified Accelerated Cost Recovery System) persists as institutional framework despite substantial evolution of deployment methods. Legislative intent was cost recovery efficiency; modern practice emphasizes tax optimization through aggressive segregation. The framework maintains itself through regulatory complexity and lobbying pressure from beneficiary industries. Theater ratio reflects performative regulatory compliance (cost segregation studies look rigorous but incorporate subjective allocation assumptions). Sunset unlikely — framework maintained through inertia and institutional lock-in.
constraint_indexing:constraint_classification(cost_segregation_depreciation_strategy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cost_segregation_depreciation_strategy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cost_segregation_depreciation_strategy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cost_segregation_depreciation_strategy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cost_segregation_depreciation_strategy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cost_segregation_depreciation_strategy, TR),
    TR >= 0.70.

:- end_tests(cost_segregation_depreciation_strategy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The strategy produces measurable tax savings (15-30% acceleration of depreciation schedules over 5-7 year window) that flow disproportionately to large-scale investors. The extraction is legitimate economic recovery optimization at the margin but operates through systematic capital scale exclusion. Over the 20-year interval, extractiveness has increased from 0.35 to 0.58 as consulting practices have matured and deployment has become expected for institutional investors, suggesting the strategy has shifted from marginal optimization to core business practice. Suppression (0.65): Moderate-high. Barriers to independent deployment include technical complexity (requires specialized engineering and accounting expertise), regulatory risk (IRS challenge potential), and capital requirements for consulting. However, suppression is not total — information about the strategy is widely available, and large investors routinely deploy it. The suppression primarily excludes small property owners through capital barriers rather than legal prohibition. Theater ratio (0.58): Moderate. Cost segregation studies appear technically rigorous with detailed asset-by-asset cost allocation, engineering reports, and useful-life justifications. The theater derives from subjective allocation assumptions (what percentage of building cost is HVAC vs. structural? 25% vs. 35% produces significantly different tax outcomes) that can be defended as reasonable but are not uniquely determined. Over 20 years, theater ratio has increased as methodologies have become more sophisticated and studies have evolved to present more detailed technical justification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap. Small property owners see pure extraction (Snare) — they are systematically disadvantaged with no accessible exit. Mid-sized companies see mixed coordination-extraction (Tangled Rope) — they can theoretically access benefits but face capital barriers that reduce net benefit. Large institutional investors see coordination (Rope) — the strategy solves their legitimate problem of accurate asset classification and accelerated capital recovery. Cost segregation firms see coordination (Rope) — they provide valuable technical service that deserves compensation. The IRS sees mixed coordination-extraction (Tangled Rope) — legitimate asset classification benefit offset by revenue loss and audit capacity constraints. The legislative framework sees its own degraded ritual (Piton) — the MACRS system persists despite significant evolution in deployment sophistication, maintained through regulatory complexity and lobbying. The gap between the powerless small owner's experience (Snare) and the institutional beneficiary's experience (Rope) reveals the constraint's true structure: it is extraction that uses coordination as its mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation flows from beneficiary/victim status combined with exit options. Large investors classified as institutional/arbitrage have low d values (~0.10-0.20) because they benefit from the constraint and have exit options (they can deploy other tax strategies if cost segregation becomes unavailable). Small property owners classified as powerless/trapped have high d values (~0.90-1.00) because they bear costs and have no exit. Mid-sized companies classified as moderate/constrained have intermediate d values (~0.60-0.70) because they experience mixed effects and face high-cost exit (abandoning property investments). The federal tax authority classified as institutional/constrained has intermediate d values (~0.55-0.65) because it experiences both revenue loss and coordination benefit, with constrained rather than arbitrage exit options (cannot simply abandon tax administration). Cost segregation consulting firms classified as institutional/arbitrage have low d values (~0.15-0.25) because they are primary beneficiaries with exit optionality. The presheaf reveals that what appears symmetric at the policy level (MACRS is available to all taxpayers) operates asymmetrically in practice (only capital-intensive actors can afford to deploy advanced depreciation strategies).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that cost segregation is genuinely BOTH coordination AND extraction. It coordinates the legitimate economic fact that different asset classes have different useful lives — an HVAC system with 10-year life should not be depreciated over 40 years like the building structure. However, it simultaneously extracts through scale-dependent access barriers that create systematic disadvantage for small-capital actors. The coordination function is real (IRS audits confirm that better asset classification produces more economically accurate depreciation). The extraction function is also real (only large-scale investors can afford the consulting to deploy it optimally). The mandatrophy resolution: this is a Tangled Rope from the analytical perspective — it REQUIRES both coordination benefit and asymmetric extraction to classify as Tangled Rope, and both are present. The false summit is any perspective that claims cost segregation is 'just coordinate asset classification' (beneficiary view) or 'just extraction' (small owner view). The structural reality is the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allocation_methodology_objectivity,
    'Are cost segregation allocation methodologies based on objective economic recovery periods or on tax optimization conventions that diverge from actual asset useful lives?',
    'Comparative analysis: allocated useful lives in cost segregation studies vs. actual observed asset replacement cycles in comparable properties. IRS audit patterns and adjustment rates across similar property types.',
    'If methodology is objective: constraint is pure coordination (Rope). If methodology is tax-optimization convention: constraint is extractive (Snare/Tangled Rope). The distinction determines whether cost segregation represents legitimate economic accuracy or intentional depreciation acceleration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_methodology_objectivity, empirical, 'Whether cost segregation allocation follows economic facts or tax optimization conventions').

omega_variable(
    alternative_verification_feasibility,
    'Could regulatory authorities cost-effectively audit cost segregation claims at scale, or is the strategy effective precisely because verification costs exceed typical exposure?',
    'Analysis of IRS audit capacity and frequency for cost segregation claims; cost-benefit model of audit risk for taxpayers; historical adjustment rates. Comparison to verification capacity in other complex tax areas.',
    'If verification is feasible: suppression is high but surmountable (taxpayers could face meaningful audit risk). If verification is cost-prohibitive: suppression is structural — strategy persists because IRS cannot realistically enforce disallowance. Affects classification from tax authority perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_verification_feasibility, empirical, 'Whether regulatory verification of cost segregation claims is feasible at scale').

omega_variable(
    capital_scale_threshold_discontinuity,
    'At what property acquisition size does cost segregation consulting become economically accessible (breakeven consulting fees against tax savings)?',
    'Analysis of typical consulting costs ($15K-$50K) against tax savings across property values ($500K-$500M+). Identify threshold where ROI becomes positive. Historical data on property sizes accessing cost segregation.',
    'If threshold is low (<$5M): access is relatively democratized (Rope perspective more universal). If threshold is high (>$50M): access gap is severe (Snare/Tangled Rope for most property owners). Identifies whether the constraint is scale-proportional or scale-exclusive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_scale_threshold_discontinuity, empirical, 'Capital scale threshold for economically viable cost segregation deployment').

omega_variable(
    regulatory_enforcement_trajectory,
    'Is the IRS increasing audit frequency and adjustment rates for cost segregation claims, or declining (budget/capacity constraints)?',
    'IRS audit statistics by tax form and strategy (Form 3115 audit rates, adjustment rates on cost segregation audits). Trend analysis over 10-year window. Budget and FTE allocation to complex real estate examinations.',
    'If enforcement is increasing: suppression may decline and strategy may face tighter constraints (toward Scaffold with sunset). If enforcement is declining: suppression remains high and strategy effectiveness persists (toward pure Snare for tax revenue). Indicates future classification trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_trajectory, empirical, 'Trajectory of IRS enforcement of cost segregation strategies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cost_segregation_depreciation_strategy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(costseg_tr_t0, cost_segregation_depreciation_strategy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(costseg_tr_t10, cost_segregation_depreciation_strategy, theater_ratio, 10, 0.5).
narrative_ontology:measurement(costseg_tr_t20, cost_segregation_depreciation_strategy, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(costseg_be_t0, cost_segregation_depreciation_strategy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(costseg_be_t10, cost_segregation_depreciation_strategy, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(costseg_be_t20, cost_segregation_depreciation_strategy, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cost_segregation_depreciation_strategy, resource_allocation).
narrative_ontology:affects_constraint(cost_segregation_depreciation_strategy, real_estate_capital_formation_asymmetry).
narrative_ontology:affects_constraint(cost_segregation_depreciation_strategy, tax_code_complexity_barrier).
narrative_ontology:affects_constraint(cost_segregation_depreciation_strategy, institutional_investor_market_dominance).

% DUAL FORMULATION NOTE:
% Cost segregation depreciation strategy is downstream of capital formation asymmetries in real estate (large institutional pools vs. individual capital) and upstream of market concentration effects (large investors acquire properties at scale, creating competitive disadvantage for small owners). The strategy's extractiveness amplifies capital-scale asymmetries that already exist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
