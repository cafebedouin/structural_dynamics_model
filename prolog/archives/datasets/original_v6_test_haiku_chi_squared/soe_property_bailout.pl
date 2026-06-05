% ============================================================================
% CONSTRAINT STORY: soe_property_bailout
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soe_property_bailout, []).

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
 *   constraint_id: soe_property_bailout
 *   human_readable: State-Directed Purchase of Distressed Real Estate Assets
 *   domain: economic/real_estate/state_coordination
 *
 * SUMMARY:
 *   China's state-directed purchase of distressed real estate assets by SOEs
 *   represents a hybrid coordination-extraction constraint that emerged after
 *   2014 as property market volatility threatened financial stability. When
 *   private developers defaulted on projects, leaving retail homebuyers with
 *   unfinished properties and creditors with heavy losses, the central
 *   planning authority directed SOEs to acquire these distressed assets. This
 *   policy exhibits a genuine coordination function (preventing cascade
 *   failure in the financial system) combined with systematic extraction
 *   (transferring losses from private actors to public equity, creating
 *   implicit subsidy mechanisms). The constraint's theater_ratio has
 *   increased over time as the mechanism shifted from emergency intervention
 *   to permanent price-stabilization theater, with SOEs holding inventory at
 *   below-cost and absorbing ongoing losses. The extractiveness has escalated
 *   as the volume of distressed assets accumulated and the implicit fiscal
 *   burden on local governments grew. From the retail homebuyer's
 *   perspective, the constraint appears as a pure snare — they are trapped in
 *   properties with uncertain completion and no recourse. From the central
 *   planning authority's perspective, it is coordination — prevented system
 *   collapse. From the SOE perspective, it is tangled rope with growing
 *   extraction burden. The global financial observer sees it as hybrid:
 *   legitimate macro-prudential intervention that has evolved into
 *   zombie-asset hoarding.
 *
 * KEY AGENTS:
 *   - Central Planning Authority: Primary beneficiary (institutional/arbitrage) — achieves stability objective, directs asset absorption, maintains price stability
 *   - State-Owned Enterprises (Collective): Primary victim (organized/constrained) — forced to absorb distressed inventory, carry non-performing assets, subsidize market stabilization
 *   - Retail Homebuyers: Secondary victim (powerless/trapped) — trapped in incomplete properties, no leverage over developer or state response
 *   - Private Developer Creditors: Mixed (moderate/constrained) — benefit from partial recovery vs total writedown, but constrained by state intervention
 *   - Local Government Fiscal Authority: Secondary victim (organized/constrained) — must fund implicit SOE subsidies through transfers
 *   - International Financial Observers: Powerful analytical agents (powerful/mobile) — can exit the constraint by diversifying away from exposure, see extraction clearly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soe_property_bailout, 0.58).
domain_priors:suppression_score(soe_property_bailout, 0.72).
domain_priors:theater_ratio(soe_property_bailout, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soe_property_bailout, extractiveness, 0.58).
narrative_ontology:constraint_metric(soe_property_bailout, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(soe_property_bailout, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soe_property_bailout, tangled_rope).
narrative_ontology:human_readable(soe_property_bailout, "State-Directed Purchase of Distressed Real Estate Assets").
narrative_ontology:topic_domain(soe_property_bailout, "economic/real_estate/state_coordination").

domain_priors:requires_active_enforcement(soe_property_bailout).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soe_property_bailout, state_owned_enterprises).
narrative_ontology:constraint_beneficiary(soe_property_bailout, state_financial_stability).
narrative_ontology:constraint_beneficiary(soe_property_bailout, developer_creditors).
narrative_ontology:constraint_victim(soe_property_bailout, soe_balance_sheets).
narrative_ontology:constraint_victim(soe_property_bailout, private_investors).
narrative_ontology:constraint_victim(soe_property_bailout, retail_homebuyers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL HOMEBUYER (SNARE) — Trapped in a property purchase where developer default occurs mid-construction. No exit without catastrophic loss. No voice in SOE bailout decisions. Extraction is total: pays developer-set price for unfinished asset, subsidizes SOE inventory burden through foregone development completion. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(soe_property_bailout, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVATE DEVELOPER CREDITORS (TANGLED ROPE) — Benefit from SOE absorption of distressed assets (avoids full writedown), but extraction mechanism is opaque: SOE purchases at 60-75% of face value, creditors recover partial principal, but subordinated creditors absorb majority losses. Constrained exit — cannot force liquidation if state intervenes. Coordination function: prevents cascade failure. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL PLANNING AUTHORITY (ROPE) — Benefits from coordination mechanism: prevented financial system spillover by directing SOE purchases. Maintains property prices and employment in construction sector. Arbitrage exit: can adjust directive parameters, stop purchases if macro conditions shift. Effective extraction is minimized from the state's perspective — the constraint achieves its coordination objective. d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.03. Net beneficiary through coordination.
constraint_indexing:constraint_classification(soe_property_bailout, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE-OWNED ENTERPRISES COLLECTIVE (TANGLED ROPE) — Benefit from coordinated directive (spreading asset burden across multiple institutions prevents individual SOE collapse), but extraction is severe: absorb distressed inventory at below-market prices, carry non-performing assets on balance sheets, face pressure to hold or rent properties below cost to stabilize prices. Constrained exit — cannot refuse directive without triggering state intervention. Requires active enforcement: repeated directives, budget supplements, capital injections. d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROPERTY MARKET INSTITUTION (PITON) — The bailout mechanism has degraded into performative price-stabilization theater. SOE purchases nominally stabilize the market, but the constraint's primary function (preventing financial collapse) has atrophied — replaced by zombie-asset holding and implicit subsidies. Theater ratio = 0.65: SOEs hold properties below-cost, absorbing losses on behalf of the state. The ritual persists through institutional inertia (career risk of admitting the real estate sector cannot self-stabilize) despite functional degradation.
constraint_indexing:constraint_classification(soe_property_bailout, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GLOBAL FINANCE (TANGLED ROPE) — From the analytical view, the state-directed purchase is a hybrid: it solves a genuine coordination problem (preventing financial cascade) while also enabling extraction (transferring developer losses to SOE public equity, implicit taxation of domestic savers through inflation/debasement). Mobile exit: international investors can rotate away from assets in the stabilization zone. See the constraint as structurally extractive despite its coordination rationale. d≈0.58, f(d)≈0.88, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soe_property_bailout_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soe_property_bailout, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soe_property_bailout, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soe_property_bailout, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soe_property_bailout, TR),
    TR >= 0.70.

:- end_tests(soe_property_bailout_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint does achieve genuine coordination (prevents financial cascade), but this is paired with significant extraction. The central authority captures macro stability benefit while SOEs and domestic savers absorb costs. Suppression (0.72): High. Multiple barriers prevent exit: SOEs cannot refuse state directive, homebuyers cannot walk away from sunken costs, local governments cannot reject fiscal transfer requirements. Private creditors face constrained liquidation options. The suppression is partly structural (asset illiquidity) and partly institutional (directive enforcement). Theater ratio (0.65): Moderate-high and rising. The bailout began as emergency intervention with clear objectives (stop cascade failure), but has evolved into performative price stabilization. SOEs hold properties below cost, absorbing losses to maintain headline price indices. The ritual persists because admitting real estate sector cannot self-stabilize carries political costs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap between the state authority (sees coordination/rope), the SOEs (see extraction/tangled_rope), the homebuyers (see pure extraction/snare), and the organized creditors (see mixed coordination and subordination/tangled_rope). The gap reflects fundamental disagreement about whether the constraint solves a real problem or merely transfers costs forward. The central authority's perspective naturalizes the bailout as necessary macro-prudential intervention. The SOEs' perspective emphasizes the cumulative burden of inventory and implicit subsidy. The homebuyers' perspective sees irreversible loss of control and no voice in the mechanism. The global observer's perspective sees a hybrid that was genuinely coordinating at t=0 but has degraded into zombie-asset holding by t=6. The theater ratio increase (0.40 → 0.65) documents this perspectival shift over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail homebuyers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction from their perspective. State authority: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary; achieved stability objective. SOEs (collective): Victim + constrained → d≈0.70, f(d)≈1.05. Significant extraction but not total; coordination benefit is real but marginal compared to burden. Developer creditors: Mixed victim + constrained → d≈0.65, f(d)≈0.95. Partial recovery vs total loss, but constrained by state intervention. Local government: Victim + constrained → d≈0.75, f(d)≈1.10. Must absorb fiscal transfers without exit. Global observer: Analytical + mobile → d≈0.58, f(d)≈0.88. Can see both coordination and extraction clearly; can exit exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED: The classification hinges on an irreducible empirical question: does the constraint actually prevent financial cascade, or does it merely postpone and enlarge it? If the bailout genuinely stabilizes the system and homebuyers eventually complete their properties (coordination function dominates), the constraint might reclassify toward rope. If SOEs accumulate massive unpayable losses and the mechanism requires escalating state subsidies indefinitely (extraction function dominates), the constraint reclassifies toward snare. The measurement trajectory (extractiveness rising from 0.35 to 0.58, theater rising from 0.40 to 0.65) suggests the constraint is drifting toward pure extraction — the coordination objective is achieved, but the institutional mechanism persists and worsens, meeting the definition of piton degradation. Mandatrophy is NOT yet resolved because the dataset (6 years) is insufficient to determine whether this is temporary stability-building (valid tangled_rope with sunset) or permanent zombie-asset hoarding (extractive snare with theater). Resolution requires 10+ year horizon and clear identification of exit conditions (when do SOEs liquidate inventory? when do homebuyers receive properties? when do implicit subsidies end?).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_boundary_threshold,
    'What fraction of residential inventory can SOEs absorb before the constraint shifts from coordination (preventing cascade) to pure extraction (zombie-asset hoarding)?',
    'Empirical tracking of cumulative SOE property holdings as percentage of local inventory; correlation with market dysfunction indicators (rental vacancy, price anchoring, credit misallocation)',
    'If threshold <5%: constraint remains coordination-focused. If threshold >15%: constraint becomes primarily extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_boundary_threshold, empirical, 'Inventory absorption threshold for shifting from coordination to extraction').

omega_variable(
    subsidy_incidence_opacity,
    'Who ultimately bears the financial loss from below-cost SOE property holdings: domestic savers through inflation, taxpayers through budget supplements, future generations through debt, or international creditors through currency depreciation?',
    'Flow-of-funds analysis: track implicit transfer mechanisms (central bank monetization, fiscal transfers to SOEs, currency movements). Identify which household/investor class absorbs the real cost.',
    'If cost borne by domestic savers invisibly: extraction is successful and hidden. If cost borne by taxpayers visibly: constraint becomes politically unsustainable. If distributed across generations: long-term fiscal drag emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_incidence_opacity, empirical, 'Identification of who bears the true financial burden of the bailout').

omega_variable(
    moral_hazard_incentive_creation,
    'Does the SOE bailout create moral hazard for future private developers, encouraging higher leverage and riskier construction projects under the expectation of state rescue?',
    'Comparison of developer leverage ratios, project risk profiles, and default rates before vs after first major bailout; survey of developer expectations about future state intervention',
    'If moral hazard is severe: constraint will require repeated intervention (escalating suppression). If moral hazard is absent: constraint may achieve temporary stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_incentive_creation, empirical, 'Whether the bailout creates expectations of future rescue that increase risk-taking').

omega_variable(
    local_government_fiscal_capacity,
    'Can local governments sustain the implicit subsidy of SOE property holdings without triggering fiscal crisis or cutting essential services?',
    'Fiscal stress test: model local government revenues, debt service, SOE transfer requirements over 5-10 year horizon; identify sustainability breakpoint.',
    'If sustainable: constraint remains stable. If unsustainable within <10 years: fiscal crisis will force constraint recalibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_government_fiscal_capacity, empirical, 'Sustainability of local government fiscal transfers to support the bailout').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soe_property_bailout, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soe_prop_tr_t0, soe_property_bailout, theater_ratio, 0, 0.4).
narrative_ontology:measurement(soe_prop_tr_t3, soe_property_bailout, theater_ratio, 3, 0.55).
narrative_ontology:measurement(soe_prop_tr_t6, soe_property_bailout, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(soe_prop_be_t0, soe_property_bailout, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soe_prop_be_t3, soe_property_bailout, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(soe_prop_be_t6, soe_property_bailout, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soe_property_bailout, resource_allocation).
narrative_ontology:affects_constraint(soe_property_bailout, developer_leverage_moral_hazard).
narrative_ontology:affects_constraint(soe_property_bailout, residential_price_stickiness).
narrative_ontology:affects_constraint(soe_property_bailout, local_government_fiscal_sustainability).

% DUAL FORMULATION NOTE:
% The state-directed bailout can be decomposed into two distinct constraints: (1) the genuine macro-prudential stabilization mechanism (ε≈0.25, mountain of financial necessity), and (2) the institutional persistence of SOE asset-holding as a price-stabilization ritual (ε≈0.70, extractive piton with theater). This story focuses on the aggregate view (ε=0.58). The upstream constraint (developer defaults triggering cascade risk) has different ε and may classify as mountain. The downstream consequence (zombie-asset hoarding creating fiscal drag) classifies as piton.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(soe_property_bailout, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
