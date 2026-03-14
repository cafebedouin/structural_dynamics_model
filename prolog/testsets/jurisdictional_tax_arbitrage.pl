% ============================================================================
% CONSTRAINT STORY: jurisdictional_tax_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisdictional_tax_arbitrage, []).

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
 *   constraint_id: jurisdictional_tax_arbitrage
 *   human_readable: Jurisdictional Tax Arbitrage
 *   domain: economic/tax_policy
 *
 * SUMMARY:
 *   Jurisdictional tax arbitrage is a structural constraint that arises from
 *   the interaction of multiple sovereign tax systems with differential
 *   rates, combined with multinational corporate structures that can
 *   partition income, shift profits through transfer pricing, and route
 *   capital through low-tax jurisdictions. The constraint extracts
 *   significant value from national tax bases, disproportionately burdening
 *   wage earners and domestic businesses while benefiting multinational
 *   corporations and tax havens. The extractiveness has increased over the
 *   measurement interval (0.42 → 0.68) as complexity of structures has
 *   increased and enforcement capacity has lagged. Theater ratio has also
 *   increased (0.30 → 0.55) as regulatory responses (BEPS, minimum tax
 *   agreements, beneficial ownership disclosure) have added compliance burden
 *   without fundamentally altering the arbitrage logic. The constraint
 *   exhibits all six classification types from different observer positions,
 *   making it a diagnostic exemplar of how indexical classification maps
 *   structural relationships.
 *
 * KEY AGENTS:
 *   - Wage Earners: Powerless/trapped (national) — bear extraction through higher effective rates relative to capital gains; no exit options
 *   - Domestic Small Businesses: Powerless/trapped (national) — lack capital to structure transfers; trapped in single jurisdiction
 *   - National Tax Authorities: Organized/constrained (national) — must coordinate collection while managing exit threats; experience mixed coordination and extraction
 *   - Multinational Corporations: Institutional/arbitrage (global) — benefit from arbitrage; high exit options enable rent extraction
 *   - Tax Haven Jurisdictions: Institutional/arbitrage (global) — benefit from capital inflow; coordinate through competitive positioning
 *   - OECD/BEPS Framework: Institutional/constrained (global) — maintains appearance of constraint through regulatory performance
 *   - Analytical Observer: Analytical/analytical (universal) — risks naturalizing contingent institutional arrangements as economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisdictional_tax_arbitrage, 0.68).
domain_priors:suppression_score(jurisdictional_tax_arbitrage, 0.72).
domain_priors:theater_ratio(jurisdictional_tax_arbitrage, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisdictional_tax_arbitrage, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisdictional_tax_arbitrage, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisdictional_tax_arbitrage, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisdictional_tax_arbitrage, snare).
narrative_ontology:human_readable(jurisdictional_tax_arbitrage, "Jurisdictional Tax Arbitrage").
narrative_ontology:topic_domain(jurisdictional_tax_arbitrage, "economic/tax_policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisdictional_tax_arbitrage, multinational_corporations).
narrative_ontology:constraint_beneficiary(jurisdictional_tax_arbitrage, tax_havens).
narrative_ontology:constraint_victim(jurisdictional_tax_arbitrage, national_tax_bases).
narrative_ontology:constraint_victim(jurisdictional_tax_arbitrage, middle_income_workers).
narrative_ontology:constraint_victim(jurisdictional_tax_arbitrage, domestic_small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Individual with income taxed at source has no exit. Cannot relocate capital, cannot restructure income through subsidiaries, cannot exploit transfer pricing. Suppression is total: withholding requirements, progressive taxation, no arbitrage options. Bears full extraction cost with zero agency.
constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC SMALL BUSINESS (SNARE) — Local competitor with operations in single jurisdiction. Cannot split revenue across low-tax havens, cannot use debt financing from tax-deductible subsidiaries abroad, cannot engage in transfer pricing. Trapped by geography and capital constraints. Faces extraction through both direct taxation and effective market disadvantage against multinationals with 15-20% global tax rates.
constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: TAX AUTHORITY (TANGLED ROPE) — Must coordinate collection from millions of taxable agents while managing competitive pressure from other jurisdictions and corporate exit threats. Genuine coordination function (sustaining public revenue) alongside asymmetric extraction: taxation itself is both coordination mechanism and coercive extraction. Constrained by exit threats (corporations relocate, capital flows, talent migration) but retains enforcement power.
constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MULTINATIONAL CORPORATION (ROPE) — Experiences jurisdictional tax arbitrage as pure coordination mechanism for managing global capital. Entities can restructure operations, shift profits, route capital flows, exploit treaty gaps. High exit option: can relocate headquarters, redomicile intellectual property, establish subsidiary networks. Constraint is coordination problem it solves elegantly: how to minimize global tax liability given multiple jurisdictions.
constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TAX HAVEN JURISDICTION (ROPE) — Low-tax or zero-tax regime benefits from multinational capital inflow and uses jurisdictional arbitrage as pure coordination mechanism. Benefits from constraint through tax revenue, financial services development, wealth management fees. No suppression experienced — freely chooses competitive position in global tax landscape. Can exit by raising rates (rare; happens only under external pressure).
constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: BEPS/OECD COMPLIANCE THEATER (PITON) — International reform efforts (Base Erosion Profit Shifting initiative, minimum tax agreements, country-by-country reporting) create appearance of constraint without structural change. Theater ratio is high (0.55) because compliance mechanisms are performative: transfer pricing documentation, profit attribution formulas, minimum tax floors create administrative burden but do not eliminate core arbitrage logic. Degraded institution maintained through consensus rather than function.
constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Observer may perceive jurisdictional tax arbitrage as an unchangeable feature of capitalist economics: rational actors minimize tax, multiple sovereigns cannot coordinate taxation, structural geometry of the problem is immutable. This perspective naturalizes institutional arrangements as laws of economics. Engine identifies as false summit: the constraint is highly contingent on corporate legal structures, treaty design, information asymmetries, and enforcement capacity.
constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisdictional_tax_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jurisdictional_tax_arbitrage, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisdictional_tax_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jurisdictional_tax_arbitrage, TR),
    TR >= 0.70.

:- end_tests(jurisdictional_tax_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting substantial value extracted from high-tax jurisdictions to low-tax structures. Measurement trajectory (0.42 → 0.68) shows acceleration as intellectual property concentration, digital economy growth, and transfer pricing sophistication have created larger profit-shifting opportunities. Suppression (0.72): High. Multiple barriers prevent trapped agents from escaping: wage withholding is enforced at source; relocation costs are prohibitive; legal structures enabling arbitrage are unavailable to individuals and small businesses; treaty networks create lock-in effects. Theater ratio (0.55): Moderate-high and increasing. BEPS, Country-by-Country Reporting, and global minimum tax create administrative compliance (documentation, reporting, audit trails) that are performance requirements rather than fundamental changes. The underlying arbitrage logic persists because it is not an enforcement problem but a structural problem: multiple sovereigns with different rates create an economically rational incentive to shift profits. Compliance is performative relative to the constraint's function. Claimed type (Snare): Justified by high extractiveness, high suppression, and asymmetric victim/beneficiary structure. The constraint exists primarily to enable extraction, not to solve a coordination problem.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap appears between wage earners (Snare) and multinational corporations (Rope). The wage earner experiences pure extraction with no coordination benefit: their income is taxed at source with no agency over structure. The multinational corporation experiences pure coordination: they are solving the legitimate problem of minimizing global tax obligations given multiple sovereigns. Both are observing the same constraint, but from fundamentally different structural positions. The tax authority's tangled_rope classification reflects their mixed position: they must coordinate collection (genuine public good) while experiencing extraction pressure from mobile capital. The BEPS framework's piton classification reflects that regulatory responses add performative burden without changing the underlying incentive structure. The false summit at the analytical level reflects risk of naturalizing what is actually a contingent institutional arrangement (multiple uncoordinated sovereigns) as an unchangeable fact of economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality and resultant effective extractiveness is derived from structural position. Wage earners as trapped powerless agents experience maximum d (approaching 1.0), yielding high f(d) and maximum χ. Multinationals as institutional arbitrage-exit agents experience low d (approaching 0.0 or negative), yielding low or negative f(d), making the constraint appear as rope (coordination) rather than extraction. Tax authorities as constrained organized agents experience intermediate d (≈0.55), balancing genuine coordination function against extraction pressure. The scope modifier σ(S) applies at national scope for trapped agents (σ=1.0), amplifying experienced extractiveness, and at global scope for multinationals (σ=1.2), but the direction of impact is opposite: for victims at national scope, global-scale extractive flows amplify local suppression; for beneficiaries at global scope, scope amplification increases arbitrage opportunity. The piton classification derives from theater_ratio exceeding 0.70 threshold expectations — regulatory theater is present but at moderate level (0.55), suggesting degraded rather than purely performative institution.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy principle by showing how structural position determines classification type independent of underlying mechanism. The same tax code, same rate differentials, same corporate structures are simultaneously (1) a pure extraction mechanism from the wage earner's perspective (Snare), (2) a coordination mechanism from the multinational's perspective (Rope), (3) a degraded regulatory regime from the OECD perspective (Piton), and (4) either a natural economic law or a contingent institutional arrangement depending on analytical framing. The mandatrophy is resolved by recognizing that each classification is correct for its structural position. The wage earner's snare classification is not false — it correctly captures their structural reality. The multinational's rope classification is not false — it correctly captures their structural reality. The constraint does not 'really' belong to one type; rather, the presheaf of classifications over observer positions fully specifies the constraint. The false summit detection applies to the analytical view that naturalizes as 'economic law' what is actually a consequence of specific institutional choices (multiple uncoordinated sovereigns, permission of corporate legal partitioning, treaty gaps).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corporate_exit_elasticity,
    'How responsive are multinational capital flows to changes in effective tax rates? Is the perceived exit threat from tax authorities real or overstated?',
    'Empirical analysis of investment location decisions post-tax-change; comparison of declared effective tax rates to realized economic incidence; estimation of true behavioral elasticity of corporate income to tax rates',
    'If elasticity is high (>1.5): tax competition is genuinely fierce and authorities are trapped. If low (<0.5): exit threat is overstated and authorities have more agency than they perceive. Affects classification of tax authority from tangled_rope toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_exit_elasticity, empirical, 'Corporate exit elasticity to tax rate changes').

omega_variable(
    transfer_pricing_enforcement_capacity,
    'Can national tax authorities effectively audit and challenge transfer pricing arrangements, or is enforcement inherently limited by information asymmetry and technical complexity?',
    'Audit success rates for transfer pricing disputes; comparison of challenged valuations to accepted/litigated outcomes; investigation of revenue recovery rates; analysis of IRS, HMRC, and similar agency capacity metrics',
    'If capacity is high: suppression is less total than measured (authorities can recover significant revenue). If capacity is low: suppression is higher than measured (authorities face systematic defeat). Affects measured suppression value and extraction calculation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transfer_pricing_enforcement_capacity, empirical, 'Tax authority enforcement capacity for transfer pricing').

omega_variable(
    profit_shifting_deadweight_loss,
    'What fraction of observed tax arbitrage behavior represents genuinely economically inefficient profit shifting (deadweight loss) versus legitimate tax planning that reflects real economic decisions?',
    'Counterfactual analysis: comparison of investment location choices under arbitrage to choices under harmonized taxation; study of parent company/subsidiary location decisions controlling for non-tax factors; analysis of profitability differentials by jurisdiction',
    'If high deadweight loss: the constraint imposes real efficiency costs on non-beneficiary agents beyond extraction measured. If low: much of the ''extraction'' is real allocation signal reflecting rational economic choice. Affects interpretation of whether classification should be snare (pure waste) or tangled_rope (extractive but functional).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(profit_shifting_deadweight_loss, empirical, 'Deadweight loss from economic profit shifting').

omega_variable(
    beneficial_ownership_disclosure_impact,
    'Do beneficial ownership registries and country-by-country reporting mandates actually reduce arbitrage capacity, or are they primarily performative compliance requirements?',
    'Measurement of tax arbitrage before/after regulatory implementation; analysis of continued use of shell companies post-disclosure; examination of OECD Common Reporting Standard impact on capital flows',
    'If disclosure is effective: regulatory regime is moving constraint toward rope/scaffold (coordination problem being solved). If performative: theater_ratio should be higher and constraint remains snare. Informs piton classification accuracy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficial_ownership_disclosure_impact, empirical, 'Effectiveness of beneficial ownership disclosure requirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisdictional_tax_arbitrage, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jta_tr_t0, jurisdictional_tax_arbitrage, theater_ratio, 0, 0.3).
narrative_ontology:measurement(jta_tr_t10, jurisdictional_tax_arbitrage, theater_ratio, 10, 0.45).
narrative_ontology:measurement(jta_tr_t20, jurisdictional_tax_arbitrage, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(jta_be_t0, jurisdictional_tax_arbitrage, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jta_be_t10, jurisdictional_tax_arbitrage, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(jta_be_t20, jurisdictional_tax_arbitrage, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisdictional_tax_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(jurisdictional_tax_arbitrage, capital_flight).
narrative_ontology:affects_constraint(jurisdictional_tax_arbitrage, intellectual_property_localization).
narrative_ontology:affects_constraint(jurisdictional_tax_arbitrage, regulatory_capture_tax_policy).

% DUAL FORMULATION NOTE:
% Jurisdictional tax arbitrage decomposes into multiple structurally distinct constraints: (1) transfer pricing avoidance (ε≈0.65, Snare) — technical arbitrage mechanism; (2) treaty shopping (ε≈0.55, Tangled Rope) — coordination problem of bilateral treaties; (3) capital flight (ε≈0.72, Snare) — sovereign constraint from capital mobility. This story focuses on the aggregate mechanism. Downstream constraints include intellectual property localization (transfer of IP to low-tax jurisdictions) and regulatory capture of tax policy (multinationals shaping tax code preferences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisdictional_tax_arbitrage, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
