% ============================================================================
% CONSTRAINT STORY: eu_affordable_housing_initiative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_affordable_housing_initiative, []).

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
 *   constraint_id: eu_affordable_housing_initiative
 *   human_readable: EU Affordable Housing Initiative (2025)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU Affordable Housing Initiative (2025) represents a structural
 *   attempt to coordinate housing policy across member states while
 *   redistributing the costs of affordability failures upward to municipal
 *   systems and downward to excluded households. The constraint exhibits a
 *   fundamental tension: it promises to solve affordability through market
 *   coordination (developer efficiency, economies of scale) while
 *   simultaneously requiring those markets to be subordinated to social
 *   objectives. This tension produces extractive outcomes in practice —
 *   developers capture permitted-density upzones and subsidies as private
 *   profit, while municipalities absorb unfunded mandates and low-income
 *   households accept conditional, means-tested access with stigma attached.
 *   The initiative's theater ratio (0.58) reflects substantial gap between
 *   stated affordability targets and actual housing cost-to-income
 *   improvements for the lowest deciles. Metrics show extraction increasing
 *   over the implementation period as initial coordination gains (harmonized
 *   standards, simplified permitting) have been captured by developers as
 *   efficiency improvements rather than passed through to affordability. The
 *   constraint classifies as tangled_rope (mixed coordination and extraction)
 *   from the analytical context, but appears as pure snare from the powerless
 *   households and pure profit-extraction from developers.
 *
 * KEY AGENTS:
 *   - Low-Income Households in Stressed Housing Markets: Primary victims (powerless/trapped) — access conditioned on developer preferences and municipal administrative capacity; no exit options
 *   - Municipal Housing Authorities: Secondary beneficiaries and constrained victims (moderate/constrained) — gain coordination benefits but absorb debt for private developer gain; limited exit through circumvention
 *   - Real Estate Development Sector: Primary beneficiaries (organized/arbitrage) — extract permitted upzones, subsidies, and labor cost reductions; can relocate investment across borders
 *   - Social Housing Cooperatives: Organized alternative pathway (organized/constrained) — building non-developer-dependent models; see initiative as temporary constraint with sunset within 15-20 years
 *   - National Housing Ministries: Institutional but degraded actors (institutional/arbitrage) — maintain form of policy enforcement but have lost causal power to housing-as-investment flows
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risk frame is naturalizing policy choices (housing-as-investment primacy) as economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_affordable_housing_initiative, 0.52).
domain_priors:suppression_score(eu_affordable_housing_initiative, 0.65).
domain_priors:theater_ratio(eu_affordable_housing_initiative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, extractiveness, 0.52).
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_affordable_housing_initiative, tangled_rope).
narrative_ontology:human_readable(eu_affordable_housing_initiative, "EU Affordable Housing Initiative (2025)").
narrative_ontology:topic_domain(eu_affordable_housing_initiative, "economic/political").

domain_priors:requires_active_enforcement(eu_affordable_housing_initiative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, real_estate_developers).
narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, construction_sector).
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, low_income_households).
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, municipal_finance_systems).
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, renters_without_property_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME HOUSEHOLDS (SNARE) — Trapped by geography and economic circumstance. The initiative creates conditional access to housing subsidies tied to developer profit targets and municipal debt absorption. Unable to exit the constraint or organize collectively. Bears extraction through means-testing, neighborhood stigma, and long waiting periods while developers capture permitted-density upzones as private gain.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MUNICIPAL HOUSING AUTHORITIES (TANGLED ROPE) — Constrained by limited fiscal capacity and legal inability to refuse housing obligations. The initiative creates real coordination benefit (shared standards, pooled procurement, cross-border expertise) but requires municipalities to absorb debt for developments that benefit private developers. Exit options limited to circumvention (reclassifying housing as commercial, shifting costs to other services). Some agency through consortium-building; significant extraction through unfunded mandates.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REAL ESTATE DEVELOPERS (ROPE) — Primary beneficiary. The initiative coordinates land-use policy across member states, enabling developers to extract permitted-density upzones and subsidized labor through apprenticeship requirements. Arbitrage available: can relocate investment across borders based on subsidy availability. Experiences constraint as pure coordination: harmonized standards reduce transaction costs and unlock profit from previously-regulated land. Net extraction flows toward this sector.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: SOCIAL HOUSING COOPERATIVES (SCAFFOLD) — Organized actors (Baugruppen, housing associations, community land trusts) view the initiative as a temporary coordination failure with emerging alternative pathways. Constrained by capital access but building cooperative ownership models that bypass developer extraction. See the initiative as a sunset mechanism: cooperative procurement power and EU carbon-neutral housing standards are creating new affordability pathways that will displace the developer-subsidy model within 15-20 years. Theater ratio lower for cooperatives (actual cooperative ownership) than for traditional social rental (bureaucratic allocation).
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: NATIONAL HOUSING MINISTRIES (PITON) — Degraded institutional function. National housing policy has atrophied as real estate investment became globalized capital flows. Ministries maintain form (statistics, targets, regulations) but have lost real enforcement capacity over actual housing supply — real decisions are made by developers, planning committees, and pension funds. The initiative's theater is high: reporting affordable units delivered, tracking completion targets, measuring affordability ratios. But functional housing allocation has already shifted to market mechanisms. Maintained through institutional inertia rather than causal force.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational perspective, housing cost-to-income ratios are subject to natural economic laws: in efficient markets, housing prices reflect the marginal productivity of land and construction labor. From this view, affordability problems are inherent to economically productive regions and cannot be engineered away through policy — the initiative is attempting to violate a natural law. However, the structural data contradicts mountain classification: this is not a logical or physical limit but a policy choice to subordinate housing-as-shelter to housing-as-investment, revealing the false summit.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_affordable_housing_initiative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_affordable_housing_initiative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_affordable_housing_initiative, TR),
    TR >= 0.70.

:- end_tests(eu_affordable_housing_initiative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The initiative creates measurable value extraction from three sources: (1) permitted-density upzones that become developer profit rather than affordability gains (~0.15 of extractiveness), (2) unpaid municipal debt absorption for obligation defaults (~0.20), (3) means-testing and administrative overhead that exclude 20-30% of eligible low-income households (~0.17). The value is real and flows consistently toward developers and away from the target population. Suppression (0.65): High. Barriers to alternative housing pathways include: capital constraints for cooperatives, regulatory preference for commercial developers, municipal fiscal limits preventing independent affordable housing, fragmentation across 27 different legal systems. But suppression is not total — cooperative models exist, some municipalities maintain public housing, alternative financing mechanisms are emerging. Theaters (0.58): Moderate-high. The initiative's public face emphasizes units delivered and affordability targets. Internal dynamics reveal substantial performance theater: 'affordable' often means 80% of median rent (unaffordable for the bottom 30% of income distribution), delivery timelines slip repeatedly, 'affordable units' frequently shift to market-rate when completion dates extend. Theater has increased over the interval as the gap between promised and delivered affordability has widened.
 *
 * PERSPECTIVAL GAP:
 *   The strongest perspectival gap exists between developers (rope/coordination) and low-income households (snare/extraction). Developers experience the initiative as pure coordination — harmonized standards reduce transaction costs and unlock profitable development they would not attempt under fragmented national regulations. Low-income households experience the same constraint as pure extraction — their nominal access is conditioned on developer willingness to participate, which means building at densities profitable for developers, in locations developers prefer, with rents developers set. The tangled_rope classification from the analytical context captures both functions simultaneously: coordination (real, measurable efficiency gains) and extraction (real, measurable benefit concentration). The scaffold perspective introduces a temporal element: cooperatives see the initiative as a temporary solution that will be displaced by cooperative ownership models within a generation. The piton perspective reveals that national housing ministries have become theater — they announce targets and track statistics, but real housing allocation decisions are made by private capital flows, not policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's perceived extractiveness (chi) is determined by the agent's structural position and exit options relative to the constraint. Low-income households have d ≈ 0.95 (powerless + trapped) producing high experienced chi — the constraint extracts from them through conditional access and stigma. Developers have d ≈ 0.10 (arbitrage exit + beneficiary status) producing low or negative chi — extraction flows toward them, not away. Municipalities have d ≈ 0.55 (constrained exit + mixed victim/beneficiary) producing moderate chi — caught between coordination gains and extraction costs. Cooperatives have d ≈ 0.45 (constrained exit + emerging beneficiary status) producing chi near the symmetric point. The piton classification reflects institutional degradation: national housing ministries maintain regulatory form but have lost actual causal power to housing-as-investment flows; the theater gate (0.58) triggers degradation classification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CANDIDATE — This constraint is currently unresolved at ε=0.52. The core mandatrophy is: Does the initiative coordinate efficient housing production (rope-like function) or primarily extract value from low-income households and municipalities (snare-like function)? The architectural evidence suggests it does both: the coordination function (harmonized standards, shared permitting databases) is real but has been captured by developers as efficiency improvement. The extraction function is also real — systematically concentrating benefit upward (developer profit, land value capture) and cost downward (municipal debt, household affordability gaps). Resolution requires measuring: (1) counterfactual developer behavior absent the initiative (would these projects be built anyway?), (2) actual vs promised affordability outcomes (rental cost as % of household income for bottom decile), (3) municipal debt accumulation pathways and service-delivery impacts. Current trajectory suggests resolution toward snare (extraction dominates) unless land-value capture mechanisms are activated and enforced. The scaffold perspective offers a different resolution path: if cooperatives scale to meaningful production volumes (>20% market share by 2035), the initiative becomes provisionally rope-like (coordination mechanism) with a sunset toward cooperative alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developer_profit_extraction_threshold,
    'At what ratio of public subsidy to private developer profit does the initiative transition from coordination to pure extraction?',
    'Cross-member-state financial analysis of subsidy flow vs developer returns; comparison of development costs vs permitted-density upzone value; tracking whether developers accept projects without subsidy at comparable densities',
    'If threshold < 0.3 (substantial extraction): snare classification correct for most municipalities. If threshold > 0.7 (minor extraction): rope classification more appropriate. Current uncertainty: estimates range 0.25-0.65.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_profit_extraction_threshold, empirical, 'Threshold distinguishing coordination benefit from developer extraction').

omega_variable(
    municipal_debt_capacity_collapse,
    'At what accumulated housing-related debt load do municipal service delivery systems functionally collapse?',
    'Time-series analysis of municipal fiscal capacity; correlation between housing subsidy obligations and degradation of other services (water, transit, education); threshold identification for service collapse in comparable cities across EU member states',
    'If collapse threshold < 15% of general fund: municipalities cannot absorb initiative obligations without devastating other services (snare becomes inevitable). If > 40%: municipal systems have significant buffering capacity (tangled_rope perspective more durable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(municipal_debt_capacity_collapse, empirical, 'Municipal debt capacity threshold before service delivery collapse').

omega_variable(
    cooperative_scale_sufficiency,
    'Can social housing cooperatives scale to affordable-housing delivery volumes comparable to commercial developer capacity within the initiative''s time horizon?',
    'Longitudinal tracking of cooperative housing production rates; comparison of capital-raising speed (cooperative bonds vs developer VC); identification of capital availability bottlenecks; assessment of regulatory barriers specific to cooperative models',
    'If cooperatives scale sufficiently (>30% of new affordable units by 2035): scaffold sunset is real structural feature (alternative pathways are materializing). If scaling stalls (<10%): scaffold is aspirational (exit path doesn''t actually exist).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cooperative_scale_sufficiency, empirical, 'Whether cooperatives can scale to displace developer-dependent housing delivery').

omega_variable(
    land_value_capture_mechanism,
    'Does the initiative contain functional mechanisms to capture publicly-created land value increases (from zoning changes, density permits) for public benefit?',
    'Legal analysis of land-value capture provisions in directive; empirical measurement of value increases from permitted upzones; tracking of whether captured value flows to housing affordability or remains with original landowners/developers',
    'If capture mechanisms functional and enforced: tangled_rope classification stable (extraction offset by coordination gain). If mechanisms absent or unenforced: pure developer extraction dominates (snare classification for municipalities becomes inevitable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(land_value_capture_mechanism, empirical, 'Effectiveness of land-value capture provisions in the initiative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_affordable_housing_initiative, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(euahi_tr_t0, eu_affordable_housing_initiative, theater_ratio, 0, 0.42).
narrative_ontology:measurement(euahi_tr_t2, eu_affordable_housing_initiative, theater_ratio, 2, 0.5).
narrative_ontology:measurement(euahi_tr_t5, eu_affordable_housing_initiative, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(euahi_be_t0, eu_affordable_housing_initiative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(euahi_be_t2, eu_affordable_housing_initiative, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(euahi_be_t5, eu_affordable_housing_initiative, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_affordable_housing_initiative, resource_allocation).
narrative_ontology:affects_constraint(eu_affordable_housing_initiative, eu_construction_labor_standards).
narrative_ontology:affects_constraint(eu_affordable_housing_initiative, municipal_fiscal_sustainability).
narrative_ontology:affects_constraint(eu_affordable_housing_initiative, real_estate_investment_regulation).

% DUAL FORMULATION NOTE:
% The affordable housing initiative decomposes into three structurally distinct constraints: (1) coordination of building standards and permitting (low ε, rope-like), (2) developer subsidy absorption and profit extraction (higher ε, snare-like for municipalities), (3) household affordability outcomes (highest ε, snare-like for low-income households). This story integrates all three, but future decomposition may separate household affordability access (snare, ε>0.6) from municipal funding obligations (tangled_rope, ε≈0.5) from developer incentive structures (rope, ε<0.35).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_affordable_housing_initiative, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
