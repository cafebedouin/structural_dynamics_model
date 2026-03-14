% ============================================================================
% CONSTRAINT STORY: nordic_social_contract
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nordic_social_contract, []).

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
 *   constraint_id: nordic_social_contract
 *   human_readable: The Nordic Social Contract
 *   domain: political_economy/labor_relations
 *
 * SUMMARY:
 *   The Nordic social contract represents a post-WWII institutional
 *   arrangement coordinating capital-labor relations through centralized wage
 *   bargaining, strong unions, comprehensive welfare provision, and active
 *   labor market policies. From institutional perspectives it appears as a
 *   genuine coordination mechanism solving collective action problems and
 *   enabling long-term production planning. From the perspective of capital
 *   holders facing constrained profits and non-union workers facing exclusion
 *   and wage suppression, it appears as extraction. The constraint exhibits
 *   all six types depending on structural position: pure extraction for
 *   non-unionized low-wage workers (Snare); coordination for union members
 *   and state providers (Rope); mixed coordination-extraction for large
 *   employers (Tangled Rope); a sunset institutional arrangement challenged
 *   by globalization (Scaffold); and a naturalized cultural law (Mountain)
 *   from civilizational perspectives that risk obscuring its contingent
 *   institutional origins. The theater ratio (0.45) reflects moderate
 *   performative content: the negotiation rituals, governance structures, and
 *   consensus-building procedures are substantive rather than purely
 *   ceremonial, but some theater has increased as the contract has faced
 *   pressure from global competition and demographic change.
 *
 * KEY AGENTS:
 *   - Labor Unions: Organized beneficiary (organized/constrained) — core institutional force defending contract; experience it as protection and coordination
 *   - Union Workers: Primary beneficiary (organized/constrained) — capture wage premiums and job security; also identify with union institution
 *   - Non-Union Low-Wage Workers: Primary victim (powerless/trapped) — excluded from bargaining; experience wage suppression and precarity
 *   - Large Employers in Union Sector: Mixed actor (powerful/arbitrage) — benefit from labor peace and long-term planning but face constrained profits; can exit through relocation
 *   - Capital Owners / Shareholders: Secondary victim (powerful/arbitrage) — constrained profit margins; arbitrage option through capital relocation or divesting from high-cost Nordic production
 *   - State Welfare Institutions: Beneficiary coordinator (institutional/arbitrage) — manage universal welfare provision; experience genuine coordination benefits
 *   - Global Competitiveness Coalition: Organized opposition (organized/constrained) — business councils and policy organizations treating contract as temporary, unsustainable constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nordic_social_contract, 0.38).
domain_priors:suppression_score(nordic_social_contract, 0.32).
domain_priors:theater_ratio(nordic_social_contract, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nordic_social_contract, extractiveness, 0.38).
narrative_ontology:constraint_metric(nordic_social_contract, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(nordic_social_contract, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nordic_social_contract, tangled_rope).
narrative_ontology:human_readable(nordic_social_contract, "The Nordic Social Contract").
narrative_ontology:topic_domain(nordic_social_contract, "political_economy/labor_relations").

domain_priors:requires_active_enforcement(nordic_social_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nordic_social_contract, labor_unions).
narrative_ontology:constraint_beneficiary(nordic_social_contract, public_sector_employees).
narrative_ontology:constraint_beneficiary(nordic_social_contract, welfare_recipients).
narrative_ontology:constraint_victim(nordic_social_contract, capital_owners).
narrative_ontology:constraint_victim(nordic_social_contract, low_wage_service_workers).
narrative_ontology:constraint_victim(nordic_social_contract, non_union_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-UNION LOW-WAGE WORKER (SNARE) — Trapped by labor market structure and immigration restrictions; excluded from union protections and collective bargaining. High suppression from limited exit options. Wage floors and working conditions are set through union-employer negotiations that exclude them. Maximum extraction experienced.
constraint_indexing:constraint_classification(nordic_social_contract, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNION WORKER (ROPE) — Core beneficiary of the contract. Experiences strong coordination: wage bargaining, workplace safety standards, and job security are achieved through institutional arrangement. High extractiveness for capital but coordination benefit is genuine and substantial. Constrained exit because union membership and collective wage-setting create career path dependencies, but these are experienced as protective rather than coercive.
constraint_indexing:constraint_classification(nordic_social_contract, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE EMPLOYER IN UNION SECTOR (TANGLED ROPE) — Coordination benefit is genuine: centralized wage bargaining reduces uncertainty, enables long-term investment in labor-intensive production, and provides labor peace. But extraction is significant: wages are higher than global competitive rates, profit margins are constrained, and employers cannot freely adjust labor inputs. Arbitrage exit option because multinational firms can relocate production. This perspective shows both genuine coordination and asymmetric extraction.
constraint_indexing:constraint_classification(nordic_social_contract, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE AS WELFARE PROVIDER (ROPE) — Experiences the social contract as coordination mechanism for risk pooling, unemployment insurance, healthcare, and education. The universal benefit design is genuinely coordinated — broad base enables low per-capita cost and high coverage. Exit option (arbitrage) through shifting welfare model, but path dependence is real. Not experiencing extraction as primary dynamic.
constraint_indexing:constraint_classification(nordic_social_contract, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GLOBAL COMPETITIVENESS COALITION (SCAFFOLD) — Neo-liberal organized opposition (business councils, economic think tanks, some policy makers) sees the social contract as a temporary high-cost institutional arrangement that will sunset as globalization forces wage convergence and capital mobility. This perspective treats the constraint as having a sunset clause — elevated labor costs and rigid labor markets are unsustainable in a global economy. Theater ratio lower from this perspective because the argument is forward-looking rather than performative.
constraint_indexing:constraint_classification(nordic_social_contract, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal scope, the Nordic contract is treated as an immutable institutional expression of Scandinavian culture, homogeneity, and social trust — a natural law of Nordic societies. This perspective risks naturalizing what is actually a contingent institutional arrangement shaped by post-WWII labor politics, resource wealth, and specific historical coalitions. The engine's false summit detector will flag this naturalization.
constraint_indexing:constraint_classification(nordic_social_contract, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nordic_social_contract_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nordic_social_contract, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nordic_social_contract, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(nordic_social_contract_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from capital owners through higher wages and profit constraints, but the extraction is not as severe as a pure snare — employers benefit from labor peace, reduced uncertainty, and long-term productivity gains from skilled, stable workforce. The contract enables high per-worker output that offsets wage costs in some sectors. The extractiveness has increased from 0.22 (1960s post-WWII) to 0.38 (present) as globalization has increased capital mobility and alternative production sites, making the constraint less sustainable. Suppression (0.32): Moderate. Barriers to exit include: capital relocation costs, union political power making unilateral wage cuts difficult, cultural/political consequences for employers breaking contract, path dependence in labor market institutions. But suppression is not total — employers have demonstrably relocated (particularly in manufacturing), and the constraint is contested (scaffold perspective). Theater ratio (0.45): Moderate. The institutional machinery (wage councils, negotiating procedures, consensus bodies) performs real coordination work but also includes some ceremonial elements: multi-stakeholder consensus meetings are sometimes cover stories for pre-negotiated settlements; the 'Swedish model' is sometimes performed for international audiences. Theater has risen from 0.35 to 0.45 as the contract has faced pressure — more of the negotiation process is now about maintaining the appearance of consensus as actual agreement has become harder to achieve.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the beneficiary institutions (unions, state welfare, large employers with long-term planning horizons) who experience coordination benefits, and the excluded populations (non-union low-wage workers) who experience pure extraction. A secondary gap exists between the institutional beneficiaries (unions, employers, state) who experience Rope or Tangled Rope, and the global competitiveness coalition + capital holders who see the contract as a temporary constraint with a sunset. The largest gap is between the mountain perspective (naturalizing the contract as Scandinavian cultural law) and the scaffold perspective (treating it as a temporary institutional arrangement unsustainable in global markets). The analytical observer risks the false summit — mistaking a contingent institutional arrangement for an immutable natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each actor's structural position: who benefits, who bears costs, what are their exit options. Non-union workers: they are victims with trapped exit (no alternative labor market access) → high d → high experienced extraction → Snare. Union workers: they are beneficiaries with constrained exit (membership is identity and career) → low d but not arbitrage → Rope. Large employers: they are victims of wage constraints but with arbitrage exit (can relocate) → moderate d → moderate experienced extraction → Tangled Rope. Capital holders: they are victims with arbitrage → moderate-high d. The state: they are beneficiary coordinator with arbitrage (policy choices) → low d → Rope. The global coalition: they are organized opposition with constrained exit (political/institutional barriers to dismantling contract) → moderate d → Scaffold. The analytical observer: structural position is universal/civilizational, at maximum remove from any specific extraction dynamic → d ≈ 0.72 canonical → baseline for mountain assessment.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates mandatrophy through the perspectival gap between institutional beneficiaries and excluded populations. The contract is genuinely a Rope for union members and welfare recipients — it solves authentic collective action problems in wage setting and risk pooling. It is genuinely a Snare for non-union low-wage workers — they bear constraints without benefits. The Tangled Rope classification at the primary beneficiary level (large employers) is correct because the constraint does provide real coordination (labor peace, productivity, long-term planning) AND real extraction (profit constraints). The false summit (mountain perspective) naturalization is exposed by historical contingency: the contract emerged from specific post-WWII political conditions (labor strength, social democratic governing coalitions, capital-labor cooperation against communism) and is now contested as globalization changes the cost-benefit for capital. The mandatrophy resolves by recognizing that all classifications are structurally legitimate from their respective positions — the question is not 'which type is correct' but 'whose structural position are we measuring from and what are the implications for stability and justice?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_homogeneity_mechanism,
    'Is the Nordic social contract causally dependent on ethnic/cultural homogeneity, or is causality running the opposite direction?',
    'Time-series analysis of contract stability versus immigration rates and demographic change; comparative analysis of contract erosion in cities vs rural areas with different demographic composition; mechanism experiments in synthetic policy contexts',
    'If contract is dependent on homogeneity: it is a vulnerability (sunset when demographics shift). If causality runs backward (contract enables tolerance and integration): it is more robust. If bidirectional (reinforcing loop): moderate stability with policy lock-in risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_homogeneity_mechanism, empirical, 'Causal relationship between social contract and demographic homogeneity').

omega_variable(
    capital_mobility_exit_viability,
    'How viable is capital exit (relocation) from the Nordic high-cost model, and does constraint-induced exit actually occur at scale?',
    'Historical data on firm relocations, investment patterns by industry; comparison of countries where employers exited (UK manufacturing) vs countries where they remained (Sweden); multinational FDI location choices; wage-cost elasticity of location decisions',
    'If exit is highly viable and occurring: constraint is unstable (snare for capital). If exit costs are high or competitive advantages offset wage costs: constraint is more stable (rope even for capital). This directly affects the ''arbitrage'' exit characterization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_mobility_exit_viability, empirical, 'Capital exit viability and actual relocation patterns').

omega_variable(
    asymmetric_distributional_impact,
    'How much of the extractiveness is actually paid by low-wage workers excluded from bargaining versus capital owners?',
    'Wage floor analysis: non-union vs union wage premiums; cost-of-living data for low-wage workers; profit margin compression analysis by industry; unemployment rate and duration for non-union workers',
    'If most extraction falls on capital: Tangled Rope is stable. If extraction is borne by excluded low-wage workers (through lower wages, higher unemployment, precarity): constraint is actually a Snare for them and a Rope + Arbitrage for insiders (bifurcated labor market). The claimed_type would need decomposition into separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_distributional_impact, empirical, 'Distribution of constraint costs across capital vs excluded workers').

omega_variable(
    welfare_universalism_mechanism,
    'Does universal welfare design actually produce lower per-capita costs through risk pooling, or does it produce higher costs through inclusion of low-productivity beneficiaries?',
    'Cost-benefit analysis of universal vs means-tested systems; comparison of healthcare/education/unemployment costs per capita across welfare regimes; administrative overhead comparisons; long-term productivity and intergenerational mobility effects',
    'If universalism is cost-efficient: welfare component is pure rope (coordination). If it is cost-inefficient but politically durable: welfare component is piton (degraded coordination, maintained by political inertia). This affects the analytical observer''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_universalism_mechanism, empirical, 'Cost-efficiency of universal versus means-tested welfare').

omega_variable(
    institutional_capture_by_insiders,
    'To what extent is the social contract maintained through insider institutional capture (union and employer organizations defending their negotiating power) versus genuine coordination benefits?',
    'Historical analysis of contract renegotiations; cases where contract was challenged or modified; comparative institutional analysis of countries that abandoned similar contracts; wage-growth analysis relative to productivity; union decline in countries with similar institutions',
    'If capture is high: constraint appears as Snare (for outsiders) and institutional preservation (for insiders). If coordination benefits are genuine: Tangled Rope classification is correct. If mixed: the perspectival gap is larger than currently characterized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_by_insiders, empirical, 'Extent of insider institutional capture versus genuine coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nordic_social_contract, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsc_tr_t0, nordic_social_contract, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nsc_tr_t20, nordic_social_contract, theater_ratio, 20, 0.4).
narrative_ontology:measurement(nsc_tr_t40, nordic_social_contract, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(nsc_be_t0, nordic_social_contract, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nsc_be_t20, nordic_social_contract, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(nsc_be_t40, nordic_social_contract, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nordic_social_contract, resource_allocation).
narrative_ontology:affects_constraint(nordic_social_contract, wage_rigidity_and_employment).
narrative_ontology:affects_constraint(nordic_social_contract, capital_relocation_and_deindustrialization).
narrative_ontology:affects_constraint(nordic_social_contract, immigrant_labor_market_access).

% DUAL FORMULATION NOTE:
% The Nordic social contract can be decomposed into three structurally distinct constraints: (1) centralized wage bargaining (ε≈0.25, Rope), (2) welfare universalism (ε≈0.20, Rope), (3) labor market closure and non-union worker exclusion (ε≈0.65, Snare). These are linked through the union-employer-state triad but have distinct extractiveness and mechanisms. The claimed_type 'tangled_rope' represents the mixed position of large employers who experience both coordination and extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nordic_social_contract, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
