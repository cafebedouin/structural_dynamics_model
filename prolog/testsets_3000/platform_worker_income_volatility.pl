% ============================================================================
% CONSTRAINT STORY: platform_worker_income_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_worker_income_volatility, []).

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
 *   constraint_id: platform_worker_income_volatility
 *   human_readable: Platform Worker Income Volatility
 *   domain: economic/labor
 *
 * SUMMARY:
 *   Platform-mediated work (ride-sharing, delivery, gig labor) has created a
 *   novel labor market structure where income volatility is structurally
 *   embedded into the work itself. Rather than traditional employment with
 *   stable wages and scheduled hours, platform workers experience earnings
 *   that fluctuate daily based on algorithmic allocation, demand patterns,
 *   and dynamic pricing. This constraint exhibits characteristics of both
 *   coordination (the platform solves the matching problem of connecting
 *   workers to tasks flexibly) and extraction (the same system suppresses
 *   wages, externalizes scheduling risk, and creates information asymmetry).
 *   The constraint's evolution over the past decade shows increasing
 *   extractiveness as platforms consolidated market power and refined
 *   algorithmic wage suppression. The theater ratio reflects growing
 *   institutional attention to platform work (regulatory hearings,
 *   litigation, union organizing) that has not yet produced material change
 *   in worker income stability, suggesting theatrical commitment without
 *   functional reform. From different structural positions — workers,
 *   households, platform operators, organizing coalitions, traditional
 *   regulators, and analytical observers — this single constraint appears as
 *   pure extraction (snare), mixed coordination-extraction (tangled rope),
 *   pure coordination (rope), a solvable temporary problem (scaffold), a
 *   degraded regulatory system (piton), and a complex hybrid requiring
 *   structural analysis.
 *
 * KEY AGENTS:
 *   - Platform Workers: Primary victims (powerless/trapped) — bear income volatility without exit options; structural dependency on platform access for livelihood
 *   - Worker Households: Secondary victims (moderate/constrained) — depend on platform income for family stability; face planning barriers from volatility but benefit from scheduling flexibility for dependent care
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture value from matching efficiency and wage suppression; can exit/redesign systems if preferred
 *   - Capital Markets/Investors: Secondary beneficiaries (institutional/arbitrage) — benefit from suppressed labor costs and platform valuation; incentivize volatility as feature
 *   - Worker Organizing Coalition: Organized collective (organized/constrained) — unions, advocacy groups, cooperative platform experiments; see regulatory sunset pathway; pushing for income guarantees and algorithmic transparency
 *   - Traditional Labor Regulators: Degraded institutional system (institutional/arbitrage) — wage floors and scheduling protections formally exist but are bypassed via contractor classification; enforcement capacity has atrophied; system persists through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees genuine hybrid coordination-extraction without reduction to either pole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_worker_income_volatility, 0.58).
domain_priors:suppression_score(platform_worker_income_volatility, 0.68).
domain_priors:theater_ratio(platform_worker_income_volatility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_worker_income_volatility, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_worker_income_volatility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_worker_income_volatility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_worker_income_volatility, tangled_rope).
narrative_ontology:human_readable(platform_worker_income_volatility, "Platform Worker Income Volatility").
narrative_ontology:topic_domain(platform_worker_income_volatility, "economic/labor").

domain_priors:requires_active_enforcement(platform_worker_income_volatility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_worker_income_volatility, platform_operators).
narrative_ontology:constraint_beneficiary(platform_worker_income_volatility, capital_markets).
narrative_ontology:constraint_victim(platform_worker_income_volatility, platform_workers).
narrative_ontology:constraint_victim(platform_worker_income_volatility, worker_household_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLATFORM WORKER (SNARE) — Trapped by economic dependency with no material exit options. Income volatility is structurally designed: algorithms suppress wages during high-demand periods, suppress opportunities during low-demand periods. Workers bear full suppression cost without coordination benefit. No alternative employment pathways available; household budget cannot absorb volatility. Effective extraction maximized.
constraint_indexing:constraint_classification(platform_worker_income_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKER HOUSEHOLD (TANGLED ROPE) — Constrained by dependent care obligations and geographic immobility but perceives some genuine coordination function: platform access solves childcare-compatible scheduling. Mixed extraction: income volatility creates planning barriers, but scheduling flexibility provides real coordination benefit. Exit available only at high cost (career reset, relocation).
constraint_indexing:constraint_classification(platform_worker_income_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination: matching supply and demand, managing pricing, optimizing driver utilization. From operator perspective, volatility is efficient price discovery and worker sorting. No perception of extraction — the system coordinates and generates value. Arbitrage exit enables institutional actor to shift markets or redesign algorithm if preferred.
constraint_indexing:constraint_classification(platform_worker_income_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WORKER ORGANIZING COALITION (SCAFFOLD) — Organized collective (unions, worker advocacy groups, cooperative platforms) perceives the volatility constraint as temporary and solvable through policy intervention: minimum income guarantees, algorithmic transparency regulation, portable benefits. See clear sunset pathway: regulatory mandates establishing income floors and scheduling predictability. High suppression justified only if policy window remains open; supports classification as transitional coordination problem.
constraint_indexing:constraint_classification(platform_worker_income_volatility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL LABOR REGULATION (PITON) — Wage floors, scheduling protections, and benefits mandates are largely theatrical when applied to platform work: workers classified as independent contractors bypass coverage; regulations designed for W-2 employment structure don't fit platform models; agencies lack enforcement capacity at scale. Theater ratio reflects that traditional regulatory apparatus is maintained (agencies still exist, laws still written) but no longer functionally controls platform labor. Enforcement has atrophied; ritual persists.
constraint_indexing:constraint_classification(platform_worker_income_volatility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (supply-demand matching, flexible scheduling, zero-friction access) AND genuine extraction (wage suppression, information asymmetry, externalizing risk onto workers). The constraint cannot be reduced to pure extraction or pure coordination. The platform architecture itself enables both — the same algorithm that matches requests also suppresses wages during peak demand. Classification as tangled_rope from all structural data: beneficiaries present (platform operators, investors), victims present (workers), active enforcement required (algorithmic control), bases for both coordination and extraction exist.
constraint_indexing:constraint_classification(platform_worker_income_volatility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_worker_income_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_worker_income_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_worker_income_volatility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_worker_income_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_worker_income_volatility, TR),
    TR >= 0.70.

:- end_tests(platform_worker_income_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators extract value through wage suppression (dynamic pricing that reduces worker share during peak demand), information asymmetry (algorithms opaque to workers), and risk externalization (workers absorb scheduling and income volatility). But coordination function is genuine — platforms do solve matching problems and provide scheduling flexibility that workers value. The extractiveness value reflects that extraction is significant and structural but not maximal (workers retain some choice, some platforms compete on wages, some income upside exists during surges). The measurement trajectory shows extractiveness increasing over the interval as algorithmic sophistication increased and market consolidation reduced worker bargaining power. Suppression (0.68): Moderately high. Workers face significant barriers to exit: economic dependency (household relies on income), geographic constraint (local labor markets often dominated by 1-2 platforms), skill non-transferability (platform-specific reputation capital), and information asymmetry (algorithm design prevents accurate wage expectations). Barriers are not absolute (some workers do exit, some have fallback employment) but are substantial. Theater ratio (0.55): Moderate. Reflects that traditional labor regulation (wage floors, scheduling protections, benefits mandates) is formally maintained but not functionally controlling platform labor — workers are classified as independent contractors outside regulatory coverage. Simultaneously, there is genuine organizing activity, regulatory proposals, and public attention (not pure degradation). The ratio reflects mixed state: some performative theater (regulations written but not enforced), but also some real coordination work (organizing, platform design alternatives). As scaffolding efforts (regulatory proposals, cooperative platforms) mature, theater ratio may decrease if real alternatives materialize.
 *
 * PERSPECTIVAL GAP:
 *   Platform operator perceives coordination (rope); worker perceives extraction (snare); household perceives mixed (tangled rope); coalition perceives solvable temporary problem (scaffold); regulator perceives degraded ritual (piton); observer perceives genuine hybrid (tangled rope). The gap is maximal because these agents experience structurally different constraints — the beneficiary experiences a functioning coordination system; the trapped victim experiences pure extraction. Same structural system; incompatible phenomenologies.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries with arbitrage exit (platform operators, investors) have low directionality values (d ≈ 0.10) derived from their capacity to exit and modify the system. Victims with trapped exit (workers) have high directionality values (d ≈ 0.95) derived from their structural inability to exit. Constrained actors (households, organizing coalition) occupy middle ground (d ≈ 0.40-0.65) reflecting real costs to exit but also some agency and alternatives. The chi formula χ = ε × f(d) × σ(S) produces different effective extractiveness for each actor based on their position: operators experience low chi (beneficiaries benefit from the system); workers experience high chi (maximum extraction from their perspective); households and coalition experience moderate chi (mixed costs and benefits). The scope modifier σ(global=1.2) amplifies chi across all perspectives because this constraint operates at global scale with high verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that both coordination and extraction are genuinely present. The platform operator's rope classification reflects real coordination value (matching, flexibility). The worker's snare classification reflects real extraction (wage suppression, risk externalization). The tangled rope classification is not a compromise — it is the structural truth: the same platform architecture that solves matching simultaneously implements wage suppression. The constraint cannot be dissolved by claiming one function is 'really' primary; both are primary. Mandatrophy resolution requires accepting that the constraint has dual function and cannot be reclassified to pure type without losing essential structure. The scaffold perspective shows one resolution pathway: policy intervention to separate coordination function from extraction mechanism (create matching function + protect wage floors separately). The piton perspective shows the degraded state: traditional regulation attempted the separation but failed through contractor classification. The organizing coalition perspective shows the structural possibility: cooperative platforms, portable benefits, algorithmic transparency could achieve coordination without extraction at lower scope but higher legitimacy cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_wage_suppression_intentionality,
    'Is wage volatility a necessary side effect of efficient supply-demand matching, or an intentionally designed extraction mechanism?',
    'Comparative analysis of platform algorithms: do platforms use different algorithmic regimes that decouple supply matching from wage suppression? Does regulatory requirement to publish algorithmic transparency reveal design choices that maximize volatility independent of matching efficiency?',
    'If necessary side effect: extractiveness drops to ~0.35 (rope classification becomes dominant). If intentional design: extractiveness confirmed at ~0.58+ (tangled_rope/snare confirmed). Classification consequence directly depends on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_wage_suppression_intentionality, empirical, 'Whether wage suppression is algorithmic design choice or matching necessity').

omega_variable(
    worker_exit_option_elasticity,
    'How elastic is the ''trapped'' exit classification? What percentage of platform workers have material alternative employment available, and at what wage/stability cost?',
    'Survey data on worker exit pathways: availability of comparable W-2 employment, feasibility of skill transition, geographic labor market conditions. Comparison of platform workers who have exited to alternative employment vs those who remain.',
    'If > 40% have viable alternatives at modest cost: reclassify as ''constrained'' (not ''trapped'') for powerless perspective → classification shifts toward tangled_rope/moderate snare. If < 20% have alternatives: ''trapped'' confirmed → snare classification solidified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_exit_option_elasticity, empirical, 'Worker exit elasticity and availability of alternative employment').

omega_variable(
    coordination_benefit_versus_volatility_cost,
    'For workers with dependent care obligations and geographic constraints, does scheduling flexibility coordination benefit outweigh or fail to offset income volatility cost?',
    'Longitudinal household budget data: comparison of workers'' ability to handle childcare coordination and financial stress with vs without platform flexibility. Threshold analysis: at what volatility threshold does coordination benefit flip to net negative?',
    'If coordination benefit > volatility cost: tangled_rope classification dominates across more perspectives; constraint is genuine hybrid. If volatility cost >> benefit: snare classification dominates; coordination function is marginal theater. Classification hinges on empirical balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_versus_volatility_cost, empirical, 'Coordination benefit magnitude relative to volatility harm').

omega_variable(
    regulatory_closure_timeline,
    'How long is the viable policy window for the scaffold sunset? When does regulatory capture or market consolidation close exit pathway?',
    'Political economy analysis: track regulatory momentum (state legislation, federal proposed changes, platform counter-lobbying). Monitor platform market consolidation trends. Identify trigger points where exit pathway becomes implausibly costly.',
    'If window > 10 years: scaffold classification legitimate. If window < 3 years already closing: scaffold is aspirational piton (performative sunset), reclassify toward piton. High confidence resolution would transform expectations about generational change timeline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_closure_timeline, empirical, 'Viability and timeline of regulatory sunset pathway').

omega_variable(
    identity_lock_versus_economic_trap,
    'For workers with long tenure on platforms, is the binding mechanism structural economic dependency (trapped) or identity fusion with platform identity/brand (identity_locked)?',
    'Qualitative research: worker self-narratives about exit barriers. Distinguish between ''I cannot leave due to financial necessity'' (structural trap) vs ''I am a [platform brand] driver/seller/creator and cannot imagine myself outside that identity'' (identity lock). Test via counterfactual: if household income could be guaranteed elsewhere, would worker exit?',
    'If identity_locked: different classification regime applies (rope at biographical time from identity_locked perspective per the immutability matrix). Exit mechanism is cognitive reframing, not material change. If trapped: snare classification confirmed. Strategic intervention depends entirely on mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_versus_economic_trap, conceptual, 'Identity fusion versus economic structural dependency as binding mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_worker_income_volatility, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pwiv_tr_t0, platform_worker_income_volatility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pwiv_tr_t3, platform_worker_income_volatility, theater_ratio, 3, 0.42).
narrative_ontology:measurement(pwiv_tr_t6, platform_worker_income_volatility, theater_ratio, 6, 0.5).
narrative_ontology:measurement(pwiv_tr_t9, platform_worker_income_volatility, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(pwiv_be_t0, platform_worker_income_volatility, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pwiv_be_t3, platform_worker_income_volatility, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(pwiv_be_t6, platform_worker_income_volatility, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(pwiv_be_t9, platform_worker_income_volatility, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_worker_income_volatility, resource_allocation).
narrative_ontology:affects_constraint(platform_worker_income_volatility, algorithmic_labor_market_power).
narrative_ontology:affects_constraint(platform_worker_income_volatility, contractor_classification_loophole).
narrative_ontology:affects_constraint(platform_worker_income_volatility, income_based_household_risk).

% DUAL FORMULATION NOTE:
% Platform worker income volatility decomposes into three structurally distinct constraints: (1) algorithmic labor market power concentration (ε≈0.52, how platforms suppress wages through information asymmetry), (2) contractor classification loophole (ε≈0.48, how legal classification bypasses labor regulation), (3) household risk externalization (ε≈0.35, how volatility propagates to family stability). All three are linked by network affects; this story captures the integrated experience. Upstream constraints have higher specificity; this story shows the aggregate volatility outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
