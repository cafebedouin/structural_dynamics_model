% ============================================================================
% CONSTRAINT STORY: sotu_1946_truman_full_employment_bill
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1946_truman_full_employment_bill, []).

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
 *   constraint_id: sotu_1946_truman_full_employment_bill
 *   human_readable: Full Employment Bill (1946): Government Responsibility for Employment Stabilization
 *   domain: economics/labor_policy
 *
 * SUMMARY:
 *   The Full Employment Bill of 1946 represents a fundamental reframing of
 *   federal government economic responsibility — from laissez-faire
 *   neutrality to active stabilization authority. President Truman's proposal
 *   commits government to 'formulate policy in light of full employment
 *   objectives' and provide 'economic assurance' to business, labor, and
 *   agriculture. The constraint exhibits Tangled Rope structure: it
 *   coordinates aggregate demand management through federal fiscal and
 *   monetary levers (genuine coordination function) while extracting costs
 *   through expanded government responsibility, deficit accumulation, and
 *   constraints on competing policy objectives (inflation control, tax policy
 *   autonomy). The constraint's extractiveness increases over the decade as
 *   inflation pressures mount and government discovers the Phillips Curve
 *   trade-off — the coordination function persists but is increasingly
 *   burdened by suppression of alternative economic policies. Theater ratio
 *   rises as the bill's implementation becomes rhetorical (politicians invoke
 *   full employment commitment without committing resources) while underlying
 *   labor market volatility persists. The constraint benefits organized
 *   labor, stabilized business, and agricultural interests through reduced
 *   cyclical uncertainty; it imposes costs on government fiscal autonomy and
 *   on unorganized workers (rural, precarious, racial minorities) who remain
 *   outside the full employment protection even as unemployment theoretically
 *   declines.
 *
 * KEY AGENTS:
 *   - Organized Labor: Primary beneficiary (organized/constrained) — gains full employment norm, collective bargaining floor, and policy participation; bears cost of wage restraint during inflation cycles
 *   - Stabilized Business Class: Primary beneficiary (institutional/arbitrage) — gains predictable aggregate demand, reduced labor unrest, and stimulus floor during recessions; maintains flexibility to profit from booms
 *   - Unemployed and Precarious Workers: Primary victim (powerless/trapped) — bear suppression of labor market insecurity while government retains escape via 'natural rate' definitions; excluded from protection through sectoral and demographic targeting
 *   - Federal Government: Secondary actor (institutional/constrained) — accepts responsibility for full employment but constrains its own fiscal autonomy and must manage inflation trade-off
 *   - Agricultural Sector: Secondary beneficiary (institutional/constrained) — gains price supports and demand assurance but subordinate to manufacturing employment priorities
 *   - Progressive Economic Coalition: Organized agent (moderate/constrained) — builds temporary institutional scaffolding around Keynesian consensus; sees constraint as solving structural problem with sunset clause
 *   - Conservative Economic Orthodoxy: Institutional residue (powerful/mobile) — maintains cultural authority through 'natural law' framing despite policy defeat; provides intellectual cover for government escape via inflation arguments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1946_truman_full_employment_bill, 0.38).
domain_priors:suppression_score(sotu_1946_truman_full_employment_bill, 0.42).
domain_priors:theater_ratio(sotu_1946_truman_full_employment_bill, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1946_truman_full_employment_bill, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1946_truman_full_employment_bill, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1946_truman_full_employment_bill, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1946_truman_full_employment_bill, tangled_rope).
narrative_ontology:human_readable(sotu_1946_truman_full_employment_bill, "Full Employment Bill (1946): Government Responsibility for Employment Stabilization").
narrative_ontology:topic_domain(sotu_1946_truman_full_employment_bill, "economics/labor_policy").

domain_priors:requires_active_enforcement(sotu_1946_truman_full_employment_bill).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1946_truman_full_employment_bill, organized_labor).
narrative_ontology:constraint_beneficiary(sotu_1946_truman_full_employment_bill, stabilized_business_class).
narrative_ontology:constraint_beneficiary(sotu_1946_truman_full_employment_bill, agricultural_sector).
narrative_ontology:constraint_victim(sotu_1946_truman_full_employment_bill, government_autonomy).
narrative_ontology:constraint_victim(sotu_1946_truman_full_employment_bill, fiscal_discipline_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNEMPLOYED WORKER (SNARE) — Trapped by economic cycles and industrial volatility. The bill creates rhetorical responsibility for full employment but provides no binding guarantee. Workers bear the suppression of labor market insecurity while government retains escape via economic forecasts or structural arguments ('natural rate of unemployment'). Maximum extraction from a structural victim with no exit option.
constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED LABOR (TANGLED ROPE) — Constrained by cyclical employment but coordinated through collective bargaining and union participation in policy forums. Beneficiary of full employment norm (higher wage floors, reduced desperation) but bears costs when government contracts to manage inflation. Mixed extraction: gains concrete coordination mechanism but loses flexibility to strike during booms.
constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STABILIZED BUSINESS CLASS (ROPE) — Benefits from predictable aggregate demand policy and reduced labor market chaos. Can arbitrage between government stimulus and private investment. Net beneficiary — the bill reduces their downside risk during recessions while maintaining upside in booms. Experiences the constraint as genuine coordination.
constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (TANGLED ROPE) — Bears enforcement costs of active economic management and accepts responsibility for full employment outcomes. Constrains fiscal policy by requiring employment objectives alongside deficit concerns. Benefits from reduced labor unrest and industrial conflict. Coordination function (stabilizing aggregate demand) is genuine; extraction lies in the asymmetric cost allocation to future administrations and generations.
constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE ECONOMIC COALITION (SCAFFOLD) — Temporary institutional scaffolding with sunset clause implied by political durability. The bill represents post-WWII consensus that federal government has responsibility to prevent 1930s-scale depression. Coalition is organized (economists, labor leaders, progressive politicians) and sees this as a structural problem being solved (Keynesian revolution in policy). Low effective extraction because agents see a temporary coordination solution pending cultural shift in economic thinking.
constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSERVATIVE ECONOMIC ORTHODOXY (PITON) — Laissez-faire ideology and balanced-budget doctrine persist despite the full employment bill becoming law. The orthodoxy is institutionally degraded (loses policy battles repeatedly) but maintains cultural authority through high-status economists and Federal Reserve independence. Theater ratio high because the constraint persists through rhetorical appeal to 'natural economic laws' rather than through demonstrated function. Business groups and conservative politicians continue invoking laissez-faire even while benefiting from full employment policy.
constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — Risk of naturalizing post-war institutional consensus as immutable economic law. 'Full employment is impossible without ruinous inflation' and 'government fiscal management creates moral hazard' are presented as laws of economics rather than contingent policy choices. This perspective risks false-summit classification — the 'natural' constraints on government economic management are actually political and institutional constraints being naturalized as economic science.
constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1946_truman_full_employment_bill_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1946_truman_full_employment_bill, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1946_truman_full_employment_bill, TR),
    TR >= 0.70.

:- end_tests(sotu_1946_truman_full_employment_bill_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The bill creates genuine coordination around full employment objectives — government commits to stabilizing aggregate demand through counter-cyclical fiscal policy, which benefits organized actors by reducing unemployment volatility. But extraction emerges through asymmetric implementation: government benefits organized labor and stabilized business while retaining escape clauses (natural rate of unemployment, inflation necessity) that allow extraction of costs onto unorganized workers. Rising extractiveness over time (0.22 → 0.38) reflects accumulating Phillips Curve constraints — each year of implementation reveals higher inflation cost of maintaining full employment, shifting the asymmetry toward extraction. Suppression (0.42): Moderate. Significant but not total barriers to coordination exist: labor market frictions, regional unemployment, sectoral shifts, and industrial decline limit government's actual capacity to maintain full employment. But suppression is not structural immobility — organized labor and stabilized business can pressure government to sustain commitment. Theater ratio (0.55): Moderate-high. The constraint includes substantial performative content: political rhetoric about full employment commitment without committing adequate fiscal resources; Federal Reserve independence that contradicts government employment responsibility; definitional flexibility ('natural rate') that allows government to escape commitment. Theater rises over the decade as implementation reveals the gap between commitment and capacity.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap lies between the Rope beneficiaries (stabilized business, organized labor) who experience genuine coordination, and the Snare victims (precarious workers, unemployed) who experience pure extraction with no exit. The bill creates coordination asymmetry: organized actors gain a seat at the policy table; unorganized actors gain only the rhetoric of full employment commitment. The government's Tangled Rope classification (moderate extraction costs mixed with genuine coordination) sits between these extremes. The Conservative Orthodox (Piton) classification captures how laissez-faire ideology persists through institutional inertia even after policy defeat — the ideology is degraded but maintains authority through claims of economic naturalism. The Analytical Observatory (Mountain) risks naturalizing what is contingent — the 'natural unemployment rate' concept emerged post-facto to rationalize why full employment commitment could not be sustained, not as a pre-existing constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (organized labor, stabilized business, agricultural sector) experience low directionality values (d ≈ 0.15-0.30) — they benefit from the constraint and have exit options (arbitrage for business, voice through collective organization for labor). Victims (unemployed workers, unorganized precarious workers) experience high directionality values (d ≈ 0.85-0.95) — they bear suppression costs with no exit options. The government's institutional position (d ≈ 0.55) reflects mixed directionality: it benefits from reduced labor unrest and political stability, but bears costs through fiscal constraints and inflation trade-off. The derivation chains for organized labor (beneficiary + constrained exit) and government (mixed beneficiary-victim + constrained exit) both produce Tangled Rope classification with moderate chi values. The powerless unemployed (victim + trapped exit) produce Snare with high chi. The organized labor coalition (moderate power + constrained exit + beneficiary) produces lower chi than the purely powerless due to their institutional voice and relative bargaining capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The Full Employment Bill resolves mandatrophy by demonstrating how a single policy constraint can be genuinely coordinating while simultaneously extractive, depending on structural position. The coordination function is real: counter-cyclical fiscal policy genuinely reduces unemployment volatility compared to laissez-faire baseline. The extraction is also real: costs of this coordination (inflation, deficit, fiscal autonomy loss) are distributed asymmetrically onto powerless agents while benefits accrue to organized and institutional actors. The constraint is Tangled Rope precisely because both aspects are constitutive — it cannot be reduced to pure coordination (Rope) nor to pure extraction (Snare). The rise in theater ratio (0.35 → 0.55) indicates degradation over the decade: initial implementation attempts genuine coordination, but as inflation constraints bind, the bill's enforcement becomes increasingly rhetorical ('commitment to full employment' without matching resources). The mandatrophy is resolved not by choosing one type, but by recognizing that the tangled quality (mixing genuine coordination with asymmetric extraction) is the constraint's essential structure. False-summit risk appears in the Analytical Observatory's Mountain perspective: 'full employment is impossible without inflation' naturalizes what is actually a political choice (accepting inflation costs) as economic law. The structural data reveals this as false — inflation is a choice about cost distribution, not a natural limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    full_employment_definition_ambiguity,
    'What counts as ''full employment'' for policy purposes — does the bill bind government to zero unemployment, or some ''natural rate'' that is politically undefined?',
    'Legislative history and executive interpretation documents; Federal Reserve policy statements and Humphrey-Hawkins correspondence (1978); employment target evolution over time',
    'If interpreted as zero unemployment: unsustainable constraint driving inflation expectations (Snare severity increases). If interpreted as variable ''natural rate'': government gains indefinite escape clause, rendering constraint performative (Piton classification strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(full_employment_definition_ambiguity, empirical, 'Definition of full employment in policy practice versus legislative intent').

omega_variable(
    government_fiscal_capacity_limit,
    'Does federal government actually have fiscal capacity to maintain full employment through business cycle, or does deficit constraint prevent sustained implementation?',
    'Long-term deficit analysis 1946-1980; correlation between recession severity and government spending response; comparison with countries that sustained lower unemployment through higher deficits',
    'If capacity exists: tangled rope classification confirmed — genuine coordination with real extraction costs. If capacity limited: constraint is performative (Piton) — government accepts responsibility but lacks tools.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_fiscal_capacity_limit, empirical, 'Federal fiscal capacity to sustain full employment policy').

omega_variable(
    inflation_trade_off_inevitability,
    'Is the Phillips Curve trade-off (unemployment-inflation) an immutable natural law or a contingent institutional relationship dependent on expectations and labor market structure?',
    'Cross-country inflation-unemployment comparison; historical shifts in Phillips Curve slope; supply-side shocks and cost-push inflation episodes; wage-setting institutional differences',
    'If immutable: full employment policy inevitably generates inflation (mountain constraint on feasibility). If contingent: extractive institution that naturalizes political choices as economic inevitability (false-summit candidate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_trade_off_inevitability, conceptual, 'Whether unemployment-inflation trade-off is natural law or contingent institutional fact').

omega_variable(
    labor_bargaining_power_asymmetry,
    'Does full employment bill strengthen worker bargaining power symmetrically across industries, or does it concentrate benefits on organized sectors while leaving precarious workers unprotected?',
    'Wage growth dispersion analysis; union density correlation with employment bill passage; sectoral employment stability comparison (union-dense vs. precarious sectors)',
    'If symmetric: genuine coordination function strengthens (Rope classification broader). If asymmetric: constraint extracts from precarious workers and benefits only organized labor (Snare complexity increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_bargaining_power_asymmetry, empirical, 'Distribution of full employment benefits across worker categories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1946_truman_full_employment_bill, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feb1946_tr_t0, sotu_1946_truman_full_employment_bill, theater_ratio, 0, 0.35).
narrative_ontology:measurement(feb1946_tr_t5, sotu_1946_truman_full_employment_bill, theater_ratio, 5, 0.48).
narrative_ontology:measurement(feb1946_tr_t10, sotu_1946_truman_full_employment_bill, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(feb1946_be_t0, sotu_1946_truman_full_employment_bill, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(feb1946_be_t5, sotu_1946_truman_full_employment_bill, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(feb1946_be_t10, sotu_1946_truman_full_employment_bill, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1946_truman_full_employment_bill, resource_allocation).
narrative_ontology:affects_constraint(sotu_1946_truman_full_employment_bill, phillips_curve_inflation_trade_off).
narrative_ontology:affects_constraint(sotu_1946_truman_full_employment_bill, federal_reserve_policy_autonomy).
narrative_ontology:affects_constraint(sotu_1946_truman_full_employment_bill, deficit_accumulation_norm).

% DUAL FORMULATION NOTE:
% Full Employment Bill decomposes into multiple structurally distinct constraints: (1) aggregate demand coordination (ε ≈ 0.15, pure Rope), (2) government fiscal responsibility acceptance (ε ≈ 0.42, Tangled Rope), (3) inflation suppression constraint (ε ≈ 0.55, Tangled Rope), (4) labor market rigidity (ε ≈ 0.48, Snare for precarious workers). Each has different beneficiaries/victims. The present story focuses on the aggregate coordination constraint (ε = 0.38 as weighted average), but downstream constraints have distinct ε values reflecting empirical debate over whether full employment commitment is feasible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1946_truman_full_employment_bill, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
