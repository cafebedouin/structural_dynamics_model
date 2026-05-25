% ============================================================================
% CONSTRAINT STORY: food_worker_wage_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_food_worker_wage_suppression, []).

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
 *   constraint_id: food_worker_wage_suppression
 *   human_readable: Food Worker Wage Suppression in Fast-Casual Sectors
 *   domain: labor/economic
 *
 * SUMMARY:
 *   Food worker wage suppression in fast-casual sectors operates through
 *   deliberate institutional design rather than market inevitability. The
 *   constraint combines genuine coordination functions (stable labor supply
 *   enables consistent service quality and predictable business operations)
 *   with asymmetric extraction (wages remain depressed far below value
 *   creation, working conditions are precarious, and worker exit options are
 *   systematically constrained). The mechanism relies on multiple nested
 *   suppression layers: labor market saturation (abundant entry-level workers
 *   reduce bargaining power), fragmented employment structure (franchising
 *   diffuses corporate responsibility), institutional legality (tip-credit
 *   wages remain lawful in much of the US), and cultural narrative (tipping
 *   is framed as 'server-driven income' rather than employer wage evasion).
 *   The extractiveness has increased over the 20-year interval (0.48 → 0.68)
 *   as real wages have stagnated while restaurant profitability has grown,
 *   indicating accumulation of extraction rather than stable coordination.
 *   The theater ratio has also increased as compliance performance (minimum
 *   wage enforcement, tip reporting theater) has become more elaborate
 *   without materially improving worker conditions.
 *
 * KEY AGENTS:
 *   - Food Service Workers: Primary victims (powerless/trapped) — bear suppression through wage stagnation, schedule unpredictability, and lack of exit options
 *   - Restaurant Operators: Primary beneficiaries (institutional/arbitrage) — extract through wage control while maintaining service quality via institutional coordination
 *   - Corporate Chains: Secondary beneficiary (institutional/arbitrage) — benefit from franchising structure that externalizes wage responsibility while maintaining brand consistency
 *   - Career Kitchen Workers: Mixed (moderate/constrained) — experience some coordination (skill-based advancement) alongside extraction (wage depression relative to value creation)
 *   - Labor Unions and Worker Centers: Organized agents (organized/constrained) — attempt collective exit but face deliberate coalition-fragmentation (high turnover, tip stratification)
 *   - Policy Interventionists: Organized reformers (organized/mobile) — see sunset pathway through minimum wage, portable benefits, and scheduling mandates
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing engineered labor market structure as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(food_worker_wage_suppression, 0.68).
domain_priors:suppression_score(food_worker_wage_suppression, 0.75).
domain_priors:theater_ratio(food_worker_wage_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(food_worker_wage_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(food_worker_wage_suppression, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(food_worker_wage_suppression, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(food_worker_wage_suppression, snare).
narrative_ontology:human_readable(food_worker_wage_suppression, "Food Worker Wage Suppression in Fast-Casual Sectors").
narrative_ontology:topic_domain(food_worker_wage_suppression, "labor/economic").

domain_priors:requires_active_enforcement(food_worker_wage_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(food_worker_wage_suppression, restaurant_operators).
narrative_ontology:constraint_beneficiary(food_worker_wage_suppression, franchise_corporate_entities).
narrative_ontology:constraint_victim(food_worker_wage_suppression, food_service_workers).
narrative_ontology:constraint_victim(food_worker_wage_suppression, household_economic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOOD SERVICE WORKER (SNARE) — Trapped by lack of transferable skills, geographic mobility constraints, immediate need for survival income, and absence of credible alternatives. Faces maximum extraction: wage suppression via labor market saturation, no-compete clauses limiting advancement, schedule unpredictability preventing second employment, and reliance on tips that are individually extracted through customer discretion rather than guaranteed. High suppression: cannot exit to higher-paying sectors without education/retraining they cannot afford; cannot organize collectively due to high turnover and hiring precarity.
constraint_indexing:constraint_classification(food_worker_wage_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER KITCHEN WORKER (TANGLED ROPE) — Constrained but not trapped: possesses specialized culinary skills that command modest premium, opportunity to advance to sous chef/head chef roles within the industry, and some bargaining power through craftsmanship reputation. Experiences mixed benefit: genuine skill-based coordination (high-quality cuisine requires coordinated labor) alongside asymmetric extraction (wages remain depressed relative to skill and value creation; hours and scheduling still unpredictable). Can exit through career advancement, small business ownership, or movement to higher-end establishments, but costs are significant (relocation, capital requirements, social risk).
constraint_indexing:constraint_classification(food_worker_wage_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MANAGER-TRACK EMPLOYEE (TANGLED ROPE) — Mobile at generational timescale: can move into shift supervisor, area manager, or corporate roles with education/tenure. Sees genuine coordination function: management structure requires reliable hierarchical coordination. Also experiences extraction: wage depression relative to responsibility; career advancement is selective and depends on location mobility and family stability (childcare, housing constraints create de facto immobility despite nominal mobility). Exit is possible but requires accepting geographic uprooting or credential investment.
constraint_indexing:constraint_classification(food_worker_wage_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: RESTAURANT OPERATOR (ROPE) — Institutional power, arbitrage-level exit: can shift labor costs through hiring mix (higher ratio of entry-level workers), franchising models, automation adoption, or relocation to lower-wage regions. Experiences the constraint as pure coordination: consistent labor pool, turnover-resistant culture creation, and predictable expense management. Net beneficiary—extraction runs toward them. The wage suppression mechanism actually solves a coordination problem: keeping labor costs stable and predictable enables menu pricing stability and franchise scalability.
constraint_indexing:constraint_classification(food_worker_wage_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: UNION ORGANIZING (SNARE with organized power) — Organized agents (labor unions, worker centers) face structural extraction: the constraint is deliberately designed to prevent stable organizing. High turnover fragments union representation, franchising structure diffuses responsibility (franchisees blame corporate pricing; corporate blames franchise economics), and the tip system creates internal wage stratification (tipped service staff have different interests than back-of-house kitchen workers). Coalition attempts generate snare classification because organizing barriers are deliberately maintained, but the organized status moderates experienced extraction relative to individual trapped workers.
constraint_indexing:constraint_classification(food_worker_wage_suppression, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POLICY INTERVENTIONIST (SCAFFOLD) — Sees the constraint as a temporary policy failure amenable to legislative remedy: minimum wage increases, portable benefits, scheduling predictability mandates, tip pooling rules. Experiences the constraint as high suppression but with a visible sunset: 10-15 year timeline for policy maturation. Exit path is explicit: state-level wage floors, federal labor standards, or sectoral collective bargaining frameworks. Theater is moderate (some compliance theater around minimum wage enforcement) but declining as digital documentation makes evasion harder.
constraint_indexing:constraint_classification(food_worker_wage_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: TIPPING SYSTEM (PITON) — The tipping mechanism is institutionally degraded: originally a mechanism to avoid fixed wage costs during economic uncertainty, it has persisted as performative theater maintaining wage suppression long after full employment alternatives exist. Establishments practice tip-credit regulations (paying $2.13/hour in some US states) despite stable, profitable business models that could absorb full wages. The system is maintained through inertia and cultural narrative ('tipping is how servers earn'), not functional necessity. Theater ratio is high—establishments perform their commitment to 'server-driven income' while actively suppressing base wages.
constraint_indexing:constraint_classification(food_worker_wage_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, some wage suppression in low-skill service work appears as an immutable natural law: marginal productivity of entry-level labor is low, competition is high, and therefore wages compress toward subsistence. However, this naturalizes contingent institutional choices (tip-credit legality, immigration policy, franchising liability exemptions, union busting). The engine's false summit detector will identify this as misclassification: labor market structure is engineered, not inevitable.
constraint_indexing:constraint_classification(food_worker_wage_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(food_worker_wage_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(food_worker_wage_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(food_worker_wage_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(food_worker_wage_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(food_worker_wage_suppression, TR),
    TR >= 0.70.

:- end_tests(food_worker_wage_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting significant asymmetric benefit to operators/corporate entities. The value created by coordinated food preparation and service delivery far exceeds worker compensation; the gap is captured by operators and franchisors. The 0.68 value reflects the strength of institutional mechanisms (franchising liability, tip legality, labor saturation) that enable this extraction. Suppression (0.75): Very high. Workers face multiple overlapping barriers to exit: lack of transferable skills (industry-specific training not readily recognized elsewhere), geographic immobility (low wages insufficient for relocation), immediate survival need (no savings buffer), education barriers (cost/time to credential-switch), immigration status constraints (where applicable), and family obligations (childcare, dependents). These are not temporary market frictions but deliberately maintained structural features. Theater ratio (0.55): Moderate, reflecting the blend of genuine coordination function (service delivery requires coordinated labor) and performative compliance (tip-credit compliance theater, minimum wage enforcement theater, corporate social responsibility narratives about 'developing workforce talent'). The theater ratio has increased over time as compliance documentation has become elaborate without materially changing suppression mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival divergence is between the operator view (coordination problem: stable labor supply) and the worker view (extraction trap: systematic wage suppression). Both are structurally correct—the constraint genuinely coordinates labor supply while genuinely extracting surplus. The gap reveals this as a legitimate Tangled Rope scenario viewed from aggregate level, but the aggregate masks internal asymmetry: operators experience it as Rope (pure benefit), workers experience it as Snare (pure harm). The piton perspective (tipping system as degraded ritual) identifies that the coordination function could be maintained with different institutional structure (full wages, predictable hours, professional training) but the current structure is maintained because suppression mechanisms are legally protected and culturally normalized.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from structural position relative to the extraction flow. Entry-level workers (powerless, trapped) experience high d (victim status, no exit → d ≈ 0.95, f(d) ≈ 1.42), producing high chi and snare classification. Restaurant operators (institutional, arbitrage) experience low d (beneficiary status, exit options abundant → d ≈ 0.05, f(d) ≈ -0.12), producing negative chi and rope classification. Career workers (moderate, constrained) occupy middle ground (d ≈ 0.60, f(d) ≈ 0.85), producing tangled rope where coordination and extraction are both present. Unions (organized, constrained) have intermediate d (multiple victims but some collective power → d ≈ 0.50, f(d) ≈ 0.65) producing snare despite organized status because the constraint is deliberately fragmented to prevent coalition effectiveness. Policy reformers (organized, mobile) have medium-low d because they have exit and adaptive capacity, producing scaffold (institutional intervention has power to change constraint structure).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between genuine coordination function and extractive institutional design. The food service sector genuinely requires coordinated labor—workers must show up at consistent times, coordinate in the kitchen and front of house, respond to unpredictable demand. A Rope classification (pure coordination) would be inappropriate because the constraint explicitly suppresses wages below market-clearing levels and structures employment to prevent worker exit. A Snare classification (pure extraction) would be incomplete because restaurant operations genuinely benefit from stable labor availability and consistent service quality—workers do create real value that is being suppressed. The Tangled Rope classification at aggregate level correctly captures this hybrid. However, the perspectival divergence is crucial: for entry-level workers, the constraint functions as a pure Snare (no genuine beneficiary function for them); for operators, it functions as pure Rope. The mandatrophy resolution requires declaring the beneficiary (operators, corporate entities) and victim groups (food workers, household economic stability) explicitly, which reveals that the classification varies by structural position: the constraint is mixed only from operators' perspective; from workers' perspective, it is extractive. The theater increase over time (38% → 55%) indicates metric substitution drift (Goodhart's law applied to labor): as wage suppression faces political pressure, institutions increase compliance theater (wage reporting, scheduling apps, worker development rhetoric) without material change to underlying extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_transferability_measurement,
    'How much of the observed wage suppression results from genuinely low transferable skill content versus from institutional barriers that artificially restrict worker mobility?',
    'Longitudinal tracking of worker wage trajectories post-exit from food service; comparison of entry-level food worker wages to other low-skill service sectors with different institutional structures (e.g., grocery retail, automotive service); analysis of skill recognition in adjacent industries',
    'If transferability is low: wage suppression reflects actual productivity constraints, and classification should shift toward Rope or Tangled Rope at higher agent power. If transferability is high but institutional barriers block mobility: suppression is extractive choice, supporting Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_transferability_measurement, empirical, 'Degree to which wage suppression reflects skill limitations vs institutional mobility barriers').

omega_variable(
    tip_system_welfare_function,
    'Does the tip system actually improve aggregate worker welfare (through higher variable compensation) compared to full-wage models, or does it reduce welfare through income volatility and employer discretion?',
    'Comparative wage analysis (base pay + mean tips vs full-wage restaurants); volatility analysis (income variance for tipped vs non-tipped workers); survey data on actual worker preferences when offered tip-free guaranteed wages',
    'If tips genuinely enhance welfare: tipping is coordination mechanism (Rope) with distributional concerns rather than pure extraction (Snare). If tips reduce welfare: tipping is performative theater maintaining suppression, supporting Piton diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tip_system_welfare_function, empirical, 'Whether tipping system improves or reduces aggregate worker welfare').

omega_variable(
    franchise_liability_externality,
    'Does the franchising liability structure (corporate entity legally separated from franchisee labor practices) create an externality that enables wage suppression not present in integrated corporate chains?',
    'Wage comparison between franchised and company-operated locations in same chain; analysis of whether franchisor wage guidance exists and enforcement mechanisms; comparison of wage compliance rates across franchise tiers',
    'If franchising enables suppression: the constraint is partly institutional design choice (corporate liability structure) rather than market outcome. Increases snare diagnosis. If wage patterns identical across structures: franchising is neutral to suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_liability_externality, empirical, 'Whether franchise liability structure enables wage suppression').

omega_variable(
    immigration_policy_suppression_coupling,
    'To what extent does wage suppression depend on immigration policy that constrains worker supply elasticity? Would eliminating immigration restrictions alone relieve the suppression?',
    'Cross-country wage comparisons (high-immigration countries vs low-immigration countries, controlling for development level); historical analysis of wage changes following visa policy shifts; labor-market modeling of elasticity under different immigration regimes',
    'If immigration is primary mechanism: constraint is partly policy-contingent with policy-level sunset potential. If suppression persists despite unrestricted immigration: mechanism is structural to labor market organization itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immigration_policy_suppression_coupling, empirical, 'Extent to which immigration policy drives wage suppression mechanism').

omega_variable(
    consumer_demand_elasticity_wage,
    'If food service businesses increased menu prices to fund full-wage (non-tipped) model, how much would demand elasticity decline? Is wage suppression economically necessary or price-preference-dependent?',
    'Natural experiments: case studies of restaurants shifting from tip to no-tip models; elasticity analysis of price increases when accompanied by wage-transparency messaging; consumer survey willingness-to-pay for guaranteed-wage service',
    'If elasticity is inelastic (price increase sustainable): wage suppression is rent extraction not economic necessity. If elasticity is elastic (demand collapses): suppression reflects genuine consumer-demand constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_demand_elasticity_wage, empirical, 'Whether price elasticity of demand permits full-wage models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(food_worker_wage_suppression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(foodwage_tr_t0, food_worker_wage_suppression, theater_ratio, 0, 0.38).
narrative_ontology:measurement(foodwage_tr_t10, food_worker_wage_suppression, theater_ratio, 10, 0.48).
narrative_ontology:measurement(foodwage_tr_t20, food_worker_wage_suppression, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(foodwage_be_t0, food_worker_wage_suppression, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(foodwage_be_t10, food_worker_wage_suppression, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(foodwage_be_t20, food_worker_wage_suppression, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(food_worker_wage_suppression, resource_allocation).
narrative_ontology:affects_constraint(food_worker_wage_suppression, household_economic_precarity).
narrative_ontology:affects_constraint(food_worker_wage_suppression, low_skill_labor_market_saturation).

% DUAL FORMULATION NOTE:
% Food worker wage suppression is downstream of broader low-skill labor market saturation and upstream of household economic precarity and food insecurity. The constraint family decomposes into wage suppression (institutional, operator-controlled), labor market saturation (demographic, policy-driven), and household outcomes (individual, context-dependent). Wage suppression has distinct ε (~0.68) from labor saturation (ε ~0.35, more rope-like) because suppression adds institutional control layers beyond supply-side saturation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(food_worker_wage_suppression, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
