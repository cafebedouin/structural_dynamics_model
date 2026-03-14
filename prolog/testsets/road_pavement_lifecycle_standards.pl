% ============================================================================
% CONSTRAINT STORY: road_pavement_lifecycle_standards
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_road_pavement_lifecycle_standards, []).

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
 *   constraint_id: road_pavement_lifecycle_standards
 *   human_readable: Road Pavement Lifecycle Standards and Maintenance Extraction
 *   domain: infrastructure_policy/civil_engineering
 *
 * SUMMARY:
 *   Road pavement lifecycle standards (typically mandating resurfacing every
 *   15-25 years depending on jurisdiction) create a structural tension
 *   between legitimate infrastructure coordination and extractive industry
 *   incentives. The standards specify when pavements must be resurfaced based
 *   primarily on age rather than condition, creating guaranteed work streams
 *   for contractors and manufacturers. While age-based standards reduced
 *   uncertainty in infrastructure planning during the 20th century when
 *   monitoring capacity was limited, modern pavement condition monitoring
 *   technology makes rigid lifecycle standards obsolete. The constraint
 *   exhibits tangled coordination (reducing planning uncertainty for
 *   municipalities and providing equipment manufacturers with predictable
 *   demand) alongside extraction (commuters bear costs of premature
 *   resurfacing; taxpayers fund unnecessary replacement). The theater ratio
 *   of 0.68 reflects that inspection protocols are increasingly performative:
 *   pavement rated as 'failed' based on chronological age rather than
 *   measured condition. Extractiveness has increased from 0.38 to 0.52 over
 *   the interval as standards have been enforced more strictly and monitoring
 *   technology has revealed how often age-based scheduling diverges from
 *   actual pavement condition. The rising theater ratio (0.52 to 0.68)
 *   indicates that the gap between functional and performative maintenance
 *   has widened — agencies continue enforcing standards while acknowledging
 *   they no longer represent optimal engineering practice.
 *
 * KEY AGENTS:
 *   - Commuters and Vehicle Operators: Primary victims (powerless/trapped) — bear vehicle damage costs, fuel inefficiency, and repair expenses from poor surface conditions during resurfacing cycles
 *   - Taxpayers: Primary victims (powerless/trapped) — mandatory funding for resurfacing regardless of pavement condition; cannot opt out of road system funding
 *   - Asphalt Contractors: Primary beneficiaries (institutional/arbitrage) — receive guaranteed work streams from age-based resurfacing; can arbitrage between jurisdictions with different enforcement
 *   - Aggregate Suppliers and Equipment Manufacturers: Secondary beneficiaries (powerful/mobile) — standardized specifications create predictable demand; can arbitrage globally
 *   - Municipal Transportation Engineers: Mixed agent (moderate/constrained) — genuinely coordinate with standards but also experience extraction through inflexible replacement schedules
 *   - State Department of Transportation: Institutional actor (institutional/arbitrage) — maintains standards through inertia; sees own inspection protocols as performative but continues enforcement
 *   - Performance-Based Pavement Coalition: Organized alternative (organized/constrained) — researchers, forward-looking municipalities, technology providers advocating condition-based maintenance with sunset clause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(road_pavement_lifecycle_standards, 0.52).
domain_priors:suppression_score(road_pavement_lifecycle_standards, 0.58).
domain_priors:theater_ratio(road_pavement_lifecycle_standards, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(road_pavement_lifecycle_standards, extractiveness, 0.52).
narrative_ontology:constraint_metric(road_pavement_lifecycle_standards, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(road_pavement_lifecycle_standards, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(road_pavement_lifecycle_standards, tangled_rope).
narrative_ontology:human_readable(road_pavement_lifecycle_standards, "Road Pavement Lifecycle Standards and Maintenance Extraction").
narrative_ontology:topic_domain(road_pavement_lifecycle_standards, "infrastructure_policy/civil_engineering").

domain_priors:requires_active_enforcement(road_pavement_lifecycle_standards).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(road_pavement_lifecycle_standards, asphalt_contractors).
narrative_ontology:constraint_beneficiary(road_pavement_lifecycle_standards, aggregate_suppliers).
narrative_ontology:constraint_beneficiary(road_pavement_lifecycle_standards, equipment_manufacturers).
narrative_ontology:constraint_victim(road_pavement_lifecycle_standards, vehicle_operators).
narrative_ontology:constraint_victim(road_pavement_lifecycle_standards, public_taxpayers).
narrative_ontology:constraint_victim(road_pavement_lifecycle_standards, long_term_infrastructure_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUTER/TAXPAYER (SNARE) — Trapped between mandatory road usage and mandatory tax contributions. Cannot exit the road system. Bears all costs: vehicle damage from poor surfaces, fuel inefficiency, repair bills, and taxes that fund cycles of premature failure and repeated resurfacing. No alternatives in most jurisdictions. Maximum experienced extraction with no coordination benefit.
constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPHALT CONTRACTOR (ROPE) — Benefits from standards that mandate regular resurfacing cycles independent of actual pavement condition. Early replacement requirements create reliable revenue streams. Can arbitrage between jurisdictions with different enforcement levels. Sees the constraint as coordination mechanism: standards specify project scope, timing, and material requirements. Net beneficiary experiencing minimal extraction.
constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: MUNICIPAL ENGINEER (TANGLED ROPE) — Constrained by state-mandated pavement lifecycle standards that specify resurfacing intervals. Experiences genuine coordination: standards eliminate uncertainty about acceptable maintenance schedules. But also experiences extraction: standards often require more frequent (and expensive) resurfacing than structural condition warrants. Career pressures align with compliance over cost optimization. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PERFORMANCE-BASED COALITION (SCAFFOLD) — Organized movement toward condition-based maintenance (measuring actual surface condition rather than age) represents sunset clause for rigid lifecycle standards. Sees standards as temporary scaffolding that enabled systematic maintenance but should decline as data-driven monitoring improves. Coalition includes transportation researchers, forward-looking municipalities, and technology providers developing pavement sensors. Sunset timeline: 15-25 years for condition-based systems to replace age-based standards.
constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE DOT (PITON) — State transportation agencies maintain rigid pavement lifecycle standards nominally for safety and longevity but functionally through institutional inertia. The standards were created decades ago when data collection was poor and engineering uncertainty was high. Modern pavement monitoring technology makes age-based standards obsolete, yet agencies continue enforcing them. Theater ratio is high: inspection protocols are performative (inspectors rate pavements as 'failed' based on age-arrived-at rather than measured condition). Standards persist because changing them requires political capital and threatens contractor relationships, not because they optimize infrastructure outcomes.
constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EQUIPMENT MANUFACTURER (TANGLED ROPE) — Large manufacturers of resurfacing equipment and materials benefit from standardized replacement cycles (predictable demand). But also coordinate supply chains and equipment specifications globally. Extraction is moderate because manufacturers have mobility (can shift supply to different markets) and genuine role in coordinating procurement and equipment standards. Experience is mixed: real coordination function plus extraction through standardized specifications favoring established products.
constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a naive engineering perspective, pavement has inherent degradation rates and lifecycles that cannot be escaped — materials fail, surfaces crack, bases weaken. This perspective sees lifecycle standards as describing immutable properties of asphalt itself. However, the structural data (extractiveness 0.52, suppression 0.58, theater 0.68) contradicts pure naturalization. Modern condition-based monitoring demonstrates that pavement lifecycles are not fixed properties but contingent on use, climate, and maintenance strategy. This is a false summit: the constraint is institutional (enforceable standards) naturalizing what appears to be a law of materials.
constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(road_pavement_lifecycle_standards_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(road_pavement_lifecycle_standards, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(road_pavement_lifecycle_standards, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(road_pavement_lifecycle_standards, TR),
    TR >= 0.70.

:- end_tests(road_pavement_lifecycle_standards_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Age-based lifecycle standards create guaranteed work streams for contractors independent of actual pavement condition, representing extraction from commuters and taxpayers. However, extractiveness is not extreme because legitimate coordination benefits exist: standards eliminate uncertainty in maintenance planning and enable long-term budget forecasting. The constraint has become more extractive over time (0.38→0.52) because pavement monitoring technology now reveals systematic overtreatment — many pavements rated as 'failed' by age criteria are structurally sound. Suppression (0.58): Moderate-high. Commuters cannot exit road systems; taxation is mandatory. Municipal engineers face state mandates and contractor relationships that constrain alternative approaches. But suppression is not total — some jurisdictions are piloting condition-based systems, and research alternatives exist. Theater ratio (0.68): High and rising. Inspection protocols are increasingly performative: inspectors apply age-based failure criteria rather than measuring actual surface condition. As condition monitoring technology improves, the gap between performative inspection (visual assessment using age cutoffs) and actual measurement widens. The theater has increased over the interval as the gap between what standards pretend to measure (objective pavement condition) and what they actually measure (calendar age) has become visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a sharp perspectival divide between beneficiaries and victims. Asphalt contractors and equipment manufacturers see the standards as pure coordination (Rope perspective) — they describe lifecycle standards as necessary predictability mechanisms. Commuters and taxpayers see pure extraction (Snare perspective) — mandatory funding for unnecessary resurfacing with no exit option. Municipal engineers experience the mixed reality (Tangled Rope perspective) — standards do coordinate planning but override cost optimization. The organized performance-based coalition sees a temporary problem with a sunset (Scaffold perspective) — condition-based systems are emerging to replace age-based rigidity. The state DOT sees its own standards as degraded (Piton perspective) — maintaining them through institutional inertia despite knowing they no longer optimize outcomes. The analytical observer risks naturalizing the standards as inherent properties of pavement degradation (Mountain perspective), but structural data reveals this as a false summit: the constraint is institutional, not physical.
 *
 * DIRECTIONALITY LOGIC:
 *   Commuters and taxpayers derive high d (→0.95) from being trapped victims with no exit options. Contractors derive low d (→0.10) from being beneficiaries with arbitrage options. Municipal engineers derive moderate d (→0.55) from being constrained victims who also benefit from coordination. Equipment manufacturers derive low-moderate d (→0.35) from being beneficiaries with global mobility. State DOT derives institutional d (→0.05) from beneficiary status (standards justify agency existence) and arbitrage options (can modify standards). The piton classification derives from the theater gate: state DOT maintains standards that it perceives as degraded, indicating performative enforcement rather than functional optimization.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing how the same institutional arrangement (pavement lifecycle standards) serves genuine coordination functions while enabling extraction. The standards coordinate municipal budget planning, equipment procurement, and contractor workflows — real benefits that justify some rigidity. But they also extract through premature resurfacing, unnecessary taxpayer spending, and commuter vehicle damage costs — real harms that contradict pure coordination. The mandatrophy is resolved by the tangled_rope classification: both elements are structurally authentic. The rising extractiveness (0.38→0.52) and rising theater (0.52→0.68) indicate that the coordination function is degrading while extraction persists — the ratio between real benefit and pure cost is shifting unfavorably. The scaffold perspective (condition-based sunset) reveals why the constraint is not a permanent snare: genuine alternatives exist and are technically feasible, meaning the extraction is contingent on institutional inertia rather than structural inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    condition_measurement_feasibility,
    'Can condition-based monitoring systems (pavement sensors, automated imaging, acoustic testing) actually measure pavement condition with sufficient accuracy and cost-effectiveness to replace age-based lifecycle standards?',
    'Deployment pilot programs comparing condition-based vs age-based maintenance scheduling; cost and accuracy analysis of monitoring technologies across diverse climate and traffic regimes',
    'If feasible: scaffold sunset is real and timeline can be estimated. If infeasible: condition-based coalition is aspirational and lifecycle standards will persist as piton rather than temporary scaffolding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(condition_measurement_feasibility, empirical, 'Whether condition-based monitoring can replace age-based standards').

omega_variable(
    optimal_resurfacing_interval,
    'What is the economically and structurally optimal resurfacing interval for pavement given specific climate, traffic, and base conditions? Does it match current state-mandated lifecycle standards or deviate systematically?',
    'Life-cycle cost analysis comparing age-based maintenance schedules to condition-based recommendations; analysis of long-term infrastructure condition data across jurisdictions with different standards',
    'If optimal interval > mandated interval: standards create unnecessary extraction. If optimal < mandated: standards provide genuine safety margin. If heterogeneous: one-size-fits-all standards are always suboptimal and extraction varies by jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_resurfacing_interval, empirical, 'Optimal resurfacing interval vs mandated standards').

omega_variable(
    contractor_influence_on_standards,
    'To what degree are state-mandated pavement lifecycle standards shaped by contractor lobbying vs independent engineering analysis?',
    'Historical analysis of standard-setting processes; comparison of standards across states with different regulatory structures and contractor concentration; expert elicitation of engineers'' versus contractors'' preferred intervals',
    'If contractor influence > 40%: standards are primarily extraction mechanisms (snare/tangled_rope from commuter perspective is accurate). If < 20%: standards reflect genuine engineering consensus (rope/mountain perspectives gain credibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractor_influence_on_standards, empirical, 'Contractor influence on lifecycle standards').

omega_variable(
    pavement_quality_outcomes,
    'Do rigid age-based lifecycle standards produce better long-term pavement quality (fewer failures, longer asset life) compared to condition-based or minimal-intervention approaches?',
    'Comparative analysis of pavement condition data, failure rates, and long-term asset value across jurisdictions and time periods; controlled experiments with pilot programs using different maintenance strategies',
    'If age-based outperforms: justifies extraction as coordination cost (tangled_rope authenticity). If condition-based or minimal equal or exceed: standards extract without coordination benefit (snare/extraction diagnosis confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pavement_quality_outcomes, empirical, 'Quality outcomes: age-based vs condition-based pavement maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(road_pavement_lifecycle_standards, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rpls_tr_t0, road_pavement_lifecycle_standards, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rpls_tr_t10, road_pavement_lifecycle_standards, theater_ratio, 10, 0.62).
narrative_ontology:measurement(rpls_tr_t20, road_pavement_lifecycle_standards, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(rpls_be_t0, road_pavement_lifecycle_standards, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rpls_be_t10, road_pavement_lifecycle_standards, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rpls_be_t20, road_pavement_lifecycle_standards, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(road_pavement_lifecycle_standards, resource_allocation).
narrative_ontology:affects_constraint(road_pavement_lifecycle_standards, municipal_transportation_budgeting).
narrative_ontology:affects_constraint(road_pavement_lifecycle_standards, construction_industry_rent_seeking).

% DUAL FORMULATION NOTE:
% Road pavement lifecycle standards decompose into two structurally distinct constraints: (1) age-based scheduling (extractive, ε≈0.52) and (2) condition-based monitoring infrastructure (coordination, ε≈0.15). This story addresses the dominant age-based constraint. The monitoring alternative is a separate constraint that affects the sunset timeline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(road_pavement_lifecycle_standards, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
