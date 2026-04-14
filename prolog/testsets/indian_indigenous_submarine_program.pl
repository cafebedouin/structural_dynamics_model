% ============================================================================
% CONSTRAINT STORY: indian_indigenous_submarine_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_indigenous_submarine_program, []).

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
 *   constraint_id: indian_indigenous_submarine_program
 *   human_readable: Indian Indigenous Submarine Program Coordination and Extraction
 *   domain: defense/maritime_security/technology_development
 *
 * SUMMARY:
 *   The Indian Indigenous Submarine Program (Project 75, Project 75I)
 *   represents a strategic commitment to indigenous naval capability
 *   development. Launched in the 1990s with Russian technical cooperation,
 *   the program coordinates genuine defense capability development
 *   (submarines deliver strategic deterrent value) alongside substantial
 *   extraction through cost inflation, schedule delays, technology transfer
 *   asymmetries, and political theater around 'indigenization.' The
 *   constraint exhibits tangled rope structure: both coordination (building
 *   indigenous defense capacity, learning technology) and extraction
 *   (excessive costs, delayed timelines, asymmetric technology dependency)
 *   operate simultaneously. The program's extractiveness has increased over
 *   15 years (from 0.35 to 0.52) as cost overruns accumulated, while theater
 *   ratio increased (from 0.42 to 0.58) as gap between claimed progress and
 *   actual capability widened. The program demonstrates how national
 *   strategic autonomy narratives can mask institutional extraction
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Indian Taxpayers: Primary victims (powerless/trapped) — bear costs through taxation with no exit option or benefit participation
 *   - Naval Operational Command: Secondary victims (moderate/constrained) — depend on submarine capability but lose autonomy over technical/timeline decisions; bear operational risk during delays
 *   - Defense Ministry/Program Administration: Primary beneficiary (institutional/arbitrage) — controls program scope, budget, timelines, success metrics; maintains political narrative regardless of technical outcomes
 *   - Mazagon Dock Shipbuilders (MDL) and HAL: Prime contractors (institutional/constrained) — benefit from guaranteed contracts and technological learning but constrained by performance requirements and political pressure
 *   - Foreign Technology Providers (Russia, France, Germany, Israel): Powerful beneficiaries (powerful/arbitrage) — maintain lucrative support contracts and dependency relationships while 'indigenous' framing obscures their role
 *   - Strategic Autonomy Coalition: Organized beneficiaries (organized/mobile) — perceive program as temporary scaffold toward genuine indigenous capacity; exit path in technology mastery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_indigenous_submarine_program, 0.52).
domain_priors:suppression_score(indian_indigenous_submarine_program, 0.48).
domain_priors:theater_ratio(indian_indigenous_submarine_program, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_indigenous_submarine_program, extractiveness, 0.52).
narrative_ontology:constraint_metric(indian_indigenous_submarine_program, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(indian_indigenous_submarine_program, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_indigenous_submarine_program, tangled_rope).
narrative_ontology:human_readable(indian_indigenous_submarine_program, "Indian Indigenous Submarine Program Coordination and Extraction").
narrative_ontology:topic_domain(indian_indigenous_submarine_program, "defense/maritime_security/technology_development").

domain_priors:requires_active_enforcement(indian_indigenous_submarine_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_indigenous_submarine_program, defense_industrial_complex).
narrative_ontology:constraint_beneficiary(indian_indigenous_submarine_program, submarine_program_elite).
narrative_ontology:constraint_beneficiary(indian_indigenous_submarine_program, strategic_autonomy_narrative).
narrative_ontology:constraint_victim(indian_indigenous_submarine_program, taxpayers).
narrative_ontology:constraint_victim(indian_indigenous_submarine_program, naval_operational_readiness).
narrative_ontology:constraint_victim(indian_indigenous_submarine_program, technology_transfer_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Powerless agents funding submarine development bear costs through taxation with no exit option. Cannot influence program decisions, cannot access strategic benefits, cannot verify expenditure claims. Trapped in funding obligation regardless of program success or timeline. Maximum extraction from this perspective.
constraint_indexing:constraint_classification(indian_indigenous_submarine_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Navy depends on submarines for maritime security but constrained by budget allocation decisions made by civilian defense ministry. Faces choice between accepting delayed/problematic indigenous submarines or importing foreign systems (politically costly). High extraction as navy bears operational risk while losing autonomy over technical decisions. Constrained rather than trapped due to theoretical ability to appeal decisions.
constraint_indexing:constraint_classification(indian_indigenous_submarine_program, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Institutional beneficiary with high arbitrage options. Controls program scope, timeline, budget allocation, and political narrative around 'indigenous' capacity. Can redirect funds, adjust timelines, or redefine success metrics without external accountability. Experiences constraint as coordination mechanism enabling strategic autonomy narrative regardless of technical outcomes.
constraint_indexing:constraint_classification(indian_indigenous_submarine_program, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Institutional actor with constrained exit (dependent on government contracts but theoretically mobile to private sector). Benefits from guaranteed funding and technological learning, but constrained by performance requirements, technical complexity, and political pressure. Genuine coordination function (actually building submarines) alongside extraction (cost overruns, schedule delays, learning asymmetries). Active enforcement through contract management and political oversight.
constraint_indexing:constraint_classification(indian_indigenous_submarine_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Powerful actors (Russia, France, Germany, Israel) provide subsystems, technical assistance, and IP licensing. Maintain arbitrage options and benefit from Indian dependence despite 'indigenous' framing. The program's claim of indigenization masks continued foreign dependency in critical subsystems (reactors, combat management systems, sonar). Theater ratio high: public narrative emphasizes Indian design while technical reality requires substantial foreign support. Piton classification reflects degradation of actual indigenous capacity claims by institutional inertia.
constraint_indexing:constraint_classification(indian_indigenous_submarine_program, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized agents (government planners, nationalist discourse, technology independence advocates) perceive the program as temporary scaffolding toward genuine indigenous capacity. Exit path visible: once technical mastery achieved, dependence on foreign subsystems ends. Sunset implicit in program logic (though not explicitly declared): 20-30 year horizon for Indian yard to achieve full design and manufacturing capability. Beneficiaries from this perspective experience constraint as coordination mechanism with declining extraction over generational timescale.
constraint_indexing:constraint_classification(indian_indigenous_submarine_program, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% From civilizational scope, the program demonstrates genuine coordination function (building maritime deterrent, developing indigenous defense capacity) alongside systematic extraction (cost inflation, schedule slippage, technology transfer asymmetries, political theater around 'indigenization'). The constraint requires active enforcement through political pressure and budgetary commitment. Base extraction (0.52) reflects substantial costs borne by taxpayers and navy, but program delivers some real strategic capability (coordination benefit). Not a pure snare because indigenous capacity does materialize; not pure rope because extraction component is significant and asymmetric.
constraint_indexing:constraint_classification(indian_indigenous_submarine_program, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_indigenous_submarine_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_indigenous_submarine_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_indigenous_submarine_program, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_indigenous_submarine_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_indigenous_submarine_program, TR),
    TR >= 0.70.

:- end_tests(indian_indigenous_submarine_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. Taxpayer funding flows into program with uncertain ROI; cost overruns inflate extraction; delayed timelines impose strategic costs on navy; technology transfer remains asymmetric with foreign suppliers. However, extraction is not total because submarines do materialize and provide genuine strategic capability — the constraint solves a real coordination problem (defense capability) alongside extractive overhead. Suppression (0.48): Moderate. Significant barriers include classification of technical details, political pressure to maintain 'indigenous' narrative despite foreign dependency, and asymmetric information about costs and timelines. But suppression is not total — parliamentary oversight exists, media reports delays, and some technical information circulates. Theater ratio (0.58): Moderately high. Public narrative emphasizes Indian design and indigenous capacity; reality involves substantial foreign technical support in critical subsystems. The program celebrates milestones that represent marginal progress; cost projections are revised repeatedly; capacity claims exceed demonstrated capability. Theater has increased as timeline slippages mounted.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal: from taxpayer's Snare perspective (trapped in extraction, no coordination benefit) to defense ministry's Rope perspective (coordination enabling strategic autonomy). The navy occupies middle ground: Snare-like extraction through operational risk, but gaining eventual strategic benefit from submarine capability. The contractor experiences Tangled Rope: both learning (coordination benefit) and performance pressure (extraction). The foreign provider sees Piton: their role is degraded by 'indigenous' framing but they maintain lucrative support contracts through institutional inertia. The analytical observer's Tangled Rope recognizes that both mechanisms operate — the program genuinely develops indigenous capacity AND extracts through inefficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (defense ministry, contractors, foreign suppliers) have low directionality values (d ≈ 0.10-0.30): institutional power with arbitrage options means they experience the constraint as coordination/opportunity rather than extraction. Victims (taxpayers, navy) have high directionality values (d ≈ 0.85-1.0): powerless/moderate power with trapped/constrained exit means they experience full extraction flow. The navy's constrained exit (can theoretically appeal but faces political pressure to accept program) places its d between trapped and mobile. The prime contractors' constrained exit (dependent on government contracts but theoretically mobile) produces moderate d. This directionality distribution explains why the program persists despite taxpayer/navy costs: institutional beneficiaries experience coordination while distributed powerless agents experience extraction and lack coordination mechanism to organize resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by distinguishing the coordination function (defense capability development) from the extraction mechanism (cost inflation, technology transfer asymmetry, delayed timelines). The program is not pure extraction (Snare) because submarines do materialize and provide strategic value. It is not pure coordination (Rope) because significant costs flow from powerless agents to institutional beneficiaries with no reciprocal benefit to taxpayers. Tangled Rope is the correct classification: genuine coordination function (building indigenous defense capacity) combined with asymmetric extraction (costs and risks concentrated on powerless/moderate agents while benefits concentrate on institutional actors). The scaffold perspective legitimately identifies a sunset clause (technology mastery timeline), but this is aspirational rather than contractually enforced — the program's institutional structure creates incentives to extend rather than complete the technology transfer. Mandatrophy is resolved by acknowledging that the program solves both a real coordination problem (maritime deterrent) and sustains an extractive structure (cost/risk/benefit asymmetry), with tension between these mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenization_threshold_ambiguity,
    'What percentage of subsystem indigenous content constitutes genuine ''indigenous'' submarine design vs. foreign-designed platform with Indian assembly?',
    'Technical audit of critical subsystems (reactor, combat management system, sonar, propulsion); IP ownership analysis; comparison with foreign platforms using equivalent components',
    'If threshold < 30% indigenous: program is extraction mechanism masquerading as autonomy; reclassify as Snare from analytical perspective. If threshold > 70% indigenous: scaffold sunset is real; maintain Tangled Rope with declining extraction trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenization_threshold_ambiguity, empirical, 'Definition and measurement of ''indigenous'' submarine capacity').

omega_variable(
    technology_transfer_asymmetry,
    'Does foreign technical assistance during submarine development create permanent dependency on foreign support or genuine knowledge transfer enabling independence?',
    'Longitudinal tracking of foreign technical personnel in Indian yards; patent and IP ownership over time; capability to design next-generation platform without foreign technical input; comparison of Indian-led vs foreign-supervised design decisions',
    'If asymmetric dependency: extraction persists indefinitely; piton perspective dominates. If genuine transfer: capacity develops; scaffold sunset becomes real timeline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_transfer_asymmetry, empirical, 'Whether foreign technical assistance enables or maintains dependency').

omega_variable(
    operational_readiness_delay_cost,
    'How much operational security loss results from 15-20 year development delays compared to importing submarines, and who bears this cost?',
    'Naval threat assessment modeling; comparison of maritime capability gap during development period vs post-completion capability; analysis of strategic advantages foregone',
    'If delay cost exceeds benefit of indigenous capacity: extraction primarily flows from taxpayers to defense industrial elite. If delay cost acceptable by strategic timeline: extraction is justified coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_readiness_delay_cost, empirical, 'Strategic cost of submarine development delays vs imported alternatives').

omega_variable(
    program_theater_ratio_driver,
    'Is high theater ratio (0.58) driven by genuine technical complexity requiring management theater, or by institutional incentives to obscure cost overruns and delays?',
    'Comparison of theater metrics (public announcements, milestone celebrations, technical detail disclosure) against actual technical progress; tracking of revised timelines and budget estimates; frequency of technical vs political problem announcements',
    'If institutional incentive-driven: theater is extraction mechanism; reclassify from Tangled Rope toward Snare. If complexity-driven: theater reflects legitimate coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_theater_ratio_driver, empirical, 'Whether high theater ratio reflects technical complexity or institutional opacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_indigenous_submarine_program, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, indian_indigenous_submarine_program, theater_ratio, 0, 0.42).
narrative_ontology:measurement(indi_tr_t8, indian_indigenous_submarine_program, theater_ratio, 8, 0.55).
narrative_ontology:measurement(indi_tr_t15, indian_indigenous_submarine_program, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, indian_indigenous_submarine_program, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(indi_be_t8, indian_indigenous_submarine_program, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(indi_be_t15, indian_indigenous_submarine_program, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_indigenous_submarine_program, resource_allocation).
narrative_ontology:affects_constraint(indian_indigenous_submarine_program, defense_procurement_cost_inflation).
narrative_ontology:affects_constraint(indian_indigenous_submarine_program, technology_transfer_asymmetry_india).

% DUAL FORMULATION NOTE:
% The submarine program decomposes into multiple structurally distinct constraints: (1) program-level resource allocation (this story, ε=0.52, Tangled Rope) — how taxpayer funds flow into submarine development; (2) foreign dependency constraint (ε=0.68, Snare-type) — asymmetric technology transfer relationships with foreign providers; (3) naval operational readiness coordination (ε=0.35, Rope-type) — genuine maritime security coordination function. These three stories are linked: the upstream foreign dependency story explains the program-level extraction; the downstream naval readiness story shows the coordination benefit realized. The program-level story is the middle node connecting foreign dependency asymmetries to operational benefit realization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_indigenous_submarine_program, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
