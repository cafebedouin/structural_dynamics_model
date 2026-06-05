% ============================================================================
% CONSTRAINT STORY: labor_deskilling_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_deskilling_dynamics, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: labor_deskilling_dynamics
 *   human_readable: Labor Deskilling Dynamics in Industrial and Knowledge Work
 *   domain: labor_economics/organizational_control
 *
 * SUMMARY:
 *   Labor deskilling dynamics represent the systematic transformation of
 *   skilled work into standardized, interchangeable labor through the
 *   decomposition of tasks, formalization of procedures, and mechanization of
 *   decision-making. Beginning with Taylor's scientific management in the
 *   early 1900s and accelerating through industrial manufacturing,
 *   computerization, and now algorithm-driven task allocation, deskilling
 *   creates a hybrid coordination-extraction constraint. Management benefits
 *   from reduced wage bills, labor interchangeability, and control over
 *   production processes. Workers lose autonomy, discretion, and the market
 *   value of their expertise. The constraint exhibits genuine coordination
 *   function (standardization enables quality control, communication across
 *   sites, and scalability) alongside asymmetric extraction (wage
 *   suppression, autonomy loss, skill devaluation). The theater ratio has
 *   risen over time as the legitimating narrative ('scientific efficiency')
 *   persists despite contradictions from modern management methods (lean,
 *   kaizen, continuous improvement) that often require worker discretion and
 *   tacit knowledge.
 *
 * KEY AGENTS:
 *   - Capital Owners / Managers: Primary beneficiary (institutional/arbitrage) — reduce wage costs, improve labor control, achieve predictability and interchangeability
 *   - Skilled Workers / Craft Knowledge Bearers: Primary victim (powerless/trapped) — lose autonomy, face credential obsolescence, trapped in deskilled labor market
 *   - Deskilled Workers: Secondary victim (powerless/trapped) — structurally vulnerable to wage suppression, easily replaced, minimal bargaining power
 *   - Labor Unions / Worker Coalitions: Organized agents (organized/mobile) — can negotiate retraining, wage floors, and work protections; represent exit path for workers
 *   - Educational Institutions: Institutional (institutional/constrained) — shape labor supply toward standardized competencies; captured by employer demands but theoretically build human capital
 *   - Consumers: Analytical agents with mixed position — benefit from lower prices and standardized quality, but also bear externalized costs (job precarity, reduced income growth, community dislocation)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent management strategies as technological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_deskilling_dynamics, 0.58).
domain_priors:suppression_score(labor_deskilling_dynamics, 0.68).
domain_priors:theater_ratio(labor_deskilling_dynamics, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_deskilling_dynamics, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_deskilling_dynamics, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(labor_deskilling_dynamics, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_deskilling_dynamics, tangled_rope).
narrative_ontology:human_readable(labor_deskilling_dynamics, "Labor Deskilling Dynamics in Industrial and Knowledge Work").
narrative_ontology:topic_domain(labor_deskilling_dynamics, "labor_economics/organizational_control").

domain_priors:requires_active_enforcement(labor_deskilling_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_deskilling_dynamics, capital_owners_managers).
narrative_ontology:constraint_beneficiary(labor_deskilling_dynamics, standardization_process_architects).
narrative_ontology:constraint_victim(labor_deskilling_dynamics, skilled_workers).
narrative_ontology:constraint_victim(labor_deskilling_dynamics, craft_knowledge_bearers).
narrative_ontology:constraint_victim(labor_deskilling_dynamics, worker_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESKILLED WORKER (SNARE) — Once skilled workers lose their specialized knowledge to standardization, they face high barriers to exit. They cannot return to craft-based work; specialized roles are shrinking; lateral movement requires accepting lower wages. Trapped by credential obsolescence and labor market segmentation. The constraint extracts their autonomy and discretion, replacing it with monitoring and procedure compliance.
constraint_indexing:constraint_classification(labor_deskilling_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SKILLED CRAFT WORKER MID-TRANSITION (TANGLED ROPE) — Still possesses valuable tacit knowledge but faces real constraints: deskilling threatens the value of their expertise; regulatory standardization forces adoption of new procedures; wage pressure from cheaper deskilled labor. Yet they also benefit from some coordination: standardization protocols enable communication across sites, equipment manufacturers standardize tools, professional associations retain some gatekeeping power. Moderate extraction with genuine hybrid character.
constraint_indexing:constraint_classification(labor_deskilling_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: CAPITAL OWNER / MANAGEMENT (ROPE) — Experiences deskilling as pure coordination benefit. Standardization reduces wage bills, enables work interchangeability, reduces dependency on particular workers, and improves quality control. Management exits any single labor relationship at will; they benefit from reduced negotiation power of workers. The constraint solves their coordination problem (stable, controllable labor) while extracting from workers.
constraint_indexing:constraint_classification(labor_deskilling_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR UNION / WORKER COALITION (SCAFFOLD) — Organized labor sees deskilling as a temporary extractive phase, not permanent. Unions can negotiate retraining, wage floors, and work rule protections that offset deskilling pressure. Union presence creates an exit path: collective bargaining raises the cost of arbitrary deskilling. Where unions are strong, deskilling proceeds more slowly or is negotiated. Sunset logic applies: as worker organization grows, deskilling's extraction mechanism weakens. Temporary high suppression with declining extraction as organization builds.
constraint_indexing:constraint_classification(labor_deskilling_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TAYLORIST EFFICIENCY NARRATIVE (PITON) — The legitimating narrative ('scientific management improves efficiency') persists long after its functional justification. Modern manufacturing uses kaizen, lean methods, and data analytics that often contradict Taylor's 1911 prescriptions. Yet the theater of 'rational scientific organization' continues. The narrative is maintained through management schools, consulting firms, and performance metrics that assume task decomposition is inherently rational. Theater_ratio high because the performative claim ('we are being scientific') substitutes for actual skill development investment.
constraint_indexing:constraint_classification(labor_deskilling_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EDUCATIONAL SYSTEM (INSTITUTIONAL CAPTURE / TANGLED ROPE) — Educational institutions both enable and enforce deskilling. Curricula shift toward standardized competencies rather than deep craft knowledge; vocational education emphasizes job readiness for deskilled positions. Yet education also theoretically builds human capital. Educational institutions face real constraints: labor market demand for deskilled workers is high; employers lobby for standardized curricula; funding pressures favor quick job placement over long craft training. They benefit from deskilling (efficiency gains, employer alignment) while trapped in it (constrained by labor market signals). Genuine hybrid: coordination function exists (matching workers to jobs) but extraction is embedded (reinforcing the deskilling regime).
constraint_indexing:constraint_classification(labor_deskilling_dynamics, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECHNOLOGICAL DETERMINISM (MOUNTAIN) — From a civilizational view, deskilling can appear as an inevitable consequence of mechanization and automation: any technology that can standardize a task will eventually do so; workers cannot resist technological progress; deskilling is a natural law of industrial development. However, this perspective naturalizes contingent institutional choices (wage setting, training investment, union suppression) as technological inevitabilities. The mountain classification is vulnerable to false summit detection — historical variation (German apprenticeship systems, Japanese quality circles, Scandinavian worker councils) shows deskilling is not inherent to technology but to specific management strategies.
constraint_indexing:constraint_classification(labor_deskilling_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_deskilling_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_deskilling_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_deskilling_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_deskilling_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_deskilling_dynamics, TR),
    TR >= 0.70.

:- end_tests(labor_deskilling_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Initial deskilling in manufacturing (1900-1960) was partially offset by strong unions and rising real wages despite skill loss. Contemporary deskilling (1980-present) in services and knowledge work shows higher extraction: service sector workers face deskilling with weak union presence, gig economy workers have minimal protections, and algorithmic management (food delivery, warehouse work) removes human discretion entirely. The upward trajectory reflects weakening countervailing power, not intensification of the underlying mechanism. Suppression (0.68): High and structural. Barriers to exit include: labor market segmentation (deskilled credentials not recognized in other sectors), wage dependence (workers cannot sustain themselves through retraining), cognitive capture (internalized belief that standardized work is 'modern progress'), geographic lock-in (limited opportunities in some regions), and direct union suppression in some sectors. Theater ratio (0.64): Moderate-high, increasing. Taylorist rationality narrative persists in management education despite contradictions from kaizen, lean, and quality circles—all of which actually require worker discretion and tacit knowledge. Corporate culture emphasizes 'empowerment' while implementing algorithmic control. Metrics obsession (time-motion studies reborn as productivity dashboards) maintains the performance of rationality. The theater has increased because the legitimating narrative must work harder to justify extraction as work has become more cognitively complex.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival range spans from snare to rope, with tangled rope as the equilibrium classification. The gap between manager perception (pure beneficial coordination) and worker perception (pure extraction) is maximal. The skilled worker's tangled rope perspective reveals the constraint's hybrid nature: standardization does solve some real coordination problems (task communication, quality control) while simultaneously enabling extraction (wage suppression, autonomy loss). The scaffold perspective from organized labor is structural reality in union sectors and aspirational in non-union sectors, revealing a geographic and sectoral split: where unions are strong, deskilling is temporary; where unions are weak, deskilling becomes permanent snare. The piton perspective on Taylorist narrative reveals the cognitive maintenance burden: the 'scientific efficiency' story must constantly be re-told and re-legitimated because modern manufacturing (lean, kaizen, six sigma, AI) actually contradicts Taylor's prescriptions. The false mountain risk is high: deskilling is easily naturalized as 'inevitable progress' or 'technological necessity' when it is actually a contingent strategic choice by capital to increase control and reduce labor costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Capital owners/managers as beneficiaries with arbitrage-level exit options (they can replace any worker, relocate production, or invest in automation) derive low d values, producing negative f(d) and negative chi — they experience the constraint as beneficial coordination. Deskilled workers as victims with trapped exit options (credential obsolescence, labor market segmentation, wage dependence) derive high d values, producing high f(d) and high chi — they experience maximum extraction. Skilled workers in transition derive moderate d values through mixed beneficiary/victim status: they benefit from some standardization protocols but suffer knowledge devaluation. The organizational power atom matters: workers with individual power but no collective organization experience high extraction (powerless/trapped); organized workers experience moderate extraction (organized/constrained or organized/mobile) because unions raise the cost of arbitrary deskilling. Union presence differentiates d values: same worker at same skill level experiences different chi depending on whether union protections exist.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Deskilling dynamics resolve the mandatrophy by showing that the constraint contains genuine coordination function alongside real extraction. The tangled rope classification is correct, not a failure to pick a 'real' type. The coordination function is: standardization enables communication across distributed production sites, provides quality control through procedure specification, and allows scaling without reproducing skilled workers. The extraction function is: wage suppression through labor interchangeability, autonomy loss through procedure enforcement, skill devaluation through market saturation. Both are structural, not artifacts of measurement. The perspectival variation (snare from victims, rope from beneficiaries, tangled rope from analysts, scaffold from organized workers) reveals that the classification itself is observer-relative: what appears as pure coordination to management appears as pure extraction to workers. The constraint's temporal evolution shows increasing theater_ratio and extractiveness, indicating that as worker power has declined (union density fallen, global labor supply increased, algorithmic control intensified), the pure coordination interpretation has become less plausible. The coordination function persists (standardization still does improve quality communication) but the extraction function has grown relative to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_definition_boundary,
    'Is deskilling a loss of skill or a redefinition of what counts as skill in the labor market?',
    'Comparative analysis of cognitive/motor task demands before and after standardization; measurement of actual problem-solving required in ''deskilled'' roles vs official job descriptions',
    'If actual skill persists but market devalues it: extraction is primarily through wage suppression, not genuine skill loss. If actual skill requirements decline: deskilling is real but may reflect genuine efficiency gains rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_definition_boundary, empirical, 'Whether deskilling represents genuine skill loss or market devaluation of existing skills').

omega_variable(
    worker_autonomy_substitution,
    'Can standardization procedures that reduce worker autonomy be offset by other forms of autonomy (schedule flexibility, remote work, participatory process improvement)?',
    'Longitudinal worker satisfaction surveys; measurement of autonomy domains (task discretion, schedule flexibility, decision participation); comparison of total autonomy across standardized vs craft-based roles',
    'If autonomy can be redistributed: the extraction mechanism weakens (workers tolerate less discretion in exchange for other benefits). If autonomy loss is not compensated: snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_autonomy_substitution, empirical, 'Whether autonomy can be redistributed across different domains').

omega_variable(
    wage_floor_offsetting,
    'Do union wage floors and protections in heavily standardized sectors (retail, hospitality, manufacturing) actually offset deskilling-driven wage suppression, or does standardization eventually break union power?',
    'Comparative wage analysis across unionized and non-unionized sectors with similar deskilling levels over 50-year periods; causal inference from union organizing success/failure timing relative to deskilling waves',
    'If unions successfully offset: scaffold classification confirmed — deskilling is temporary extraction with a real sunset. If unions fail: deskilling becomes permanent snare even with organizing attempts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_floor_offsetting, empirical, 'Whether union protections offset deskilling-driven wage suppression').

omega_variable(
    reskilling_possibility,
    'Is deskilling reversible through deliberate reskilling investment, or does it create path-dependent lock-in where skills once lost cannot be recovered?',
    'Historical case studies of sectors attempting reskilling after deskilling (postwar German manufacturing, post-2008 US manufacturing); measurement of time and cost to restore craft knowledge vs initial deskilling cycle',
    'If reversible: constraint is tangled rope with possible exit. If irreversible: constraint is snare with permanent lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reskilling_possibility, empirical, 'Whether deskilling can be reversed through reskilling investment').

omega_variable(
    standardization_benefit_distribution,
    'Do consumers benefit from standardized product quality and lower prices from deskilling, or is standardization primarily a management control mechanism with minimal consumer benefit?',
    'Product quality comparison (defect rates, durability, customizability) across handmade vs mass-produced goods at equivalent price points; consumer surplus analysis; measurement of actual quality improvement attributable to deskilling vs other factors',
    'If consumers benefit significantly: deskilling is partial coordination benefit (not pure extraction). If consumers don''t benefit: deskilling is pure extraction masked as efficiency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standardization_benefit_distribution, empirical, 'Whether consumers benefit from standardized products from deskilled labor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_deskilling_dynamics, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deskill_tr_t0, labor_deskilling_dynamics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(deskill_tr_t50, labor_deskilling_dynamics, theater_ratio, 50, 0.52).
narrative_ontology:measurement(deskill_tr_t100, labor_deskilling_dynamics, theater_ratio, 100, 0.64).

% Extraction over time
narrative_ontology:measurement(deskill_be_t0, labor_deskilling_dynamics, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(deskill_be_t50, labor_deskilling_dynamics, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(deskill_be_t100, labor_deskilling_dynamics, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_deskilling_dynamics, resource_allocation).
narrative_ontology:boltzmann_floor_override(labor_deskilling_dynamics, 0.18).
narrative_ontology:affects_constraint(labor_deskilling_dynamics, wage_stagnation_structural).
narrative_ontology:affects_constraint(labor_deskilling_dynamics, union_power_decline).
narrative_ontology:affects_constraint(labor_deskilling_dynamics, algorithmic_management_systems).

% DUAL FORMULATION NOTE:
% Labor deskilling is structurally upstream of wage stagnation (deskilling enables wage suppression) and algorithmic management (standardization enables algorithmic replacement of human judgment). It is structurally downstream of union power decline (weakened unions cannot resist deskilling). The three constraints form a causal chain: union decline → deskilling enabled → wage stagnation accelerated → algorithmic control normalized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_deskilling_dynamics, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
