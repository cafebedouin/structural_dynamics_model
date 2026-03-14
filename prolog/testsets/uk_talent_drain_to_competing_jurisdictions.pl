% ============================================================================
% CONSTRAINT STORY: uk_talent_drain_to_competing_jurisdictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_talent_drain_to_competing_jurisdictions, []).

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
 *   constraint_id: uk_talent_drain_to_competing_jurisdictions
 *   human_readable: UK Talent Drain to Competing Jurisdictions
 *   domain: economic_policy/labor_migration/human_capital
 *
 * SUMMARY:
 *   The UK talent drain to competing jurisdictions represents a structural
 *   extraction mechanism embedded within what appears as natural labor market
 *   equilibrium. Higher-skilled workers (healthcare professionals,
 *   researchers, financial specialists) migrate to jurisdictions offering
 *   superior salaries, work environments, visa accessibility, and career
 *   opportunities. This creates cascading costs: NHS and university staffing
 *   crises, innovation capacity loss, and reduced public goods provision. The
 *   constraint is not zero-sum — competing jurisdictions benefit from talent
 *   acquisition, UK emigrants benefit from higher salaries and mobility,
 *   multinational employers benefit from labor arbitrage — but these benefits
 *   are asymmetric. UK public services and the domestic innovation ecosystem
 *   bear extraction costs while having limited exit options or recovery
 *   mechanisms. The constraint exhibits genuine coordination function
 *   (efficient allocation of skilled labor across global markets) overlaid
 *   with asymmetric distribution of costs and benefits (extraction flowing
 *   from less-developed public sectors to more-developed private sectors and
 *   competitive jurisdictions). Post-Brexit institutional changes
 *   (points-based immigration system, visa fee increases, reduced EU free
 *   movement) have hardened the constraint structure, increasing both
 *   extractiveness and theater (immigration policy appears to manage talent
 *   flow but actually accelerates it).
 *
 * KEY AGENTS:
 *   - UK Public Service Sector: Primary victim (powerless/trapped) — NHS, education, research institutions unable to compete with private sector and international opportunities; no mechanism to recover lost talent or exit constraints
 *   - UK Mid-Career Professionals: Secondary victim (moderate/constrained) — structurally mobile but face visa barriers, relocation costs, social ties; experience both coordination benefits (early-career UK training) and extraction (salary differential, opportunity limitation)
 *   - Competing Jurisdictions: Primary beneficiary (institutional/arbitrage) — US, Canada, Singapore, Australia benefit from active talent recruitment; experience constraint as pure coordination mechanism
 *   - Multinational Employers: Beneficiary (institutional/arbitrage) — access labor arbitrage and global talent pools; benefit from visa restrictions that create mobility barriers for competing employers
 *   - UK Government and Institutions: Conflicted institutional actor (institutional/constrained) — bound by treaty obligations, fiscal constraints, and political economy of immigration policy; simultaneously constrained by and enforcing the extraction mechanism
 *   - Professional Networks and Diaspora Organizations: Organized agents (organized/mobile) — see talent drain as temporary coordination problem with sunset potential through circular migration and repatriation incentives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies constraint as hybrid coordination-extraction mechanism rather than natural market outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_talent_drain_to_competing_jurisdictions, 0.58).
domain_priors:suppression_score(uk_talent_drain_to_competing_jurisdictions, 0.52).
domain_priors:theater_ratio(uk_talent_drain_to_competing_jurisdictions, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_talent_drain_to_competing_jurisdictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_talent_drain_to_competing_jurisdictions, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(uk_talent_drain_to_competing_jurisdictions, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_talent_drain_to_competing_jurisdictions, tangled_rope).
narrative_ontology:human_readable(uk_talent_drain_to_competing_jurisdictions, "UK Talent Drain to Competing Jurisdictions").
narrative_ontology:topic_domain(uk_talent_drain_to_competing_jurisdictions, "economic_policy/labor_migration/human_capital").

domain_priors:requires_active_enforcement(uk_talent_drain_to_competing_jurisdictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_talent_drain_to_competing_jurisdictions, competing_jurisdictions).
narrative_ontology:constraint_beneficiary(uk_talent_drain_to_competing_jurisdictions, multinational_employers).
narrative_ontology:constraint_beneficiary(uk_talent_drain_to_competing_jurisdictions, uk_high_earner_emigrants).
narrative_ontology:constraint_victim(uk_talent_drain_to_competing_jurisdictions, uk_public_services).
narrative_ontology:constraint_victim(uk_talent_drain_to_competing_jurisdictions, uk_innovation_capacity).
narrative_ontology:constraint_victim(uk_talent_drain_to_competing_jurisdictions, uk_domestic_labor_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UK PUBLIC SERVICE WORKERS (SNARE) — Trapped in understaffed NHS, education, and research institutions by lack of resources and overseas competition. Cannot exit without sacrificing career in their field. Bearing full cost of talent drain while having no mechanism to exit or recover.
constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UK MID-CAREER PROFESSIONALS (TANGLED ROPE) — Constrained by visa restrictions, social ties, and housing markets, but genuinely mobile. Benefit from UK education and early career networks; face high exit cost (visa sponsorship, relocation, social disruption) but not insurmountable. Experience both coordination (labor market signaling) and extraction (brain drain tax through upskilled workforce loss).
constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPETING JURISDICTIONS (ROPE) — Benefit from active talent recruitment and visa liberalization. Experience the constraint as pure coordination: attracting skilled workers solves their labor market needs. Net beneficiary position; extraction flows toward these actors.
constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UK GOVERNMENT AND INSTITUTIONS (TANGLED ROPE) — Constrained by treaty obligations (freedom of movement legacy, trade agreements), fiscal pressure, and political economy of immigration policy. Must enforce visa restrictions and salary caps; simultaneously loses high-value workers. Mixed experience: genuine coordination function (managing labor supply) overlaid with extraction (brain drain taxes innovation capacity).
constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: UK PROFESSIONAL NETWORKS AND DIASPORA ORGANIZATIONS (SCAFFOLD) — Organized actors (alumni networks, professional associations, returning talent programs) see talent drain as a temporary coordination failure with sunset potential. High mobility and agency; see exit path through circular migration norms, remote work, and repatriation incentives. Theater is low — actual value creation through network effects.
constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: UK IMMIGRATION POLICY FRAMEWORK (PITON) — Post-Brexit points-based immigration system is substantially performative: ostensibly manages talent acquisition but largely reactive to market shortages rather than proactive development. Framework persists through institutional inertia despite evidence that visa restrictions harm both competitiveness and public services. Theater ratio high because policy ritual (points calculation, visa categories) has decoupled from actual talent flow outcomes.
constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, talent mobility is partially coordination (efficient allocation of human capital) and partially extraction (concentration of opportunity in wealthy jurisdictions, loss of public goods provision in origin countries). The constraint exhibits genuine coordination function alongside asymmetric distribution of benefits. Tangled Rope classification reveals that brain drain is neither pure market efficiency nor pure extraction, but hybrid mechanism embedding both.
constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_talent_drain_to_competing_jurisdictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_talent_drain_to_competing_jurisdictions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_talent_drain_to_competing_jurisdictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_talent_drain_to_competing_jurisdictions, TR),
    TR >= 0.70.

:- end_tests(uk_talent_drain_to_competing_jurisdictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits significant extraction — UK public services lose high-skill workers without compensation, innovation capacity declines, and salary differentials create persistent incentive asymmetry. However, extractiveness is not maximal because the constraint operates through genuine market mechanisms (salary competition) rather than coercion, and both parties (emigrant and destination) benefit from the migration. The value reflects that this is a market-driven extraction mechanism, not predatory. Extractiveness has increased from 0.35 to 0.58 over the measurement interval, indicating that salary differentials have widened and visa barriers have hardened post-Brexit, making emigration more attractive and more costly to reverse. Suppression (0.52): Moderate. Multiple barriers constrain UK workers' alternatives: visa sponsorship requirements (high cost), social ties and housing market integration (high cost), repatriation bias (cultural perception that emigration is permanent), and institutional inertia in UK salary structures (public sector wage caps). Barriers are not insurmountable (many workers do emigrate, so suppression is not total), but they are significant and asymmetric — easier to emigrate than to return. Theater ratio (0.48): Moderate, below piton threshold. Post-Brexit immigration policy exhibits moderate theater — the points-based system is performatively rigorous but functionally reactive. However, the constraint's primary mechanism is not theatrical (it is market-driven) and the theater ratio remains below 0.70, so piton classification does not apply. The theater reflects policy ritual (visa categories, points calculations) that obscures actual talent flow dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between public service workers and competing jurisdictions is maximal: both experience the same structural phenomenon (skilled worker migration), but the worker-beneficiary gap produces opposite classifications (Snare vs. Rope). This gap reveals that the constraint's existence depends entirely on asymmetric structural position — it appears natural or inevitable only from the beneficiary perspective. From the victim perspective, the constraint is extraction without coordination. From the organized actor perspective (diaspora networks), the constraint is temporary. The gap is diagnostic: if all perspectives produced the same classification, the constraint would be natural law (mountain); the perspectival divergence proves it is institutional (tangled rope at the analytical level).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values are derived from the structural pipeline. Competing jurisdictions (beneficiary + arbitrage mobility) → d ≈ 0.10 → f(d) ≈ -0.08 → experienced as net coordination, not extraction (rope). UK public services (victim + trapped) → d ≈ 0.95 → f(d) ≈ 1.42 → experienced as high extraction (snare). Mid-career professionals (victim + constrained) → d ≈ 0.68 → f(d) ≈ 1.05 → experienced as moderate-high extraction with some benefit (tangled rope). UK institutions (mixed: enforcer of policy + victim of outcomes) → d ≈ 0.45-0.55 (split) → experienced as tangled coordination-extraction (tangled rope). The analytical observer (observer with global scope) → d ≈ 0.72 (canonical analytical) → experiences the constraint as tangled rope because the global view reveals both coordination (efficient labor allocation) and extraction (asymmetric cost/benefit distribution). Scope σ(S) slightly dampens national-scope perspectives (σ=0.9) and amplifies global-scope beneficiary perspectives (σ=1.2), which increases the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: The constraint has high extractiveness (0.58) but is not a mandatrophy case because it does NOT resolve as pure Snare (extraction ≥ 0.66, suppression ≥ 0.60). The suppression value (0.52) is below the Snare gate (≥ 0.60), indicating that the constraint operates through market mechanisms (salary differentials) rather than coercion, and that exit is technically possible though costly. This classification as Tangled Rope (not Snare) from the analytical perspective is correct: the constraint has genuine coordination function (efficient global labor allocation, matching skilled workers to opportunities) overlaid with asymmetric extraction (loss of public goods provision, innovation capacity loss in origin countries). The mandatrophy resolves by recognizing that 'brain drain' is not pure extraction but hybrid: beneficial globally (workers + destination employers), harmful to origin public goods (extraction), benefiting destination and source workers. The theater ratio (0.48, below 0.70) confirms this is not Piton (degraded function) but Tangled Rope (hybrid but functional). The constraint exhibits real coordination function, so it cannot be degraded (Piton) or pure extraction (Snare) — it is genuinely tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    talent_quality_threshold,
    'What skill/qualification threshold defines ''talent drain'' versus normal labor mobility?',
    'Comparative analysis of emigration rates by education level and field; identification of which sectors experience disproportionate loss; correlation with measurable output metrics (research output, innovation, public service capacity)',
    'If threshold set low: classify constraint as affecting broad labor market (Snare). If threshold high: constraint affects only elite knowledge workers (Rope from some perspectives). Determines scope and severity of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_quality_threshold, empirical, 'Skill threshold defining talent drain severity').

omega_variable(
    salary_differential_causality,
    'How much of talent emigration is driven by salary differentials versus lifestyle, work environment, and visa accessibility?',
    'Econometric analysis of emigration decisions; qualitative interviews with recent emigrants; comparison of salary-adjusted emigration rates; analysis of emigration from high-paying UK sectors versus low-paying sectors',
    'If salary-dominant: constraint is extraction mechanism (Snare from victim perspective). If multi-causal with visa as primary barrier: constraint is institutional artifact (Piton). If lifestyle-driven: constraint reflects values alignment (Rope from beneficiary perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(salary_differential_causality, empirical, 'Causal drivers of talent emigration').

omega_variable(
    circular_migration_potential,
    'Can UK rebuild ''pull'' factors sufficient to enable circular migration rather than permanent brain drain?',
    'Tracking of return migration rates; analysis of countries with successful circular migration (Germany, Canada); assessment of remote work and distributed team viability; identification of policy changes that could increase repatriation',
    'If high circular migration potential: scaffold sunset is real (constraint is temporary). If low potential: drain is permanent extraction (Snare classification dominates). Determines whether constraint has natural sunset or requires active policy intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circular_migration_potential, empirical, 'Feasibility of circular migration to reverse brain drain').

omega_variable(
    public_goods_replacement_cost,
    'What is the economic cost of replacing lost NHS doctors, university researchers, and skilled educators through recruitment, training, or service reduction?',
    'Cost accounting of public service vacancies; comparison of salary/recruitment costs to replacement training costs; measurement of service quality degradation; estimation of innovation loss in research sectors',
    'If replacement cost exceeds salary differential: talent drain is net extraction (Snare from victim perspective). If replacement cost lower than keeping uncompetitive workers: drain is efficiency mechanism (Rope). Determines whether suppression is structural (economic necessity) or institutional (policy choice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_goods_replacement_cost, empirical, 'Economic cost of replacing lost talent in public services').

omega_variable(
    institutional_identity_lock,
    'To what degree does UK identity (as ''world capital of finance/research'') depend on retaining domestic talent versus attracting global talent?',
    'Historical analysis of UK competitive advantage; identification of threshold where talent concentration becomes identity critical; analysis of reputation effects of public service degradation',
    'If identity-dependent: UK institutions are identity_locked into visa restrictions that harm them (institutional agents cannot perceive mutability). If instrumental: institutions can adapt policy pragmatically. Determines whether institutional perspectives are captured or strategic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock, conceptual, 'Institutional identity dependence on talent retention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_talent_drain_to_competing_jurisdictions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uktalent_tr_t0, uk_talent_drain_to_competing_jurisdictions, theater_ratio, 0, 0.32).
narrative_ontology:measurement(uktalent_tr_t5, uk_talent_drain_to_competing_jurisdictions, theater_ratio, 5, 0.42).
narrative_ontology:measurement(uktalent_tr_t10, uk_talent_drain_to_competing_jurisdictions, theater_ratio, 10, 0.48).
narrative_ontology:measurement(uktalent_tr_t7, uk_talent_drain_to_competing_jurisdictions, theater_ratio, 7, 0.45).

% Extraction over time
narrative_ontology:measurement(uktalent_be_t0, uk_talent_drain_to_competing_jurisdictions, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uktalent_be_t5, uk_talent_drain_to_competing_jurisdictions, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(uktalent_be_t10, uk_talent_drain_to_competing_jurisdictions, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(uktalent_be_t7, uk_talent_drain_to_competing_jurisdictions, base_extractiveness, 7, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_talent_drain_to_competing_jurisdictions, resource_allocation).
narrative_ontology:boltzmann_floor_override(uk_talent_drain_to_competing_jurisdictions, 0.18).
narrative_ontology:affects_constraint(uk_talent_drain_to_competing_jurisdictions, nhs_staffing_crisis).
narrative_ontology:affects_constraint(uk_talent_drain_to_competing_jurisdictions, university_research_funding).
narrative_ontology:affects_constraint(uk_talent_drain_to_competing_jurisdictions, uk_innovation_competitiveness).
narrative_ontology:affects_constraint(uk_talent_drain_to_competing_jurisdictions, post_brexit_labor_market_adjustment).

% DUAL FORMULATION NOTE:
% The UK talent drain decomposes into sector-specific constraints (NHS staffing crisis, university brain drain, financial sector talent mobility) with different ε values. The umbrella constraint (talent drain to competing jurisdictions) has ε=0.58 and represents the aggregate extraction mechanism. Downstream constraints have higher ε (NHS crisis ε≈0.72) reflecting more severe extraction in specific sectors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_talent_drain_to_competing_jurisdictions, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
