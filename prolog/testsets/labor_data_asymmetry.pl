% ============================================================================
% CONSTRAINT STORY: labor_data_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_data_asymmetry, []).

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
 *   constraint_id: labor_data_asymmetry
 *   human_readable: Labor Data Asymmetry in Employment Markets
 *   domain: labor_economics/employment_coordination
 *
 * SUMMARY:
 *   Labor data asymmetry represents a structural coordination failure in
 *   employment markets where employers aggregate information about wage
 *   distributions, hiring criteria, performance metrics, and worker
 *   histories, while individual workers operate with fragmented knowledge of
 *   only their own offers and second-hand rumors. This asymmetry creates
 *   extraction opportunities alongside genuine coordination functions:
 *   employers can match workers to roles more efficiently with concentrated
 *   information, but they also use the same information advantage to suppress
 *   wage pressure, prevent labor mobility, and maintain demographic wage
 *   gaps. The constraint exhibits all eight perspectives, ranging from snare
 *   (powerless workers trapped without alternatives) through tangled rope
 *   (workers with partial mobility and collective capacity) to rope
 *   (employers' genuine coordination benefit) to scaffold (transparency
 *   advocates building alternative information pathways) to piton (HR
 *   industry's performative wage-setting rituals) to mountain (false
 *   naturalization as inherent information economics). The extractiveness has
 *   increased from 0.42 to 0.58 over the measured interval (10 years),
 *   reflecting layered digitization and algorithmic wage management. The
 *   theater ratio has remained relatively low (0.38 to 0.48), indicating the
 *   constraint functions substantially through material extraction rather
 *   than pure ritual.
 *
 * KEY AGENTS:
 *   - Individual Workers: Primary victim (powerless/trapped) — unable to access aggregated wage information; cannot verify if pay is competitive without risking professional identity
 *   - Worker Collectives / Unions: Secondary victim (moderate/constrained) — experience both coordination function (collective wage negotiations) and extraction (resisted by employers; legal barriers to information sharing among members)
 *   - Employers (Aggregated): Primary beneficiary (institutional/arbitrage) — benefit from information concentration enabling selective wage offers and labor cost control; experience constraint as coordination mechanism
 *   - Tech Sector Workers (High-Demand): Intermediate agent (powerful/mobile) — have greater information access and mobility options due to competitive hiring; still experience selective information suppression but with lower extraction intensity
 *   - Career-Climbed Identity-Locked Professionals: Secondary victim (moderate/identity_locked) — structurally mobile but cognitively bound to professional norms against wage discussion; suppression mechanism is internalized
 *   - Pay Transparency Advocates: Organized reformers (organized/constrained) — building alternative information pathways (legislation, crowd-sourcing) with real exit timeline
 *   - Human Resources Industry: Institutional actor (institutional/arbitrage) — maintains performative wage-setting rituals that appear scientific but function as justification for employer-set bands
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional asymmetry as inherent to information economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_data_asymmetry, 0.58).
domain_priors:suppression_score(labor_data_asymmetry, 0.65).
domain_priors:theater_ratio(labor_data_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_data_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_data_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_data_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_data_asymmetry, tangled_rope).
narrative_ontology:human_readable(labor_data_asymmetry, "Labor Data Asymmetry in Employment Markets").
narrative_ontology:topic_domain(labor_data_asymmetry, "labor_economics/employment_coordination").

domain_priors:requires_active_enforcement(labor_data_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_data_asymmetry, employers_aggregated).
narrative_ontology:constraint_beneficiary(labor_data_asymmetry, staffing_platforms).
narrative_ontology:constraint_beneficiary(labor_data_asymmetry, labor_brokers).
narrative_ontology:constraint_victim(labor_data_asymmetry, individual_workers).
narrative_ontology:constraint_victim(labor_data_asymmetry, collective_bargaining_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL WORKER (SNARE) — Trapped in asymmetric information structure. Employer knows wage distribution, hiring criteria, performance benchmarks, worker history; worker knows only their own offers and rumors. No alternative labor market exists with symmetric information. Career mobility, wage negotiation, and job search are constrained by inability to verify labor market conditions independently. Cannot organize or exit without bearing massive cost (relocation, retraining, unemployment gap).
constraint_indexing:constraint_classification(labor_data_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKER COLLECTIVE / UNION (TANGLED ROPE) — Genuine coordination function: collective bargaining pools information, establishes reference wages, enforces norms. But information asymmetry is actively maintained — employers resist wage transparency, legally challenge disclosure agreements, suppress internal pay databases. Collective bears extraction alongside coordination benefit. Exit costs are high (decertification fights, employer retaliation) but surmountable with sustained organizing.
constraint_indexing:constraint_classification(labor_data_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYERS AGGREGATED (ROPE) — Pure coordination mechanism from this perspective. Data asymmetry solves the genuine coordination problem of matching workers to roles without opening wages to competitive pressure. Employers benefit from information advantage but structure it as coordination: 'we need to maintain salary bands for internal equity,' 'market rates are proprietary,' 'publicizing ranges would disrupt recruiting.' No direct extraction experienced — the mechanism is framed as mutual benefit (employer hires reliably, worker receives fair salary within band).
constraint_indexing:constraint_classification(labor_data_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-DEMAND SECTOR WORKER (TANGLED ROPE) — Genuine coordination function exists (skill matching is real), alongside extraction. Worker has mobile options (can change employers, can freelance, can negotiate) but faces selective information access — only certain workers (high-demand skill sets) receive transparent offers and salary comps. Extraction is lower for this agent than for powerless workers, but the mechanism is the same. The asymmetry coordinates matching while extracting selective advantage for employers.
constraint_indexing:constraint_classification(labor_data_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CAREER CLIMBER / IDENTITY-LOCKED PROFESSIONAL (SNARE) — Structurally mobile (has skills, can job-search, no geographic barriers), but identity-fused with professional advancement narrative. The asymmetry is perceived as natural professional discipline: 'salary discussions are unprofessional,' 'you don't ask for market comps, you prove your value,' 'transparency is career suicide.' Identity frame prevents exercise of actual structural mobility. Cannot discuss wages without threat to professional identity (being 'greedy,' 'not a team player'). The mechanism is snare because the binding is cognitive rather than material.
constraint_indexing:constraint_classification(labor_data_asymmetry, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 6: PAY TRANSPARENCY ADVOCATES / ORGANIZED REFORM (SCAFFOLD) — See data asymmetry as a temporary coordination failure solvable through policy intervention: wage disclosure laws, salary band publication, algorithmic transparency mandates. Organized movement for state-mandated transparency is building alternative verification pathways (salary.com, levels.fyi, Glassdoor crowd-sourcing). Exit path exists if transparency laws mature. Suppression is high (employers fight disclosure laws actively), but the organized movement has agency and exit timeline. Classification as scaffold reflects the sunset clause: 'if pay transparency laws pass and cultural norms shift toward openness, this extraction mechanism collapses.'
constraint_indexing:constraint_classification(labor_data_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: HUMAN RESOURCES INDUSTRY (PITON) — The institutional maintenance ritual around 'confidential salary discussions' and 'market-based compensation' is substantially performative. HR departments claim to use market data, benchmarking studies, and scientific methods to set wages, but the data sources are proprietary, methodologies are opaque, and the underlying logic (that 'fair pay' equals 'what you had to accept') is tautological. The ritual persists through institutional inertia — HR practices are codified in policy, training, and legal precedent — despite workers increasingly circumventing formal channels (peer conversations, online forums, recruiting calls with transparent offers). Theater is high; functional coordination is lower. Piton classification: the mechanism is degraded but maintained.
constraint_indexing:constraint_classification(labor_data_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an information economics perspective, wage data asymmetry appears as an immutable feature of labor markets: workers and employers have fundamentally different information access (adverse selection, moral hazard), so complete transparency is impossible and some asymmetry is inherent. This perspective risks naturalizing what is actually a contingent institutional choice — to maintain or enforce the asymmetry, rather than letting workers access market data. The structural data contradicts the mountain classification, revealing this as false naturalization.
constraint_indexing:constraint_classification(labor_data_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_data_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_data_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_data_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_data_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_data_asymmetry, TR),
    TR >= 0.70.

:- end_tests(labor_data_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Employers extract significant value from information asymmetry through reduced wage pressure, suppressed labor mobility, and demographic wage gap persistence. The extraction is not maximal (0.66+) because many workers do find alternative information sources (peer networks, online databases, recruiter calls), and some sectors (tech) have developed partially transparent hiring. But the baseline asymmetry remains substantial. Suppression (0.65): High. Barriers to worker access to aggregated wage information include: (1) legal structures preventing information pooling (non-compete and non-disclosure agreements), (2) cultural norms against wage discussion (professionalism frame), (3) technical barriers (wage data is dispersed, proprietary, methodologically opaque), (4) retaliation risks for workers who seek or share wage information, (5) identity fusion that makes wage inquiry feel transgressive. Theater ratio (0.48): Moderate-low. While HR departments perform 'scientific' wage-setting (benchmarking studies, market analyses, compensation committees), these rituals are substantially functional rather than purely performative. The asymmetry delivers real coordination benefits (efficient matching) alongside extraction. The theater ratio is not high because the mechanism works — it's not maintained purely through ritual, but through actual information advantage and structural enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates complete perspectival divergence. The individual worker perceives a snare: trapped, no alternatives, pure extraction. The worker collective perceives tangled rope: genuine coordination function (collective wage-setting) coexists with extraction (employer resistance, legal barriers, retaliation). The employer perceives pure rope: information concentration solves the matching problem, with no experienced extraction. The tech worker perceives tangled rope but with lower extraction intensity — they have partial information access and mobility. The identity-locked professional perceives snare despite having high structural mobility — the binding is cognitive. The transparency advocate perceives scaffold — data asymmetry is a solvable coordination failure with sunset clause (transparency laws, cultural norm shift). The HR industry perceives its own piton status — wage-setting rituals are degraded but maintained through institutional inertia. The analytical observer risks false mountain classification — information asymmetry appears as necessary feature of labor economics, naturalizing what is actually maintained institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent structural position. Individual workers trapped without exit information (d ≈ 0.92) experience maximum extraction through f(d). Worker collectives with constrained exit (d ≈ 0.68) experience high extraction but not maximum — they have partial agency through collective action. Employers with arbitrage options (d ≈ 0.12) experience low or negative extraction (f(d) ≈ -0.05) — they are net beneficiaries. High-demand sector workers with mobile options (d ≈ 0.58) experience moderate extraction (f(d) ≈ 0.65). Identity-locked professionals with high structural mobility but cognitive binding (d ≈ 0.78) experience high extraction despite having theoretical exit options — the identity lock reduces effective mobility. The scope modifier σ(S) applies at national/global levels: at national scope (σ=1.0), the constraint is most extractive (employers can shift location within country); at global scope (σ=1.2), extraction amplifies because competition for workers is truly global, yet information asymmetry persists across borders.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that labor data asymmetry is simultaneously all types from different structural positions. The constraint is a snare from the powerless worker perspective (no exit, pure extraction). It is tangled rope from the collective organizing perspective (coordination function exists alongside extraction). It is rope from the employer perspective (genuine coordination benefit experienced). It is scaffold from the reformer perspective (asymmetry is solvable with policy intervention and sunset timeline). It is piton from the HR industry perspective (rituals are degraded but maintained). It is mountain from the analytical perspective (false naturalization as inherent to information economics). The question 'is labor data asymmetry really a snare or a tangled rope?' has no single answer — it depends on the observer's structural position. The presheaf of classifications IS the answer: the constraint delivers genuine coordination benefits to employers while functioning as a snare for powerless workers. The fact that the same structural arrangement produces opposite classification types for different agents reveals that the asymmetry's primary function is not coordination but extraction — coordination is secondary benefit that justifies the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asymmetry_source_ambiguity,
    'Is the measured data asymmetry a necessary feature of labor market information economics, or a maintained institutional arrangement?',
    'Cross-national comparison: jurisdictions with mandatory pay transparency (Iceland, Denmark, Switzerland) vs opaque markets (US, UK); correlation between transparency laws and wage compression (reduced gender/demographic wage gaps) vs market efficiency metrics',
    'If necessary: classification shifts toward mountain (natural law of markets). If maintained: classification confirms snare/tangled_rope (institutional extraction). Current evidence suggests asymmetry is maintained, not necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetry_source_ambiguity, empirical, 'Whether data asymmetry is inherent or institutionally maintained').

omega_variable(
    worker_information_access_threshold,
    'At what level of wage information access does the extraction mechanism lose force (worker power increases, snare becomes tangled_rope or rope)?',
    'Experimental/quasi-experimental: regions where pay transparency laws were implemented; measure wage compression, labor mobility, negotiation success pre/post; identify critical information threshold',
    'If threshold is low (< 50% market comps available): transparency partially disrupts snare but tangled_rope persists. If threshold is moderate (50-80%): snare collapses to rope. If threshold is high (> 90%): full transparency required to break mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_information_access_threshold, empirical, 'Information threshold for disrupting extraction mechanism').

omega_variable(
    identity_lock_persistence,
    'If wage information became freely available, would workers still suppress own wage discussions due to internalized professional norms?',
    'Post-transparency behavior tracking: jurisdictions with transparency laws; survey of worker willingness to discuss wages openly; correlation between availability of information and actual communication patterns',
    'If norms persist despite information access: identity-lock mechanism is real (cognitive binding independent of structural barriers). Snare persists as social enforcement even after information asymmetry dissolves. If norms shift with information: identity-lock was secondary to structural suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether professional norms against wage discussion persist after information access').

omega_variable(
    crowd_sourced_data_reliability,
    'Do crowd-sourced wage databases (Glassdoor, levels.fyi, Salary.com) provide sufficient information density and accuracy to disrupt the employer information advantage?',
    'Accuracy validation: cross-check crowd-sourced data against internal pay data for matched roles/demographics; coverage analysis: what percentage of employers/roles are represented; temporal validity: how quickly does crowd-sourced data reflect actual market shifts',
    'If reliable and comprehensive: scaffold perspective confirmed — crowd-sourcing can bypass employer data control, supporting sunset timeline for traditional asymmetry. If unreliable or sparse: employers maintain de facto data monopoly; scaffold timeline extends or fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crowd_sourced_data_reliability, empirical, 'Whether crowd-sourced wage data provides sufficient information density').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_data_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(labdata_tr_t0, labor_data_asymmetry, theater_ratio, 0, 0.38).
narrative_ontology:measurement(labdata_tr_t5, labor_data_asymmetry, theater_ratio, 5, 0.44).
narrative_ontology:measurement(labdata_tr_t10, labor_data_asymmetry, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(labdata_be_t0, labor_data_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(labdata_be_t5, labor_data_asymmetry, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(labdata_be_t10, labor_data_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_data_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(labor_data_asymmetry, gender_wage_gap).
narrative_ontology:affects_constraint(labor_data_asymmetry, labor_mobility_restriction).
narrative_ontology:affects_constraint(labor_data_asymmetry, demographic_wage_discrimination).
narrative_ontology:affects_constraint(labor_data_asymmetry, collective_bargaining_erosion).

% DUAL FORMULATION NOTE:
% Labor data asymmetry is the upstream constraint that enables downstream extraction: gender wage gaps, labor mobility restrictions, and demographic discrimination all depend on information asymmetry as a causal prerequisite. Collective bargaining erosion is bidirectionally linked — weaker bargaining power reduces workers' ability to demand information access, and information asymmetry weakens bargaining position. Each downstream constraint has its own ε value reflecting domain-specific extraction mechanisms, but all are structurally dependent on this asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_data_asymmetry, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
