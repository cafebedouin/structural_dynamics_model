% ============================================================================
% CONSTRAINT STORY: labor_market_information_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_information_asymmetry, []).

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
 *   constraint_id: labor_market_information_asymmetry
 *   human_readable: Labor Market Information Asymmetry
 *   domain: economic/labor_markets
 *
 * SUMMARY:
 *   Labor market information asymmetry — the structural gap between what
 *   employers know about jobs, compensation, and career prospects versus what
 *   workers know — creates a constraint system that appears simultaneously as
 *   pure coordination, mixed coordination with extraction, and pure
 *   extraction depending on the agent's structural position and exit options.
 *   The constraint has intensified over the interval as job complexity has
 *   increased, geographic mobility has accelerated, and compensation opacity
 *   has been actively maintained through institutional practices (non-compete
 *   agreements, salary secrecy norms, opaque promotion criteria). Job seekers
 *   without networks or industry knowledge face maximum information
 *   extraction through unfavorable job offers, below-market compensation, and
 *   hidden expectations. Connected professionals experience the same
 *   constraint as enabling access to broader opportunities through
 *   information flow. Employers and recruiters experience the constraint as
 *   providing the coordination function that matches distributed workers to
 *   distributed opportunities. The constraint exhibits suppression through
 *   institutional norms (salary secrecy, non-disclosure agreements),
 *   information gatekeeping (credential requirements that signal beyond
 *   actual role needs), and asymmetric access to data (employers run salary
 *   surveys workers cannot access). Theater has remained low-to-moderate
 *   because the coordination function (job matching) is substantive — it is
 *   not purely performative — but it has increased slightly as resume
 *   screening and algorithmic filtering have become less transparent.
 *
 * KEY AGENTS:
 *   - Job Seekers Without Networks: Primary victims (powerless/trapped) — bear maximum extraction from asymmetric information about compensation, role requirements, and career trajectory
 *   - Career-Switching Workers: Secondary victims (moderate/constrained) — benefit from job boards but pay premium for credential translation and industry entry uncertainty
 *   - Senior Connected Professionals: Moderate beneficiary/victim (powerful/mobile) — benefit from network information access while participating in system that excludes others
 *   - Large Employers: Primary beneficiaries (institutional/arbitrage) — use asymmetry to attract candidate pools, compress wages, reduce bargaining power; experience the constraint as pure coordination
 *   - Recruitment Industry: Institutional beneficiary (institutional/arbitrage) — profit from information aggregation and intermediation; reduce asymmetry while maintaining information rents
 *   - Organized Labor / Labor Economists: Organized victim (organized/constrained) — perceive asymmetry as enabling wage suppression; face active resistance to transparency initiatives
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing institutional information suppression as inherent to labor markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_information_asymmetry, 0.58).
domain_priors:suppression_score(labor_market_information_asymmetry, 0.65).
domain_priors:theater_ratio(labor_market_information_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_information_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_market_information_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_market_information_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_information_asymmetry, tangled_rope).
narrative_ontology:human_readable(labor_market_information_asymmetry, "Labor Market Information Asymmetry").
narrative_ontology:topic_domain(labor_market_information_asymmetry, "economic/labor_markets").

domain_priors:requires_active_enforcement(labor_market_information_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_information_asymmetry, employers).
narrative_ontology:constraint_beneficiary(labor_market_information_asymmetry, recruiters).
narrative_ontology:constraint_beneficiary(labor_market_information_asymmetry, hiring_gatekeepers).
narrative_ontology:constraint_victim(labor_market_information_asymmetry, job_seekers).
narrative_ontology:constraint_victim(labor_market_information_asymmetry, workers_transitioning_industries).
narrative_ontology:constraint_victim(labor_market_information_asymmetry, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOB SEEKER WITHOUT NETWORKS (SNARE) — Trapped in information scarcity. Without access to informal networks, industry contacts, or privileged salary/role information, the job seeker faces maximum extraction: employers extract information asymmetrically about role requirements, compensation bands, career trajectory, and workplace culture. The seeker's only exit option is accepting unfavorable terms or prolonged unemployment. No alternatives exist without overcoming the information barrier itself.
constraint_indexing:constraint_classification(labor_market_information_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER-SWITCHING WORKER (TANGLED ROPE) — Constrained by industry transition costs and skill translation uncertainty. The constraint provides genuine coordination: job boards and industry intermediaries help match skills across domains. But asymmetric extraction occurs because industry insiders capture information premiums — they know credential requirements that switchers don't, hidden compensation bands, and which skills transfer. The worker benefits from matching infrastructure but pays a stability penalty for the information gap.
constraint_indexing:constraint_classification(labor_market_information_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE EMPLOYER (ROPE) — Experiences the constraint as pure coordination. The information asymmetry enables the employer to broadcast job requirements widely and attract candidate flow for screening. Employers benefit from centralized job posting platforms and candidate database systems. The constraint solves a genuine matching problem: employers and workers are distributed across geography and skill spaces. The employer has arbitrage options (they can source from multiple markets, access internal talent pools, use recruitment agencies) and experiences low extraction.
constraint_indexing:constraint_classification(labor_market_information_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONNECTED SENIOR PROFESSIONAL (TANGLED ROPE) — Mobile across industries and geographies, with strong networks. The constraint provides coordination benefit (job boards, recruiters enable access to broader opportunities). But extraction still occurs because professional networks concentrate information — the connected agent benefits from knowing salaries, hidden roles, and advancement paths that junior workers don't. The agent is mobile enough to exercise optionality but participates in a system where information rents flow to the well-connected.
constraint_indexing:constraint_classification(labor_market_information_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RECRUITMENT INDUSTRY (ROPE) — Experiences the constraint as creating the coordination function they provide. Recruitment agencies, job boards, LinkedIn, resume databases, and salary transparency platforms all exist to reduce information asymmetry. The intermediaries profit from the constraint (matching fees, listings, data access) but also genuinely reduce it through information aggregation and distribution. The constraint enables their business model without creating meaningful extraction beyond the transaction fee — which is a legitimate coordination cost.
constraint_indexing:constraint_classification(labor_market_information_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR MARKET EFFICIENCY (ORGANIZED) — Organized agents (unions, policy advocates, labor economists) perceive the constraint as creating both coordination and extraction. Information asymmetry enables matching but also enables wage suppression through information rents captured by employers and gatekeepers. Collective action to increase transparency (wage transparency laws, union salary surveys, industrywide standards) faces resistance from beneficiaries. The organized perspective sees this as a solvable constraint with non-zero suppression of alternatives — transparency regulations are actively resisted by employers.
constraint_indexing:constraint_classification(labor_market_information_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some information asymmetry is inherent to any decentralized labor market: job seekers are distributed, employers have heterogeneous demands, and no single agent has complete knowledge of all opportunities. This perspective sees the constraint as immutable — an inevitable cost of large-scale labor matching. However, this reading risks naturalizing what is actually a contingent institutional choice: the level of asymmetry is variable (it is lower in Scandinavian transparent-salary regimes, higher in US opaque regimes).
constraint_indexing:constraint_classification(labor_market_information_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_information_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_information_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_information_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_information_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(labor_market_information_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Employers extract information rents through wage suppression, hidden advancement criteria, and asymmetric knowledge of market compensation. The extraction is not at maximum (0.85+) because coordination gains are real — job boards, recruiters, and platforms do match workers to opportunities. But extractiveness has increased over the interval as job complexity has outpaced transparency (specialized roles have fewer comparable positions, making wage benchmarking harder) and employer information collection (resume databases, reference networks, background checks) has expanded. Suppression (0.65): High. Institutional norms actively maintain asymmetry through salary secrecy agreements, non-compete clauses, opaque promotion criteria, and gatekeeping through credential requirements. Exit barriers include geographic constraints (jobs are location-specific), skill specificity (retraining for new fields is costly), and network dependence (without connections, information becomes even scarcer). Theater ratio (0.48): Low-to-moderate. The coordination function is substantive — job matching does solve a real problem — but theater has increased as algorithmic resume screening has become less transparent and employer screening criteria have become more arcane. Some theater appears in resume/credential inflation requirements that exceed actual job demands.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the powerless job seeker (Snare) and the institutional employer (Rope) is maximal: they perceive the same constraint as pure extraction versus pure coordination respectively. This gap reveals that the constraint's classification is not observer-independent — it depends critically on structural position. A policy implication emerges: the gap itself indicates the asymmetry is extractive. If the constraint were genuinely pure coordination, both beneficiaries and targets would perceive low extraction. The large gap between Snare and Rope indicates that one party's coordination benefit comes at another's extraction cost — the hallmark of Tangled Rope, not Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extraction (χ) derives from base extractiveness (ε = 0.58), directionality (d), and scope (σ = 1.0 for national labor markets). Job seekers without networks: d = 0.95 (trapped victim), f(d) ≈ 1.42, χ ≈ 0.82. Career switchers: d = 0.70 (constrained victim), f(d) ≈ 0.92, χ ≈ 0.53. Connected professionals: d = 0.45 (mobile, mixed beneficiary/victim), f(d) ≈ 0.55, χ ≈ 0.32. Employers: d = 0.08 (arbitrage beneficiary), f(d) ≈ -0.12, χ ≈ -0.07 (negative — the constraint subsidizes them). The spread from χ = -0.07 to χ = 0.82 across the spectrum confirms the tangled_rope classification: asymmetric extraction powered by coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the apparent contradiction between 'information asymmetry as natural law' and 'information asymmetry as extractive institutional design.' The natural law reading (mountain) is a false summit because the level of asymmetry is variable and historically contingent: Scandinavian countries with salary transparency laws have demonstrably lower information asymmetry than US labor markets with salary secrecy norms. The constraint is not immutable; it is maintained by active institutional choices (non-compete enforcement, salary confidentiality norms, gatekeeping through credential inflation). The analytical observer who naturalizes it risks serving as an accomplice to the extraction itself — 'this is just how labor markets work' makes it less likely that solutions (transparency mandates, collective bargaining, skill-based credential reform) will be pursued. The classification is correctly Tangled Rope, not Mountain: the constraint solves a genuine matching problem (coordination function) while enabling information-rent extraction (asymmetric power). Policy can reduce extraction without destroying coordination by increasing transparency (wage bands, advancement criteria, skill translation paths) while maintaining job matching platforms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_institutional_asymmetry,
    'How much information asymmetry is inherent to decentralized labor matching versus how much is produced by institutional choices (secrecy norms, gatekeeping, asymmetric access to data)?',
    'Cross-national comparative analysis: measure asymmetry levels in high-transparency regimes (Scandinavian salary transparency laws, EU transparency requirements) versus low-transparency regimes (US salary secrecy, equity clawback norms). If transparency regimes show significantly lower measured asymmetry (searchable salary bands, clear advancement paths, open internal mobility), the ''natural law'' reading is falsified.',
    'If institutional: the mountain classification is a false summit. The constraint is a tangled_rope at all perspectives, and policy can reduce extraction through transparency mandates. If inherent: the constraint approaches mountain universally, but organized labor can still reduce suppression through collective information aggregation (union surveys, industry standards).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_institutional_asymmetry, empirical, 'Natural versus institutional sources of labor market information asymmetry').

omega_variable(
    information_reduction_extraction_tradeoff,
    'Does reducing information asymmetry through transparency (salary bands, skill inventories, advancement criteria) reduce extraction or enable new forms of extraction (algorithmic filtering, discrimination proxying)?',
    'Longitudinal study of wage gaps, job access, and advancement rates before/after transparency interventions. Measure whether publicized salary bands reduce gender/race wage gaps or enable employers to compress wages more efficiently. Track whether algorithmic hiring systems trained on transparent data perpetuate or amplify previous disparities.',
    'If transparency reduces extraction: policy should mandate information access. If it enables new extraction: need coordinated reforms (transparency + discrimination monitoring + collective bargaining power). If neutral: information asymmetry is a false target for labor market reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_reduction_extraction_tradeoff, empirical, 'Whether transparency reduces or redirects extraction').

omega_variable(
    network_substitution_dynamics,
    'Can informal networks ever be fully replaced by transparent platforms, or does information asymmetry inevitably regenerate through network privilege (some workers know people, others don''t)?',
    'Network analysis of job acquisition: track proportion of jobs filled through referral versus application versus recruiter across time and demographic groups. Measure whether job board adoption reduces referral-based hiring or merely adds a parallel channel that referral-advantaged candidates can also exploit.',
    'If networks cannot be eliminated: information asymmetry is structural to labor markets (quasi-mountain property). Policy should focus on reducing the extraction powered by asymmetry (wage transparency, advancement transparency) rather than eliminating asymmetry itself. If networks can be displaced: market adoption of platforms with algorithmic matching could reduce extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_substitution_dynamics, empirical, 'Whether network-based information advantages are eliminable or structural').

omega_variable(
    employer_information_asymmetry,
    'Is the dominant asymmetry employer-to-worker (employers know more about roles/compensation/culture) or worker-to-employer (employers cannot assess candidate quality, commitment, or actual productivity)?',
    'Measure asymmetry bidirectionality: survey workers and employers separately on what information gaps most constrain hiring/job acceptance decisions. Compare measurement error rates in worker self-assessment versus employer job description accuracy. Track post-hire surprise rates (worker disappointed with role, employer disappointed with hire) as proxy for asymmetry impact.',
    'If asymmetry is primarily employer-to-worker: transparency and disclosure mandates reduce extraction. If bidirectional: both sides perceive extraction; information platforms provide mutual value (rope). If primarily worker-to-employer: asymmetry benefits workers (they exploit employers'' uncertainty); extraction framing is reversed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_information_asymmetry, empirical, 'Directionality of dominant information asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_information_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmia_tr_t0, labor_market_information_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lmia_tr_t10, labor_market_information_asymmetry, theater_ratio, 10, 0.42).
narrative_ontology:measurement(lmia_tr_t20, labor_market_information_asymmetry, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(lmia_be_t0, labor_market_information_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lmia_be_t10, labor_market_information_asymmetry, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(lmia_be_t20, labor_market_information_asymmetry, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_information_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(labor_market_information_asymmetry, wage_gap_persistence).
narrative_ontology:affects_constraint(labor_market_information_asymmetry, credential_inflation_signaling).
narrative_ontology:affects_constraint(labor_market_information_asymmetry, occupational_licensing_gatekeeping).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
