% ============================================================================
% CONSTRAINT STORY: job_market_clearinghouse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_job_market_clearinghouse, []).

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
 *   constraint_id: job_market_clearinghouse
 *   human_readable: Job Market Clearinghouse Mechanism
 *   domain: labor_economics/employment_matching
 *
 * SUMMARY:
 *   The job market clearinghouse is a structural constraint that matches
 *   workers to employers by aggregating labor supply and job vacancy
 *   information. In principle, this solves a genuine coordination problem:
 *   information asymmetry creates deadweight loss (jobless workers, unfilled
 *   positions). In practice, the clearinghouse mechanism has layered
 *   extraction mechanisms on top of its coordination function. Credential
 *   inflation uses employers' need to screen as cover for raising barriers to
 *   entry. Wage compression uses information aggregation to enable employer
 *   coordination on wages. Platform dependency traps workers in systems they
 *   cannot exit without forgoing job access. Algorithmic opacity hides
 *   discriminatory extraction behind technical necessity. The theater ratio
 *   has risen from 0.38 to 0.71 over the interval, indicating increasing
 *   performativity (credential screening, algorithmic matching,
 *   user-generated reviews) relative to functional matching. Base
 *   extractiveness has risen from 0.35 to 0.52, indicating both
 *   intensification of existing extraction and accumulation of new
 *   mechanisms. The constraint exhibits all characteristics of a Tangled
 *   Rope: it solves a real coordination problem (genuine clearing function)
 *   while simultaneously enabling asymmetric extraction (through credential
 *   inflation, wage compression, platform dependency, and algorithmic
 *   opacity). The mandatrophy remains unresolved because the analytical
 *   observer cannot determine whether rising extractiveness reflects
 *   deepening of the extraction overlay or discovery of previously hidden
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Job Seekers (Unemployed): Primary victims (powerless/trapped) — dependent on clearinghouse for survival income; experience maximum extraction through credential requirements, wage suppression, and information asymmetry
 *   - Job Seekers (Employed, Seeking Transition): Secondary victims (moderate/constrained) — some bargaining power but face significant costs to exit current employment; experience moderate extraction through credential signaling and job market fragmentation
 *   - High-Credential Professionals: Partial beneficiaries (powerful/mobile) — have strong bargaining position but still experience extraction through credential inflation that raises barriers for others; benefit from scarcity premium
 *   - Large Employers: Primary beneficiaries (institutional/arbitrage) — experience genuine coordination benefit; reduced search costs and expanded effective labor supply; high exit optionality
 *   - Platform Intermediaries: Apparent beneficiaries (institutional/constrained) — extract rent through job posting fees and recruiter subscriptions, but themselves trapped by network effects and data dependency; must continuously maintain engagement on both sides
 *   - Educational Institutions: Secondary beneficiaries (institutional/arbitrage) — credential inflation increases enrollment and tuition demand; maintain institutional legitimacy through employer coordination around degrees
 *   - Labor Collectives (Unions/Guilds): Victim-beneficiaries (organized/constrained) — historically used alternative clearinghouses (apprenticeships, union hiring halls) but increasingly sidelined by platforms; constrained by fragmentation of labor supply
 *   - Analytical Observer: System-level view (analytical/analytical) — sees mixture of genuine coordination and layered extraction; mandatrophy unresolved
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(job_market_clearinghouse, 0.52).
domain_priors:suppression_score(job_market_clearinghouse, 0.48).
domain_priors:theater_ratio(job_market_clearinghouse, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(job_market_clearinghouse, extractiveness, 0.52).
narrative_ontology:constraint_metric(job_market_clearinghouse, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(job_market_clearinghouse, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(job_market_clearinghouse, tangled_rope).
narrative_ontology:human_readable(job_market_clearinghouse, "Job Market Clearinghouse Mechanism").
narrative_ontology:topic_domain(job_market_clearinghouse, "labor_economics/employment_matching").

domain_priors:requires_active_enforcement(job_market_clearinghouse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(job_market_clearinghouse, employers_with_bargaining_power).
narrative_ontology:constraint_beneficiary(job_market_clearinghouse, platform_intermediaries).
narrative_ontology:constraint_victim(job_market_clearinghouse, job_seekers_with_constrained_mobility).
narrative_ontology:constraint_victim(job_market_clearinghouse, wage_equilibrium).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNEMPLOYED JOB SEEKER (SNARE) — Structurally trapped by immediate need for income, geographic immobility, lack of alternative income sources, and information asymmetry. Must accept terms set by employers; cannot exit the clearinghouse mechanism without forgoing survival resources. Experiences maximum extraction: employer screening, wage suppression, credential inflation, and precarity.
constraint_indexing:constraint_classification(job_market_clearinghouse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYED WORKER SEEKING TRANSITION (TANGLED ROPE) — Has some exit capacity (current income, skills, geographic optionality) but faces significant costs: job search time, risk of wage loss, relocation expenses, family disruption. The clearinghouse coordinates matching between workers and firms but also extracts through wage compression, credential screening, and job market signaling. Moderate agency with asymmetric benefits/costs.
constraint_indexing:constraint_classification(job_market_clearinghouse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE EMPLOYER (ROPE) — Experiences the clearinghouse as genuine coordination: access to labor supply, standardized screening mechanisms, brand-building through employer reviews. High exit optionality (can employ anywhere, recruit internationally, use automation). Net beneficiary — the clearinghouse reduces their search costs and expands their effective labor supply through information aggregation.
constraint_indexing:constraint_classification(job_market_clearinghouse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-CREDENTIAL PROFESSIONAL (TANGLED ROPE) — Strong negotiating position (scarce skills, multiple offers) but still experiences coordination benefits and some extraction. The clearinghouse indexes their credentials to employer demands, enabling efficient matching. They also benefit from credential inflation (their scarcity premium increases as barriers to entry rise). Moderate extraction experienced through ongoing credential screen escalation.
constraint_indexing:constraint_classification(job_market_clearinghouse, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR COLLECTIVE (ROPE) — Organized labor sees the clearinghouse as a coordination mechanism that enables standardized wage and benefit negotiation. Historically, unions used industry-wide bargaining and apprenticeship systems as alternative clearinghouses. Modern labor collectives face extraction through credential fragmentation and gig economy emergence, but retain organizing power. Classified as Rope because the clearing function is genuine, even as effective bargaining power has declined.
constraint_indexing:constraint_classification(job_market_clearinghouse, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PLATFORM INTERMEDIARY (SNARE) — Appears as beneficiary (extracts rent through premium listings, sponsored job postings, recruiter subscriptions) but is itself increasingly trapped by network effects and data dependency. Must continuously invest in keeping both sides of the market engaged. Theatre is high (user-generated content, algorithmic matching promises, review systems) masking concentration of information asymmetry extraction. Victim of its own extraction mechanisms: fragmentation, data toxicity, need for constant moderation.
constraint_indexing:constraint_classification(job_market_clearinghouse, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: EDUCATIONAL CREDENTIALING SYSTEM (PITON) — University degrees, certifications, and skill assessments maintain their clearing function through institutional inertia despite declining predictive power for job performance. Theater is high: 4-year degree requirement persists even when most job skills require only 6-12 months of training. The clearing mechanism (college = qualified) is performative but self-perpetuating because employers coordinate hiring around the credential signal, not the actual competence. Degraded function maintained through network lock-in.
constraint_indexing:constraint_classification(job_market_clearinghouse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the job market clearinghouse solves the genuine coordination problem of matching millions of workers to millions of jobs efficiently. But this coordination is layered with extraction mechanisms: credential inflation (degree requirements rise not from skill demand but from competitive screening), wage suppression (information aggregation enables employer coordination on wage bands), precarity (fragmented clearinghouses increase worker switching costs), and algorithmic opacity (resume screening systems hide extraction mechanisms). The constraint is mixed: irreducible coordination function + extractive overlay. Mandatrophy remains unresolved.
constraint_indexing:constraint_classification(job_market_clearinghouse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(job_market_clearinghouse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(job_market_clearinghouse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(job_market_clearinghouse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(job_market_clearinghouse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(job_market_clearinghouse, TR),
    TR >= 0.70.

:- end_tests(job_market_clearinghouse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The clearinghouse extracts through multiple mechanisms: credential inflation (raises barriers without matching skill demand), wage compression (information aggregation enables employer coordination), platform dependency (workers cannot discover jobs outside platforms), algorithmic opacity (hides discriminatory screening). However, extraction is not maximal because genuine coordination benefits exist (massive matching efficiency gains, reduced search costs for workers and employers). The rising trajectory (0.35→0.52) reflects accumulation of extraction mechanisms over time, not fundamental shift in coordination function. Suppression (0.48): Moderate. Information asymmetry is significant but not total — alternative job discovery mechanisms exist (networks, direct outreach, industry conferences), though they are increasingly marginalized. Workers face high costs to exit the clearinghouse but not impossible barriers. Theater ratio (0.62): Moderately high. Credential screening theater is substantial (degree requirements for jobs requiring 6-12 months training; resume screening algorithms with hidden criteria). But matching function itself is genuine (jobs are actually filled through platforms). The rising trajectory (0.38→0.71) indicates increasing performativity — credentials become signals of screening adherence rather than job capability; algorithmic matching becomes opaque ritual rather than transparent efficiency.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between powerless job seekers (who see Snare: trapped, no exit, maximum extraction) and institutional employers (who see Rope: genuine coordination benefit, high exit optionality, net benefit). The employed transition-seeker occupies the middle ground, experiencing both coordination benefits and extraction costs. The analytical observer sees this gap as structural: it reflects real differences in power, exit options, and beneficiary status — not disagreement about facts but genuine difference in constraint experience. The platform intermediary occupies an unstable position: they appear as beneficiary (extracting rent) but are actually constrained by network effects, data dependency, and continuous maintenance burden — they are experiencing Snare from within their apparent beneficiary position. The educational system's perspective (Piton) reveals that credential theater is maintained through institutional coordination even as its functional value has declined — the screening function is real but increasingly performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d reflects each agent's structural position relative to extraction flow. Unemployed job seekers: d≈0.95 (full target, trapped, no exit) → high f(d) → experience maximum extraction. Employed transition-seekers: d≈0.75 (significant target, constrained mobility) → high f(d) → experience substantial extraction. High-credential professionals: d≈0.45 (partial beneficiary, strong mobility) → moderate f(d) → experience mild extraction. Large employers: d≈0.10 (strong beneficiary, arbitrage optionality) → low/negative f(d) → experience negative extraction (benefit from coordination). Platform intermediaries: d≈0.85 (appear as beneficiary but are themselves trapped) → high f(d) → their extraction mechanisms create extraction for themselves. Analytical observer: d≈0.72 (distributed across perspectives, system-level analysis). Scope modifier σ(S) amplifies extraction at larger scope: local job markets (σ=0.8) have lower effective extraction due to relationship-based hiring; national markets (σ=1.0) show moderate scope effect; global markets (σ=1.2) show highest extraction amplification through algorithmic matching and credential standardization.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint exhibits genuine Tangled Rope structure — irreducible coordination function (matches millions of workers to jobs efficiently) overlaid with asymmetric extraction (credential inflation, wage compression, platform dependency). However, the analytical observer cannot determine whether rising extractiveness reflects: (A) intensification of the extraction overlay (deliberate firm strategy to suppress wages, credential inflation), or (B) discovery of previously hidden mechanisms (algorithmic bias, platform lock-in effects), or (C) natural evolution of information asymmetry as coordination scales. Each resolution has different implications. If (A): firms are deliberately extracting — classification confirmed as Tangled Rope with possibility of movement toward Snare. If (B): mechanisms were always present but now visible — classification remains Tangled Rope. If (C): extraction is inherent cost of coordination at scale — classification shifts toward Rope (acceptance that all large-scale coordination has extraction costs). The rising theater ratio (0.38→0.71) suggests mechanisms are becoming more performative, but interpretations differ: optimists see credential signaling as efficient filtering; critics see it as extraction theater. The rising extractiveness (0.35→0.52) suggests growing asymmetry, but interpretations differ: optimists see efficiency discovery; critics see extraction accumulation. Mandatrophy remains unresolved pending empirical evidence on whether credential inflation serves genuine skill demand or represents pure filtering cost extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_inflation_mechanism,
    'Is credential inflation driven by genuine demand for skills or by firms using credentials as screening proxies to reduce hiring costs?',
    'Empirical analysis of job posting requirements vs actual job task complexity; time-series correlation between degree inflation and task complexity growth; employer surveys on credential screening motivation',
    'If genuine skill demand: credential inflation represents rational coordination signal (Rope component strengthens). If screening proxy: inflation represents extractive equilibrium where firms coordinate on higher barriers to reduce applicant pool (Snare component strengthens). Classification moves from Tangled Rope toward pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_mechanism, empirical, 'Whether credential inflation serves genuine skill needs or employer cost reduction').

omega_variable(
    wage_compression_versus_efficiency,
    'Does wage compression in matched jobs reflect market efficiency discovery or employer information aggregation enabling wage coordination?',
    'Time-series analysis of wage variance pre/post clearinghouse adoption; geographic wage convergence analysis; correlation between platform adoption rates and wage level changes; controlled comparison of wages in sectors with vs without centralized clearinghouses',
    'If efficiency: wage compression is legitimate coordination outcome (extraction minimal). If coordination: wage compression is extractive mechanism enabling employer bargaining power (extraction significant). Moves classification from Rope/Tangled Rope toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_compression_versus_efficiency, empirical, 'Whether wage compression reflects market efficiency or employer coordination').

omega_variable(
    gig_fragmentation_intentionality,
    'Does fragmentation of the job market into multiple clearinghouses (job boards, gig platforms, internal mobility systems) represent technical limitation or deliberate suppression of worker bargaining power?',
    'Historical analysis of platform competition and merger patterns; behavioral evidence of firms deliberately fragmenting labor supply; worker switching cost analysis; comparison of worker outcomes in unified vs fragmented clearinghouse regimes',
    'If technical: fragmentation is coordination challenge requiring integration (Rope problem). If intentional: fragmentation is extraction mechanism maintaining worker isolation (Snare mechanism). Classification shifts toward pure Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gig_fragmentation_intentionality, conceptual, 'Whether market fragmentation is technical limitation or strategic suppression').

omega_variable(
    algorithm_opacity_extraction,
    'Do resume screening algorithms and job recommendation systems improve matching efficiency or hide discriminatory extraction mechanisms?',
    'Algorithmic auditing for demographic disparities; comparison of human vs algorithm selection outcomes; transparency analysis of platform matching criteria; long-term outcome tracking for hired vs rejected candidates',
    'If efficiency: opacity is technical necessity (extraction minimal). If discriminatory: opacity enables extraction through disguised bias (extraction substantial). Moves classification from Tangled Rope toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_opacity_extraction, empirical, 'Whether algorithmic opacity improves efficiency or enables extraction').

omega_variable(
    platform_dependency_trap,
    'Are job seekers trapped by platform dependency (must use clearinghouse to access jobs) or is this a genuine coordination externality?',
    'Analysis of alternative job discovery mechanisms; measurement of job seeker switching costs between platforms; historical data on job market outcomes before/after platform dominance; geographic variation in platform dependency',
    'If genuine externality: dependency reflects coordination success (extraction moderate). If trap: platform dependency enables rent extraction (extraction high). Classification moves from Rope toward Snare for worker perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_dependency_trap, empirical, 'Whether platform dependency is genuine coordination or entrapment mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(job_market_clearinghouse, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jmch_tr_t0, job_market_clearinghouse, theater_ratio, 0, 0.38).
narrative_ontology:measurement(jmch_tr_t5, job_market_clearinghouse, theater_ratio, 5, 0.5).
narrative_ontology:measurement(jmch_tr_t10, job_market_clearinghouse, theater_ratio, 10, 0.62).
narrative_ontology:measurement(jmch_tr_t15, job_market_clearinghouse, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(jmch_be_t0, job_market_clearinghouse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jmch_be_t5, job_market_clearinghouse, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(jmch_be_t10, job_market_clearinghouse, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(jmch_be_t15, job_market_clearinghouse, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(job_market_clearinghouse, resource_allocation).
narrative_ontology:affects_constraint(job_market_clearinghouse, wage_compression_dynamics).
narrative_ontology:affects_constraint(job_market_clearinghouse, credential_inflation_equilibrium).
narrative_ontology:affects_constraint(job_market_clearinghouse, platform_network_effects).

% DUAL FORMULATION NOTE:
% The job market clearinghouse decomposes into three structurally distinct constraints: (1) wage_compression_dynamics (ε≈0.45, Tangled Rope) — whether wage compression reflects market efficiency or employer coordination, (2) credential_inflation_equilibrium (ε≈0.55, Snare) — whether credential requirements reflect skill demand or screening cost reduction, (3) platform_network_effects (ε≈0.60, Snare) — whether platform dependency is technical necessity or strategic lock-in. Each has different ε values and different empirical resolution mechanisms. This story treats the unified clearinghouse mechanism; decomposed stories address specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(job_market_clearinghouse, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
