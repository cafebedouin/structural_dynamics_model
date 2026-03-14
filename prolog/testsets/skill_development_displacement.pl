% ============================================================================
% CONSTRAINT STORY: skill_development_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_skill_development_displacement, []).

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
 *   constraint_id: skill_development_displacement
 *   human_readable: Skill Development Displacement in Labor Market Transitions
 *   domain: labor_economics/human_capital
 *
 * SUMMARY:
 *   Skill development displacement occurs when labor market transitions
 *   create barriers to worker retraining and credential acquisition that
 *   extract value from displaced workers while concentrating benefits among
 *   employers and high-credential holders. The constraint operates at the
 *   intersection of labor market coordination (matching workers to skill
 *   requirements) and extractive rent-seeking (credential inflation and
 *   barrier maintenance). Displaced workers face suppression mechanisms
 *   including lack of income support during retraining, geographic
 *   immobility, age discrimination, and rising credential requirements that
 *   outpace actual skill needs. Extractiveness has increased over 15 years as
 *   credential inflation accelerated and retraining program funding failed to
 *   keep pace with labor market velocity. Alternative credentialing platforms
 *   (bootcamps, micro-credentials) represent a scaffold structure with sunset
 *   potential — if these platforms can displace traditional degree
 *   requirements, the constraint's extraction mechanism loses force. The
 *   theater ratio remains moderate (0.48) because while retraining programs
 *   involve genuine skill development (low theater), traditional
 *   credentialing increasingly decouples from actual job competency (moderate
 *   theater). The constraint is diagnosed as Tangled Rope because it contains
 *   both genuine coordination (skills matching, labor market adjustment) and
 *   asymmetric extraction (workers bear transition costs, employers capture
 *   gains).
 *
 * KEY AGENTS:
 *   - Displaced Workers: Primary victims (powerless/trapped) — face skill obsolescence with minimal exit options; retraining is underfunded and lengthy with no income guarantee
 *   - Incumbent Employers: Primary beneficiaries (institutional/arbitrage) — benefit from labor supply adjustment and credential signaling without bearing transition costs; extract value through wage suppression during surplus periods
 *   - High-Skill Credentialed Workers: Secondary beneficiaries (powerful/mobile) — extract value from credential scarcity and benefit from reduced wage competition as lower-credentialed workers are displaced
 *   - Retraining Programs: Institutional actors (institutional/constrained) — coordinate labor supply adjustment but extract through opportunity costs and prolonged dependency; suppression mechanisms limit uptake and completion
 *   - Traditional Credentialing Bodies: Institutional actors (institutional/constrained) — maintain authority through inertia despite erosion of predictive value; theater ratio reflects performative verification
 *   - Alternative Credentialing Platforms: Organized actors (organized/constrained) — provide lower-cost exit pathways with genuine sunset mechanics; represent coordination alternative with lower overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(skill_development_displacement, 0.58).
domain_priors:suppression_score(skill_development_displacement, 0.65).
domain_priors:theater_ratio(skill_development_displacement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(skill_development_displacement, extractiveness, 0.58).
narrative_ontology:constraint_metric(skill_development_displacement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(skill_development_displacement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(skill_development_displacement, tangled_rope).
narrative_ontology:human_readable(skill_development_displacement, "Skill Development Displacement in Labor Market Transitions").
narrative_ontology:topic_domain(skill_development_displacement, "labor_economics/human_capital").

domain_priors:requires_active_enforcement(skill_development_displacement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(skill_development_displacement, incumbent_employers).
narrative_ontology:constraint_beneficiary(skill_development_displacement, high_skill_credential_holders).
narrative_ontology:constraint_victim(skill_development_displacement, displaced_workers).
narrative_ontology:constraint_victim(skill_development_displacement, skill_acquisition_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKER (SNARE) — Faces skill obsolescence with minimal exit options. Retraining programs are underfunded, lengthy, and provide no income guarantee during transition. Labor market barriers (age discrimination, credential requirements, geographic immobility) make exit nearly impossible. Worker bears full extraction cost with suppression mechanisms preventing alternative pathways.
constraint_indexing:constraint_classification(skill_development_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RETRAINING PARTICIPANT (TANGLED ROPE) — Constrained by financial barriers, time costs, and outcome uncertainty; retraining programs coordinate labor supply adjustment but extract value through opportunity costs and credential inflation. Participants benefit from skill development infrastructure while bearing disproportionate risk and cost of the transition.
constraint_indexing:constraint_classification(skill_development_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT EMPLOYERS AND CREDENTIAL SYSTEMS (ROPE) — Experience the constraint as coordination: efficient labor market clearing, skills matching, and credential signaling. Benefit from worker retraining through labor supply adjustment without bearing transition costs. Can arbitrage between regions, sectors, and credential tiers with low friction.
constraint_indexing:constraint_classification(skill_development_displacement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADITIONAL SKILLS CERTIFICATION BODIES (PITON) — Maintain licensing and credentialing authority through institutional inertia despite erosion of their actual predictive value. Theater ratio (0.48) reflects that many traditional credentials no longer correlate with job performance or provide genuine verification of competency. Persist through legal requirement and career path dependence, not through functional necessity.
constraint_indexing:constraint_classification(skill_development_displacement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE CREDENTIALING PLATFORMS (SCAFFOLD) — Bootcamps, micro-credentials, competency-based platforms (Coursera, LinkedIn Learning) provide lower-cost pathways with shorter timelines and reduced theater. See the traditional constraint as temporary — alternative verification methods (portfolio assessment, practical demonstration, employer endorsement) are creating exit paths. Extractiveness is tempered by genuine coordination function and visible sunset mechanisms.
constraint_indexing:constraint_classification(skill_development_displacement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HIGH-SKILL CREDENTIALED WORKERS (TANGLED ROPE) — Mobile across labor markets; credentials serve as coordination mechanism enabling career mobility. Also extract value from credential scarcity and signaling advantage. Benefit from the displacement of lower-credentialed workers (reduces wage competition) while contributing to the skill development constraint through credential inflation cycles.
constraint_indexing:constraint_classification(skill_development_displacement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (skills matching, labor supply adjustment) embedded within asymmetric extraction (workers bear transition costs, employers capture productivity gains). The constraint is neither pure coordination nor pure extraction but a genuine hybrid. Civilizational scope reveals multi-generational credential inflation cycles that compound extraction over time.
constraint_indexing:constraint_classification(skill_development_displacement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(skill_development_displacement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(skill_development_displacement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(skill_development_displacement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(skill_development_displacement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(skill_development_displacement, TR),
    TR >= 0.70.

:- end_tests(skill_development_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from displaced workers through suppression mechanisms (lack of income support, credential inflation, age discrimination, geographic barriers) while concentrating benefits among employers and high-skill workers. The 15-year upward trajectory (0.35 → 0.58) reflects credential inflation accelerating faster than retraining infrastructure improvements. Suppression (0.65): High. Structural barriers are substantial: retraining costs (tuition, opportunity cost of foregone wages), time requirements (2-4 years for degree vs weeks for bootcamp), age discrimination, geographic immobility, and psychological barriers to mid-career transition. Theater ratio (0.48): Moderate. Retraining programs involve genuine skill development (low theater), but traditional credentialing has increasing disconnect from job requirements (moderate-high theater). Credential inflation has outpaced task complexity in many fields; employers use degrees as screening mechanisms rather than competency verification. The moderate theater reflects that some genuine learning occurs within a system that also maintains barriers for non-skill reasons.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates marked divergence between beneficiary and victim perspectives. Displaced workers experience the constraint as a snare with trap-like properties: the more they try to escape (enrolling in retraining), the more the constraint extracts (opportunity costs, tuition, time away from income). Employers experience the same constraint as rope — efficient labor market clearing. The gap reveals that the coordination function (skills matching) is real but serves primarily the interests of employers and high-skill workers, while extracting heavily from those transitioning between labor market roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions within the constraint. Displaced workers (powerless/trapped) have high d (~0.95) because they are structural targets with no exit capacity — the constraint extracts maximum value. Retraining participants (moderate/constrained) have d ~0.65 because they face high but surmountable costs to exit — they have some agency but significant barriers. Employers (institutional/arbitrage) have low d (~0.10) because they can access labor across multiple credential markets and geographic regions — they are structural beneficiaries. High-skill workers (powerful/mobile) have low-moderate d (~0.35) because they benefit from credential scarcity (concentration of value) but also share exposure to credential inflation as a norm setter. Traditional credentialing bodies (institutional/constrained) have moderate d (~0.45) because they maintain authority through institutional inertia but lack true arbitrage capacity — they cannot easily exit their role without losing legitimacy. Alternative credential platforms (organized/constrained) have moderate d (~0.40) because they face barriers to market penetration but retain genuine exit/competition options.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT RESOLVES AS TANGLED ROPE: This constraint satisfies all three gates for tangled rope classification: (1) Genuine coordination function present: skills matching, labor supply adjustment, credential signaling reduce friction in labor market transitions; (2) Asymmetric extraction present: displaced workers bear transition costs (foregone income, tuition, time, psychological stress) while employers capture productivity gains without bearing adjustment costs; (3) Active enforcement required: credential requirements, degree gates, licensing laws maintain the constraint through institutional mechanisms. The mandatrophy is resolved by recognizing that the apparent naturalness of 'you need credentials to get jobs' naturalizes what is actually an enforced institutional arrangement. The credential inflation trajectory shows the extraction mechanism strengthening over time — credentials that once signaled real competency now function as pure barriers, but the enforcement mechanisms (employer hiring practices, professional licensing, degree requirements) persist. The alternative credentialing platforms (scaffold) represent the real exit route, which will become viable if they can prove equivalent or superior competency signals at lower cost. The constraint is neither pure coordination (benefits are asymmetric) nor pure extraction (real coordination occurs), but a genuine hybrid where the coordination function has become increasingly subordinate to the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retraining_program_effectiveness,
    'Do subsidized retraining programs genuinely improve long-term employment outcomes or merely shuffle workers between low-wage sectors?',
    'Longitudinal wage tracking 5+ years post-completion; comparison of program completers vs matched non-participants; sector mobility analysis of graduates',
    'If effective: constraint is higher-quality Rope/Scaffold (coordination with real outcomes). If ineffective: constraint is higher-extractiveness Snare (displacement channeled through theater of retraining).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retraining_program_effectiveness, empirical, 'Effectiveness of retraining programs in improving long-term outcomes').

omega_variable(
    credential_inflation_accumulation,
    'Is rising credential requirements a response to genuine skill gaps or does it function primarily as rent-seeking and credential inflation?',
    'Analysis of job posting requirements vs task complexity over time; worker productivity metrics by education level; wage premium decline for credentials holding task constant',
    'If skill-driven: extraction is moderate (justified by needs). If rent-seeking-driven: extraction increases substantially (unjustified credential barriers amplify snare dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_accumulation, empirical, 'Whether credential requirements reflect genuine skill needs or rent-seeking').

omega_variable(
    alternative_credentialing_viability,
    'Can micro-credentials and competency-based platforms (bootcamps, digital certificates) actually displace traditional degree requirements or are they relegated to secondary labor markets?',
    'Wage and career progression comparison for alternative-credential vs traditional-degree holders in same role; hiring manager acceptance analysis; salary premium retention analysis',
    'If viable: scaffold perspective confirmed — genuine sunset mechanism exists. If confined to secondary markets: alternative credentials remain lower-status, perpetuating traditional credential requirement barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Whether alternative credentials can displace traditional credentials').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.65) primarily structural (material barriers to retraining access) or internalized (workers internalize credential requirements as necessary)?',
    'Survey analysis of worker perception of necessity vs actual barrier cost; counterfactual behavior if barriers were removed; cultural shift analysis as alternative credentials gain acceptance',
    'If structural: removing barriers (subsidized retraining, income support) directly reduces suppression. If internalized: workers carry suppression with them post-exit; requires narrative shift alongside material support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    generational_accumulation,
    'Does credential inflation compound across generations (children of displaced workers face higher credential barriers) or is displacement reset between generations?',
    'Intergenerational mobility analysis; credential requirement escalation across cohorts; parental displacement effect on child educational attainment and wage outcomes',
    'If compounding: extractiveness increases with each generation; constraint should be reclassified as high-intensity Snare. If reset: constraint remains Tangled Rope with moderate per-generation extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_accumulation, empirical, 'Whether credential inflation compounds across generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(skill_development_displacement, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skd_tr_t0, skill_development_displacement, theater_ratio, 0, 0.32).
narrative_ontology:measurement(skd_tr_t5, skill_development_displacement, theater_ratio, 5, 0.4).
narrative_ontology:measurement(skd_tr_t10, skill_development_displacement, theater_ratio, 10, 0.48).
narrative_ontology:measurement(skd_tr_t15, skill_development_displacement, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(skd_be_t0, skill_development_displacement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(skd_be_t5, skill_development_displacement, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(skd_be_t10, skill_development_displacement, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(skd_be_t15, skill_development_displacement, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(skill_development_displacement, resource_allocation).
narrative_ontology:affects_constraint(skill_development_displacement, labor_market_wage_suppression).
narrative_ontology:affects_constraint(skill_development_displacement, credential_inflation_rent_seeking).
narrative_ontology:affects_constraint(skill_development_displacement, geographic_mobility_barriers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(skill_development_displacement, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
