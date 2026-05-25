% ============================================================================
% CONSTRAINT STORY: skills_based_hiring
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_skills_based_hiring, []).

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
 *   constraint_id: skills_based_hiring
 *   human_readable: Skills-Based Hiring (De-credentialing)
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   Skills-based hiring represents a structural shift in labor market
 *   matching away from educational credentials toward demonstrated
 *   competency. This constraint exhibits the full range of DR classification
 *   types depending on observational position. For tech companies,
 *   de-credentialing solves a genuine coordination problem: credential
 *   inflation has decoupled from actual capability, and hiring for specific
 *   skills enables more efficient talent matching. For traditional
 *   credential-dependent workers, the shift represents extraction through
 *   barrier migration: degrees have become devalued while alternative
 *   pathways (bootcamps, portfolio construction, unpaid internships) impose
 *   higher resource requirements and greater dependence on informal networks.
 *   The constraint is hybrid: it contains genuine coordination value
 *   (reducing credential inflation) but is increasingly laced with extractive
 *   mechanisms (privatized skills assessment, vendor lock-in, barrier
 *   substitution rather than elimination). The measurement trajectory shows
 *   rising theater ratio as skills-based assessment becomes more performative
 *   (interview-based portfolio evaluation, nebulous 'culture fit' signals),
 *   and rising extractiveness as the coordination mechanism is overlaid with
 *   extraction mechanisms (bootcamp vendors, platform credentialing, network
 *   gatekeeping).
 *
 * KEY AGENTS:
 *   - Tech Companies: Primary beneficiaries (powerful/arbitrage) — solve talent matching problem, reduce credential filtering costs, access alternative labor pools
 *   - Credential-Dependent Workers: Primary victims (powerless/trapped) — degrees devalued, face new barrier construction (bootcamps, portfolios, unpaid internships), no exit option
 *   - Historically Excluded Communities: Secondary victims (moderate/constrained) — promised barrier elimination but face resource-intensive alternative barriers; geographic, network, and wealth constraints persist
 *   - Skills-Training Vendors: Beneficiaries + minor extractors (organized/constrained) — capture tuition revenue and user data; coordinate market transition but introduce proprietary assessment dependencies
 *   - Public Workforce Systems: Organized infrastructure (organized/constrained) — attempt to bridge credential devaluation through subsidized retraining; sunset logic applies as alternative credentials mature
 *   - Traditional Universities: Institutional degradation (institutional/constrained) — credential gatekeeping role erodes; institutional inertia maintains degree rituals despite declining market value
 *   - Analytical Observer: Civilizational scope (analytical/analytical) — sees hybrid constraint combining genuine coordination correction with emerging extraction mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(skills_based_hiring, 0.38).
domain_priors:suppression_score(skills_based_hiring, 0.42).
domain_priors:theater_ratio(skills_based_hiring, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(skills_based_hiring, extractiveness, 0.38).
narrative_ontology:constraint_metric(skills_based_hiring, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(skills_based_hiring, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(skills_based_hiring, tangled_rope).
narrative_ontology:human_readable(skills_based_hiring, "Skills-Based Hiring (De-credentialing)").
narrative_ontology:topic_domain(skills_based_hiring, "economic/technological/social").

domain_priors:requires_active_enforcement(skills_based_hiring).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(skills_based_hiring, credential_alternative_workers).
narrative_ontology:constraint_beneficiary(skills_based_hiring, tech_companies).
narrative_ontology:constraint_beneficiary(skills_based_hiring, high_demand_skill_sectors).
narrative_ontology:constraint_victim(skills_based_hiring, traditional_credential_gatekeepers).
narrative_ontology:constraint_victim(skills_based_hiring, credential_dependent_workers).
narrative_ontology:constraint_victim(skills_based_hiring, historically_excluded_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TECH COMPANY (ROPE) — Experiences skills-based hiring as pure coordination. The constraint solves a genuine market problem: credential inflation has decoupled from actual capability. Hiring for demonstrated competency enables efficient talent matching. For tech firms with arbitrage options (can relocate projects, outsource, use contractor pools), de-credentialing is a beneficial coordination mechanism with minimal coercion. Net beneficiary — extraction runs toward this actor.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: CREDENTIAL-DEPENDENT WORKER (SNARE) — Trapped agent experiencing severe extraction. Workers whose human capital is signaled primarily through degrees (liberal arts, humanities, advanced degrees, professional credentials) face devaluation and exclusion. Entry into high-skill sectors now requires portfolio construction, unpaid internships, bootcamp costs, or continuous informal skill demonstration — alternative barriers to traditional credentialing that are more opaque, more resource-intensive, and more dependent on network access. No exit option; bears full cost of credential devaluation. Maximum experienced extraction.
constraint_indexing:constraint_classification(skills_based_hiring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SKILLS-TRAINING COALITION (TANGLED ROPE) — Organized agents (bootcamps, online learning platforms, community colleges, workforce development programs) experience hybrid coordination-extraction. De-credentialing creates genuine market coordination: these programs bridge credential devaluation by providing alternative credentialing mechanisms. But they also extract: workers must pay for skills training, time-to-earnings lengthens, and the coordination mechanism depends on platform vendors maintaining proprietary assessment standards. Beneficiaries (platforms capture tuition and user data) and victims (workers bear costs and curriculum opacity) coexist.
constraint_indexing:constraint_classification(skills_based_hiring, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC WORKFORCE DEVELOPMENT (SCAFFOLD) — Government-funded job retraining and skills certification programs (regional tech hubs, subsidized bootcamps, apprenticeships) see de-credentialing as a temporary mismatch problem with a sunset clause. These programs build alternative credentialing pathways (industry certifications, demonstrated portfolios, apprenticeship models) that aim to reduce dependence on traditional degrees. Theater is moderate: the programs are genuinely building capacity, not just performative. Sunset logic applies: as alternative credentials mature and achieve employer legitimacy, the public subsidy for bridging transitions should decline.
constraint_indexing:constraint_classification(skills_based_hiring, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL UNIVERSITY (PITON) — Institutional actor experiencing constraint as degradation of historical function. Universities were credential gatekeepers; de-credentialing erodes that role. But universities persist in issuing degrees largely through institutional inertia and accreditation ritual. Degree completion has become partially performative — signaling conformity, network access, and delayed labor market entry rather than reliably signaling competency for many fields. Theater ratio rising as degree value declines yet completion persists. Extraction mechanism is weakening (less gate control), but the institution survives through ritual maintenance and prestige signaling in specialized domains.
constraint_indexing:constraint_classification(skills_based_hiring, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICALLY EXCLUDED GROUPS (SNARE) — The de-credentialing narrative promises opportunity without gatekeeping, but structural extraction persists. Skills-based hiring requires portfolio construction, bootcamp costs (often unaffordable without subsidy), geographic mobility to tech hubs, unpaid internship access (requires family wealth to forgo income), and network capital to discover opportunities. Communities with wealth barriers, geographic constraints, and weak network ties to tech sectors experience skills-based hiring as substituting traditional credential barriers with resource barriers that are equally opaque and sometimes more costly. Extraction is high; suppression is severe (resource requirements are structural, not policy-changeable).
constraint_indexing:constraint_classification(skills_based_hiring, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, skills-based hiring is a hybrid constraint. It solves genuine coordination: credential inflation and degree devaluation required market correction. But it introduces new extraction: skills assessment costs are privatized, alternative credentialing creates vendor lock-in, and the shift from transparent credentials (degree attainment) to opaque skills (portfolio quality) increases information asymmetry favoring firms. The constraint exhibits both coordination value (efficient matching) and extraction cost (barrier migration rather than barrier elimination).
constraint_indexing:constraint_classification(skills_based_hiring, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(skills_based_hiring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(skills_based_hiring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(skills_based_hiring, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(skills_based_hiring, TR),
    TR >= 0.70.

:- end_tests(skills_based_hiring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, trending upward. Initial value (0.18) reflects genuine coordination benefit: de-credentialing solves credential inflation. But extractiveness has risen to 0.38 because barrier substitution is occurring faster than barrier elimination. Bootcamp costs, portfolio construction requirements, unpaid internship access, and network gatekeeping impose extraction on workers who cannot afford or navigate alternative pathways. The trajectory suggests extraction will continue rising as vendor lock-in and platform credentialing mature. Suppression (0.42): Moderate. Barriers to skills-based hiring are structural (require portfolio-building capacity, geographic mobility to tech hubs, network access, financial resources to forgo income during training) but not immutable. Some workers can and do transition; public workforce programs are building capacity. But suppression is high for low-income and geographically isolated workers. Theater ratio (0.55): Moderate, trending upward. Initial skills assessment was more transparent (demonstrate actual capability in portfolio), but assessment has become increasingly performative: interview-based portfolio evaluation, nebulous 'cultural fit' signals, firm-specific assessment tools with opacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (credential devaluation + skills-based hiring) produces contradictory classifications from different positions. Tech companies experience Rope because they have exit options and benefit from reduced friction. Credential-dependent workers experience Snare because they have no exit and bear the cost of barrier migration. The core disagreement is over whether skills-based hiring eliminates gatekeeping (Rope perspective — true opening) or substitutes gatekeeping (Snare perspective — barrier relocation with higher cumulative cost). The analytical observer sees Tangled Rope because both effects are real: genuine coordination correction + emerging extraction mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is determined by their structural position relative to credential gatekeeping and skills assessment. Tech companies benefit from reduced credentialing friction and expanded talent pools — they have arbitrage options (hire globally, use contractors) and low exit costs. Their d-value is low, producing negative effective extraction (they gain). Credential-dependent workers lose credential value with no viable alternative path — trapped exit option, victim status, d-value near 1.0, maximum experienced extraction. Historically excluded groups are constrained but not fully trapped: some can access bootcamp subsidies or enter lower-credential sectors, but resource barriers are structural. Their d-value is moderate-high (0.70-0.85), producing high experienced extraction. Platforms are constrained by market competition but benefit from vendor lock-in — moderate d-value (0.45-0.55), moderate experienced extraction (they extract from workers but face competitive pressure). Public workforce systems are organized with mobile exit (can pivot to other workforce needs), constrained by funding cycles — d-value moderate (0.40-0.55), experienced extraction moderate but declining as alternatives mature.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint avoids mandatrophy by acknowledging hybrid extraction-coordination. The initial framing (skills-based hiring as pure Rope — barrier elimination) was a false natural law. The actual constraint is Tangled Rope: it solves credential inflation (genuine coordination value) but introduces privatized skills assessment, vendor lock-in, and barrier substitution (extraction mechanisms). The mandatrophy is resolved by recognizing that 'de-credentialing' conflates two structurally distinct claims: (1) reducing credential inflation (real coordination benefit, ε ≈ 0.08, Rope), and (2) implementing skills-based assessment as the replacement mechanism (extractive due to privatization and resource barriers, ε ≈ 0.45, Tangled Rope). These should be decomposed into separate constraint stories if the goal is to distinguish the genuine coordination value from the extraction overlay. The current story models them unified because they are institutionally coupled: you cannot solve credential inflation without implementing some alternative assessment mechanism, and all implemented mechanisms so far have introduced new extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    barrier_substitution_vs_elimination,
    'Does skills-based hiring eliminate educational barriers or merely substitute transparent credentials with opaque, resource-intensive barriers?',
    'Longitudinal comparison of labor market entry costs: degree-path (tuition + time) vs skills-path (bootcamp + portfolio construction + unpaid internships). Demographic composition of both pathways. Earnings trajectories at 5-year, 10-year marks.',
    'If elimination: constraint is Rope from most perspectives. If substitution: constraint is Snare for resource-constrained groups and Tangled Rope system-wide.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(barrier_substitution_vs_elimination, empirical, 'Whether skills-based hiring eliminates or substitutes educational barriers').

omega_variable(
    portfolio_quality_verification,
    'Can employers reliably assess portfolio quality and skills without credential signaling, or does portfolio assessment suffer from hidden information and adverse selection?',
    'Analysis of hiring decision reversals: do portfolio-hired employees experience higher separation rates, lower tenure-weighted productivity, or faster promotion than degree-credentialed peers? Audits of portfolio assessment practices across firms.',
    'If reliable: skills-based hiring reduces information asymmetry (Rope). If unreliable: firms revert to proxy signals (networks, interview performance, educational background), and barriers persist (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portfolio_quality_verification, empirical, 'Whether portfolio assessment can reliably substitute for credential signaling').

omega_variable(
    network_capital_substitution,
    'In the absence of transparent degree credentials, does skills-based hiring increase or decrease dependence on informal network capital for job discovery and insider referrals?',
    'Job sourcing analysis: percentage of positions filled via direct application vs referral, comparing degree-based vs skills-based firms. Network homogeneity analysis of hired cohorts. Comparison of diversity outcomes.',
    'If increased network dependence: skills-based hiring substitutes structural barriers (degree gatekeeping) with relational barriers (network gatekeeping), reducing accessibility for outsiders. Snare for excluded groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_capital_substitution, empirical, 'Whether skills-based hiring increases network capital dependence').

omega_variable(
    alternative_credential_vendor_lock,
    'Do alternative credentialing vendors (bootcamp platforms, online learning systems, industry certification bodies) achieve employer legitimacy independently, or does legitimacy depend on sustained vendor investment and marketing?',
    'Longitudinal analysis of employer acceptance: which certifications persist without vendor marketing subsidy. Cross-firm variation in which platforms they hire from. Discontinuation of certifications when vendor support ends.',
    'If independent legitimacy: Scaffold sunset is real, and public workforce investment can transition to commodity learning. If vendor-dependent: constraint becomes extractive (Snare for workers dependent on platform legitimacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_vendor_lock, empirical, 'Whether alternative credentials achieve vendor-independent legitimacy').

omega_variable(
    disparity_in_portfolio_access,
    'What is the structural disparity in portfolio-building access between high-income and low-income workers?',
    'Cost analysis: bootcamp tuition, opportunity costs of unpaid internships, geographic mobility for hub access, tool/software subscriptions. Comparison of portfolio quality metrics achieved across income cohorts. Hiring outcomes stratified by initial income quartile.',
    'If disparity > traditional degree path: skills-based hiring substitutes transparent economic barriers (tuition) with hidden cumulative barriers (unpaid time, relocation, network). Snare for low-resource groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disparity_in_portfolio_access, empirical, 'Structural disparity in portfolio-building access across income cohorts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(skills_based_hiring, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sbh_tr_t0, skills_based_hiring, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sbh_tr_t5, skills_based_hiring, theater_ratio, 5, 0.45).
narrative_ontology:measurement(sbh_tr_t10, skills_based_hiring, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(sbh_be_t0, skills_based_hiring, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sbh_be_t5, skills_based_hiring, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(sbh_be_t10, skills_based_hiring, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(skills_based_hiring, resource_allocation).
narrative_ontology:affects_constraint(skills_based_hiring, credential_inflation).
narrative_ontology:affects_constraint(skills_based_hiring, bootcamp_labor_market_integration).
narrative_ontology:affects_constraint(skills_based_hiring, interview_bias_and_assessment_opacity).

% DUAL FORMULATION NOTE:
% Skills-based hiring is downstream of credential inflation constraint (broader problem: degrees no longer reliably signal competency). The de-credentialing shift affects multiple downstream constraints: bootcamp integration into legitimate labor market pathways, interview-based assessment opacity, and platform credentialing vendor dependency. Decomposition into three stories would distinguish: (1) credential inflation as root cause (Mountain/Rope), (2) skills-based hiring as coordination mechanism (Rope component), and (3) implementation via privatized platforms as extraction mechanism (Tangled Rope/Snare component).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(skills_based_hiring, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
