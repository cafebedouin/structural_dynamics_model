% ============================================================================
% CONSTRAINT STORY: sotu_1995_clinton_new_covenant_skills_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1995_clinton_new_covenant_skills_framework, []).

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
 *   constraint_id: sotu_1995_clinton_new_covenant_skills_framework
 *   human_readable: Clinton's New Covenant: Skills Framework for Information Economy Access
 *   domain: education/labor_policy/social_contract
 *
 * SUMMARY:
 *   The New Covenant, articulated in President Clinton's 1995 State of the
 *   Union address and subsequent policy framework, represents a fundamental
 *   reframing of the American social contract around individual human-capital
 *   investment in response to economic transition toward the information
 *   economy. Rather than a traditional safety net that protects workers from
 *   technological displacement through income maintenance, the New Covenant
 *   conditions public support on demonstrable effort to develop skills and
 *   adaptive learning capacity. The Government commits to providing
 *   educational tools, training infrastructure, and retraining access;
 *   individuals commit to continuous skill development and labor-market
 *   adaptation. This constraint exhibits significant structural tension: it
 *   provides genuine coordination benefits (expanded access to education and
 *   retraining) while simultaneously extracting individual risk-assumption
 *   and responsibility that was previously collectivized through union
 *   representation and welfare protections. The constraint is tangled rope
 *   from most institutional perspectives because both the coordination and
 *   extraction functions are essential to its operation — it cannot function
 *   as pure coordination without the individual responsibility mechanism, and
 *   it cannot function as pure extraction without some genuine training
 *   benefits. However, from the perspective of structurally displaced or
 *   low-skill workers, the constraint appears as snare: the exit from the
 *   retraining obligation means forfeiting safety-net access, creating a
 *   trapped position where acceptance of individual responsibility is
 *   non-negotiable. The theater ratio reflects the growing importance of
 *   skills certification, competency attestation, and workforce development
 *   metrics that may not correlate with actual labor-market outcomes.
 *
 * KEY AGENTS:
 *   - Structurally Displaced Workers: Primary victims (powerless/trapped) — bear individual responsibility for skill transition without collective risk-pooling; face geographic and age-based barriers to retraining success
 *   - Workers with Retraining Capacity: Secondary agents (moderate/constrained) — benefit from training access but bear transition risk and opportunity costs; constrained by relocation and family obligations
 *   - Employers and Industry: Primary beneficiaries (institutional/arbitrage) — gain access to skilled workforce without bearing training costs; can arbitrage to other jurisdictions if domestic skill supply is insufficient
 *   - Labor Unions and Worker Advocacy: Organized agents (organized/mobile) — lose collective bargaining protection but gain retraining infrastructure; see individual competition replacing collective risk-pooling
 *   - Community Colleges and Training Institutions: Implementers (organized/constrained) — tasked with execution; benefit from increased funding but constrained by outcome accountability metrics
 *   - Welfare Administration Bureaucracy: Pre-existing infrastructure transitioning to new function (institutional/arbitrage) — maintains power to gate access but actual income-support function is nominally replaced
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a policy choice (individualized responsibility) as an economic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1995_clinton_new_covenant_skills_framework, 0.52).
domain_priors:suppression_score(sotu_1995_clinton_new_covenant_skills_framework, 0.48).
domain_priors:theater_ratio(sotu_1995_clinton_new_covenant_skills_framework, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1995_clinton_new_covenant_skills_framework, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1995_clinton_new_covenant_skills_framework, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1995_clinton_new_covenant_skills_framework, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1995_clinton_new_covenant_skills_framework, tangled_rope).
narrative_ontology:human_readable(sotu_1995_clinton_new_covenant_skills_framework, "Clinton's New Covenant: Skills Framework for Information Economy Access").
narrative_ontology:topic_domain(sotu_1995_clinton_new_covenant_skills_framework, "education/labor_policy/social_contract").

domain_priors:requires_active_enforcement(sotu_1995_clinton_new_covenant_skills_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1995_clinton_new_covenant_skills_framework, employers_skilled_workforce_access).
narrative_ontology:constraint_beneficiary(sotu_1995_clinton_new_covenant_skills_framework, high_skill_workers_advanced_training).
narrative_ontology:constraint_beneficiary(sotu_1995_clinton_new_covenant_skills_framework, government_fiscal_discipline).
narrative_ontology:constraint_victim(sotu_1995_clinton_new_covenant_skills_framework, displaced_workers_transition_burden).
narrative_ontology:constraint_victim(sotu_1995_clinton_new_covenant_skills_framework, low_skill_workers_ongoing_inadequacy).
narrative_ontology:constraint_victim(sotu_1995_clinton_new_covenant_skills_framework, collective_social_safety_net).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURALLY DISPLACED WORKER (SNARE) — Faces the New Covenant as pure extraction with no exit. Cannot refuse the retraining responsibility without forfeiting safety-net access. Must accept individual human-capital burden that was previously collectivized. Geographic immobility, family obligations, and age-related hiring discrimination make retraining costly and its success uncertain. Effective extraction chi is high because this agent has no arbitrage option — must accept the terms or fall below subsistence.
constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKER WITH RETRAINING CAPACITY (TANGLED ROPE) — Moderate power agent who can access retraining and benefits from skills investment (genuine coordination function) but bears individual responsibility for success and career transition risk. Exit is constrained by opportunity cost (foregone wages during training, geographic relocation requirements) and social stigma of failed retraining. Extraction is real but not total — some agents genuinely benefit from government training investment and move to higher-value work. The constraint coordinates skill development with labor market needs while extracting individual risk assumption.
constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYERS AND INDUSTRY (ROPE) — Institutional beneficiary with arbitrage access. Gain direct access to a skilled workforce without bearing training costs. The constraint is coordination from their perspective: Government-funded education and retraining solves the market failure of undersupply of skilled workers. Employers experience this as a subsidy — the social cost of workforce adaptation is borne collectively, benefits flow to employers as lower hiring costs and reduced internal training investment. Can exit (relocate to countries with lower-cost skilled labor) but retain full coordination benefits within the domestic framework.
constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNIONS AND WORKER ADVOCACY (TANGLED ROPE) — Organized agents see the New Covenant as a restructuring that simultaneously provides genuine training access (coordination) while shifting risk from collective bargaining protection to individual human-capital competition (extraction). Mobile exit option (can organize outside the constraint framework through wage negotiations, sectoral agreements) but also benefit from retraining infrastructure. The constraint dissolves collective bargaining power in exchange for individual opportunity. Classification is tangled rope because both coordination (access to training) and asymmetric extraction (loss of collective risk-pooling) are structural.
constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COMMUNITY COLLEGES AND TRAINING INSTITUTIONS (SCAFFOLD) — Institutional actors tasked with implementing the New Covenant framework. Benefit from increased government funding for skills training (genuine coordination benefit) but constrained by accountability metrics and workforce outcomes requirements (extraction via performance pressure). The constraint is temporarily supported — assumes that government investment in retraining remains sufficient. As funding cycles tighten or labor markets shift, the institutional infrastructure degrades. Sunset logic: if labor market shifts faster than training can respond, the framework collapses and institutions are left with underutilized capacity and dissolved purpose.
constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: WELFARE ADMINISTRATION AND BUREAUCRACY (PITON) — Pre-existing institutional infrastructure (social safety-net administration) transitions to a new function (skills-testing and retraining gatekeeping) but much of the work becomes performative. Measuring skill development, certifying competency, and verifying 'effort to adapt' requires theater — attestations, assessments, documentation that may not correlate with actual labor-market outcomes. The bureaucracy persists through institutional inertia and path-dependence; its core function (income support) is nominally replaced by its new function (human-capital enforcement) but much capacity is spent on verification ritual rather than actual outcome measurement. Theater ratio high because burden-of-proof shifts to the individual to demonstrate adaptation effort.
constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECONOMIC STRUCTURAL CHANGE (MOUNTAIN) — From a civilizational and global perspective, the reframing toward human-capital investment in response to information economy shift appears as natural law: technological change structurally requires workforce skill upgrading; the risk of economic obsolescence is inherent to rapid technological transition; individuals must adapt or fall behind. This perspective sees the New Covenant as recognition of an immutable fact: the social contract cannot protect workers from technological displacement; only continuous learning can. However, the structural data reveals this as a false summit: the 'necessity' of individualized human-capital responsibility naturalizes what is a policy choice about how to distribute the costs and risks of technological transition.
constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1995_clinton_new_covenant_skills_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1995_clinton_new_covenant_skills_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1995_clinton_new_covenant_skills_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1995_clinton_new_covenant_skills_framework, TR),
    TR >= 0.70.

:- end_tests(sotu_1995_clinton_new_covenant_skills_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint restructures who bears the cost of workforce adaptation. Pre-New Covenant, risk was partially collectivized through union bargaining, unemployment insurance, and welfare protections. Post-New Covenant, risk is individualized — workers must invest time and foregone wages in retraining; failure to adapt results in downward mobility rather than income maintenance. The extractiveness reflects this risk-transfer but is not as severe as pure snare (0.66+) because some genuine coordination benefits exist: expanded government funding for education and retraining creates real opportunities that didn't exist under purely market-driven training. However, for structurally displaced and low-skill workers, the benefit-cost asymmetry is severe. Suppression (0.48): Moderate. The constraint involves significant barriers: opportunity cost of retraining (foregone wages, family obligations), geographic mobility requirements, age discrimination in hiring, credentialing gaps that don't correlate with actual skill. But suppression is not total (not 0.60+) because workers retain formal choice — they can technically refuse retraining, though doing so means forfeiting safety-net support. The suppression is partly structural (real barriers) and partly internalized (acceptance of individual responsibility framing). Theater ratio (0.58): Moderate-high. The New Covenant framework requires measurement and certification of workforce development, skills training completion, and demonstrated learning. These metrics grow in importance over the interval as accountability pressure increases. Much of the theater reflects the difficulty of measuring actual skill acquisition and labor-market readiness from credentials and transcripts. Actual occupational competence is tacit and context-dependent; formal credentials provide theater that approximates real skill.
 *
 * PERSPECTIVAL GAP:
 *   The New Covenant generates one of the widest perspectival gaps in policy constraints. From the employer perspective, the constraint is coordination — the government is solving a market failure (undersupply of skilled labor) by funding training, allowing firms to access a skilled workforce at lower cost. From the displaced worker perspective, the constraint is extraction — the government is reducing its obligation to provide income protection and shifting risk to workers who lack the resources to bear it. From the labor union perspective, the constraint is loss of power — the shift from collective bargaining to individual skill-based competition undermines union leverage. From the training institution perspective, the constraint is opportunity and then burden — increased funding creates growth but accountability metrics and outcome requirements create performance pressure. From the analytical perspective that sees technological change as inevitable, the constraint is mountain — a recognition of natural economic law. But the false-summit detector identifies this as naturalization: the choice to individualize responsibility is policy, not physics. The perspectival gap reveals that the New Covenant's framing ('opportunity for all who will develop skills') obscures a distribution of costs and benefits that is highly asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across the agent spectrum. For structurally displaced workers, d approaches 1.0 (full target of extraction): they are victims of technological displacement, lose collective protection, and are forced to accept individual responsibility for adaptation. For employers, d approaches 0.0 (full beneficiary): they receive a government-subsidized skilled workforce without bearing training costs. For moderate workers with retraining capacity, d ≈ 0.55 (mixed): they benefit from training access but bear transition costs. For organized labor, d ≈ 0.45 (slight beneficiary of training but victim of collective bargaining loss). The perspectival gap reflects this directionality distribution: agents with low d (high beneficiary position) perceive rope or scaffold; agents with high d (victimhood position) perceive snare; institutional agents perceive tangled rope based on their exit options and beneficiary status. The engine derives d from beneficiary/victim declarations and exit options; the high perspectival variance in this constraint reflects real heterogeneity in structural positions within a nominally uniform policy framework.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATH: The mandatrophy in the New Covenant is resolved by recognizing that it genuinely is a tangled rope — not a hidden snare that should be reclassified, nor a hidden rope that should be celebrated. The constraint provides real coordination benefits (expanded access to education and retraining infrastructure) while simultaneously shifting individual risk-bearing (extraction of responsibility from the collective). Both functions are essential to the constraint's operation. The tension is not resolved but managed through perspectival indexing. From the perspective of high-skill workers and employers, the constraint is rope: they benefit from the coordination without bearing extraction. From the perspective of displaced workers, the constraint is snare: they bear extraction (responsibility without adequate support) without sufficient coordination benefit. The mandatrophy dissolves not by choosing a single type but by accepting that the constraint operates differently for different agents, and that this differential operation is precisely the source of its political tension. The false-summit perspective (analytical/civilizational) naturalizes the constraint as inevitable economic law; this is methodologically problematic because it obscures the policy choices embedded in the framework. The constraint is tangled rope because it authentically combines coordination (skill development infrastructure) with extraction (risk transfer), not because of measurement ambiguity. The classification is stable across measurement methodologies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retraining_success_rates_heterogeneity,
    'Do retraining outcomes depend primarily on individual effort and aptitude (justifying New Covenant framing) or on unobservable structural factors (age, race, geographic opportunity) that make individual responsibility attribution unjust?',
    'Longitudinal tracking of retraining program participants; regression analysis controlling for demographic factors; comparison of outcomes across different retraining modalities and geographic contexts',
    'If individual factors dominate: individual responsibility framing is justified and extraction is moderate. If structural factors dominate: individual responsibility is fiction and extraction is severe (snare classification becomes more appropriate across more perspectives). Risk: attribution error naturalizes structural inequality as individual failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retraining_success_rates_heterogeneity, empirical, 'Whether retraining outcomes depend on individual effort or structural factors').

omega_variable(
    skill_obsolescence_velocity,
    'At what rate do skills depreciate in the information economy? Is the retraining cycle (every 5 years? 10 years? 20 years?) feasible within a continuous-learning framework?',
    'Historical analysis of skill-half-life in major occupational categories; tracking of job tenure and wage progression for workers with multiple retraining events; comparison with pre-information-economy labor market stability',
    'If skills remain valuable for 15+ years: periodic retraining is feasible and New Covenant extraction is moderate. If skills obsolete within 5 years: continuous retraining becomes a treadmill and extraction becomes severe (snare behavior).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_obsolescence_velocity, empirical, 'Rate of skill obsolescence in information economy').

omega_variable(
    government_commitment_sustainability,
    'Is the Government''s commitment to funding training infrastructure genuinely binding, or does the New Covenant framework reduce welfare spending while leaving the obligation to individual workers as residual?',
    'Comparative analysis of government spending on education and retraining across business cycles; tracking of funding as share of GDP and as absolute dollars; correlation with unemployment rates and worker displacement events',
    'If funding is sustained countercyclically: coordination function is real and constraint is tangled rope. If funding contracts during recessions or fiscal stress: individual responsibility remains while government support disappears, creating snare behavior.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_commitment_sustainability, empirical, 'Sustainability of government training investment commitment').

omega_variable(
    collective_bargaining_power_compensation,
    'Do workers who lose collective bargaining protection (through union decline or gig economy transition) recover equivalent risk-pooling through individual retraining access, or does the constraint create a net loss of worker security?',
    'Comparison of wage volatility, income stability, and benefit coverage before and after New Covenant implementation; analysis of workers with and without union representation; tracking of career interruption costs',
    'If retraining access compensates: tangled rope classification stands (coordination and extraction balance). If it doesn''t: the constraint is primarily extractive (snare from more perspectives) and the reframing is a false trade-off.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_bargaining_power_compensation, empirical, 'Whether retraining access compensates for lost collective bargaining').

omega_variable(
    false_summit_naturalization,
    'Is the shift to individual human-capital responsibility a recognition of inevitable technological change, or a policy choice that naturalizes what could be collectivized differently (sectoral adjustment funds, wage insurance, worker buyouts)?',
    'Comparative institutional analysis across OECD countries; examination of alternative policy designs for managing technological displacement; cost-benefit analysis of individualized vs collective risk-pooling models',
    'If choice is inevitable: mountain classification appropriate; New Covenant is recognizing reality. If choice is contingent: false summit detected; reframing is policy that benefits risk-transferors and disadvantages risk-bearers. Creates major perspectival gap between those who benefit from individualized responsibility (employers, high-skill workers) and those who bear costs (displaced, low-skill workers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether individual responsibility is inevitable or a policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1995_clinton_new_covenant_skills_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(covenant_tr_t0, sotu_1995_clinton_new_covenant_skills_framework, theater_ratio, 0, 0.42).
narrative_ontology:measurement(covenant_tr_t3, sotu_1995_clinton_new_covenant_skills_framework, theater_ratio, 3, 0.52).
narrative_ontology:measurement(covenant_tr_t6, sotu_1995_clinton_new_covenant_skills_framework, theater_ratio, 6, 0.61).
narrative_ontology:measurement(covenant_tr_t9, sotu_1995_clinton_new_covenant_skills_framework, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(covenant_be_t0, sotu_1995_clinton_new_covenant_skills_framework, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(covenant_be_t3, sotu_1995_clinton_new_covenant_skills_framework, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(covenant_be_t6, sotu_1995_clinton_new_covenant_skills_framework, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(covenant_be_t9, sotu_1995_clinton_new_covenant_skills_framework, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1995_clinton_new_covenant_skills_framework, resource_allocation).
narrative_ontology:affects_constraint(sotu_1995_clinton_new_covenant_skills_framework, union_decline_information_economy).
narrative_ontology:affects_constraint(sotu_1995_clinton_new_covenant_skills_framework, welfare_reform_individual_responsibility).
narrative_ontology:affects_constraint(sotu_1995_clinton_new_covenant_skills_framework, skills_gap_employer_demand_mismatch).

% DUAL FORMULATION NOTE:
% The New Covenant coordinates workforce development (resource allocation) while simultaneously extracting individual responsibility for adaptation. Upstream constraints include technological displacement (inevitable but distributed unequally) and labor-market structural change (shift toward service and information work). Downstream constraints include welfare reform implementation (which operationalizes the individual responsibility framing), union decline (which removes collective risk-pooling), and skills mismatch (persistent gap between training pipeline output and employer demand). All three downstream constraints inherit the risk-transfer logic from the New Covenant framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
