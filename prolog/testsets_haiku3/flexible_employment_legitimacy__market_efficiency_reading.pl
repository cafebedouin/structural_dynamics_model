% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market-Clearing Mechanism (Efficiency Reading)
 *   domain: labor_economics/platform_economy
 *
 * SUMMARY:
 *   The market-efficiency reading treats flexible employment (gig work,
 *   on-demand labor, platform-mediated work) as a legitimate market mechanism
 *   for clearing labor supply and demand. Under this reading, workers are
 *   autonomous agents choosing when and how much to work, platforms are
 *   neutral coordination infrastructure, wage signals reflect scarcity and
 *   preference, and the arrangement benefits workers (access to work),
 *   consumers (service availability), and employers (demand-responsive
 *   labor). This reading competes with two sibling readings: the
 *   precarity-extraction reading, which reads the same institutional
 *   arrangement as structurally extractive (workers are trapped by
 *   algorithmic control and income volatility); and the developmental-state
 *   reading, which treats flexible employment as transitional, requiring
 *   state management toward formalization and social insurance. The
 *   market-efficiency reading is ONE instantiation of a contested kernel; it
 *   is not the discovered truth of labor markets—it is a committed frame that
 *   makes specific premises about worker autonomy, algorithmic neutrality,
 *   and price signals.
 *
 * KEY AGENTS:
 *   - Platform operators: Institutional power, agenda-setter role in this reading (frame algorithms as neutral, promote worker autonomy narrative)
 *   - Workers seeking flexibility: Moderate power, beneficiary in this reading (gain access to work on their terms)
 *   - Consumers: Organized power, beneficiary (service availability at market-clearing prices)
 *   - Traditional employers: Powerful, beneficiary (access to demand-responsive labor)
 *   - Regulatory authorities: Institutional power, excluded from the market-efficiency frame (would assert employment relationships require mandatory terms)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.38).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.22).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, '8620ae29-10f4-4bc1-b877-bb65eef1c636').
narrative_ontology:cs_kernel_codification('8620ae29-10f4-4bc1-b877-bb65eef1c636', distributed).
narrative_ontology:cs_authority_grounding('8620ae29-10f4-4bc1-b877-bb65eef1c636', distributed).
narrative_ontology:cs_reading_relation('8620ae29-10f4-4bc1-b877-bb65eef1c636', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8620ae29-10f4-4bc1-b877-bb65eef1c636', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('8620ae29-10f4-4bc1-b877-bb65eef1c636', foundational, worker_autonomy_maximized).
narrative_ontology:cs_axiom_status(worker_autonomy_maximized, holdable).
narrative_ontology:cs_axiom_grounding('8620ae29-10f4-4bc1-b877-bb65eef1c636', worker_autonomy_maximized, instrumental).
narrative_ontology:cs_axiom('8620ae29-10f4-4bc1-b877-bb65eef1c636', foundational, labor_market_clearing_hypothesis).
narrative_ontology:cs_axiom_status(labor_market_clearing_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('8620ae29-10f4-4bc1-b877-bb65eef1c636', labor_market_clearing_hypothesis, empirically_contingent).
narrative_ontology:cs_reference_frame('8620ae29-10f4-4bc1-b877-bb65eef1c636', equilibrium_labor_pricing).
narrative_ontology:cs_drift_state('8620ae29-10f4-4bc1-b877-bb65eef1c636', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8620ae29-10f4-4bc1-b877-bb65eef1c636', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_services).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, workers_seeking_flexible_schedule).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, labor_market_clearing_hypothesis).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, algorithmic_neutrality_doctrine).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, worker_autonomy_maximization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate labor-matching algorithms that enable real-time supply-demand equilibration. Frame their role as neutral coordination infrastructure. Benefit from reduced labor management overhead, lower employer liability, and algorithmic price discovery that expands addressable market. Under this reading, they are primarily coordinators, secondarily recipients of scalable economics.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Can enter and exit work arrangements on their own schedule, accessing income opportunities that were previously unavailable (second jobs, caregiving-compatible work, trial employment). Under this reading, they exercise genuine autonomy: they choose when to work, for whom, and can arbitrage between platforms.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, workers_seeking_flexible_schedule, beneficiary,
    moderate, biographical, mobile, regional).

% Can adjust labor input on demand rather than maintaining fixed workforce with downside risk in slack periods. Benefit from wage competition between workers seeking flexible arrangements and from reduced statutory employment protections that apply to contingent labor.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers, beneficiary,
    powerful, biographical, constrained, national).

% Benefit from service availability matched to demand: ride-hailing, delivery, and gig services exist because flexible labor supply makes them economically viable. Prices reflect market clearing rather than administered rates. Access is frictionless.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_services, beneficiary,
    organized, immediate, mobile, regional).

% Measure wage convergence in flexible-labor segments and model it as market signal of scarcity and preference alignment. Treat platform algorithms as neutral price discovery and matching mechanisms. Analyze employment patterns through the lens of revealed preference.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% Would argue that flexible employment disguises employment relationships that should carry mandatory protections (minimum wage, benefits, collective bargaining rights). Under this reading they are structurally excluded because their core claim—that these ARE traditional employment relationships requiring mandatory terms—contradicts the market-efficiency reading's core premise that labor pricing should be fully flexible.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, regulatory_authorities, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches labor supply to real-time demand through algorithmic price signals and worker autonomy: workers supply effort when prices make the work attractive to them; platforms aggregate this supply and route it to consumer demand; the result is efficient utilization without administrative allocation.
% TRANSFER_FUNCTION: Moves income from consumers (via service fees) to workers proportional to their productive contribution, mediated by market prices rather than administered wages or employment contracts. Platform operators capture a transaction fee as the cost of coordination infrastructure.
% ABSENT_VOICES: Regulatory authorities and labor advocates who argue employment relationships are present and mandatory terms should apply. They are excluded from the market-efficiency reading's own framework because their core claim (these ARE employment relationships requiring mandatory standards) contradicts the reading's foundational premise (labor is a commodity matched via price signals with no mandatory relational terms).
% DISAPPEARANCE_RATIONALE: If flexible labor markets disappeared overnight—all on-demand platforms shuttered, wage floors set administratively, employment re-formalized—service availability would contract dramatically, consumer prices would spike, workers with caregiving or portfolio-work preferences would lose access to work, and labor allocation would shift from price signals to formal hiring gates and employer discretion. The service economy as currently constituted would reorganize entirely.
% FOUNDING_PROBLEM: Rigid labor markets with high hiring/firing costs and mandatory employment protections created unemployment for marginalized workers (youth, carers, immigrants) and chronic service shortages for consumers. Flexible labor routing via platforms promised to solve both: match workers to opportunities at marginal labor cost and serve consumer demand elastically.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and pro-market economists attest the founding problem is live and persistent. Independent labor economists studying gig participation document genuine access to work for previously excluded populations; they also document substantial heterogeneity in worker experience (some report autonomy gains, others report income volatility). The mismatch between these coexisting reports is itself signal that the reading is contested, not settled—corroboration is partial and conditional on perspective.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).
:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The market-efficiency reading author measures extractiveness at 0.38 (moderate)—lower than a precarity reading would, but not negligible. The reading acknowledges platform fees and information asymmetry but asserts these are coordination costs, not extraction. Suppression is low (0.22) because under this reading no coercive mechanism is needed: workers voluntarily enter arrangements that suit their preferences. Theater is low (0.18) because the coordination function—algorithmic matching—is described as performing its stated role. Accessibility of alternatives is moderate (0.42) because while workers can switch platforms or seek traditional employment, the absence of one platform constrains their menu. Resistance is substantial (0.58) because labor advocates, unions, and regulatory authorities actively contest the market-efficiency framing—they do not treat it as settled. The measurement series track a modest upward drift in extractiveness and suppression from 2010 to 2026, reflecting gradual tightening of algorithmic control and rising income volatility even as platform narrative emphasizes worker autonomy.
 *
 * PERSPECTIVAL GAP:
 *   The platform-operator seat and the worker-seeking-flexibility seat should compute DIFFERENTLY under this reading. From the operator's position: they are coordinators offering genuine access to work, extractiveness is low. From an individual worker's position: they face algorithmic wage-setting and rating systems that constrain their opportunities, extractiveness is higher. The engine will compute per-seat directionality: operators sit at low d (beneficiary), workers sit at moderate d (mixed: benefit from access, pay through algorithmic control). The regulatory authority seat, excluded from this reading's own framework, would compute very high extractiveness and high suppression if granted a seat—but under the market-efficiency reading's premises, regulatory exclusion IS the point (because mandatory employment terms would price workers out of the labor market). This perspectival gap is not a defect in the reading; it is the structure of the contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators: beneficiaries in this reading (collect fees, frame the system, set terms). d ≈ 0.2 (beneficiary end). Workers seeking flexibility: beneficiaries according to the market-efficiency reading (gain access to work they value), but paying through algorithmic control and income volatility. d ≈ 0.45 (near-symmetric). Consumers: beneficiaries (service availability, low prices). d ≈ 0.25. Traditional employers: beneficiaries (demand-responsive labor). d ≈ 0.30. Regulatory authorities: excluded entirely—they would compute high extractiveness and high suppression if granted a seat within the market-efficiency frame, but the frame's logic REQUIRES their exclusion because their core claim (mandatory terms) would undermine market clearing. The sibling precarity-extraction reading would place workers at high d (targets of extraction); the developmental-state reading would place workers and the state at moderate d (coordination partners in formalization). These divergences ARE the measurement the corpus takes.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is the risk that the founding problem (rigid labor markets pricing out marginal workers) is dead or has degraded, but the flexible-employment constraint persists by institutional inertia rather than solving an active problem. The market-efficiency reading asserts founding_problem_status=live: marginal workers still face rigid hiring gates, service shortages are real, and flexible labor still solves both. The precarity-extraction reading would mark founding_problem_status=dead: the rigid hiring gates are no longer the binding constraint; precarious income is now the binding constraint. Whether the founding problem is live or dead determines whether the arrangement is adaptive coordination or zombie extraction. The measurement trajectory (extractiveness drifting upward, theater ratio rising, suppression rising) creates risk signal for mandatrophy. If extractiveness continues to rise while suppression rises faster than worker resistance, the constraint risks becoming piton (maintained theatrically, no party benefits enough to defend it actively, no party is hurt enough to change it). The committer-frame analysis: the market-efficiency reading treats worker autonomy as foundational; if algorithmic control mechanisms eventually make autonomy illusory, the reading's core axiom is overridden, and the committer who authored this reading would be forced to reclassify toward precarity-extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    worker_autonomy_mechanism,
    'Is the autonomy workers exercise in flexible employment genuine (they choose when to work because the tradeoff is real and in their favor) or illusory (they participate because their income options are constrained to: flexible gig work or unemployment)?',
    'Counterfactual research: control-group workers offered guaranteed traditional employment at equivalent expected income; measure take-up rate of flexible arrangements. If flexible participation is robust against guaranteed alternatives, autonomy is real; if collapse to traditional employment, autonomy is constrained.',
    'If autonomy is illusory, the market-efficiency reading''s foundational axiom ''worker_autonomy_maximized'' is overridden; the constraint reclassifies toward precarity-extraction. Extraction rises, suppression rises, the arrangement becomes coercive rather than coordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_autonomy_mechanism, empirical, 'Whether worker participation in flexible labor is autonomous or forced by income constraint.').

omega_variable(
    algorithmic_neutrality_doctrine,
    'Are platform algorithms neutral price-discovery and matching mechanisms, or do they embed distributional objectives that extract surplus from workers (wage suppression algorithms, intentional rating variance, surge-pricing volatility)?',
    'Algorithm audit: internal platform documentation, reverse-engineering of decision boundaries, A/B testing of wage-setting rules, comparison of wage distributions across demographically similar worker cohorts.',
    'If algorithms are neutral and only perform matching, extractiveness stays moderate (0.38) and suppression stays low (0.22), supporting the market-efficiency reading. If algorithms embed extraction (e.g., wage caps, artificial scarcity signaling), extractiveness rises and the precarity-extraction reading becomes dominant. This is the central empirical fault line between the sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_neutrality_doctrine, empirical, 'Whether platform algorithms are neutral or extractive.').

omega_variable(
    foundational_problem_status_contest,
    'Is the foundational problem the market-efficiency reading was built to solve (rigid labor markets pricing out marginal workers) still the binding constraint on labor market participation, or has it been superseded by a different binding constraint (income volatility, algorithmic control)?',
    'Longitudinal survey of flexible-labor participants: measure why they entered (access vs. income adequacy vs. schedule), track income stability and variability over time, measure exit rates and re-entry patterns, correlate exit with wage levels, volatility, and algorithmic rating changes.',
    'If rigid hiring remains binding, founding_problem_status=live, the arrangement is adaptive coordination. If income volatility becomes binding, founding_problem_status=dead, mandatrophy risk rises and the arrangement risks becoming inertial zombie extraction (maintained theatrically while no party actively benefits enough to defend it and no party is hurt enough to overthrow it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_status_contest, empirical, 'Whether the founding problem is still live or has been superseded by new binding constraints.').

omega_variable(
    reading_selection_framing,
    'Is the market-efficiency reading the correct frame for flexible employment, or is the frame selection itself an exercise of institutional power by platform operators and pro-market economists?',
    'Discourse analysis and power-mapping: trace the genealogy of the market-efficiency framing; identify who authored it, who funded research supporting it, which institutions propagate it; compare its institutional resources and media reach to the precarity-extraction and developmental-state readings; measure policy uptake across jurisdictions.',
    'If the market-efficiency frame is the product of concentrated institutional power (platform operators funding research, pro-market economists dominating policy discourse), then the reading itself is not neutral discovery but a manifestation of the constraint''s extraction dynamic—the reading naturalizes what is actually constructed. The axiom ''labor_market_clearing_hypothesis'' becomes suspect as vindicated_proposition rather than empirically grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_framing, conceptual, 'Whether the market-efficiency reading is objective analysis or institutional-power-backed narrative selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 2010, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2010, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement_basis(flex_tr_t2010, observed).
narrative_ontology:measurement(flex_tr_t2014, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement_basis(flex_tr_t2014, observed).
narrative_ontology:measurement(flex_tr_t2018, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement_basis(flex_tr_t2018, observed).
narrative_ontology:measurement(flex_tr_t2022, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2022, 0.17).
narrative_ontology:measurement_basis(flex_tr_t2022, observed).
narrative_ontology:measurement(flex_tr_t2026, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(flex_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(flex_be_t2010, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement_basis(flex_be_t2010, observed).
narrative_ontology:measurement(flex_be_t2014, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement_basis(flex_be_t2014, observed).
narrative_ontology:measurement(flex_be_t2018, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2018, 0.33).
narrative_ontology:measurement_basis(flex_be_t2018, observed).
narrative_ontology:measurement(flex_be_t2022, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2022, 0.37).
narrative_ontology:measurement_basis(flex_be_t2022, observed).
narrative_ontology:measurement(flex_be_t2026, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(flex_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2010, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement_basis(flex_su_t2010, observed).
narrative_ontology:measurement(flex_su_t2014, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2014, 0.16).
narrative_ontology:measurement_basis(flex_su_t2014, observed).
narrative_ontology:measurement(flex_su_t2018, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement_basis(flex_su_t2018, observed).
narrative_ontology:measurement(flex_su_t2022, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2022, 0.21).
narrative_ontology:measurement_basis(flex_su_t2022, observed).
narrative_ontology:measurement(flex_su_t2026, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(flex_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=2010, tn=2026
narrative_ontology:measurement(flex_grid_01, flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse(class), 2010, 0.3).
narrative_ontology:measurement(flex_grid_02, flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse(class), 2026, 0.42).
narrative_ontology:measurement(flex_grid_03, flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse(individual), 2010, 0.25).
narrative_ontology:measurement(flex_grid_04, flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse(individual), 2026, 0.38).
narrative_ontology:measurement(flex_grid_05, flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse(organizational), 2010, 0.35).
narrative_ontology:measurement(flex_grid_06, flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse(organizational), 2026, 0.48).
narrative_ontology:measurement(flex_grid_07, flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse(structural), 2010, 0.4).
narrative_ontology:measurement(flex_grid_08, flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse(structural), 2026, 0.45).
narrative_ontology:measurement(flex_grid_09, flexible_employment_legitimacy__market_efficiency_reading, resistance(class), 2010, 0.6).
narrative_ontology:measurement(flex_grid_10, flexible_employment_legitimacy__market_efficiency_reading, resistance(class), 2026, 0.62).
narrative_ontology:measurement(flex_grid_11, flexible_employment_legitimacy__market_efficiency_reading, resistance(individual), 2010, 0.45).
narrative_ontology:measurement(flex_grid_12, flexible_employment_legitimacy__market_efficiency_reading, resistance(individual), 2026, 0.52).
narrative_ontology:measurement(flex_grid_13, flexible_employment_legitimacy__market_efficiency_reading, resistance(organizational), 2010, 0.65).
narrative_ontology:measurement(flex_grid_14, flexible_employment_legitimacy__market_efficiency_reading, resistance(organizational), 2026, 0.68).
narrative_ontology:measurement(flex_grid_15, flexible_employment_legitimacy__market_efficiency_reading, resistance(structural), 2010, 0.55).
narrative_ontology:measurement(flex_grid_16, flexible_employment_legitimacy__market_efficiency_reading, resistance(structural), 2026, 0.58).
narrative_ontology:measurement(flex_grid_17, flexible_employment_legitimacy__market_efficiency_reading, stakes_inflation(class), 2010, 0.18).
narrative_ontology:measurement(flex_grid_18, flexible_employment_legitimacy__market_efficiency_reading, stakes_inflation(class), 2026, 0.3).
narrative_ontology:measurement(flex_grid_19, flexible_employment_legitimacy__market_efficiency_reading, stakes_inflation(individual), 2010, 0.15).
narrative_ontology:measurement(flex_grid_20, flexible_employment_legitimacy__market_efficiency_reading, stakes_inflation(individual), 2026, 0.28).
narrative_ontology:measurement(flex_grid_21, flexible_employment_legitimacy__market_efficiency_reading, stakes_inflation(organizational), 2010, 0.2).
narrative_ontology:measurement(flex_grid_22, flexible_employment_legitimacy__market_efficiency_reading, stakes_inflation(organizational), 2026, 0.32).
narrative_ontology:measurement(flex_grid_23, flexible_employment_legitimacy__market_efficiency_reading, stakes_inflation(structural), 2010, 0.25).
narrative_ontology:measurement(flex_grid_24, flexible_employment_legitimacy__market_efficiency_reading, stakes_inflation(structural), 2026, 0.35).
narrative_ontology:measurement(flex_grid_25, flexible_employment_legitimacy__market_efficiency_reading, suppression(class), 2010, 0.1).
narrative_ontology:measurement(flex_grid_26, flexible_employment_legitimacy__market_efficiency_reading, suppression(class), 2026, 0.2).
narrative_ontology:measurement(flex_grid_27, flexible_employment_legitimacy__market_efficiency_reading, suppression(individual), 2010, 0.08).
narrative_ontology:measurement(flex_grid_28, flexible_employment_legitimacy__market_efficiency_reading, suppression(individual), 2026, 0.15).
narrative_ontology:measurement(flex_grid_29, flexible_employment_legitimacy__market_efficiency_reading, suppression(organizational), 2010, 0.12).
narrative_ontology:measurement(flex_grid_30, flexible_employment_legitimacy__market_efficiency_reading, suppression(organizational), 2026, 0.22).
narrative_ontology:measurement(flex_grid_31, flexible_employment_legitimacy__market_efficiency_reading, suppression(structural), 2010, 0.15).
narrative_ontology:measurement(flex_grid_32, flexible_employment_legitimacy__market_efficiency_reading, suppression(structural), 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__market_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% The constraint 'flexible_employment_legitimacy' decomposes into three structurally distinct constraint stories, each instantiating a different reading of the contested kernel. Each reading has a different epsilon (extractiveness), different beneficiary/victim structure, and different classification. The market-efficiency reading (this file) treats flexible employment as legitimate coordination with moderate extractiveness (0.38), no mandatory victims, and three competing beneficiary groups. The precarity-extraction reading treats the same institutional arrangement as highly extractive (epsilon ≈ 0.72+), with workers as trapped victims. The developmental-state reading treats it as transitional scaffolding requiring state management. The three stories are linked via network.affects_constraints and share the kernel context (flexible_employment_legitimacy). No story is 'correct'; each is a committed frame with distinct premises. The engine's per-seat classification will reveal seat divergence: a platform operator will compute this reading as rope (coordination), while a worker locked into precarious gig work may compute it (from their seat) as snare (extraction). This divergence is exactly the measurement the corpus is designed to capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
