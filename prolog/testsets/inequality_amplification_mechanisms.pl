% ============================================================================
% CONSTRAINT STORY: inequality_amplification_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inequality_amplification_mechanisms, []).

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
 *   constraint_id: inequality_amplification_mechanisms
 *   human_readable: Inequality Amplification Mechanisms
 *   domain: economic/social/systemic
 *
 * SUMMARY:
 *   Inequality amplification mechanisms are systemic arrangements that
 *   convert initial advantage into compounding disadvantage for those without
 *   early capital, networks, or institutional access. These mechanisms
 *   operate across multiple domains (education, housing, finance, healthcare)
 *   and reinforce through feedback: initial wealth enables access to
 *   credentialing, which enables earning power, which enables asset
 *   accumulation, which enables intergenerational transfer. Simultaneously,
 *   lack of initial resources triggers a degrading cascade: educational
 *   barriers, credit constraints, limited network access, concentrated
 *   exposure to extractive financial products, and reduced ability to
 *   navigate bureaucratic systems. The constraint exhibits genuine
 *   coordination functions (capital allocation, specialization incentives,
 *   productivity rewards) AND asymmetric extraction (barrier elevation, rent
 *   capture, intergenerational lock-in). It is fundamentally a Tangled Rope:
 *   both functions are real and simultaneously structural. The perspectival
 *   gap reveals how the same mechanisms appear as coordination to
 *   beneficiaries, pure extraction to the locked-out, mixed
 *   extraction/coordination to moderate agents, and legitimate (if
 *   incomplete) incentive structures to analytical observers.
 *
 * KEY AGENTS:
 *   - Low-Income Cohorts: Primary victim (powerless/trapped) — trapped by cumulative disadvantage with no viable exit path; bear maximum extraction through limited access, higher costs, and barrier elevation
 *   - Intergenerational Mobility: Structural victim (powerless/trapped) — abstract collective good; mobility rates decline as amplification mechanisms intensify
 *   - Middle-Income Climber: Secondary actor (moderate/constrained) — structurally mobile but constrained by credential inflation, time requirements, and information asymmetries; partially benefits from coordination while partially bearing extraction
 *   - Wealth Concentrators: Primary beneficiary (institutional/arbitrage) — institutional actors with capital access, asset appreciation, and preferential institutional treatment; arbitrage options allow them to avoid or escape specific mechanisms while maintaining wealth
 *   - Reform Coalition: Organized agent (organized/constrained) — labor unions, community organizations, advocacy groups that benefit from collective coordination but face suppression through legal and institutional barriers
 *   - Meritocratic Narrative System: Institutional actor (institutional/arbitrage) — maintains legitimizing performance of fairness; the narrative function has degraded (pure theater) while institutional actors continue to enforce its observance
 *   - Analytical Observer: Civilizational position (analytical/analytical) — sees both coordination and extraction functions; recognizes the constraint as tangled rather than pure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inequality_amplification_mechanisms, 0.58).
domain_priors:suppression_score(inequality_amplification_mechanisms, 0.65).
domain_priors:theater_ratio(inequality_amplification_mechanisms, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inequality_amplification_mechanisms, extractiveness, 0.58).
narrative_ontology:constraint_metric(inequality_amplification_mechanisms, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(inequality_amplification_mechanisms, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inequality_amplification_mechanisms, tangled_rope).
narrative_ontology:human_readable(inequality_amplification_mechanisms, "Inequality Amplification Mechanisms").
narrative_ontology:topic_domain(inequality_amplification_mechanisms, "economic/social/systemic").

domain_priors:requires_active_enforcement(inequality_amplification_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inequality_amplification_mechanisms, wealth_concentrators).
narrative_ontology:constraint_beneficiary(inequality_amplification_mechanisms, institutional_gatekeepers).
narrative_ontology:constraint_victim(inequality_amplification_mechanisms, low_income_cohorts).
narrative_ontology:constraint_victim(inequality_amplification_mechanisms, intergenerational_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-OUT COHORT (SNARE) — Trapped by cumulative disadvantage: lack initial capital, face educational barriers, encounter discriminatory lending, bear healthcare costs that erode savings, and face structural obstacles to wealth accumulation. Exit options are severely constrained — the mechanisms that amplify inequality for others are extractive for this agent. Maximum experienced extraction with no meaningful coordination benefit.
constraint_indexing:constraint_classification(inequality_amplification_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME CLIMBER (TANGLED ROPE) — Moderate extractiveness with partial coordination function. Educational access and social mobility are real but conditional: requires sustained effort, benefits from network effects, but also faces credential inflation and competition intensification. The constraint both enables (coordination through credentials) and extracts (raising barriers for those without access). Constrained by time, capital, and information asymmetries.
constraint_indexing:constraint_classification(inequality_amplification_mechanisms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL ACCUMULATOR (ROPE) — Benefits from mechanisms that concentrate wealth: asset appreciation, tax arbitrage, institutional lending advantages, and preferential access to investment vehicles. Experiences the constraint as pure coordination: the system enables them to coordinate capital, leverage, and institutional access. Net beneficiary with genuine arbitrage options — can diversify, relocate, or exit specific mechanisms while maintaining wealth.
constraint_indexing:constraint_classification(inequality_amplification_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITION (TANGLED ROPE) — Labor unions, community organizations, and advocacy groups perceive both the coordination function (collective action to set wage floors, bargaining standards) and the extraction (capital's structural advantage). They benefit from coordination (collective power) but bear costs through suppression (anti-union legislation, legal barriers to organizing). Extractiveness is moderate because they have agency and partial institutional power.
constraint_indexing:constraint_classification(inequality_amplification_mechanisms, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGITIMIZING NARRATIVE (PITON) — Meritocratic framing ('inequality reflects effort and talent') persists despite contradicting evidence. Theater ratio is high: institutional actors maintain the performance that the system is fundamentally fair while structural mechanisms visibly amplify inequality. The narrative function has largely atrophied; it persists through institutional inertia and because elites benefit from its maintenance. Theater_ratio (0.48 base) understates the performative content of the meritocratic narrative itself — the narrative is nearly pure theater.
constraint_indexing:constraint_classification(inequality_amplification_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a comprehensive analytical position, inequality amplification mechanisms exhibit genuine coordination functions (capital allocation, productivity incentives, specialization efficiency) AND asymmetric extraction (barrier elevation, rent capture, intergenerational lock-in). Both functions are real and structural, not one masking the other. The effective extractiveness reflects the empirically measured gap between claimed and actual mobility rates.
constraint_indexing:constraint_classification(inequality_amplification_mechanisms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inequality_amplification_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inequality_amplification_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inequality_amplification_mechanisms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inequality_amplification_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inequality_amplification_mechanisms, TR),
    TR >= 0.70.

:- end_tests(inequality_amplification_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting both real coordination functions and substantial asymmetric extraction. The measurement trajectory shows extraction intensification over the 45-year interval (0.35→0.58), indicating that amplification mechanisms are strengthening faster than countervailing policies. This is not pure extraction (which would be >0.66 in snare range) because coordination functions are genuine — capital allocation mechanisms do work, and productivity incentives are real. But extraction is substantial because barrier elevation, credential inflation, and rent capture are also real structural features. Suppression (0.65): High. Multiple barriers to exit: lack of initial capital (not a choice), educational gatekeeping (credentialism), discriminatory lending (structural), healthcare costs (burden-bearing), time constraints (exploitation), and limited political voice (disenfranchisement). Trapped agents face severe suppression from multiple orthogonal mechanisms — no single barrier is insurmountable, but their combination creates effective lock-in. Theater ratio (0.48): Moderate. The measured theater reflects meritocratic narratives that legitimize the constraint while masking structural mechanisms. Theater is lower than might be expected because extraction mechanisms are somewhat visible (wealth gaps are undeniable) — the theater is in the *framing* (meritocratic story) rather than in hiding the extraction itself. However, the narrative's theater function has substantially increased over the interval as evidence of immobility has accumulated, requiring more performative work to maintain the meritocratic framing.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries see coordination and low extraction (Rope). Trapped agents see maximum extraction (Snare). Moderate agents see mixed extraction/coordination (Tangled Rope). Organized reform agents see partial extraction with coalition power (Tangled Rope with constrained exit). The meritocratic narrative sees its own function as degraded (Piton: theater without coordination benefit). The analytical observer sees both functions genuine and simultaneously structural (Tangled Rope), recognizing that the constraint is not fundamentally a story of hidden extraction masked by narrative, but rather a hybrid system with both real coordination and real extraction, with asymmetric distribution of each.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealth concentrators: beneficiary + arbitrage exit → d ≈ 0.08 → f(d) ≈ -0.10 → experienced as pure benefit. Low-income cohorts: victim + trapped exit → d ≈ 0.92 → f(d) ≈ 1.36 → experienced as pure extraction. Middle-income: both (moderate benefit from mobility coordination, moderate cost from credential inflation) + constrained exit → d ≈ 0.60 → f(d) ≈ 0.75 → experienced as mixed with slight target bias. Reform coalition: organized agent with constrained exit (legal/institutional suppression) + some beneficiary function (collective power) + some victim function (facing suppression) → d ≈ 0.68 → f(d) ≈ 1.05 → experienced as moderate-high extraction with partial agency.
 *
 * MANDATROPHY ANALYSIS:
 *   Inequality amplification mechanisms resolve mandatrophy by clarifying that the system is genuinely Tangled Rope, not pure Snare disguised as Rope. The mandatrophy danger was: 'Are we looking at coordination mechanisms that happen to disadvantage some groups (Rope with unequal distribution), or are we looking at extraction mechanisms that use coordination narrative as cover (Snare with legitimation)?' The structural analysis shows both are present and simultaneously real. The coordination functions (capital allocation, productivity incentives, institutional specialization) exist and work. The extraction functions (barrier elevation, rent capture, intergenerational lock-in) also exist and work. Neither is the 'real' mechanism masking the other — they are orthogonal features of the same institutional arrangement. This is precisely what Tangled Rope classification captures: genuine coordination AND asymmetric extraction, both structural, both necessary to understand the constraint. The perspectival gap (beneficiaries see Rope, trapped see Snare, analytical see Tangled Rope) is not a failure to identify the 'true' type but a demonstration that all three are correct relative to the observer's structural position. The constraint is not one type viewed from different angles — it genuinely exhibits different extraction/coordination ratios for different agents, which the framework models through chi formula differentiation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'How much of measured inequality amplification reflects legitimate coordination incentives (rewarding productivity, allocating capital efficiently) versus asymmetric extraction (barrier elevation, rent capture)?',
    'Counterfactual analysis: compare mobility rates under alternative institutional arrangements (Nordic model vs US model vs pure redistribution); identify which mechanisms are necessary for coordination function and which serve extraction only',
    'If coordination-heavy (>60%): reclassify as Rope with higher beneficiary weighting. If extraction-heavy (>70%): reclassify as Snare from more perspectives. Current classification assumes 50/50 split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Proportion of inequality amplification driven by coordination vs extraction').

omega_variable(
    intergenerational_lock_mechanism,
    'Is intergenerational wealth concentration primarily driven by inherited capital accumulation (linear) or by barrier elevation and credential inflation (nonlinear amplification)?',
    'Longitudinal data: track wealth and mobility across generational cohorts; decompose inequality growth by inheritance vs earned income vs asset appreciation vs credential premium; cross-national comparison of lineage-dependent trajectories',
    'If inheritance-dominated: suppression metric should be higher (trapped agents lack initial capital with no recovery path). If barrier-dominated: theater ratio should be higher (credentialism masks capital-driven selection). Current split: 40% inheritance, 60% barrier elevation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_lock_mechanism, empirical, 'Whether intergenerational lock-in is driven by inherited capital or barrier elevation').

omega_variable(
    identity_lock_vs_structural_constraint,
    'To what extent do trapped agents perceive inequality as structurally immutable (trapped exit) versus self-perpetuating through internalized narratives (identity_locked exit)?',
    'Qualitative analysis: interviews and narrative analysis of how low-mobility agents frame their opportunities; measurement of belief in mobility vs belief in structural barriers; analysis of effort-outcome correlation in subjective experience',
    'If primarily identity_locked: reclassify powerless perspective''s exit_options as ''identity_locked''; theater ratio increases (internalized constraint theater); perspectives shift from Mountain (immutable) to Rope (changeable in principle) at biographical horizon. If primarily trapped: current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_constraint, empirical, 'Whether powerless agents are trapped by structure or identity-locked by internalized framing').

omega_variable(
    amplification_mechanism_specificity,
    'Are the primary amplification mechanisms domain-general (capital accumulation, network effects, institutional gatekeeping) or domain-specific (housing, healthcare, education, finance)?',
    'Comparative analysis across domains: measure extractiveness and suppression for inequality in housing access vs healthcare vs education vs financial services; identify which mechanisms are universal and which domain-dependent',
    'If domain-specific: decompose into separate constraint stories (housing_inequality_amplification, education_inequality_amplification, etc.), each with its own ε and perspectives. If domain-general: current monolithic story is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amplification_mechanism_specificity, empirical, 'Whether inequality amplification is domain-general or requires decomposition').

omega_variable(
    policy_ceiling_effect,
    'Do redistributive policies (taxes, transfers, social programs) function as genuine corrections to inequality amplification or as performative theater that legitimizes the underlying mechanisms?',
    'Time-series analysis: measure redistribution policy intensity vs actual post-tax inequality; compare policy impact pre- and post-regulatory capture; identify whether policy tightening/loosening correlates with underlying structural changes or is orthogonal',
    'If genuine correction: theater ratio should be lower than 0.48 (policies actually reduce amplification). If performative: theater ratio understates the performative content; should be >0.60. If ceiling effect (policies hit decreasing returns): constraints affecting low-income cohorts should increase over time even as policy intensity increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_ceiling_effect, empirical, 'Whether redistributive policies genuinely correct inequality or perform legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inequality_amplification_mechanisms, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ineq_amp_tr_t0, inequality_amplification_mechanisms, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ineq_amp_tr_t15, inequality_amplification_mechanisms, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ineq_amp_tr_t30, inequality_amplification_mechanisms, theater_ratio, 30, 0.48).
narrative_ontology:measurement(ineq_amp_tr_t45, inequality_amplification_mechanisms, theater_ratio, 45, 0.55).

% Extraction over time
narrative_ontology:measurement(ineq_amp_be_t0, inequality_amplification_mechanisms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ineq_amp_be_t15, inequality_amplification_mechanisms, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(ineq_amp_be_t30, inequality_amplification_mechanisms, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ineq_amp_be_t45, inequality_amplification_mechanisms, base_extractiveness, 45, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inequality_amplification_mechanisms, resource_allocation).
narrative_ontology:affects_constraint(inequality_amplification_mechanisms, intergenerational_wealth_transfer).
narrative_ontology:affects_constraint(inequality_amplification_mechanisms, credentialism_barrier_elevation).
narrative_ontology:affects_constraint(inequality_amplification_mechanisms, financial_access_gatekeeping).
narrative_ontology:affects_constraint(inequality_amplification_mechanisms, network_effect_concentration).

% DUAL FORMULATION NOTE:
% Inequality amplification mechanisms decompose into domain-specific constraints: housing_inequality_amplification (ε=0.62, tangled_rope), education_inequality_amplification (ε=0.55, tangled_rope), healthcare_access_inequality (ε=0.52, tangled_rope), and financial_discrimination (ε=0.60, tangled_rope). Each domain has distinct ε values reflecting different amplification mechanism intensities. The monolithic story captures the generic mechanisms; domain stories capture specific institutional dynamics. All are linked through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inequality_amplification_mechanisms, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
