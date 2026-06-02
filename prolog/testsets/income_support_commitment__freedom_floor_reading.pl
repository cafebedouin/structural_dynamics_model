% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Income Support Expands Negative Liberty (Freedom Floor Reading)
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the FREEDOM FLOOR READING of the
 *   contested income_support_commitment kernel. The reading asserts that
 *   income support expands negative liberty by removing the coercive
 *   necessity of the market — workers cannot be forced to accept exploitative
 *   wages when subsistence is guaranteed. This reading is one of three
 *   structurally distinct interpretations of what income support does and
 *   should do. The freedom_floor_reading treats income support as a
 *   coordination mechanism that removes a market-driven form of coercion. The
 *   dependency_trap_reading treats it as creating psychological and
 *   institutional dependency that restricts autonomy. The
 *   care_economy_reading treats it as misframed entirely — the policy should
 *   ground unpaid care work as primary economic function, not supplement
 *   desperate wage workers. Each reading coheres as a complete social policy
 *   framework with different metrics, different beneficiary/victim
 *   assignments, and different terminal states. This story analyzes ONLY the
 *   freedom_floor_reading as a clean, ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - Precarious workers / low-wage labor market participants: Primary beneficiaries (powerless/mobile) — income support removes desperation-driven wage acceptance, expanding genuine exit options from exploitative employment
 *   - Employers of low-wage labor: Primary victims (institutional/constrained) — income support reduces labor supply at marginal wage rates, pressuring wages upward and constraining profitability of low-wage business models
 *   - Taxpayers funding support: Secondary victims (moderate/constrained) — bear the fiscal cost of expansion, though may also be beneficiaries if they are precarious workers themselves
 *   - Welfare state apparatus: Structural actor (institutional/constrained) — gains political legitimacy through expanding liberty, but faces enforcement overhead and means-testing administration burden
 *   - Labor movement coalition: Organized actors (organized/mobile) — see income support as transitional coordination mechanism with sunset logic (temporary during structural transition)
 *   - Bureaucratic implementation layer: Institutional actor (institutional/arbitrage) — maintains performative eligibility verification and conditionality enforcement; sees own function as increasingly theater-dependent
 *   - Analytical observer: Civilization-level view (analytical/analytical) — risks naturalizing a contingent policy choice (income support) as if it were a universal principle (negative liberty)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.38).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.42).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Income Support Expands Negative Liberty (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_commitment__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '6d11a319-0a52-496e-9c2e-754eddad227c').
narrative_ontology:cs_kernel_codification('6d11a319-0a52-496e-9c2e-754eddad227c', formalized).
narrative_ontology:cs_authority_grounding('6d11a319-0a52-496e-9c2e-754eddad227c', lineage).
narrative_ontology:cs_interpretation_layer_present('6d11a319-0a52-496e-9c2e-754eddad227c').
narrative_ontology:cs_reading_relation('6d11a319-0a52-496e-9c2e-754eddad227c', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d11a319-0a52-496e-9c2e-754eddad227c', income_support_commitment__care_economy_reading, influences).
narrative_ontology:cs_axiom('6d11a319-0a52-496e-9c2e-754eddad227c', foundational, negative_liberty_requires_desperation_escape).
narrative_ontology:cs_axiom_status(negative_liberty_requires_desperation_escape, holdable).
narrative_ontology:cs_axiom_grounding('6d11a319-0a52-496e-9c2e-754eddad227c', negative_liberty_requires_desperation_escape, deontological).
narrative_ontology:cs_axiom('6d11a319-0a52-496e-9c2e-754eddad227c', foundational, market_wage_desperation_is_coercion).
narrative_ontology:cs_axiom_status(market_wage_desperation_is_coercion, holdable).
narrative_ontology:cs_axiom_grounding('6d11a319-0a52-496e-9c2e-754eddad227c', market_wage_desperation_is_coercion, deontological).
narrative_ontology:cs_reference_frame('6d11a319-0a52-496e-9c2e-754eddad227c', liberal_negative_liberty_with_material_freedom_floor).
narrative_ontology:cs_drift_state('6d11a319-0a52-496e-9c2e-754eddad227c', contemporary_welfare_state_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6d11a319-0a52-496e-9c2e-754eddad227c', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, low_wage_labor_market_participants).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers_of_low_wage_labor).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, taxpayers_funding_support).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (ROPE) — Income support removes the coercive necessity to accept exploitative wages. The constraint coordinates labor market participation without unacceptable extraction: the support enables genuine choice (refuse unfair wages, negotiate better terms, invest in skills). From this view, the constraint is pure coordination — it solves the collective action problem of market power asymmetry without creating new forms of coercion.
constraint_indexing:constraint_classification(income_support_commitment__freedom_floor_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-WAGE EMPLOYER (SNARE) — Income support forces internalization of labor costs previously externalized onto workers. The support reduces labor supply at marginal wage rates and pressures wage floors upward, constraining profitability of low-wage business models. From this view, the constraint extracts value through wage pressure and labor scarcity, with minimal offsetting coordination benefit. The employer sees only the cost imposition.
constraint_indexing:constraint_classification(income_support_commitment__freedom_floor_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE STATE APPARATUS (TANGLED ROPE) — Income support coordinates labor market participation and reduces the coercive asymmetry of market dependence, but also extracts legitimacy and tax revenue from the broader public. The state gains political legitimacy through expanding negative liberty, but also faces enforcement costs (surveillance, means-testing, benefit clawback administration). Genuine coordination function coupled with asymmetric extraction of public resources.
constraint_indexing:constraint_classification(income_support_commitment__freedom_floor_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MOVEMENT COALITION (SCAFFOLD) — Income support is a transitional device: it temporarily absorbs the shock of structural economic change (deindustrialization, automation, wage stagnation) while labor markets adjust. The coalition sees this as a sunset mechanism — as productivity gains are distributed or structural jobs return, the support level recedes. Coordination with built-in termination condition (economic recovery, wage floor achievement).
constraint_indexing:constraint_classification(income_support_commitment__freedom_floor_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BUREAUCRATIC IMPLEMENTATION LAYER (PITON) — The actual delivery of income support has become largely performative: means-testing and surveillance consume enormous resources (administrative overhead, applicant burden, conditionality compliance), while the stated coordination goal (removing coercive necessity) is often undermined by the implementation theater. The constraint persists through institutional inertia despite high theater costs.
constraint_indexing:constraint_classification(income_support_commitment__freedom_floor_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIBERTY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the removal of coercive market necessity through income support is a fundamental shift in what negative liberty means: it naturalizes the idea that freedom requires exit from desperation, not merely freedom from formal coercion. This reading risks treating a policy choice (to provide support) as if it were an immutable principle of liberty itself.
constraint_indexing:constraint_classification(income_support_commitment__freedom_floor_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(income_support_commitment__freedom_floor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(income_support_commitment__freedom_floor_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, TR),
    TR >= 0.70.

:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts value from employers of low-wage labor (reduced supply, wage pressure) and from taxpayers (fiscal cost), but these extractions are coupled with genuine coordination benefits: workers gain negotiating power, labor markets become less desperation-driven. The asymmetry is real (employers and broad taxpayers bear costs; precarious workers gain most benefit), but not severe because the coordination benefit is widely distributed and the extraction is justified as removal of prior market coercion. Suppression (0.42): Moderate. Barriers to exit the constraint include: political difficulty of reducing benefits once granted, employer lobbying against wage floors, taxpayer resistance to expansion, and bureaucratic inertia in means-testing administration. But suppression is not high because the constraint is not maintained through hidden violence or epistemic closure — it is openly contested and regularly renegotiated in democratic arenas. Theater ratio (0.35): Low-moderate. The coordination function (removing desperation-driven wage acceptance) is substantive and measurable. Theater emerges primarily through means-testing and eligibility verification administration, which consume resources without directly enabling the coordination goal. As implementation theater increases (time_point 0→10: 0.25→0.35), the extractiveness also increases slightly, suggesting the constraint is slowly being compromised by administrative burden.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival plurality from a single policy design. The precarious worker experiences Rope (pure coordination enabling genuine choice). The employer experiences Snare (coercive cost imposition). The welfare state experiences Tangled Rope (genuine coordination coupled with extraction of public resources and legitimacy). The labor coalition experiences Scaffold (temporary device with sunset logic). The bureaucratic layer experiences Piton (coordination function undermined by implementation theater). The analytical observer risks a Mountain classification that naturalizes the policy as a universal principle rather than analyzing its specific structural dynamics. The perspectival gaps reveal the constraint's contested nature — different structural positions legitimately generate different classifications because the constraint genuinely performs different functions (and requires different justifications) from each position.
 *
 * DIRECTIONALITY LOGIC:
 *   The freedom_floor reading's directionality profile reflects that precarious workers are beneficiaries (their negative liberty genuinely expands) while employers of low-wage labor are victims (they face wage pressure and labor scarcity). Beneficiaries with mobile exit options (workers can leave low-wage jobs) experience low effective extraction because the support genuinely expands their choice set, not because it imposes costs on them. Victims with constrained exit (employers cannot easily relocate or shift to capital-intensive production) experience higher effective extraction because the wage pressure and labor supply constraints are durable costs they cannot escape. The welfare state and bureaucratic apparatus occupy intermediate positions: they benefit from the legitimacy gains and the institutional role, but face suppression costs through means-testing administration and the political burden of sustaining an expanded program.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negative_liberty_grounding,
    'Does negative liberty primarily mean absence of formal coercion, or does it require absence of desperation-driven choice?',
    'Philosophical and historical analysis of liberty conceptions (Berlin, Pettit, libertarian vs. progressive framings). Empirical measurement of wage acceptance patterns with and without income support.',
    'If liberty = absence of formal coercion only: income support is extraction creating dependency (Snare). If liberty = freedom from desperation: income support is coordination removing coercion (Rope). If liberty = structurally protected capacity to refuse: income support is partial (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(negative_liberty_grounding, conceptual, 'Philosophical disagreement over what negative liberty requires').

omega_variable(
    labor_supply_elasticity_empirical,
    'Does income support measurably increase workers'' ability to refuse exploitative wages, or does it primarily function as wage supplementation with minimal bargaining effect?',
    'Quasi-experimental analysis: wage negotiation patterns, job acceptance rates, and exit frequencies in labor markets with vs. without income support. Longitudinal tracking of worker bargaining capacity.',
    'If supply elasticity is high: workers genuinely gain bargaining power (Rope/Tangled Rope classification). If elasticity is low: support functions primarily as income transfer without labor market restructuring (Scaffold/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_empirical, empirical, 'Wage elasticity of labor supply in presence of income support').

omega_variable(
    implementation_overhead_ceiling,
    'Can income support be administered with theater_ratio below 0.5, or is verification and conditionality enforcement structurally unavoidable?',
    'Administrative burden data: ratio of (staff time spent on eligibility verification + compliance monitoring) to (benefits actually distributed). Comparative analysis across policy designs (universal basic income, means-tested, categorical support).',
    'If overhead < 0.5: support is genuine coordination, Rope/Tangled Rope classification stable. If overhead > 0.5: Piton classification confirmed, suggesting the coordination function is being compromised by implementation burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_overhead_ceiling, empirical, 'Whether income support administration can maintain low performative theater').

omega_variable(
    freedom_floor_vs_dependency_trap_distinction,
    'Is this reading''s core claim (income support expands negative liberty by removing market desperation) logically compatible with the dependency_trap_reading''s core claim (income support creates psychological/institutional dependency that restricts choice)?',
    'Conceptual analysis of liberty, choice, and dependency. Empirical measurement of aspiration trajectories, behavioral autonomy, and decision-making patterns in recipients.',
    'If logically incompatible (the readings truly foreclose one another): one reading is the correct committer framework; the other is mistaken. If empirically distinguishable: the readings coexist across different subpopulations or time horizons. If structurally dependent: this reading influences the dependency_trap reading (creates conditions that make dependency possible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_floor_vs_dependency_trap_distinction, conceptual, 'Relationship between freedom_floor and dependency_trap readings — whether they coexist or foreclose').

omega_variable(
    care_economy_institutional_primacy,
    'Does this reading''s focus on negative liberty (freedom from desperation in wage labor) underestimate the care_economy_reading''s structural claim (that unpaid care work is the primary economic function and income support should ground it)?',
    'Comparative analysis of policy implementation across readings. Measurement of how income support allocation differs when care work is recognized as primary vs. when wage labor is treated as default. Institutional analysis of which reading shapes policy design.',
    'If care_economy reading is institutionally primary: this reading''s analysis of negative liberty in wage markets is incomplete and reflects a narrow view of economic participation. If readings are structurally independent: different policy regimes emerge from each.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_economy_institutional_primacy, conceptual, 'Whether this reading''s framework underestimates care economy structural claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incsupp_freedom_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(incsupp_freedom_tr_t5, income_support_commitment__freedom_floor_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(incsupp_freedom_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(incsupp_freedom_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(incsupp_freedom_be_t5, income_support_commitment__freedom_floor_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(incsupp_freedom_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(incsupp_freedom_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(incsupp_freedom_su_t5, income_support_commitment__freedom_floor_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(incsupp_freedom_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__care_economy_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, labor_market_wage_floor_constraint).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, desperation_driven_employment_coercion).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel has three distinct readings, each with its own constraint story and its own ε value. The freedom_floor_reading (this story) emphasizes negative liberty expansion (ε=0.38, Tangled Rope). The dependency_trap_reading models the constraint as creating psychological dependency (ε may be higher, Snare). The care_economy_reading treats the constraint as fundamentally misframed (ε and type vary depending on whether care work is recognized as primary). These stories are linked via network.affects_constraints because each reading's policy implementation creates conditions that affect the other readings' empirical outcomes. The three stories together form a constraint family decomposed along the kernel dimension rather than along the ε-invariance principle — they are the same policy noun (income support) read through three different philosophical frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__freedom_floor_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
