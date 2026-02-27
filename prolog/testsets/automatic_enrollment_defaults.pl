% ============================================================================
% CONSTRAINT STORY: automatic_enrollment_defaults
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_automatic_enrollment_defaults, []).

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
 *   constraint_id: automatic_enrollment_defaults
 *   human_readable: Automatic Enrollment Defaults in Retirement Plans
 *   domain: economic/social
 *
 * SUMMARY:
 *   Automatic enrollment in retirement plans represents a foundational choice
 *   architecture intervention where employees are placed into a 401(k) or
 *   equivalent plan by default, with an explicit opt-out option, rather than
 *   requiring active opt-in. Since widespread adoption after the Pension
 *   Protection Act of 2006, automatic enrollment has dramatically increased
 *   retirement plan participation rates — from ~60% participation in
 *   traditional opt-in plans to ~80% in automatic enrollment plans. The
 *   constraint exhibits a perspectival plurality: plan sponsors and financial
 *   intermediaries see efficient coordination that solves a behavioral
 *   problem; middle-income employees benefit from increased savings
 *   discipline; low-wage workers experience forced deductions during
 *   financially constrained periods; and policy observers increasingly
 *   recognize the mechanism as both socially beneficial (for those who can
 *   afford it) and extractive (for those who cannot). The theater ratio has
 *   risen over the measurement interval as the behavioral justification
 *   ('people are irrational about savings') has become normalized,
 *   potentially obscuring the underlying mechanism's equity effects.
 *
 * KEY AGENTS:
 *   - Low-Wage Workers: Primary victims (powerless/trapped) — automatically enrolled with unaffordable deductions; minimal exit options due to social friction and employer control
 *   - Middle-Income Employees: Secondary actors (moderate/constrained) — experience coordination benefit and extraction simultaneously; constrained exits
 *   - Plan Sponsors (Employers): Primary beneficiaries (institutional/arbitrage) — reduce administrative burden and cost; benefit from coordination mechanism
 *   - Financial Intermediaries: Primary beneficiaries (institutional/arbitrage) — expand Assets Under Management; benefit from scale increase
 *   - Retirement Security Advocates: Tertiary beneficiaries (organized/constrained) — pursue long-term policy goal of universal retirement coverage
 *   - Paternalistic State Authority: Institutional designer (institutional/arbitrage) — maintains default mechanism despite equity concerns
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes hybrid coordination-extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(automatic_enrollment_defaults, 0.32).
domain_priors:suppression_score(automatic_enrollment_defaults, 0.35).
domain_priors:theater_ratio(automatic_enrollment_defaults, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(automatic_enrollment_defaults, extractiveness, 0.32).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(automatic_enrollment_defaults, tangled_rope).
narrative_ontology:human_readable(automatic_enrollment_defaults, "Automatic Enrollment Defaults in Retirement Plans").
narrative_ontology:topic_domain(automatic_enrollment_defaults, "economic/social").

domain_priors:requires_active_enforcement(automatic_enrollment_defaults).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, plan_sponsors).
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, financial_intermediaries).
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, retirement_security_advocates).
narrative_ontology:constraint_victim(automatic_enrollment_defaults, low_wage_workers).
narrative_ontology:constraint_victim(automatic_enrollment_defaults, financially_constrained_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-WAGE WORKER (SNARE) — Automatically enrolled with salary deductions they cannot afford. Exit options are minimal: requesting manual opt-out incurs social friction and potential employer disapproval. Cannot substitute with competing savings vehicles without employer plan access. Bears full extraction cost through forced savings that reduce take-home pay during financially constrained years.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME EMPLOYEE (TANGLED ROPE) — Experiences both coordination benefit and extraction. Automatic enrollment increases retirement savings, which benefits long-term security, but the default contribution rate and investment allocation are often mismatched to individual circumstances. Constrained exits: opt-out is available but carries friction (form-filling, potential social judgment). Both benefits from the security mechanism and extracted through suboptimal defaults.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLAN SPONSOR (ROPE) — Benefits from automatic enrollment through reduced administrative burden, lower per-capita costs, and regulatory compliance with fiduciary standards. Experiences the mechanism as coordination: managing workforce benefits without forcing individual decision-making overhead onto employees. Arbitrage exit available: can choose plan design parameters, vendor, contribution matching levels. Net beneficiary.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL INTERMEDIARY (ROPE) — Benefits from larger asset pools under management; automatic enrollment increases plan participation and Assets Under Management (AUM). Experiences constraint as coordination mechanism that solves the information problem ('how do we get people to save?'). Arbitrage exit: can adjust fee structures, investment menus, marketing approaches. Net beneficiary through increased scale.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RETIREMENT SECURITY COALITION (SCAFFOLD) — Organized advocates (labor unions, consumer protection groups, policy organizations) see automatic enrollment as a temporary solution with explicit sunset: as financial literacy improves and portable retirement accounts (IRA aggregation, multi-employer plans) mature, the need for employer-mandated defaults declines. Theater ratio is relatively low because the mechanism delivers measurable outcomes (participation rates). Suppression may decline as alternatives emerge.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PATERNALISTIC STATE (PITON) — Policy design that relies on behavioral defaults to achieve social goals (retirement security) increasingly appears performative at scale. Automatic enrollment was justified by demonstrated behavioral inertia but persists despite: (1) growing awareness that defaults entrench inequality (low-wage workers cannot afford deductions), (2) rise of alternative mechanisms (automatic IRA accounts, portable benefits), and (3) cultural shift toward transparency over paternalism. The mechanism functions but its rationale has atrophied. Theater ratio has risen as debates shift from 'does automatic enrollment work?' to 'does paternalism through default design fit our values?'
constraint_indexing:constraint_classification(automatic_enrollment_defaults, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, automatic enrollment is structurally a hybrid: it solves a genuine coordination problem (how to increase retirement savings participation when active choice generates procrastination and inertia) AND simultaneously extracts through mismatched defaults that concentrate wealth benefits toward higher-income employees who can afford the deductions and benefit from longer investment horizons. The constraint is not a natural law but a deliberate policy choice that produces both genuine coordination gains and systematic extraction from the financially constrained.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(automatic_enrollment_defaults_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(automatic_enrollment_defaults, TR),
    TR >= 0.70.

:- end_tests(automatic_enrollment_defaults_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.32): Moderate. Automatic enrollment does extract from workers through reduced take-home pay, but the extraction is not maximal (ε > 0.46) because: (1) the mechanism solves a genuine behavioral problem (procrastination in retirement savings), (2) workers retain formal exit rights (opt-out is legally permitted), and (3) many workers, particularly middle-income and higher, genuinely benefit from the commitment device. The extraction concentrates on low-wage workers who face genuine affordability constraints — this population heterogeneity keeps the overall ε moderate. Suppression (0.35): Moderate. Exit costs include form-filling friction, potential social/reputational friction from employer awareness of opt-out, and information barriers (many workers don't know opt-out exists or are uncertain about consequences). However, suppression is not high (≥ 0.60) because: exit is formally available, alternative savings vehicles exist (though with friction), and regulatory frameworks increasingly require plan sponsors to disclose opt-out procedures. Theater ratio (0.42): Moderate. The mechanism delivers measurable outcomes (participation rates increase from ~60% to ~80%), so it has genuine functional content. However, theater has risen over time as behavioral justifications have become standardized and equity concerns have emerged — the performative aspect (framing as 'helping workers save' while potentially reducing take-home pay for the poorest) has increased.
 *
 * PERSPECTIVAL GAP:
 *   The classification diverges sharply across power levels and exit options. The low-wage worker sees pure extraction (Snare: powerless/trapped). The middle-income employee sees mixed coordination-extraction (Tangled Rope: moderate/constrained). The plan sponsor and financial intermediary see pure coordination (Rope: institutional/arbitrage). The retirement security coalition sees a temporary solution (Scaffold: organized/constrained with sunset as alternative mechanisms mature). The state apparatus sees paternalism that may be increasingly performative (Piton: institutional/arbitrage with rising theater as paternalism gets questioned). The analytical observer, integrating all perspectives, sees the constraint as fundamentally hybrid: it genuinely solves a behavioral coordination problem AND simultaneously produces systematic extraction from the financially vulnerable. This perspectival gap reveals that 'automatic enrollment' is not a single constraint viewed from multiple angles — it is a hybrid mechanism with genuinely different structural effects depending on income level and exit capacity. The policy choice to implement automatic enrollment at the default contribution rate (typically 3-6% of salary) rather than with income-scaled or means-tested rates directly creates the extraction asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) value emerges from the agent's structural position relative to the extraction flow. Low-wage workers (powerless/trapped) experience high d (≈0.90): they are targets of extraction with minimal exit options. Middle-income employees (moderate/constrained) experience moderate d (≈0.60): they both bear costs and receive benefits, with constrained (not absent) exit options. Plan sponsors and intermediaries (institutional/arbitrage) experience low d (≈0.15-0.20): they are beneficiaries with full exit options (can choose plan design parameters, vendors, matching levels). The analytical observer uses canonical d for institutional power (≈0.00-0.15) because the perspective is not embedded in a specific actor but observes the aggregate structure. The engine derives these d values from the beneficiary/victim declarations plus power and exit options; the sigmoid f(d) then scales effective extractiveness accordingly. The low-wage worker experiences high χ (effective extraction), while the plan sponsor experiences negative χ (they benefit from the coordination). This directionality differentiation is what produces the perspectival gap — the same constraint generates different experienced extractiveness values depending on where the agent sits in the income and power hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination from extractive paternalism through the lens of affordability and exit options. The constraint is NOT purely extraction (Snare) because middle-income and higher-income workers genuinely benefit from the behavioral commitment device, and the mechanism solves a real problem (retirement savings procrastination). The constraint is NOT purely coordination (Rope) because the uniform default contribution rate produces systematic extraction from low-wage workers who cannot afford deductions. The Tangled Rope classification (0.40 ≤ χ ≤ 0.90, base extraction ≥ 0.30, suppression ≥ 0.40, requires_active_enforcement = true, beneficiaries AND victims) captures this hybrid accurately: the mechanism has a genuine coordination function (solve the behavioral problem) and also produces asymmetric extraction (systematic disadvantage for low-wage workers). The mandate-erosion risk exists in two directions: (1) if income-differentiated defaults or means-tested contribution rates are implemented, the extraction component diminishes and the constraint trends toward pure Rope, and (2) if alternatives (portable retirement accounts, automatic IRA aggregation) mature, the coordination problem dissolves and the constraint's functional role disappears, leaving only the extractive shell (piton degradation then obsolescence). Current policy evolution (safe-harbor expansions, multi-employer plan authorization, automatic IRA exploration) suggests movement toward the constraint's scaffold scenario — a temporary solution being replaced by better-designed alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    default_affordability_threshold,
    'At what income level does automatic enrollment transition from coordination benefit to extraction mechanism?',
    'Analysis of retirement security outcomes by income quintile; correlation between default contribution rates and financial hardship metrics (emergency fund adequacy, debt-to-income ratios)',
    'If threshold < 150% federal poverty line: automatic enrollment is primary mechanism for extracting from the poorest workers. If threshold > 250%: extraction narrative is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(default_affordability_threshold, empirical, 'Income threshold where automatic enrollment becomes extractive rather than beneficial').

omega_variable(
    framing_effect_persistence,
    'Do automatic enrollment defaults persist primarily because of genuine behavioral inertia or because of institutional path-dependency and regulatory capture?',
    'Comparison of opt-out rates when defaults are paired with high-salience, low-friction exit mechanisms vs current design; longitudinal analysis of whether younger cohorts (with higher financial literacy) maintain similar participation rates',
    'If behavioral inertia drives persistence: tangled rope classification confirmed. If institutional capture drives persistence: constraint shifts toward snare for powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_effect_persistence, conceptual, 'Whether behavioral inertia or institutional path-dependency maintains the constraint').

omega_variable(
    portable_retirement_adequacy,
    'Can alternative mechanisms (multi-employer plans, portable IRAs with auto-contribution features) deliver equivalent retirement security gains with lower extraction from low-wage workers?',
    'Pilot analysis of alternative designs; comparison of retirement outcomes for workers with access to portable accounts vs traditional automatic enrollment',
    'If alternatives deliver equal/superior outcomes with lower extraction: scaffold sunset is real — constraint will degrade to piton then obsolescence. If alternatives fail: automatic enrollment remains the least-bad option despite extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portable_retirement_adequacy, empirical, 'Whether alternative mechanisms can replace automatic enrollment without reducing retirement security').

omega_variable(
    default_rate_paternalism_boundary,
    'Is there a principled distinction between automatic enrollment (default participation) and automatic contribution rate defaults, or does the paternalism concern apply equally to both?',
    'Normative analysis of behavioral autonomy; comparison of participation-rate defaults vs contribution-rate defaults in terms of value preservation for the individual',
    'If distinction is principled: enrollment defaults remain tangled rope (solving participation problem without imposing rates). If distinction dissolves: entire mechanism shifts toward paternalism critique, elevating piton classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(default_rate_paternalism_boundary, preference, 'Whether paternalism boundary exists between enrollment vs contribution defaults').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(automatic_enrollment_defaults, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aed_tr_t0, automatic_enrollment_defaults, theater_ratio, 0, 0.28).
narrative_ontology:measurement(aed_tr_t5, automatic_enrollment_defaults, theater_ratio, 5, 0.38).
narrative_ontology:measurement(aed_tr_t10, automatic_enrollment_defaults, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(aed_be_t0, automatic_enrollment_defaults, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(aed_be_t5, automatic_enrollment_defaults, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(aed_be_t10, automatic_enrollment_defaults, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(automatic_enrollment_defaults, resource_allocation).
narrative_ontology:affects_constraint(automatic_enrollment_defaults, retirement_savings_adequacy).
narrative_ontology:affects_constraint(automatic_enrollment_defaults, household_financial_liquidity).
narrative_ontology:affects_constraint(automatic_enrollment_defaults, behavioral_commitment_mechanisms).

% DUAL FORMULATION NOTE:
% Automatic enrollment can be decomposed into two structurally distinct constraints: (1) automatic_enrollment_participation — the mechanism that increases plan participation through behavioral default (lower ε, higher coordination benefit), and (2) automatic_enrollment_extraction — the uniform default contribution rate that produces income-dependent extraction (higher ε, asymmetric impact). This story models the hybrid (both effects simultaneously). Alternative decomposition would assign the participation mechanism to coordination and the default-rate effect to extraction, allowing separate treatment. Current story treats them as inseparable because policy interventions typically affect both together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(automatic_enrollment_defaults, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
