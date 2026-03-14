% ============================================================================
% CONSTRAINT STORY: universal_credit_taper_cliff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_universal_credit_taper_cliff, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: universal_credit_taper_cliff
 *   human_readable: Universal Credit Taper Cliff: Work Incentive Vs. Welfare Extraction
 *   domain: economic_policy/social_welfare
 *
 * SUMMARY:
 *   The Universal Credit taper cliff is a structural mechanism in UK welfare
 *   policy where benefits phase out at 65 pence per pound of additional
 *   earnings (as of 2024; previously 63%). This creates a de facto marginal
 *   tax rate of 65% for workers in the taper zone — substantially higher than
 *   the standard income tax rate of 20%. The constraint operates as a hybrid:
 *   it coordinates unified benefit administration and work incentive
 *   maintenance (genuine coordination function) while simultaneously
 *   extracting earnings from low-income workers through the aggressive
 *   phase-out rate (extraction mechanism). The taper cliff is not incidental
 *   to UC — it is a designed feature intended to balance fiscal control with
 *   work incentives. This makes it a canonical Tangled Rope: coordination +
 *   designed extraction. The constraint's theater ratio (0.35) is low because
 *   the mechanism is transparent (no performative obscuring), but the
 *   extractiveness (0.58) is elevated relative to pure coordination systems
 *   (e.g., Nordic welfare models with 15-20% taper rates) because the UK rate
 *   appears to extract beyond what coordination requires.
 *
 * KEY AGENTS:
 *   - Low-Income Workers in Taper Zone: Primary victims (powerless/trapped) — earn incrementally but lose 65 pence per pound to benefit phase-out; constrained by childcare, transport, skills barriers; no realistic exit from taper zone without major life restructuring
 *   - Treasury/Fiscal Authority: Primary beneficiary (institutional/arbitrage) — captures reduced welfare spending and maintains fiscal control narrative; can change taper rate unilaterally
 *   - Welfare Reform Coalition: Organized agents (organized/constrained) — advocacy groups, unions push for lower taper rates; have institutional voice but limited veto power over policy
 *   - Welfare Bureaucracy: Institutional maintainer (institutional/arbitrage) — administers taper calculations, reassessments, sanctions; maintains performative compliance infrastructure
 *   - High-Income Non-Recipients: Structural beneficiaries (powerful/mobile) — experience UC as temporary poverty management infrastructure, receive no extraction because above eligibility threshold
 *   - Analytical Observer: Civilizational position (analytical/analytical) — integrates all perspectives; detects both genuine coordination and designed extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(universal_credit_taper_cliff, 0.58).
domain_priors:suppression_score(universal_credit_taper_cliff, 0.65).
domain_priors:theater_ratio(universal_credit_taper_cliff, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(universal_credit_taper_cliff, extractiveness, 0.58).
narrative_ontology:constraint_metric(universal_credit_taper_cliff, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(universal_credit_taper_cliff, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(universal_credit_taper_cliff, tangled_rope).
narrative_ontology:human_readable(universal_credit_taper_cliff, "Universal Credit Taper Cliff: Work Incentive Vs. Welfare Extraction").
narrative_ontology:topic_domain(universal_credit_taper_cliff, "economic_policy/social_welfare").

domain_priors:requires_active_enforcement(universal_credit_taper_cliff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(universal_credit_taper_cliff, treasury_fiscal_control).
narrative_ontology:constraint_beneficiary(universal_credit_taper_cliff, high_earners_relative_safety).
narrative_ontology:constraint_victim(universal_credit_taper_cliff, low_income_workers).
narrative_ontology:constraint_victim(universal_credit_taper_cliff, benefit_claimants_in_taper_zone).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED LOW-INCOME WORKER (SNARE) — Faces effective marginal tax rate of 65% in the taper zone (standard taper rate). Each additional pound earned results in 65 pence deducted from benefits. No meaningful exit: childcare, transport, and skill constraints prevent jumping above the taper threshold. Suppression is structural and economic — the worker is trapped between benefit dependency and unaffordable cost-of-living if employment increases. Pure extraction with minimal coordination function. Maximum experienced chi.
constraint_indexing:constraint_classification(universal_credit_taper_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTRAINED PARTIAL WORKER (TANGLED ROPE) — Some benefits from part-time work coordination (income + benefits combined provides subsistence). But asymmetric extraction: the taper mechanism extracts earnings as benefits phase out. Exit is possible (take full-time work above taper threshold, or accept zero benefits), but at high cost (transport, childcare arrangements, loss of safety net during transition). Genuine coordination (benefits + work = survival) but with embedded extraction (taper regime).
constraint_indexing:constraint_classification(universal_credit_taper_cliff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY / FISCAL AUTHORITY (ROPE) — Experiences the taper cliff as a coordination mechanism: benefits administration + work incentives = poverty control without unlimited spending. The 65% taper is a calibrated coordination tool. Treasury benefits from: (a) reduced gross welfare payouts, (b) maintained work incentive (though weakened), (c) fiscal constraint satisfaction. Net beneficiary. Arbitrage exit available (change the taper rate or abolish UC). Effective extraction runs toward this agent — benefits extraction from workers toward state coffers.
constraint_indexing:constraint_classification(universal_credit_taper_cliff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WELFARE REFORM COALITION (TANGLED ROPE) — Organized actors (labor unions, poverty advocacy groups) see genuine coordination function (unified welfare system replacing means-tested fragmentation) alongside extraction (taper cliff creates perverse work disincentives). Coalition has agency (lobbying power, alternative proposal capacity) but faces institutional constraints. Advocates point to 20% phase-in rates in other systems (Germany, Nordic models) as evidence that coordination doesn't require 65% extraction. Constrained but not powerless.
constraint_indexing:constraint_classification(universal_credit_taper_cliff, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE BUREAUCRACY (PITON) — Universal Credit administration maintains substantial performative overhead: complex taper calculations, frequent reassessment cycles, compliance monitoring, sanctions regime. Much of the administrative apparatus persists from legacy means-tested systems (Tax Credits). Theater ratio reflects that enforcement and verification costs are high relative to actual welfare distribution. Original function (coordinate welfare access) has been substantially replaced by function (enforce work incentive compliance). Piton: degraded coordination mechanism maintained through institutional inertia.
constraint_indexing:constraint_classification(universal_credit_taper_cliff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HIGH-INCOME NON-RECIPIENT (SCAFFOLD) — Well-above taper threshold; experiences UC as temporary coordination infrastructure for poverty management. Marginal tax rate (20%) is substantially lower than taper zone rate (65%). Receives no direct extraction because income is above UC eligibility. Sees welfare system as time-limited problem to be solved (reduce dependency, encourage work). This perspective is *temporary support* relative to powerless agent — scaffold classification reflects that UC is conceived as transitional poverty management, not permanent extraction from the high-income perspective.
constraint_indexing:constraint_classification(universal_credit_taper_cliff, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Integrating all positions: UC clearly coordinates welfare access and work incentives (coordination function is real, ε baseline ~0.30 for pure coordination). But the 65% taper cliff generates asymmetric extraction: the mechanism extracts earnings from workers to reduce fiscal burden on the state. This is not a flaw in coordination — it's a designed feature. The taper IS the extraction lever. Effective ε ≈ 0.58 when empirically measured across affected populations. Classification: Tangled Rope (coordination + designed extraction hybrid).
constraint_indexing:constraint_classification(universal_credit_taper_cliff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(universal_credit_taper_cliff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(universal_credit_taper_cliff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(universal_credit_taper_cliff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(universal_credit_taper_cliff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(universal_credit_taper_cliff, TR),
    TR >= 0.70.

:- end_tests(universal_credit_taper_cliff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The UC system coordinates benefits + work, which is genuine coordination (ε baseline ~0.30). But the 65% taper rate adds extraction beyond what coordination requires. Comparative evidence (Germany 12%, Nordic 15-20%) suggests coordination is achievable at 20-30% ε. The UK's 58% ε represents 0.28+ ε points of pure extraction relative to lower-taper-rate systems. This extraction is deliberate — calibrated by Treasury to reduce fiscal burden while nominally maintaining work incentives. Suppression (0.65): High. Workers in the taper zone face multiple barriers: childcare costs, transport, skills gaps, and crucially, the disincentive of earning and losing 65 pence per pound. Suppression is structural (external barriers) + behavioral (work disincentive). Theater ratio (0.35): Low. UC administration is relatively transparent — the taper rate is explicit policy, not hidden through administrative opacity. Complex bureaucracy exists (reassessments, sanctions, compliance monitoring), but the extraction mechanism itself is legible. Low theater reflects that this is not a hidden or obscured constraint — it is explicit policy defended on work-incentive grounds.
 *
 * PERSPECTIVAL GAP:
 *   Trapped workers and Treasury have opposite experienced extractiveness. The trapped worker's snare classification (maximum chi) reflects that they perceive the 65% taper as inescapable economic punishment for earning. The Treasury's rope classification reflects that they perceive the taper as a calibrated coordination mechanism. Both are partially correct: the coordination is real (unified system is better than fragmented), but so is the extraction (65% is higher than comparative systems require). The Tangled Rope classification from the analytical perspective resolves this gap by accepting both as structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position. Trapped workers: high d (0.92+) — full victims, no exit options, experience maximum extraction via f(d). Treasury/institutional: low d (0.08-0.15) — beneficiary position with arbitrage exit, experiences negative effective extraction (extraction flows toward them). Organized reform coalition: moderate d (0.55) — victim of the constraint's design (advocating for change) but with some agency, experience moderate extraction. The analytical observer at institutional power with analytical exit: d ≈ 0.73 (observer position, structured asymmetry is visible). Directionality overrides not needed — derived d values accurately reflect structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: This constraint demonstrates the distinction between coordination function and extraction mechanism. Pure Rope would be: unified benefits administration + work incentives without excessive phase-out (e.g., 20% taper like Germany). Pure Snare would be: extraction mechanism with no coordination function. UK UC is Tangled Rope because both exist. The 65% taper rate is *designed extraction* — not incidental overhead or enforcement cost, but an intentional fiscal lever. This distinguishes it from Rope (where extraction is minimal spillover) and from Snare (where coordination is minimal cover). The constraint resolves mandatrophy by showing that high ε and suppression are compatible with genuine coordination function when the extraction is designed rather than emergent. The false summit risk is whether policymakers/economists defend 65% as 'necessary for incentives' when comparative evidence suggests 20-25% would suffice — if so, the natural-law framing is false, and the constraint is contingent policy, not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_response_magnitude,
    'What is the empirical elasticity of labor supply response to the 65% taper rate? Does the suppression of work behavior actually reduce overall employment, or do most workers continue working at constrained hours?',
    'Randomized controlled trial comparing labor supply in 65% taper regime vs. 20% phase-in regimes (Germany, Nordic models). Administrative data on hours worked pre/post taper rate changes (e.g., 2024 reduction from 63% to 55%).',
    'If elasticity is high (workers exit labor force): extraction mechanism is working as designed, and classification remains Tangled Rope. If elasticity is low (workers tolerate suppression): behavior is not actually suppressed, and suppression metric should be downgraded, potentially reclassifying as Rope. This determines whether suppression is structural or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_response_magnitude, empirical, 'Empirical labor supply elasticity response to taper rate').

omega_variable(
    coordination_function_necessity,
    'Is the 65% taper rate necessary for the coordination function (unified benefits administration + work incentive maintenance), or is it an extractive layer added on top of functioning coordination?',
    'Comparative policy analysis: do systems with lower taper rates (15-25%) achieve similar poverty reduction and work incentive maintenance? Evidence: Germany (12% withdrawal rate), Switzerland (15%), Nordic countries (15-20% typical). If lower rates work, the 65% UK rate is extraction, not coordination cost.',
    'If lower rates are sufficient: extraction is 0.30+ ε points of pure rent-seeking, potentially reclassifying UK UC as Snare rather than Tangled Rope. If lower rates fail: 65% is coordination floor, and classification as Tangled Rope is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether lower taper rates maintain coordination function').

omega_variable(
    suppression_internalization,
    'Is the suppression of work behavior structural (external barriers: childcare, transport, skills gap) or internalized (cognitive capture: recipients believe they ''should not'' work, or deserve poverty, or are incapable)?',
    'Post-exit analysis: recipients who move above taper threshold (either through increased earnings or benefits removal) — do they maintain reduced work attachment, or do hours recover? If recovery occurs, suppression was structural; if it persists, suppression is partially internalized.',
    'If internalized: effective suppression is higher than the metric (0.65) suggests — the constraint carries forward even after mechanism removal. If structural: suppression is accurate, and removal of taper would yield immediate work behavior recovery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of work behavior is structural or internalized').

omega_variable(
    false_summit_natural_law,
    'Is the 65% taper rate presented as a natural law of welfare economics (''incentive compatibility requires high phase-in''; ''work incentives require high marginal rates'') when it is actually a contingent policy choice?',
    'Institutional history: trace rhetoric in policy documents and academic defense of UC design. Compare to historical policy rationales (similar language used to defend other welfare regimes that are now recognized as extractive). Evidence: if other systems achieve similar poverty control with 20% taper rates, the 65% ''natural law'' framing is false naturalization.',
    'If confirmed: the mountain perspective (welfare taper is immutable physics of incentive compatibility) is a false summit, revealing that the constraint''s persistence relies on naturalizing contingent choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether taper rate is naturalized as immutable law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(universal_credit_taper_cliff, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uctc_tr_t0, universal_credit_taper_cliff, theater_ratio, 0, 0.28).
narrative_ontology:measurement(uctc_tr_t3, universal_credit_taper_cliff, theater_ratio, 3, 0.32).
narrative_ontology:measurement(uctc_tr_t6, universal_credit_taper_cliff, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(uctc_be_t0, universal_credit_taper_cliff, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uctc_be_t3, universal_credit_taper_cliff, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(uctc_be_t6, universal_credit_taper_cliff, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(universal_credit_taper_cliff, resource_allocation).
narrative_ontology:boltzmann_floor_override(universal_credit_taper_cliff, 0.25).
narrative_ontology:affects_constraint(universal_credit_taper_cliff, in_work_poverty_trap).
narrative_ontology:affects_constraint(universal_credit_taper_cliff, childcare_cost_barrier).
narrative_ontology:affects_constraint(universal_credit_taper_cliff, regional_wage_variation).

% DUAL FORMULATION NOTE:
% UC taper cliff is downstream of the broader welfare state restructuring (shift from means-tested Tax Credits to unified Universal Credit). Upstream constraint: welfare_consolidation_transition (ε ~0.35, Tangled Rope). The taper cliff represents extraction embedded within the coordination mechanism of UC itself. Separate from UC, lower-taper-rate systems (Germany resource_allocation_welfare, ε ~0.28, Rope) represent alternative coordination with less extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(universal_credit_taper_cliff, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
