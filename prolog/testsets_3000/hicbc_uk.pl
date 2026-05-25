% ============================================================================
% CONSTRAINT STORY: hicbc_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hicbc_uk, []).

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
 *   constraint_id: hicbc_uk
 *   human_readable: UK High Income Child Benefit Charge (HICBC)
 *   domain: economic/fiscal_policy
 *
 * SUMMARY:
 *   The High Income Child Benefit Charge (HICBC), introduced in January 2013,
 *   claws back Child Benefit from families where at least one partner earns
 *   above a threshold (now £60,000, with phase-out to full clawback at
 *   ~£80,000). The constraint embodies a fundamental tension between two
 *   legitimate policy objectives: (1) maintaining fiscal sustainability by
 *   not paying full Child Benefit to high-income families, and (2) preserving
 *   work incentives by avoiding marginal tax rates so steep that additional
 *   earnings produce no net gain. The design exhibits both genuine
 *   coordination (targeting by income) and genuine extraction (perverse
 *   marginal incentives). The clawback applies not per child but to the
 *   entire Child Benefit entitlement, creating a cliff-edge effect that
 *   disproportionately affects dual-earner families where one partner crosses
 *   the threshold. This makes the constraint analyzable as tangled_rope from
 *   multiple perspectives: the Treasury sees legitimate fiscal coordination,
 *   the marginal earner sees a snare, labor advocates see mixed
 *   coordination-extraction, and universal-credits reformers see it as a
 *   temporary scaffold to be dismantled. The rising theater_ratio (0.48 →
 *   0.58) reflects increasing gap between the stated purpose ('high income'
 *   families) and actual incidence (captured middle-class dual-earners),
 *   indicating Goodhart drift as the mechanism becomes a target.
 *
 * KEY AGENTS:
 *   - Marginal Earners: Primary victims (powerless/trapped) — families where one partner's income crosses £60k threshold; experience total clawback with no exit option
 *   - Dual-Earner Households: Secondary victims (moderate/constrained) — face effective marginal tax rates >60% around threshold; can constrain hours but with career cost
 *   - Lower-Income Households: Beneficiaries (powerless/trapped) — receive full Child Benefit; benefit from the fiscal targeting, though not from the design's work-disincentive properties
 *   - Treasury & HM Revenue & Customs: Institutional beneficiary (institutional/arbitrage) — achieves fiscal objective of limiting benefit cost; can adjust threshold and phase-out rates
 *   - Labor Organizations & Family Advocacy Groups: Organized actors (organized/constrained) — benefit from policy as focal point for reform advocacy but trapped by persistence despite evidence
 *   - Welfare Reform Coalition: Organized actors (organized/mobile) — see HICBC as temporary dysfunction within broader income-support system; advocate for consolidated replacement (UBI, negative income tax, unified credits)
 *   - Analytical Observer: Structural analyst (analytical/analytical) — identifies stable tangled_rope classification across all measurement bases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hicbc_uk, 0.38).
domain_priors:suppression_score(hicbc_uk, 0.52).
domain_priors:theater_ratio(hicbc_uk, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hicbc_uk, extractiveness, 0.38).
narrative_ontology:constraint_metric(hicbc_uk, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hicbc_uk, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hicbc_uk, tangled_rope).
narrative_ontology:human_readable(hicbc_uk, "UK High Income Child Benefit Charge (HICBC)").
narrative_ontology:topic_domain(hicbc_uk, "economic/fiscal_policy").

domain_priors:requires_active_enforcement(hicbc_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hicbc_uk, lower_income_households).
narrative_ontology:constraint_beneficiary(hicbc_uk, public_revenue_pool).
narrative_ontology:constraint_victim(hicbc_uk, higher_earning_families).
narrative_ontology:constraint_victim(hicbc_uk, work_incentive_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL EARNER AT THRESHOLD (SNARE) — A family where one partner's income crosses the £60k threshold experiences sudden, total clawback of Child Benefit. No negotiation possible, no exit option. The constraint operates via fiscal mechanism, not institutional choice. The earner is trapped: earning slightly more results in net loss due to marginal tax rate + clawback. Maximal experienced extraction with suppression of alternative earnings paths.
constraint_indexing:constraint_classification(hicbc_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED WORKERS & FAMILY ADVOCATES (TANGLED ROPE) — These organizations benefit from the HICBC as a focal point for labor advocacy (demonstrating how fiscal policy undermines work incentives) but are trapped by the constraint's persistence despite documented perverse effects. They see both coordination (the HICBC makes visible the need for income support reform) and extraction (policy persists despite their evidence that it discourages work). Exit is constrained by political economy — the constraint requires legislative repeal.
constraint_indexing:constraint_classification(hicbc_uk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY & FISCAL AUTHORITIES (ROPE) — For fiscal planners, the HICBC solves a coordination problem: targeting support to lower-income households while managing the fiscal envelope. The mechanism is extractive in effect but coordinative in intent. The Treasury can arbitrage by adjusting thresholds and phase-out rates; it has policy agency. Net beneficiary position: the constraint generates revenue while appearing fiscally neutral (reclaimed benefit = reduced spending).
constraint_indexing:constraint_classification(hicbc_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UBI & REFORM ADVOCATES (SCAFFOLD) — See the HICBC as a temporary, dysfunctional stopgap that will be replaced by consolidated income support (UBI, negative income tax, or expanded universal credits). The constraint has a sunset: as policy reform matures, consolidated systems will obsolete the clawback mechanism entirely. Low experienced extraction because advocates see an exit path (legislative reform) with clear timeline and backing.
constraint_indexing:constraint_classification(hicbc_uk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL MEANS-TESTING MACHINERY (PITON) — The HICBC is a vestigial means-test grafted onto a nominally universal benefit. Means-testing itself is degraded institutional logic — the performative distinction between 'universal' and 'means-tested' persists despite both being targeting mechanisms. The theater ratio is high: the benefit is marketed as 'universal Child Benefit' while being clawed back at income levels many consider middle-class. The machinery persists through inertia — replacing it requires legislative overhaul, which political economy has repeatedly deferred.
constraint_indexing:constraint_classification(hicbc_uk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a structural standpoint, the HICBC exhibits both genuine coordination (targeting by income) and genuine extraction (perverse marginal incentives, administrative overhead). The classification as tangled_rope is stable across observables: the mechanism simultaneously solves a targeting problem and creates a work-disincentive problem. No natural law reading is credible here — the constraint is a policy artifact with clear design intent.
constraint_indexing:constraint_classification(hicbc_uk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hicbc_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hicbc_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hicbc_uk, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hicbc_uk, TR),
    TR >= 0.70.

:- end_tests(hicbc_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The HICBC extracts income via the tax system from families above the threshold, but the extraction is partial (phase-out, not cliff-edge in all scenarios) and serves a legitimate coordination objective (income-targeting). The 0.28→0.38 trajectory reflects that the initial design was more coordinative (focus on limiting benefit cost) while later iterations added extractive elements (narrower definitions of 'high income' as real earnings rose, failure to index threshold). Suppression (0.52): Moderate-high. Alternatives are suppressed by the mechanism's legality and universality — families cannot opt out or negotiate. But suppression is not total: organized labor has visible exit paths (legislative reform, welfare consolidation), and individual families have constrained exit (spousal income-shifting, part-time work). Theater ratio (0.58): Moderate-high. The performance gap between 'targeting high-income families' and actual incidence (dual-earner professionals earning £50-70k) creates substantial theater. The marketing as 'Child Benefit' (universal frame) while applying a means-test (targeted frame) adds performative layer. The rising trajectory reflects that as real earnings growth has continued, the nominal 'high income' threshold has captured an increasingly middle-class population, making the theater more visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications across structural positions. The marginal earner (powerless/trapped) experiences it as pure extraction (Snare) with no alternatives. The Treasury (institutional/arbitrage) experiences it as coordination (Rope) — solving the fiscal targeting problem. Welfare reformers (organized/mobile) experience it as temporary dysfunction (Scaffold) with a clear sunset path. The historical means-testing apparatus (institutional/arbitrage with high theater) appears as Piton — degraded machinery persisting through inertia. Labor advocates (organized/constrained) experience it as mixed coordination-extraction (Tangled Rope) — it makes visible the problem they're trying to solve, but it also traps them in defending against its effects. The analytical observer sees stable Tangled Rope across all observables because the mechanism genuinely exhibits both coordination (targeting) and extraction (work disincentive) simultaneously. The perspectival gap is not observational variance but structural: different agents have genuinely different exit options and benefit-cost asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural relationship to the extraction flow. Lower-income households have d ≈ 0.05-0.15 (beneficiaries with arbitrage options—they can adjust family labor supply without losing benefit) → negative f(d) → they experience the constraint as coordination (benefit support). Marginal earners have d ≈ 0.90 (victims with trapped options—their income is the clawback trigger, and they cannot escape it without earning less) → high f(d) ≈ 1.30 → maximum experienced extraction. Treasury has d ≈ 0.10 (beneficiary institution with arbitrage—it can adjust threshold, phase-out rate, and administration) → negative f(d) ≈ -0.08 → coordination experience. Organized labor has d ≈ 0.65 (victim-advocate with constrained options—they benefit from the policy as evidence for reform but are trapped advocating against it) → moderate f(d) ≈ 1.0 → mixed experience. The derived d values correctly rank the agent's structural extraction burden.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint classifies as tangled_rope from the analytical perspective because it exhibits both genuine coordination (income-targeting for fiscal sustainability) and genuine extraction (perverse marginal incentives, regressivity among dual-earner professionals). The mandatrophy question is not 'which type is correct?' but 'which design intent dominates?' The answer is empirical: the behavioral response magnitude (omega_1) determines whether the extraction is incidental (large coordination, small extraction → Rope) or equal (Tangled Rope) or dominant (small coordination, large extraction → Snare). Current evidence suggests moderate behavioral response and moderate work-disincentive effects, supporting tangled_rope classification. The Piton perspective (high theater, degraded machinery) reflects that the policy has drifted from its stated intent (targeting wealthy families) to its revealed incidence (capturing dual-earner professionals), a classic Goodhart drift signature. The Scaffold perspective (temporary dysfunction) reflects that genuine welfare reform alternatives exist (consolidated income support) and are politically salient, though inertia has delayed reform. The snare perspective (marginal earner experience) is real and should not be dismissed as 'just a policy choice'—the constraint's operation creates genuine traps in the marginal incentive structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_response_magnitude,
    'How large is the actual behavioral response to HICBC-induced marginal tax rates? Do families actually reduce work hours, or is the clawback economically small relative to household income?',
    'Labor supply elasticity studies; comparison of work-hour changes at HICBC threshold vs control groups; regression discontinuity analysis at £60k income threshold',
    'If elasticity > 0.3: extraction classification strengthens (Snare from target perspective). If elasticity < 0.1: coordination classification strengthens (Rope from Treasury perspective) — the policy achieves targeting with minimal distortion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_response_magnitude, empirical, 'Whether HICBC causes significant labor supply reduction or is economically negligible').

omega_variable(
    threshold_selection_intent,
    'Was the £60k threshold chosen to target genuinely high-income families, or was it set knowing it would capture dual-earner middle-class households and thereby increase effective phase-out?',
    'Policy memoranda and Treasury analysis from threshold-setting period (2012-2013); examination of fiscal impact forecasts vs realized distributional effects; interviews with policymakers',
    'If threshold was mis-estimated: classification shifts toward Scaffold (temporary error, will be corrected). If threshold was deliberately set to capture middle class: classification shifts toward Snare (intentional regressive extraction disguised as ''high income'' targeting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_selection_intent, conceptual, 'Whether HICBC threshold reflects genuine high-income targeting or deliberate middle-class capture').

omega_variable(
    reform_political_economy,
    'What prevents legislative repeal or consolidation of HICBC into a unified income support system? Is it genuine fiscal constraint, or political economy inertia (budget rules, salience bias, path dependence)?',
    'Comparative analysis of welfare reforms in UK vs OECD peers; timeline of reform proposals and blocking coalitions; cost-benefit analysis of consolidation scenarios',
    'If genuine fiscal constraint: Scaffold classification is aspirational (reform harder than advocates assume). If inertia: Piton classification confirmed (degraded machinery persists despite viable alternatives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_political_economy, preference, 'Whether HICBC persists due to fiscal necessity or political economy inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hicbc_uk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hicbc_tr_t0, hicbc_uk, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hicbc_tr_t5, hicbc_uk, theater_ratio, 5, 0.54).
narrative_ontology:measurement(hicbc_tr_t10, hicbc_uk, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(hicbc_be_t0, hicbc_uk, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hicbc_be_t5, hicbc_uk, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(hicbc_be_t10, hicbc_uk, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hicbc_uk, resource_allocation).
narrative_ontology:affects_constraint(hicbc_uk, uk_child_poverty_trap).
narrative_ontology:affects_constraint(hicbc_uk, dual_earner_work_incentive_structure).

% DUAL FORMULATION NOTE:
% The HICBC is distinct from but structurally related to broader UK means-testing apparatus (Universal Credit taper, child poverty trap). Each constraint has its own extractiveness value reflecting the specific mechanism, but they form a family linked by shared fiscal targeting logic and reform coalitions. The HICBC's moderate extractiveness (0.38) makes it a focal point for demonstrating how coordination objectives can be undermined by extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hicbc_uk, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
