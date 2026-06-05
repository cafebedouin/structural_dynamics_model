% ============================================================================
% CONSTRAINT STORY: universal_child_benefit_universality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_universal_child_benefit_universality, []).

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
 *   constraint_id: universal_child_benefit_universality
 *   human_readable: Universal Child Benefit Universality Constraint
 *   domain: social_policy/welfare_economics
 *
 * SUMMARY:
 *   The universal child benefit universality constraint captures a
 *   fundamental structural tension in welfare state design: the gap between
 *   the nominal promise of universal coverage and the actual implementation
 *   that targets benefits through means-testing. This constraint demonstrates
 *   how the same policy mechanism appears radically different depending on
 *   the observer's structural position. To high-income households, the
 *   program is pure coordination — a simple mechanism ensuring child welfare.
 *   To low-income families, the same program becomes a snare: means-testing
 *   creates administrative burden, clawback mechanisms suppress work
 *   incentives, and the extraction of targeting eligibility determination
 *   falls disproportionately on those with least capacity to navigate
 *   bureaucratic complexity. The constraint has strengthened over the 15-year
 *   interval as means-testing thresholds have contracted, benefit levels have
 *   stagnated relative to costs of child-rearing, and administrative
 *   complexity has increased. Theater ratio has risen (0.42 to 0.58) as the
 *   means-testing machinery has become increasingly elaborate relative to
 *   actual savings achieved. Extractiveness has increased (0.38 to 0.52) as
 *   the gap between universal promise and targeted delivery has widened.
 *
 * KEY AGENTS:
 *   - Low-Income Families: Primary victim (powerless/trapped) — bear full suppression of administrative complexity, clawback disincentives, and stigma; cannot exit
 *   - Working-Class Parents: Secondary victim (moderate/constrained) — experience both program support and work disincentives; constrained by employment options and wage dynamics
 *   - Government Administration: Primary beneficiary (institutional/arbitrage) — captures budgetary arbitrage through means-testing design; can adjust parameters without bearing costs
 *   - High-Income Households: Secondary beneficiary (powerful/arbitrage) — receive full or near-full benefits with minimal administrative burden; extract pure coordination benefit
 *   - Welfare Reform Coalition: Organized agent (organized/constrained) — seeks systemic reform path through expanded public services; sees current benefit as temporary scaffold
 *   - Means-Testing Apparatus: Institutional actor (institutional/arbitrage) — maintains performative administrative machinery; benefits from continued complexity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees hybrid coordination-extraction structure; identifies false universality claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(universal_child_benefit_universality, 0.52).
domain_priors:suppression_score(universal_child_benefit_universality, 0.48).
domain_priors:theater_ratio(universal_child_benefit_universality, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(universal_child_benefit_universality, extractiveness, 0.52).
narrative_ontology:constraint_metric(universal_child_benefit_universality, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(universal_child_benefit_universality, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(universal_child_benefit_universality, tangled_rope).
narrative_ontology:human_readable(universal_child_benefit_universality, "Universal Child Benefit Universality Constraint").
narrative_ontology:topic_domain(universal_child_benefit_universality, "social_policy/welfare_economics").

domain_priors:requires_active_enforcement(universal_child_benefit_universality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(universal_child_benefit_universality, high_income_households).
narrative_ontology:constraint_beneficiary(universal_child_benefit_universality, government_budgetary_balance).
narrative_ontology:constraint_victim(universal_child_benefit_universality, low_income_families).
narrative_ontology:constraint_victim(universal_child_benefit_universality, program_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME FAMILY (SNARE) — Trapped by economic necessity. A truly universal program promises equal access, but means-testing or clawback mechanisms create invisible extraction barriers. Low-income families bear the suppression of administrative complexity and stigma while high-income families extract the full benefit. No exit option; maximum experienced extraction.
constraint_indexing:constraint_classification(universal_child_benefit_universality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-CLASS PARENT (TANGLED ROPE) — Constrained by wage levels and employment stability. Experiences both genuine coordination (the program provides essential child-rearing support) and extraction (means-testing reduces benefit as income rises, creating perverse work disincentives). Moderate agency but significant structural asymmetry.
constraint_indexing:constraint_classification(universal_child_benefit_universality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENT ADMINISTRATION (ROPE) — Experiences the constraint as pure coordination. The benefit structure solves a collective action problem (ensuring child welfare without individualizing shame) and enables administrative arbitrage through means-testing design. The government can adjust universality parameters without bearing costs.
constraint_indexing:constraint_classification(universal_child_benefit_universality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-INCOME HOUSEHOLD (ROPE) — Sees the constraint as pure coordination benefit. Receives the full universal benefit (or near-full if means-testing exempts higher brackets) while extracting value from the program's efficiency and simplicity. No suppression experienced; maximum benefit capture.
constraint_indexing:constraint_classification(universal_child_benefit_universality, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE REFORM COALITION (SCAFFOLD) — Organized actors (poverty advocacy groups, progressive economists) see universality as a temporary coordination bridge with a sunset: full decommodification of child welfare through public childcare, education, and healthcare would make the benefit redundant. The constraint is temporary support pending systemic reform. Has exit pathway and organized agency.
constraint_indexing:constraint_classification(universal_child_benefit_universality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MEANS-TESTING APPARATUS (PITON) — The administrative machinery for income verification, clawback calculation, and benefit adjustment has become substantially theatrical. The means-testing process often costs more to administer than the targeted savings it achieves. The apparatus persists through institutional inertia despite low functional efficiency — maintained because political consensus hasn't fully shifted to pure universality, not because targeting works.
constraint_indexing:constraint_classification(universal_child_benefit_universality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global comparative perspective, the constraint reveals a hybrid: genuine coordination function (child welfare support solves collective action around child rearing) coupled with asymmetric extraction (targeting mechanisms concentrate benefits among high-income households while imposing administrative burden on low-income recipients). The tension is structural, not resolvable through design tweaks to means-testing thresholds.
constraint_indexing:constraint_classification(universal_child_benefit_universality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(universal_child_benefit_universality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(universal_child_benefit_universality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(universal_child_benefit_universality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(universal_child_benefit_universality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(universal_child_benefit_universality, TR),
    TR >= 0.70.

:- end_tests(universal_child_benefit_universality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The policy nominally provides coordination (ensuring child welfare without stigma) but actual implementation extracts through targeting: means-testing creates compliance costs disproportionately borne by low-income families, clawback mechanisms create work disincentives, and the gap between flat nominal rates and inflation-adjusted purchasing power has widened. The 0.52 value reflects that the coordination function (50% of program delivery) is genuine but increasingly subordinate to the extraction function (benefit concentration among higher-income brackets through exemption thresholds and inflation decay). Suppression (0.48): Moderate. Significant structural barriers include means-testing complexity, clawback work disincentives, and stigma. But suppression is not total — families can and do claim benefits, and organizational advocates provide navigation support. The value reflects real but partially surmountable barriers. Theater ratio (0.58): Moderate-high. Means-testing administration involves substantial performative activity (income verification, threshold checking, eligibility reassessment) that often costs more to implement than the targeting savings achieved. The ratio has risen over time as administrative complexity has increased relative to actual program targeting efficiency.
 *
 * PERSPECTIVAL GAP:
 *   The universal child benefit illustrates how the same policy can be perceived as pure coordination (Rope) by beneficiaries with exit options and pure extraction (Snare) by those locked into dependency. The gap reflects not disagreement about facts but genuine structural asymmetry: the program's design concentrates administrative burden on low-income recipients while concentrating benefit concentration among higher-income households. The high-income family has little reason to question the program's fairness (they receive full benefit). The low-income family experiences the program's fairness claims as false — the nominally universal benefit becomes means-tested extraction once administrative complexity is factored in.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's position relative to the extraction flow. High-income beneficiaries with arbitrage options experience low d (0.10-0.20), producing negative or near-zero effective extraction chi. Government administrators similarly experience low d (arbitrage, no direct cost bearing). Low-income families trapped by economic necessity experience high d (0.85-0.95), producing maximum experienced extraction chi. Working-class parents constrained by employment options experience medium-high d (0.60-0.70). The coalition with organized exit pathways experiences medium d (0.40-0.50). The means-testing apparatus maintains institutional arbitrage, thus low d. The analytical observer at civilizational scope derives d = 0.72 (observer position), revealing the structural asymmetry that individuals in the system cannot perceive from their trapped or constrained positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is resolved through recognizing that 'universal' has been semantically captured. Policy calls the program 'universal child benefit' but implements targeted means-testing. The nominal universality (all families eligible in principle) is contradicted by actual universality (equal benefit to all regardless of income). The program cannot simultaneously be both: either it is truly universal (flat payment, no means-testing) or it is targeted. The current design extracts the coordination legitimacy of universalism while implementing the extraction efficiency of targeting. The Tangled Rope classification acknowledges this hybrid: genuine coordination function (child welfare support) coupled with asymmetric extraction (benefit concentration through targeting). Resolution requires choosing: move toward true universality (flat payment, sunset means-testing apparatus) or acknowledge the program as targeted extraction and defend targeting on efficiency grounds rather than falsely claiming universality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_definition_ambiguity,
    'Does ''universal child benefit'' mean equal flat payment to all families, or equal access with income-adjusted claiming?',
    'Policy document analysis and implementation audit. Cross-national comparison of claimed vs actual universality (e.g., comparing UK Child Benefit to means-tested programs in other countries).',
    'If defined as flat payment: extraction is lower (truly universal). If defined as needs-based access: extraction is higher (means-testing imposes suppression). Classification shifts between Rope (flat) and Snare (means-tested).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_definition_ambiguity, conceptual, 'Definition of universality: flat payment vs needs-based access').

omega_variable(
    administrative_cost_extraction,
    'Do means-testing administrative costs genuinely reduce program efficiency or do they constitute secondary extraction disguised as targeting?',
    'Cost-benefit analysis: total administrative spend vs targeted savings. Comparison with flat universal payment administrative costs. Audit of clawback mechanism overhead.',
    'If administrative costs exceed savings: means-testing is pure extraction mechanism. If savings exceed costs: means-testing is legitimate targeting. Confidence in Tangled Rope classification depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_cost_extraction, empirical, 'Whether means-testing administrative cost exceeds targeted savings').

omega_variable(
    work_disincentive_suppression,
    'Does the means-testing clawback (benefit reduction as income rises) constitute structural suppression of work effort or rational economic response to changing family resources?',
    'Behavioral analysis: earnings trajectories of families near clawback thresholds. Econometric estimation of labor supply elasticity with respect to benefit phase-out rate.',
    'If suppression mechanism: trapped/constrained agents experience involuntary earnings caps. If rational response: experienced extraction is lower. Determines experienced suppression value for low-income perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_disincentive_suppression, empirical, 'Whether clawback creates structural work disincentive').

omega_variable(
    political_universality_commitment,
    'Is the constraint''s structure determined by economic necessity or by political choice to preserve means-testing as a fairness principle?',
    'Policy history analysis. Comparison of fiscal cost of pure universality vs current means-tested system. Interviews with policy designers and political coalition builders.',
    'If economic necessity: universality is constrained by budgetary reality (Tangled Rope). If political choice: universality is an identity/ideological stance (Snare for those locked into fairness framing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_universality_commitment, conceptual, 'Whether means-testing structure reflects budgetary or political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(universal_child_benefit_universality, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ucb_tr_t0, universal_child_benefit_universality, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ucb_tr_t5, universal_child_benefit_universality, theater_ratio, 5, 0.5).
narrative_ontology:measurement(ucb_tr_t10, universal_child_benefit_universality, theater_ratio, 10, 0.58).
narrative_ontology:measurement(ucb_tr_t15, universal_child_benefit_universality, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(ucb_be_t0, universal_child_benefit_universality, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ucb_be_t5, universal_child_benefit_universality, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ucb_be_t10, universal_child_benefit_universality, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ucb_be_t15, universal_child_benefit_universality, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(universal_child_benefit_universality, resource_allocation).
narrative_ontology:affects_constraint(universal_child_benefit_universality, means_testing_stigma_spiral).
narrative_ontology:affects_constraint(universal_child_benefit_universality, wage_substitution_effect).

% DUAL FORMULATION NOTE:
% Universal child benefit universality is downstream of broader welfare state design tension (universalism vs targeting). It can be decomposed into: (1) nominal universality (policy promise: all families eligible) vs actual universality (implementation: means-tested delivery), and (2) coordination function (child welfare support) vs extraction function (benefit concentration). These are distinct constraints with different epsilon values. The current story captures the hybrid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(universal_child_benefit_universality, powerful, 0.15).
constraint_indexing:directionality_override(universal_child_benefit_universality, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
