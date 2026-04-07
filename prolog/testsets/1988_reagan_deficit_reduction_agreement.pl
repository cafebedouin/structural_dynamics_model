% ============================================================================
% CONSTRAINT STORY: 1988_reagan_deficit_reduction_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1988_reagan_deficit_reduction_agreement, []).

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
 *   constraint_id: 1988_reagan_deficit_reduction_agreement
 *   human_readable: 1988 Reagan Deficit Reduction Agreement: Budget Discipline Mechanism
 *   domain: economics/fiscal_policy/governance
 *
 * SUMMARY:
 *   The 1988 Reagan deficit reduction agreement establishes a governance
 *   mechanism requiring coordinated executive-legislative deficit reduction
 *   targeting $76 billion over two years. This constraint exhibits the
 *   characteristic structure of fiscal discipline mechanisms: it creates
 *   binding authority over spending while distributing costs asymmetrically
 *   across constituencies. The mechanism benefits future taxpayers and
 *   creditors through lower long-term interest rates and reduced
 *   intergenerational wealth transfer; costs fall on current spending
 *   constituencies through program cuts and reduced discretionary spending
 *   authority. The constraint functions as a coordination mechanism (how to
 *   allocate necessary reductions across competing priorities) simultaneously
 *   embedded within an extraction mechanism (certain groups bear
 *   disproportionate costs). The theater ratio (0.55) reflects that much of
 *   the announced deficit reduction relies on accounting adjustments,
 *   baseline assumption changes, and dynamic revenue forecasting rather than
 *   explicit spending eliminations, creating a structural mismatch between
 *   the constraint's rhetorical severity and its functional enforcement
 *   capacity.
 *
 * KEY AGENTS:
 *   - Current Spending Constituencies: Primary victims (powerless/trapped) — federal employees, contractors, social program beneficiaries face mandatory reductions with no exit option
 *   - Congressional Budget Committees: Primary coordinators (moderate/constrained) — manage allocative tradeoffs across competing priorities while facing political cost of cuts
 *   - Fiscal Conservative Coalition: Primary beneficiary (institutional/arbitrage) — benefits from establishment of deficit discipline norm and long-term fiscal credibility
 *   - Economic Growth Advocates: Organized actors (organized/mobile) — accept temporary constraint with sunset; see growth-driven revenue expansion as exit pathway
 *   - International Financial Markets: Secondary observer (institutional/arbitrage) — perceive agreement as theatrical fiscal signaling; maintain pricing based on credibility assessment
 *   - Future Taxpayers/Creditors: Diffuse beneficiary (powerless/analytical) — benefit from reduced long-term fiscal burden but have no agency in current constraint design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1988_reagan_deficit_reduction_agreement, 0.52).
domain_priors:suppression_score(1988_reagan_deficit_reduction_agreement, 0.58).
domain_priors:theater_ratio(1988_reagan_deficit_reduction_agreement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1988_reagan_deficit_reduction_agreement, extractiveness, 0.52).
narrative_ontology:constraint_metric(1988_reagan_deficit_reduction_agreement, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(1988_reagan_deficit_reduction_agreement, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(1988_reagan_deficit_reduction_agreement, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(1988_reagan_deficit_reduction_agreement, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1988_reagan_deficit_reduction_agreement, tangled_rope).
narrative_ontology:human_readable(1988_reagan_deficit_reduction_agreement, "1988 Reagan Deficit Reduction Agreement: Budget Discipline Mechanism").
narrative_ontology:topic_domain(1988_reagan_deficit_reduction_agreement, "economics/fiscal_policy/governance").

domain_priors:requires_active_enforcement(1988_reagan_deficit_reduction_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1988_reagan_deficit_reduction_agreement, future_taxpayers).
narrative_ontology:constraint_beneficiary(1988_reagan_deficit_reduction_agreement, long_term_creditors).
narrative_ontology:constraint_beneficiary(1988_reagan_deficit_reduction_agreement, fiscal_conservatives).
narrative_ontology:constraint_victim(1988_reagan_deficit_reduction_agreement, current_spending_constituencies).
narrative_ontology:constraint_victim(1988_reagan_deficit_reduction_agreement, federal_programs).
narrative_ontology:constraint_victim(1988_reagan_deficit_reduction_agreement, discretionary_social_spending).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENT SPENDING CONSTITUENCIES (SNARE) — Federal employees, contractors, and social program beneficiaries face mandatory budget cuts with no exit option. Suppression is high: restructuring or program elimination offers no alternatives at equivalent benefit levels. The constraint extracts from this group to service future fiscal discipline. Maximum experienced extraction for trapped agents with no mobility across spending sectors.
constraint_indexing:constraint_classification(1988_reagan_deficit_reduction_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL PROGRAM MANAGERS (TANGLED ROPE) — Coordinate allocative tradeoffs across portfolios (genuine coordination function) while experiencing extraction through reduced discretionary authority and political cost of cuts. Suppression is moderate: they can propose alternatives but within binding totals. Constrained exit reflects career risk and constituency pressure.
constraint_indexing:constraint_classification(1988_reagan_deficit_reduction_agreement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL CONSERVATIVE COALITION (ROPE) — Benefits from the constraint's establishment of fiscal discipline norm. Experiences the mechanism as coordination: binding deficit targets create bargaining leverage for anti-spending groups. Arbitrage options allow investment in political messaging around fiscal responsibility. Net beneficiary with low effective extraction.
constraint_indexing:constraint_classification(1988_reagan_deficit_reduction_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ECONOMIC GROWTH ADVOCATES (SCAFFOLD) — Accept deficit reduction as temporary constraint with sunset: dynamic economic growth will eventually reduce deficit ratios through revenue expansion. See the agreement as scaffolding for fiscal credibility during transition to growth. Exit pathway is growth-driven revenue increases that relax the constraint. High agency through mobile options.
constraint_indexing:constraint_classification(1988_reagan_deficit_reduction_agreement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL FINANCIAL MARKETS (PITON) — Perceive the agreement as primarily theatrical: U.S. deficit discipline is performatively signaled but underlying structural drivers (entitlements, military spending) remain untouched. Markets observe budget gimmicks (asset sales, accounting reclassifications) substituting for real cuts. Theater ratio is high because the mechanism's real enforcement capacity is weak. Piton classification reflects degraded function masked by institutional persistence.
constraint_indexing:constraint_classification(1988_reagan_deficit_reduction_agreement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, budget deficits cannot fund permanent prosperity; intertemporal constraints always bind eventually. The agreement appears to instantiate a natural law of fiscal sustainability. However, the structural data contradicts this: identifiable beneficiaries (future creditors, fiscal conservatives) exist, congressional discretion is real, and enforcement capacity is contingent. Engine false summit detector will flag this as naturalization of a political arrangement.
constraint_indexing:constraint_classification(1988_reagan_deficit_reduction_agreement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1988_reagan_deficit_reduction_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1988_reagan_deficit_reduction_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1988_reagan_deficit_reduction_agreement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1988_reagan_deficit_reduction_agreement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1988_reagan_deficit_reduction_agreement, TR),
    TR >= 0.70.

:- end_tests(1988_reagan_deficit_reduction_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The agreement embeds asymmetric costs: current constituencies bear immediate reduction burden while benefits accrue to future taxpayers and creditors. However, extraction is not maximal because some genuine coordination benefit exists (rational allocation of necessary reductions beats arbitrary cuts). Suppression (0.58): Moderate-high. Spending constituencies face high barriers to exit: they cannot refuse the constraint through program discontinuation or sector migration. Wage and benefit reductions are quasi-mandatory for affected federal workers. However, suppression is not total because some spending programs negotiate exemptions, and economic growth can provide offsetting employment. Theater ratio (0.55): Moderate. The constraint achieves ~55% of its stated reduction target through real spending cuts; the remaining 45% derives from baseline assumption adjustments, revenue side improvements from economic growth assumptions, and accounting reclassifications (asset sales, mandatory spending redefinition). This theater ratio is consistent with major fiscal agreements where political messaging requires announcing large deficit reductions while actual spending authority reduction is more modest.
 *
 * PERSPECTIVAL GAP:
 *   The gap between institutional beneficiary perspectives (Rope at ~0.35 effective extraction) and powerless constituency perspectives (Snare at ~0.75 effective extraction) is approximately 2:1, indicating significant distributional asymmetry. The gap between scaffold (organized agents, mobile, ~0.30 effective extraction) and snare (powerless, trapped, ~0.75) is nearly 3:1, showing that exit options and power differentiate the same structural constraint into opposite perceived classes. The piton perspective observes that theater ratio growth (0.42 → 0.61) outpaces extractiveness growth (0.35 → 0.58), indicating degrading functionality: the constraint persists through institutional commitment and fiscal hawkishness messaging even as its real enforcement capacity declines relative to its stated targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (future taxpayers, fiscal conservatives, creditors) have low directionality values derived from their structural position as net gainers from the constraint: lower future interest costs, policy influence through fiscal discipline norm, enhanced credibility in international markets. These agents have arbitrage exit options (they can invest in political influence for different fiscal policies, shift investment allocation based on credibility perceptions) and institutional power, producing low f(d) → low effective extraction. Victims (current spending constituencies) have high directionality values derived from trapped position with mandatory cost-bearing and no exit alternative. Their d approaches 1.0, producing f(d) >> 0.65, high experienced extraction χ. Congressional managers occupy the middle: they benefit from the coordination solution (ability to make rational allocative tradeoffs) while bearing political cost, producing d ≈ 0.55, moderate χ. The analytical observer's perspective uses canonical d for analytical context (0.73), producing the mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is partially resolved through the tangled rope classification, which captures both genuine coordination function (allocating necessary reductions rationally across competing priorities) and asymmetric extraction (current constituencies bear disproportionate cost). The coordination function is real: without the agreement, deficit reduction would proceed either chaotically (random program termination) or not at all (fiscal drift). The extraction is real: benefits accrue to creditors and future taxpayers while costs fall on current beneficiaries with no exit option. The unresolved mandatrophy question is whether the coordination benefit justifies the extractive burden — this is a value question that the framework classifies as preference-dependent. From a powerless constituency perspective, even if the coordination is optimal, they perceive the constraint as pure extraction (Snare) because they cannot participate in the optimization. From a fiscal conservative perspective, the coordination is the entire point — the constraint is beneficial discipline (Rope). The scaffold perspective resolves the mandatrophy dynamically: the coordination is temporary (two-year window), benefiting from fiscal credibility during the transition to growth-driven surplus. The piton perspective suggests the mandatrophy is pseudo-resolved through theater: announcing coordination and discipline (which the constraint delivers) masks the extractive burden (which falls on constituencies unable to resist).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_credibility,
    'Is the $76 billion reduction target credibly enforceable or achievable through accounting reclassification and off-budget mechanisms?',
    'Post-agreement audit of actual budget reductions vs. accounting adjustments; comparison of targeted vs. realized deficit trajectory; investigation of asset sales, lease-back arrangements, and mandatory spending workarounds',
    'If enforcement is weak (high accounting gaming): effective extractiveness drops to 0.25, constraint reclassifies as Piton with theater >> function. If enforcement is strong: extractiveness remains ~0.52, Tangled Rope classification holds. Theater ratio rises with accounting gaming.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_credibility, empirical, 'Credibility and enforceability of $76B reduction target').

omega_variable(
    revenue_vs_spending_composition,
    'Does the deficit reduction mechanism rely on spending cuts, revenue increases, or accounting adjustments, and what is the causal composition?',
    'Detailed budget reconciliation breaking down the $76B reduction into: (1) discretionary spending cuts, (2) entitlement program reductions, (3) revenue measures, (4) accounting reclassifications, (5) economic baseline assumption changes',
    'If primarily entitlement cuts: victims are powerless social program beneficiaries, suppression >> 0.60, Snare classification strengthens. If primarily revenue increases: extraction flows toward taxpayers, beneficiary reversal. If primarily accounting: theater ratio rises, Piton emerges as primary classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_vs_spending_composition, empirical, 'Composition of deficit reduction: spending cuts vs. revenue vs. accounting').

omega_variable(
    counterfactual_baseline_assumption,
    'What baseline budget trajectory is embedded in the $76B reduction target? Does the target represent cuts from current spending levels or slower growth relative to projected spending?',
    'Historical reconstruction of OMB baseline projections; analysis of CBO vs. administration estimates; comparison of pre-agreement spending trajectories to post-agreement adjusted baselines',
    'If baseline is current spending: real cuts are immediate and severe, suppression >> 0.60. If baseline is projected growth: nominal spending may still increase while ''deficit reduction'' occurs through slower growth, suppression << 0.40, constraint reclassifies to Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_baseline_assumption, empirical, 'Baseline assumptions embedded in deficit reduction target').

omega_variable(
    political_feasibility_sunset,
    'Is the deficit reduction agreement politically sustainable over the two-year enforcement window, or does it contain a de facto sunset when electoral cycles reset budget negotiations?',
    'Legislative history of similar deficit reduction agreements; tracking of congressional enforcement actions and waivers; analysis of whether subsequent budgets maintain or abandon the reduction discipline',
    'If politically unsustainable (high likelihood of waiver/reset): constraint is better modeled as Scaffold with sunset at next budget cycle, theater ratio rises, extractiveness for current constituencies becomes time-limited. If sustainable: Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_sunset, conceptual, 'Political sustainability and effective sunset of deficit reduction discipline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1988_reagan_deficit_reduction_agreement, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reagan_tr_t0, 1988_reagan_deficit_reduction_agreement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(reagan_tr_t6, 1988_reagan_deficit_reduction_agreement, theater_ratio, 6, 0.48).
narrative_ontology:measurement(reagan_tr_t12, 1988_reagan_deficit_reduction_agreement, theater_ratio, 12, 0.55).
narrative_ontology:measurement(reagan_tr_t18, 1988_reagan_deficit_reduction_agreement, theater_ratio, 18, 0.61).

% Extraction over time
narrative_ontology:measurement(reagan_be_t0, 1988_reagan_deficit_reduction_agreement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(reagan_be_t6, 1988_reagan_deficit_reduction_agreement, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(reagan_be_t12, 1988_reagan_deficit_reduction_agreement, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(reagan_be_t18, 1988_reagan_deficit_reduction_agreement, base_extractiveness, 18, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1988_reagan_deficit_reduction_agreement, resource_allocation).
narrative_ontology:affects_constraint(1988_reagan_deficit_reduction_agreement, federal_entitlement_program_constraints).
narrative_ontology:affects_constraint(1988_reagan_deficit_reduction_agreement, military_spending_floor_constraints).
narrative_ontology:affects_constraint(1988_reagan_deficit_reduction_agreement, interest_rate_credibility_spiral).

% DUAL FORMULATION NOTE:
% The deficit reduction agreement coordinates two structurally distinct constraints: (1) aggregate deficit target ($76B reduction) functions as Tangled Rope, coordinating allocative tradeoffs while extracting from current constituencies; (2) distributional mechanism (which programs bear the cuts) functions separately, often classified as Snare or Mountain depending on whether the distribution reflects political power or immutable spending baselines. Each story has its own ε value reflecting the empirical status of the specific constraint. Network links capture structural influence: success or failure of the deficit reduction target affects credibility with financial markets, influencing interest rate expectations and long-term fiscal sustainability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1988_reagan_deficit_reduction_agreement, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
