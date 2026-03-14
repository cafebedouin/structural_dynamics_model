% ============================================================================
% CONSTRAINT STORY: inflation_expectation_anchoring
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inflation_expectation_anchoring, []).

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
 *   constraint_id: inflation_expectation_anchoring
 *   human_readable: Inflation Expectation Anchoring
 *   domain: monetary_policy/macroeconomics
 *
 * SUMMARY:
 *   Inflation expectation anchoring is the central bank's primary policy tool
 *   for maintaining price stability: by credibly committing to keep inflation
 *   within a target range and communicating this commitment through forward
 *   guidance, interest-rate settings, and institutional independence, central
 *   banks suppress agents' inflationary expectations. When expectations are
 *   anchored, actual inflation can be lower than it would be if agents
 *   expected high inflation and adjusted prices upward preemptively
 *   (self-fulfilling prophecy). This constraint exhibits tangled-rope
 *   structure: it solves a real coordination problem (preventing runaway
 *   price-wage spirals) while simultaneously distributing gains and losses
 *   asymmetrically across power classes. Creditors and central banks benefit
 *   from anchored expectations protecting real returns; wage earners and
 *   fixed-income recipients bear costs as their nominal income growth is
 *   suppressed below inflation drift. The constraint's extractiveness has
 *   risen over the measurement interval as union decline and wage-bargaining
 *   erosion have reduced workers' capacity to adjust nominal wages upward to
 *   compensate for inflation surprise. The theater ratio has also risen as
 *   central bank communication has become increasingly performative:
 *   'flexible average inflation targeting,' 'symmetrical response function'
 *   narratives, and 'realistic growth rate assessments' all function to
 *   justify outcomes that diverge from the stated target while maintaining
 *   credibility claims.
 *
 * KEY AGENTS:
 *   - Central Bank: Primary beneficiary (institutional/arbitrage) — achieves price stability mandate and reduced policy costs through credibility
 *   - Creditor Class: Primary beneficiary (powerful/arbitrage) — real returns on debt instruments protected by suppressed inflation expectations
 *   - Wage Earners: Primary victim (powerless/trapped) — nominal wage growth suppressed below inflation, purchasing power erodes during expectation-reality gaps
 *   - Fixed-Income Recipients: Primary victim (powerless/trapped) — pensions and fixed-rate returns systematically eroded by positive inflation drift
 *   - Small Business Owners: Secondary actor (moderate/constrained) — mixed position: benefit from reduced price volatility, bear extraction through wage-pressure constraints
 *   - Wage-Bargaining Institutions (Labor Unions): Tertiary actor (organized/constrained) — institutionally eroded, cannot enforce inflation-compensating adjustments as capacity to negotiate has declined
 *   - Analytical Observer: Observes systemic structure (analytical/analytical) — risks naturalizing creditor-protective inflation targeting as necessary economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inflation_expectation_anchoring, 0.52).
domain_priors:suppression_score(inflation_expectation_anchoring, 0.48).
domain_priors:theater_ratio(inflation_expectation_anchoring, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inflation_expectation_anchoring, extractiveness, 0.52).
narrative_ontology:constraint_metric(inflation_expectation_anchoring, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(inflation_expectation_anchoring, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inflation_expectation_anchoring, tangled_rope).
narrative_ontology:human_readable(inflation_expectation_anchoring, "Inflation Expectation Anchoring").
narrative_ontology:topic_domain(inflation_expectation_anchoring, "monetary_policy/macroeconomics").

domain_priors:requires_active_enforcement(inflation_expectation_anchoring).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inflation_expectation_anchoring, central_bank).
narrative_ontology:constraint_beneficiary(inflation_expectation_anchoring, creditor_interests).
narrative_ontology:constraint_victim(inflation_expectation_anchoring, wage_earners).
narrative_ontology:constraint_victim(inflation_expectation_anchoring, fixed_income_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Structurally trapped by labor market power asymmetry and credit dependency. Cannot exit inflation-exposed wage contracts; bears extraction through purchasing power loss. Suppression is high: limited job mobility, skill-specific barriers, relocation costs. The wage earner experiences effective extraction as the central bank's credibility framework locks future wage growth below inflation drift.
constraint_indexing:constraint_classification(inflation_expectation_anchoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIXED-INCOME RECIPIENT (SNARE) — Pensioners and savers on fixed returns face maximum extraction through inflation drift. No exit capacity: pensions cannot be renegotiated, fixed-rate instruments mature without recourse. Suppression is total: legal barriers prevent salary indexation in many jurisdictions; market barriers eliminate inflation-hedging instruments at purchasing-power-preserving yields. Pure extraction mechanism.
constraint_indexing:constraint_classification(inflation_expectation_anchoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANK (ROPE) — Experiences inflation expectation anchoring as coordination mechanism: communicating credible low-inflation commitment enables price stability and reduces the real cost of monetary policy. The central bank benefits from the expectation-coordination function (reduced volatility, lower policy rate needed). Exit options: can conduct monetary operations globally, can revise policy framework at will. The constraint enables rather than extracts from this actor.
constraint_indexing:constraint_classification(inflation_expectation_anchoring, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDITOR CLASS (ROPE) — Benefits from anchored low-inflation expectations: real returns on debt instruments are protected, and inflation surprise risk is suppressed. The constraint coordinates creditor interests and aggregate price stability simultaneously. Exit options: can hold inflation-hedged assets, can reallocate capital globally, can lobby for central bank independence to strengthen commitment devices. Net beneficiary experiencing the constraint as beneficial coordination.
constraint_indexing:constraint_classification(inflation_expectation_anchoring, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SMALL BUSINESS OWNER (TANGLED ROPE) — Mixed position: benefits from anchored inflation reducing uncertainty for pricing and investment decisions; bears extraction through wage pressure (must offer inflation-compensating raises to retain labor, but inflation expectations are anchored below actual inflation). Suppression is moderate: can relocate operations, can shift to capital-intensive production, but faces switching costs and reduced access to credit during high-rate periods. Real extraction exists but coexists with genuine coordination benefit.
constraint_indexing:constraint_classification(inflation_expectation_anchoring, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INFLATION-INDEXED WAGE REGIME (PITON) — Historical wage-bargaining institutions that once coordinated labor-capital relations through explicit inflation indexation (COLA clauses, wage-setting floors tied to price indices) have largely atrophied. The scaffolding remains in some jurisdictions but is performative: indexed clauses exist but trigger only in high-inflation states, and their enforcement is politically contested. Theater ratio is high: the indexation mechanism persists through institutional inertia, but its real function (protecting workers from inflation surprise) has degraded as inflation targeting succeeded in suppressing expectations. The piton represents a former coordination mechanism now maintained theatrically.
constraint_indexing:constraint_classification(inflation_expectation_anchoring, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global view, inflation expectation anchoring exhibits both coordination and extraction: it solves the real coordination problem of price stability and reduces monetary policy's real costs (Rope function) while simultaneously protecting creditor interests and suppressing wage-growth expectations below productivity growth (Snare function). The constraint is not decomposable into pure coordination or pure extraction — it is genuinely hybrid. Active enforcement via central bank credibility and communication. Beneficiaries are clear (creditors, central bank); victims are clear (wage earners, fixed-income recipients). Asymmetric distribution of gains and losses across power classes.
constraint_indexing:constraint_classification(inflation_expectation_anchoring, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inflation_expectation_anchoring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inflation_expectation_anchoring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inflation_expectation_anchoring, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inflation_expectation_anchoring, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inflation_expectation_anchoring, TR),
    TR >= 0.70.

:- end_tests(inflation_expectation_anchoring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from wage earners and fixed-income recipients through suppressed nominal growth expectations. The extraction is not maximal (would be 0.70+) because inflation-targeting frameworks do produce genuine price stability, which is a real public good. Some of the value captured by creditors reflects legitimate coordination gains, not pure extraction. However, empirical wage growth has trended below productivity growth, and this divergence has accelerated since inflation targeting's adoption, suggesting extractive mechanisms are real and growing. Suppression (0.48): Moderate. Wage earners face high but not insurmountable barriers to exiting inflation-exposure: labor mobility exists (changing jobs, sectors), but carries costs (skill loss, relocation). The erosion of union power and wage-bargaining institutions has increased suppression over the interval (reducing workers' capacity to adjust contracts mid-stream). Fixed-income recipients face higher suppression (cannot renegotiate pensions, bonds mature without recourse). Theater ratio (0.61): Moderate-high. Central bank communication about inflation targeting has become increasingly performative. Forward guidance that claims symmetrical response functions contradicts observed asymmetry (central banks respond more aggressively to inflation above target than to employment below target). 'Flexible' average inflation targeting narratives justify overshooting the target while claiming commitment. The indexation of wages to inflation in labor contracts has declined (piton effect), but central bank communication about inflation expectations persists in sophisticated technical language that naturalizes the constraint as economic law rather than institutional choice. Extractiveness has drifted upward from 0.18 (1980s, when wage bargaining was stronger) to 0.52 (2020s, with union decline). Theater ratio has risen from 0.25 to 0.61 as the communication apparatus has become more elaborate and less transparently aligned with actual policy outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The wage earner and central bank perspectives reveal the full perspectival gap. The wage earner sees pure extraction (snare): inflation expectations are locked low by central bank communication, but actual inflation drifts upward, creating systematic purchasing-power loss. The wage earner cannot exit the labor contract or renegotiate during inflation surprises. The central bank sees coordination (rope): inflation expectation anchoring solves the runaway-inflation problem and reduces the policy rate needed to achieve price stability. Both actors are analyzing the same constraint structure, but their structural positions produce opposite classifications. The creditor sees coordination (rope): anchored expectations protect real returns. The small business owner sees mixed coordination and extraction (tangled rope): price-stability coordination helps planning, but wage pressure (workers demanding inflation compensation) constrains margins. The piton perspective (degraded wage indexation) reveals that historical wage-adjustment institutions have atrophied — workers once had contractual mechanisms to compensate for inflation surprise, but these have largely disappeared, leaving them trapped rather than constrained. The analytical observer sees tangled rope with active enforcement: creditor protection and wage suppression are both real, and both are actively maintained through central bank credibility signals.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position relative to the extraction flow. The wage earner (powerless/trapped) derives d ≈ 0.95, experiencing maximum extraction because they have no exit options and bear full cost of inflation drift. The fixed-income recipient (powerless/trapped) also derives d ≈ 0.95. The central bank (institutional/arbitrage) derives d ≈ 0.05–0.10, experiencing negative effective extraction because they are the beneficiary of the coordination mechanism — anchoring enables low policy rates and achieves their mandate. The creditor (powerful/arbitrage) derives d ≈ 0.10, also experiencing minimal extraction. The small business owner (moderate/constrained) derives d ≈ 0.50, experiencing moderate extraction because they benefit from reduced volatility but bear extraction through wage pressure and working-capital costs during rate-hike cycles. The wage-bargaining institution (organized/constrained) derives d ≈ 0.65, experiencing above-moderate extraction because their primary function (wage negotiation to preserve real income) has been eroded by the constraint. The analytical observer (analytical/analytical) derives d ≈ 0.72 per canonical fallback, identifying structural extraction while maintaining observational distance.
 *
 * MANDATROPHY ANALYSIS:
 *   Inflation expectation anchoring resolves the mandatrophy by demonstrating that tangled rope is the correct classification despite appearances of pure coordination. A common error would be to classify this as pure rope (coordination mechanism) because it genuinely solves the inflation-instability problem, or as pure snare (extraction mechanism) because it protects creditor interests while suppressing wages. The tangled-rope classification holds because BOTH properties are true simultaneously: the constraint actively enforces a coordination function (reducing inflation volatility requires central bank credibility, which requires belief in the commitment), AND it asymmetrically extracts from powerless agents (wage earners) to benefit powerful agents (creditors). The active enforcement requirement is met: central bank communication, credibility signaling, institutional independence frameworks, and communication platforms all actively maintain the constraint. Beneficiaries (central bank, creditors) are identifiable and real. Victims (wage earners, fixed-income recipients) are identifiable and real. The constraint cannot be dissolved into pure coordination or pure extraction — both dimensions are structural. The mandatrophy is resolved by recognizing that hybrid constraints are not incoherent; they are real institutional formations where coordination and extraction are inextricably linked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_reality_drift,
    'Are inflation expectations actually anchored, or have they shifted upward undetectably by central bank communication that naturalizes higher inflation as ''target-consistent''?',
    'Longitudinal survey of inflation expectations vs actual inflation; comparison of expectation-formation methods (surveys, breakeven inflation, wage-setting behavior); analysis of central bank communication shifts in ''flexible average inflation targeting'' narratives',
    'If expectations have drifted upward while central banks claim anchoring: the tangled rope classification holds but with higher ε (extraction mechanism strengthened by false credibility claims). If expectations are genuinely stable: tangled rope classification is correct as stated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_reality_drift, empirical, 'Whether inflation expectations are actually anchored or have shifted undetectably').

omega_variable(
    wage_bargaining_capacity_erosion,
    'Has the erosion of wage-bargaining institutions (labor union decline, gig economy expansion, outsourcing) made wage earners unable to negotiate inflation adjustments, transforming them from constrained to trapped?',
    'Historical analysis of wage-setting mechanisms and union density; correlation between institutional erosion and nominal wage growth lag; comparison of wage dynamics in high-union-density vs low-union-density sectors',
    'If erosion is substantial: trapped classification becomes more accurate for wage earners, and the snare perspective''s d-value should be higher (stronger extraction signal). If institutional change is modest: powerless/constrained remains accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_bargaining_capacity_erosion, empirical, 'Whether wage earners have moved from constrained to trapped exit options').

omega_variable(
    creditor_extraction_vs_systemic_stability,
    'Does protecting creditor purchasing power through anchored inflation expectations serve genuine macroeconomic stability or does it primarily extract from debtors to benefit creditors?',
    'Historical comparison of monetary regime outcomes (stagflation era vs inflation targeting era): growth, employment, distributional metrics; counterfactual analysis of alternative anchor mechanisms (e.g., nominal GDP targets, employment mandates)',
    'If stability is genuine: tangled rope classification holds with coordination function as primary justification. If creditor protection dominates: snare classification becomes more accurate, and beneficiary/victim asymmetry is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_extraction_vs_systemic_stability, preference, 'Whether anchoring serves macroeconomic stability or creditor extraction').

omega_variable(
    identity_locked_inflation_naturalization,
    'Have wage earners and fixed-income recipients internalized the inflation-targeting framework as natural necessity rather than contingent institutional choice, preventing recognition of alternative monetary regimes?',
    'Survey analysis of inflation expectation reasoning; comparison of public discourse about inflation targeting as ''the only responsible policy'' vs awareness of alternative frameworks; historical analysis of monetary regime shifts and public acceptance',
    'If identity-locked mechanism is substantial: exit options for wage earners may be misclassified as trapped when they could be constrained; the suppression mechanism is partially internalized rather than purely structural. Analytical perspective may also be identity-locked (demonstrating oracle gap).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_inflation_naturalization, conceptual, 'Whether inflation targeting is internalized as natural necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inflation_expectation_anchoring, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iea_tr_t0, inflation_expectation_anchoring, theater_ratio, 0, 0.25).
narrative_ontology:measurement(iea_tr_t10, inflation_expectation_anchoring, theater_ratio, 10, 0.48).
narrative_ontology:measurement(iea_tr_t20, inflation_expectation_anchoring, theater_ratio, 20, 0.61).
narrative_ontology:measurement(iea_tr_t5, inflation_expectation_anchoring, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(iea_be_t0, inflation_expectation_anchoring, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(iea_be_t10, inflation_expectation_anchoring, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(iea_be_t20, inflation_expectation_anchoring, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(iea_be_t5, inflation_expectation_anchoring, base_extractiveness, 5, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inflation_expectation_anchoring, resource_allocation).
narrative_ontology:affects_constraint(inflation_expectation_anchoring, wage_labor_power_asymmetry).
narrative_ontology:affects_constraint(inflation_expectation_anchoring, real_debt_burden_distribution).
narrative_ontology:affects_constraint(inflation_expectation_anchoring, union_decline_and_bargaining_power).

% DUAL FORMULATION NOTE:
% Inflation expectation anchoring is downstream of monetary policy regime choice (inflation targeting framework) but represents a distinct structural constraint on wage-setting, savings behavior, and credit allocation. Upstream: the theoretical case for inflation targeting as a policy regime (lower inflation, greater price transparency). Downstream: the asymmetric distribution of gains and losses across power classes under inflation-targeting regimes, particularly the erosion of wage-earner exit options through institutional decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inflation_expectation_anchoring, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
