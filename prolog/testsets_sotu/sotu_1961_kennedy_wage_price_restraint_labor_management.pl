% ============================================================================
% CONSTRAINT STORY: sotu_1961_kennedy_wage_price_restraint_labor_management
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1961_kennedy_wage_price_restraint_labor_management, []).

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
 *   constraint_id: sotu_1961_kennedy_wage_price_restraint_labor_management
 *   human_readable: Kennedy's Voluntary Wage-Price Restraint Policy (1961)
 *   domain: economics/incomes_policy
 *
 * SUMMARY:
 *   President Kennedy's voluntary wage-price restraint policy (1961-1965)
 *   represents a critical incomes policy experiment in post-war economics: an
 *   attempt to decouple employment growth from inflationary pressure through
 *   coordinated restraint monitored by the President's Advisory Committee on
 *   Labor-Management Policy. The policy rests on a coordination premise: if
 *   wage-setters and firms collectively restrain increases, inflation
 *   expectations stabilize, enabling recovery without wage-price spiral. The
 *   policy benefits consumers (stable prices), export sectors
 *   (competitiveness), and macroeconomic recovery. Costs fall on organized
 *   labor (suppressed nominal wage growth, eroded real wages if inflation
 *   persists) and on firms (pricing constraints that reduce profit margins).
 *   Enforcement is entirely voluntary — no legal mandate, only presidential
 *   social pressure, union leadership alignment, and public commitment. The
 *   constraint exhibits all six DR types depending on perspective, revealing
 *   fundamental tensions between coordination and distribution in macro
 *   policy.
 *
 * KEY AGENTS:
 *   - Rank-and-File Workers: Primary victim (powerless/trapped) — absorbed wage suppression without effective exit option; real wages eroded as inflation proved harder to control than anticipated
 *   - Organized Labor Leadership: Constrained beneficiary (organized/constrained) — gained policy influence and legitimacy from participation but imposed costs on membership; captured by institutional need to appear 'responsible'
 *   - Large Manufacturing Firms: Constrained beneficiary (powerful/constrained) — gained price stability and investment climate confidence but faced pricing constraints; retained discretion over cost-cutting and employment adjustment
 *   - Export Sector & Consumers: Primary beneficiary (institutional/arbitrage) — gained from reduced inflation and improved competitiveness; experienced constraint as pure coordination
 *   - Presidential Advisory Committee: Temporary coordinator (institutional/arbitrage) — tasked with managing recovery transition; saw policy as crisis solution with implicit sunset
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing distributional conflict (wage-price-employment trilemma) as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1961_kennedy_wage_price_restraint_labor_management, 0.52).
domain_priors:suppression_score(sotu_1961_kennedy_wage_price_restraint_labor_management, 0.48).
domain_priors:theater_ratio(sotu_1961_kennedy_wage_price_restraint_labor_management, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1961_kennedy_wage_price_restraint_labor_management, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1961_kennedy_wage_price_restraint_labor_management, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1961_kennedy_wage_price_restraint_labor_management, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1961_kennedy_wage_price_restraint_labor_management, tangled_rope).
narrative_ontology:human_readable(sotu_1961_kennedy_wage_price_restraint_labor_management, "Kennedy's Voluntary Wage-Price Restraint Policy (1961)").
narrative_ontology:topic_domain(sotu_1961_kennedy_wage_price_restraint_labor_management, "economics/incomes_policy").

domain_priors:requires_active_enforcement(sotu_1961_kennedy_wage_price_restraint_labor_management).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1961_kennedy_wage_price_restraint_labor_management, consumer_base).
narrative_ontology:constraint_beneficiary(sotu_1961_kennedy_wage_price_restraint_labor_management, export_competitive_firms).
narrative_ontology:constraint_beneficiary(sotu_1961_kennedy_wage_price_restraint_labor_management, macroeconomic_recovery_objective).
narrative_ontology:constraint_victim(sotu_1961_kennedy_wage_price_restraint_labor_management, organized_labor).
narrative_ontology:constraint_victim(sotu_1961_kennedy_wage_price_restraint_labor_management, domestic_firms_constrained_pricing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RANK-AND-FILE WORKER (SNARE) — Trapped by labor supply dependency and union leadership alignment with administration policy. Workers cannot exit wage restraint without breaking union solidarity, cannot exit labor market without catastrophic household income loss. Presidential social pressure + union cooperation create suppression with minimal coordination benefit to individual worker. Experiences maximum extraction: wage growth suppressed while prices rise (inflation erodes real wages); costs borne individually while benefits accrue to exporters and consumers.
constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNION LEADERSHIP (TANGLED ROPE) — Constrained by political capital with Kennedy administration and need to maintain collective bargaining legitimacy. Leadership benefits from policy coordination (seat at the table, presidential recognition, influence over macro policy) while imposing costs on membership. Genuine coordination function exists (preventing wage-price spiral that would harm all workers) alongside asymmetric extraction (leadership's political benefits exceed membership's wage gains). Active enforcement through presidential pressure and public pledges. High suppression of alternative wage strategies.
constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE MANUFACTURING FIRMS (TANGLED ROPE) — Constrained by presidential pressure and public commitment to restraint, but benefits from price stability that protects market position and capital investment climate. Genuine coordination function: firms need predictable input costs and labor peace for production planning. Asymmetric extraction: firms retain pricing discretion (can pass through input costs, can cut costs via employment contraction) while labor absorbs nominal wage ceiling. Enforcement through public pledges and reputational cost of breaking consensus. Suppression of alternative pricing strategies through presidential advisory committee oversight and media monitoring.
constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXPORT SECTOR & CONSUMER CONSTITUENCY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: wage-price restraint stabilizes inflation expectations, protects export competitiveness, and maintains purchasing power for consumers. No suppression experienced; exit is not relevant because this perspective benefits. The policy directly solves the collective action problem of coordinating expectations. Benefits include improved trade balance, consumer purchasing power, capital investment climate.
constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PRESIDENTIAL ADVISORY COMMITTEE & ADMINISTRATION (SCAFFOLD) — Sees the policy as a temporary coordination mechanism with implicit sunset. The committee's function is to manage the recovery transition and restore price stability; once inflation is controlled and full employment approaches, the need for restraint diminishes. Suppression is tolerated (voluntary compliance, not legal mandate) because the time horizon is understood as temporary. As conditions improve, the policy loses rationale and gives way to normal market wage-setting. The committee experiences this as solving a crisis problem with dignity and without coercion.
constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL LEGITIMACY THEATER (PITON) — At the civilizational level, the policy's core function (coordinating wage-price expectations) persists independent of whether it actually works. The Presidential Advisory Committee's real operational function is maintaining the appearance of coordination and preventing wage-price spiral narratives from dominating public discourse. Theater ratio (0.65) reflects substantial performative content: public pledges matter more than enforcement mechanisms; meetings and statements reinforce commitment more than monitoring produces actual behavior change. The constraint persists through institutional inertia even if effectiveness declines.
constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the wage-price-unemployment trilemma appears as an immutable constraint of market economies: you cannot simultaneously achieve low inflation, high employment, and unrestrained wage growth. The policy response to this immutable tradeoff is necessarily extraction from one group (workers' nominal wages) to benefit another (consumers, exporters). This perspective risks naturalizing what is actually a contingent institutional choice (who bears the cost of adjustment) as an unavoidable law of economics. The engine's false summit detector will identify this as naturalization of a distributional conflict.
constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1961_kennedy_wage_price_restraint_labor_management_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1961_kennedy_wage_price_restraint_labor_management, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1961_kennedy_wage_price_restraint_labor_management, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1961_kennedy_wage_price_restraint_labor_management, TR),
    TR >= 0.70.

:- end_tests(sotu_1961_kennedy_wage_price_restraint_labor_management_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting that workers' real wages are suppressed while benefits accrue primarily to exporters and consumers. The extraction is not maximal (0.46+) because the policy genuinely solves a coordination problem (preventing wage-price spiral) that would harm all workers if it occurred; the extraction is partially justified by the coordination function. However, extraction exceeds coordination benefit because firms retain pricing discretion and can pass through costs via employment contraction, asymmetrically shifting adjustment burden to labor. Suppression (0.48): Moderate-high. Workers cannot effectively exit wage restraint through individual action (labor supply dependent on union) or collective action (union leadership aligned with administration policy); firms cannot exit through price increases without reputational and political cost; both face suppression of alternative strategies through committee oversight and media monitoring. Theater ratio (0.65): Moderate-high, reflecting that the policy's real enforcement mechanism is presidential social pressure and institutional legitimacy rather than sanctions or legal mandate. Public pledges, committee meetings, and administration statements reinforce restraint commitment more than monitoring produces behavior change. As policy persists (years 6-8), theater ratio increases relative to enforcement effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   Rank-and-file worker (powerless/trapped) sees Snare: suppressed wages, constrained by union leadership alignment, no exit option. Union leadership (organized/constrained) sees Tangled Rope: policy influence and legitimacy benefit balanced against membership costs and suppression of alternative wage strategies. Firms (powerful/constrained) see Tangled Rope: price stability and investment climate benefits balanced against pricing constraints and discretion to cut employment. Exporters and consumers (institutional/arbitrage) see Rope: pure coordination benefit with no suppression or sacrifice. Committee (institutional/arbitrage) sees Scaffold: temporary crisis solution with understood sunset. Analytical observer at civilizational scope risks false summit (mountain): the wage-price-employment trilemma appears natural and immutable, but structural data (beneficiaries are identifiable, policy is institutional choice) reveals it as naturalization of distributional conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive directionality computation. Beneficiaries are consumer base, export sector, and macroeconomic recovery (low d, benefit from constraint). Victims are organized labor and firms constrained in pricing (high d, bear costs). Union leadership occupies ambiguous position: organized agents (canonical d ≈ 0.40) who are constrained (raises d toward 0.50) but experience benefits from policy participation (lowers d toward 0.35). Firms are powerful (canonical d ≈ 0.48) but constrained by public pledges (raises d toward 0.55); however, firms can exit constraints via employment contraction rather than price restraint (arbitrage-like discretion, lowers d toward 0.42). Rank-and-file workers are trapped (canonical d = 1.0, maximum extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying the relationship between coordination and distribution. The policy is genuinely coordinated (prevents wage-price spiral that would harm all workers) — this is not mislabeled extraction. But the coordination is ASYMMETRIC in incidence: workers bear the cost (nominal wage suppression), exporters/consumers bear benefit (price stability), and union leadership negotiates the distribution without full worker consent. This is precisely what Tangled Rope captures: genuine coordination function + asymmetric extraction + active enforcement (presidential pressure). The mandatrophy is resolved by recognizing that coordination and extraction can coexist and that the policy is appropriately classified as Tangled Rope, not Rope (which would imply symmetric benefits) or Snare (which would imply no coordination function). The false summit risk (mountain) is prevented by the explicit beneficiary/victim declarations showing the policy is institutional choice, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_compliance_binding_power,
    'Does voluntary compliance backed by presidential social pressure constitute genuine constraint, or is it theater masking selective enforcement?',
    'Comparative analysis of wage and price behavior between firms/unions with direct administration ties vs. those without; tracking of firms that broke pledges and consequences faced (or lack thereof)',
    'If compliance is genuinely enforced through reputational cost: constraint is Tangled Rope with real suppression. If enforcement is selective or performative: constraint is Piton with high theater ratio. Classification shifts based on whether ''voluntary'' is structural binding or institutional theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_compliance_binding_power, empirical, 'Whether voluntary compliance is structurally binding or performative').

omega_variable(
    inflation_causation_attribution,
    'To what degree does the wage-price restraint policy actually cause the moderation in inflation observed during 1961-1965, versus how much is attributable to slack in the labor market, productivity gains, and global supply conditions?',
    'Time-series econometric decomposition: counterfactual wage and price paths under alternative policy scenarios; cross-country comparison with nations lacking coordinated wage-price policies during the same period',
    'If restraint policy causes substantial inflation moderation: extraction of worker consent for genuine macroeconomic benefit (Tangled Rope justified). If inflation moderation is driven by labor market slack and productivity: worker restraint extracts real wages with minimal coordination benefit (Snare). High impact on whether policy is disguised redistribution or actual coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_causation_attribution, empirical, 'Causal impact of wage-price restraint on inflation outcomes').

omega_variable(
    union_leadership_capture,
    'Are union leaders'' compliance with restraint policy driven by genuine belief in macroeconomic necessity, by political accommodation of the Kennedy administration, or by institutional self-interest in maintaining collective bargaining legitimacy (which requires demonstrating ''responsibility'')?',
    'Analysis of union leadership communications (private vs. public statements); interviews or memoirs documenting decision rationales; comparison of restraint compliance across unions with different political alignments',
    'If genuine macroeconomic belief: union leadership correctly internalizes constraint (Tangled Rope from leadership perspective is accurate). If political accommodation or institutional self-interest: leadership is captured and imposing costs on membership without full justification (Snare from membership perspective becomes more severe; leadership becomes beneficiary rather than constrained agent). Reveals whether asymmetric extraction is justified or predatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_leadership_capture, conceptual, 'Union leadership''s motivations for policy compliance').

omega_variable(
    sunset_mechanism_presence,
    'Does the policy contain explicit or implicit conditions for its termination? When should restraint cease?',
    'Review of policy statements, committee charter, and Kennedy administration documents for exit criteria; analysis of whether restraint persisted beyond the recovery period and how it was eventually abandoned',
    'If clear sunset exists (restraint until unemployment below 4%, inflation below 2%): policy is genuine Scaffold. If no sunset mechanism: constraint may persist indefinitely despite changing conditions, degrading from Scaffold to Piton. Historical outcome (restraint lingered into Nixon era as conditions changed) suggests initial Scaffold intent corrupted by institutional inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_mechanism_presence, empirical, 'Presence of explicit or implicit policy sunset conditions').

omega_variable(
    distributional_conflict_naturalness,
    'Is the wage-price-employment trilemma a genuine natural law (mountain) or a constructed constraint reflecting institutional choice about who bears adjustment costs?',
    'Cross-regime comparison: do wage/price/employment relationships differ under different policy frameworks (e.g., strong labor bargaining power vs. weak; price controls vs. voluntary restraint; fiscal expansion vs. restraint)? Historical analysis of whether the trilemma is invariant or contingent on institutional setup.',
    'If mountain: the extraction from workers is unavoidable cost of stability. If constructed: the policy choice to extract from workers (rather than from capital via higher taxes, inflation, or employment guarantees) is political, not natural. Determines whether false summit detector should reclassify this perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_conflict_naturalness, conceptual, 'Whether wage-price-employment trilemma is natural law or institutional construct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1961_kennedy_wage_price_restraint_labor_management, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kprs_tr_t0, sotu_1961_kennedy_wage_price_restraint_labor_management, theater_ratio, 0, 0.52).
narrative_ontology:measurement(kprs_tr_t2, sotu_1961_kennedy_wage_price_restraint_labor_management, theater_ratio, 2, 0.6).
narrative_ontology:measurement(kprs_tr_t4, sotu_1961_kennedy_wage_price_restraint_labor_management, theater_ratio, 4, 0.68).
narrative_ontology:measurement(kprs_tr_t6, sotu_1961_kennedy_wage_price_restraint_labor_management, theater_ratio, 6, 0.65).
narrative_ontology:measurement(kprs_tr_t8, sotu_1961_kennedy_wage_price_restraint_labor_management, theater_ratio, 8, 0.62).

% Extraction over time
narrative_ontology:measurement(kprs_be_t0, sotu_1961_kennedy_wage_price_restraint_labor_management, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kprs_be_t2, sotu_1961_kennedy_wage_price_restraint_labor_management, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(kprs_be_t4, sotu_1961_kennedy_wage_price_restraint_labor_management, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(kprs_be_t6, sotu_1961_kennedy_wage_price_restraint_labor_management, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(kprs_be_t8, sotu_1961_kennedy_wage_price_restraint_labor_management, base_extractiveness, 8, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1961_kennedy_wage_price_restraint_labor_management, resource_allocation).
narrative_ontology:affects_constraint(sotu_1961_kennedy_wage_price_restraint_labor_management, phillips_curve_consensus_1960s).
narrative_ontology:affects_constraint(sotu_1961_kennedy_wage_price_restraint_labor_management, labor_management_collective_bargaining).
narrative_ontology:affects_constraint(sotu_1961_kennedy_wage_price_restraint_labor_management, inflation_expectations_anchor).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the broader Phillips curve consensus (wage-price relationship) and labor-management institutional framework. The voluntary restraint policy is one manifestation of the broader attempt to coordinate wage-price expectations in a context of full employment recovery. Related constraints include the institutional legitimacy of collective bargaining (which the policy both reinforces and exploits) and the anchoring of inflation expectations (which the policy attempts to manage through voluntary commitment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1961_kennedy_wage_price_restraint_labor_management, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
