% ============================================================================
% CONSTRAINT STORY: fiscal_dominance_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiscal_dominance_trap, []).

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
 *   constraint_id: fiscal_dominance_trap
 *   human_readable: The Debt-Monetary Bind
 *   domain: economic/political
 *
 * SUMMARY:
 *   The fiscal dominance trap is a structural bind where a central bank loses
 *   monetary independence because its primary policy tool — raising interest
 *   rates to control inflation — would trigger government debt insolvency.
 *   This occurs when government debt is large relative to GDP and maturity
 *   structure is short, making interest rate increases immediately explosive
 *   for debt servicing costs. The constraint is active when: (1) debt-to-GDP
 *   exceeds ~90%, (2) primary deficit is structurally large (cannot be closed
 *   by growth alone), (3) central bank holds significant government debt
 *   (either explicitly or through financial system stability concerns). The
 *   trap creates a dilemma: loose monetary policy causes inflation and
 *   currency debasement (harming savers and wage-earners), while tight
 *   monetary policy triggers sovereign debt crisis (harming everyone through
 *   financial instability). Neither option exits the constraint; both are
 *   forms of extraction. The constraint is not inherent to any monetary
 *   system — it is contingent on debt accumulation and fiscal structure.
 *   Low-debt economies (e.g., Germany <60% debt-to-GDP) experience no fiscal
 *   dominance trap. The theater ratio (0.55) reflects that central banks
 *   maintain inflation-targeting frameworks and independence narratives even
 *   when those frameworks are operationally constrained by fiscal dominance.
 *   Regulatory stress tests and macroprudential regulation persist as
 *   performative activity once fiscal dominance becomes the binding
 *   constraint — the theater is the communication of central bank control
 *   that no longer exists.
 *
 * KEY AGENTS:
 *   - Central Bank: Institutional actor with constrained exit (institutional/constrained) — nominally independent but structurally dominated by fiscal authority
 *   - Fiscal Authority (Government): Primary beneficiary (institutional/arbitrage) — captures benefits of monetary accommodation; can run deficits without immediate discipline
 *   - Currency Creditors: Primary victims (powerless/trapped) — hold government debt; unable to exit without losses; trapped between rate risk and inflation risk
 *   - Working-Age Population: Secondary victims (powerless/trapped) — wage-earners trapped between inflation and unemployment
 *   - Financial Stability Regulator: Institutional actor (institutional/constrained) — maintains performative regulation; actual function degraded
 *   - International Monetary Community: Organized actors (organized/constrained) — can provide temporary scaffolding through multilateral financing and coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiscal_dominance_trap, 0.58).
domain_priors:suppression_score(fiscal_dominance_trap, 0.72).
domain_priors:theater_ratio(fiscal_dominance_trap, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiscal_dominance_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(fiscal_dominance_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fiscal_dominance_trap, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiscal_dominance_trap, snare).
narrative_ontology:human_readable(fiscal_dominance_trap, "The Debt-Monetary Bind").
narrative_ontology:topic_domain(fiscal_dominance_trap, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_victim(fiscal_dominance_trap, inflation_control_capacity).
narrative_ontology:constraint_victim(fiscal_dominance_trap, currency_creditors).
narrative_ontology:constraint_victim(fiscal_dominance_trap, working_age_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENCY CREDITOR (SNARE) — Holds government debt denominated in local currency. Cannot exit without realizing losses. Central bank rate increases devalue their holdings through inflation or capital loss; rate decreases erode real returns. Trapped between monetary tightening (insolvency risk) and loose money (debasement). d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(fiscal_dominance_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-AGE POPULATION (SNARE) — Wage earners cannot exit the constraint. Caught between: (a) loose monetary policy erodes real wages through inflation, (b) tight monetary policy triggers unemployment. No exit option. Trapped through employment dependence and currency-local savings. d≈0.90, f(d)≈1.35, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(fiscal_dominance_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANK (TANGLED ROPE) — Nominally independent but structurally constrained. Coordination function: monetary policy stabilization. But enforcement is constrained by fiscal dominance — raising rates to fight inflation risks triggering sovereign debt crisis. Benefits from: maintained monetary authority narrative, technical credibility in benign fiscal regimes. Costs: actual power eroded, forced choice between mandate failure (inflation) or solvency crisis (debt spiral). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(fiscal_dominance_trap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL AUTHORITY / GOVERNMENT (ROPE) — Primary beneficiary. Fiscal dominance allows government to run primary deficits without immediate market discipline. Central bank monetization (implicit or explicit) provides backstop. Experiences constraint as coordination mechanism: monetary accommodation enables fiscal transfers. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary through arbitrage exit (can shift costs to central bank, currency debtors).
constraint_indexing:constraint_classification(fiscal_dominance_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FINANCIAL STABILITY REGULATOR (PITON) — Maintains performative stress-testing and macroprudential regulation, but actual function is diminished. Regulation persists through institutional ritual (Basel III, stress tests, capital requirements) even though systemic risk is now driven by fiscal dominance, not bank leverage. Theater ratio = 0.55 reflects that roughly half of regulatory output is response to fiscal constraint rather than actual financial-sector risk management. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(fiscal_dominance_trap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL MONETARY COMMUNITY (SCAFFOLD) — Organized actors (IMF, multilateral development banks, peer central banks) can temporarily mitigate fiscal dominance through: (a) emergency financing, (b) capacity-building for fiscal reform, (c) coordination of debt restructuring expectations. But the fundamental exit (fiscal consolidation or debt haircut) remains. Sunset logic: fiscal consolidation or external discipline eventually breaks the trap. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.15. Moderate suppression by external monitoring; organized actors have exit pathways (conditionality, multilateral governance).
constraint_indexing:constraint_classification(fiscal_dominance_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalization: 'Fiscal dominance is an inherent feature of monetary systems where sovereign debt is substantial.' But the structural data (ε=0.58, suppression=0.72, theater=0.55) contradicts the mountain classification. Fiscal dominance is contingent on: (a) debt-to-GDP ratios, (b) maturity structure, (c) exchange rate regime, (d) inflation expectations — all policy-dependent variables. A low-debt state experiences no fiscal dominance trap. The mountain perspective is a false summit revealing how much contingent institutional arrangement naturalizes as 'law of economics.'
constraint_indexing:constraint_classification(fiscal_dominance_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_dominance_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fiscal_dominance_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_dominance_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fiscal_dominance_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fiscal_dominance_trap, TR),
    TR >= 0.70.

:- end_tests(fiscal_dominance_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from savers and wage-earners through the forced choice between inflation and unemployment. Extraction increases over time as debt accumulates (initial ε=0.32 → final ε=0.58) because the range of policy options narrows. The original fiscal authority captures benefits of monetary accommodation at the cost of creditor and worker welfare. Suppression (0.72): High. Exits are severely constrained: (a) creditors cannot exit debt holdings without realizing losses; (b) workers cannot exit employment or currency dependence; (c) central bank cannot raise rates without triggering fiscal crisis. The only exit is emigration (arbitrage for high-skill workers) or debt restructuring (coercive). Suppression reflects that alternatives to accepting the bind are politically difficult or expensive. Theater ratio (0.55): Moderate. Central banks maintain inflation-targeting frameworks, independence narratives, stress-testing programs, and forward guidance. Roughly half of this activity is genuine coordination (managing expectations, technical credibility) and half is performative — the framework persists even though the constraint is now driven by fiscal fundamentals, not monetary technique. The theater increases slightly over the interval (0.35→0.55) as the gap between stated mandate and operational capacity widens.
 *
 * PERSPECTIVAL GAP:
 *   The fiscal dominance trap produces a sharp perspectival gap between beneficiaries and victims. The fiscal authority (government) sees Rope — monetary accommodation as a coordination mechanism enabling fiscal transfers. They experience low extraction (d≈0.08) and benefit from the arbitrage of shifting costs to savers and monetary authorities. The central bank sees Tangled Rope — they have a genuine coordination function (monetary stability) but cannot execute it without triggering fiscal crisis. Their d rises to 0.68 because the structural constraint now extracts their autonomy. Currency creditors and workers see Snare — they experience maximal extraction (d≈0.90+) with no exit option. The international community sees Scaffold — they can provide temporary mitigation through multilateral financing, but the fundamental problem (fiscal consolidation or debt restructuring) remains. The analytical observer risks a Mountain perspective (naturalizing 'inherent fiscal constraint') but the structural data reveals this as a false summit: the trap depends entirely on debt-to-GDP and primary deficit ratios, both of which are policy-contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Fiscal Authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Monetary accommodation allows continued deficits. Central Bank: Victim (of fiscal dominance) + constrained → d≈0.68, f(d)≈1.05. Significant extraction of autonomy. Cannot raise rates without triggering crisis. Cannot lower rates without ignoring inflation mandate. Currency Creditors: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit from debt holdings. Rate increases devalue holdings; rate decreases debase currency. Workers: Victim + trapped → d≈0.90, f(d)≈1.35. Maximum extraction. No exit from employment or currency dependence. Trapped between inflation and unemployment. Financial Regulator: Victim (of fiscal constraint) + constrained → d≈0.55, f(d)≈0.75. Moderate extraction. Original prudential function is constrained; regulation becomes performative. International Community: Organized + constrained → d≈0.35, f(d)≈0.35. Low effective extraction; organized actors have exit pathways and can provide temporary scaffolding.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The snare classification resolves the mandatrophy by showing that fiscal dominance is NOT a natural law (false mountain) and NOT a coordination mechanism (false rope). It is pure extraction: the constraint forces a choice between two bad outcomes (inflation or unemployment), both of which harm the victims. The coordination function — monetary stability — is genuine in benign fiscal regimes but is completely dominated by fiscal dynamics once debt-to-GDP is high. The central bank's mandate becomes incompatible with government solvency. This is NOT a temporary or beneficial constraint (rope/scaffold) that should be maintained. It is an extractive trap (snare) that reflects fiscal irresponsibility, not monetary necessity. The difference between this and a true coordination problem (e.g., inflation targeting with low debt, where rate increases protect savers and workers by maintaining currency value) is precisely the fiscal dominance: when debt is sustainable, monetary tightening is a coordination good. When debt is unsustainable, the same monetary tightening becomes extraction. The extractiveness (0.58) and suppression (0.72) scores confirm snare classification: high extraction, severe suppression of alternatives, no genuine coordination function once fiscal solvency is at risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_consolidation_feasibility,
    'Is fiscal consolidation (reducing primary deficits) politically feasible before the fiscal dominance trap becomes self-reinforcing through currency debasement and capital flight?',
    'Historical comparison of fiscal consolidation attempts in similar fiscal dominance regimes; measurement of political party consensus on deficit reduction; polling on citizen willingness to accept austerity vs inflation',
    'If feasible (<2 years to credible consolidation plan): trap is temporary (scaffold). If infeasible (>5 years): trap becomes permanent structural feature requiring debt restructuring or high inflation equilibrium (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_consolidation_feasibility, empirical, 'Political feasibility window for fiscal consolidation before self-reinforcement').

omega_variable(
    central_bank_monetary_transmission,
    'At what level of fiscal dominance does central bank monetary policy transmission mechanism break entirely (rate changes no longer affect inflation or employment)?',
    'Empirical monetary policy event study: compare inflation response and employment response to policy rate shocks before vs during fiscal dominance regime; measure credibility of forward guidance',
    'If transmission breaks before fiscal crisis (ε→0.75): central bank is neutered faster than commonly assumed. If transmission persists through crisis (ε stays ~0.58): central bank retains some indirect control through expectations management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(central_bank_monetary_transmission, empirical, 'At what fiscal dominance level does monetary transmission break').

omega_variable(
    debt_sustainability_threshold,
    'What debt-to-GDP ratio or debt service cost threshold triggers market confidence collapse and forces immediate fiscal adjustment or debt restructuring?',
    'Cross-country analysis of debt crises; measurement of CDS spreads, currency volatility, and capital flight timing relative to debt metrics; identification of discontinuities in market behavior',
    'If threshold is high (>150% debt-to-GDP): fiscal dominance trap can persist longer, increasing entrapment severity. If threshold is low (<80%): trap triggers sooner, enabling earlier structural adjustment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability_threshold, empirical, 'Debt threshold triggering market confidence collapse').

omega_variable(
    inflation_expectations_anchoring,
    'Can central bank maintain inflation expectations anchoring through communication alone once fiscal dominance becomes widely recognized, or does credibility erode inevitably?',
    'Time-series analysis of inflation swap rates, survey-based inflation expectations, and central bank communication tone; measurement of divergence between forward-guidance and market pricing',
    'If anchoring persists (expectations remain <3% despite fiscal dominance): constraint severity is overstated; central bank retains credibility tool. If anchoring fails (expectations drift >4%): fiscal dominance becomes visible to market, accelerating capital flight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_expectations_anchoring, empirical, 'Whether inflation expectations remain anchored under fiscal dominance recognition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiscal_dominance_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiscal_tr_t0, fiscal_dominance_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fiscal_tr_t5, fiscal_dominance_trap, theater_ratio, 5, 0.45).
narrative_ontology:measurement(fiscal_tr_t10, fiscal_dominance_trap, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(fiscal_be_t0, fiscal_dominance_trap, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fiscal_be_t5, fiscal_dominance_trap, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(fiscal_be_t10, fiscal_dominance_trap, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiscal_dominance_trap, enforcement_mechanism).
narrative_ontology:affects_constraint(fiscal_dominance_trap, currency_credibility).
narrative_ontology:affects_constraint(fiscal_dominance_trap, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(fiscal_dominance_trap, inflation_expectations_formation).

% DUAL FORMULATION NOTE:
% Fiscal dominance is downstream of government debt accumulation and primary deficit structure. These upstream constraints have their own ε values reflecting fiscal sustainability; fiscal dominance represents the specific coupling mechanism between monetary independence and fiscal solvency. Decomposition: (1) sovereign_debt_sustainability (ε=0.35, structural debt dynamics), (2) fiscal_dominance_trap (ε=0.58, monetary-fiscal coupling), (3) inflation_expectations_formation (ε=0.42, belief cascade from fiscal dominance recognition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fiscal_dominance_trap, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
