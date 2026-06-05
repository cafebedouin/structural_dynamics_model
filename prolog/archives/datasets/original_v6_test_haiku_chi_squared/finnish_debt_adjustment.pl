% ============================================================================
% CONSTRAINT STORY: finnish_debt_adjustment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finnish_debt_adjustment, []).

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
 *   constraint_id: finnish_debt_adjustment
 *   human_readable: Finnish Private Debt Adjustment System
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Finnish Private Debt Adjustment system (Yksityishenkilön
 *   velkaantumisongelmat) represents a hybrid institutional arrangement
 *   balancing creditor recovery with debtor rehabilitation. Enacted as
 *   emergency crisis response during the 1995 recession, the system has
 *   evolved into a structural feature of Finnish welfare economics. Debtors
 *   meeting insolvency criteria enter a court-supervised restructuring
 *   process lasting 3-5 years (renewable to 20), with supervised partial debt
 *   repayment leading to eventual discharge. The system coordinates
 *   collective forbearance: creditors agree to structured payment schedules
 *   rather than aggressive collection; debtors commit to transparent income
 *   disclosure and payment obligations; the court supervises administration.
 *   However, the actual extraction mechanics are complex: debtors lose
 *   negotiating power, accept long-term income constraints, and bear
 *   reputational costs; unsecured creditors absorb 30-80% losses while
 *   secured creditors (typically banks) recover through collateral and wage
 *   assignment priority; insolvency administrators charge fees and extract
 *   administrative rent. The constraint exhibits tangled rope structure:
 *   genuine coordination function (stabilizing debtor incomes, preventing
 *   chaotic defaults) combined with asymmetric extraction (weaker debtors and
 *   unsecured creditors absorb disproportionate costs). The theatrical
 *   element (court supervision, formal process) appears elevated relative to
 *   functional outcomes, suggesting piton characteristics emerge over time as
 *   the system ages from emergency measure to routine administrative
 *   machinery.
 *
 * KEY AGENTS:
 *   - Over-indebted debtors: Primary victim (powerless/trapped) — subject to court supervision, wage assignment, long-term repayment obligations with minimal negotiating power
 *   - Secured creditors (banking sector): Primary beneficiary (institutional/arbitrage) — recover claims through priority access to wages and collateral liquidation; extract rents through participation in adjustment process
 *   - Unsecured creditors: Secondary victim (moderate/constrained) — accept 30-80% writedowns; constrained by court decisions and creditor composition in negotiations
 *   - Insolvency administrators: Institutional beneficiary (institutional/arbitrage) — extract administrative fees and court-supervised rents from adjustment process
 *   - Consumer welfare advocates: Organized agents (organized/constrained) — see system as temporary safety net pending structural reforms in income support and housing affordability
 *   - Court system: Institutional actor (institutional/arbitrage) — maintains formal supervision role; supplies appearance of debtor protection while actual outcomes depend on private creditor negotiation
 *   - Analytical observer: Global perspective (analytical/analytical) — assesses system as modest extraction embedded in larger Nordic welfare coordination; contrasts with more severe debt regimes in other jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finnish_debt_adjustment, 0.38).
domain_priors:suppression_score(finnish_debt_adjustment, 0.42).
domain_priors:theater_ratio(finnish_debt_adjustment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finnish_debt_adjustment, extractiveness, 0.38).
narrative_ontology:constraint_metric(finnish_debt_adjustment, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(finnish_debt_adjustment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finnish_debt_adjustment, tangled_rope).
narrative_ontology:human_readable(finnish_debt_adjustment, "Finnish Private Debt Adjustment System").
narrative_ontology:topic_domain(finnish_debt_adjustment, "economic/political").

domain_priors:requires_active_enforcement(finnish_debt_adjustment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, creditors).
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, banking_sector).
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, insolvency_administrators).
narrative_ontology:constraint_victim(finnish_debt_adjustment, over_indebted_debtors).
narrative_ontology:constraint_victim(finnish_debt_adjustment, unsecured_creditors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVER-INDEBTED DEBTOR (SNARE) — Trapped in debt restructuring with minimal negotiating power. Must accept court-supervised adjustment or face wage garnishment and asset seizure. No credible exit option. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(finnish_debt_adjustment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNSECURED CREDITOR (TANGLED ROPE) — Experiences both coordination (system prevents debtor exit via flight or default) and extraction (debt adjustment may write down claims by 30-80%). Constrained by court decisions; cannot freely pursue collection. d≈0.62, f(d)≈0.83, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(finnish_debt_adjustment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BANKING SECTOR (ROPE) — Primary beneficiary. System coordinates debtor behavior (predictable payment schedule), reduces capital flight risk, enables recovery of secured claims. Can exit through collateral liquidation or wage assignment. d≈0.12, f(d)≈0.05, σ=1.0 → χ≈0.02. Near-zero extraction; pure coordination benefit.
constraint_indexing:constraint_classification(finnish_debt_adjustment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSUMER DEBT ADVOCATES (SCAFFOLD) — See the system as temporary backstop before structural reforms (income support, housing affordability, wage floors). System coordinates creditor forbearance while broader social policy matures. Sunset: economic transition to lower-debt equilibrium. d≈0.48, f(d)≈0.63, σ=1.0 → χ≈0.24.
constraint_indexing:constraint_classification(finnish_debt_adjustment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSOLVENCY ADMINISTRATION (PITON) — Originally designed as temporary crisis relief (1995 recession); now performs largely theatrical role. Court supervision creates appearance of debtor protection while actual outcomes depend on private creditor agreement. theater_ratio=0.58; high administrative overhead for modest recovery. Maintained through institutional inertia despite efficiency questions.
constraint_indexing:constraint_classification(finnish_debt_adjustment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — System coordinates collective debtor-creditor restraint while extracting from weakest debtors through extended repayment obligations (5-20 years) and reputational constraints. Global scope: Finnish model influences Nordic welfare policy; extraction is modest relative to Anglo-American debt markets. d≈0.58, f(d)≈0.75, σ=1.2 → χ≈0.34.
constraint_indexing:constraint_classification(finnish_debt_adjustment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finnish_debt_adjustment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(finnish_debt_adjustment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finnish_debt_adjustment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(finnish_debt_adjustment, TR),
    TR >= 0.70.

:- end_tests(finnish_debt_adjustment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The system extracts rents through extended repayment obligations (often 5+ years longer than voluntary settlements would require), administrative fees, reputational constraints, and reduced future creditworthiness. However, extraction is bounded by the formal structure — creditors cannot use coercive mechanisms beyond court-supervised wage assignment, and the system prevents catastrophic outcomes (homelessness, debt slavery) more effectively than unregulated debt markets. The value of 0.38 reflects that this is structured extraction, not predatory extraction. Suppression (0.42): Moderate. Debtors have formal legal right to petition for adjustment and court representation; the process is transparent and rule-governed. However, suppression is real: the threshold for qualification (insolvency, good faith efforts at negotiation) excludes many over-indebted but employed debtors; the process requires accepting long-term repayment obligations as the only alternative to wage garnishment; social stigma and credit score destruction suppress voluntary disclosure. Theater ratio (0.58): Moderate-high and increasing. Court supervision creates appearance of neutral, independent debtor protection while the actual outcomes depend heavily on private creditor agreement and relative bargaining power. Insolvency administrators serve formal gatekeeping role but most work is administrative fee collection. As the system has aged from emergency response (1995) to routine procedure, the theater ratio has risen as ritualization increases and functional outcomes become less predictable from the formal process.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence here reflects power asymmetry and structural position. Over-indebted debtors (powerless/trapped) experience maximum extraction (snare): they face binary choice between adjustment and catastrophic collection tactics, and the adjustment imposes 5-20 year repayment obligations with no negotiating power. Unsecured creditors (moderate/constrained) experience tangled rope: they benefit from the coordination (predictable payment instead of default contagion) but absorb disproportionate losses relative to secured creditors. Banks (institutional/arbitrage) experience rope: the system prevents debtor exit (capital flight, default) while preserving their priority access to secured collateral and wages. Welfare advocates (organized/constrained) see scaffold: the system buys time for structural reforms in housing affordability and income support while the theater temporarily masks creditor interests. The court/administration (institutional/arbitrage) sees piton: their formal supervisory role is largely theatrical, with outcomes determined by private negotiation, yet they maintain the institutional role through fee extraction and formal gatekeeping. The analytical observer (global scope) sees tangled rope: the system is less extractive than unregulated debt markets but more extractive than pure coordination mechanisms; classified as moderate hybrid at civilizational scale.
 *
 * DIRECTIONALITY LOGIC:
 *   Over-indebted debtors: Victim + trapped → d≈0.92, f(d)≈1.39. Nearly maximal extraction. They are forced into the system, have no credible outside option, and bear the largest adjustment burden relative to their baseline no-system outcome (default + wage garnishment). Banks: Beneficiary + arbitrage → d≈0.12, f(d)≈0.05. Minimal extraction; net coordination benefit. They can exit through secured collateral liquidation, participate voluntarily in adjustment (they negotiate terms), and benefit from prevented default contagion. Unsecured creditors: Victim + constrained → d≈0.62, f(d)≈0.83. Significant extraction but not maximal. Constrained by court decisions and creditor composition; cannot exit bilateral negotiations once debtor enters formal process. Insolvency administrators: Beneficiary + arbitrage → d≈0.15, f(d)≈0.07. Low extraction; administrative fees are modest relative to claim size, and they participate voluntarily in the system. Consumer advocates: Organized + constrained → d≈0.48, f(d)≈0.63. Moderate extraction; they have collective voice and institutional access but are constrained by existing debt stock and structural reform timelines. Analytical observer: analytical → d≈0.58, f(d)≈0.75. Moderate extraction from global perspective; the system is less severe than predatory debt regimes but more constrained than pure voluntary coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through structured institutional analysis: the system is NOT pure coordination (rope) because it creates systematic asymmetric outcomes — weaker debtors and unsecured creditors absorb losses while secured creditors extract rents. It is also NOT pure extraction (snare) at the system level because it prevents worse outcomes (uncontrolled default cascades, debt slavery) and provides genuine debtor rehabilitation pathways. The tangled rope classification captures the hybrid: real coordination function (stabilizing debtor behavior, preventing systemic contagion, enabling partial recovery) combined with real asymmetric extraction (creditor rents, debtor long-term income constraints, reputational costs). The theater ratio indicates institutional aging: the system began as emergency relief (lower theater) but has evolved into routine procedure with increasing ritual and declining functional transparency. The piton perspective (insolvency administration) reveals the administrative layer is substantially performative — maintaining formal supervision while creditors negotiate outcomes privately. This is not mandatrophy failure but mandatrophy resolution through perspectival precision: each agent experiences a different structural reality, and the system's true nature emerges from the presheaf of classifications, not from forcing a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_write_down_threshold,
    'What proportion of debt should be written down to constitute genuine debtor relief vs. creditor-friendly reduction?',
    'Longitudinal comparison: debtors with >60% writedown vs. <20% writedown; tracking of post-adjustment reemployment, reaccumulation, poverty rates',
    'If writedown threshold is structural relief: system is more rope than snare. If threshold is creditor-protective floor: system is more snare than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_write_down_threshold, empirical, 'Debt writedown threshold determining genuine relief vs. creditor protection').

omega_variable(
    debtor_outside_option_availability,
    'Do debtors have credible alternative exit strategies (emigration, informal economy, default acceptance) that make the formal adjustment process voluntary or merely least-worst option?',
    'Survey of debtor preferences for informal vs. formal settlement; analysis of cross-border debt enforcement and emigration patterns; comparison with countries without formal adjustment systems',
    'If credible alternatives exist: system is coordination (Rope). If alternatives are costlier than formal system: system is extraction (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debtor_outside_option_availability, empirical, 'Availability of debtor exit options outside formal system').

omega_variable(
    creditor_incentive_alignment,
    'Do secured creditors (banks) use adjustment system strategically to reduce unsecured creditors'' recovery, thereby concentrating losses on non-bank creditors?',
    'Analysis of secured vs. unsecured recovery rates; correlation between bank participation and non-bank creditor writedown; comparison of adjustment outcomes in bank-heavy vs. diverse creditor compositions',
    'If creditor composition affects outcomes: system enables inter-creditor extraction disguised as debtor relief. Classification shifts toward snare for unsecured creditors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_incentive_alignment, empirical, 'Whether secured creditors use adjustment system to reduce unsecured losses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finnish_debt_adjustment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fin_debt_tr_t0, finnish_debt_adjustment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fin_debt_tr_t10, finnish_debt_adjustment, theater_ratio, 10, 0.51).
narrative_ontology:measurement(fin_debt_tr_t20, finnish_debt_adjustment, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(fin_debt_be_t0, finnish_debt_adjustment, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fin_debt_be_t10, finnish_debt_adjustment, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(fin_debt_be_t20, finnish_debt_adjustment, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finnish_debt_adjustment, enforcement_mechanism).
narrative_ontology:affects_constraint(finnish_debt_adjustment, nordic_credit_market_discipline).
narrative_ontology:affects_constraint(finnish_debt_adjustment, welfare_state_bankruptcy_regime).

% DUAL FORMULATION NOTE:
% The Finnish system is downstream of European insolvency harmonization efforts but upstream of broader Nordic welfare state configuration. The system's extraction mechanism (debtor income constraints, creditor write-downs) is structurally distinct from alternative mechanisms (bankruptcy discharge, creditor priority regimes) with different ε values reflecting their functional designs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(finnish_debt_adjustment, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
