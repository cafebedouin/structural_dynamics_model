% ============================================================================
% CONSTRAINT STORY: emerging_market_debt_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emerging_market_debt_trap, []).

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
 *   constraint_id: emerging_market_debt_trap
 *   human_readable: Emerging Market Debt Trap
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The emerging market debt trap is a structural mechanism through which
 *   capital-exporting nations and international financial institutions
 *   systematize the extraction of wealth from developing countries through
 *   debt service obligations. The constraint operates through multiple
 *   reinforcing mechanisms: (1) currency mismatch — borrowing in foreign
 *   currency creates irreducible vulnerability to exchange rate shocks; (2)
 *   austerity conditionality — IMF programs enforce pro-cyclical fiscal
 *   contraction that suppresses domestic investment and growth; (3)
 *   collateral threat — asset seizure and capital flight risk prevent exit;
 *   (4) elite capture — domestic elites benefit from debt-funded
 *   infrastructure and export markets, creating internal coalitions aligned
 *   with creditors. The constraint exhibits high extractiveness (0.68)
 *   because the extraction is systematic and persistent: debt service
 *   transfers wealth across borders continuously, and the mechanism is
 *   self-reinforcing — austerity reduces growth, which increases debt-to-GDP
 *   ratios, which increases extraction pressure. Suppression is very high
 *   (0.72) because structural barriers to exit include currency controls, IMF
 *   conditionality, sovereign collateral seizure threats, and capital flight
 *   mechanisms. The theater ratio (0.55) reflects that a substantial portion
 *   of the institutional apparatus (IMF conditionality, structural adjustment
 *   programs, poverty reduction papers) is performative — these documents
 *   articulate growth and poverty-reduction narratives while functionally
 *   enforcing extraction. The debt trap is not immutable (not a mountain)
 *   because alternative institutional arrangements are theoretically possible
 *   (local currency debt markets, debt forgiveness, capital controls). It is
 *   not pure coordination (not a rope) because it systematically benefits
 *   some (capital exporters, domestic elites) at severe cost to others
 *   (debtor nations' populations, labor sector). The constraint is a snare: a
 *   mechanism of pure extraction with minimal coordination function,
 *   sustained by suppressing alternatives.
 *
 * KEY AGENTS:
 *   - Debtor Nation Government: Primary victim (powerless/trapped) — faces currency mismatch, debt service obligations, IMF conditionality; cannot exit without currency collapse and capital flight
 *   - Domestic Population: Primary victim (powerless/trapped) — bears austerity costs, reduced public investment, intergenerational wealth transfer through debt service
 *   - External Creditors: Primary beneficiary (institutional/arbitrage) — capture high-yield returns on emerging market debt; can redeploy capital with minimal friction
 *   - Capital-Exporting Nations: Secondary beneficiary (institutional/arbitrage) — benefit from debt-service flows, collateral acquisition, geopolitical leverage over debtor nations
 *   - Domestic Elites: Mixed agent (moderate/constrained) — benefit from debt-funded infrastructure and export markets; constrained by dependence on external credit and capital flight risk
 *   - Labor and Small Business Sector: Organized victim (organized/constrained) — face wage suppression, unemployment from austerity; dependent on debt-financed economy but bear extraction costs
 *   - International Financial Institutions (IMF/World Bank): Enforcement mechanism (institutional/arbitrage) — administer conditionality programs that maintain extraction mechanism; functionally aligned with creditors despite stated development mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emerging_market_debt_trap, 0.68).
domain_priors:suppression_score(emerging_market_debt_trap, 0.72).
domain_priors:theater_ratio(emerging_market_debt_trap, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emerging_market_debt_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(emerging_market_debt_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(emerging_market_debt_trap, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emerging_market_debt_trap, snare).
narrative_ontology:human_readable(emerging_market_debt_trap, "Emerging Market Debt Trap").
narrative_ontology:topic_domain(emerging_market_debt_trap, "economic/geopolitical").

domain_priors:requires_active_enforcement(emerging_market_debt_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emerging_market_debt_trap, external_creditors).
narrative_ontology:constraint_beneficiary(emerging_market_debt_trap, capital_exporting_nations).
narrative_ontology:constraint_victim(emerging_market_debt_trap, emerging_market_governments).
narrative_ontology:constraint_victim(emerging_market_debt_trap, domestic_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR NATION (SNARE) — Trapped by currency mismatch, external debt obligations, and foreign-denominated liabilities. Exit is not feasible: default triggers capital flight, currency collapse, and humanitarian crisis. Domestic population bears full extraction burden through austerity, reduced investment in social services, and intergenerational wealth transfer to foreign creditors. Maximum suppression — structural barriers (currency controls, IMF conditionality, collateral seizure threat) prevent exit.
constraint_indexing:constraint_classification(emerging_market_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXTERNAL CREDITORS (ROPE) — Net beneficiaries experiencing the debt relationship as coordination. Access to high-yield emerging market assets, debt restructuring optionality, and collateral claims provide positive directionality. Can exit (redirect capital) with minimal cost through diversification. Experience the mechanism as a legitimate coordination of capital flows — their perspective naturalizes the extraction as fair market pricing.
constraint_indexing:constraint_classification(emerging_market_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DOMESTIC ELITES (TANGLED ROPE) — Constrained but with asymmetric benefits. Elites capture rents from debt-funded infrastructure and resource extraction; export capital and accumulate foreign assets; but also depend on external credit access for political stability and personal wealth preservation. Face high costs of exit (capital controls, reputational damage, loss of external account access) but gain material benefits from the arrangement. Mixed coordination and extraction experienced.
constraint_indexing:constraint_classification(emerging_market_debt_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR AND SMALL BUSINESS (TANGLED ROPE) — Constrained by capital flight, currency devaluation, and austerity measures; faces reduced credit access, wage suppression, and employment losses. Also depends on the debt-financed economy for livelihoods — coordinated credit expansion fueled growth. Extraction is severe but not absolute: organized labor can strike, demand wage indexation, mobilize political pressure. Constrained rather than trapped exit options create perspectival gap from powerless perspective.
constraint_indexing:constraint_classification(emerging_market_debt_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: IMF/WORLD BANK SYSTEM (PITON) — Institutional structures (IMF conditionality, structural adjustment programs) that maintain the debt trap mechanism persist through structural inertia despite degraded function. IFI programs nominally coordinate macroeconomic stabilization but functionally enforce extraction on behalf of creditors. Theater ratio high — conditionality documents articulate poverty-reduction and growth narratives while enforcing austerity and privatization. The mechanism persists not because it works (growth outcomes are often negative) but because the alternative (allowing default/restructuring) is politically unacceptable to creditor nations.
constraint_indexing:constraint_classification(emerging_market_debt_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the debt trap is a structural mechanism of global capital accumulation. Currency issuance monopoly in creditor countries + liabilities denominated in foreign currency + limited exit options = systematic extraction mechanism. The constraint is not a coordination problem or immutable law but a designed (or emergent) institutional arrangement that concentrates wealth globally toward capital exporters and creditor nations.
constraint_indexing:constraint_classification(emerging_market_debt_trap, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emerging_market_debt_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emerging_market_debt_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emerging_market_debt_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emerging_market_debt_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emerging_market_debt_trap, TR),
    TR >= 0.70.

:- end_tests(emerging_market_debt_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and accumulating. The initial extractiveness (0.35) reflects the early-stage debt boom when growth rates are positive and debt service feels manageable. As the constraint tightens over the interval, extractiveness rises to 0.52 at mid-interval and 0.68 at the end. The accumulation reflects debt maturity effects (compound interest), currency devaluation (increasing foreign-currency obligations), and austerity-induced growth collapse (increasing debt ratios). At 0.68, extractiveness is clearly in snare territory. Suppression (0.72): Very high and stable. Structural barriers to exit include irreversible currency mismatch (cannot un-borrow in foreign currency), IMF conditionality (reduces policy autonomy), capital flight mechanisms (any debt default triggers immediate currency run), and collateral seizure threats (creditors can seize sovereign assets). These are not temporary or negotiable — they are baked into the debt contract structure and international law. Theater ratio (0.55): Moderate and rising slightly. IMF conditionality documents (Poverty Reduction Strategy Papers, Letters of Intent) articulate growth and poverty-reduction narratives, but the actual conditionality (privatization, public sector cuts, trade liberalization) typically worsens growth and increases inequality in the short to medium term. The theater is real but not overwhelming — some of the institutional apparatus does attempt genuine stabilization (currency reserves management, debt restructuring frameworks), but these functions are subordinated to extraction maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence because it involves fundamental asymmetry in exit capacity and wealth concentration. The powerless debtor nation sees a snare with no escape route. The institutional creditor sees a rope — a coordination mechanism that functions perfectly from their perspective. The gap reveals that 'coordinate' from the creditor's view means 'extract' from the debtor's view. The same cash flows that the creditor experiences as fair return on risk, the debtor experiences as coercive extraction. The piton classification for IFI institutions reveals that their conditionality programs are theater — they perform development/stabilization narratives while functionally enforcing extraction. The snare classification is correct: high extractiveness, high suppression, minimal coordination function except from the creditor's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary dramatically by agent. The debtor nation government and domestic population are victims with trapped exit options: d ≈ 0.95, f(d) ≈ 1.42, producing maximum chi. External creditors are beneficiaries with arbitrage exit: d ≈ 0.05, f(d) ≈ -0.12, producing negative chi (they experience subsidy). Domestic elites are mixed beneficiaries and constrained agents: d ≈ 0.35, f(d) ≈ 0.40, producing moderate chi. Labor/small business are victims with constrained exit (organized): d ≈ 0.55, f(d) ≈ 0.75, producing moderate-high chi. The IFI system is an enforcement institution with arbitrage capacity: d ≈ 0.30, f(d) ≈ 0.35. These differentiated directionality values explain why the same constraint produces radically different classifications across perspectives: the trapped victim experiences maximum extraction; the beneficiary experiences subsidy; the organized victim has agency; the piton sees degradation. The directionality logic is not about fairness — it is structural. The debtor cannot leave; the creditor can. That asymmetry drives everything.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The debt trap is a snare from nearly all victim perspectives (debtor government, domestic population, labor sector) and a rope from creditor perspectives. The mandatrophy is resolved by rejecting the false choice between 'this is pure extraction' and 'this is legitimate coordination.' The resolution is: it IS legitimate coordination from the creditor/capital-exporter perspective (a functioning market mechanism) AND it IS pure extraction from the debtor perspective (a mechanism with minimal coordination function that they experience as inescapable). The trap persists because institutional arrangements (currency mismatch, IMF conditionality, collateral law) lock victims into suppression while preserving creditor exit capacity. The analytical observer recognizes both truths: the market logic is coherent from the creditor side, AND the extraction logic is equally coherent from the debtor side. The constraint is not misclassified — it is genuinely a snare operating within a rope-like institutional ecosystem. Mandatrophy is resolved by understanding that the constraint type depends on the structural position of the observer, and that structural position is what the constraint itself creates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_sustainability_threshold,
    'What debt-to-GDP ratio threshold distinguishes sustainable borrowing from a trap?',
    'Historical default analysis; correlation between debt ratios, growth rates, and likelihood of restructuring or default across 30+ emerging markets over 40 years',
    'If threshold < 60%: many countries are perpetually trapped. If threshold > 90%: trap detection is delayed until crisis. Actual threshold varies by creditor nation confidence, currency reserve position, and commodity price volatility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability_threshold, empirical, 'Debt-to-GDP ratio threshold for trap identification').

omega_variable(
    currency_mismatch_necessity,
    'Is currency mismatch (borrowing in foreign currency when revenues are domestic) inherent to capital flows or a design choice by creditors and IFIs?',
    'Analysis of alternative lending architectures (SDR-denominated debt, domestic currency lending with inflation hedging); comparison of outcomes in countries that successfully built local currency debt markets vs those locked in foreign currency dependence',
    'If inherent: debt trap is natural consequence of global capital asymmetry (mountain-adjacent). If design choice: alternative arrangements are possible, and the trap is contingent institutional structure (snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(currency_mismatch_necessity, empirical, 'Whether currency mismatch is inherent or institutional choice').

omega_variable(
    elite_defection_threshold,
    'At what combination of extraction intensity and exit cost do domestic elites switch from beneficiary to co-victim and organize against the trap?',
    'Case studies of successful debt restructurings vs failed ones; analysis of elite coalition dynamics in Iceland (2008), Argentina (2001), Ecuador (2008), Greece (2015) relative to extraction intensity and elite options',
    'If threshold is low: organized elite defection could trigger rapid regime change. If threshold is high: elites remain beneficiaries even under extreme extraction of masses, and trap persists until external creditor appetite declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_defection_threshold, empirical, 'Elite defection threshold in debt trap stability').

omega_variable(
    external_enforcement_dependence,
    'How much of the debt trap''s suppression derives from explicit enforcement (gunboat diplomacy, IMF conditionality, collateral seizure threats) vs implicit enforcement (market expectations, rating agency downgrades, creditor withdrawal)?',
    'Historical analysis of debt crises and restructuring: comparison of countries with explicit enforcement pressure vs market-driven enforcement; study of effectiveness of debt resistance (Argentina 2001-2005, Ecuador 2008) relative to external enforcement intensity',
    'If explicit enforcement dominates: trap is vulnerable to unified debtor resistance. If market enforcement dominates: trap is self-perpetuating through expectations and harder to break collectively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_enforcement_dependence, empirical, 'Explicit vs implicit enforcement mechanisms in debt trap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emerging_market_debt_trap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emdt_tr_t0, emerging_market_debt_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emdt_tr_t10, emerging_market_debt_trap, theater_ratio, 10, 0.48).
narrative_ontology:measurement(emdt_tr_t20, emerging_market_debt_trap, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(emdt_be_t0, emerging_market_debt_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emdt_be_t10, emerging_market_debt_trap, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(emdt_be_t20, emerging_market_debt_trap, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emerging_market_debt_trap, resource_allocation).
narrative_ontology:affects_constraint(emerging_market_debt_trap, currency_crisis_cascade).
narrative_ontology:affects_constraint(emerging_market_debt_trap, structural_adjustment_austerity).
narrative_ontology:affects_constraint(emerging_market_debt_trap, capital_flight_mechanism).

% DUAL FORMULATION NOTE:
% The emerging market debt trap decomposes into three related constraints: (1) currency crisis dynamics (ε≈0.55, immediate/national scope), (2) structural adjustment austerity conditionality (ε≈0.60, biographical/national scope), and (3) capital flight mechanism (ε≈0.50, immediate/global scope). Each has distinct measurement profiles and remedy pathways. The umbrella debt trap story (this file) represents the integrated mechanism at civilizational scope (ε≈0.68); the subsidiary stories capture domain-specific extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emerging_market_debt_trap, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
