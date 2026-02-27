% ============================================================================
% CONSTRAINT STORY: g7_debt_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_g7_debt_trap, []).

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
 *   constraint_id: g7_debt_trap
 *   human_readable: G7 Debt Trap for Developing Nations
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The G7 debt trap operates as a structural extraction mechanism disguised
 *   as a coordination solution. Developing nations facing acute financing
 *   needs accept loans conditional on 'structural adjustment' policies
 *   (privatization, currency devaluation, subsidy elimination, public sector
 *   reductions) that are theoretically justified as growth-promoting
 *   liberalization but empirically correlate with increased inequality,
 *   reduced social spending, and weakened domestic industries. The constraint
 *   exhibits high suppression (limited exit options, enforcement through
 *   conditionality gates, threatened exclusion from capital markets) and
 *   moderate extractiveness (debt servicing plus loss of policy autonomy).
 *   The trap persists through a mix of genuine coordination need (debtor
 *   nations do need financing) and asymmetric institutional power (G7 and IMF
 *   set conditions unilaterally). Over 30 years, the mechanism has evolved:
 *   extractiveness has increased as conditions have accumulated, alternative
 *   creditors have emerged offering exits, and the intellectual justification
 *   (Washington Consensus) has degraded as empirical outcomes contradict the
 *   theory. The constraint manifests differently across structural positions:
 *   as immutable law (analytical observer naturalization), as degraded ritual
 *   (Piton perspective on Washington Consensus doctrine), as a sunset problem
 *   being solved (organized coalition building alternatives), as snare
 *   extraction (debtor populations and domestic industries), and as mixed
 *   coordination-extraction (debtor governments managing competing
 *   pressures).
 *
 * KEY AGENTS:
 *   - G7 Creditor Governments: Primary beneficiary (institutional/arbitrage) — capture debt servicing flows, gain market access through adjustment-mandated liberalization, maintain geopolitical leverage
 *   - Developing Nation Populations: Primary victim (powerless/trapped) — bear costs of adjustment policy (wage stagnation, employment loss, service cuts) with no exit option or negotiating power
 *   - Developing Nation Governments: Secondary actor (moderate/constrained) — need financing but face extraction through conditionality; have constrained options but some negotiating capacity
 *   - IMF/World Bank: Institutional actor (institutional/arbitrage) — maintain lending framework and institutional legitimacy; experience constraint as pure coordination
 *   - Domestic Industries in Debtor Nations: Secondary victim (moderate/constrained) — eliminated by import liberalization and currency devaluation required by adjustment
 *   - Multinational Corporations: Secondary beneficiary (powerful/arbitrage) — gain market access in debtor nations through adjustment-mandated liberalization
 *   - Debt-for-Climate Coalition: Organized alternative (organized/mobile) — building exit pathways through alternative funding, debt swaps, and regional development banks
 *   - Washington Consensus Doctrine: Institutional narrative (institutional/arbitrage) — provides legitimating theory for extraction; increasingly degraded as empirical outcomes contradict predictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(g7_debt_trap, 0.58).
domain_priors:suppression_score(g7_debt_trap, 0.72).
domain_priors:theater_ratio(g7_debt_trap, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(g7_debt_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(g7_debt_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(g7_debt_trap, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(g7_debt_trap, tangled_rope).
narrative_ontology:human_readable(g7_debt_trap, "G7 Debt Trap for Developing Nations").
narrative_ontology:topic_domain(g7_debt_trap, "economic/geopolitical").

domain_priors:requires_active_enforcement(g7_debt_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(g7_debt_trap, g7_creditor_governments).
narrative_ontology:constraint_beneficiary(g7_debt_trap, multinational_corporations).
narrative_ontology:constraint_beneficiary(g7_debt_trap, structural_adjustment_contractors).
narrative_ontology:constraint_victim(g7_debt_trap, developing_nation_populations).
narrative_ontology:constraint_victim(g7_debt_trap, domestic_industries).
narrative_ontology:constraint_victim(g7_debt_trap, public_sector_employment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR NATION'S POPULATION (SNARE) — Trapped in multi-generational debt servicing with no exit option. Structural adjustment conditions (privatization, subsidy cuts, currency devaluation, public sector reductions) directly harm wages, employment, and access to basic services. Population experiences maximum extraction with no agency to negotiate or exit the constraint.
constraint_indexing:constraint_classification(g7_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEBTOR NATION'S GOVERNMENT (TANGLED ROPE) — Constrained by financing need but also experiences coordination benefit through access to international capital markets and IMF technical support. Government has some agency in negotiating terms but faces real exit barriers: refusing loans means losing access to future financing. Mixed extraction and coordination — enforcement is active (conditionality reviews, disbursement gates).
constraint_indexing:constraint_classification(g7_debt_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: G7 CREDITOR GOVERNMENTS (ROPE) — Experiences the constraint as coordination: standardized loan terms, conditionality frameworks, and debt servicing obligations create predictable international financial architecture. Exit option is arbitrage — creditors can shift geopolitical focus or portfolio composition. Net beneficiary: debt servicing flows back to creditor nations, and structural adjustment opens markets for their multinational firms.
constraint_indexing:constraint_classification(g7_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IMF/WORLD BANK FRAMEWORK (ROPE) — Experiences the constraint as pure coordination: standardized adjustment criteria, conditionality benchmarks, and technical support mechanisms enable synchronized lending and risk management across creditor institutions. Institutional framework has arbitrage options (can modify lending criteria, can focus on specific regions). Net beneficiary — constraint maintains their institutional legitimacy and access to funding flows.
constraint_indexing:constraint_classification(g7_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DOMESTIC INDUSTRIES IN DEBTOR NATIONS (SNARE) — Structural adjustment conditions (import liberalization, currency devaluation, subsidy cuts) eliminate protections and make domestic firms uncompetitive against multinational imports. Domestic industries face forced exit from markets, with constrained options for survival or transition. High extraction with significant suppression of alternatives.
constraint_indexing:constraint_classification(g7_debt_trap, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEBT-FOR-CLIMATE COALITION (SCAFFOLD) — Organized actors (NGOs, progressive governments, multilateral development banks) are building alternative pathways: debt-for-nature swaps, climate finance without adjustment conditions, and regional development banks (BRICS, ADB) creating exits from G7 debt dependency. These represent sunset mechanisms — as alternatives mature, G7 trap extraction loses force. Suppression is declining as coalition builds organizing capacity.
constraint_indexing:constraint_classification(g7_debt_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: WASHINGTON CONSENSUS DOCTRINE (PITON) — The theoretical justification for structural adjustment (that liberalization and privatization maximize growth) is largely degraded: 40+ years of empirical work shows adjustment-conditional growth does not materialize at promised rates, inequality increases, and social outcomes worsen. Yet the doctrine persists through institutional inertia — creditors continue mandating adjustment because the intellectual infrastructure was built to justify it, not because the outcomes support it. Theater ratio high (policy rhetoric vs actual outcomes). Primary function (theoretical justification) atrophied; constraint maintained through ideological momentum.
constraint_indexing:constraint_classification(g7_debt_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — CAPITAL MARKET VIEW (FALSE SUMMIT) — From a universal/civilizational perspective, some debt servicing discipline is inherent to any lending system: creditors must have incentives to perform and borrowers must face consequences for default. The constraint appears immutable (natural law of finance). However, the structural data contradicts the mountain classification: the trap mechanism (conditionality that worsens borrower outcomes, suppression of exit options, enforcement through institutional coercion) is not an immutable feature of lending — it is a specific institutional design choice. Alternative lending models (concessional terms, debt forgiveness, technology transfer conditions) exist and have different extraction profiles. The 'natural law' framing naturalizes what is contingent policy.
constraint_indexing:constraint_classification(g7_debt_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(g7_debt_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(g7_debt_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(g7_debt_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(g7_debt_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(g7_debt_trap, TR),
    TR >= 0.70.

:- end_tests(g7_debt_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mechanism extracts through multiple channels: debt servicing transfers wealth from debtor to creditor, adjustment conditions eliminate domestic industries and reduce public employment (destroying domestic purchasing power and tax base), and policy conditionality transfers sovereignty to external institutions. However, extraction is not maximal (0.75+) because genuine development finance occurs — some loans do fund productive investment, and creditors have incentives to avoid outright collapse of debtors. The value of 0.58 reflects the middle ground between 'these are cooperative loans helping development' (too low) and 'these are pure predatory extraction' (too high). Suppression (0.72): High. Suppression operates through multiple gates: alternative financing sources are limited/expensive, exit from debt dependency risks loss of capital market access and potential sanctions, adjustment conditions are enforced through disbursement gates and IMF program reviews, and policy autonomy is constrained by conditionality requirements. Domestic alternatives are suppressed (privatization eliminates public sector solutions), and international alternatives are expensive (borrowing outside the system costs more). Theater ratio (0.55): Moderate. The constraint includes significant performative elements: adjustment conditions are justified by 'growth will result' rhetoric that 40+ years of data does not support, IMF reviews focus on compliance with criteria rather than actual development outcomes, and the Washington Consensus doctrine persists despite contradicted predictions. But the constraint is not purely theatrical — real wealth transfers occur, real policy autonomy is extracted, and real people experience real harm. The theater has increased over time (initial adjustment programs claimed to produce rapid growth; current programs acknowledge slower benefits, which indicates the theory has degraded but the practice persists).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and explains the paradox of 'why developing nations keep accepting these loans if they're harmful.' From the G7/IMF perspective, the constraint is coordination (standardized lending terms, technical support, market discipline) — they see Rope. From the debtor government perspective, the constraint is mixed (need capital but lose autonomy) — they see Tangled Rope. From the debtor population perspective, the constraint is pure extraction (bear all adjustment costs, have no exit, have no voice in terms) — they see Snare. From the domestic industry perspective, the constraint is elimination (forced exit from markets) — they also see Snare. From the alternative-funding coalition perspective, the constraint is temporary (sunset is real, alternatives are scaling) — they see Scaffold. From the theoretical doctrine perspective, the constraint is degraded ritual (Washington Consensus is contradicted by empirical outcomes but persists through institutional inertia) — they see Piton. From the analytical observer perspective, the constraint risks appearing immutable (finance always requires discipline) — the false summit risk is highest here. The perspectives are not equally valid — the powerlss/trapped perspective is more structurally accurate than the institutional/arbitrage perspective — but the framework models all six. This generates a perspectival diversity that explains why debtor governments accept the trap: they are in the Tangled Rope middle ground (not powerless, have some negotiating room) and see the constraint as mixed rather than pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position and exit capacity. G7 creditor governments are beneficiaries with arbitrage options (can shift lending focus, can renegotiate terms, can exit to other investment opportunities) — they experience low or negative d values. Developing nation governments are victims with constrained exits (need financing but options are limited) — they experience moderate d values (~0.55-0.65). Developing nation populations are victims with trapped status (no financing alternative, no policy voice, suffer direct adjustment harm) — they experience high d values (0.85+). Multinational corporations are beneficiaries with mobile/arbitrage options (can source from any liberalized market) — they experience low d values. The analytical observer at civilizational scope risks naturalizing the constraint as immutable (high d from a 'system must have some discipline' perspective), but the structural data shows the constraint is contingent on specific institutional choices (foreign-currency debt, unilateral conditionality, adjustment-first rather than development-first lending), not on inherent necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness > 0.70 threshold waived via structure analysis): The constraint classifies as Tangled Rope at base and Snare from debtor perspectives, which requires active enforcement (conditionality gates, disbursement withholding) and asymmetric extraction (beneficiaries = creditors/multinationals, victims = debtor populations/industries). These structural requirements are met: beneficiaries array is populated (g7_creditor_governments, multinational_corporations, structural_adjustment_contractors), victims array is populated (developing_nation_populations, domestic_industries, public_sector_employment), and requires_active_enforcement is true. The mandatrophy resolution is narrative: the constraint is classified as Tangled Rope because it provides genuine coordination value (access to financing, technical support, institutional frameworks) to debtor governments while simultaneously extracting asymmetrically from debtor populations. The extraction is not disguised — it is explicitly part of the adjustment program. The coordination is not extraction disguised as coordination — it is real (debtor governments do benefit from capital access and policy coordination frameworks). The constraint avoids false positive Rope (pure coordination) because the suppression and asymmetry are too high, and avoids false positive Snare (pure extraction) because the coordination function is genuine. Mandatrophy is resolved: this is correctly a Tangled Rope, and the six-perspective ensemble confirms it is not misclassified as either pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adjustment_outcome_causality,
    'Do structural adjustment conditions cause worse development outcomes, or do debtor nations'' pre-existing weaknesses produce both the need for loans and poor outcomes regardless of conditions?',
    'Longitudinal studies comparing debtor nations receiving identical adjustment conditions with different implementation vigor; propensity-score matching of loan recipients vs non-recipients with similar initial conditions; regression analysis of adjustment intensity vs growth/inequality controlling for selection effects',
    'If causality is confirmed (adjustment causes harm): constraint is pure extraction (Snare/Tangled Rope from debtor perspective). If causality is selection bias: constraint is coordination mechanism (Rope), and debtor nations bear inherent structural challenges that loans address imperfectly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adjustment_outcome_causality, empirical, 'Whether structural adjustment conditions cause worse development outcomes or reflect selection bias').

omega_variable(
    exit_option_availability,
    'How many credible alternative funding sources exist for developing nations? Can they actually exit G7/IMF dependency without catastrophic costs?',
    'Audit of regional development banks (BRICS, ADB, AfDB) lending terms, interest rates, and conditionality; comparison of exit costs (lost trade access, sanctions threat, currency collapse scenarios); documented cases of nations successfully exiting G7 debt without economic collapse',
    'If alternatives are few/costly: exit_options for debtor governments is ''trapped'' or ''constrained'', driving Snare classification. If alternatives are robust/affordable: exit_options is ''mobile'', changing classification to Tangled Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Whether credible alternative funding sources exist for developing nations to exit G7 dependency').

omega_variable(
    conditionality_enforcement_mechanism,
    'Does enforcement of adjustment conditions rely primarily on financial coercion (withholding disbursements) or on geopolitical/institutional pressure (threat of exclusion, sanctions, loss of diplomatic standing)?',
    'Historical record of IMF disbursement withholds vs threats of geopolitical consequences; debtor nation testimony on primary pressure vectors; analysis of countries that violated conditions without financial penalty but faced other consequences',
    'If primarily financial: suppression mechanism is high but transparent (Snare with clear extraction mechanism). If primarily geopolitical: suppression is opaque, constraint may be misclassified as negotiated (Rope) when actual enforcement is coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_enforcement_mechanism, empirical, 'Whether adjustment enforcement relies on financial coercion or geopolitical pressure').

omega_variable(
    debt_denominated_currency_trap,
    'To what extent do G7 debt traps persist because debts are denominated in foreign currency, forcing debtors to earn export revenue in volatile exchange rates while servicing in hard currency?',
    'Comparison of debt-in-local-currency vs debt-in-foreign-currency sustainability models; historical analysis of currency crises triggered by FX debt servicing needs; accounting for terms-of-trade volatility vs fixed debt obligations',
    'If currency trap is primary mechanism: constraint has a natural-law-like component (real structural asymmetry that no adjustment policy fully corrects). If currency trap is secondary: constraint is primarily institutional design (could be modified with local-currency lending).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_denominated_currency_trap, empirical, 'Whether foreign-currency debt denomination creates structural vulnerability independent of adjustment policy').

omega_variable(
    alternative_lending_model_effectiveness,
    'Do development outcomes improve under alternative lending models (Chinese infrastructure loans, regional bank concessional terms, debt-for-climate swaps) compared to G7-conditional lending?',
    'Controlled comparison of outcomes in nations receiving alternative-model loans vs G7-conditional loans, controlling for selection bias and initial conditions; long-term tracking of debt sustainability and development indicators',
    'If alternatives produce better outcomes: scaffold perspective confirmed — sunset is real and structural improvement is possible. If alternatives produce similar or worse outcomes: constraint is not institutional choice but inherent to any large external financing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_lending_model_effectiveness, empirical, 'Whether alternative lending models (Chinese, regional bank, climate-linked) produce better development outcomes than G7-conditional loans').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(g7_debt_trap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(g7debt_tr_t0, g7_debt_trap, theater_ratio, 0, 0.38).
narrative_ontology:measurement(g7debt_tr_t15, g7_debt_trap, theater_ratio, 15, 0.48).
narrative_ontology:measurement(g7debt_tr_t30, g7_debt_trap, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(g7debt_be_t0, g7_debt_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(g7debt_be_t15, g7_debt_trap, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(g7debt_be_t30, g7_debt_trap, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(g7_debt_trap, resource_allocation).
narrative_ontology:affects_constraint(g7_debt_trap, terms_of_trade_volatility).
narrative_ontology:affects_constraint(g7_debt_trap, foreign_currency_debt_asymmetry).
narrative_ontology:affects_constraint(g7_debt_trap, structural_adjustment_doctrine).
narrative_ontology:affects_constraint(g7_debt_trap, capital_flight_suppression).

% DUAL FORMULATION NOTE:
% The G7 debt trap is upstream of more specific financial constraints. The foreign_currency_debt_asymmetry story decompose the currency denomination mechanism (ε ≈ 0.12, Mountain). The structural_adjustment_doctrine story decomposes the theoretical justification mechanism (ε ≈ 0.42, Piton). The capital_flight_suppression story decomposes the exit-blocking mechanism (ε ≈ 0.55, Snare). The G7 debt trap integrates these into a single extraction architecture. The terms_of_trade_volatility story represents an upstream natural law constraint that amplifies the debt trap's severity. These are linked as a constraint family: debt trap affects all downstream mechanisms, and all provide structural support to the debt trap's extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(g7_debt_trap, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
