% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Gold-Fiat Transition: Creditor Discipline Elimination (Reserve Issuer Reading)
 *   domain: monetary_economics/political_economy/history
 *
 * SUMMARY:
 *   The gold-fiat transition (1944 Bretton Woods → 1971 Nixon Shock → ~1980
 *   stabilization in fiat regime) is read here as primarily a shift in
 *   creditor discipline: under the gold standard and Bretton Woods, nations
 *   running balance-of-payments deficits faced automatic pressure (reserve
 *   drain) forcing adjustment; creditor nations' accumulation of foreign
 *   exchange was backed by the threat of redemption. The transition to fiat
 *   eliminated redemption, eliminating the veto power creditors held through
 *   the threat of running out of gold. Reserve-currency issuer (primarily US)
 *   and deficit-running nations (debtors) gained fiscal flexibility;
 *   creditors (Germany, Japan, eventually China) found their reserve
 *   accumulation was no longer redeemable and their currency discipline
 *   replaced by inflation pressure. This is one of three competing readings
 *   of the same kernel transition.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer (US): gains seigniorage and fiscal autonomy, eliminates external discipline
 *   - debtor_nations (UK, France, developing countries): gain fiscal flexibility, constrained by inflation instead of reserve exhaustion
 *   - creditor_nations (Germany, Japan, China): lose redemption veto, trapped by forced reserve accumulation and currency appreciation pressure
 *   - working_class_savers (global): benefit from employment gains initially, harmed long-term by inflation erosion of real wages and purchasing power
 *   - financial_creditors (institutional investors, wealthy individuals): benefit from asset price inflation enabled by fiat regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.82).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.71).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, snare).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Gold-Fiat Transition: Creditor Discipline Elimination (Reserve Issuer Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '1ccda6ab-bce3-42ee-8bb3-ccf13403c048').
narrative_ontology:cs_kernel_codification('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', formalized).
narrative_ontology:cs_authority_grounding('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', extraction).
narrative_ontology:cs_interpretation_layer_present('1ccda6ab-bce3-42ee-8bb3-ccf13403c048').
narrative_ontology:cs_reading_relation('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', foundational, creditor_redemption_veto_enforces_discipline).
narrative_ontology:cs_axiom_status(creditor_redemption_veto_enforces_discipline, overridden).
narrative_ontology:cs_axiom_grounding('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', creditor_redemption_veto_enforces_discipline, empirically_contingent).
narrative_ontology:cs_axiom('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', foundational, reserve_issuer_fiscal_autonomy_contingent_on_fiat).
narrative_ontology:cs_axiom_status(reserve_issuer_fiscal_autonomy_contingent_on_fiat, holdable).
narrative_ontology:cs_axiom_grounding('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', reserve_issuer_fiscal_autonomy_contingent_on_fiat, deontological).
narrative_ontology:cs_reference_frame('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', gold_redeemable_discipline_framework).
narrative_ontology:cs_drift_state('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', post_1971_fiat_regime, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1ccda6ab-bce3-42ee-8bb3-ccf13403c048', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_currency_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, working_class_savers).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, financial_creditors).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, fiscal_authorities_reserve_issuer).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, working_class_savers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, central_banks_non_reserve).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, the dominant reserve-currency issuer post-WWII. Under gold standard, faced balance-of-payments discipline: deficits drained gold reserves, forcing contraction. Transition to fiat eliminated this external constraint, enabling persistent deficits and monetary expansion without redemption risk. Sets the value and terms of reserve currency; enforces its acceptance through network effects, military power, institutional arrangements, and denomination of global commodity prices.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).

% Nations running persistent payment deficits (UK post-WWII, US increasingly from 1960s, many developing nations). Under gold standard, faced hard budget constraints: deficits triggered reserve drain and forced austerity. Fiat transition, linked to dollar reserve status, enabled deficit financing through currency issuance. Gained fiscal flexibility at the cost of inflation risk and currency depreciation pressure.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    powerful, generational, constrained, global).

% Nations running trade surpluses (Germany post-reconstruction, Japan, later China). Under gold standard, creditor status conferred veto power: debtor nations' deficits translated to creditor reserve accumulation backed by redemption. Transition eliminated redemption option; creditor reserves became trapped paper. Forced choice: accept currency depreciation of reserves through inflation, revalue currency (losing export competitiveness), or continue accumulation (asset concentration). Bear the cost of financing debtor deficits.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, generational, trapped, global).

% Nations, central banks, corporations, individuals holding non-reserve currencies (sterling, DM, yen, euros). Under gold standard, all currencies had parity; value was anchored. Fiat transition subjected non-reserve holders to the monetary policy of the reserve issuer: if reserve currency inflates, non-reserve holdings lose value. Must hold some foreign reserves for trade settlement; exit options limited by network effects and transaction costs.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_currency_holders, payer,
    moderate, biographical, constrained, global).

% Individual wage-earners and savers. Benefited initially from fiscal expansion and employment gains. Long-term payers: inflation erodes real wages, purchasing power, and savings denominated in fiat currency. Cannot exit into alternatives (most do not hold gold, equities, or foreign assets); forced to hold nominal wages and savings in depreciating medium. Identity-locked: economic identity constituted through fiat-wage employment.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, working_class_savers, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, working_class_savers, beneficiary).

% Institutional investors, banks, wealthy individuals. Benefited substantially from fiat regime: enabled asset price inflation (equities, real estate, bonds), which concentrated wealth upward. Could arbitrage between currencies, assets, and jurisdictions; could hedge inflation through real asset ownership. Gained substantially relative to non-sophisticated savers.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, financial_creditors, beneficiary,
    institutional, biographical, arbitrage, global).

% Central banks of non-reserve nations (Germany, Japan, etc.). Gained nominal monetary autonomy (no longer constrained by gold redemption). In practice, became trapped: managing national currency value against reserve currency became perpetual discipline (inflation targeting, reserve adequacy ratios, capital-flow management) replacing one constraint with another. Unable to fully escape the constraint without economic isolation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, central_banks_non_reserve, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, central_banks_non_reserve, agenda_setter).

% Government finance ministries and legislatures of reserve-currency nations (primarily US). Eliminated the external balance-of-payments discipline on fiscal deficits. Gained policy space for sustained deficits, counter-cyclical spending, and entitlements without immediate currency crisis. Constraint shifted from external (reserve exhaustion) to internal (inflation, debt-to-GDP) and market-based (interest-rate expectations).
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, fiscal_authorities_reserve_issuer, beneficiary,
    institutional, generational, arbitrage, national).

% Institutional designers of mid-20th-century monetary order (IMF, World Bank, fixed-rate system). Attempted to maintain gold discipline while enabling monetary flexibility through institutional mechanisms. Observed the system's failure: the attempted compromise proved structurally unsustainable once capital mobility increased and deficits accumulated.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, bretton_woods_architects, observer,
    institutional, generational, analytical, global).

% Unions, worker organizations, labor advocates globally. Would argue fiat regime enabled capital mobility and wage arbitrage: deficit spending allowed corporations to offshore production while maintaining demand through devaluation, but real wages stagnated as inflation eroded gains. Excluded from constraint-setting; their interests (full employment, real-wage growth) diverge from reserve issuer and financial creditor interests.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, global_labor_movements, excluded,
    organized, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold-standard system theoretically coordinates global monetary settlement and price stability by enforcing discipline: a creditor nation's threat of redemption forces debtors to adjust, and all participants are anchored to a common physical reserve. The system prevents monetary excess and maintains exchange-rate stability through the redemption mechanism. The creditor-discipline reading emphasizes that this coordination worked through asymmetric power: creditors had veto (redemption threat), debtors faced automatic pressure (reserve drain).
% TRANSFER_FUNCTION: Moves seigniorage gains and geopolitical leverage from creditor nations and non-reserve-currency holders to the reserve-currency issuer and debtor nations. Creditor reserves are transferred to debtors (through reserve inflation erosion and currency depreciation) and to the reserve issuer (through seigniorage — the ability to create currency at zero cost and spend it). Non-reserve-currency savers transfer real purchasing power to holders of real assets and reserve currency.
% ABSENT_VOICES: Global labor movements and small-nation central banks would contest the constraint's elimination: they would argue fiat regime enabled capital flight from poor nations, wage arbitrage, and monetary instability that harmed workers and developing-nation policymakers. Developing-nation central banks excluded from Bretton Woods design had limited voice in constraint modification. Alternative monetary visions (multi-reserve systems, gold-backed currencies, regional integration) were suppressed by the constraint's institutional enforcement.
% DISAPPEARANCE_RATIONALE: If the fiat transition reversed and gold redemption were restored, the entire post-1971 international monetary architecture would collapse. Debtor-nation fiscal expansion would be reversed through automatic reserve-drain discipline. Creditor nations would regain veto power and enforce adjustment through redemption threats. The decades of deficit-spending by reserve issuers and some debtors would be unwound through austerity; central-bank autonomy would be constrained by gold holdings. Global power distribution would shift back from reserve-currency issuer toward creditor nations. The constraint's disappearance would reorganize the entire geopolitical economy.
% FOUNDING_PROBLEM: The Bretton Woods compromise attempted to maintain gold discipline on debtors while enabling greater monetary flexibility through institutional mechanisms (IMF loans, fixed-rate targets, capital controls). It failed when US deficits accumulated faster than the system could absorb, capital became more mobile, and the constraint became structurally unsustainable. The founding problem: how to maintain price stability and fiscal discipline while allowing sufficient monetary expansion for growth and full employment?
% FOUNDING_PROBLEM_CORROBORATION: Economists across the political spectrum (Milton Friedman, post-Keynesian economists, development economists, economic historians including Barry Eichengreen, Benn Steil, and Niall Ferguson) corroborate that the gold standard created real deflationary discipline and constrained growth. The Triffin dilemma (identified by Robert Triffin in 1960, widely corroborated by subsequent analysis) documented the structural incompatibility of a national currency serving as global reserve while being redeemable in gold. Academic consensus: Bretton Woods failed not because the founding problem persisted but because the compromise was unsustainable. The problem — disciplining deficits — was genuinely solved by abandoning gold and moving to fiat plus institutional cooperation. Yet the constraint (fiat regime with concentrated reserve power) persists, not because the founding problem requires it, but because institutional and power arrangements maintain it.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 by 2000) because the transition fundamentally redistributes wealth from creditor nations and non-reserve-currency savers to the reserve issuer and financial assets holders. The constraint is actively enforced (suppression = 0.71): maintaining fiat's legitimacy requires suppressing alternative stores of value, capital controls preventing reserve flight, and institutional arrangements keeping reserve currency in demand. Theater ratio is moderate (0.28): the IMF and central bank frameworks maintain the appearance of monetary cooperation and discipline, but the primary mechanism (creditor veto elimination) is the reality underneath. Accessibility collapse is moderate (0.48) because alternatives exist at each level — countries can dollarize away, savers can hold gold or commodities, central banks can cooperate on currency baskets — but these alternatives face high institutional barriers and network-effects costs. Resistance is substantial (0.73): creditor nations resist the arrangement through mercantilist policies, capital controls, and demands for reserve-diversification; savers resist through inflation hedging; this resistance has shaped the entire post-1971 institutional debate.
 *
 * PERSPECTIVAL GAP:
 *   The reserve-issuer and debtor-nation seats should compute this constraint as enabling beneficial coordination and fiscal flexibility — a Rope or Scaffold, not a Snare. The beneficiaries experience escape from external discipline. From the creditor and non-reserve-holder seats, the same structure operates as extraction without alternatives — forced reserve accumulation, currency depreciation, suppression of wage growth. The engine computes per-seat classification from structural data: reserve issuer has arbitrage-grade exit (can choose to lose reserve status but at enormous cost), high power, generational time horizon → low directionality → low/negative extraction. Creditors have trapped exit (cannot avoid accumulating reserves if running surpluses), high power but constrained by currency markets, generational time horizon → high directionality → high extraction. Working-class savers are powerless, identity-locked into holding fiat wages, biographical time horizon → maximum directionality → maximum extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Reserve issuer: Full beneficiary (d ≈ 0.05). Controls the constraint, gains seigniorage, escapes discipline, sets terms for others. Exit is arbitrage-grade: could lose reserve status by mismanaging, but that exit is so costly and slow that it is not a real constraint on behavior. Debtor nations: Beneficiary (d ≈ 0.35). Gain fiscal flexibility but remain constrained by inflation and currency depreciation. Exit options are constrained: could attempt to dollarize or peg to alternative reserve, but network effects and institutional barriers make this costly. Creditor nations: Full target (d ≈ 0.95). Forced to accumulate reserves, cannot redeem them, forced to choose between reserve loss (revaluation), or reserve inflation-erosion (holding). Exit is trapped: cannot avoid surplus accumulation without trade contraction, cannot exit reserves without losing competitive advantage. Non-reserve-currency holders: Target (d ≈ 0.85). Forced to hold or transact in reserve currency, subject to its inflation. Exit is constrained: could try alternative reserves or commodities, but barriers are high. Working-class savers: Target (d ≈ 1.0). Powerless, identity-locked into fiat-wage earnings, no real exit except into real estate (requiring capital). Subject to inflation erosion of real purchasing power without compensation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem (how to maintain gold discipline while enabling monetary expansion) is demonstrably dead: no serious economist or policymaker believes the gold standard as redesigned in Bretton Woods could be restored, and the IMF's entire institutional structure is predicated on fiat. Yet the constraint persists: decades after the founding problem died, the fiat system remains. This is classic mandatrophy: the founding problem is gone (solved by fiat transition), but the constraint (fiat regime itself, with its concentration of seigniorage and power) persists because beneficiaries (reserve issuer, financial creditors) maintain it and because alternatives (return to gold, global multi-reserve system) face enormous coordination barriers. The theater ratio is low-to-moderate because central banks and the IMF maintain the appearance of monetary cooperation and stability, performing the function of 'discipline' even though the real discipline is gone. This reading deliberately skips the claim-metric reconciliation: it claims Snare while noting that reserve issuers and some debtors might genuinely experience it as Rope (beneficial coordination). That divergence is what per-seat classification measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creditor_discipline_intensity,
    'Under the gold standard, how binding was the creditor discipline mechanism? Did balance-of-payments deficits force adjustment through automatic reserve drain (gold loss), or could governments maintain deficits through institutional cooperation, capital controls, and forward purchases?',
    'Historical analysis of pre-1971 balance-of-payments crises (Suez 1956, pound devaluation 1967, etc.): Did they stem from automatic reserve exhaustion or from political choices and institutional failures to manage cooperation?',
    'If discipline was automatic and binding, the transition is a major power redistribution. If discipline was often bypassed through cooperation, the transition represents a shift in the TERMS of cooperation rather than elimination of constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_discipline_intensity, empirical, 'Degree to which gold-standard discipline was automatic versus avoidable through institutional cooperation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1944, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement_basis(gold_tr_t1944, observed).
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement_basis(gold_tr_t1960, observed).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.18).
narrative_ontology:measurement_basis(gold_tr_t1971, observed).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement_basis(gold_tr_t1980, observed).
narrative_ontology:measurement(gold_tr_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1990, 0.27).
narrative_ontology:measurement_basis(gold_tr_t1990, observed).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(gold_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement_basis(gold_be_t1944, observed).
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1960, 0.31).
narrative_ontology:measurement_basis(gold_be_t1960, observed).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement_basis(gold_be_t1971, observed).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1980, 0.79).
narrative_ontology:measurement_basis(gold_be_t1980, observed).
narrative_ontology:measurement(gold_be_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1990, 0.81).
narrative_ontology:measurement_basis(gold_be_t1990, observed).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement_basis(gold_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement_basis(gold_su_t1944, observed).
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement_basis(gold_su_t1960, observed).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.62).
narrative_ontology:measurement_basis(gold_su_t1971, observed).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement_basis(gold_su_t1980, observed).
narrative_ontology:measurement(gold_su_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement_basis(gold_su_t1990, observed).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement_basis(gold_su_t2000, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=2000
narrative_ontology:measurement(gold_grid_01, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(class), 1944, 0.72).
narrative_ontology:measurement(gold_grid_02, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(class), 2000, 0.48).
narrative_ontology:measurement(gold_grid_03, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(individual), 1944, 0.62).
narrative_ontology:measurement(gold_grid_04, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(individual), 2000, 0.35).
narrative_ontology:measurement(gold_grid_05, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(organizational), 1944, 0.88).
narrative_ontology:measurement(gold_grid_06, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(organizational), 2000, 0.52).
narrative_ontology:measurement(gold_grid_07, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(structural), 1944, 0.85).
narrative_ontology:measurement(gold_grid_08, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(structural), 2000, 0.38).
narrative_ontology:measurement(gold_grid_09, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(class), 1944, 0.35).
narrative_ontology:measurement(gold_grid_10, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(class), 2000, 0.72).
narrative_ontology:measurement(gold_grid_11, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(individual), 1944, 0.28).
narrative_ontology:measurement(gold_grid_12, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(individual), 2000, 0.65).
narrative_ontology:measurement(gold_grid_13, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(organizational), 1944, 0.22).
narrative_ontology:measurement(gold_grid_14, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(organizational), 2000, 0.75).
narrative_ontology:measurement(gold_grid_15, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(structural), 1944, 0.18).
narrative_ontology:measurement(gold_grid_16, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(structural), 2000, 0.68).
narrative_ontology:measurement(gold_grid_17, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(class), 1944, 0.38).
narrative_ontology:measurement(gold_grid_18, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(class), 2000, 0.71).
narrative_ontology:measurement(gold_grid_19, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(individual), 1944, 0.28).
narrative_ontology:measurement(gold_grid_20, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(individual), 2000, 0.65).
narrative_ontology:measurement(gold_grid_21, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(organizational), 1944, 0.52).
narrative_ontology:measurement(gold_grid_22, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(organizational), 2000, 0.81).
narrative_ontology:measurement(gold_grid_23, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(structural), 1944, 0.45).
narrative_ontology:measurement(gold_grid_24, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(structural), 2000, 0.78).
narrative_ontology:measurement(gold_grid_25, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(class), 1944, 0.42).
narrative_ontology:measurement(gold_grid_26, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(class), 2000, 0.71).
narrative_ontology:measurement(gold_grid_27, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(individual), 1944, 0.28).
narrative_ontology:measurement(gold_grid_28, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(individual), 2000, 0.58).
narrative_ontology:measurement(gold_grid_29, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(organizational), 1944, 0.35).
narrative_ontology:measurement(gold_grid_30, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(organizational), 2000, 0.74).
narrative_ontology:measurement(gold_grid_31, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(structural), 1944, 0.25).
narrative_ontology:measurement(gold_grid_32, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(structural), 2000, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, bretton_woods_institutional_design).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_seigniorage_extraction).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nation_trap_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a triadic kernel (gold_fiat_transition_mechanism). The automatic_constraint_reading frames the transition as a shift from material to institutional constraint; the composite_overdetermination_reading emphasizes multiple independent drivers (technology, labor, geopolitics) converging. This reading emphasizes power redistribution through elimination of creditor veto. All three affect downstream constraints: reserve-currency seigniorage extraction, creditor-nation trap dynamics, institutional legitimacy of fiat, and contemporary debates over multi-reserve systems. The three readings do not foreclose each other; they coexist as different interpretations held by different analytical communities (monetary economists, historians, political economists).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
