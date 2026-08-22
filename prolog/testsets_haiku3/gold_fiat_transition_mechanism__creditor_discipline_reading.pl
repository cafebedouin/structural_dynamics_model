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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Gold Redemption Threat Elimination (Creditor Discipline Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   Under Bretton Woods (1944–1971), the US dollar was pegged to gold at
 *   $35/oz; other currencies pegged to the dollar; gold could be redeemed at
 *   a fixed rate. This created automatic discipline: creditor nations
 *   accumulating dollars could demand gold, limiting reserve-issuer deficits.
 *   The transition to fiat (accelerating 1968–1973, crystallized by Nixon
 *   Shock 1971) eliminated this external constraint. The reserve issuer (US)
 *   could now create fiat claims on itself without physical redemption
 *   obligation. This reading frames the transition as a redistribution of
 *   veto power: from creditor nations (who could discipline through
 *   redemption threats) to the reserve-currency issuer (who could now run
 *   unconstrained deficits). The reading does NOT claim the transition solved
 *   coordination problems or eliminated physical constraints (those are the
 *   automatic_constraint and composite_overdetermination readings). Instead,
 *   it asserts that a real coercive mechanism—creditor veto—was abolished,
 *   transferring power from creditors to debtors and, especially, to the
 *   reserve issuer. Extraction is high because the new arrangement allows
 *   reserve issuers to monetize deficits and export inflation; suppression is
 *   substantial because maintaining the fiat standard requires central banks
 *   to actively prevent return to commodity backing and to police capital
 *   flows.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer (US Federal Reserve, US Treasury): sets the fiat standard, eliminates redemption discipline, enables deficit spending
 *   - debtor_nations (most developing and middle-income countries): gain fiscal flexibility but remain constrained by currency risk; non-reserve debtors lose leverage
 *   - creditor_nations (historically Germany, Japan; more recently China): lose veto power, must accept infinite reserve creation without redemption threat
 *   - non_reserve_currency_holders (central banks of non-reserve issuers): trapped in the architecture, facing currency depreciation risk
 *   - private creditors (institutional investors, bond holders): benefit from asset inflation and unlimited debt issuance but exposed to depreciation risk
 *   - labor and wage earners (globally, especially in non-reserve issuers): bear inflation cost without asset-appreciation benefit; real purchasing power eroded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.65).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Gold Redemption Threat Elimination (Creditor Discipline Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '1aebeaab-2f52-410e-bb84-3b48744aa24a').
narrative_ontology:cs_kernel_codification('1aebeaab-2f52-410e-bb84-3b48744aa24a', formalized).
narrative_ontology:cs_authority_grounding('1aebeaab-2f52-410e-bb84-3b48744aa24a', extraction).
narrative_ontology:cs_interpretation_layer_present('1aebeaab-2f52-410e-bb84-3b48744aa24a').
narrative_ontology:cs_reading_relation('1aebeaab-2f52-410e-bb84-3b48744aa24a', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('1aebeaab-2f52-410e-bb84-3b48744aa24a', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('1aebeaab-2f52-410e-bb84-3b48744aa24a', foundational, creditor_veto_power_eliminated).
narrative_ontology:cs_axiom_status(creditor_veto_power_eliminated, holdable).
narrative_ontology:cs_axiom_grounding('1aebeaab-2f52-410e-bb84-3b48744aa24a', creditor_veto_power_eliminated, empirically_contingent).
narrative_ontology:cs_axiom('1aebeaab-2f52-410e-bb84-3b48744aa24a', foundational, reserve_issuer_fiscal_autonomy_established).
narrative_ontology:cs_axiom_status(reserve_issuer_fiscal_autonomy_established, holdable).
narrative_ontology:cs_axiom_grounding('1aebeaab-2f52-410e-bb84-3b48744aa24a', reserve_issuer_fiscal_autonomy_established, deontological).
narrative_ontology:cs_reference_frame('1aebeaab-2f52-410e-bb84-3b48744aa24a', gold_redemption_discipline).
narrative_ontology:cs_drift_state('1aebeaab-2f52-410e-bb84-3b48744aa24a', fiat_standard_crystallization, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1aebeaab-2f52-410e-bb84-3b48744aa24a', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_currency_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, private_creditors_institutional).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, asset_holders_rentiers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, private_creditors_institutional).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, labor_and_wage_earners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The nation whose currency serves as the global reserve (the United States post-1971). Under Bretton Woods, faced discipline via gold redemption threat: creditor nations could demand physical gold for dollars at a fixed rate, constraining money supply and fiscal spending. The transition eliminated this external constraint, granting the reserve issuer the power to create fiat claims on itself without redemption obligation. Sets monetary policy and fiscal parameters without external veto; enforces the discipline-elimination through institutional commitment to fiat-standard central banking and capital-market dominance.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Nations running persistent current-account or fiscal deficits (including but not limited to the reserve issuer). Under Bretton Woods, faced discipline through gold-reserve depletion and redemption threats; balance-of-payments deficits forced fiscal contraction or devaluation. The transition to fiat eliminated the automatic physical limit on deficit spending for reserve issuers and reduced it for others. Debtor nations gained fiscal flexibility to run counter-cyclical policy and accumulate debt in their own currency, but non-reserve debtors remain constrained by currency risk and creditor-nation capital flows.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    institutional, civilizational, constrained, global).

% Nations running persistent current-account surpluses and accumulating foreign reserves (historically Germany, Japan, oil-producing states, China in the post-Bretton Woods era). Under the gold standard and Bretton Woods, possessed explicit veto power: the threat to demand gold redemption or abandon pegged rates disciplined debtor-nation spending. The transition eliminated this veto mechanism for debtor-nation fiscal policy and reserve issuance; creditor leverage shifted from redemption rights to capital-flow management and trade negotiations. Modern creditors face unconstrained reserve-currency creation they cannot prevent.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, civilizational, constrained, global).

% Central banks and nations holding foreign reserves in non-reserve currencies or lacking credible domestic fiat institutions. Cannot issue globally-accepted fiat; must accumulate reserves in reserve currencies (dollars, euros, yen) or gold to defend their own currency pegs or finance deficits. Bear the currency risk of reserve-issuer inflation and are subject to balance-of-payments discipline through currency depreciation and capital flight when they run deficits. Locked into the architecture by institutional path dependence and capital-market dollarization.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_currency_holders, payer,
    moderate, generational, identity_locked, global).

% Global financial institutions, banks, and bond holders. Gained the ability to hold and trade unlimited fiat debt claims (bonds, deposits) without redemption risk in reserve currencies; simultaneously face inflation risk and currency depreciation risk. Early beneficiaries of asset inflation driven by monetary expansion; later subject to policy rate swings and real-return compression. Can arbitrage across currencies and assets but are exposed to reserve-issuer policy shifts.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, private_creditors_institutional, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, private_creditors_institutional, payer).

% The institutional framework (IMF, pegged exchange rates, gold-reserve audits) that mediated Bretton Woods discipline. Was systematically dismantled by the transition; creditor nations would have preferred its continuation but were excluded from the redesign process. Represents the loss of governance mechanism that constrained reserve-issuer unilateralism.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, bretton_woods_authority_structure, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(gold_fiat_transition_mechanism__creditor_discipline_reading, bretton_woods_authority_structure).

% The coordinating body of monetary authorities (especially the US Federal Reserve, but also collaborating central banks). Enforces the fiat standard through institutional commitment to non-convertibility, defense of reserve currencies, and coordination on capital flows. Prevents return to gold-based or commodity-backed discipline by maintaining policy independence and accepting inflation as the cost of fiat flexibility.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, central_banks_collectively, agenda_setter,
    institutional, generational, mobile, global).

% Workers in all nations, especially non-reserve issuers. Bear the inflation cost of unconstrained fiat creation without the benefit of asset appreciation. Real wages compressed by monetary expansion benefiting asset holders; labor bargaining power eroded as macro instability and off-shoring weakened union leverage. Trapped in currencies they cannot exit and employment markets where purchasing power is systematically eroded.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, labor_and_wage_earners, payer,
    powerless, biographical, trapped, global).

% Concentrated wealth holders, corporations, and institutional investors positioned to arbitrage across currencies, commodities, and financial assets. Benefit from the inflation inherent in fiat creation (assets rise nominally), from unconstrained debt issuance (borrowing becomes cheaper), and from capital-market liberalization. Can diversify globally; early advantages in asset accumulation translate to later inflation hedges.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, asset_holders_rentiers, beneficiary,
    powerful, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable global settlement medium (the dollar) that enables international trade, capital flows, cross-border lending, and emergency lending without the computational burden of multi-currency conversion or the instability of commodity-price-driven exchange rates.
% TRANSFER_FUNCTION: Transfers fiscal flexibility and seigniorage benefits from creditor nations (and non-reserve holders) to the reserve-currency issuer; simultaneously transfers inflation risk and currency depreciation risk to creditor and non-reserve nations and to unhedged wage earners globally.
% ABSENT_VOICES: Gold-standard advocates, commodity-backed-fiat proponents, and labor representatives in non-reserve issuers are excluded from central-bank governance; they would object to unlimited fiat creation and the resulting inflation. Creditor-nation central bankers participate in governance forums (BIS, Basel Accords) but lack veto power over reserve-issuer monetary policy.
% DISAPPEARANCE_RATIONALE: If the fiat-standard constraint vanished and redemption discipline returned (gold standard or hard-peg regime), reserve issuers would face immediate balance-of-payments limits; deficits would force fiscal contraction or currency devaluation; cross-border capital positions (currently denominated in fiat dollars) would reorganize around physical scarcity; asset prices would collapse as deflation replaced inflation expectations; labor costs would adjust downward in nominal terms; international trade would become more volatile as exchange rates floated or re-pegged. The entire accumulation structure built on unlimited fiat creation since 1971 would reverse.
% FOUNDING_PROBLEM: Post-WWII instability: competitive devaluations during 1930s/40s, capital flight, and lack of a trusted settlement medium threatened international trade. Bretton Woods aimed to provide stable pegged rates and gold-convertible reserves to restore trade and development.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians and economists outside the reserve-issuer establishment (European, Japanese, and emerging-market central banks; heterodox economists like Minsky, Graeber, Hudson) document that the founding problem of post-WWII instability was solved by Bretton Woods for 25 years but the mechanism itself became incompatible with full capital mobility and fiscal autonomy by the late 1960s (the Triffin dilemma: reserve currency must be both a safe store of value and infinitely supplied to finance world trade—impossible under commodity backing). Reserve-issuer apologists describe the problem as solved and hence moot; creditor nations and labor advocates describe a NEW stability problem (fiat-standard inflation, asset bubbles, wage stagnation) that replaced the old one.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness reaches 0.78 by 2024 because the constraint enables unlimited fiat creation without redemption, allowing the reserve issuer to monetize deficits and transfer inflation to creditor and non-reserve nations. The series shows sharp acceleration 1960–1973 (the 'Triffin dilemma' decade when redemption pressures mounted and extraction became visible) and stabilization post-1973 once fiat was institutionalized. Suppression is 0.65 at endpoint and moderately high throughout (0.35 at baseline, rising to 0.64 by 1973) because maintaining the fiat standard requires constant enforcement: central banks must prevent capital flight into gold or foreign currency, defend the reserve currency's purchasing power, and police any signals of return to commodity backing. Theater is low (0.22) because the coordination function—global medium of exchange—is genuine and continues to be necessary; the extractive mechanism (elimination of redemption discipline) is not performed theatrically but operationalized in policy. Accessibility collapse is asymmetric across levels: structural (0.88 at endpoint) because the entire international financial architecture is locked into fiat settlement; organizational (0.52) because central banks could theoretically coordinate a return to gold or a multi-currency reserve system, but the path-dependent costs are prohibitive; class and individual (0.41, 0.18) because workers in non-reserve nations have almost no exit option—they cannot opt out of the currency or trade system—while individuals in reserve-issuer nations have arbitrage options (hold foreign currency, commodities, gold). Stakes inflation is extremely high at individual level (0.72) in non-reserve nations because workers bear the full purchasing-power erosion without compensation; lower at organizational level (0.65) because central banks can partially sterilize inflation or raise rates. Resistance is high across all levels (0.58–0.75) because the constraint is contested: creditor nations resent the veto-elimination, labor organizations in non-reserves oppose inflation, gold-standard advocates continue to argue for commodity backing, and China/Russia periodically signal interest in alternatives (though institutional lock-in prevents defection).
 *
 * PERSPECTIVAL GAP:
 *   From the reserve-issuer perspective (US), the constraint appears as a transition from external discipline to self-determined monetary policy—a gain in autonomy framed as necessary for macroeconomic flexibility and crisis response. The 2008 and 2020 experiences (unlimited QE in the US; Japanese and ECB expansion) are presented as prudent crisis management. From the creditor-nation perspective (Germany, Japan, and now China), the same constraint is a loss of leverage and a mechanism for exporting inflation—creditors accumulate nominally increasing but real-depreciating reserves as the reserve issuer monetizes deficits. From the labor perspective in non-reserves, the constraint is pure extraction: wages stagnate while the currency depreciates and capital flees. The engine computes per-seat directionalities: reserve issuer at d ≈ 0.1 (full beneficiary), creditor nations at d ≈ 0.9 (full targets), debtor non-reserves at d ≈ 0.75 (high targets but with some fiscal flexibility), labor at d ≈ 1.0 (total targets with no exit). These divergences follow from the structural data (beneficiary vs. victim declarations + exit options).
 *
 * DIRECTIONALITY LOGIC:
 *   Reserve-currency issuer: declared as beneficiary because it collects the power to create infinite fiat and transfer inflation globally. Exit options = arbitrage (can always return to gold or multi-currency system at perceived high cost, but available as a threat against other debtors, not as realistic exit for the issuer itself—hence arbitrage, not trapped). Power = institutional (controls the global reserve mechanism). Directionality d ≈ 0.1 (near full beneficiary). Creditor nations: declared as victims because they lose the veto power that constrained reserve-issuer deficits. Exit options = constrained (can threaten capital flight or demand gold, but cannot actually leave the dollar system without massive disruption to their own capital positions; Japan and Germany tried to reduce dollar dependence in the 1980s but found no viable alternative). Power = powerful (large economies with substantial capital positions) but trapped by path dependence. Directionality d ≈ 0.9 (near full target). Debtor non-reserves: declared as beneficiary (fiscal flexibility to run counter-cyclical policy) and payer (currency depreciation, inflation export). Exit options = constrained (cannot issue globally-accepted reserve currency, cannot revert to commodity backing without massive macroeconomic disruption). Power = moderate to powerful (large emerging markets like India and Brazil have some negotiating leverage, but smaller debtors have none). Directionality d ≈ 0.65 (high target but with some benefits). Labor in non-reserves: declared as payer (bear inflation and real-wage erosion). Exit options = trapped (cannot exit the currency system, cannot arbitrage into other assets without capital). Power = powerless (unorganized globally; unions weakened by offshoring and monetary instability). Directionality d ≈ 1.0 (total target). No directionality overrides needed; the canonical derivation from exit_options + power captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not show mandatrophy. The founding problem (post-WWII instability, need for stable exchange rates and settlement mechanism) is DEAD—Bretton Woods solved it for 27 years, but the constraint itself (gold redemption and pegged rates) became incompatible with full capital mobility and national fiscal autonomy. When the US abandoned gold convertibility in 1971, the original mandate ceased to bind. The new constraint (fiat standard) has a different mandate: enable unlimited fiat creation and reserve issuance to maintain financial stability and smooth deficits. This mandate is LIVE and is actively being fulfilled—every central bank expansion, every QE program, every deficit-financed fiscal stimulus relies on the fiat standard. The constraint is not a zombie; it is a redesigned mechanism with a new mandate. The question is whether the NEW mandate (unlimited fiat creation) is legitimate, not whether the OLD mandate persists. The divergence between claimed_type (tangled_rope: coordination + extraction) and the engine's computation will turn on the balance of coordination benefit (real global settlement mechanism) vs. extractive cost (transfer of veto power and inflation distribution). The claim is deliberate: from the reserve-issuer seat, it is a tangled rope (coordination + benefit); from the creditor seat, it is a snare (pure extraction). The measurement series should resolve this divergence: if extraction rises monotonically and theater stays low, snare evidence is strong; if coordination benefits rise with extractiveness, tangled rope holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_power_actual_exercise,
    'How often and with what effectiveness did creditor nations actually exercise redemption veto under Bretton Woods, and how constraining was it to reserve-issuer fiscal policy?',
    'Historical analysis of Triffin dilemma literature, Federal Reserve archives (McNamara-Fowler correspondence on gold outflows), and econometric study of US fiscal policy 1950–1971 to detect veto-induced constraint.',
    'If redemption veto was rarely exercised and US deficits persisted anyway, the reading overstates creditor discipline and the transition is less of a power shift than asserted. If veto was frequently threatened and US policy was visibly constrained, extraction-elimination is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_power_actual_exercise, empirical, 'Whether creditor-nation veto was a real constraint or a latent threat.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the creditor_discipline reading foreclose the automatic_constraint reading, or can both be simultaneously true (the transition eliminated both a physical constraint AND a political-veto mechanism)?',
    'Conceptual: if creditor veto was a function of gold scarcity (veto power derived from physical limits), the readings coexist. If creditor veto was a political choice (creditors exercised or threatened redemption not because gold was scarce but because they possessed it and preferred to use it), the readings diverge but do not foreclose.',
    'Coexistence (composite mechanism) means both readings are valid perspectives on the same transition; foreclosure (one reading''s core premise contradicts the other) would require that physical constraint and political veto are mutually exclusive explanations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether physical constraint and political-veto readings of the transition are mutually exclusive or complementary.').

omega_variable(
    inflation_transfer_mechanism,
    'To what degree is post-1971 inflation in non-reserve currencies a direct result of reserve-issuer fiat creation (extraction via currency depreciation) vs. a result of oil shocks, labor power, and independent monetary expansion by other central banks?',
    'Time-series econometrics (VAR, impulse response) isolating the effect of US monetary expansion on foreign inflation, real exchange rates, and wage levels 1973–2024.',
    'If reserve-issuer monetization causes 60%+ of foreign inflation, the extraction reading is supported (creditor nations bear the cost of reserve-issuer deficits). If independent factors account for most inflation, extraction is less clear and coordination benefits are more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_transfer_mechanism, empirical, 'Whether non-reserve inflation is principally driven by reserve-issuer fiat creation or by independent factors.').

omega_variable(
    alternative_coordination_equilibria,
    'Would a multi-currency reserve system, commodity-backed fiat, or SDR-based reserve mechanism provide equivalent coordination benefits to the dollar standard while reducing extraction of non-reserves?',
    'Comparative institutional analysis (Mundell-Fleming, Trilemma literature) and case studies (Euro, gold standard, 1980s attempts at diversified reserves) examining whether the coordination function requires dollar dominance or is achievable with alternatives.',
    'If equivalently good equilibria exist but are not chosen, the constraint is a snare (extraction sustained by institutional lock-in and agenda-setter preference). If the dollar standard is Pareto-optimal for coordination, the tangled-rope reading is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_equilibria, conceptual, 'Whether the dollar standard is necessary for global financial coordination or whether extraction could be reduced via alternative arrangements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(gold_tr_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(gold_tr_t1973, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1985, 0.21).
narrative_ontology:measurement(gold_tr_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(gold_be_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(gold_be_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1968, 0.52).
narrative_ontology:measurement(gold_be_t1973, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1973, 0.71).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1985, 0.76).
narrative_ontology:measurement(gold_be_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2008, 0.79).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement(gold_su_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1968, 0.55).
narrative_ontology:measurement(gold_su_t1973, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1973, 0.64).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1985, 0.66).
narrative_ontology:measurement(gold_su_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2024, 0.65).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=2024
narrative_ontology:measurement(gold_grid_01, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(class), 1944, 0.38).
narrative_ontology:measurement(gold_grid_02, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(class), 2024, 0.41).
narrative_ontology:measurement(gold_grid_03, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(individual), 1944, 0.22).
narrative_ontology:measurement(gold_grid_04, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(individual), 2024, 0.18).
narrative_ontology:measurement(gold_grid_05, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(organizational), 1944, 0.45).
narrative_ontology:measurement(gold_grid_06, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(organizational), 2024, 0.52).
narrative_ontology:measurement(gold_grid_07, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(structural), 1944, 0.82).
narrative_ontology:measurement(gold_grid_08, gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse(structural), 2024, 0.88).
narrative_ontology:measurement(gold_grid_09, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(class), 1944, 0.38).
narrative_ontology:measurement(gold_grid_10, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(class), 2024, 0.75).
narrative_ontology:measurement(gold_grid_11, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(individual), 1944, 0.22).
narrative_ontology:measurement(gold_grid_12, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(individual), 2024, 0.58).
narrative_ontology:measurement(gold_grid_13, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(organizational), 1944, 0.42).
narrative_ontology:measurement(gold_grid_14, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(organizational), 2024, 0.68).
narrative_ontology:measurement(gold_grid_15, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(structural), 1944, 0.35).
narrative_ontology:measurement(gold_grid_16, gold_fiat_transition_mechanism__creditor_discipline_reading, resistance(structural), 2024, 0.72).
narrative_ontology:measurement(gold_grid_17, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(class), 1944, 0.05).
narrative_ontology:measurement(gold_grid_18, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(class), 2024, 0.58).
narrative_ontology:measurement(gold_grid_19, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(individual), 1944, 0.02).
narrative_ontology:measurement(gold_grid_20, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(individual), 2024, 0.72).
narrative_ontology:measurement(gold_grid_21, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(organizational), 1944, 0.08).
narrative_ontology:measurement(gold_grid_22, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(organizational), 2024, 0.65).
narrative_ontology:measurement(gold_grid_23, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(structural), 1944, 0.15).
narrative_ontology:measurement(gold_grid_24, gold_fiat_transition_mechanism__creditor_discipline_reading, stakes_inflation(structural), 2024, 0.78).
narrative_ontology:measurement(gold_grid_25, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(class), 1944, 0.12).
narrative_ontology:measurement(gold_grid_26, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(class), 2024, 0.44).
narrative_ontology:measurement(gold_grid_27, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(individual), 1944, 0.08).
narrative_ontology:measurement(gold_grid_28, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(individual), 2024, 0.68).
narrative_ontology:measurement(gold_grid_29, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(organizational), 1944, 0.15).
narrative_ontology:measurement(gold_grid_30, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(organizational), 2024, 0.48).
narrative_ontology:measurement(gold_grid_31, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(structural), 1944, 0.28).
narrative_ontology:measurement(gold_grid_32, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression(structural), 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.18).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The gold-fiat transition kernel instantiates three structurally distinct constraints via different readings: automatic_constraint (physical limitation replaced by discretionary authority), composite_overdetermination (convergence of multiple independent mechanisms), and creditor_discipline (political-veto elimination). Each has different epsilon, different victim/beneficiary, different type. The three readings are linked by network.affects_constraints: the creditor_discipline reading depends on the automatic_constraint reading (if no physical constraint existed, the veto was purely political; if the physical constraint was binding, veto was derivative). The composite_overdetermination reading is orthogonal to both (claims that no single reading fully explains the transition; multiple causes overdetermine the outcome).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
