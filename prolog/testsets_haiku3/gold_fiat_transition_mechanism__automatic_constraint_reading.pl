% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Fiat Money Discretionary Authority (Automatic Constraint Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   This constraint embodies the automatic-constraint reading of the
 *   gold-fiat transition: the shift from a material physical limit on money
 *   creation (gold reserves) to discretionary institutional authority
 *   (central bank policy) represented the replacement of one constraint type
 *   with another. The automatic reading emphasizes that gold provided an
 *   external, machine-like boundary that could not be violated without
 *   cascading crises; fiat money vests discretion in institutions, which
 *   operate under political pressure and are capable of debasement. This
 *   reading contests others that see the transition as driven by composite
 *   structural change (technology, labor power, Bretton Woods collapse)
 *   rather than a single swap of constraint types. The automatic reading
 *   focuses narrowly on the mechanism: what changed was not WHY institutions
 *   acted, but WHAT bounded their action. The reading is held by
 *   gold-standard advocates, creditor-nations' economists, and some policy
 *   observers who see the transition as a loss of discipline. The
 *   extractiveness metric reflects the reading's assessment: high extraction
 *   because the new institutional discretion is used to expand money beyond
 *   coordination costs, yielding seigniorage for authorities and creditor
 *   losses.
 *
 * KEY AGENTS:
 *   - monetary_authorities (institutional beneficiary, gained discretion)
 *   - creditor_class (powerful payer, lost automatic protection)
 *   - deficit_financing_states (institutional beneficiary, gained fiscal space)
 *   - fixed_income_earners (powerless payer, bore inflation costs)
 *   - reserve_currency_issuers (institutional beneficiary, gained geopolitical tax)
 *   - gold_standard_advocates (excluded, analytical position marginalized)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.78).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.62).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Fiat Money Discretionary Authority (Automatic Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '6e1a72f5-ee52-4c49-b6e8-eaee97d2f791').
narrative_ontology:cs_kernel_codification('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', fixed_text).
narrative_ontology:cs_authority_grounding('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', extraction).
narrative_ontology:cs_interpretation_layer_present('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791').
narrative_ontology:cs_reading_relation('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', foundational, monetary_supply_bound_by_commodity_reserve).
narrative_ontology:cs_axiom_status(monetary_supply_bound_by_commodity_reserve, overridden).
narrative_ontology:cs_axiom_grounding('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', monetary_supply_bound_by_commodity_reserve, empirically_contingent).
narrative_ontology:cs_axiom('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', foundational, automatic_constraint_preferable_to_discretionary_authority).
narrative_ontology:cs_axiom_status(automatic_constraint_preferable_to_discretionary_authority, holdable).
narrative_ontology:cs_axiom_grounding('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', automatic_constraint_preferable_to_discretionary_authority, deontological).
narrative_ontology:cs_reference_frame('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', gold_standard_monetary_constitution).
narrative_ontology:cs_drift_state('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', contemporary_fiat_regime, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('6e1a72f5-ee52-4c49-b6e8-eaee97d2f791', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, deficit_financing_states).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_earners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, foreign_reserve_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_unions).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, industrial_borrowers).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, reserve_currency_issuers).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, financial_intermediaries).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_unions).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_exporters).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, international_trading_partners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, commodity_producers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, financial_intermediaries).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, discretionary_monetary_policy_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, countercyclical_fiscal_capacity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks gained unilateral discretion to expand money supply beyond physical gold holdings. They justify this as enabling countercyclical stabilization and crisis response; they enforce it by controlling legal tender definitions and refusing gold convertibility. Their primary gain is policy flexibility unconstrained by external commodity reserves.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Fiscal authorities gained capacity to run sustained deficits by borrowing from their own central banks (or from markets confident in central bank accommodation). This enabled welfare-state expansion, military spending, and stimulus programs that would have been impossible under gold standard hard constraints. Their gain is unconstrained fiscal space.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, deficit_financing_states, beneficiary,
    institutional, generational, arbitrage, national).

% Lost the automatic protection of gold redemption threat (balance-of-payments discipline). Creditors who held government bonds, savings accounts, and fixed-rate instruments faced currency debasement they could not prevent. Under the gold standard, persistent deficits triggered crisis forcing fiscal correction; under fiat, monetary expansion could occur without that check. Their exit options narrowed: capital flight remained possible but faced currency controls in many regimes.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, biographical, constrained, global).

% Retirees, bondholders, and wage-earners on nominal incomes bore the cost of inflation generated by monetary expansion. The constraint's operation transferred real wealth from savers to debtors (governments and borrowers). Their only exit was asset-price hedging, which required financial sophistication most powerless actors lacked.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_earners, payer,
    powerless, biographical, constrained, national).

% Foreign governments and institutions holding the reserve currency (USD post-WWII) faced systematic devaluation as U.S. monetary expansion proceeded. The U.S. could export inflation to trading partners and reserve-holders; they could neither redeem in gold (after 1971) nor easily exit dollar dependence without facing severe transaction costs. This read as a geopolitical loss of discipline mechanism: the reserve-currency issuer gained the capacity to tax the rest of the world through seigniorage.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, foreign_reserve_holders, payer,
    powerful, biographical, constrained, global).

% Benefited from full-employment targeting and nominal wage growth enabled by accommodative monetary policy; bore costs when inflation eroded real wages faster than bargaining could recover them. Their position is structurally dual: the constraint enabled them to demand higher nominal wages in tight labor markets, but also allowed employers to erode real compensation through inflation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_unions, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_unions, payer).

% Large corporations and infrastructure projects gained access to long-term low-rate financing from accommodative central banks and from inflation-eroded real debt burdens. They could borrow in nominal terms and repay in depreciated currency. Their exit options were superior: they could shift to foreign markets or raise capital internationally.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, industrial_borrowers, beneficiary,
    powerful, biographical, mobile, national).

% Mining-dependent economies (South Africa, USSR, Canada) lost the automatic demand for gold as a monetary reserve backing. The transition eliminated their commodity's role as universally-demanded money-backing; gold became a speculative commodity subject to fiat-denominated pricing. Their terms of trade deteriorated as gold's strategic role diminished.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_exporters, payer,
    moderate, biographical, constrained, global).

% Exporting countries accumulated U.S. dollars as payment for goods, only to watch the dollars depreciate as U.S. monetary expansion proceeded. They could not demand gold (U.S. window closed 1971) and were trapped holding depreciating reserves. Their exclusion from decision-making about U.S. monetary policy is structural: they bore costs of decisions made unilaterally in Washington.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, international_trading_partners, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, international_trading_partners, excluded).

% Oil, agricultural, and mineral exporters faced volatile pricing in fiat terms, denominated in currencies subject to discretionary inflation. Under the gold standard, commodity prices had a nominal anchor; under fiat, they became subject to real-balance effects and monetary shocks originating far from commodity markets. Their hedging costs increased; their trade terms became more volatile.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, commodity_producers, payer,
    moderate, biographical, constrained, global).

% Monetarist and Austrian school economists argued the constraint provided essential discipline. Their institutional position was marginalized after 1971, their policy proposals rejected as reactionary. They remain excluded from central-bank decision-making about monetary rules, though they retain analytical voice in academic debate and policy commentary.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_standard_advocates, excluded,
    powerful, biographical, trapped, global).

% The United States (and to a lesser degree, other reserve-currency countries) gained the ability to run global deficits without facing gold redemption crises. Seigniorage (the profit from issuing currency accepted globally) became an ongoing transfer from the rest of the world. The constraint shift was geopolitical: reserve-currency status became more extractive, not less.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, reserve_currency_issuers, beneficiary,
    institutional, generational, arbitrage, global).

% Banks and financial firms gained arbitrage opportunities from inflation (borrowing at nominal rates, repaying in depreciated currency) but also faced inflation-driven yield-curve volatility and re-intermediation risk. Their net position shifted over the interval: early beneficiary (rising leverage, inflation gains), later payer (rising rates to combat inflation, asset-liability mismatches). A genuine dual-role story.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, financial_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, financial_intermediaries, payer).

% Economic theorists measure the constraint's effects: Triffin dilemma, monetary non-neutrality, inflation-unemployment tradeoffs, redistribution mechanisms. They produce evidence used by both sides of the gold-standard debate but remain outside the decision-making process that enforces the constraint.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, analytical_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The transition from automatic physical constraint (gold reserves) to discretionary institutional authority coordinated a global monetary system with faster adjustment capacity, reduced transaction costs from reserve-hoarding, and centralized crisis-response machinery. The coordination problem solved: how to enable flexible money supply without losing the discipline function of gold backing.
% TRANSFER_FUNCTION: Moves real wealth from creditors, savers, and fixed-income earners to debtors, borrowers, and monetary authorities (via seigniorage). Transfers policy discretion from external commodity constraint to internal institutional actors. Transfers geopolitical monetary power from creditor nations to reserve-currency-issuing nations.
% ABSENT_VOICES: Gold standard advocates were structurally excluded from post-1971 monetary decision-making; they would argue for commodity-backed constraints but were marginalized as 'reactionary.' Savers and creditors had no formal voice in central-bank policy choices that inflated away their purchasing power. Commodity-exporting and trading-partner nations were excluded from decisions about reserve-currency monetary policy that imposed costs on them.
% DISAPPEARANCE_RATIONALE: If discretionary fiat authority disappeared and gold-standard constraints were restored overnight, deficits would trigger immediate balance-of-payments crises forcing fiscal contraction, welfare-state spending would compress, monetary policy would lose countercyclical capacity, real interest rates would rise (savings would no longer face institutional inflation), and the geopolitical structure of reserve-currency dominance would collapse. The entire post-1971 fiscal and monetary order would reorganize.
% FOUNDING_PROBLEM: The gold standard produced deflationary rigidity during depressions and created external constraints on national policy during balance-of-payments crises. The Bretton Woods hybrid (gold peg with IMF adjustability) aimed to preserve discipline while allowing some flexibility, but the Triffin dilemma (reserve-currency issuer must run persistent deficits to supply global liquidity, eroding reserve value) made the hybrid unstable. The founding problem: how to enable monetary flexibility without losing the discipline function of commodity backing.
% FOUNDING_PROBLEM_CORROBORATION: Monetary economists (Keynesian and non-Austrian) attest the founding problem (deflationary rigidity, balance-of-payments crises) was historically real and a legitimate target for reform. Gold-standard advocates attest it was over-stated and that pre-1929 gold standard operated with less rigidity than narratives suggest. Reserve-currency discipline advocates attest the founding problem was real but argue the solution created worse problems (persistent inflation, creditor losses). No corroboration from outside the benefiting parties; the problem-statement itself is contested across economic schools.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (early transition period, when discretionary authority was announced but not yet fully weaponized for persistent deficit spending) to 0.78 (mature fiat regime, when discretionary inflation became normalized and benefit-realization of seigniorage was clear). The curve is monotonic: each time the constraint was tested (recessions, deficits, inflation surges) and held, confidence in discretionary authority grew and creditor resistance was further suppressed. Suppression rises from 0.28 to 0.62 because the elimination of gold convertibility removed the automatic exit mechanism (the balance-of-payments crisis that would force fiscal correction). Early suppression is lower because gold-standard advocates still had political voice and capital controls were incomplete; later suppression rises as institutions normalized fiat and marginalized alternative voices. Theater ratio is moderate (0.41 at interval end) because the central claim — that monetary policy is countercyclical stabilization — is partly functional (some recessions were genuinely damped) but increasingly performative (inflation-fighting rhetoric masked the institutional interest in accommodative policy). The shared time grid ensures every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the monetary-authority seat, the constraint is a legitimate liberation from external rigidity, enabling rational crisis management and full-employment targeting. From the creditor seat, it is uncompensated confiscation: the promise was stable purchasing power for savings; the delivery was systematic erosion. From the reserve-holder seat (say, a central banker in Germany or Japan), the constraint is a geopolitical extraction mechanism: paying for U.S. deficits through currency depreciation. From the gold-standard-advocate seat, the constraint represents a failed experiment: it was supposed to be temporary (to solve 1930s rigidity) but became entrenched because beneficiaries had no incentive to restore discipline. The engine should compute these divergences from the structural data: same constraint, widely different d values by seat. The authored claim (tangled_rope) asserts genuine coordination (flexible policy, crisis response) overlaid on asymmetric extraction (seigniorage, creditor losses) requiring active enforcement (legal tender, gold-convertibility prohibition).
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities are clear beneficiaries (d near 0.0) because the constraint directly grants them discretion they exercise for policy goals (and incidentally for institutional expansion). Deficit-financing states are beneficiaries (d near 0.1) because they gain fiscal space without external discipline. Creditor class are clear targets (d near 1.0) because the constraint transfers wealth from them to debtors via inflation and seigniorage; their exit (capital flight, foreign-currency holdings) is costly and faces legal barriers (capital controls were extensive through the 1970s-80s, less so later). Fixed-income earners are targets (d near 0.95) because they bear the pure cost of monetary expansion with no offsetting benefit; their exit (asset-price hedging, foreign currency) requires sophistication most lack. Reserve-currency holders are targets (d near 0.85) because they bear systematic devaluation; their exit (diversification from dollars) is constrained by network effects and geopolitical pressure. Labor unions sit near symmetric (d around 0.5) because they benefit from accommodative policy enabling wage bargains but lose when inflation erodes real wages. Industrial borrowers are beneficiaries (d near 0.2) because they capture inflation gains in nominal debt repayment while maintaining pricing power. The automatic reading produces the largest target/beneficiary gap: the mechanism is purely hierarchical (authorities decide, others bear costs), so the structural directionality is maximized.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deflationary rigidity, balance-of-payments crises) was real in 1920s-1960s. By the 1990s-2000s, the problem statement had shifted: no one was defending the gold standard on rigidity grounds; the debate became whether inflation management and financial stability had improved or worsened. The constraint persists because the beneficiaries (monetary authorities, deficit-financing states) choose to maintain it and have the power to do so. The victim class (creditors, savers, foreign reserve-holders) lack the power to restore the gold standard unilaterally. Mandatrophy is UNRESOLVED in the tangled_rope reading: the constraint carries a genuine coordination function (flexible money supply, crisis response) that would be lost if eliminated, so the claim that it is pure extraction is false. But the extraction is high and undisputed, so the claim that it is pure coordination is also false. The classification as tangled_rope is correct because it captures this hybrid: coordination + extraction, both real, both structural. The theater_ratio rise to 0.41 indicates increasing performance (inflation-fighting rhetoric while accommodative policy continues, forward-guidance theater as substitute for transparent institutional constraints), but the functional coordination component is never entirely hollow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automaticity_vs_institutional_discretion,
    'Is the distinction between ''automatic'' physical constraint and ''discretionary'' institutional authority empirically real, or is it a framing artifact? Did institutions under the gold standard not have discretion, or did they have different incentives?',
    'Historical analysis of gold-standard-era monetary policy: how much discretion did central banks actually exercise within the gold constraint? Did they use open-market operations, reserve requirements, discount rates to moderate the constraint, or did they strictly follow mechanical rules?',
    'If central banks had substantial discretion even under gold (via sterilization, reserve adjustments, gold-point management), the automatic-reading distinction collapses and the real change is in INCENTIVES, not in MECHANISM type. This would support the creditor-discipline reading over the automatic reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automaticity_vs_institutional_discretion, empirical, 'Whether the gold standard was truly automatic or was discretionary within institutional bounds.').

omega_variable(
    coordination_function_persistence,
    'Does the fiat-regime coordination function (flexible money supply, crisis response) genuinely require loss of the gold-standard discipline mechanism, or is it theoretically possible to have flexible supply WITH commodity constraint?',
    'Hypothetical design analysis: could a system use fiat money for transactions but maintain a commodity-valuation floor as a long-term price anchor, unbinding reserves from short-term money supply? Do historical hybrid systems (Bretton Woods, target-zone regimes) show that flexibility and commodity discipline are separable?',
    'If separable, the automatic reading overstates the necessity of the transition: some of the extraction could be eliminated by re-instating commodity discipline while retaining flexible short-term policy. If inseparable, the high extraction is the price of the coordination function and the constraint is accurately classified as tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_persistence, conceptual, 'Whether monetary flexibility and commodity discipline are structurally separable.').

omega_variable(
    seigniorage_distribution_divergence,
    'The automatic reading assumes seigniorage accrues to monetary authorities and reserve-currency issuers, inflating away creditor savings. But do different institutional designs distribute seigniorage differently? Could a fiat system retain the discipline function by rebating seigniorage to the public or to savers?',
    'Comparative institutional analysis of seigniorage distribution: in some fiat systems (e.g., where central bank profits are remitted to treasury, or where inflation-indexed bonds dominate), does the extraction pattern match this reading''s assumptions? Do monetary systems with different seigniorage-distribution rules show different victim/beneficiary structures?',
    'If seigniorage distribution is a distinct institutional choice from the constraint itself, the high extraction might reflect distributive choices, not the unavoidable cost of fiat money. This would suggest the constraint could be reformed without elimination (shifting from tangled_rope toward pure_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_distribution_divergence, conceptual, 'Whether seigniorage distribution is intrinsic to fiat constraint or an independent institutional design choice.').

omega_variable(
    creditor_class_identity_and_exit,
    'The reading assumes creditor-class exit is structurally constrained (capital controls, currency-denomination dependence, legal restrictions on gold holdings). But did creditors actually lack exit options, or did they fail to exercise available exits (political economy of belief)?',
    'Historical case study of creditor behavior: post-1971, could creditors have shifted holdings to gold, foreign currencies, commodities, or real assets? Did they avoid these exits due to legal prohibition or due to belief that fiat would prove stable? Did shifts occur only when inflation became undeniable?',
    'If exit was blocked by law (gold-holdings prohibition, capital controls), the suppression reading is accurate and the constraint is more snare-like (forced participation). If exit was available but avoided due to belief in fiat stability, the suppression is lower and the constraint is more rope-like (coordination through shared expectations). This shifts classification toward tangled_rope or rope depending on exit-option assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_class_identity_and_exit, empirical, 'Whether creditor exit was structurally blocked or merely unattractive under the constraint.').

omega_variable(
    kernel_reading_coexistence_test,
    'Can the automatic-constraint reading and the creditor-discipline reading both be true about the SAME transition event, or do their core premises directly contradict?',
    'Logical analysis: the automatic reading says ''the mechanism changed from material to institutional.'' The creditor-discipline reading says ''geopolitical power shifted from creditor-nations to reserve-currency-issuers.'' These are not mutually exclusive descriptions of the same event — a mechanism swap could occur AS PART OF a geopolitical shift. The question is whether holding both readings requires commitment to contradictory premises about (e.g.) whether the transition was intentional policy choice or structural inevitability.',
    'If the readings are compatible, they coexist (not foreclosed); if they require contradictory premises, one forecloses the other. Compatibility assessment determines the reading_relations entry in cs_structure (coexists_with vs. forecloses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_test, conceptual, 'Whether the automatic and creditor-discipline readings are logically compatible or foreclosed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gold_tr_t0, observed).
narrative_ontology:measurement(gold_tr_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(gold_tr_t5, observed).
narrative_ontology:measurement(gold_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(gold_tr_t10, observed).
narrative_ontology:measurement(gold_tr_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(gold_tr_t15, observed).
narrative_ontology:measurement(gold_tr_t25, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement_basis(gold_tr_t25, observed).
narrative_ontology:measurement(gold_tr_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 35, 0.39).
narrative_ontology:measurement_basis(gold_tr_t35, observed).
narrative_ontology:measurement(gold_tr_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(gold_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(gold_be_t0, observed).
narrative_ontology:measurement(gold_be_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(gold_be_t5, observed).
narrative_ontology:measurement(gold_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(gold_be_t10, observed).
narrative_ontology:measurement(gold_be_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(gold_be_t15, observed).
narrative_ontology:measurement(gold_be_t25, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(gold_be_t25, observed).
narrative_ontology:measurement(gold_be_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 35, 0.74).
narrative_ontology:measurement_basis(gold_be_t35, observed).
narrative_ontology:measurement(gold_be_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(gold_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(gold_su_t0, observed).
narrative_ontology:measurement(gold_su_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement_basis(gold_su_t5, observed).
narrative_ontology:measurement(gold_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(gold_su_t10, observed).
narrative_ontology:measurement(gold_su_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(gold_su_t15, observed).
narrative_ontology:measurement(gold_su_t25, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(gold_su_t25, observed).
narrative_ontology:measurement(gold_su_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 35, 0.6).
narrative_ontology:measurement_basis(gold_su_t35, observed).
narrative_ontology:measurement(gold_su_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(gold_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.25).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, bretton_woods_collapse_mechanism).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, reserve_currency_seigniorage_extraction).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, inflation_tax_mechanism).

% DUAL FORMULATION NOTE:
% This story is ONE reading of a contested kernel: the gold-fiat transition. Three independent constraint stories decompose the single historical event into structurally distinct claims: (1) automatic_constraint_reading (this file) — mechanism swap from material to institutional constraint; (2) creditor_discipline_reading — geopolitical shift in reserve-currency discipline; (3) composite_overdetermination_reading — convergence of independent structural changes rather than single causal swap. The three readings share identical historical referents but differ in what constraint IS the referent. The ε-invariance principle requires separate stories: ε (extractiveness) differs substantially across readings (automatic reading scores high extraction; composite reading might score lower if it denies single-cause extraction mechanism). Each reading is a valid analytical claim held by different scholarly and policy communities. They are linked by network edges and omega variables documenting the kernel dispute, not merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
