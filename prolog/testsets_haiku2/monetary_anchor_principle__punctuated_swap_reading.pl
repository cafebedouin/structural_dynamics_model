% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_punctuated_swap, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Bretton Woods → Floating Rate Transition (Punctuated Swap Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the PUNCTUATED SWAP READING of the
 *   contested kernel 'monetary_anchor_principle'. The reading frames the
 *   transition from Bretton Woods to floating exchange rates as a discrete
 *   institutional choice made on August 15, 1971, when President Nixon
 *   announced the suspension of gold convertibility. From this reading's
 *   perspective, the transition was reversible in principle—the U.S. Treasury
 *   chose to abandon the gold peg rather than defend it or negotiate
 *   multilateral reform. This choice transferred seigniorage gains to the
 *   U.S. (fiscal autonomy, ability to run deficits without external
 *   constraint) and imposed losses on foreign dollar holders (whose reserves
 *   devalued) and fixed-peg regimes (who faced revaluation or devaluation
 *   shocks). The constraint operates as ROPE (coordination function) infected
 *   with EXTRACTION (unilateral defection from the Bretton Woods commitment).
 *   The reading competes with two sibling interpretations: the
 *   overdetermined_composite_reading, which sees the transition as inevitable
 *   outcome of structural forces (Triffin dilemma, Vietnam deficits, capital
 *   mobility), and the triffin_inevitability_reading, which identifies
 *   structural impossibility in the gold standard's logic itself.
 *
 * KEY AGENTS:
 *   - U.S. Treasury/Federal Reserve: unilateral decision-maker (agenda_setter, institutional power, global scope)
 *   - Foreign dollar holders: targets of devaluation expropriation (payers, powerful but constrained exit)
 *   - Fixed-peg regimes: coordinated by the dollar anchor, shocked by its unilateral dissolution (payers, organized power)
 *   - U.S. exporters: beneficiaries of subsequent dollar depreciation (beneficiaries, powerful)
 *   - Gold standard advocates: excluded from the decision (moderate power, constrained by U.S. institutional choice)
 *   - Bretton Woods institutions (IMF/World Bank): observers with authority to monitor but not veto
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.58).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.31).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Bretton Woods → Floating Rate Transition (Punctuated Swap Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '42541b6d-95b7-4793-82a0-7cd42f7e84a7').
narrative_ontology:cs_kernel_codification('42541b6d-95b7-4793-82a0-7cd42f7e84a7', formalized).
narrative_ontology:cs_authority_grounding('42541b6d-95b7-4793-82a0-7cd42f7e84a7', lineage).
narrative_ontology:cs_interpretation_layer_present('42541b6d-95b7-4793-82a0-7cd42f7e84a7').
narrative_ontology:cs_reading_relation('42541b6d-95b7-4793-82a0-7cd42f7e84a7', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('42541b6d-95b7-4793-82a0-7cd42f7e84a7', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('42541b6d-95b7-4793-82a0-7cd42f7e84a7', foundational, monetary_regime_choice_is_reversible).
narrative_ontology:cs_axiom_status(monetary_regime_choice_is_reversible, holdable).
narrative_ontology:cs_axiom_grounding('42541b6d-95b7-4793-82a0-7cd42f7e84a7', monetary_regime_choice_is_reversible, deontological).
narrative_ontology:cs_axiom('42541b6d-95b7-4793-82a0-7cd42f7e84a7', foundational, unilateral_monetary_policy_legitimate_under_sovereignty).
narrative_ontology:cs_axiom_status(unilateral_monetary_policy_legitimate_under_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('42541b6d-95b7-4793-82a0-7cd42f7e84a7', unilateral_monetary_policy_legitimate_under_sovereignty, deontological).
narrative_ontology:cs_reference_frame('42541b6d-95b7-4793-82a0-7cd42f7e84a7', bretton_woods_fixed_peg_dollar_gold).
narrative_ontology:cs_drift_state('42541b6d-95b7-4793-82a0-7cd42f7e84a7', post_1971_floating_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('42541b6d-95b7-4793-82a0-7cd42f7e84a7', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_autonomy).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, fixed_peg_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_multinational_exporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets monetary policy unilaterally. In August 1971, Nixon administration chose to suspend gold convertibility without prior multilateral negotiation, asserting U.S. sovereignty to change the monetary regime. The decision was presented as temporary ("Nixon Shock") but became permanent. This seat controls whether the regime persists or reverts and thus sets the terms for foreign agents.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_treasury_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).

% Foreign central banks and large private investors held dollar reserves under the assumption of $35/oz convertibility. After August 1971, the conversion guarantee vanished and the dollar devalued over the following decade. They could not individually exit—the dollar was the global reserve currency and their reserves were already in dollars. Only coordinated multilateral action could have negotiated an alternative, but each nation faced incentives to defect. The expropriation was via devaluation and inflation, not confiscation.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    powerful, biographical, identity_locked, global).

% Most non-communist nations pegged their currencies to the dollar under Bretton Woods. When the dollar's gold anchor dissolved, they faced a trilemma: maintain the peg and watch their currency appreciate in real terms (losing export competitiveness), abandon the peg and allow their currency to float (exchange-rate risk), or implement capital controls (restricting capital mobility). Each choice imposed costs; none were appealing. The coordinating principle that had bound them was unilaterally severed by the issuer of the anchor.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, fixed_peg_regimes, payer,
    organized, biographical, constrained, global).

% U.S.-based manufacturers and exporters benefited substantially from the subsequent dollar depreciation (effective devaluation of 10–20% over the 1970s). The shift from fixed to floating allowed the dollar to weaken when U.S. inflation exceeded trading partners', automatically restoring export competitiveness without requiring domestic deflation or wage-price controls. This seat's prosperity depended on the regime change.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_multinational_exporters, beneficiary,
    powerful, biographical, mobile, global).

% Economists and policymakers who believed the gold standard imposed necessary fiscal discipline were excluded from the August 1971 decision. They would have opposed suspension and argued for either defending the peg through deflation or returning to gold at a revalued parity. Their voice was overridden by Keynesian arguments that fiscal autonomy was necessary for full employment.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, gold_standard_advocates, excluded,
    moderate, biographical, constrained, national).

% The IMF was created at Bretton Woods to monitor and coordinate the fixed-peg system. When the U.S. suspended gold convertibility, the IMF's foundational mandate became obsolete. The institution had authority to observe and recommend but not to veto U.S. monetary policy. It adapted by adopting special drawing rights and moving toward surveillance of floating rates, but the August 1971 shock revealed that the IMF's legitimacy was conditional on the U.S. respecting the Bretton Woods commitment.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Economists in universities and central banks analyzed the transition and generated competing interpretations: Keynesian economists emphasized the benefits of monetary autonomy; monetarists argued for rules-based policy; structuralists identified Triffin dilemma as inevitable; institutional economists examined the politics of the decision. Their analyses fed into policy legitimacy and shaped how future generations understood the transition, but none determined the August 1971 choice.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, academic_economists_profession, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_treasury_federal_reserve).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods fixed-peg system coordinated global trade and capital flows by anchoring all currencies to the dollar, which was convertible to gold at $35 per ounce. This solved the 1930s coordination failure (competitive devaluations) by providing a stable numeraire and predictable exchange rates. Nations could conduct trade and long-term investment with confidence that their trading partners' currencies would not capriciously devalue. The dollar-gold anchor hierarchized the currency system (the dollar was numeraire, other currencies were pegged to it) and reduced transaction costs for international commerce.
% TRANSFER_FUNCTION: Moves seigniorage gains from foreign holders to the U.S. Treasury. While the peg was maintained (1944–1971), the U.S. benefited from being able to run deficits in dollars (which other nations held as reserves) without immediately losing gold. After August 1971, when convertibility was suspended, the U.S. explicitly gained the ability to depreciate its currency (and thus its liabilities) while foreign dollar holders lost the anchor for their reserves. The transfer was from foreign central banks and dollar-holding investors to the U.S. fiscal authority.
% ABSENT_VOICES: Foreign governments pegged to the dollar were informed of the suspension but not consulted in advance. Gold standard and hard-money advocates were excluded from the decision-making process—the administration had settled on floating-rate policy and did not solicit their objections. Debtor nations that benefited from dollar inflation were not seated as deliberative parties but felt the effects (reduced real debt burden but also currency risk). The decision was made by the U.S. executive branch unilaterally; a multilateral decision would have included all Bretton Woods participants and perhaps some dissent from U.S. domestic constituencies.
% DISAPPEARANCE_RATIONALE: If the August 15, 1971 decision had not occurred—if the U.S. had instead defended the gold peg through deflation, negotiated a multilateral reform, or implemented capital controls—the entire international monetary and financial order would have evolved differently. The floating-rate system, dollar depreciation of the 1970s, petrodollar recycling, and subsequent capital-market developments were all downstream of the regime change. Alternative monetary architectures (return to gold, reserve-basket system, managed float with fixed parities) would have produced different trade patterns, capital flows, and inflation outcomes.
% FOUNDING_PROBLEM: After World War II, the world needed a common monetary standard that could coordinate trade and capital flows without requiring deflationary discipline on all nations. The gold standard of the 1920s had required countries to defend their exchange rate by adjusting internal prices downward, which meant accepting unemployment and lower output. Bretton Woods was designed to provide a stable numeraire (the dollar, convertible to gold) that allowed countries to peg their currencies without requiring perpetual internal deflation. The founding problem was the classic problem of monetary coordination under scarcity: how to create confidence in a common medium of exchange without that medium becoming so scarce that it prevents growth.
% FOUNDING_PROBLEM_CORROBORATION: The Bretton Woods architects (Keynes, White, and the IMF founders) explicitly identified the coordination problem and designed the system to solve it. Their founding documents confirm the problem was acute: without a common standard, nations would resort to bilateral barter, capital would flee unstable currencies, and trade would collapse as it had in the 1930s. The problem remained live throughout 1944–1971: all nations continued to need a monetary standard for trade and investment. However, the empirical status of whether Bretton Woods was still solving the founding problem by 1971 is contested: U.S. policymakers argued it had become an obstacle to growth (Keynesian perspective); advocates of the gold standard argued it was solving the problem but the U.S. was violating its commitment (hard-money perspective); Triffin dilemma adherents argued the founding problem was inherently unsolvable under gold-standard constraints (structural inevitability perspective). Independent observers outside the U.S. and outside the benefiting parties—foreign central banks, the IMF, and academic economists from many nations—attested that the founding problem was live but that Bretton Woods had become dysfunctional by the 1960s. This is where the contest emerges: did the founding problem require Bretton Woods to persist, or did it only require SOME solution, for which alternatives existed?
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under the punctuated swap reading, extractiveness is MODERATE (0.58 at 1971) because the transition was an institutional choice, not a structural inevitability. The U.S. could have chosen differently: defend the peg through deflation, negotiate a revalued-but-still-fixed system, or implement capital controls. The fact that it chose instead to suspend convertibility and extract seigniorage is a policy decision, reversible in principle. Extractiveness RISES from 1944 (0.15, founding-problem coordination) to 1971 (0.58, post-shock expropriation) because the gap between the founding coordinating principle (symmetric Bretton Woods) and its operating reality (asymmetric U.S. dominance) widens over time. After 1971, extractiveness levels off (0.62 at 1975, 0.58 at 1985) because the floating regime settles into a new equilibrium where the extraction is openly admitted rather than hidden behind gold-standard rules. SUPPRESSION is low to moderate (0.31 at 1971) because the floating regime does not require active suppression of alternatives—the regime change was unilateral and not reversible by foreign holders, but the new floating system does not need continuous coercion to maintain. THEATER is elevated (0.22 at 1971, rising afterward) because the U.S. initially justified the suspension as temporary ("Nixon Shock") and framed it as necessary to combat inflation and unemployment, when the underlying choice was fundamentally about regaining fiscal autonomy. The theater decays slightly after 1975 as the floating regime becomes the accepted standard. ACCESSIBILITY_COLLAPSE is low (0.42) because alternatives remained intellectually viable even after the transition: gold standard advocates, monetarists, and later hard-money economists continued to argue for rule-based monetary regimes. The constraint's persistence depended on institutional lock-in and path dependence, not on the collapse of alternatives. RESISTANCE is substantial (0.67) because the transition met active opposition: foreign governments protested, gold advocates opposed the suspension, and the transition sparked a decade of instability (inflation, oil shocks, exchange-rate volatility) that generated sustained criticism of the floating regime.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. Treasury/Federal Reserve seat computes the transition as a necessary institutional adaptation to maintain U.S. price competitiveness and fiscal autonomy—a rope coordination function. Foreign dollar holders and fixed-peg regimes compute the same transition as a unilateral expropriation disguised as monetary reform—a snare or tangled rope with them as targets. The engine computes per-seat classifications from the structural data: the institutional beneficiary (U.S. fiscal autonomy + seigniorage) vs. the payer seats (foreign reserves devalued, exchange-rate revaluation shocks). The disagreement is not about metrics but about whether the transition was justified (foundational claim about institutional sovereignty vs. commitment to Bretton Woods) and whether it was reversible (factual claim about degrees of freedom, but contested by reference to counterfactuals).
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury holds near-zero directionality (full beneficiary: d ≈ 0.0) because the constraint's operation—suspension of gold convertibility and transition to floating—directly subsidizes U.S. fiscal autonomy. Foreign dollar holders hold high directionality (near-full target: d ≈ 0.8–0.9) because their holdings were expropriated by devaluation they did not choose and could not exit from individually. Fixed-peg regimes hold moderate-to-high directionality (d ≈ 0.65–0.75) because they faced revaluation shocks and were forced to choose between defending the peg (absorbing appreciation) or abandoning it (devaluation risk). U.S. exporters hold low directionality (d ≈ 0.2–0.3, net beneficiary) because subsequent dollar depreciation improved their export competitiveness. The exit-options modulation is crucial: foreign dollar holders were IDENTITY_LOCKED (their reserves were in dollars; exiting required official policy change and coordination; individual actors could not exit) and CONSTRAINED (the dollar was the global standard and reserve currency; no substitute existed in 1971). The U.S. Treasury was ARBITRAGE (it could choose the regime and change it; the constraint operates at its will). This structural difference drives the directionality divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stable monetary coordination for post-war trade) was genuinely live at Bretton Woods in 1944. By 1971, the problem remained live (the world still needed a monetary standard) but the Bretton Woods solution to it had become CONTESTED—critics argued that the rigid dollar-gold peg was incompatible with simultaneous pursuit of full employment, price stability, and capital mobility (Triffin dilemma). The punctuated swap reading answers the mandatrophy question by asserting that the founding problem and its Bretton Woods solution were SEPARABLE: one could have solved the coordination problem with a different regime (return to gold at revalued parity, negotiated reserve basket, special drawing rights, or floating with agreed intervention rules). The August 1971 choice to suspend convertibility unilaterally was thus NOT mandated by the founding problem's persistence—it was a policy choice that replaced coordination with unilateral dominance. This distinguishes the punctuated swap reading from the triffin_inevitability reading, which claims the founding problem's solution was mathematically impossible under gold-standard constraints. The classification as ROPE reflects this: a rope solves a genuine coordination problem without extractive override. But the punctuated swap reading adds extraction ON TOP of the coordination problem—the regime change extracts asymmetric seigniorage gains from foreign holders. The reading thus describes a ROPE INFECTED WITH EXTRACTION, or transitionally a TANGLED ROPE: the founding coordination function (stable exchange rates) is achieved, but asymmetrically, via unilateral institutional choice, with active suppression of alternatives (gold standard, capital controls, bilateral negotiation). The theater ratio rising after 1971 reflects the increasing rhetorical work needed to justify floating-rate outcomes that departed from both stable-money and free-trade ideals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_choice_vs_structural_inevitability,
    'Was the August 15, 1971 decision to suspend gold convertibility a genuine institutional choice (the U.S. could have chosen differently) or a structurally forced adaptation to unsustainable conditions?',
    'Counterfactual institutional history: What policy instruments were available to the U.S. in 1971? Could the U.S. have defended the gold peg through: (a) deflation to restore price competitiveness; (b) negotiated revaluation of the dollar within Bretton Woods; (c) capital controls to stem gold outflows; (d) multilateral reform to a reserve-basket system? Archival evidence from Nixon administration deliberations, Federal Reserve records, and economists'' contemporaneous recommendations would establish the actual option set the decision-makers faced.',
    'If the transition was a genuine choice among viable alternatives, the punctuated swap reading stands: the U.S. chose unilateral advantage over multilateral coordination, and the constraint is better classified as tangled rope (coordination function + asymmetric extraction). If the transition was structurally forced (no viable alternatives existed by 1971), then the triffin_inevitability reading is stronger, and the constraint better approximates a forced adaptation—possibly still tangled rope but with extraction as a side effect rather than the driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_choice_vs_structural_inevitability, empirical, 'Whether the regime transition was an avoidable institutional choice or a forced structural adaptation.').

omega_variable(
    triffin_dilemma_operationality,
    'Did the Triffin dilemma (reserve-currency issuer under gold standard must run deficits to supply liquidity, which exhausts reserves and forces abandonment) operate as a mathematical constraint that forced the August 1971 transition, or was it an analytical finding that policy makers could have circumvented through institutional innovation?',
    'Historical-counterfactual analysis: Did U.S. gold reserves decline to a critical threshold by 1971 that made the peg arithmetically impossible to maintain? What were the actual alternatives available to supply international liquidity without depleting U.S. gold (SDRs, reserve-basket system, negotiated dollar devaluation, capital controls)? Did these alternatives exist in 1971 but were rejected for political reasons?',
    'If Triffin was operationally decisive (gold reserves fell below the point where the peg was physically defensible), then the transition was forced and the punctuated swap reading overstates institutional agency. If Triffin was analytically correct but policy-avoidable (gold reserves could have been defended through institutional innovation or negotiated reform), then the punctuated swap reading is vindicated: the U.S. chose float over alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_operationality, empirical, 'Whether the Triffin dilemma operated as a hard mathematical constraint or as a policy finding that could have been circumvented.').

omega_variable(
    founding_problem_malleability,
    'Was the Bretton Woods system the ONLY possible solution to the post-war monetary coordination problem, or could alternative regime architectures (e.g., symmetric reserve-basket system, negotiated parity grid, return to gold at revalued levels) have solved the founding coordination problem while avoiding Triffin-dilemma instability?',
    'Comparative institutional analysis: Examine proposals for alternative post-war monetary systems that were rejected or superseded (Keynes''s Bancor plan, commodity-basket standards, special drawing rights as the primary reserve asset). Did any of these alternatives offer lower extractiveness or more symmetric coordination than Bretton Woods? Would adopting them have forestalled or reshaped the 1971 transition?',
    'If the founding problem admits multiple solutions and Bretton Woods was merely one choice among viable alternatives, then the transition away from Bretton Woods was not mandated by the founding problem''s persistence—it was a policy choice to extract seigniorage rather than adapt the regime. If the founding problem narrowly admits only the Bretton Woods solution and all alternatives are mathematically or politically infeasible, then the transition reflects the binding constraint of the founding problem itself, not unilateral U.S. choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_malleability, conceptual, 'Whether Bretton Woods was the unique solution to post-war monetary coordination or one choice among institutional alternatives.').

omega_variable(
    reading_contest_empirical_closure,
    'Do the three readings of the monetary_anchor_principle kernel—punctuated swap, overdetermined composite, Triffin inevitability—forecast empirically different outcomes or policy recommendations, or do they converge on the same facts while disagreeing only on causal narrative?',
    'Policy-relevance test: If the U.S. faced a future choice about monetary regime reform (e.g., return to gold standard, adoption of regional currency basket, IMF special drawing rights), would the three readings recommend different paths? Or do they all predict similar outcomes (floating-rate persistence) while disagreeing on WHY the transition happened in 1971?',
    'If the readings diverge on future policy recommendations, they are distinct structural claims and should generate different per-seat classifications (e.g., punctuated swap → tangled rope; Triffin inevitability → forced adaptation, closer to mountain). If they converge on empirical predictions and differ only on historical narrative, they are alternative causal stories about the same constraint, and the contest is methodological rather than substantive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_empirical_closure, conceptual, 'Whether the three kernel readings represent distinct structural constraints or alternative narratives of the same empirical phenomenon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1944, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement_basis(mone_tr_t1944, projected).
narrative_ontology:measurement(mone_tr_t1960, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement_basis(mone_tr_t1960, observed).
narrative_ontology:measurement(mone_tr_t1970, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement_basis(mone_tr_t1970, observed).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.22).
narrative_ontology:measurement_basis(mone_tr_t1971, observed).
narrative_ontology:measurement(mone_tr_t1975, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement_basis(mone_tr_t1975, observed).
narrative_ontology:measurement(mone_tr_t1985, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(mone_tr_t1985, observed).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement_basis(mone_be_t1944, projected).
narrative_ontology:measurement(mone_be_t1960, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement_basis(mone_be_t1960, observed).
narrative_ontology:measurement(mone_be_t1970, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement_basis(mone_be_t1970, observed).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.58).
narrative_ontology:measurement_basis(mone_be_t1971, observed).
narrative_ontology:measurement(mone_be_t1975, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement_basis(mone_be_t1975, observed).
narrative_ontology:measurement(mone_be_t1985, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement_basis(mone_be_t1985, observed).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1944, 0.02).
narrative_ontology:measurement_basis(mone_su_t1944, projected).
narrative_ontology:measurement(mone_su_t1960, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement_basis(mone_su_t1960, observed).
narrative_ontology:measurement(mone_su_t1970, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1970, 0.24).
narrative_ontology:measurement_basis(mone_su_t1970, observed).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.31).
narrative_ontology:measurement_basis(mone_su_t1971, observed).
narrative_ontology:measurement(mone_su_t1975, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1975, 0.29).
narrative_ontology:measurement_basis(mone_su_t1975, observed).
narrative_ontology:measurement(mone_su_t1985, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1985, 0.27).
narrative_ontology:measurement_basis(mone_su_t1985, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__punctuated_swap_reading, 0.18).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, petrodollar_system_emergence).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, floating_exchange_rate_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest. All three readings (punctuated_swap, overdetermined_composite, triffin_inevitability) share the same referent—the August 1971 transition from gold-convertible dollar to floating exchange rates—but differ in whether the transition was a CHOICE (punctuated swap), an INEVITABLE outcome of structural forces (overdetermined composite, Triffin), or a MATHEMATICALLY FORCED adaptation to reserve-currency dilemma (Triffin specificity). The readings are linked via network.affects_constraints and routed to omega variables and cs_structure.reading_relations. They do not merge into one story: each reading instantiates a different constraint with different epsilon, beneficiary/victim structure, and class (rope vs. tangled_rope vs. forced adaptation). The kernel is the principle that currencies should be anchored to a stable numeraire; the readings interpret whether that principle was voluntarily abandoned or necessarily abandoned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__punctuated_swap_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
