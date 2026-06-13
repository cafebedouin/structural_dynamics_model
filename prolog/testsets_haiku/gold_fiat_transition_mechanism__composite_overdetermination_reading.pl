% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-to-Fiat Transition as Composite Structural Convergence
 *   domain: economic/political/historical
 *
 * SUMMARY:
 *   The transition from gold-anchored to fiat currency between 1944-1985 is
 *   typically narrated as a single causal event: Nixon Shock (August 1971)
 *   terminated US gold redemption and unleashed floating-rate monetary
 *   autonomy. This reading rejects that singular framing. The
 *   composite-overdetermination reading asserts the transition was the
 *   convergence of four structurally independent shifts: (1)
 *   telecommunications technology enabling instant capital flows (SWIFT,
 *   satellite communications, undersea fiber, automated trading systems
 *   matured during the 1960s); (2) Balance-of-payments dynamics shifting from
 *   creditor to debtor dominance as US external deficits accumulated (the
 *   Triffin dilemma became acute by the mid-1960s); (3) labor bargaining
 *   power shifts—unionized wage growth in developed economies faced new
 *   competitive pressure from capital mobility and corporate relocation
 *   threats as the technical capacity for instant capital flight became real;
 *   (4) legal-tender enforcement apparatus maturity—by the 1970s, tax
 *   systems, monetary regulation, and fiat banking infrastructure had evolved
 *   to the point where fiat currency could operate autonomously without
 *   commodity backing. Each of these shifts had its own causal history and
 *   distributional consequences independent of the others. The reading
 *   asserts that Nixon's 1971 announcement was the MARKER, not the cause—the
 *   formal termination of a rule set that had already become operationally
 *   impossible to maintain. If any one of the four structural changes had not
 *   occurred, the transition might have taken a different form or timeline,
 *   but the combined pressure made some form of regime shift nearly
 *   inevitable.
 *
 * KEY AGENTS:
 *   - Reserve-currency issuer (US Federal Reserve / Treasury): Loses the constraint of gold redemption pledge, gains autonomy in monetary policy, but faces new constraint of capital mobility and floating-rate discipline.
 *   - High-frequency traders and multinational capital: Gain from the technical capacity to arbitrage rates and repatriate earnings, become winners from volatility and deregulation.
 *   - Fixed-peg-dependent economies: Lose the stabilizer of fixed exchange rates, forced to choose among floating (volatility), controls (rigidity), or subordinate pegs (new dependency).
 *   - Wage labor collective power: Face new competitive pressure from capital mobility, erode organized bargaining as corporations credibly threaten relocation.
 *   - Subsistence commodity economies: Lose price stabilization, face boom-bust cycles, accumulate debt in reserve currencies, become vulnerable to speculative flows.
 *   - Legal-tender enforcement apparatus: Gains the institutional capacity to operate fiat systems autonomously; the apparatus matured alongside the other structural shifts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.48).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.31).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-to-Fiat Transition as Composite Structural Convergence").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "economic/political/historical").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '460b41c7-fe0d-450b-90d0-6850de3354de').
narrative_ontology:cs_kernel_codification('460b41c7-fe0d-450b-90d0-6850de3354de', fixed_text).
narrative_ontology:cs_authority_grounding('460b41c7-fe0d-450b-90d0-6850de3354de', extraction).
narrative_ontology:cs_interpretation_layer_present('460b41c7-fe0d-450b-90d0-6850de3354de').
narrative_ontology:cs_reading_relation('460b41c7-fe0d-450b-90d0-6850de3354de', gold_fiat_transition_mechanism__automatic_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('460b41c7-fe0d-450b-90d0-6850de3354de', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('460b41c7-fe0d-450b-90d0-6850de3354de', foundational, transition_overdetermined_by_independent_shifts).
narrative_ontology:cs_axiom_status(transition_overdetermined_by_independent_shifts, holdable).
narrative_ontology:cs_axiom_grounding('460b41c7-fe0d-450b-90d0-6850de3354de', transition_overdetermined_by_independent_shifts, empirically_contingent).
narrative_ontology:cs_axiom('460b41c7-fe0d-450b-90d0-6850de3354de', foundational, no_single_causal_node_sufficient).
narrative_ontology:cs_axiom_status(no_single_causal_node_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('460b41c7-fe0d-450b-90d0-6850de3354de', no_single_causal_node_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('460b41c7-fe0d-450b-90d0-6850de3354de', unified_monetary_constraint_assumption).
narrative_ontology:cs_drift_state('460b41c7-fe0d-450b-90d0-6850de3354de', contemporary_floating_rate_regime, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('460b41c7-fe0d-450b-90d0-6850de3354de', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuers).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, high_frequency_traders).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_capital).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_peg_dependent_economies).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, wage_labor_collective_power).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, subsistence_commodity_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_reserve_custodians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central bank of the dominant economy (US). Controls monetary base, sets interest rate policy, determines inflation trajectory within constraints of capital flows and exchange-rate expectations. Under Bretton Woods, bound by gold redemption pledge at $35/oz; post-transition, sets policy autonomously. The formal power shift occurred in 1971 with Nixon's announcement, but the institutional capacity to operate independently had matured for several years prior as technical and balance-of-payments conditions eroded the peg's sustainability.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Developing and developed countries whose balance-of-payments position and trade infrastructure depended on stable exchange rates within Bretton Woods bands. When pegs collapsed, faced choice among floating (exchange-rate volatility, inflation pass-through), capital controls (institutional rigidity, capital flight), or subordinate peg (new dependency on reserve-currency issuer's policy). The transition imposed adjustment costs (unemployment, external deficits, inflation) that were borne disproportionately by these economies. No unilateral exit once the global infrastructure assumed fixed pegs.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_peg_dependent_economies, payer,
    moderate, generational, constrained, global).

% Capital market participants whose trading strategy depends on rapid response to rate differentials across markets. Under gold standard and fixed pegs, their function was constrained by arbitrage bands; trading volumes were limited. As instant capital flows became technically feasible (SWIFT, automated execution, satellite comms), the volatility of floating rates created profitable opportunities. The transition from fixed to floating rates directly enabled their emergence as a major market force.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, high_frequency_traders, beneficiary,
    organized, biographical, mobile, global).

% Large corporations with multi-currency cash flows. Under Bretton Woods, faced exchange controls and repatriation restrictions; overseas earnings had to be brought home at fixed rates or held in restricted accounts. As pegs collapsed and capital controls loosened, multinational balance-sheet management became a profit center. Transfer pricing, tax arbitrage, and currency hedging strategies became central to corporate optimization. The instant-capital-flow infrastructure directly enabled this shift.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_capital, beneficiary,
    powerful, biographical, arbitrage, global).

% Unions and organized labor in developed economies. Under Bretton Woods (1945-1965 especially), real wage growth tracked productivity, inflation was low and stable, labor had institutional representation in wage negotiations. As the fixed-peg system deteriorated, central banks adopted inflation-fighting mandates, the technical capacity for capital relocation became credible, and floating-rate balance-of-payments crises became discipline mechanisms. Labor's structural bargaining power declined not from a single cause but from the confluence of capital mobility, central-bank priorities, and the labor-discipline regimes that followed. Exit from this constraint is either emigration (costly, identity-locked) or subordination to lower wages.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, wage_labor_collective_power, payer,
    organized, biographical, constrained, national).

% Developing economies dependent on commodity exports (food, minerals, energy). Under Bretton Woods, commodity prices had some stabilization from fixed-peg regime and institutional commodity agreements. As pegs floated, commodity prices became volatile, subject to speculative currency flows, and decoupled from stable purchasing power. Without access to stable credit in their own currencies, these economies faced boom-bust cycles, debt accumulation in reserve currencies, and structural vulnerability to capital flows beyond their control. No exit: trapped in commodity dependence and external-debt vulnerability.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, subsistence_commodity_economies, payer,
    powerless, generational, trapped, global).

% The state apparatus enforcing fiat currency as legal tender: tax collection in fiat, prosecution of counterfeit, contract enforcement in fiat terms, banking regulation that treats fiat as the unit of account. Under gold standard, constrained by finite commodity reserves; enforcement infrastructure only needed to prevent counterfeiting and ensure contract enforcement. As fiat detached from gold, the apparatus matured: tax systems became more sophisticated, monetary regulation more intricate, capital controls more refined. The apparatus did not cause the transition but enabled its completion once fiat became operational.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, legal_tender_enforcement_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, legal_tender_enforcement_apparatus, beneficiary).
narrative_ontology:stakeholder_non_agent(gold_fiat_transition_mechanism__composite_overdetermination_reading, legal_tender_enforcement_apparatus).

% The set of fixed-peg rules (±1% band around par), IMF adjustment mechanisms, and capital-control norms that governed 1944-1971. Operated on assumptions that exchange-rate stability was a collective good and that some capital immobility was necessary to sustain it. As the reading asserts, the framework did not fail because of a single structural break—it crumbled under pressure from four independent directions: technology (instant capital flows made institutional immobility assumption obsolete), balance-of-payments dynamics (shifted creditor/debtor distribution), labor discipline (capital mobility created new competitive pressure), and enforcement maturation (fiat systems became operationally capable). The framework's collapse was overdetermined; no single pressure alone would have destroyed it, but the combination was devastating.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_institutional_framework, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_institutional_framework).

% Central banks holding gold reserves as backing for their currencies' claims. Under Bretton Woods, obligated to exchange gold for dollars at $35/oz on demand from other central banks; the US obligated to do the same. The constraint was not that gold was literally running out (it was, but slowly)—the constraint was that the institutional commitment became unsustainable once instant capital mobility allowed central banks to withdraw reserves strategically and once the other structural shifts undermined confidence in the peg. Reserves were nominally constant but their signaling role deteriorated. The 1968 London Gold Pool collapse and the US's suspension of gold sales to private markets presaged the 1971 termination.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_reserve_custodians, payer,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stable exchange rates and predictable international trade settlement: Bretton Woods coordinated capital flows to maintain fixed pegs, provided a common unit of account for bilateral trade, and created an institutional framework for balance-of-payments adjustment. The coordination problem it solved was the 1930s/1940s chaos: currency wars, competitive devaluations, trade collapse, and absence of a neutral arbiter of value. The system's coordination function was real but fragile—it required capital immobility, stable political dominance of a creditor bloc, and acceptance of inflation/deflation discipline from the commodity anchor.
% TRANSFER_FUNCTION: The system transferred purchasing power and seigniorage to reserve-currency issuers (primarily the US): the US could print dollars, the world had to accept them at fixed rates to settle trade, and the US could run persistent current-account deficits financed by accumulation of dollars abroad. Simultaneously, it transferred adjustment burden (deflationary discipline, unemployment, wage pressure) to deficit countries. As the technical and structural conditions enabling the system's stability deteriorated, the transfer mechanism persisted longer than the coordination mechanism—the beneficiaries (reserve-currency issuer, capital holders with mobile assets, high-frequency traders once they could move at speed) pushed to maintain the transfer even as coordination was breaking down.
% ABSENT_VOICES: Commodity-exporting and subsistence-dependent economies were not in the 1944 Bretton Woods negotiation; the institutional framework reflected creditor-nation preferences and the geopolitical moment of US dominance. Labor organizations had input in domestic wage-bargaining but were excluded from international monetary design; union objections to the inflation-fighting mandates that followed would come only after the transition was complete. Developing-country governments that would face the most destabilizing effects of floating rates and speculative capital flows were not seated at the redesign.
% DISAPPEARANCE_RATIONALE: If the gold-fiat transition had not occurred (i.e., if Bretton Woods somehow persisted indefinitely), the world would have reorganized differently: international trade would operate under capital controls or regional currency unions, multinational corporations would face persistent restrictions on fund repatriation and cross-border investment, wage bargaining would not have experienced the same competitive pressure from capital mobility, and commodity economies would have had somewhat more stability in purchasing power. The present configuration of global capital flows, the architecture of multinational finance, and the labor discipline regime all depend on the fiat transition having happened. Its removal would require massive structural rebuilding.
% FOUNDING_PROBLEM: The founding problem of Bretton Woods was preventing the 1930s-style currency chaos and enabling post-war international trade reconstruction. The founding problem was NOT stable money supply or central-bank autonomy—those were secondary. The founding problem was coordination and settlement: how do nations trade goods when currencies are distrusted, deflation is feared, and no mechanism exists to allocate the burden of adjustment? Bretton Woods answered: fix exchange rates, supply liquidity through the IMF and the dollar, and let the system stabilize trade.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing currency chaos and enabling trade) is attested by all parties including contemporary economists and historians outside the benefiting seats. However, WHETHER the founding problem persisted in its 1944 form by 1971 is contested: the beneficiaries of the system (reserve-currency issuer, capital holders) assert the system still solved the coordination problem and that the transition was a power grab; critics (developing-country economists, labor historians, some heterodox economists) assert the founding problem had been substantially solved by the 1960s and the system's persistence served only to transfer rents and avoid discipline. The UK government, which would be subordinate under either regime, attested that the system was unsustainable by 1968 (the Sterling crisis, the London Gold Pool collapse). No single corroborating voice outside the beneficiary seats; the dispute is genuine.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the reading's core claim: this is not a pure extraction (which would have high-extractiveness, low-theater) nor a pure coordination (low-extractiveness, high-accessibility-collapse). Instead, it scores as tangled-rope because different structural shifts had mixed distributional effects. The extractiveness trajectory (0.12→0.51→0.48) shows cumulative capture of seigniorage and exchange-rate flexibility benefits early (1944-1975) but a slight decay by 1985 as floating rates stabilized and new players (including developing-country central banks with petrodollar surpluses) began hedging. The theater_ratio peaks at 1971 (0.28) because that is when the formal institutional narrative (Bretton Woods still operating) diverges most sharply from the operational reality (four structural pressures making the fixed-peg system technically impossible to sustain). By 1985, theater declines (0.22) because the new fiat system has settled into operational stability. Suppression_requirement shows the enforcement cost of maintaining the old regime (high mid-1960s when the Triffin dilemma was acute) declining somewhat after 1975 as the new regime stabilizes and only capital controls (in non-reserve-currency countries) require ongoing suppression. The coercion grid differentiates effects by level: individual-level accessibility to currency and credit tightens most (from 0.45 to 0.72) because floating rates and speculative volatility constrain retail currency access; organizational-level (firms, unions) also experiences tightening but less dramatically because scale enables hedging; class-level resistance actually increases (0.35→0.72) because labor organizations mount sustained resistance to the wage discipline imposed by new capital mobility; structural-level shows the most stability because the macro system finds new equilibrium. Suppression at structural level remains modest (0.12→0.15) because the new fiat regime requires less overt institutional coercion once it is normalized than the old fixed-peg regime required (the fix itself was the suppression).
 *
 * PERSPECTIVAL GAP:
 *   The reserve-currency issuer seat and the fixed-peg-dependent seats should compute radically differently. For the issuer, the transition is liberation: a loss of constraint (gold redemption pledge), a gain of autonomy (monetary policy without commodity anchor). From the fixed-peg-dependent seats, the transition is a loss of stability anchor and a gain of exposure to speculative capital flows. For multinational capital, it is a gain (barriers to fund repatriation and arbitrage removed). For wage labor, it is a loss (new competitive pressure, eroding bargaining power). For subsistence commodity economies, it is a loss (price destabilization, debt accumulation risk). The engine should compute five different types from the same structural data because the directionality differs sharply. The reserve-currency issuer sits near d=0.0 (beneficiary); fixed-peg dependents sit near d=0.8 (target); labor organizations sit near d=0.65-0.75 (target, but with organizational power to mount resistance); commodity economies sit near d=0.85+ (trapped target). No single type adequately characterizes the arrangement because the arrangement is not unified—it is an overdetermined convergence of four independent shifts that happen to occur in the same historical period.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries of the extractive side of the transition: (1) reserve-currency issuer gains seigniorage from dollar dominance in global settlement, gains monetary autonomy, gains ability to run persistent deficits financed by dollar accumulation abroad; (2) high-frequency traders and multinational capital gain from barriers removed to repatriation and arbitrage, gain from volatility (floating rates make arbitrage profitable). Victims of the extractive side: (1) fixed-peg-dependent economies lose the exchange-rate stability they built their trade and development strategies on, forced to absorb adjustment costs (unemployment, capital flight, inflation); (2) wage labor loses bargaining power as capital becomes mobile and threatens relocation, loses the inflation-stability coordination of the Bretton Woods era, faces new discipline from central banks focused on creditor preferences; (3) subsistence commodity economies lose price stability and gain exposure to speculative flows, accumulate debt in reserve currencies, become dependent on capital inflows that are increasingly unstable. The coordination side of the arrangement (which is why the constraint is tangled_rope, not pure snare): the transition enabled genuine coordination improvements for capital-mobile actors—multinational firms could optimize global operations, traders could arbitrage price differences and liquidate inefficiencies, developing countries that could access dollar borrowing could accelerate development. But these coordination gains accrued to a narrow set (capital-mobile actors), while the distributional costs (exchange-rate volatility, inflation discipline, loss of bargaining power) were borne by a broader set. Directionality overrides are not needed because the structural derivation from beneficiary/victim declarations should capture the variation correctly—the engine's per-seat classification will show the reserve-currency issuer and capital-mobile actors at low d (beneficiaries), and the constrained/trapped seats at high d (targets).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap that ensnares the automatic_constraint_reading. The automatic reading claims the transition eliminated a physical constraint (gold reserve limits), suggesting the constraint was purely natural/technical. But if it were purely natural, it would not require the enforcement apparatus maturation that occurred—the legal-tender enforcement system had to be upgraded to handle fiat currency. This reading correctly identifies both a coordination function (enabling instant capital flows for genuine efficiency gains, enabling central banks to run flexible monetary policy) and an extractive function (seigniorage capture, labor discipline, shift of adjustment burden to constrained economies). The arrangement persists because it delivers real coordination benefits to some seats while extracting from others, not because it is a natural law. Declaring mandatrophy_resolved: false—the founding problem (preventing 1930s-style currency chaos) is contested as to whether it still exists in 1985, so the constraint has not reached the stage of persisting entirely on theatrical grounds. It has reached the stage (by 1985) where the benefits to reserve-currency issuer and capital-mobile actors are clear and the costs to constrained actors are clear, and the political economy of reform has become gridlocked—no seat has sufficient power to change the arrangement unilaterally, and the seats benefiting are sufficiently powerful to prevent major reform. This is tangled_rope stability, not piton inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_attribution_problem,
    'How do we assign causal weight to four structurally independent shifts that occurred simultaneously? Is the transition overdetermined (any one shift would have forced some regime change), or do the shifts require each other to have produced the specific fiat-transition outcome that occurred?',
    'Counterfactual modeling and historical narrative analysis: construct scenarios where each shift is absent and model the system''s evolution; compare with the observed path to determine whether the full set is required or whether subsets sufficed.',
    'If overdetermined (any subset suffices), the transition is a contingent convergence, and the automatic and creditor readings misattribute causality to single factors. If the full set is required (tight coupling), the readings might be capturing partial truths about coupled causation. This determines whether the kernel is genuinely contested or merely differently narrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causality_attribution_problem, conceptual, 'Whether the four structural shifts are independent overdetermined causes or tightly coupled necessities.').

omega_variable(
    beneficiary_distribution_contention,
    'Did the transition systematically benefit some actors and harm others, or did it produce a Pareto-improving trade-off where all seats gained (at different rates) even while relative positions shifted?',
    'Empirical historical accounting: did wage labor''s real purchasing power increase or decrease? Did developing economies'' growth rates accelerate or decelerate? Did capital-mobile actors'' returns increase? Did consumer welfare in developed economies improve?',
    'A Pareto-improving transition would reduce the extractiveness score and reframe the constraint as pure rope. A Pareto-worsening-for-most transition with concentrated gains (current reading) confirms tangled_rope with meaningful asymmetry. The reading''s extraction value of 0.48 assumes concentrated gains and distributed costs; if the empirics show broad-based gains, the value should be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_distribution_contention, empirical, 'Whether the transition was Pareto-improving or extractive in its distributional effect.').

omega_variable(
    technological_determinism_risk,
    'Does the reading commit a technological-determinism error by treating instant capital flows as inevitably requiring floating rates? Could capital controls have been tightened instead, enforcing fixed pegs through regulatory discipline rather than abandoning the peg?',
    'Historical comparison with countries that did tighten capital controls (France, Spain, Chile post-1973) versus those that floated; examination of whether capital control regimes were politically feasible in the US/UK context given Cold War financial competition and neoliberal ideological shifts.',
    'If capital controls were politically feasible but ideologically rejected, the transition is more attributable to political choice (beneficiary preference for deregulation) than to technological inevitability. This would raise both extractiveness and suppression scores (suppression of capital control advocates) and reframe the constraint as more explicitly snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_risk, empirical, 'Whether instant capital-flow technology required fiat adoption or merely enabled it via political choice.').

omega_variable(
    kernel_singularity_assumption,
    'Is the kernel itself well-posed? Is there a single ''gold-to-fiat transition'' or are there multiple regime shifts (Bretton Woods collapse, the dollar-standard emergence, the Euromarket unbunking, petrodollar recycling, the Plaza Accord) that share a rhetorical label but have distinct causal structures?',
    'Structural decomposition of regime-shift events; examination of whether treating them as a single kernel conflates independent constraints that should be modeled separately.',
    'If the kernel decomposes, this reading''s claim (that the transition is overdetermined by four independent shifts) is itself incomplete—the kernel was never unified in the first place. This would require further decomposition of the constraint family itself, with separate stories for each regime-shift component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_singularity_assumption, conceptual, 'Whether the kernel ''gold-fiat transition'' is a single coherent event or a rhetorical grouping of distinct regime shifts.').

omega_variable(
    fiat_enforcement_apparatus_maturity,
    'What is the causal role of legal-tender enforcement apparatus maturity? Did fiat enforcement infrastructure enable the transition, or did the transition necessitate and accelerate the infrastructure development?',
    'Timeline analysis of tax system, banking regulation, and monetary-policy infrastructure development relative to the four other structural shifts; examination of whether countries with less-mature enforcement apparatus (developing economies, colonial-legacy states) experienced the transition differently.',
    'If enforcement maturity enabled the transition, it is a necessary condition and should be weighted equally with the other four shifts. If it was necessitated by the transition, it is a consequence rather than a cause, and the reading''s claimed ''four independent shifts'' should be re-examined for temporal ordering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiat_enforcement_apparatus_maturity, empirical, 'Whether fiat-enforcement maturity was a cause or consequence of the transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1944, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1944, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement_basis(gold_tr_t1944, observed).
narrative_ontology:measurement(gold_tr_t1955, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1955, 0.08).
narrative_ontology:measurement_basis(gold_tr_t1955, observed).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement_basis(gold_tr_t1965, observed).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement_basis(gold_tr_t1971, observed).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.26).
narrative_ontology:measurement_basis(gold_tr_t1975, observed).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement_basis(gold_tr_t1985, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1944, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1944, 0.12).
narrative_ontology:measurement_basis(gold_be_t1944, observed).
narrative_ontology:measurement(gold_be_t1955, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1955, 0.24).
narrative_ontology:measurement_basis(gold_be_t1955, observed).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement_basis(gold_be_t1965, observed).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.51).
narrative_ontology:measurement_basis(gold_be_t1971, observed).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.54).
narrative_ontology:measurement_basis(gold_be_t1975, observed).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement_basis(gold_be_t1985, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1944, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1944, 0.18).
narrative_ontology:measurement_basis(gold_su_t1944, observed).
narrative_ontology:measurement(gold_su_t1955, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1955, 0.22).
narrative_ontology:measurement_basis(gold_su_t1955, observed).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.32).
narrative_ontology:measurement_basis(gold_su_t1965, observed).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1971, 0.41).
narrative_ontology:measurement_basis(gold_su_t1971, observed).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement_basis(gold_su_t1975, observed).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1985, 0.31).
narrative_ontology:measurement_basis(gold_su_t1985, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=1985
narrative_ontology:measurement(gold_grid_01, gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse(class), 1944, 0.58).
narrative_ontology:measurement(gold_grid_02, gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse(class), 1985, 0.54).
narrative_ontology:measurement(gold_grid_03, gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse(individual), 1944, 0.45).
narrative_ontology:measurement(gold_grid_04, gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse(individual), 1985, 0.72).
narrative_ontology:measurement(gold_grid_05, gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse(organizational), 1944, 0.52).
narrative_ontology:measurement(gold_grid_06, gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse(organizational), 1985, 0.68).
narrative_ontology:measurement(gold_grid_07, gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse(structural), 1944, 0.68).
narrative_ontology:measurement(gold_grid_08, gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse(structural), 1985, 0.62).
narrative_ontology:measurement(gold_grid_09, gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance(class), 1944, 0.35).
narrative_ontology:measurement(gold_grid_10, gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance(class), 1985, 0.72).
narrative_ontology:measurement(gold_grid_11, gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance(individual), 1944, 0.08).
narrative_ontology:measurement(gold_grid_12, gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance(individual), 1985, 0.42).
narrative_ontology:measurement(gold_grid_13, gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance(organizational), 1944, 0.18).
narrative_ontology:measurement(gold_grid_14, gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance(organizational), 1985, 0.65).
narrative_ontology:measurement(gold_grid_15, gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance(structural), 1944, 0.62).
narrative_ontology:measurement(gold_grid_16, gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance(structural), 1985, 0.58).
narrative_ontology:measurement(gold_grid_17, gold_fiat_transition_mechanism__composite_overdetermination_reading, stakes_inflation(class), 1944, 0.42).
narrative_ontology:measurement(gold_grid_18, gold_fiat_transition_mechanism__composite_overdetermination_reading, stakes_inflation(class), 1985, 0.48).
narrative_ontology:measurement(gold_grid_19, gold_fiat_transition_mechanism__composite_overdetermination_reading, stakes_inflation(individual), 1944, 0.28).
narrative_ontology:measurement(gold_grid_20, gold_fiat_transition_mechanism__composite_overdetermination_reading, stakes_inflation(individual), 1985, 0.61).
narrative_ontology:measurement(gold_grid_21, gold_fiat_transition_mechanism__composite_overdetermination_reading, stakes_inflation(organizational), 1944, 0.35).
narrative_ontology:measurement(gold_grid_22, gold_fiat_transition_mechanism__composite_overdetermination_reading, stakes_inflation(organizational), 1985, 0.58).
narrative_ontology:measurement(gold_grid_23, gold_fiat_transition_mechanism__composite_overdetermination_reading, stakes_inflation(structural), 1944, 0.12).
narrative_ontology:measurement(gold_grid_24, gold_fiat_transition_mechanism__composite_overdetermination_reading, stakes_inflation(structural), 1985, 0.18).
narrative_ontology:measurement(gold_grid_25, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression(class), 1944, 0.18).
narrative_ontology:measurement(gold_grid_26, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression(class), 1985, 0.28).
narrative_ontology:measurement(gold_grid_27, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression(individual), 1944, 0.15).
narrative_ontology:measurement(gold_grid_28, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression(individual), 1985, 0.38).
narrative_ontology:measurement(gold_grid_29, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression(organizational), 1944, 0.22).
narrative_ontology:measurement(gold_grid_30, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression(organizational), 1985, 0.32).
narrative_ontology:measurement(gold_grid_31, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression(structural), 1944, 0.12).
narrative_ontology:measurement(gold_grid_32, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression(structural), 1985, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.22).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel gold_fiat_transition_mechanism. The sibling readings (automatic_constraint_reading, creditor_discipline_reading) decompose the same historical event by different causal attribution. This reading (composite_overdetermination_reading) asserts the event is not a unified transition but a convergence of independent structural shifts. All three readings have the same rough interval (1944-1985) and address the same historical phenomenon, but structure the constraint differently. Readers should compare all three stories to understand the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized, 0.72).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
