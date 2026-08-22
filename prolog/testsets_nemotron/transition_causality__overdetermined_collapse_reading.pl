% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Fixed-Rate Regime Collapse (Overdetermined Structural Inevitability Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the overdetermined_collapse_reading of
 *   the transition_causality kernel. It models the Bretton Woods
 *   fixed-exchange-rate regime (1944-1971) as a Mountain constraint whose
 *   collapse was structurally inevitable due to multiple reinforcing
 *   contradictions: the Triffin Dilemma (reserve currency must run deficits
 *   to supply liquidity, undermining confidence in the anchor), the
 *   gold-convertibility peg at $35/oz incompatible with expanding dollar
 *   liabilities, the Eurodollar market's endogenous money creation outside
 *   the gold discipline, and the growing structural surplus of
 *   reserve-holding nations that could not be sterilized without
 *   contractionary pressure. The regime's extractiveness grew from 0.32 to
 *   0.88 over the interval as the coordination function (stable exchange
 *   rates for postwar reconstruction) was progressively overwhelmed by the
 *   extraction function (seigniorage to the US, rent to Eurodollar operators,
 *   forced accumulation by surplus countries). The constraint claimed
 *   Mountain status (natural law of monetary physics) but operated as a false
 *   summit: identifiable beneficiaries (US Treasury/Fed, commercial banks,
 *   petrodollar recyclers) extracted rents from the structural impossibility
 *   of maintaining convertibility at the fixed parity while supplying global
 *   liquidity.
 *
 * KEY AGENTS:
 *   - us_treasury_federal_reserve: Primary beneficiary (institutional/arbitrage) — issues reserve currency, collects seigniorage, controls the gold window
 *   - international_commercial_banks: Primary beneficiary (organized/arbitrage) — operates Eurodollar market, creates dollar liabilities unconstrained by gold discipline
 *   - petrodollar_recyclers: Beneficiary (institutional/mobile) — intermediates oil-surplus recycling into dollar assets after 1973, but positioned during the interval
 *   - eurodollar_market_operators: Beneficiary (organized/arbitrage) — offshore dollar creators who benefit from the regime's liquidity expansion without its discipline
 *   - surplus_country_central_banks: Victim (institutional/constrained) — forced to accumulate dollars they cannot convert at par without collapsing the system (Germany, Japan, later OPEC)
 *   - deficit_country_central_banks: Victim (institutional/constrained) — subject to adjustment burden without symmetric surplus-country obligation (UK, Italy, etc.)
 *   - fixed_exchange_rate_pegged_economies: Victim (organized/constrained) — developing and peripheral economies locked into parities that transmit US inflation/deflation
 *   - dollar_reserve_holders: Victim (organized/constrained) — central banks and private holders of dollar claims facing inevitable devaluation or default
 *   - gold_standard_advocates: Victim (moderate/trapped) — displaced by the regime's operation; their alternative was suppressed by the regime's dominance
 *   - analytical_observer: Observer (analytical/analytical) — sees the full structural trap from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.88).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.75).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Fixed-Rate Regime Collapse (Overdetermined Structural Inevitability Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, 'f001827e-bbba-4172-b8d0-6d09d3f5a7e3').
narrative_ontology:cs_kernel_codification('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', formalized).
narrative_ontology:cs_authority_grounding('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', extraction).
narrative_ontology:cs_interpretation_layer_present('f001827e-bbba-4172-b8d0-6d09d3f5a7e3').
narrative_ontology:cs_reading_relation('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', foundational, triffin_dilemma_is_inescapable_mountain).
narrative_ontology:cs_axiom_status(triffin_dilemma_is_inescapable_mountain, holdable).
narrative_ontology:cs_axiom_grounding('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', triffin_dilemma_is_inescapable_mountain, empirically_contingent).
narrative_ontology:cs_axiom('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', foundational, counterfactual_reform_paths_all_blocked_by_beneficiary_power).
narrative_ontology:cs_axiom_status(counterfactual_reform_paths_all_blocked_by_beneficiary_power, holdable).
narrative_ontology:cs_axiom_grounding('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', counterfactual_reform_paths_all_blocked_by_beneficiary_power, empirically_contingent).
narrative_ontology:cs_reference_frame('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', bretton_woods_original_design).
narrative_ontology:cs_drift_state('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', id_1968_gold_pool_collapse, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f001827e-bbba-4172-b8d0-6d09d3f5a7e3', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, us_treasury_federal_reserve).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, international_commercial_banks).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, petrodollar_recyclers).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, eurodollar_market_operators).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, surplus_country_central_banks).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, deficit_country_central_banks).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_exchange_rate_pegged_economies).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, dollar_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, gold_standard_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary reserve currency, controls the gold window at $35/oz, and collects seigniorage from global dollar demand. Sets the rules of the regime (convertibility, swap lines, capital controls enforcement) and can unilaterally suspend convertibility (August 1971). Benefits from the regime's liquidity provision function but also bears the political cost of the gold drain. Exit is arbitrage-grade: they can float the dollar, reprice gold, or redesign the system — they hold the monopoly on the reserve asset.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, us_treasury_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, us_treasury_federal_reserve, beneficiary).

% Operate the Eurodollar market (offshore dollar deposit/lending) centered in London, creating dollar liabilities unconstrained by US reserve requirements or gold backing. Profit from the spread between Eurodollar rates and regulated US rates, and from intermediating global dollar flows. Their business model depends on the regime's liquidity expansion without its discipline. Exit is arbitrage-grade: they can shift to other currencies, create synthetic dollars, or relocate offshore centers.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, international_commercial_banks, beneficiary,
    organized, biographical, arbitrage, global).

% After 1973, intermediate the recycling of oil-surplus dollars into US Treasuries and commercial bank deposits. During the Bretton Woods interval, they are positioned as the emerging coalition that will capture the post-collapse recycling rents. Their interest aligns with regime persistence until the pivot point, then with managed transition. Exit is mobile: they follow the reserve asset wherever it leads.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, petrodollar_recyclers, beneficiary,
    institutional, biographical, mobile, global).

% The specific subset of commercial banks and financial firms that actively create and trade Eurodollar liabilities. They benefit from the regime's expansion of global dollar liquidity (which fuels their balance sheets) while evading its discipline (gold convertibility, Regulation Q). They are the operational engine of the Triffin Dilemma's 'liquidity without discipline' dynamic. Exit is arbitrage-grade: their business is portable across jurisdictions and currencies.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, eurodollar_market_operators, beneficiary,
    organized, biographical, arbitrage, global).

% Central banks of persistent surplus countries (West Germany, Japan, Netherlands, later OPEC) forced to accumulate dollar reserves to maintain their fixed parities against the dollar. They cannot convert dollars to gold at $35/oz without collapsing the system (which would destroy their own export markets and reserve values). They bear the inflationary consequences of US monetary expansion and the capital loss on dollar holdings. Exit is constrained: unilateral revaluation hurts exports; collective action requires US cooperation; gold conversion triggers the collapse they seek to avoid.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, surplus_country_central_banks, payer,
    institutional, generational, constrained, global).

% Central banks of persistent deficit countries (UK, Italy, France pre-1968) subject to the asymmetric adjustment burden: they must contract to defend their parities, while surplus countries face no symmetric obligation to expand. They lose reserves, face speculative attacks, and depend on IMF conditionality. Exit is constrained: devaluation is politically costly and invites retaliation; floating is not permitted under the regime's rules until the very end.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, deficit_country_central_banks, payer,
    institutional, biographical, constrained, global).

% Developing and peripheral economies that peg to the dollar or sterling, importing US monetary policy without representation. They transmit US inflation/deflation directly, face sudden stops when US rates rise, and have no voice in the regime's governance. Exit is constrained: breaking the peg invites capital flight and loss of trade credit; maintaining it imports instability.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, fixed_exchange_rate_pegged_economies, payer,
    organized, biographical, constrained, global).

% Central banks and private institutions holding dollar-denominated claims (Treasuries, deposits, commercial paper) as reserves. They face inevitable capital loss either through devaluation (if the peg breaks) or inflation (if the US inflates to reduce real debt burden). They cannot diversify en masse without triggering the devaluation they fear. Exit is constrained: collective action problem — individual diversification accelerates the collapse; collective diversification requires coordination the regime prevents.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, dollar_reserve_holders, payer,
    organized, biographical, constrained, global).

% Economists, policymakers, and political movements advocating a return to a genuine gold standard (full convertibility, fixed parities, no discretionary monetary policy). They are excluded from the regime's operation because their alternative would require the US to abandon seigniorage and the Eurodollar banks to accept gold discipline. Their exit is trapped: their intellectual framework makes the regime's operation illegible as anything but fraud, but they lack the power to change it until the regime itself breaks.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, gold_standard_advocates, excluded,
    moderate, generational, trapped, global).

% The analytical seat that sees the full structural trap: the Triffin Dilemma as a Mountain constraint, the beneficiaries' extraction, the victims' entrapment, and the overdetermined convergence of causal pathways. This seat does not collect or pay; it classifies. Its exit is analytical: it can adopt any reading of the kernel.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable fixed-exchange-rate anchor for postwar reconstruction and trade expansion (1944-1958), solving the coordination problem of international payments without bilateral clearing or gold shipment for every transaction. The dollar-gold peg at $35/oz served as a credible nominal anchor when US gold reserves covered dollar liabilities.
% TRANSFER_FUNCTION: Moves seigniorage from global dollar holders to the US Treasury/Fed (via the reserve currency's deficit privilege), moves intermediation rents from regulated banking to Eurodollar operators (via offshore dollar creation), and moves adjustment costs from the US to surplus and deficit countries (via asymmetric adjustment burden). The extraction is the gap between the coordination service (stable parities) and the rent extracted (seigniorage + offshore rents + forced savings).
% ABSENT_VOICES: The Global South (non-OECD, non-aligned economies) had no seat at Bretton Woods and no voice in the regime's operation — they were price-takers of the dollar standard. The domestic US labor movement was excluded from the international monetary governance that shaped the trade and inflation environment affecting their wages. Future generations (post-1971) who inherit the fiat dollar system's instabilities are structurally absent from the 1944-1971 governance.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods fixed-rate constraint vanished overnight in any year 1944-1971, the world monetary system would rearrange: exchange rates would float or re-peg at new parities, the Eurodollar market would either collapse or become the new core dollar system, global trade would face currency volatility, and the US would lose its exorbitant privilege. The 1971 Nixon shock (closing the gold window) was precisely this disappearance — and the world did rearrange into the fiat dollar standard.
% FOUNDING_PROBLEM: Postwar monetary chaos: competitive devaluations, bilateral clearing, gold standard collapse, no trusted international payment system. The regime was built to provide stable exchange rates for reconstruction trade, a credible nominal anchor via gold convertibility, and a multilateral payments framework via the IMF.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (postwar monetary stability for reconstruction) is corroborated as dead by: (1) independent economic historians (Eichengreen, Bordo, James) documenting that European reconstruction was complete by 1958 and the regime's original purpose was fulfilled; (2) the IMF's own internal reviews (1960s) acknowledging the regime had outlived its founding function; (3) the Committee of Twenty (1972) convened precisely because the founding problem was gone and a new system was needed. The US Treasury/Fed (beneficiaries) claimed the problem was still live to justify maintaining the regime — this is the mandatrophy signal. No corroborating source outside the beneficiary set attests the problem was still live after 1968.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 to 0.88 because the regime's coordination function (stable parities for trade reconstruction) was progressively hollowed out by the extraction function: the US supplied liquidity by running deficits (seigniorage), Eurodollar banks created dollars without gold backing (rent), and surplus countries were forced to accumulate claims they knew were unsustainable (forced savings). Suppression (0.75 at end) reflects the active maintenance of the $35/oz peg through the London Gold Pool (1961-1968), swap lines, moral suasion, and capital controls — all suppressing the market's discovery that the parity was fictive. Theater ratio remains low (0.22) because the coordination function was real and operationally genuine in the early period; the performative element grew only as the gap widened. Accessibility collapse (0.92) is near-total: once the Triffin Dilemma is understood, no alternative fixed-rate architecture can resolve the reserve-currency paradox without either abandoning the anchor or restricting capital mobility — both of which the regime's beneficiaries blocked. Resistance (0.18) is low because the constraint's victims (surplus central banks) were structurally complicit: they could not exit without triggering the collapse they feared, and the beneficiaries controlled the enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury/Fed seat (agenda_setter, institutional, arbitrage exit), the regime was a coordination mechanism they built and managed — the extraction was the price of leadership. From surplus central bank seats (payer, institutional, constrained exit), the same structure was an inescapable trap: they had to buy dollars to maintain their pegs, funding US deficits. From Eurodollar operator seats (beneficiary, organized, arbitrage exit), the regime was a profitable arbitrage: they issued dollar liabilities unconstrained by gold. The engine computes per-seat χ from these structural positions; the analytical observer sees the overdetermined convergence of all pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   US Treasury/Fed and Eurodollar operators are structural beneficiaries (d ≈ 0.15): they issue the reserve asset and offshore its creation, collecting seigniorage and intermediation rents. Surplus and deficit central banks, pegged economies, and dollar holders are structural targets (d ≈ 0.85): they bear the adjustment costs, hold the devaluing claims, and cannot exit without system collapse. Gold standard advocates are identity_locked targets (d ≈ 0.9): their epistemic commitment to the gold discipline made exit cognitively unavailable until the regime itself broke. The regime's Mountain claim (emerges_naturally: true) is the false summit — the beneficiaries benefit from the perception that the collapse was inevitable natural law rather than a constructed constraint they maintained.
 *
 * MANDATROPHY ANALYSIS:
 *   The regime's founding problem (postwar monetary stability for reconstruction) was live 1944-1958, contested 1958-1968, and dead by 1968 — but the constraint persisted because its beneficiaries (US, Eurodollar banks) extracted enough to maintain it, while its victims (surplus countries) were trapped by the exit-cost asymmetry. The mandate atrophied into a pure extraction vehicle; the 'inevitability' narrative served to naturalize what was a political choice to prioritize US seigniorage and financial-sector rents over systemic reform. The overdetermined_collapse_reading correctly identifies the structural convergence but misattributes agency: the contradictions were real, but the timing and form of collapse were shaped by the beneficiaries' resistance to reform (e.g., rejection of the Triffin/Planck proposals, defense of the gold window until August 1971).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_inevitability,
    'Is the Triffin Dilemma a genuine natural law of monetary systems (Mountain) or a constructed constraint maintained by beneficiaries who blocked reform alternatives?',
    'Counterfactual analysis of whether any known fixed-rate architecture (e.g., Keynes''s bancor, SDR substitution account, symmetric adjustment rules) could have resolved the reserve-currency paradox without either abandoning the gold anchor or restricting capital mobility — and whether the beneficiaries'' political power blocked those alternatives.',
    'If natural law, the overdetermined_collapse_reading is correct and the Mountain classification holds (with FSM flag for beneficiaries). If constructed, the constraint is a false summit: a Snare or Tangled Rope whose beneficiaries naturalized their extraction as inevitability. This changes the classification of the 1944-1971 regime from Mountain to extractive construct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_inevitability, conceptual, 'Whether structural inevitability is ontological or political').

omega_variable(
    counterfactual_viability_of_reform_paths,
    'Were there technically viable reform paths (bancor, SDR substitution, symmetric adjustment, gold price adjustment) that could have extended the regime, and were they blocked by beneficiary power?',
    'Historical analysis of IMF reform proposals (1960s SDR design, 1967-68 Committee of Twenty precursors, Triffin''s own substitution account) and the political coalition that defeated each — specifically whether US veto and Eurodollar bank lobbying were decisive.',
    'If viable paths existed and were blocked, the ''inevitability'' is manufactured — the constraint is a Snare with Mountain camouflage. If no viable path existed even in principle, the Mountain claim is descriptively accurate (though FSM still triggers on beneficiaries).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_viability_of_reform_paths, empirical, 'Viability and blockade of reform alternatives').

omega_variable(
    kernel_reading_foreclosure,
    'Does the overdetermined_collapse_reading''s core premise (collapse was structurally inevitable, counterfactual viability near-zero) logically foreclose the contingent_choice_reading (collapse was avoidable policy choice) within a single analytical framework?',
    'Logical analysis of whether ''structurally inevitable'' and ''avoidable by different choices'' can both be true of the same event in the same framework. If inevitability means zero viable counterfactuals, then contingent choice is foreclosed. If inevitability means high probability given initial conditions but non-zero counterfactual space, they coexist.',
    'If forecloses, the kernel has a genuine logical bifurcation — the readings cannot both be held by one analyst. If coexists_with, different parties can hold each reading simultaneously without internal contradiction. This determines the reading_relation in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between overdetermined and contingent readings').

omega_variable(
    beneficiary_structure_across_readings,
    'Do the sibling readings identify the same beneficiaries, or does each reading''s causal story implicate a different beneficiary coalition?',
    'Compare the beneficiary/victim structures across the three readings: contingent_choice_reading likely implicates US policymakers as choosers; hybrid_trigger_reading implicates trigger-event actors; overdetermined_collapse_reading implicates the structural beneficiaries (US, Eurodollar banks) as maintainers. If beneficiary sets differ, the kernel''s extraction structure is reading-dependent.',
    'If beneficiary structures diverge, the kernel itself has multiple extraction configurations — each reading instantiates a different constraint with different χ profiles. This validates the ε-invariance decomposition: one kernel, multiple constraints, each with its own ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_across_readings, conceptual, 'Whether beneficiary structure is invariant across readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(tran_tr_t1950, transition_causality__overdetermined_collapse_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(tran_tr_t1958, transition_causality__overdetermined_collapse_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(tran_tr_t1960, transition_causality__overdetermined_collapse_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(tran_tr_t1964, transition_causality__overdetermined_collapse_reading, theater_ratio, 1964, 0.16).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__overdetermined_collapse_reading, theater_ratio, 1968, 0.19).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.22).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.32).
narrative_ontology:measurement(tran_be_t1950, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1950, 0.41).
narrative_ontology:measurement(tran_be_t1958, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1958, 0.55).
narrative_ontology:measurement(tran_be_t1960, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1960, 0.62).
narrative_ontology:measurement(tran_be_t1964, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1964, 0.73).
narrative_ontology:measurement(tran_be_t1968, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1968, 0.81).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement(tran_su_t1950, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement(tran_su_t1958, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1958, 0.55).
narrative_ontology:measurement(tran_su_t1960, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(tran_su_t1964, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1964, 0.68).
narrative_ontology:measurement(tran_su_t1968, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1968, 0.72).
narrative_ontology:measurement(tran_su_t1971, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__overdetermined_collapse_reading, 0.15).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, triffin_dilemma_mountain).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, eurodollar_market_emergence).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, petrodollar_recycling_system).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the transition_causality kernel. The contingent_choice_reading and hybrid_trigger_reading are sibling constraints with different ε values and different beneficiary/victim structures. The Triffin Dilemma itself (triffin_dilemma_mountain) is the upstream Mountain constraint that this reading invokes as the primary causal pathway. The Eurodollar market and petrodollar system are downstream constraints that this regime's collapse enabled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__overdetermined_collapse_reading, institutional, 0.15).
constraint_indexing:directionality_override(transition_causality__overdetermined_collapse_reading, organized, 0.2).
constraint_indexing:directionality_override(transition_causality__overdetermined_collapse_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
