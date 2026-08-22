% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Bretton Woods Dissolution via Contingent Policy Choice
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   Under this reading, the 1971 transition from fixed exchange rates and
 *   gold convertibility to floating rates was a deliberate policy choice by
 *   the U.S. executive. The decision was not forced by inexorable structural
 *   collapse — the system was creaking under Triffin's dilemma (the dollar
 *   serving as both national and reserve currency) and gold outflows, but
 *   these constraints were manageable through alternative policy paths:
 *   negotiated revaluation, gradual gold-price adjustment, or intensified
 *   adjustment mechanisms within the existing frame. Nixon chose instead to
 *   unilaterally suspend convertibility, granting the U.S. monetary autonomy
 *   at the cost of the Bretton Woods coordination architecture. This reading
 *   emphasizes counterfactual viability: the choice could have gone
 *   otherwise. The beneficiary (U.S. executive and multinational capital)
 *   gained policy freedom; the victims (creditor nations,
 *   fixed-rate-dependent economies) lost the coordination anchor. The
 *   constraint classified as tangled rope because the system coordinated
 *   exchange rate stability and capital flows (genuine coordination function)
 *   while extracting the cost of U.S. monetary discipline from those who held
 *   dollar reserves or depended on the peg (asymmetric extraction). The
 *   measurement series shows extractiveness rising sharply in 1971 (from 0.42
 *   to 0.58) as the choice point itself — before the full post-transition
 *   equilibrium — reflecting the unilateral reimposition of terms.
 *
 * KEY AGENTS:
 *   - united_states_executive: Chose to exit; gained monetary autonomy; beneficiary seat
 *   - bretton_woods_gold_standard_creditors: Held dollar claims suddenly devalued; victim seat; trapped
 *   - foreign_central_banks: Forced to revalue or accept exchange losses; victim seat; constrained
 *   - fixed_exchange_rate_anchor_dependents: Lost coordination anchor; victim seat; constrained
 *   - u_s_multinational_corporations: Benefited from export competitiveness via devaluation; secondary beneficiary
 *   - u_s_labor_unions: Short-term beneficiary (protected employment); conditional role
 *   - academic_economists_bretton_woods_defenders: Excluded from the decision; analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.68).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.52).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Bretton Woods Dissolution via Contingent Policy Choice").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '73950e77-78df-4496-aae3-bd84a6bcc276').
narrative_ontology:cs_kernel_codification('73950e77-78df-4496-aae3-bd84a6bcc276', fixed_text).
narrative_ontology:cs_authority_grounding('73950e77-78df-4496-aae3-bd84a6bcc276', lineage).
narrative_ontology:cs_interpretation_layer_present('73950e77-78df-4496-aae3-bd84a6bcc276').
narrative_ontology:cs_reading_relation('73950e77-78df-4496-aae3-bd84a6bcc276', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('73950e77-78df-4496-aae3-bd84a6bcc276', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('73950e77-78df-4496-aae3-bd84a6bcc276', foundational, policy_decision_agency_causal_node).
narrative_ontology:cs_axiom_status(policy_decision_agency_causal_node, holdable).
narrative_ontology:cs_axiom_grounding('73950e77-78df-4496-aae3-bd84a6bcc276', policy_decision_agency_causal_node, instrumental).
narrative_ontology:cs_axiom('73950e77-78df-4496-aae3-bd84a6bcc276', foundational, counterfactual_viability_of_bretton_woods_persistence).
narrative_ontology:cs_axiom_status(counterfactual_viability_of_bretton_woods_persistence, holdable).
narrative_ontology:cs_axiom_grounding('73950e77-78df-4496-aae3-bd84a6bcc276', counterfactual_viability_of_bretton_woods_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('73950e77-78df-4496-aae3-bd84a6bcc276', bretton_woods_fixed_exchange_rate_anchor).
narrative_ontology:cs_drift_state('73950e77-78df-4496-aae3-bd84a6bcc276', august_1971_suspension_of_convertibility, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('73950e77-78df-4496-aae3-bd84a6bcc276', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, united_states_executive).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, bretton_woods_gold_standard_creditors).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, foreign_central_banks).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, fixed_exchange_rate_anchor_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, u_s_multinational_corporations).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, u_s_labor_unions).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, u_s_labor_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authorized the August 1971 decision to suspend gold convertibility and abandon the fixed exchange rate peg. The executive branch (Nixon, Kissinger, Treasury) framed the decision as technical monetary adjustment responding to a loss of gold reserves and balance-of-payments strain. In reality, the decision granted the U.S. monetary policy autonomy it had lacked under the Bretton Woods constraint: the ability to print currency without redemption obligation and to depreciate the dollar strategically. This reading treats the decision as a choice among viable alternatives, not as a forced response.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, united_states_executive, agenda_setter,
    institutional, biographical, arbitrage, global).

% Held dollar reserves under the pledge of gold convertibility at $35/oz. When convertibility was suspended, those reserves lost the backing that justified their valuation. Central banks that had accumulated dollars as safe-haven assets discovered the U.S. had unilaterally rewritten the contract. Their recourse was limited: calling in gold when supplies were depleted, or accepting depreciated claims on future U.S. production. Trapped because the dollar was the global reserve medium; exiting meant abandoning the entire apparatus that made their reserves fungible.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, bretton_woods_gold_standard_creditors, payer,
    powerful, biographical, trapped, global).

% Implemented the Bretton Woods system's fixed exchange rate regime domestically, pegging their own currencies to the dollar. The suspension of gold convertibility and subsequent devaluation of the dollar against other currencies forced a rapid recalibration of monetary policy. European central banks suddenly had to choose: revalue their own currencies (raising import costs and domestic inflation), or maintain the peg and accept foreign exchange losses. Their exit options were constrained by the lack of a coordinated alternative.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, foreign_central_banks, payer,
    organized, generational, constrained, global).

% Export-oriented economies and developing nations that had organized trade, capital flows, and domestic price expectations around the anchor of fixed dollar parity. The transition to floating rates introduced exchange rate volatility that raised hedging costs, disrupted trade pricing, and created uncertainty for long-term investment. Unlike major creditors, they had no leverage to negotiate; they simply absorbed the external shock.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, fixed_exchange_rate_anchor_dependents, payer,
    moderate, biographical, constrained, global).

% Historical actors (Keynes, White, others) who designed the system had embedded assumptions about U.S. willingness to honor its redemption obligation as the foundation of the entire architecture. They are no longer present to contest the dissolution, but their framing was invoked by both the system's defenders and its critics.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, bretton_woods_system_architects, observer,
    analytical, civilizational, analytical, global).

% The dollar devaluation made U.S. exports cheaper and more competitive, boosting profitability of multinational firms with significant export exposure. The move to floating rates allowed the dollar to depreciate strategically without the gold-standard discipline that would have required domestic deflation to rebalance. They benefited from monetary policy autonomy without bearing the direct cost of the rebalancing.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, u_s_multinational_corporations, beneficiary,
    powerful, biographical, mobile, global).

% In the short term, the devaluation protected U.S. manufacturing employment by raising the competitiveness of domestic production. Unions could negotiate wages that reflected stronger labor-market tightness. Long-term, however, the volatility and subsequent dollar weakness relative to other major currencies created sustained imported inflation that eroded real wages — making this a conditional beneficiary role with delayed victim elements.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, u_s_labor_unions, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, u_s_labor_unions, payer).

% The IMF was designed to defend the fixed exchange rate system and provide liquidity to member states defending their pegs. The unilateral U.S. decision to exit the system undermined the Fund's core mandate and required a fundamental restructuring of its role. From observer vantage point, the transition revealed the system's fragility to great-power defection.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, imf_governance_structure, observer,
    institutional, generational, analytical, global).

% Economists who argued the system could be reformed and defended through policy discipline were sidelined by the executive decision. Their arguments for adjustment within the constraint were never given a chance to prove viability; the constraint was dissolved before the evidence could accumulate.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, academic_economists_bretton_woods_defenders, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, united_states_executive).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods provided a stable, multilaterally agreed exchange rate anchor, centralized gold redemption discipline, and coordinated capital flow governance — solving the problem of inter-war currency chaos and competitive devaluation by binding monetary policy to a gold standard that constrained unilateral action.
% TRANSFER_FUNCTION: Moves the burden of monetary discipline from the U.S. (which could not sustain gold redemption at fixed parity with full employment) to creditor nations (which held depreciated dollar claims) and to fixed-rate-dependent economies (which lost the anchor and absorbed volatility). The U.S. transferred its constraint outward, gaining autonomy at others' cost.
% ABSENT_VOICES: The labor movements and export-dependent communities of creditor nations (particularly Western Europe and Japan) that would have objected to unilateral abrogation of the Bretton Woods compact were not seated at the Nixon-Kissinger decision table. Developing nations with no reserve accumulation faced fait accompli currency revaluation they did not negotiate.
% DISAPPEARANCE_RATIONALE: If Nixon had chosen the alternative — honoring the gold redemption pledge or negotiating a coordinated revaluation within the Bretton Woods framework — the entire post-1971 monetary order would not have emerged. No floating rates, different inflation trajectories, different geopolitical alignments (dollar hegemony was underwritten by the transition; had it not occurred, competing reserve currencies would have remained structurally viable). The world does not rearrange into the old system, but into a fundamentally different post-transition equilibrium.
% FOUNDING_PROBLEM: The Bretton Woods system was founded to prevent inter-war competitive devaluation and currency instability by anchoring all major currencies to gold and coordinating adjustment through a rules-based framework. The founding problem was real: 1930s currency wars had been catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: The U.S. Treasury and academic monetarists of the 1970s attested the problem had shifted: the gold-standard discipline was now THE problem, forcing deflationary policy when Keynesian stimulus was preferred. Creditor-nation economists and central banks attested the founding problem of currency instability remained live and Bretton Woods had successfully contained it — the system's dissolution was choice, not necessity. Empirically, post-1973 floating rates produced higher volatility, not lower; the founding problem (chaotic currency swings) re-emerged in new form, corroborating the creditor reading.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply across the interval (0.15 → 0.68), with the 1971 inflection point marking the decision itself. In the early Bretton Woods period (1944–1960s), the constraint's extractiveness was lower because the U.S. was genuinely constrained by the gold standard and accepted the rules; both creditors and the U.S. were bound. After 1971, extractiveness measures the regime's operation under the new terms: the U.S. free to depreciate, creditors bearing currency losses and volatility, developing economies absorbing external shocks. Theater ratio (performative vs. functional) remains low throughout because even when the system was maintained, it worked; the performance was not theatrical. It rises slightly post-1971 (0.18) because much subsequent policy discussion was rhetorical defense of the transition's inevitability, but the core mechanism was functional (floating rates did operate as a market). Suppression is moderate (0.52 final) because the system was not sustained by coercion — it collapsed when the U.S. chose to exit. The enforcement burden was pre-collapse: maintaining the gold peg against speculative pressure required capital controls and policy coordination that were increasingly costly. This reading treats the suppression as the effort required to hold the constraint in place before the choice point, not afterward (post-transition, floating rates are sustained by market forces, not suppression). The accessibility-collapse metric (0.38) reflects that alternatives to Bretton Woods existed and were intellectually viable (floating-rate economists, regional blocs, SDR-based systems) — the collapse of alternatives was contingent on the political choice to exit, not structural inevitability.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. executive seat and the creditor-nation seat compute differently. From the executive perspective, the decision was rational response to binding constraints (gold depletion, Triffin dilemma pressure, full-employment goals) — the system became unsustainable and the choice was forced. From the creditor-nation perspective, the decision was a unilateral breach of contract, driven by U.S. preference for autonomy, not necessity — alternatives existed and were foreclosed by U.S. choice. The engine computes these divergent readings from the structural data: the executive's arbitrage-level exit options (could print money, devalue at will, reshape the regime) versus the creditors' trapped status (could not exit the dollar system without dismantling their own monetary frameworks). The divergence is real and measured; the claim/metric independence discipline keeps them separate from the engine's computed type.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. executive is the beneficiary: the decision granted it the freedom to conduct monetary policy unconstrained by gold redemption. Directionality is low (d ≈ 0.15–0.25) — the executive is subsidized by the regime shift, gaining options it lacked before. Creditor nations and fixed-rate-dependent economies are the victims: they lost the anchor (Bretton Woods) and absorbed volatility (floating rates). Directionality is high (d ≈ 0.75–0.85) — they bore the extraction costs without negotiating them. U.S. multinationals and labor are secondary beneficiaries: they benefited from the devaluation without engineering it. The divergence between beneficiary and target seats is the foundation of the tangled-rope classification: coordination function (the system did coordinate exchange rates and capital flows) plus asymmetric extraction (the U.S. extracted its way out at others' expense).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of Bretton Woods was currency instability and competitive devaluation (1930s lesson). This problem remained substantively live in 1971 — the system had successfully prevented such chaos for 25+ years. The choice to dissolve it was not mandatrophy (obsolete founding problem), but rather a conscious decision to prioritize a different goal (U.S. monetary autonomy) over the original mandate (exchange rate stability and cooperation). The tension between the founding mandate and the actual transition is the kernel of the contingent-choice reading: had a different actor (say, a supranational monetary authority not beholden to national interest) been steering, or had U.S. priorities been ordered differently (full-employment sacrifice to maintain gold discipline), the constraint would have persisted. The reading does NOT claim the constraint becomes mandatrophic; it claims the constraint was chosen to be dissolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_negotiated_reform,
    'Could the Bretton Woods system have been reformed through negotiated multilateral adjustment (Smithsonian Agreement-style coordination) rather than unilateral U.S. abrogation?',
    'Comparative institutional analysis of the Smithsonian Agreement (December 1971) and other attempted coordination frameworks: did they fail because alternatives were impossible, or because the U.S. preferred the freedom of floating rates?',
    'If negotiated reform was genuinely viable and was foreclosed by U.S. preference for autonomy, the contingent-choice reading gains strength. If reform was impossible (structural constraints were binding), the reading shifts toward hybrid or overdetermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_negotiated_reform, empirical, 'Whether negotiated reform was a viable alternative to unilateral transition.').

omega_variable(
    doctrine_vs_structure_in_policy_motivation,
    'Did the U.S. executive decision reflect a genuine belief that the gold standard was economically untenable (structural inevitability reading), or a deliberate choice to prioritize full-employment and inflation objectives over Bretton Woods obligations (preference/doctrine reading)?',
    'Archival evidence from Nixon-era policy memos, oral histories with Kissinger/Connally, and contemporaneous academic debate: what was the articulated reasoning for the choice?',
    'If the decision was framed as necessary response to binding constraints, the contingent-choice reading is weakened (the constraint becomes more hybrid). If it was framed as strategic autonomy-seeking, the reading is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_structure_in_policy_motivation, empirical, 'Whether the decision was necessity-driven or preference-driven.').

omega_variable(
    beneficiary_alignment_with_causation,
    'Did the U.S. executive benefit from the transition because it was necessary (everyone would have benefited from reform), or because it was unilaterally advantageous (the executive captured gains that would have been shared under negotiated reform)?',
    'Welfare-economic analysis of counterfactual scenarios: what would have been the distribution of costs and benefits under negotiated revaluation versus unilateral floating-rate transition?',
    'If the executive captured genuinely extractive gains (higher than in negotiated alternatives), the contingent-choice and tangled-rope classifications are confirmed. If the transition was merely the least-bad option, extraction is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_alignment_with_causation, conceptual, 'Whether the U.S. benefited through choice or through necessity.').

omega_variable(
    sibling_reading_foreclosure_empirical_test,
    'Is the overdetermined-collapse reading foreclosed by evidence that alternatives were viable, or do the readings coexist as different emphases on the same causal sequence?',
    'Logical reconstruction: if alternatives were genuinely viable and the decision was chosen, overdetermined reading is foreclosed (its core premise—inevitability—is false). If all paths led to floating rates eventually but through different mechanisms, readings coexist.',
    'Foreclusion would establish the contingent-choice reading''s monopoly on the causal structure. Coexistence would mean both readings are live framings of the same kernel, differing only in emphasis (contingency vs. inevitability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_empirical_test, conceptual, 'Whether the contingent-choice reading forecloses or coexists with the overdetermined-collapse reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1944, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__contingent_choice_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement_basis(tran_tr_t1944, observed).
narrative_ontology:measurement(tran_tr_t1955, transition_causality__contingent_choice_reading, theater_ratio, 1955, 0.08).
narrative_ontology:measurement_basis(tran_tr_t1955, observed).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__contingent_choice_reading, theater_ratio, 1965, 0.11).
narrative_ontology:measurement_basis(tran_tr_t1965, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.18).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).
narrative_ontology:measurement(tran_tr_t1975, transition_causality__contingent_choice_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement_basis(tran_tr_t1975, observed).
narrative_ontology:measurement(tran_tr_t1980, transition_causality__contingent_choice_reading, theater_ratio, 1980, 0.21).
narrative_ontology:measurement_basis(tran_tr_t1980, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__contingent_choice_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement_basis(tran_be_t1944, observed).
narrative_ontology:measurement(tran_be_t1955, transition_causality__contingent_choice_reading, base_extractiveness, 1955, 0.28).
narrative_ontology:measurement_basis(tran_be_t1955, observed).
narrative_ontology:measurement(tran_be_t1965, transition_causality__contingent_choice_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement_basis(tran_be_t1965, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.58).
narrative_ontology:measurement_basis(tran_be_t1971, observed).
narrative_ontology:measurement(tran_be_t1975, transition_causality__contingent_choice_reading, base_extractiveness, 1975, 0.65).
narrative_ontology:measurement_basis(tran_be_t1975, observed).
narrative_ontology:measurement(tran_be_t1980, transition_causality__contingent_choice_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement_basis(tran_be_t1980, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__contingent_choice_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement_basis(tran_su_t1944, observed).
narrative_ontology:measurement(tran_su_t1955, transition_causality__contingent_choice_reading, suppression_requirement, 1955, 0.38).
narrative_ontology:measurement_basis(tran_su_t1955, observed).
narrative_ontology:measurement(tran_su_t1965, transition_causality__contingent_choice_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement_basis(tran_su_t1965, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.48).
narrative_ontology:measurement_basis(tran_su_t1971, observed).
narrative_ontology:measurement(tran_su_t1975, transition_causality__contingent_choice_reading, suppression_requirement, 1975, 0.51).
narrative_ontology:measurement_basis(tran_su_t1975, observed).
narrative_ontology:measurement(tran_su_t1980, transition_causality__contingent_choice_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement_basis(tran_su_t1980, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__contingent_choice_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, floating_rate_volatility_extraction).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, dollar_hegemony_post_bretton_woods).

% DUAL FORMULATION NOTE:
% The transition_causality kernel decomposes into three constraint stories, each reading the same historical event (1971 collapse of Bretton Woods) through different causal lenses. The contingent_choice_reading anchors on Nixon's August decision as the causal node; the overdetermined_collapse_reading emphasizes structural inevitability (Triffin's dilemma, gold depletion); the hybrid_trigger_reading places structural contradictions + contingent pressure events on equal footing. Each story has a distinct ε and beneficiary/victim structure derived from its reading's premises. They are linked via network.affects_constraints to establish the constraint family and enable cross-reading comparison of how the same historical moment instantiates different structural configurations depending on causal framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
