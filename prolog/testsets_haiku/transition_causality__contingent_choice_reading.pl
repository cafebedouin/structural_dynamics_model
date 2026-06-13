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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Bretton Woods Collapse as Contingent Policy Decision (Nixon Choice Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of a contested kernel
 *   about the Bretton Woods transition. The kernel is the historical event:
 *   the August 15, 1971 suspension of U.S. dollar-gold convertibility and the
 *   subsequent collapse of fixed exchange rates. Three structural readings
 *   exist: (1) contingent_choice_reading (THIS STORY) — Nixon's decision was
 *   avoidable; counterfactual scenarios where the peg persisted are coherent
 *   and plausible. (2) hybrid_trigger_reading — structural contradictions
 *   (Triffin dilemma, Vietnam inflation, capital outflows) accumulated
 *   inexorably, but required specific contingent trigger events (e.g.,
 *   Bundesbank confidence loss, British pound pressure in 1971) to actualize
 *   the collapse. (3) overdetermined_collapse_reading — the system was
 *   structurally unsustainable; the transition was inevitable regardless of
 *   policy choice. This story authors the transition as a policy choice under
 *   the contingent-choice frame: the Nixon administration faced genuine
 *   alternatives (defend the peg through capital controls and austerity,
 *   negotiate a realigned gold price, implement an orderly international
 *   agreement to broaden reserve currencies) and selected unilateral
 *   suspension because it maximized U.S. monetary autonomy and benefited U.S.
 *   corporations. The ε-invariance principle requires that each reading
 *   describe a different constraint with a different beneficiary structure,
 *   even though all three readings describe the same historical event.
 *
 * KEY AGENTS:
 *   - nixon_administration: Primary agenda-setter; made the August 1971 decision to suspend convertibility
 *   - us_treasury_department: Institutional beneficiary; gained monetary policy autonomy after the peg broke
 *   - us_multinational_corporations: Beneficiary; gained export competitiveness and arbitrage opportunities under floating rates
 *   - allied_central_banks: Victims; held devalued dollar reserves; excluded from the decision
 *   - bretton_woods_institutional_complex: Victim with identity-lock; institutional mandate dissolved by the transition
 *   - developing_nations_with_dollar_debt: Powerless victims; debt service burden increased by currency depreciation
 *   - bretton_woods_system_architects: Analytical observer; design assumptions about the system's sustainability are contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.62).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.41).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Bretton Woods Collapse as Contingent Policy Decision (Nixon Choice Reading)").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, 'ddfe47b6-38bf-4d84-84a0-44092954110b').
narrative_ontology:cs_kernel_codification('ddfe47b6-38bf-4d84-84a0-44092954110b', fixed_text).
narrative_ontology:cs_authority_grounding('ddfe47b6-38bf-4d84-84a0-44092954110b', extraction).
narrative_ontology:cs_interpretation_layer_present('ddfe47b6-38bf-4d84-84a0-44092954110b').
narrative_ontology:cs_reading_relation('ddfe47b6-38bf-4d84-84a0-44092954110b', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddfe47b6-38bf-4d84-84a0-44092954110b', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('ddfe47b6-38bf-4d84-84a0-44092954110b', foundational, policy_decision_primary_causal_node).
narrative_ontology:cs_axiom_status(policy_decision_primary_causal_node, holdable).
narrative_ontology:cs_axiom_grounding('ddfe47b6-38bf-4d84-84a0-44092954110b', policy_decision_primary_causal_node, empirically_contingent).
narrative_ontology:cs_axiom('ddfe47b6-38bf-4d84-84a0-44092954110b', foundational, counterfactual_peg_defense_feasibility).
narrative_ontology:cs_axiom_status(counterfactual_peg_defense_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('ddfe47b6-38bf-4d84-84a0-44092954110b', counterfactual_peg_defense_feasibility, empirically_contingent).
narrative_ontology:cs_axiom('ddfe47b6-38bf-4d84-84a0-44092954110b', secondary, institutional_power_supersedes_structural_inevitability).
narrative_ontology:cs_axiom_status(institutional_power_supersedes_structural_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('ddfe47b6-38bf-4d84-84a0-44092954110b', institutional_power_supersedes_structural_inevitability, deontological).
narrative_ontology:cs_reference_frame('ddfe47b6-38bf-4d84-84a0-44092954110b', bretton_woods_gold_anchored_fixed_rates).
narrative_ontology:cs_drift_state('ddfe47b6-38bf-4d84-84a0-44092954110b', august_1971_unilateral_suspension, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ddfe47b6-38bf-4d84-84a0-44092954110b', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_monetary_policy_autonomy).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_capital_outflow_suppressors).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, fixed_exchange_rate_system_dependents).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, dollar_reserve_currency_holders).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, allied_central_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_treasury_department).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_multinational_corporations).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, bretton_woods_institutional_complex).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, developing_nations_with_dollar_debt).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, human_agency_in_monetary_systems).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, policy_choice_over_structural_determinism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faced mounting U.S. gold outflows, persistent trade deficits, and Vietnam War inflation pressures. Held genuine alternatives: defend the peg through capital controls and austerity, negotiate a realigned gold price with allies, or implement orderly multilateral agreement to broaden reserve currencies. Selected unilateral suspension of convertibility on August 15, 1971. This decision maximized U.S. monetary policy discretion and enabled independent macroeconomic management without the gold-stock constraint. Captured full policy choice; bore no cost from the transition.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, nixon_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Primary institutional beneficiary of the transition. Gained unrestricted monetary and fiscal policy autonomy once the gold peg was severed. No longer required to balance external balance-of-payments constraints against domestic inflation and unemployment priorities. Collected the direct benefit of policy discretion without bearing transition costs. Administered the Smithsonian Agreement (Dec. 1971) and subsequent floating-rate system with U.S.-favorable terms.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_treasury_department, beneficiary,
    institutional, generational, arbitrage, global).

% Benefited substantially from the floating-rate transition. Dollar depreciation (1971-1973) enhanced U.S. export competitiveness; currency volatility created arbitrage opportunities for firms with international balance sheets. Under the fixed peg, they had absorbed the cost of dollar overvaluation and faced capital repatriation restrictions. Floating rates shifted currency risk onto foreign exchange markets and other parties.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_multinational_corporations, beneficiary,
    powerful, generational, mobile, global).

% Held dollar reserves under the assumption of gold backing (implied by Bretton Woods rules). August 15, 1971 suspension created sudden, involuntary devaluation of their foreign exchange reserves. Bundesbank, Banque de France, Bank of Japan all experienced reserve losses. Excluded from the decision-making process; learned of the suspension through public announcement rather than prior consultation. Constrained exit: could not abandon dollar reserve holdings without losing their status in the international system.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, allied_central_banks, payer,
    institutional, generational, constrained, global).

% IMF, World Bank, and GATT framework were architecturally designed to operate within the fixed-rate Bretton Woods arrangement. The August 1971 transition forced fundamental institutional restructuring. IMF governance remained U.S.-dominated, but the mandate shifted from fixed-rate coordination to floating-rate management. Identity-locked: the institutions' legitimacy derived from their role in the post-war Bretton Woods order; the transition dissolved that founding mandate and forced them to reinvent their function around new monetary arrangements (SDR development, floating-rate surveillance).
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, bretton_woods_institutional_complex, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, bretton_woods_institutional_complex, observer).

% Held substantial dollar-denominated foreign debt incurred under the assumption of stable exchange rates. Post-1971 dollar depreciation (relative to commodity prices and gold) increased real debt service burdens. Trapped: could not diversify currency exposure or influence U.S. monetary choices; faced deteriorating terms of trade as commodity prices spiked in the mid-1970s. No voice in the August 1971 decision; learned of the transition as external fact.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, developing_nations_with_dollar_debt, payer,
    powerless, biographical, trapped, global).

% Economists and policymakers who argued for defense of the peg through restructured gold pricing (revaluation above $35/oz), expanded gold backing (return to gold standard), or international agreement to broaden gold-based anchoring. Excluded from the August 1971 decision; mounted real but unsuccessful opposition. Constrained: their ability to implement alternative frameworks was limited by U.S. institutional dominance and the fait accompli of the unilateral suspension.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_standard_advocates, excluded,
    moderate, civilizational, constrained, global).

% Keynes, White, and subsequent architects of the fixed-rate system designed it to solve the competitive-devaluation problem of the 1930s and to enable post-war recovery. From this analytical seat, the transition appears as falsification of the system's core design assumptions. The reading contests whether those assumptions were structurally unsustainable (supporting the overdetermined_collapse reading) or merely abandoned for policy convenience (supporting the contingent_choice reading). Analytical seat with full visibility but no power to influence events.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, bretton_woods_system_architects, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, us_treasury_department).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods coordinated post-war international exchange rates and capital flows by anchoring all currencies to the dollar, the dollar to gold at a fixed price ($35 per ounce), and gold to a national reserve standard. This solved the problem of competitive devaluation from the 1930s, enabled trade reconstruction, and created a common reference point for international capital flows. The coordination function was substantive: it reduced exchange-rate uncertainty, facilitated trade growth, and provided confidence that currency values would remain stable.
% TRANSFER_FUNCTION: The transition moved policy autonomy from the U.S. (constrained by the gold peg and reserve obligation) to the U.S. Treasury and Federal Reserve (freed to conduct independent monetary and fiscal policy). It also transferred wealth from dollar-reserve holders (allied central banks, developing nations with dollar debt) to U.S. policymakers and multinational corporations. The constraint extracted U.S. policy flexibility and transferred it to unilateral U.S. discretion; it extracted reserve asset values from foreign central banks and transferred them to U.S. interests.
% ABSENT_VOICES: Gold standard advocates, structuralist economists who viewed floating rates as inferior coordination mechanisms, and developing nations that would bear debt-service burdens from post-transition currency volatility were excluded from the August 1971 decision. They mounted opposition and alternative analyses afterward, but were not consulted when the choice was made. The decision was made by the Nixon administration in closed session and announced as a fait accompli.
% DISAPPEARANCE_RATIONALE: If the Nixon administration had selected a different policy alternative in August 1971 — negotiated realignment, defense of the peg through capital controls and austerity, or multilateral agreement to broaden reserve currencies — the Bretton Woods fixed-rate system would have persisted (at least through the 1970s). The floating-rate order that emerged, the petrodollar system, currency volatility, and the subsequent debt crises of the 1980s would not have taken their actual form. A substantial portion of the post-1971 international monetary architecture depends on the contingent choice to suspend convertibility unilaterally.
% FOUNDING_PROBLEM: Post-World War II international monetary chaos: competitive devaluation, capital flight, bilateral trading blocs, and currency instability threatened post-war recovery and trade growth. Bretton Woods was architected to solve this by creating a rules-based, gold-anchored system with the dollar as the anchor currency and fixed exchange rates for all members.
% FOUNDING_PROBLEM_CORROBORATION: The U.S. Treasury and Federal Reserve argue that the founding problem (international monetary instability) had accumulated additional contradictions by 1971 that made the peg unsustainable (Triffin dilemma: the dollar could not simultaneously be a stable reserve currency and a flexible domestic currency under fixed gold backing). This reading disputes that framing on empirical grounds: structuralist economist Robert Triffin himself argued the Triffin dilemma was foreseeable but solvable with policy adjustment (expanded SDR backing, negotiated gold-price adjustment, capital controls). No independent economist or allied central bank — outside the benefiting circles of the U.S. policymakers — corroborates the claim that the transition was structurally forced rather than chosen for autonomy reasons.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).

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
 *   EXTRACTIVENESS (0.62 at interval end): This reading measures extraction as the U.S. gain in monetary autonomy coupled with the losses borne by reserve holders and dollar debtors. The transition transferred policy discretion from the constrained fixed-peg regime to the floating-rate regime where U.S. preferences dominated. This is classifiable as extraction because (a) the beneficiaries (U.S. Treasury, U.S. corporations) gained directly from the decision, (b) the victims (allied central banks, developing nations) bore the cost involuntarily, and (c) the decision was unilateral and non-negotiated. Extract value rises over time (0.35 → 0.62) because the petrodollar system stabilized and U.S. monetary dominance consolidated — early uncertainty about the transition outcome resolved in favor of continued U.S. advantage. SUPPRESSION (0.41, stable): Suppression is moderate because the transition required active enforcement (capital controls had to be lifted, alternative peg proposals had to be rejected, allied objections had to be overridden), but resistance to the floating-rate system was not sustained long-term. Developing nations and structuralist economists mounted real objections, but the U.S. institutional power suppressed alternative frameworks (IMF governance remained U.S.-dominated, the petrodollar replaced the gold-backed dollar as the new anchor). THEATER_RATIO (0.28, rising): Theater rises from 0.08 to 0.28 because post-transition U.S. framing emphasized the inevitability of the collapse (the Triffin dilemma narrative, structural crisis language) rather than the contingent policy choice. This re-narration functions as cover for a discretionary decision that benefited specific interests. ACCESSIBILITY_COLLAPSE (0.48): Alternatives were not completely collapsed at the choice point — negotiated realignment, gold price adjustment, and extended capital controls were all live options. The collapse occurred after the choice was made: once floating rates began, the return to pegged rates became institutionally harder to imagine. RESISTANCE (0.55): Substantial real resistance existed: Bundesbank pressure, allied objections, gold-standard advocates, and structuralist economists all mounted real alternatives to unilateral suspension. The Nixon administration overrode this resistance through institutional power, not through consensus.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. Treasury and Nixon administration perceive this constraint as a rational escape from an unsustainable system — they frame the transition as inevitable structural collapse forced on them by external contradictions. From this seat, the decision was forced and the extraction is coincidental. Allied central banks and developing nations perceive the same event as a unilateral policy seizure of monetary dominance — the constraint persists because U.S. institutional power enforces floating rates and continues to set monetary terms. From these seats, the decision was avoidable and the extraction is deliberate. The engine computes directionality per seat: U.S. institutional actors sit near d=0.0 (beneficiaries); allied and developing-nation seats sit near d=1.0 (targets). The perspectival gap is built into the structure, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Nixon administration and U.S. Treasury: d ≈ 0.0-0.15 (full beneficiaries). Gained policy autonomy, faced no constraint on their choices going forward, captured the extraction directly. Power=institutional, exit_options=arbitrage (could have chosen alternatives, deliberately selected the option maximizing their autonomy). U.S. multinationals: d ≈ 0.2 (beneficiaries). Gained export advantage and arbitrage opportunities from floating rates, but were not the decision-makers and shared the benefit indirectly. Power=powerful, exit_options=mobile (could have adapted to any monetary regime; benefited from this one). Allied central banks: d ≈ 0.85-1.0 (full targets). Held devalued reserves, were excluded from the decision, faced a worsening position. Power=institutional, exit_options=constrained (could not exit the dollar system without losing reserve currency role). Bretton Woods institutional complex: d ≈ 0.9 (target with identity-lock). Mandates dissolved, institutional roles were redefined by others, identity locked because they had no alternative to operate within the post-Bretton Woods order. Developing nations with dollar debt: d ≈ 1.0 (trapped targets). Debt service burdens increased, no voice in the decision, no exit from dollar exposure. Power=powerless, exit_options=trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading DOES declare mandatrophy resolution: the founding problem of Bretton Woods (post-war international monetary instability and competitive devaluation) was substantially solved by the system itself through the 1950s and 1960s. The transition occurred not because the coordination function failed (it had succeeded in enabling post-war recovery and trade growth) but because the structural arrangement began to extract from the core beneficiary (the U.S. gold stock) while also conferring benefits on other parties (allied central banks, which needed dollar reserves). The mandate had changed from 'stabilize international trade' to 'preserve U.S. monetary flexibility,' and these are incommensurable within a single framework. By 1971, the system was functioning well as coordination (hence the resistance to its collapse) but was being consciously rejected by the U.S. to escape the mandate constraint. This is exactly a mandatrophy case: the system was working, its primary beneficiary rejected it for autonomy reasons, and the transition was triggered by policy choice rather than functional failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_peg_defense,
    'Was the Bretton Woods peg genuinely unsustainable in 1971, or could the U.S. have preserved it through feasible policy alternatives (capital controls, austerity, negotiated realignment)?',
    'Retrospective economic analysis of U.S. balance-of-payments mechanics, gold reserve sufficiency under alternative policies, and allied central bank capacity to absorb continued dollar holdings. Comparative analysis of how other nations (Germany, Japan, Switzerland) managed their balance-of-payments constraints under fixed rates.',
    'If the peg could have been defended, this reading is validated: the transition was a discretionary policy choice, not a forced structural collapse. If defense was genuinely infeasible, the hybrid_trigger or overdetermined readings gain credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_peg_defense, empirical, 'Whether the Nixon decision was forced or chosen.').

omega_variable(
    policy_autonomy_as_benefit_or_necessity,
    'Was the U.S. monetary autonomy gained by ending the peg a deliberate extraction benefit, or a necessary response to domestic inflation and unemployment pressures that the fixed-peg constraint had made unmanageable?',
    'Analysis of U.S. unemployment and inflation data pre-1971, comparison to contemporaneous statements by Federal Reserve and Treasury officials about policy constraints, and retrospective assessment of whether domestic objectives were genuinely incompatible with peg maintenance.',
    'If autonomy was a deliberate extraction goal, this reading stands as written (policy choice for benefit). If autonomy was a response to binding domestic constraints, the reading shifts toward hybrid_trigger (structural pressure at the domestic macroeconomic level).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_autonomy_as_benefit_or_necessity, conceptual, 'Whether U.S. autonomy gain was deliberate extraction or necessary macroeconomic adjustment.').

omega_variable(
    allied_central_bank_alternatives,
    'Could allied central banks (Germany, France, Japan) have negotiated better terms if the Bretton Woods decision had been made multilaterally instead of unilaterally, or was U.S. institutional power so dominant that outcome would have been identical?',
    'Diplomatic history and counterfactual analysis of multilateral negotiation scenarios (e.g., if the Smithsonian Agreement of Dec. 1971 had preceded the August shock instead of following it).',
    'If negotiation could have yielded better reserve protection or transition terms, the unilateral character becomes a higher extraction differentiator. If U.S. power would have produced the same outcome regardless, extraction is present but lower-magnitude than this reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_central_bank_alternatives, empirical, 'Whether the unilateral character was essential to the extraction structure.').

omega_variable(
    reading_committer_ambiguity,
    'Is the transition best understood as a policy choice (contingent_choice_reading), or do structural contradictions (Triffin dilemma, U.S. inflation, capital flight) take causal primacy such that the decision was forced (hybrid_trigger or overdetermined_collapse)?',
    'This is a conceptual omega rooted in the committer structure — three coherent framings of the same historical event, none definitively falsifiable. The resolution lies in accepting that the kernel admits multiple readings, each with its own ε, its own beneficiary structure, and its own classification. This reading (contingent_choice) is valid as authored; the sibling readings are equally valid as separate constraint stories.',
    'If the contingent_choice frame is adopted, the Nixon decision is the primary causal node and extraction is deliberate U.S. policy. If hybrid_trigger is adopted, extraction is mitigated (structural forces bear some responsibility). If overdetermined_collapse is adopted, extraction vanishes (the transition was forced on all parties).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Committer frame ambiguity: contingent decision vs. structural inevitability.').

omega_variable(
    suppression_of_alternative_frameworks,
    'How much of the post-1971 monetary system''s structure reflects genuine superiority of floating rates, and how much reflects U.S. institutional power suppressing alternatives (gold-backed systems, multi-currency baskets, SDR-based arrangements)?',
    'Analysis of IMF governance, petrodollar system architecture, and counterfactual institutional designs that could have emerged if U.S. power had not been dominant. Comparative examination of why alternative reserve arrangements (SDR, ECU, commodity baskets) were adopted in some contexts and not others.',
    'High suppression of alternatives elevates the extraction characterization; low suppression suggests the floating system won through functional superiority rather than power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternative_frameworks, empirical, 'Degree to which floating-rate system dominance reflects power vs. superior function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__contingent_choice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tran_tr_t0, observed).
narrative_ontology:measurement(tran_tr_t2, transition_causality__contingent_choice_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement_basis(tran_tr_t2, observed).
narrative_ontology:measurement(tran_tr_t4, transition_causality__contingent_choice_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement_basis(tran_tr_t4, observed).
narrative_ontology:measurement(tran_tr_t8, transition_causality__contingent_choice_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(tran_tr_t8, observed).
narrative_ontology:measurement(tran_tr_t14, transition_causality__contingent_choice_reading, theater_ratio, 14, 0.27).
narrative_ontology:measurement_basis(tran_tr_t14, observed).
narrative_ontology:measurement(tran_tr_t20, transition_causality__contingent_choice_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(tran_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__contingent_choice_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(tran_be_t0, observed).
narrative_ontology:measurement(tran_be_t2, transition_causality__contingent_choice_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement_basis(tran_be_t2, observed).
narrative_ontology:measurement(tran_be_t4, transition_causality__contingent_choice_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement_basis(tran_be_t4, observed).
narrative_ontology:measurement(tran_be_t8, transition_causality__contingent_choice_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement_basis(tran_be_t8, observed).
narrative_ontology:measurement(tran_be_t14, transition_causality__contingent_choice_reading, base_extractiveness, 14, 0.61).
narrative_ontology:measurement_basis(tran_be_t14, observed).
narrative_ontology:measurement(tran_be_t20, transition_causality__contingent_choice_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(tran_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__contingent_choice_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(tran_su_t0, observed).
narrative_ontology:measurement(tran_su_t2, transition_causality__contingent_choice_reading, suppression_requirement, 2, 0.38).
narrative_ontology:measurement_basis(tran_su_t2, observed).
narrative_ontology:measurement(tran_su_t4, transition_causality__contingent_choice_reading, suppression_requirement, 4, 0.39).
narrative_ontology:measurement_basis(tran_su_t4, observed).
narrative_ontology:measurement(tran_su_t8, transition_causality__contingent_choice_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement_basis(tran_su_t8, observed).
narrative_ontology:measurement(tran_su_t14, transition_causality__contingent_choice_reading, suppression_requirement, 14, 0.41).
narrative_ontology:measurement_basis(tran_su_t14, observed).
narrative_ontology:measurement(tran_su_t20, transition_causality__contingent_choice_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(tran_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(transition_causality__contingent_choice_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, petrodollar_system_enforcement).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, international_monetary_fund_governance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the transition_causality kernel. The sibling readings (overdetermined_collapse_reading and hybrid_trigger_reading) instantiate the same historical event under different causal framings. Each reading has its own ε, its own beneficiary structure, and its own classification. The three constraints form a family linked by network.affects_constraints; they are NOT the same constraint viewed from different observables. The ε-invariance principle (DP-001) requires separate constraint stories because the readings employ different mechanistic causal evaluations: contingent_choice prioritizes the Nixon decision as the primary node; hybrid_trigger prioritizes the interplay of structural contradictions and contingent trigger events; overdetermined_collapse prioritizes structural inevitability. These are not measurement-basis differences — they are different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
