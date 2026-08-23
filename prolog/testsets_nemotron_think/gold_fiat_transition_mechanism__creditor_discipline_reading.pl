% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Post-1971 Fiat Reserve System — Creditor Veto Elimination Reading
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story models the post-1971 fiat dollar system from the
 *   creditor_discipline_reading of the gold_fiat_transition_mechanism kernel.
 *   The reading identifies the elimination of creditor veto power — the gold
 *   redemption threat that disciplined debtor balance-of-payments — as the
 *   core structural shift. The US, as reserve issuer, gained unlimited fiscal
 *   flexibility; former creditors lost their leverage; non-reserve holders
 *   face tighter external constraints. The claimed_type is tangled_rope: the
 *   system coordinates global trade (genuine function) but extracts
 *   asymmetrically via exorbitant privilege (extraction). The measurement
 *   series shows rising extractiveness and theater from 1971–2024 as the
 *   privilege institutionalized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.65).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Post-1971 Fiat Reserve System — Creditor Veto Elimination Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, 'fb7d2573-b3fd-4cb6-9562-e471b3751f73').
narrative_ontology:cs_kernel_codification('fb7d2573-b3fd-4cb6-9562-e471b3751f73', fixed_text).
narrative_ontology:cs_authority_grounding('fb7d2573-b3fd-4cb6-9562-e471b3751f73', lineage).
narrative_ontology:cs_interpretation_layer_present('fb7d2573-b3fd-4cb6-9562-e471b3751f73').
narrative_ontology:cs_reading_relation('fb7d2573-b3fd-4cb6-9562-e471b3751f73', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb7d2573-b3fd-4cb6-9562-e471b3751f73', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('fb7d2573-b3fd-4cb6-9562-e471b3751f73', foundational, reserve_currency_exorbitant_privilege).
narrative_ontology:cs_axiom_status(reserve_currency_exorbitant_privilege, holdable).
narrative_ontology:cs_axiom_grounding('fb7d2573-b3fd-4cb6-9562-e471b3751f73', reserve_currency_exorbitant_privilege, empirically_contingent).
narrative_ontology:cs_axiom('fb7d2573-b3fd-4cb6-9562-e471b3751f73', foundational, creditor_discipline_elimination_as_power_shift).
narrative_ontology:cs_axiom_status(creditor_discipline_elimination_as_power_shift, holdable).
narrative_ontology:cs_axiom_grounding('fb7d2573-b3fd-4cb6-9562-e471b3751f73', creditor_discipline_elimination_as_power_shift, empirically_contingent).
narrative_ontology:cs_reference_frame('fb7d2573-b3fd-4cb6-9562-e471b3751f73', gold_standard_creditor_veto).
narrative_ontology:cs_drift_state('fb7d2573-b3fd-4cb6-9562-e471b3751f73', post_nixon_shock, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('fb7d2573-b3fd-4cb6-9562-e471b3751f73', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, global_trade_participants).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, triffin_dilemma_resolution_via_reserve_privilege).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, fiscal_flexibility_as_hegemonic_asset).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary reserve currency. Runs persistent current account deficits without gold redemption discipline. Sets global monetary conditions through Fed policy. Collects seigniorage and exerts structural power over swap lines and sanctions regimes. Exit is effectively arbitrary — can restructure the system unilaterally.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_issuer, beneficiary).

% Former gold-standard creditors (Germany, Japan, Switzerland, oil exporters) who held gold-convertible claims. Lost veto power over debtor fiscal policy when Nixon closed the gold window. Now hold dollar reserves that depreciate via US inflation policy. Cannot exit dollar system without triggering self-inflicted capital losses. Coordinate through G7/G20 but lack structural leverage.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, biographical, constrained, global).

% Emerging markets and developing economies that must borrow in dollars, accumulate dollar reserves for crisis insurance, and import US monetary policy. Face tighter external constraints than under Bretton Woods — capital flow volatility, sudden stops, and dollar-denominated debt crises. No voice in Fed decisions that determine their financing conditions.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_holders, payer,
    moderate, biographical, trapped, global).

% Advanced and developing economies that gained fiscal flexibility from the end of gold convertibility. Can run larger deficits and pursue countercyclical policy without gold reserve targets. But remain subject to dollar funding cycles and IMF conditionality — flexibility is asymmetric and conditional on market access.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    moderate, biographical, constrained, regional).

% Firms and households benefiting from stable dollar invoicing, deep dollar funding markets, and reduced transaction costs of a single global vehicle currency. Coordination gains are real but unevenly distributed — the system solves a genuine collective action problem in trade finance while extracting from non-participants in the reserve privilege.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, global_trade_participants, beneficiary,
    organized, biographical, mobile, global).

% Central banks (Fed, ECB, BoJ, PBOC, BIS) that administer the fiat system. The Fed sets the global price of liquidity; others manage dollar exposure and negotiate swap lines. They produce the analytical frameworks that legitimate the arrangement (Triffin dilemma management, global safety net narratives) while negotiating its operational parameters.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, monetary_authorities, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, monetary_authorities, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single global vehicle currency (USD) with elastic supply, deep funding markets, and a lender-of-last-resort backstop (Fed swap lines) that enables cross-border trade and finance without bilateral gold settlement or fixed parities.
% TRANSFER_FUNCTION: Moves seigniorage, fiscal space, and crisis insurance from creditor nations and non-reserve holders to the US reserve issuer. The US runs structural deficits financed by global demand for dollar assets; others accumulate low-yield reserves and absorb US monetary spillovers. In crises, swap lines selectively insure core allies while peripheral holders face sudden stops.
% ABSENT_VOICES: Post-colonial monetary sovereignty movements (e.g., 1970s NIEO proposals, BRICS currency initiatives, Global South central bank governors) who argued for symmetric adjustment rules, SDR-based reserve systems, or capital controls. Their exclusion was structural — the transition locked in dollar centrality before they gained institutional voice. They remain outside G7/G20 decision cores.
% DISAPPEARANCE_RATIONALE: If the dollar reserve system vanished overnight, global trade invoicing would fragment, dollar-denominated debt chains would rupture, central bank reserve portfolios would collapse, and the Fed's lender-of-last-resort function would disappear. A new settlement architecture (multipolar, SDR-based, or regional blocs) would have to be improvised under crisis conditions — the world would rearrange violently.
% FOUNDING_PROBLEM: The Bretton Woods gold-exchange standard collapsed because US gold reserves could not cover outstanding dollar liabilities at the fixed parity — the Triffin dilemma made the system unsustainable. The Nixon Shock (1971) was the emergency suspension that became permanent.
% FOUNDING_PROBLEM_CORROBORATION: The Triffin dilemma's resolution via floating rates and dollar inconvertibility is attested by mainstream monetary historians (Eichengreen, Obstfeld, Bordo) and the IMF's own official history — sources outside the US beneficiary position. The founding problem (fixed parity gold convertibility) is objectively gone; the arrangement that replaced it persists and expanded.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the reserve issuer collects seigniorage and fiscal space from the entire dollar zone without reciprocal constraint. Suppression (0.65) is substantial: non-reserve holders are structurally trapped in dollar funding, creditor nations cannot exit without self-harm, and the system actively suppresses alternatives (capital controls discouraged, SDRs marginalized, swap lines conditional). Theater (0.42) reflects the growing gap between G20/IMF coordination rhetoric and the unilateral reality of Fed policy dominance. Accessibility_collapse (0.58) and resistance (0.45) are moderate — alternatives exist (euro, yuan, crypto, SDRs) but network effects and institutional lock-in prevent adoption.
 *
 * PERSPECTIVAL GAP:
 *   From the us_reserve_issuer seat, the system is a rope — it built and maintains the global monetary infrastructure others use. From creditor_nations and non_reserve_holders seats, it is a snare — they are locked into a depreciating asset class with no voice in its management. The engine computes this divergence from the structural data; the authored claim (tangled_rope) acknowledges both the coordination and extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   The us_reserve_issuer is the structural beneficiary (d near 0) — the constraint subsidizes its fiscal position. creditor_nations and non_reserve_holders are targets (d near 1) — they bear the extraction via reserve depreciation and dollar dependency. debtor_nations and global_trade_participants sit near symmetric (d ~0.5) — they gain coordination benefits but pay indirect costs. The engine derives these from the beneficiary/victim declarations and exit options: arbitrage exit for the issuer, trapped/constrained for the payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Triffin dilemma under fixed gold parity) is dead — confirmed by independent monetary historians. Yet the arrangement persists and has expanded its extraction (rising ε). This is classic mandatrophy: the emergency suspension became a permanent privilege. The constraint now serves the reserve issuer's fiscal flexibility, not the original coordination problem. The theater_ratio rise tracks the performative maintenance (G20 communiqués, IMF surveillance) that masks the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creditor_veto_elimination_vs_material_constraint_removal,
    'Does the creditor_discipline_reading''s core claim (elimination of creditor veto) structurally contradict the automatic_constraint_reading''s claim (removal of physical gold constraint on money creation), or do they describe different layers of the same transition?',
    'Formal analysis of whether gold redemption operated as a creditor veto (political economy) versus a physical reserve constraint (monetary mechanics) — if the former, the readings occupy different causal levels and coexist; if the latter, they compete for the same explanatory slot.',
    'If they contradict, this reading forecloses the automatic reading''s causal primacy; if they coexist, the kernel''s causal structure is multi-layered and both readings are partial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_veto_elimination_vs_material_constraint_removal, conceptual, 'Structural relationship between creditor_discipline_reading and automatic_constraint_reading — forecloses vs. coexists_with.').

omega_variable(
    composite_overdetermination_vs_single_mechanism,
    'Is the creditor veto elimination a sufficient causal node for the transition, or is the composite_overdetermination_reading correct that multiple independent shifts (telecom, labor, legal tender, Bretton Woods collapse) overdetermine the outcome?',
    'Counterfactual historical analysis: if Nixon had not closed the gold window but other shifts (Eurodollar growth, labor power decline, telecom) had occurred, would a fiat-like system have emerged anyway?',
    'If overdetermined, this reading''s causal claim is inflated — the creditor veto elimination was one thread in a convergent weave, not the causal node. The reading''s ε would need re-estimation against a multi-causal baseline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_overdetermination_vs_single_mechanism, empirical, 'Whether the creditor_discipline_reading identifies a necessary causal mechanism or one thread in an overdetermined transition.').

omega_variable(
    exorbitant_privilege_measurement,
    'Can the extraction from non-reserve holders (seigniorage, crisis insurance cost, monetary spillover absorption) be quantified separately from the coordination gains (trade invoicing efficiency, funding market depth)?',
    'Empirical decomposition of dollar zone welfare effects: compare crisis outcomes for dollarized vs. non-dollarized economies controlling for fundamentals; estimate seigniorage flows from foreign reserve accumulation; model counterfactual multipolar reserve system.',
    'If extraction and coordination are inseparable, the tangled_rope classification holds; if extraction dominates coordination for non-reserve holders, the reading may understate snare characteristics for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_measurement, empirical, 'Quantifiability of asymmetric extraction vs. coordination gains in the dollar reserve system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftm_cdr_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.12).
narrative_ontology:measurement(gftm_cdr_tr_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(gftm_cdr_tr_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement(gftm_cdr_tr_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(gftm_cdr_tr_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(gftm_cdr_tr_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(gftm_cdr_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.35).
narrative_ontology:measurement(gftm_cdr_be_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(gftm_cdr_be_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(gftm_cdr_be_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2008, 0.72).
narrative_ontology:measurement(gftm_cdr_be_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(gftm_cdr_be_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gftm_cdr_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.45).
narrative_ontology:measurement(gftm_cdr_su_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(gftm_cdr_su_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(gftm_cdr_su_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2008, 0.63).
narrative_ontology:measurement(gftm_cdr_su_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(gftm_cdr_su_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.18).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, dollar_hegemony_swap_line_architecture).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_conditionality_as_dollar_enforcement).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, eurodollar_market_offshore_dollar_creation).

% DUAL FORMULATION NOTE:
% This story is one of three readings in the gold_fiat_transition_mechanism constraint family. The automatic_constraint_reading models the transition as removal of a physical constraint on money creation; the composite_overdetermination_reading models it as convergent structural shifts. This reading identifies creditor veto elimination as the key power-shift mechanism. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, institutional, 0.05).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, powerful, 0.85).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
