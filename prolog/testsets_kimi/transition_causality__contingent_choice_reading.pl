% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Post-Bretton Woods Dollar Standard (Contingent Choice Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint instantiates the contingent-choice reading of the
 *   post-Bretton Woods monetary transition. It treats the Nixon
 *   administration's 1971 suspension of gold convertibility not as a
 *   structurally inevitable collapse, but as a discretionary policy decision
 *   that could have been avoided through alternative cooperative or
 *   unilateral measures. Under this reading, the resulting dollar standard is
 *   a constructed arrangement that coordinates global liquidity while
 *   asymmetrically extracting policy autonomy and seigniorage for the United
 *   States. The arrangement is actively enforced through IMF conditionality,
 *   swap-line politics, sanctions infrastructure, and the network
 *   externalities of dollar invoicing.
 *
 * KEY AGENTS:
 *   - us_policy_apparatus: Primary beneficiary/agenda-setter (institutional/global scope) â captures policy autonomy and sets the monetary-sanctions architecture.
 *   - us_financial_sector: Secondary beneficiary (powerful/arbitrage) â intermediates dollar flows and profits from reserve-currency centrality.
 *   - foreign_central_banks: Primary payer (institutional/constrained) â hold reserves, import Fed shocks, manage subordinated monetary policy.
 *   - non_us_consumers_and_taxpayers: Diffuse payer (powerless/trapped) â bear imported inflation and austerity with no exit.
 *   - global_trade_sector: Constrained payer (organized/constrained) â depends on dollar rails, absorbs volatility.
 *   - alternative_reserve_advocates: Excluded voice (moderate/constrained) â propose alternatives but lack institutional access.
 *   - monetary_policy_scholars: Analytical observer (analytical) â evaluates causal claims and distributional effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.65).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.63).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Post-Bretton Woods Dollar Standard (Contingent Choice Reading)").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '617c4954-5d98-4b03-9c29-0cf193342fae').
narrative_ontology:cs_kernel_codification('617c4954-5d98-4b03-9c29-0cf193342fae', formalized).
narrative_ontology:cs_authority_grounding('617c4954-5d98-4b03-9c29-0cf193342fae', extraction).
narrative_ontology:cs_interpretation_layer_present('617c4954-5d98-4b03-9c29-0cf193342fae').
narrative_ontology:cs_reading_relation('617c4954-5d98-4b03-9c29-0cf193342fae', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('617c4954-5d98-4b03-9c29-0cf193342fae', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('617c4954-5d98-4b03-9c29-0cf193342fae', foundational, state_contingency_over_structural_necessity).
narrative_ontology:cs_axiom_status(state_contingency_over_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('617c4954-5d98-4b03-9c29-0cf193342fae', state_contingency_over_structural_necessity, empirically_contingent).
narrative_ontology:cs_axiom('617c4954-5d98-4b03-9c29-0cf193342fae', foundational, policy_autonomy_as_legitimate_state_interest).
narrative_ontology:cs_axiom_status(policy_autonomy_as_legitimate_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('617c4954-5d98-4b03-9c29-0cf193342fae', policy_autonomy_as_legitimate_state_interest, conventional).
narrative_ontology:cs_reference_frame('617c4954-5d98-4b03-9c29-0cf193342fae', sovereign_policy_discretion).
narrative_ontology:cs_drift_state('617c4954-5d98-4b03-9c29-0cf193342fae', contemporary_multipolarity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('617c4954-5d98-4b03-9c29-0cf193342fae', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_policy_apparatus).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_financial_sector).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, foreign_central_banks).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, non_us_consumers_and_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, global_trade_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dollar standard through Federal Reserve monetary policy, Treasury debt management, and sanctions architecture. Captures policy autonomyâthe ability to run persistent deficits, monetize debt, and set domestic interest rates without external convertibility constraints.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_policy_apparatus, agenda_setter,
    institutional, generational, mobile, global).

% Profits from dollar-denominated intermediation, deep Treasury markets, and payment-system centrality. Benefits from the exorbitant privilege embedded in reserve-currency status and from liquidity backstops that accrue to the core.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_financial_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Hold dollar reserves to manage exchange rates and insure against capital flight. Bear imported U.S. inflation and interest-rate cycles; their monetary policy is subordinated to Federal Reserve decisions. Diversification is possible only slowly and at systemic risk.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, foreign_central_banks, payer,
    institutional, generational, constrained, global).

% Use local currencies whose purchasing power is eroded when the Fed eases; pay higher costs for dollar-denominated energy and imports; finance fiscal austerity when U.S. rate hikes trigger capital outflows. No individual exit from the system.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, non_us_consumers_and_taxpayers, payer,
    powerless, immediate, trapped, national).

% Depends on dollar payment rails for cross-border settlement. Absorbs exchange-rate volatility and transaction costs from the dollar's intermediary role. Lacks a scalable alternative invoicing currency and bears the costs of the system's asymmetries.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, global_trade_sector, payer,
    organized, biographical, constrained, global).

% Propose multi-polar reserve systems or commodity-backed settlement frameworks. Are structurally marginalized from IMF governance and standard-setting committees dominated by the G7 and dollar bloc.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, alternative_reserve_advocates, excluded,
    moderate, generational, constrained, global).

% Debate the necessity and distributional consequences of the Nixon transition. Produce counterfactual analysis and historical accounts that either vindicate or challenge the contingent-choice reading.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, monetary_policy_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, us_policy_apparatus).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a global monetary anchor and liquidity mechanism after the collapse of the Bretton Woods fixed-exchange system, allowing trade and capital flows to continue without a gold-backed settlement mechanism.
% TRANSFER_FUNCTION: Moves seigniorage, macroeconomic adjustment costs, and inflation-export burdens from the United States to foreign central banks and non-U.S. consumers, while transferring policy autonomy to the U.S. Treasury and Federal Reserve.
% ABSENT_VOICES: Alternative reserve-currency blocs and gold-standard advocates are structurally marginalized in IMF and BIS governance; foreign publics who bear imported inflation lack voice in Federal Reserve Open Market Committee decisions.
% DISAPPEARANCE_RATIONALE: If the contingent-choice constraint disappearedâmeaning the U.S. suddenly abandoned the policy autonomy framework and the dollar lost its privileged reserve roleâglobal reserve portfolios would reprice, trade invoicing would fragment into currency blocs, and U.S. fiscal and monetary space would contract dramatically. The world would rearrange around multiple monetary anchors.
% FOUNDING_PROBLEM: The Triffin dilemma and gold overhang threatened to force a deflationary adjustment on the U.S. or a run on U.S. gold reserves under fixed convertibility.
% FOUNDING_PROBLEM_CORROBORATION: International monetary historians outside the U.S. policy apparatus (e.g., Eichengreen, Bordo) attest that the Triffin dilemma was a real structural pressure in the 1960s; they contest whether it required the specific suspension chosen or whether alternative cooperative reforms could have solved it without the current extraction structure.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.65) because the U.S. captures substantial policy autonomy and seigniorage while externalizing adjustment costs. Suppression is similarly high (0.63) because the constraint's persistence depends on an active architecture of sanctions, IMF alignment, and petrodollar recycling, not merely participant preference. Theater ratio is moderate (0.42): rhetoric of market-determined floating rates and central-bank independence obscures the managed nature of dollar hegemony. Accessibility collapse is high (0.75) because network effects in invoicing, clearing, and reserves make alternatives nearly unreachable once the system is understood. Resistance is moderate (0.45): de-dollarization efforts are real but fragmented. Temporal measurements trace the ratchet from the 1971 shock through the Volcker era, the Washington Consensus, the GFC, and the recent sanctions-weaponization phase, showing steadily accumulating extraction and enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. policy apparatus experiences the constraint as a necessary and legitimate framework that solves global liquidity problems while preserving sovereign discretion. Foreign central banks experience the same structure as a subordination mechanism that imports U.S. macroeconomic volatility. Non-U.S. consumers experience it as imported inflation and austerity with no voice. The engine computes this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   The us_policy_apparatus is the structural beneficiary (low d, near full subsidy) because the constraint subsidizes its fiscal and monetary freedom. The us_financial_sector also sits near the beneficiary end. Foreign_central_banks and non_us_consumers_and_taxpayers are structural targets (high d) because the constraint extracts from their monetary sovereignty and purchasing power. Global_trade_sector sits near symmetric but tilted toward target because it receives coordination benefit (liquidity) while paying extraction (volatility, dollar costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe Triffin dilemma and gold overhangâis dead. The arrangement persists not because fixed convertibility is still a threat, but because it has been repurposed to sustain U.S. policy autonomy and financial-sector rents. This is not a piton: extraction is concentrated, beneficiaries are identifiable, and enforcement is active and intensifying. It is a tangled rope with zombie coordination: the coordination function (global liquidity) is real but increasingly overlaid with extraction that exceeds the coordination cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_1971,
    'Could the U.S. have maintained Bretton Woods through alternative policy choices (revaluation, SDR expansion, capital controls) without triggering a worse collapse?',
    'Historical counterfactual modeling and archival evidence on options considered by the Nixon administration and the Ford/Federal Reserve Board in 1971.',
    'High viability supports the contingent-choice reading and raises extractiveness (the choice was avoidable and thus the current system is constructed extraction); low viability pushes classification toward overdetermined or hybrid readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_1971, empirical, 'Whether the Nixon transition was genuinely avoidable via alternative policy').

omega_variable(
    suppression_structural_vs_active,
    'Is the persistence of the dollar standard maintained primarily by network-effect structural lock-in, or by active enforcement (sanctions, IMF conditionality, swap-line politics)?',
    'Comparative analysis of de-dollarization episodes and U.S. policy responses to assess the proportion of structural inertia versus active suppression.',
    'If active enforcement dominates, the constraint is more extractive and leans toward snare; if structural lock-in dominates, it leans toward rope or inertial mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_active, conceptual, 'Structural versus active suppression mechanism in dollar hegemony').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__contingent_choice_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tran_tr_t10, transition_causality__contingent_choice_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(tran_tr_t20, transition_causality__contingent_choice_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(tran_tr_t30, transition_causality__contingent_choice_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(tran_tr_t40, transition_causality__contingent_choice_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(tran_tr_t50, transition_causality__contingent_choice_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(tran_tr_t55, transition_causality__contingent_choice_reading, theater_ratio, 55, 0.42).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__contingent_choice_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tran_be_t10, transition_causality__contingent_choice_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(tran_be_t20, transition_causality__contingent_choice_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(tran_be_t30, transition_causality__contingent_choice_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(tran_be_t40, transition_causality__contingent_choice_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(tran_be_t50, transition_causality__contingent_choice_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(tran_be_t55, transition_causality__contingent_choice_reading, base_extractiveness, 55, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__contingent_choice_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tran_su_t10, transition_causality__contingent_choice_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(tran_su_t20, transition_causality__contingent_choice_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(tran_su_t30, transition_causality__contingent_choice_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(tran_su_t40, transition_causality__contingent_choice_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(tran_su_t50, transition_causality__contingent_choice_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(tran_su_t55, transition_causality__contingent_choice_reading, suppression_requirement, 55, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, global_infrastructure).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the transition_causality kernel, decomposed per the Îµ-invariance principle. The contingent-choice reading isolates the Nixon decision as the primary causal node with high counterfactual viability, distinct from the structural-inevitability reading (overdetermined_collapse_reading) and the primed-trigger reading (hybrid_trigger_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
