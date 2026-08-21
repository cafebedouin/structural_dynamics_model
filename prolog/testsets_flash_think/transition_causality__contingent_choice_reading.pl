% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Contingent Choice Reading of the Bretton Woods Transition
 *   domain: Monetary Economics / Political Economy / International Finance
 *
 * SUMMARY:
 *   This constraint story analyzes the 'contingent choice' reading of the
 *   Bretton Woods transition, specifically the U.S. decision in 1971 to
 *   unilaterally suspend the dollar's convertibility to gold (the 'Nixon
 *   Shock'). This reading posits that the transition was a policy choice that
 *   could have been avoided with different decisions, rather than an
 *   inevitable structural collapse. The constraint's operation is seen as a
 *   deliberate act by the U.S. to gain policy autonomy, with significant
 *   extractive consequences for other nations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.75).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.65).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Contingent Choice Reading of the Bretton Woods Transition").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "Monetary Economics / Political Economy / International Finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, 'cf3d611f-b8ce-4b95-9f38-02acd0c9d61d').
narrative_ontology:cs_kernel_codification('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', formalized).
narrative_ontology:cs_authority_grounding('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', extraction).
narrative_ontology:cs_interpretation_layer_present('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d').
narrative_ontology:cs_reading_relation('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', foundational, national_monetary_autonomy_supremacy).
narrative_ontology:cs_axiom_status(national_monetary_autonomy_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', national_monetary_autonomy_supremacy, conventional).
narrative_ontology:cs_axiom('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', foundational, international_agreements_are_subordinate_to_national_interest).
narrative_ontology:cs_axiom_status(international_agreements_are_subordinate_to_national_interest, holdable).
narrative_ontology:cs_axiom_grounding('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', international_agreements_are_subordinate_to_national_interest, conventional).
narrative_ontology:cs_reference_frame('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', bretton_woods_fixed_parity_regime).
narrative_ontology:cs_drift_state('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', nixon_shock_1971, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cf3d611f-b8ce-4b95-9f38-02acd0c9d61d', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, federal_reserve).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, gold_speculators).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, countries_with_dollar_reserves).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, export_dependent_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary decision-maker, the U.S. Treasury initiated the suspension of dollar-gold convertibility, gaining significant policy autonomy and flexibility in managing domestic economic challenges without the constraints of the fixed exchange rate system.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited from the increased independence in monetary policy, no longer needing to defend the dollar's gold parity. This allowed for greater focus on domestic objectives like employment and inflation.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, federal_reserve, beneficiary,
    institutional, generational, arbitrage, global).

% Held substantial dollar reserves, which lost their fixed convertibility to gold, leading to uncertainty and potential devaluation. They bore the costs of increased currency volatility and the loss of a predictable international monetary anchor.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, countries_with_dollar_reserves, payer,
    organized, biographical, constrained, global).

% Suffered from increased exchange rate volatility, making international trade and investment planning more difficult and risky. Their economic stability was directly impacted by the shift to floating rates.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, export_dependent_economies, payer,
    moderate, biographical, constrained, global).

% The institution designed to administer the Bretton Woods system, it observed the unilateral U.S. decision and subsequently adapted its role to a floating exchange rate regime, losing some of its original mandate.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Profited significantly from the uncertainty and subsequent rise in gold prices following the suspension of convertibility, leveraging market dislocations for financial gain.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_speculators, beneficiary,
    powerful, immediate, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The decision asserted national policy autonomy, coordinating domestic economic priorities over international monetary stability. It effectively coordinated the U.S. government's response to the Triffin dilemma.
% TRANSFER_FUNCTION: Transferred the burden of maintaining dollar-gold convertibility from the U.S. to other nations, who then faced currency instability and the need to manage floating exchange rates. It also transferred policy flexibility to the U.S. monetary authorities.
% ABSENT_VOICES: Developing nations and smaller economies, whose voices held less sway in international financial forums, would have strongly objected to the unilateral nature of the decision and the instability it introduced, but lacked the power to prevent it.
% DISAPPEARANCE_RATIONALE: If the policy decision to suspend dollar-gold convertibility had not occurred, the global financial system would have continued under some form of fixed exchange rate regime, likely leading to different patterns of trade, capital flows, and national economic development. The entire post-Bretton Woods era would be absent.
% FOUNDING_PROBLEM: The U.S. faced the 'Triffin dilemma': maintaining dollar convertibility to gold while simultaneously funding global trade and military commitments led to a persistent balance of payments deficit and a drain on U.S. gold reserves, threatening the stability of the entire Bretton Woods system.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and international relations scholars, independent of U.S. policy-making bodies, widely corroborate the Triffin dilemma as a significant pressure point on the Bretton Woods system. They also attest that the specific problem of dollar-gold convertibility is now resolved due to the system's collapse.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` is high (0.75) because the U.S. gained substantial policy flexibility and offloaded the costs of maintaining the gold standard onto other nations. `Suppression` is moderate-high (0.65) as the U.S. had the power to unilaterally break the system, and other nations had limited effective recourse. `Theater_ratio` is low (0.15) because the U.S. explicitly abandoned the gold standard; there was little pretense of maintaining the old system after the decision. `Accessibility_collapse` is high (0.80) for other nations, as the fixed exchange rate system they relied upon collapsed. `Resistance` is moderate (0.50) as other nations protested but could not prevent the shift. The measurements show a sharp increase in extractiveness and suppression around 1971, reflecting the immediate impact of the policy decision.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. perspective, the decision was a necessary act of national sovereignty to address domestic economic pressures. From the perspective of other nations, particularly those with large dollar reserves, it was a unilateral and extractive act that destabilized the global financial order. This reading emphasizes the agency of the U.S. in making a choice that had profound, asymmetric impacts.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury and Federal Reserve are clear beneficiaries, gaining policy autonomy. Gold speculators also benefited from the resulting market volatility. Countries holding dollar reserves and export-dependent economies are victims, bearing the costs of currency instability and the loss of a predictable monetary system. The IMF, while an observer, also experienced a shift in its mandate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_alternatives,
    'Could the U.S. have genuinely pursued alternative policies (e.g., devaluation, stricter fiscal discipline, international cooperation) to avoid suspending gold convertibility without severe domestic economic consequences?',
    'Detailed counterfactual historical analysis by economic historians, modeling the likely outcomes of alternative policy paths given the political and economic constraints of the era.',
    'If high counterfactual viability is established, it strengthens the ''contingent choice'' framing and the extractive nature of the unilateral decision. If low, it lends credence to the ''overdetermined collapse'' or ''hybrid trigger'' readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_alternatives, empirical, 'The degree to which alternative policy choices were genuinely available to the U.S. prior to 1971.').

omega_variable(
    causal_primacy_of_policy_vs_structure,
    'Was the Nixon Shock primarily a policy decision, or was it merely the final, contingent trigger of deeper, overdetermined structural contradictions within the Bretton Woods system?',
    'Comparative historical analysis of the relative weight of agency (U.S. policy choices) versus structural forces (Triffin dilemma, balance of payments deficits) in the system''s collapse, particularly contrasting with the ''overdetermined_collapse_reading'' and ''hybrid_trigger_reading''.',
    'If policy choice is primary, this reading''s classification as a Tangled Rope (deliberate, extractive coordination) is reinforced. If structural forces were dominant, it would shift towards a Mountain (inevitable) or Piton (atrophied system) from the perspective of the U.S. as well.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_primacy_of_policy_vs_structure, conceptual, 'The fundamental causal attribution for the Bretton Woods transition: policy choice vs. structural inevitability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1965, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1965, transition_causality__contingent_choice_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__contingent_choice_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.15).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__contingent_choice_reading, theater_ratio, 1973, 0.1).
narrative_ontology:measurement(tran_tr_t1975, transition_causality__contingent_choice_reading, theater_ratio, 1975, 0.15).

% Extraction over time
narrative_ontology:measurement(tran_be_t1965, transition_causality__contingent_choice_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(tran_be_t1968, transition_causality__contingent_choice_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(tran_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.75).
narrative_ontology:measurement(tran_be_t1973, transition_causality__contingent_choice_reading, base_extractiveness, 1973, 0.78).
narrative_ontology:measurement(tran_be_t1975, transition_causality__contingent_choice_reading, base_extractiveness, 1975, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1965, transition_causality__contingent_choice_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(tran_su_t1968, transition_causality__contingent_choice_reading, suppression_requirement, 1968, 0.55).
narrative_ontology:measurement(tran_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.65).
narrative_ontology:measurement(tran_su_t1973, transition_causality__contingent_choice_reading, suppression_requirement, 1973, 0.68).
narrative_ontology:measurement(tran_su_t1975, transition_causality__contingent_choice_reading, suppression_requirement, 1975, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, petrodollar_system).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, global_financial_deregulation).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'transition_causality' kernel, focusing on the Bretton Woods collapse. This 'contingent_choice_reading' emphasizes the U.S. policy decision as the primary cause, distinct from readings that emphasize structural inevitability or hybrid triggers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
