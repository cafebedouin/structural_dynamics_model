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
 *   human_readable: Contingent Choice Reading of Bretton Woods Transition
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story represents the 'contingent choice' reading of the
 *   1971 Nixon Shock and the end of the Bretton Woods system. It argues that
 *   the transition from fixed to floating exchange rates was a policy
 *   decision, not an inevitable structural collapse, and that alternative
 *   policy choices could have preserved a modified fixed-rate regime. The
 *   decision primarily benefited the US by granting monetary policy autonomy,
 *   while imposing costs on other nations, particularly those holding dollar
 *   reserves. The constraint is classified as a Tangled Rope because it
 *   involved a genuine coordination function (international monetary
 *   stability) but also asymmetric extraction (US policy autonomy at others'
 *   expense) maintained by active enforcement (US diplomatic and economic
 *   power).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.65).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.7).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Contingent Choice Reading of Bretton Woods Transition").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '388c183a-d6eb-405f-bb86-1bbad2b0be76').
narrative_ontology:cs_kernel_codification('388c183a-d6eb-405f-bb86-1bbad2b0be76', formalized).
narrative_ontology:cs_authority_grounding('388c183a-d6eb-405f-bb86-1bbad2b0be76', extraction).
narrative_ontology:cs_interpretation_layer_present('388c183a-d6eb-405f-bb86-1bbad2b0be76').
narrative_ontology:cs_reading_relation('388c183a-d6eb-405f-bb86-1bbad2b0be76', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('388c183a-d6eb-405f-bb86-1bbad2b0be76', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('388c183a-d6eb-405f-bb86-1bbad2b0be76', foundational, policy_autonomy_is_primary_driver).
narrative_ontology:cs_axiom_status(policy_autonomy_is_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('388c183a-d6eb-405f-bb86-1bbad2b0be76', policy_autonomy_is_primary_driver, instrumental).
narrative_ontology:cs_axiom('388c183a-d6eb-405f-bb86-1bbad2b0be76', foundational, counterfactual_alternatives_were_viable).
narrative_ontology:cs_axiom_status(counterfactual_alternatives_were_viable, holdable).
narrative_ontology:cs_axiom_grounding('388c183a-d6eb-405f-bb86-1bbad2b0be76', counterfactual_alternatives_were_viable, empirically_contingent).
narrative_ontology:cs_reference_frame('388c183a-d6eb-405f-bb86-1bbad2b0be76', nixon_shock_as_policy_choice).
narrative_ontology:cs_drift_state('388c183a-d6eb-405f-bb86-1bbad2b0be76', contemporary_economic_history, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('388c183a-d6eb-405f-bb86-1bbad2b0be76', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, united_states_government).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_multinational_corporations).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, european_central_banks).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, developing_nations_with_dollar_reserves).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the flexibility gained by ending dollar convertibility, allowing greater domestic policy autonomy without external constraint. Actively enforced the new floating exchange rate regime through diplomatic and economic pressure.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, united_states_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited from increased flexibility in international trade and investment, as floating exchange rates reduced the risk of currency overvaluation for the dollar, facilitating global expansion and profit repatriation.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_multinational_corporations, beneficiary,
    powerful, biographical, mobile, global).

% Held large dollar reserves that depreciated in value post-convertibility, losing a stable anchor for their monetary policy. Forced to manage floating exchange rates, introducing new volatility and policy challenges.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, european_central_banks, payer,
    institutional, biographical, constrained, regional).

% Suffered significant losses on their dollar reserves and faced increased instability in international trade and debt servicing due to floating exchange rates, with limited capacity to influence the new regime.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, developing_nations_with_dollar_reserves, payer,
    powerless, generational, trapped, global).

% The institution designed to manage the Bretton Woods system, it adapted to the new floating regime by developing new surveillance and lending tools, but its original mandate was fundamentally altered by the US decision.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system coordinated international monetary policy around fixed exchange rates tied to gold, providing stability for trade and investment. The transition ended this specific coordination, replacing it with a more flexible, but less stable, system.
% TRANSFER_FUNCTION: The transition transferred the burden of adjustment from the US (which could print dollars without gold constraint) to other nations, particularly those holding dollar reserves, by allowing the dollar to float freely.
% ABSENT_VOICES: Developing nations, whose economic stability was significantly impacted by the shift to floating exchange rates and dollar depreciation, had minimal voice in the decision-making process. Their concerns were largely unaddressed.
% DISAPPEARANCE_RATIONALE: If the contingent choice to end dollar convertibility had not occurred, the international monetary system would have continued under a different, likely more constrained, fixed-exchange-rate regime, forcing the US to make different domestic policy choices or negotiate a different global monetary order. The current global financial architecture would not exist.
% FOUNDING_PROBLEM: The Bretton Woods system was designed to prevent the competitive devaluations and financial instability that characterized the interwar period, establishing a stable international monetary order.
% FOUNDING_PROBLEM_CORROBORATION: While the original problem of competitive devaluations was largely solved, the specific mechanism (dollar-gold convertibility) became unsustainable for the US by the late 1960s. Economists and historians outside the US government widely corroborate that the system's internal contradictions were manageable through policy adjustments, making the 'collapse' a choice rather than an inevitability.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) reflects the significant transfer of adjustment costs from the US to other nations. Suppression (0.70) indicates the active diplomatic and economic pressure exerted by the US to ensure the adoption of the new floating regime, limiting alternatives for other countries. Theater ratio (0.20) is low, as the US decision was a direct policy action with clear, intended consequences, not a performative maintenance of an atrophied function. The rising extractiveness and suppression over the interval reflect the increasing pressure on the Bretton Woods system and the eventual unilateral US decision.
 *
 * PERSPECTIVAL GAP:
 *   From the US perspective, the decision was a necessary act of self-preservation and a move towards a more 'natural' market-driven system. From the perspective of other nations, particularly those with large dollar holdings, it was a unilateral act of extraction that destabilized their economies. This reading emphasizes the agency and choice of the US, contrasting with views that stress structural inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   The US government and its multinational corporations are beneficiaries, gaining policy flexibility and reduced currency risk. European central banks and developing nations holding dollar reserves are victims, bearing the costs of dollar depreciation and increased monetary instability. The IMF, while adapting, saw its foundational mandate altered.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (Bretton Woods stability) was effectively superseded by the US's pursuit of domestic policy autonomy. This reading argues that the 'mandate' of fixed exchange rates was not truly dead due to structural forces, but rather actively killed by a policy choice, preventing mislabeling as a Piton and instead highlighting the active extraction of a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_alternatives,
    'To what extent were viable policy alternatives to ending dollar convertibility truly available to the US in 1971?',
    'Historical counterfactual analysis by economic historians, evaluating the feasibility and political costs of alternative policies (e.g., devaluation, capital controls, fiscal austerity).',
    'If alternatives were highly viable, it strengthens the ''contingent choice'' argument and the classification as a Tangled Rope. If alternatives were extremely costly or politically impossible, it would lend more weight to the ''overdetermined collapse'' reading, potentially shifting the classification towards a Mountain (of political economy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_alternatives, empirical, 'Assesses the genuine availability of alternative policy paths for the US.').

omega_variable(
    distribution_of_adjustment_costs,
    'How were the costs of adjusting to the new floating exchange rate regime distributed among different nations and economic blocs?',
    'Detailed econometric studies analyzing trade balances, capital flows, and currency valuations across different countries post-1971.',
    'If costs were highly concentrated on vulnerable nations, it reinforces the high extractiveness and Snare-like qualities of the transition. If costs were more evenly distributed, it might suggest a more Rope-like, albeit still imperfect, coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distribution_of_adjustment_costs, empirical, 'Measures the actual burden of adjustment on different international actors.').

omega_variable(
    framing_of_inevitability_vs_choice,
    'Is the framing of the Bretton Woods collapse as ''inevitable'' a genuine analytical conclusion or a rhetorical strategy to legitimize the US''s unilateral policy choice?',
    'Discourse analysis of official statements and academic literature from the period, comparing arguments for inevitability with evidence of policy agency and available alternatives.',
    'If primarily a rhetorical strategy, it would increase the ''theater_ratio'' and highlight the ''suppression'' of alternative narratives, reinforcing the Tangled Rope or even Snare classification. If a genuine analytical conclusion, it would support the ''overdetermined_collapse_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_inevitability_vs_choice, conceptual, 'Examines the role of narrative in shaping the understanding of the transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1960, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1960, transition_causality__contingent_choice_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__contingent_choice_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(tran_tr_t1970, transition_causality__contingent_choice_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(tran_tr_t1975, transition_causality__contingent_choice_reading, theater_ratio, 1975, 0.2).

% Extraction over time
narrative_ontology:measurement(tran_be_t1960, transition_causality__contingent_choice_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(tran_be_t1965, transition_causality__contingent_choice_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(tran_be_t1970, transition_causality__contingent_choice_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(tran_be_t1975, transition_causality__contingent_choice_reading, base_extractiveness, 1975, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1960, transition_causality__contingent_choice_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(tran_su_t1965, transition_causality__contingent_choice_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(tran_su_t1970, transition_causality__contingent_choice_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(tran_su_t1975, transition_causality__contingent_choice_reading, suppression_requirement, 1975, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
