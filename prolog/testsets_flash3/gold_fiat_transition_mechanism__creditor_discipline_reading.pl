% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Gold-Fiat Transition: Creditor Discipline Reading
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story analyzes the gold-fiat transition from the
 *   perspective of 'creditor discipline,' arguing that the shift eliminated
 *   the power of creditor nations to impose fiscal discipline on debtor
 *   nations, particularly the reserve currency issuer, through the threat of
 *   gold redemption. This reading highlights a geopolitical power shift and
 *   the enabling of greater fiscal flexibility for some at the expense of
 *   others. This is one reading of the 'gold_fiat_transition_mechanism'
 *   kernel, with sibling readings 'automatic_constraint_reading' and
 *   'composite_overdetermination_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.9).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, snare).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Gold-Fiat Transition: Creditor Discipline Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '1258da07-5e4e-4c9a-8ed7-817a4303148e').
narrative_ontology:cs_kernel_codification('1258da07-5e4e-4c9a-8ed7-817a4303148e', formalized).
narrative_ontology:cs_authority_grounding('1258da07-5e4e-4c9a-8ed7-817a4303148e', extraction).
narrative_ontology:cs_interpretation_layer_present('1258da07-5e4e-4c9a-8ed7-817a4303148e').
narrative_ontology:cs_reading_relation('1258da07-5e4e-4c9a-8ed7-817a4303148e', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('1258da07-5e4e-4c9a-8ed7-817a4303148e', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('1258da07-5e4e-4c9a-8ed7-817a4303148e', foundational, creditor_discipline_is_essential).
narrative_ontology:cs_axiom_status(creditor_discipline_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('1258da07-5e4e-4c9a-8ed7-817a4303148e', creditor_discipline_is_essential, instrumental).
narrative_ontology:cs_axiom('1258da07-5e4e-4c9a-8ed7-817a4303148e', foundational, gold_convertibility_is_a_veto_power).
narrative_ontology:cs_axiom_status(gold_convertibility_is_a_veto_power, holdable).
narrative_ontology:cs_axiom_grounding('1258da07-5e4e-4c9a-8ed7-817a4303148e', gold_convertibility_is_a_veto_power, conventional).
narrative_ontology:cs_reference_frame('1258da07-5e4e-4c9a-8ed7-817a4303148e', gold_standard_creditor_hegemony).
narrative_ontology:cs_drift_state('1258da07-5e4e-4c9a-8ed7-817a4303148e', contemporary_fiat_system, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('1258da07-5e4e-4c9a-8ed7-817a4303148e', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the global reserve currency, this nation gained immense fiscal flexibility and seigniorage benefits by no longer being constrained by gold convertibility. It can now run larger deficits without fear of gold outflows, effectively shifting the burden of adjustment to other nations.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).

% These nations, particularly the reserve currency issuer, benefited from the removal of the external constraint on their fiscal policy. They gained the ability to pursue domestic policy goals without immediate balance-of-payments crises triggered by gold redemption threats.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    powerful, biographical, mobile, global).

% These nations lost their primary leverage over debtor nations: the threat of demanding gold redemption for accumulated dollar balances. Their ability to impose fiscal discipline on others was significantly curtailed, leading to a loss of geopolitical influence and a shift in the global financial architecture.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, biographical, constrained, global).

% These institutions (e.g., IMF) saw their role shift from managing a fixed exchange rate system to providing conditional lending and surveillance in a floating rate world. They analyze the implications of this power shift but do not directly control the underlying mechanism.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_financial_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard provided a mechanism for international balance-of-payments adjustment, coordinating national fiscal policies through the discipline of gold convertibility. Its removal necessitated new forms of coordination or allowed for greater national discretion.
% TRANSFER_FUNCTION: The transition transferred significant fiscal and monetary policy autonomy from creditor nations (who previously held veto power via gold redemption) to debtor nations, particularly the reserve currency issuer, who could now print money without external constraint.
% ABSENT_VOICES: Small, non-reserve currency nations, who now face increased volatility and are subject to the policies of the reserve currency issuer without reciprocal leverage, would argue for a more equitable international monetary system. Their voices were largely absent from the decision-making process.
% DISAPPEARANCE_RATIONALE: If the gold-fiat transition were reversed overnight, the global financial system would undergo a massive reorganization. Debtor nations would face immediate and severe fiscal constraints, creditor nations would regain significant leverage, and the international monetary order would revert to a more rigid, commodity-backed system.
% FOUNDING_PROBLEM: The gold standard imposed a rigid external constraint on national economic policy, leading to deflationary pressures and limiting the ability of governments to respond to economic crises or pursue full employment policies.
% FOUNDING_PROBLEM_CORROBORATION: Economists and policymakers in debtor nations, particularly the reserve currency issuer, attest that the problem of external constraints on fiscal policy remains live, justifying the current fiat system. Creditor nations and some international economists argue that the problem has shifted, with the lack of discipline now creating new instabilities; their corroboration comes from balance-of-payments data and historical analysis.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.85) because the transition fundamentally altered the distribution of power and benefits in the international monetary system, allowing the reserve currency issuer to extract seigniorage and fiscal flexibility. Suppression is also high (0.90) as the new fiat system effectively suppresses the previous mechanism of creditor leverage, making it nearly impossible for creditor nations to enforce discipline through redemption. Theater ratio is low (0.10) as the shift was a direct, functional change with little performative aspect. Accessibility collapse is high (0.75) because the alternative of gold-backed convertibility was largely eliminated for the reserve currency issuer, though it remains a theoretical option for others. Resistance is moderate (0.40) as creditor nations and some economists continue to advocate for greater fiscal discipline or alternative monetary arrangements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of debtor nations, the transition was a liberation from an oppressive external constraint, enabling national sovereignty over economic policy. From the perspective of creditor nations, it was a loss of legitimate disciplinary power, leading to moral hazard and instability. The engine's per-seat classification will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve_currency_issuer and debtor_nations are the primary beneficiaries, gaining fiscal flexibility and seigniorage (low directionality). Creditor_nations are the victims, losing their leverage and facing the costs of a less disciplined international system (high directionality). International_financial_institutions act as observers, analyzing the system without direct control over its fundamental structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from this reading, was to remove creditor veto power. This mandate is arguably 'live' for debtor nations who continue to benefit from the absence of this constraint. However, for creditor nations, the 'problem' of lost discipline is also live, but from their perspective, the constraint's persistence is a problem, not a solution. The classification as a snare reflects the ongoing extraction from creditor nations by the new arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_creditor_discipline,
    'Was the elimination of creditor veto power the primary causal mechanism of the gold-fiat transition, or a significant but secondary effect of other factors?',
    'Counterfactual historical analysis: what would have happened if creditor discipline had been maintained while other factors (e.g., technological changes, Bretton Woods collapse) still occurred?',
    'If primary, this reading''s high extractiveness and suppression are fully justified. If secondary, the constraint''s true extractiveness might be lower, as some effects would have occurred regardless, potentially shifting the classification towards a Tangled Rope or even a Rope if coordination benefits were more central.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_creditor_discipline, conceptual, 'The extent to which the loss of creditor discipline was the driving force versus a consequence of the gold-fiat transition.').

omega_variable(
    long_term_stability_of_fiat_system,
    'Is the fiscal flexibility enabled by the gold-fiat transition sustainable in the long term, or does it lead to inherent instabilities that will eventually re-impose external constraints?',
    'Empirical observation of future global financial crises, sovereign debt defaults, and the emergence of new international monetary arrangements.',
    'If unsustainable, the long-term extractiveness from creditor nations might be re-evaluated as a temporary benefit for debtor nations, eventually leading to a more diffuse cost. This could shift the classification towards a Scaffold (if temporary) or a Piton (if the system persists by inertia despite its unsustainability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_stability_of_fiat_system, empirical, 'The long-term sustainability of the fiscal flexibility gained by debtor nations post-transition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of creditor leverage structural (due to the new fiat system) or internalized (creditor nations accepting the new reality)?',
    'Post-crisis policy responses: if creditor nations consistently fail to reassert leverage even when opportunities arise, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — creditor nations carry the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for creditor leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.8).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1985, 0.82).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(gold_be_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2010, 0.86).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.85).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1985, 0.88).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(gold_su_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2010, 0.91).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
