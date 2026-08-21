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
 *   the power of creditor nations to impose fiscal constraints on debtor
 *   nations through the threat of gold redemption. This reading highlights
 *   the geopolitical power shift from creditors to the reserve-currency
 *   issuer, which gained unprecedented fiscal flexibility. The constraint is
 *   classified as a Snare because it fundamentally altered the terms of
 *   international economic engagement, creating a system where the
 *   reserve-currency issuer benefits disproportionately at the expense of
 *   traditional creditor leverage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.9).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, snare).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Gold-Fiat Transition: Creditor Discipline Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '6919f387-251f-4562-9d53-05b807e979e9').
narrative_ontology:cs_kernel_codification('6919f387-251f-4562-9d53-05b807e979e9', formalized).
narrative_ontology:cs_authority_grounding('6919f387-251f-4562-9d53-05b807e979e9', extraction).
narrative_ontology:cs_reading_relation('6919f387-251f-4562-9d53-05b807e979e9', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('6919f387-251f-4562-9d53-05b807e979e9', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('6919f387-251f-4562-9d53-05b807e979e9', foundational, creditor_leverage_is_a_constraint).
narrative_ontology:cs_axiom_status(creditor_leverage_is_a_constraint, holdable).
narrative_ontology:cs_axiom_grounding('6919f387-251f-4562-9d53-05b807e979e9', creditor_leverage_is_a_constraint, conventional).
narrative_ontology:cs_axiom('6919f387-251f-4562-9d53-05b807e979e9', foundational, fiscal_flexibility_is_a_national_good).
narrative_ontology:cs_axiom_status(fiscal_flexibility_is_a_national_good, holdable).
narrative_ontology:cs_axiom_grounding('6919f387-251f-4562-9d53-05b807e979e9', fiscal_flexibility_is_a_national_good, instrumental).
narrative_ontology:cs_reference_frame('6919f387-251f-4562-9d53-05b807e979e9', post_bretton_woods_fiat_system).
narrative_ontology:cs_drift_state('6919f387-251f-4562-9d53-05b807e979e9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6919f387-251f-4562-9d53-05b807e979e9', '').
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

% Gained fiscal flexibility and reduced external discipline on their balance of payments, no longer constrained by gold redemption threats. This allowed for more expansive domestic policy choices.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    powerful, generational, mobile, global).

% Achieved unparalleled geopolitical power by eliminating the gold convertibility constraint, allowing it to run persistent trade deficits and fund global military and economic initiatives without external discipline. It effectively became the ultimate beneficiary of the new system.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Lost their primary leverage over debtor nations, which was the threat of demanding gold redemption for accumulated foreign currency reserves. Their ability to impose fiscal discipline on trading partners was significantly diminished, shifting the balance of power.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    organized, biographical, constrained, global).

% Observed and adapted to the new monetary regime, often facilitating the transition and managing the new system's rules. Their role shifted from managing a gold-backed system to one based on fiat currencies and floating exchange rates.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_financial_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The transition coordinated the global monetary system away from a gold-backed standard, establishing a new framework for international payments and exchange rates based on fiat currencies.
% TRANSFER_FUNCTION: Transferred significant geopolitical and economic leverage from creditor nations (who could demand gold) to debtor nations, particularly the reserve currency issuer, by removing the external constraint on fiscal and monetary policy.
% ABSENT_VOICES: Advocates for a return to a gold standard or alternative forms of international monetary discipline, who argue for external constraints on national fiscal policy, are largely marginalized in the current fiat system. Their arguments for 'sound money' are often dismissed as anachronistic.
% DISAPPEARANCE_RATIONALE: The gold-fiat transition is a historical event; its 'disappearance' would mean a counterfactual return to a gold standard, which would fundamentally rearrange the global economic and political order. The current system is built upon its premise.
% FOUNDING_PROBLEM: The gold standard imposed rigid constraints on national monetary policy, limiting governments' ability to respond to economic crises and manage domestic demand, and created inherent instability in the international monetary system due to gold flow imbalances.
% FOUNDING_PROBLEM_CORROBORATION: Economists and policymakers widely agree that the gold standard's rigidity was a significant problem, particularly during the Great Depression. However, the 'creditor discipline' aspect of the problem is contested; some argue it was a necessary check on profligate spending, while others see it as an undue external imposition. The reserve currency issuer's perspective strongly supports the 'dead' status, while some creditor nations might argue the 'problem' of fiscal discipline has merely shifted, not disappeared.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_unchanged).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the transition fundamentally removed a check on the reserve-currency issuer's fiscal policy, allowing it to accrue significant benefits (e.g., funding deficits, geopolitical influence) without the previous external discipline. Suppression is also high (0.90) because the new fiat system, once established, effectively suppressed any return to gold-backed discipline, making it nearly impossible for creditor nations to reassert their former leverage. The system is actively enforced through the global acceptance of the reserve currency and the lack of viable alternatives for international settlement. Theater ratio is low (0.05) as the system's function is direct and effective in maintaining the new power dynamic, with little performative overhead.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the reserve-currency issuer, the transition was a necessary evolution to a more flexible and stable global monetary system. From the perspective of creditor nations, it was a loss of legitimate leverage and a shift towards an imbalanced system. The engine's classification as a Snare reflects the structural asymmetry of this power shift, regardless of the 'necessity' arguments.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve-currency issuer and debtor nations are clear beneficiaries, gaining fiscal and monetary autonomy. Creditor nations are the victims, losing their primary mechanism for imposing discipline. International financial institutions act as observers, adapting to and managing the new system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from this reading, was to free debtor nations from creditor discipline. While the 'problem' of gold-standard rigidity is widely considered dead, the 'problem' of fiscal discipline (or lack thereof) is contested. The persistence of the fiat system, with its high extractiveness and suppression from the creditor's perspective, indicates that the constraint continues to serve the interests of its beneficiaries, even if the original problem it 'solved' has evolved or is viewed differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_creditor_discipline,
    'To what extent was the elimination of creditor discipline the primary driver of the gold-fiat transition, versus other factors like the inherent instability of the Bretton Woods system or the desire for domestic monetary autonomy?',
    'Detailed historical counterfactual analysis, comparing outcomes in scenarios where creditor discipline was maintained but other factors varied, or vice versa. Econometric analysis of the relative impact of different pressures leading to the transition.',
    'If creditor discipline was a minor factor, this reading''s high extractiveness and snare classification might be overstated, suggesting a more complex or less intentionally extractive transition. If it was a primary driver, the classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_creditor_discipline, empirical, 'Assessing the causal weight of creditor discipline in the gold-fiat transition.').

omega_variable(
    long_term_stability_of_fiat_system,
    'Is the current fiat system, freed from gold-backed creditor discipline, inherently more stable or unstable in the long run compared to a gold-backed system?',
    'Longitudinal economic data analysis comparing volatility, crisis frequency, and wealth distribution under both regimes. Theoretical modeling of monetary system resilience under different constraints.',
    'If the fiat system proves less stable, the ''beneficiary'' status of debtor nations might be re-evaluated in the long term, suggesting a hidden cost to the gained flexibility. If more stable, it strengthens the coordination aspect of the transition, potentially lowering effective extraction over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_stability_of_fiat_system, empirical, 'Evaluating the long-term stability implications of the gold-fiat transition.').

omega_variable(
    normative_value_of_creditor_discipline,
    'Is creditor discipline, as enforced by a gold standard, a legitimate and desirable mechanism for global economic stability, or an illegitimate imposition of power by creditors?',
    'Philosophical and ethical analysis of international economic governance, examining principles of national sovereignty, global equity, and the role of external constraints on fiscal policy. This is a preference-based question.',
    'The classification of the transition as ''snare'' is based on the structural transfer of power and extraction. A normative judgment on the desirability of creditor discipline would not change the structural classification but would alter the moral valence assigned to the outcome by different observers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_value_of_creditor_discipline, preference, 'Normative assessment of creditor discipline in international finance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(gold_tr_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1990, 0.06).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(gold_tr_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.8).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(gold_be_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1990, 0.83).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(gold_be_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.85).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1980, 0.88).
narrative_ontology:measurement(gold_su_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1990, 0.89).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(gold_su_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2010, 0.9).
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
