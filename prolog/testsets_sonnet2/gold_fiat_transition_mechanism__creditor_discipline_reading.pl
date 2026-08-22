% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Nixon Shock as Elimination of Creditor Redemption Discipline
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This story reads the 1971 suspension of dollar-gold convertibility as,
 *   structurally, the elimination of a creditor veto: prior to Nixon's
 *   declaration, foreign central banks holding dollar claims could demand
 *   gold redemption, and the credible threat of a gold run disciplined US
 *   balance-of-payments behavior. Closing the gold window removed that
 *   discipline unilaterally, shifting bargaining power from creditor nations
 *   to the reserve-currency issuer, who thereafter could run deficits and
 *   inflate obligations away without the prior external check. This is ONE of
 *   three declared readings of the gold-to-fiat transition kernel. The
 *   automatic_constraint_reading frames the same event as a
 *   material-to-institutional constraint-type change (gold reserve ceiling to
 *   discretionary central bank authority) without emphasizing a
 *   creditor/debtor power transfer. The composite_overdetermination_reading
 *   denies the Nixon Shock is even the causal node, treating it as a symbolic
 *   marker for a convergence of telecom-enabled capital mobility, Bretton
 *   Woods peg collapse, and labor-power shifts. This reading's ε is high
 *   because it identifies a specific, asymmetric transfer of bargaining
 *   leverage with named winners (the issuer) and losers (creditor nations) —
 *   a different empirical claim from either sibling, not a restatement of
 *   them under another lens.
 *
 * KEY AGENTS:
 *   - united_states_treasury: primary beneficiary and agenda-setter — gained unilateral discretion
 *   - creditor_nations: primary victim — lost redemption leverage without negotiation
 *   - foreign_dollar_reserve_holders: diffuse ongoing victim — bear inflation/devaluation risk with constrained exit
 *   - international_monetary_economists: analytical observer of the bargaining-power shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.68).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Nixon Shock as Elimination of Creditor Redemption Discipline").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '3cbac807-ffd5-4d09-8f66-03ed9b8d2e52').
narrative_ontology:cs_kernel_codification('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', formalized).
narrative_ontology:cs_authority_grounding('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', extraction).
narrative_ontology:cs_interpretation_layer_present('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52').
narrative_ontology:cs_reading_relation('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', foundational, reserve_issuer_bargaining_advantage_is_structural_not_incidental).
narrative_ontology:cs_axiom_status(reserve_issuer_bargaining_advantage_is_structural_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', reserve_issuer_bargaining_advantage_is_structural_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', secondary, unilateral_suspension_constitutes_uncompensated_transfer).
narrative_ontology:cs_axiom_status(unilateral_suspension_constitutes_uncompensated_transfer, holdable).
narrative_ontology:cs_axiom_grounding('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', unilateral_suspension_constitutes_uncompensated_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', bretton_woods_convertibility_bargain).
narrative_ontology:cs_drift_state('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', post_1971_discretionary_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('3cbac807-ffd5-4d09-8f66-03ed9b8d2e52', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, united_states_treasury).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer_governments).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_debtor_constituencies).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, export_surplus_economies).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, sovereign_fiscal_flexibility_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_exorbitant_privilege_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unilaterally closed the gold window in August 1971, ending convertibility of dollars held by foreign central banks. Retains the power to run balance-of-payments deficits without the prior threat of a gold run forcing contractionary policy. Issues the reserve currency other nations must hold, so it alone escaped the redemption discipline it had itself imposed on others under Bretton Woods.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, united_states_treasury, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, united_states_treasury, beneficiary).

% Successor and allied reserve-adjacent issuers (and dollar-linked financial centers) benefit from the same removal of automatic external discipline extended by proximity to the dollar system, gaining latitude other governments do not have.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer_governments, beneficiary,
    institutional, generational, mobile, global).

% US fiscal and monetary authorities gained room to run deficits and expand credit for domestic priorities (war financing, social spending, employment stabilization) without an external gold-redemption trigger forcing austerity. This flexibility flows to domestic borrowers and policymakers, not directly to foreign counterparties.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_debtor_constituencies, beneficiary,
    moderate, biographical, constrained, national).

% Nations (notably France, and later surplus economies generally) that had accumulated dollar claims and previously could threaten redemption in gold to discipline US deficit spending lost that leverage overnight. They hold dollar reserves that can now be devalued by US monetary policy with no convertibility exit; their bargaining tool was removed by unilateral declaration, not negotiation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, generational, trapped, global).

% Central banks and sovereign holders of dollar-denominated reserves bear the inflation and devaluation risk of US policy choices they cannot veto. Diversifying away from the dollar is costly and slow because the dollar remains the invoicing and settlement currency for most global trade; exit exists in principle but is throttled by network effects the constraint itself sustains.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_dollar_reserve_holders, payer,
    organized, generational, constrained, global).

% Economies that ran trade surpluses with the United States and accumulated dollar claims as the counterpart now hold an asset whose real value the issuer can erode unilaterally through inflation or exchange-rate policy, with no automatic mechanism forcing US adjustment in return.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, export_surplus_economies, payer,
    powerful, generational, constrained, global).

% Analyze the shift in bargaining power between debtor and creditor nations as a structural consequence of reserve-currency status, separate from the technical mechanics of the convertibility suspension itself.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, united_states_treasury).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the pre-1971 gold-convertibility peg genuinely coordinated international settlement by giving creditor nations a credible, non-negotiable enforcement mechanism against reserve-currency profligacy — a real function, but one whose coordination benefit accrued asymmetrically to creditors holding the veto.
% TRANSFER_FUNCTION: The transition moved bargaining leverage from creditor nations (who could force US policy adjustment via redemption threats) to the reserve-currency issuer (who gained unilateral discretion over the value of obligations owed to those same creditors). Concretely: fiscal and monetary flexibility flowed to the United States and dollar-adjacent debtors; devaluation and inflation risk flowed to foreign holders of dollar claims.
% ABSENT_VOICES: Creditor-nation finance ministries (France under de Gaulle most vocally) objected in real time and were overruled by unilateral US action; non-reserve-currency developing nations that had no comparable escape valve were not party to the decision at all and bore compounding effects through dollar-denominated trade and debt.
% DISAPPEARANCE_RATIONALE: If reserve-currency discretion were reversed and a redemption-style discipline restored, the United States and other reserve issuers would face renewed external constraint on deficit spending, creditor nations would regain leverage to force adjustment, and global reserve-holding patterns and trade-settlement currency choices would reorganize substantially.
% FOUNDING_PROBLEM: Bretton Woods gold convertibility was built to give creditor nations a credible check against reserve-currency debasement, so that a country's dollar claims retained a floor value regardless of the issuer's domestic political incentives.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary French officials (de Gaulle, Giscard d'Estaing, coining 'exorbitant privilege') attested from outside the beneficiary set that the arrangement had shifted from mutual discipline to asymmetric extraction well before 1971. Post-hoc international monetary historians analyzing reserve-currency seigniorage corroborate that the check function is structurally dead for the issuer and was never revived by any successor arrangement.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78 at interval end) because the reading's central claim is a bargaining-power transfer with an identifiable extracting party (the reserve issuer) and identifiable payers (creditor and surplus nations) who cannot force reciprocal adjustment. Suppression (0.68) reflects that the new arrangement persists via the structural lock-in of dollar-invoicing and settlement networks, not through participant preference — exiting dollar reserve exposure is costly and slow, which is itself a suppression mechanism rather than a natural feature of trade. Theater ratio rose over the interval (0.1 to 0.42) as G7/IMF coordination fora (Plaza Accord, G20 communiques on 'global imbalances') increasingly perform concern about reserve-currency asymmetry without altering the underlying discretion the issuer retains.
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury's seat, the 1971 decision reads as a sovereign nation reclaiming domestic policy autonomy from an external constraint that had become untenable given depleted gold reserves — a defensible unilateral act. From the creditor-nation seat, the same act reads as an extraction: a debtor with control over the settlement medium unilaterally rewrote the terms of outstanding claims held against it, with no compensating mechanism offered to the parties who lost their enforcement tool. The engine computes these as different effective-extraction values from the same structural data, driven by directionality — the beneficiary's arbitrage-grade exit versus the creditor's trapped position.
 *
 * DIRECTIONALITY LOGIC:
 *   United States Treasury and reserve-currency-adjacent governments sit near the full-beneficiary end: they retained or gained discretion and face no binding external redemption threat. Creditor nations and foreign dollar reserve holders sit near the full-target end: they hold devaluable claims with no exit that does not itself impose large switching costs (abandoning dollar invoicing, unwinding reserve portfolios). Export-surplus economies are intermediate-high: their surplus accumulation was a rational trade strategy that became a structural liability only after the redemption exit closed.
 *
 * MANDATROPHY ANALYSIS:
 *   The pre-1971 convertibility peg had a genuine coordination function (giving dollar claims a credible floor value), which this reading does not deny — it is authored as tangled_rope, not pure snare, because the coordination function was real for the system's first 25 years. What changed is not the existence of coordination but its capture: the issuer retained the coordination benefit (a trusted reserve currency other nations still must hold) while shedding the reciprocal discipline that made the coordination bargain fair. Classifying this as tangled_rope rather than snare avoids erasing the genuine settlement-coordination value the dollar system still provides; classifying it as tangled_rope rather than rope avoids treating the asymmetric burden on creditor nations as a benign byproduct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_necessity_vs_extraction,
    'Was the 1971 suspension a necessary sovereign response to an untenable gold-reserve depletion (a forced move with no real alternative), or a discretionary extraction of bargaining advantage available specifically because the US held reserve-currency status?',
    'Historical analysis of US gold reserve trajectories and alternative policy paths available in 1971 (e.g., negotiated devaluation within Bretton Woods vs. unilateral suspension) would clarify whether necessity or opportunism better explains the choice of unilateral action over negotiation.',
    'If necessity dominates, this reading''s extraction claim weakens toward the automatic_constraint_reading''s framing (structural adaptation, not power grab). If opportunism dominates, the creditor_discipline_reading''s high epsilon is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_necessity_vs_extraction, conceptual, 'Whether unilateral suspension was forced necessity or discretionary extraction of reserve-currency advantage.').

omega_variable(
    creditor_nation_counterfactual_leverage,
    'Would creditor nations have actually exercised the redemption veto to force US adjustment absent the suspension, or was the threat itself already hollow by 1971 given the scale of outstanding dollar claims relative to US gold stock?',
    'Quantitative comparison of outstanding foreign dollar claims versus Fort Knox gold reserves in 1971, plus documentary evidence of actual redemption demands (France''s 1965-1967 gold purchases) versus rhetorical threats.',
    'If the veto was already practically unexercisable at scale, the ''elimination'' framing overstates what was actually lost — the discipline may have already been eroding independent of the formal declaration, favoring the composite_overdetermination_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_nation_counterfactual_leverage, empirical, 'Whether the creditor veto had genuine remaining force in 1971 or was already structurally hollow.').

omega_variable(
    kernel_reading_partition_boundary,
    'Where exactly does the creditor_discipline_reading''s causal claim (power transfer from creditors to issuer) stop being compatible with the automatic_constraint_reading''s claim (material-to-institutional constraint substitution) — are these genuinely independent claims about the same event, or is the power-transfer claim simply a value-laden restatement of the constraint-type-substitution claim?',
    'Structural test: does the constraint-type-substitution claim (gold ceiling to discretionary authority) entail the power-transfer claim (creditor to issuer), or are there possible worlds where the constraint type changed without any creditor/debtor power shift (e.g., if convertibility had been replaced by a multilaterally-negotiated SDR-based discipline rather than unilateral discretion)?',
    'If the power-transfer claim is logically independent of the constraint-type claim (as the SDR counterfactual suggests), the two readings are genuinely decomposed constraints per the epsilon-invariance principle, each earning its own file. If the power-transfer claim is entailed by the constraint-type claim, the decomposition may be over-fine and the readings should be merged with disclosed sub-claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_partition_boundary, conceptual, 'Whether the creditor_discipline_reading is genuinely independent of automatic_constraint_reading or a value-laden restatement of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(gold_tr_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.3).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(gold_be_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1944, 0.2).
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(gold_be_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1968, 0.5).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.7).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1985, 0.72).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1944, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1944, 0.3).
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(gold_su_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1985, 0.63).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2000, 0.66).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, petrodollar_recycling_arrangement).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_special_drawing_rights_facility).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the gold_fiat_transition_mechanism kernel, each authored as a separate constraint per the epsilon-invariance principle. automatic_constraint_reading frames the 1971 event as a material-to-institutional constraint-type substitution with low-to-moderate epsilon and no central power-transfer claim. composite_overdetermination_reading denies the Nixon Shock is even the primary causal node, distributing causation across telecom-driven capital mobility, Bretton Woods peg collapse, and labor bargaining shifts, with correspondingly diffuse beneficiary/victim structure. creditor_discipline_reading (this story) authors the highest epsilon of the three because it makes the specific, contestable claim of an identifiable bargaining-power transfer from creditor nations to the reserve-currency issuer, with named winners and losers. The three do not average into a single epsilon; each is a distinct structurally-precise claim linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
