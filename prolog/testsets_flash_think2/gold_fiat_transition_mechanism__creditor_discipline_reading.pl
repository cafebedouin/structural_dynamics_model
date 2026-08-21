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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Loss of Creditor Discipline Post Gold-Fiat Transition
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story instantiates the 'creditor_discipline_reading' of
 *   the 'gold_fiat_transition_mechanism' kernel. It focuses on the
 *   geopolitical power shift resulting from the abandonment of gold
 *   convertibility. The constraint is the new fiat-based international
 *   monetary system, which eliminated the gold redemption threat as a form of
 *   balance-of-payments discipline, thereby removing creditor veto power and
 *   enabling greater fiscal flexibility for debtor nations, particularly the
 *   reserve currency issuer. This shift is interpreted as a Tangled Rope
 *   because it provides a coordination function (fiscal flexibility) but
 *   simultaneously extracts leverage from creditor nations through an
 *   actively enforced new monetary order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.75).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Loss of Creditor Discipline Post Gold-Fiat Transition").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '9e9629a3-a010-4bf2-966f-dfcace4b43e4').
narrative_ontology:cs_kernel_codification('9e9629a3-a010-4bf2-966f-dfcace4b43e4', formalized).
narrative_ontology:cs_authority_grounding('9e9629a3-a010-4bf2-966f-dfcace4b43e4', extraction).
narrative_ontology:cs_interpretation_layer_present('9e9629a3-a010-4bf2-966f-dfcace4b43e4').
narrative_ontology:cs_reading_relation('9e9629a3-a010-4bf2-966f-dfcace4b43e4', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e9629a3-a010-4bf2-966f-dfcace4b43e4', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('9e9629a3-a010-4bf2-966f-dfcace4b43e4', foundational, fiscal_flexibility_priority).
narrative_ontology:cs_axiom_status(fiscal_flexibility_priority, holdable).
narrative_ontology:cs_axiom_grounding('9e9629a3-a010-4bf2-966f-dfcace4b43e4', fiscal_flexibility_priority, instrumental).
narrative_ontology:cs_axiom('9e9629a3-a010-4bf2-966f-dfcace4b43e4', foundational, creditor_discipline_as_veto).
narrative_ontology:cs_axiom_status(creditor_discipline_as_veto, holdable).
narrative_ontology:cs_axiom_grounding('9e9629a3-a010-4bf2-966f-dfcace4b43e4', creditor_discipline_as_veto, conventional).
narrative_ontology:cs_reference_frame('9e9629a3-a010-4bf2-966f-dfcace4b43e4', post_bretton_woods_discretionary_era).
narrative_ontology:cs_drift_state('9e9629a3-a010-4bf2-966f-dfcace4b43e4', contemporary_global_finance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9e9629a3-a010-4bf2-966f-dfcace4b43e4', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The nation whose currency serves as the global reserve. Gained significant fiscal flexibility and geopolitical leverage by no longer being constrained by gold convertibility, allowing for greater monetary policy discretion and seigniorage benefits. Actively shaped and benefits from the new fiat system.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Nations that historically faced balance-of-payments discipline under the gold standard. Benefited from increased fiscal flexibility, allowing them to pursue more expansionary monetary and fiscal policies without the immediate threat of gold outflows or creditor veto.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    organized, generational, constrained, global).

% Nations that previously held significant leverage over debtor nations through the threat of gold redemption. Lost this 'veto power' and a key mechanism for enforcing fiscal discipline on international borrowers, experiencing a relative decline in geopolitical influence.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    organized, generational, constrained, global).

% Organizations like the IMF and World Bank, which adapted to and helped manage the new fiat-based international monetary system. Their role shifted from enforcing gold-backed discipline to managing floating exchange rates and providing conditional lending, often aligning with the interests of major reserve currency issuers.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_financial_institutions, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, international_financial_institutions, observer).

% Economists, politicians, and citizens who believed in the inherent discipline and stability of a gold-backed monetary system. Their preferred mechanism for international financial order was dismantled, and their arguments for its restoration are largely marginalized in mainstream policy debates.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_standard_advocates, excluded,
    moderate, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled greater fiscal and monetary policy flexibility for debtor nations by removing the rigid constraint of gold reserves, allowing for more responsive economic management in a globalized economy.
% TRANSFER_FUNCTION: Transferred geopolitical leverage and balance-of-payments discipline from creditor nations (who previously held a 'veto' via gold redemption threats) to the reserve currency issuer, which gained greater monetary autonomy and seigniorage benefits.
% ABSENT_VOICES: Creditor nations, particularly those with large gold reserves, who lost a significant mechanism of influence. Also, gold standard advocates, whose arguments for a disciplined, commodity-backed monetary system were sidelined by the new fiat regime.
% DISAPPEARANCE_RATIONALE: The global financial system is fundamentally structured around fiat currencies and floating exchange rates. If the mechanisms that eliminated creditor gold-redemption power vanished, it would imply a return to a gold-backed system, requiring a complete overhaul of central banking, international trade, and debt structures, leading to massive economic and political reorganization.
% FOUNDING_PROBLEM: The gold standard imposed rigid balance-of-payments discipline, limiting national fiscal and monetary policy responses to economic shocks and creating deflationary pressures, which was seen as an impediment to economic growth and stability.
% FOUNDING_PROBLEM_CORROBORATION: Debtor nations and economists advocating for counter-cyclical fiscal policy corroborate the problem, citing historical periods of deflation and limited policy tools. Creditor nations and gold standard advocates dispute its severity, arguing that the discipline was necessary for long-term stability, and that the current system fosters instability and moral hazard. Corroboration for the 'problem solved' reading comes from the sustained growth and policy flexibility observed in the post-gold era, while the 'problem still live' reading points to increased financial instability and debt accumulation.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because creditor nations lost a significant, tangible form of leverage and influence over international finance. Suppression is high (0.75) as the gold standard was actively dismantled and replaced by a system enforced by central banks and international agreements, which effectively suppresses any return to gold-backed discipline. Theater ratio is low (0.10) because the transition was a fundamental, structural change, not a performative one. The increasing extractiveness and suppression over the interval reflect the gradual erosion of the gold standard's influence leading up to and immediately following the Nixon Shock in 1971, and the hardening of the new fiat regime.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of debtor nations and the reserve currency issuer, the transition was a liberation from an outdated, restrictive system, enabling modern economic management. From the perspective of creditor nations, it was a loss of legitimate disciplinary power and a shift towards a less stable, more inflationary global financial order. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a 'Rope-like' coordination and victims experiencing a 'Snare-like' extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve currency issuer and debtor nations are beneficiaries (low directionality) as they gained fiscal flexibility and geopolitical leverage. Creditor nations are targets (high directionality) as they lost their previous disciplinary power. International financial institutions, while playing an agenda-setting role in the new system, also act as observers. Gold standard advocates are excluded, as their preferred system was dismantled.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the post-gold system as pure extraction (Snare) by acknowledging the genuine coordination function of increased fiscal flexibility for debtor nations. Conversely, it prevents mislabeling it as pure coordination (Rope) by highlighting the asymmetric extraction of power from creditor nations. The 'contested' status of the founding problem further supports the Tangled Rope classification, as the original problem (gold standard rigidity) is seen as solved by some, but its 'solution' created new problems (loss of discipline) for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creditor_leverage_quantification,
    'How precisely can the ''creditor veto power'' under the gold standard be quantified, and what was its actual impact on debtor nation fiscal policy?',
    'Historical econometric analysis comparing fiscal policy outcomes and balance-of-payments adjustments in gold-standard vs. fiat regimes, controlling for other variables.',
    'A clear quantification would strengthen the claim of significant extraction from creditor nations; a weaker or ambiguous impact would suggest lower extractiveness and a more ''Rope-like'' coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_leverage_quantification, empirical, 'Quantifying the lost leverage of creditor nations.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of creditor discipline primarily structural (new institutional arrangements, legal tender laws) or internalized (creditor nations accepting the new norm)?',
    'Analysis of creditor nation policy responses and advocacy efforts post-transition: if active resistance ceased quickly, it suggests internalization; if it continued but was ineffective, it suggests structural suppression.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as creditor nations carry the suppression with them. If purely structural, the constraint''s persistence depends entirely on external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of creditor power.').

omega_variable(
    kernel_reading_divergence_automatic_constraint,
    'To what extent does this ''creditor_discipline_reading'' diverge from the ''automatic_constraint_reading'' of the gold-fiat transition?',
    'Comparative analysis of historical narratives and economic models: if the primary causal mechanism is identified as the removal of a physical limit (automatic constraint), this reading''s emphasis on power shift is secondary; if the power shift is primary, the automatic constraint is a proximate cause.',
    'If the ''automatic_constraint_reading'' is more accurate, the ''creditor_discipline_reading'' might be reclassified as a secondary effect or a less fundamental constraint, potentially lowering its extractiveness and suppression scores as a primary mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence_automatic_constraint, conceptual, 'Divergence from the automatic constraint reading of the gold-fiat transition.').

omega_variable(
    kernel_reading_divergence_composite_overdetermination,
    'How does this ''creditor_discipline_reading'' integrate with the ''composite_overdetermination_reading'' that views the transition as a confluence of multiple factors?',
    'Historical counterfactual analysis: if removing the gold standard alone would not have produced the observed power shift without other concurrent changes (e.g., capital mobility, labor power), then this reading is a partial explanation within a larger overdetermined system.',
    'If the ''composite_overdetermination_reading'' is more accurate, this reading''s causal claims might be re-evaluated as one component among many, potentially reducing its perceived agency and the directness of its extractive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_composite_overdetermination, conceptual, 'Integration with the composite overdetermination reading of the gold-fiat transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(gold_tr_t1970, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1980, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(gold_be_t1970, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1975, 0.8).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1980, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(gold_su_t1970, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1980, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gold_fiat_transition_mechanism' kernel, focusing on the shift in geopolitical power and creditor discipline. It is linked to sibling readings that emphasize other aspects of the transition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
