% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Monetary Anchor Principle: Punctuated Swap Reading (Nixon Shock)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story models the 'punctuated swap' reading of the
 *   monetary anchor principle, focusing on the August 15, 1971, decision by
 *   the U.S. to unilaterally suspend the dollar's convertibility to gold.
 *   This reading emphasizes the discrete institutional choice that shifted
 *   the global monetary regime from fixed exchange rates (Bretton Woods) to a
 *   floating system. It views the event as a coordination failure leading to
 *   a unilateral defection, benefiting U.S. fiscal autonomy while effectively
 *   expropriating foreign dollar holders through devaluation. The constraint
 *   is claimed as a Rope, as it represents a new, albeit unilaterally
 *   imposed, coordination mechanism for international finance, with moderate
 *   extractiveness due to its reversibility in principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.45).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.6).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Monetary Anchor Principle: Punctuated Swap Reading (Nixon Shock)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '2950561f-d429-4697-bc77-2bc9ff221eb3').
narrative_ontology:cs_kernel_codification('2950561f-d429-4697-bc77-2bc9ff221eb3', formalized).
narrative_ontology:cs_authority_grounding('2950561f-d429-4697-bc77-2bc9ff221eb3', lineage).
narrative_ontology:cs_interpretation_layer_present('2950561f-d429-4697-bc77-2bc9ff221eb3').
narrative_ontology:cs_reading_relation('2950561f-d429-4697-bc77-2bc9ff221eb3', monetary_anchor_principle__overdetermined_composite_reading, forecloses).
narrative_ontology:cs_reading_relation('2950561f-d429-4697-bc77-2bc9ff221eb3', monetary_anchor_principle__triffin_inevitability_reading, forecloses).
narrative_ontology:cs_axiom('2950561f-d429-4697-bc77-2bc9ff221eb3', foundational, institutional_agency_is_primary).
narrative_ontology:cs_axiom_status(institutional_agency_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('2950561f-d429-4697-bc77-2bc9ff221eb3', institutional_agency_is_primary, conventional).
narrative_ontology:cs_axiom('2950561f-d429-4697-bc77-2bc9ff221eb3', foundational, monetary_regime_is_discrete_choice).
narrative_ontology:cs_axiom_status(monetary_regime_is_discrete_choice, holdable).
narrative_ontology:cs_axiom_grounding('2950561f-d429-4697-bc77-2bc9ff221eb3', monetary_regime_is_discrete_choice, conventional).
narrative_ontology:cs_reference_frame('2950561f-d429-4697-bc77-2bc9ff221eb3', sovereign_monetary_autonomy).
narrative_ontology:cs_drift_state('2950561f-d429-4697-bc77-2bc9ff221eb3', post_nixon_shock, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2950561f-d429-4697-bc77-2bc9ff221eb3', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authorities).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_trading_partners).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, us_trading_partners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Made the unilateral decision to suspend dollar convertibility to gold, gaining immediate fiscal autonomy and flexibility in monetary policy. Benefited from the ability to devalue the dollar without gold constraints.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Held large reserves of U.S. dollars that were suddenly devalued against gold, representing an effective expropriation of wealth. Their options were limited due to the dollar's role as the global reserve currency.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    institutional, biographical, constrained, global).

% The institution designed to manage the Bretton Woods system, which was effectively dismantled by the Nixon Shock. Its role shifted from managing fixed exchange rates to overseeing floating ones.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Initially faced uncertainty and devaluation of their dollar reserves, but eventually benefited from increased competitiveness of their exports due to the weaker dollar. Had to adapt to a new floating exchange rate regime.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_trading_partners, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_trading_partners, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system provided a framework for international monetary coordination, stabilizing exchange rates and facilitating global trade and investment through a dollar-gold peg. The 'punctuated swap' was a unilateral defection from this coordination.
% TRANSFER_FUNCTION: The constraint, as a 'punctuated swap,' transferred fiscal autonomy and monetary policy flexibility to the U.S. at the expense of foreign dollar holders, who experienced a devaluation of their reserves.
% ABSENT_VOICES: Foreign central banks and treasuries, particularly those with large dollar holdings, were not consulted in the unilateral decision. They would have argued for a multilateral solution or a more gradual transition to protect their reserves.
% DISAPPEARANCE_RATIONALE: If the decision to suspend gold convertibility had not occurred, the international monetary system would have continued under the Bretton Woods rules, likely leading to a different path for global finance, trade, and U.S. fiscal policy. The world would have rearranged around a different monetary anchor.
% FOUNDING_PROBLEM: The U.S. faced a 'balance of payments' crisis, with increasing deficits and a dwindling gold supply, making the dollar's convertibility to gold unsustainable under the existing fixed exchange rate regime.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians widely corroborate the existence of the balance of payments crisis and the pressure on gold reserves. While the 'solution' is contested, the problem itself is not, and similar pressures on reserve currencies continue to be debated by international financial institutions and academics.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).
:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.45) because while foreign dollar holders experienced a loss, the new floating regime offered other benefits and was not entirely coercive. Suppression (0.6) reflects the unilateral nature of the decision, which left other nations with limited immediate recourse. The low theater ratio (0.1) indicates that the action was a direct, functional policy change with little performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of U.S. fiscal authorities, the decision was a necessary act to restore national economic sovereignty and address an unsustainable balance of payments. From the perspective of foreign dollar holders, it was a unilateral act of expropriation. The engine's classification will reflect this divergence based on the declared beneficiary/victim structure and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. fiscal authorities are the primary beneficiaries, gaining significant monetary policy freedom (low d). Foreign dollar holders are the victims, bearing the cost of devaluation with constrained exit options (high d). The IMF and U.S. trading partners occupy more complex positions, with the latter experiencing both costs and benefits, leading to a more symmetric directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_of_nixon_shock,
    'Was the Nixon Shock a discrete institutional choice, or was it an inevitable outcome of underlying structural pressures (e.g., Triffin dilemma, Vietnam War deficits)?',
    'Counterfactual historical analysis: detailed modeling of alternative policy paths and their likelihood given the economic and political constraints of the time. Examination of archival evidence for decision-making processes.',
    'If primarily a choice, the constraint is more ''constructed'' and potentially reversible (Rope/Tangled Rope). If inevitable, it leans towards a ''Mountain'' or a highly constrained ''Snare'' where agents had little agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_of_nixon_shock, conceptual, 'Ambiguity between institutional choice and structural inevitability in the 1971 monetary transition.').

omega_variable(
    expropriation_vs_adjustment,
    'To what extent was the devaluation of foreign dollar holdings an act of expropriation versus a necessary market adjustment to an overvalued currency?',
    'Economic modeling comparing the actual devaluation to a ''fair value'' determined by purchasing power parity or other fundamental metrics, alongside legal analysis of international monetary agreements.',
    'A higher degree of expropriation would increase the effective extractiveness for foreign dollar holders, potentially shifting the constraint towards a Snare. A greater degree of adjustment would support the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expropriation_vs_adjustment, empirical, 'The nature of wealth transfer from foreign dollar holders post-Nixon Shock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1971, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'monetary_anchor_principle' kernel, emphasizing the discrete institutional choice of the Nixon Shock. It is linked to sibling readings that focus on structural inevitability or overdetermined factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
