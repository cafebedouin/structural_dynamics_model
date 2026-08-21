% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Overdetermined Collapse of the Gold Standard (Composite Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story describes the gold standard's collapse as an
 *   overdetermined outcome of multiple, converging structural pressures in
 *   the late 1960s. These pressures included the Triffin dilemma (inherent
 *   contradiction of a reserve currency under a fixed exchange rate),
 *   escalating Vietnam War deficits, a dominant Keynesian policy consensus
 *   favoring fiscal flexibility, and increasing technological capital
 *   mobility. The constraint is classified as a Tangled Rope because it had a
 *   genuine coordination function (monetary stability) but its persistence
 *   became increasingly extractive as it constrained state fiscal capacity
 *   and was actively enforced against mounting pressures, ultimately leading
 *   to its breakdown. The high extractiveness reflects the growing costs of
 *   maintaining an unsustainable system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.85).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.9).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Overdetermined Collapse of the Gold Standard (Composite Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, 'e3080b2d-b8ab-436b-9dc7-6a62817baab4').
narrative_ontology:cs_kernel_codification('e3080b2d-b8ab-436b-9dc7-6a62817baab4', formalized).
narrative_ontology:cs_authority_grounding('e3080b2d-b8ab-436b-9dc7-6a62817baab4', practice).
narrative_ontology:cs_interpretation_layer_present('e3080b2d-b8ab-436b-9dc7-6a62817baab4').
narrative_ontology:cs_reading_relation('e3080b2d-b8ab-436b-9dc7-6a62817baab4', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3080b2d-b8ab-436b-9dc7-6a62817baab4', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('e3080b2d-b8ab-436b-9dc7-6a62817baab4', foundational, monetary_regime_is_emergent_property).
narrative_ontology:cs_axiom_status(monetary_regime_is_emergent_property, holdable).
narrative_ontology:cs_axiom_grounding('e3080b2d-b8ab-436b-9dc7-6a62817baab4', monetary_regime_is_emergent_property, empirically_contingent).
narrative_ontology:cs_axiom('e3080b2d-b8ab-436b-9dc7-6a62817baab4', foundational, structural_pressures_overwhelm_policy_choice).
narrative_ontology:cs_axiom_status(structural_pressures_overwhelm_policy_choice, holdable).
narrative_ontology:cs_axiom_grounding('e3080b2d-b8ab-436b-9dc7-6a62817baab4', structural_pressures_overwhelm_policy_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('e3080b2d-b8ab-436b-9dc7-6a62817baab4', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('e3080b2d-b8ab-436b-9dc7-6a62817baab4', late_1960s_early_1970s, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e3080b2d-b8ab-436b-9dc7-6a62817baab4', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_makers).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, international_investors).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, international_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the removal of the gold constraint, allowing governments to finance deficits (e.g., Vietnam War) without immediate balance of payments crises or gold outflows. This expanded fiscal space and policy flexibility.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity, beneficiary,
    institutional, generational, arbitrage, national).

% Gained greater freedom to implement counter-cyclical fiscal and monetary policies without the rigid external constraint of gold convertibility. This aligned with the prevailing economic consensus of the era.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_makers, beneficiary,
    institutional, biographical, mobile, national).

% The abstract principle of monetary discipline, particularly the constraint on inflation imposed by a fixed gold anchor, was a victim. Its erosion led to increased inflationary pressures in subsequent decades.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).

% The system of fixed exchange rates, which relied on gold convertibility, collapsed. This led to increased volatility in international currency markets and a shift towards floating exchange rates.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_regimes, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_regimes).

% Faced increased currency risk and volatility after the collapse, but also gained new opportunities for arbitrage and speculation in floating exchange rate markets. Their capital mobility was a key pressure point on the old system.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, international_investors, payer,
    powerful, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, international_investors, beneficiary).

% Argued for the inherent stability and discipline of the gold standard, but their arguments were increasingly marginalized by the overwhelming structural pressures and policy consensus favoring its abandonment. Their voice was not central to the decision-making process.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_advocates, excluded,
    moderate, generational, identity_locked, global).

% Analyze the complex interplay of factors leading to the gold standard's collapse, seeking to understand the causal pathways and the inevitability of the outcome. They are detached from the direct benefits or costs of the constraint.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard coordinated international monetary policy by providing a fixed anchor for currency values, facilitating trade and investment by reducing exchange rate risk.
% TRANSFER_FUNCTION: The collapse transferred the constraint of gold convertibility from national treasuries and central banks to the global financial system, allowing for greater fiscal and monetary policy autonomy at the cost of increased currency volatility and inflationary potential.
% ABSENT_VOICES: Advocates for a return to a gold-backed system or alternative fixed-rate mechanisms were largely absent from the policy discussions that led to the final abandonment, their arguments deemed impractical against the backdrop of overwhelming structural pressures.
% DISAPPEARANCE_RATIONALE: The gold standard's collapse fundamentally reshaped international finance, leading to the era of fiat currencies, floating exchange rates, and increased national monetary policy independence. The global economy would be unrecognizable if it had persisted.
% FOUNDING_PROBLEM: The gold standard was established to provide a stable, credible anchor for national currencies, preventing inflation and facilitating international trade by fixing exchange rates.
% FOUNDING_PROBLEM_CORROBORATION: While some economists and political factions still advocate for a return to gold, the consensus among mainstream economists and central bankers is that the original problem of providing a stable monetary anchor is now addressed by independent central banks and flexible exchange rates, rendering the gold standard's specific solution obsolete. Historical accounts from non-beneficiary academic sources corroborate the shift in problem status.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the gold standard, by the late 1960s, imposed severe and unsustainable costs on national fiscal policy and international liquidity, forcing governments to choose between domestic stability and external balance. Suppression (0.90) was also high, as the system required active enforcement (e.g., capital controls, diplomatic pressure) to prevent gold outflows and maintain convertibility in the face of overwhelming market forces. The low theater ratio (0.10) indicates that the system's breakdown was a genuine structural failure, not merely a performance; the efforts to maintain it were increasingly futile against the underlying pressures. The rising extractiveness and suppression over the interval reflect the increasing strain on the system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state fiscal capacity, the gold standard became an increasingly untenable constraint, while from the perspective of monetary discipline, its erosion represented a loss of a vital anchor. The engine's per-seat classification would reflect this divergence, with beneficiaries experiencing a low effective extraction (or even subsidy) and victims experiencing high effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   State fiscal capacity and Keynesian policy makers were beneficiaries, as the collapse freed them from the gold constraint, allowing for greater policy autonomy. Monetary discipline and fixed exchange rate regimes were victims, as the structural pressures eroded their viability. International investors experienced both costs (volatility) and benefits (arbitrage opportunities) as the system transitioned. Gold standard advocates were excluded, their arguments unable to stem the tide of structural inevitability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_weight_of_factors,
    'What was the precise causal weighting of each contributing factor (Triffin dilemma, Vietnam deficits, Keynesian consensus, capital mobility) in the gold standard''s collapse?',
    'Counterfactual historical analysis and econometric modeling attempting to isolate the impact of each factor, though definitive resolution is likely impossible due to their entanglement.',
    'A clearer weighting would refine the understanding of which ''mountains'' were most influential, potentially shifting the focus of policy lessons for future monetary regimes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_weight_of_factors, empirical, 'Ambiguity in the relative importance of multiple causal factors.').

omega_variable(
    inevitability_vs_agency,
    'To what extent was the collapse truly ''inevitable'' due to structural pressures, versus being a consequence of specific policy choices and political will?',
    'Comparative historical analysis of other countries'' responses to similar pressures, and detailed examination of the decision-making processes leading up to August 1971.',
    'If agency played a larger role, the constraint might be reclassified closer to a Snare (pure extraction by choice) or a Rope (coordination failure), rather than a Tangled Rope driven by structural forces. If inevitability is confirmed, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_vs_agency, conceptual, 'The balance between structural inevitability and human agency in the constraint''s resolution.').

omega_variable(
    triffin_dilemma_sufficiency,
    'Was the Triffin dilemma alone sufficient to cause the collapse, or did other factors (e.g., Vietnam War deficits) provide necessary acceleration?',
    'Theoretical modeling and historical counterfactuals exploring scenarios where the Triffin dilemma existed without the other accelerating factors.',
    'If the Triffin dilemma was sufficient, the ''triffin_inevitability_reading'' gains strength, potentially influencing the network relationship between this composite reading and the Triffin-specific one. If not, the composite reading''s emphasis on multiple factors is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_sufficiency, empirical, 'Whether the Triffin dilemma was a sufficient or merely necessary condition for collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1960, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1960, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(mone_tr_t1963, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1963, 0.2).
narrative_ontology:measurement(mone_tr_t1966, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1966, 0.15).
narrative_ontology:measurement(mone_tr_t1969, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1969, 0.12).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1960, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(mone_be_t1963, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1963, 0.68).
narrative_ontology:measurement(mone_be_t1966, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1966, 0.75).
narrative_ontology:measurement(mone_be_t1969, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1969, 0.82).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1960, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(mone_su_t1963, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1963, 0.78).
narrative_ontology:measurement(mone_su_t1966, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1966, 0.85).
narrative_ontology:measurement(mone_su_t1969, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1969, 0.88).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, fiat_currency_regime).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, floating_exchange_rate_system).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'monetary_anchor_principle' kernel, focusing on the overdetermined composite of structural pressures leading to the gold standard's collapse. It is linked to sibling readings that emphasize different causal pathways or moments of transition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
