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
 *   overdetermined outcome of multiple, interacting structural pressures: the
 *   Triffin dilemma (inherent conflict between reserve currency provision and
 *   gold convertibility), escalating Vietnam War deficits, the prevailing
 *   Keynesian policy consensus favoring fiscal flexibility, and increasing
 *   technological capital mobility. These forces collectively made the gold
 *   standard unsustainable by the late 1960s, leading to its formal
 *   abandonment in 1971. This reading emphasizes the systemic inevitability
 *   rather than a single policy choice or dilemma.
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
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, 'f8124312-5af2-456e-83ed-c209c72c32bb').
narrative_ontology:cs_kernel_codification('f8124312-5af2-456e-83ed-c209c72c32bb', formalized).
narrative_ontology:cs_authority_grounding('f8124312-5af2-456e-83ed-c209c72c32bb', extraction).
narrative_ontology:cs_interpretation_layer_present('f8124312-5af2-456e-83ed-c209c72c32bb').
narrative_ontology:cs_reading_relation('f8124312-5af2-456e-83ed-c209c72c32bb', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8124312-5af2-456e-83ed-c209c72c32bb', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('f8124312-5af2-456e-83ed-c209c72c32bb', foundational, monetary_regime_is_emergent_property).
narrative_ontology:cs_axiom_status(monetary_regime_is_emergent_property, holdable).
narrative_ontology:cs_axiom_grounding('f8124312-5af2-456e-83ed-c209c72c32bb', monetary_regime_is_emergent_property, empirically_contingent).
narrative_ontology:cs_axiom('f8124312-5af2-456e-83ed-c209c72c32bb', foundational, multiple_structural_pressures_converged).
narrative_ontology:cs_axiom_status(multiple_structural_pressures_converged, holdable).
narrative_ontology:cs_axiom_grounding('f8124312-5af2-456e-83ed-c209c72c32bb', multiple_structural_pressures_converged, empirically_contingent).
narrative_ontology:cs_reference_frame('f8124312-5af2-456e-83ed-c209c72c32bb', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('f8124312-5af2-456e-83ed-c209c72c32bb', late_1960s_early_1970s, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f8124312-5af2-456e-83ed-c209c72c32bb', '').
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

% The principle of monetary discipline, enforced by the gold standard's convertibility, was eroded. This led to increased inflationary pressures and a loss of the automatic adjustment mechanism that gold provided.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).

% The system of fixed exchange rates, underpinned by the gold standard, collapsed. This introduced greater volatility into international finance and required new mechanisms for currency management.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_regimes, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_regimes).

% Faced increased currency risk and volatility after the gold standard's collapse, but also gained new opportunities for arbitrage and speculation in floating exchange rate markets. Their capital mobility was a key pressure on the old system.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, international_investors, payer,
    powerful, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, international_investors, beneficiary).

% Argued for the inherent stability and discipline of the gold standard, but their arguments were increasingly marginalized by the structural pressures and policy choices that led to its demise. Their preferred system was no longer viable.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard coordinated international monetary policy and trade by providing a fixed anchor for currency values, facilitating predictable cross-border transactions and limiting inflationary domestic policies.
% TRANSFER_FUNCTION: The collapse transferred the constraint of external monetary discipline from national fiscal and monetary authorities, allowing for greater domestic policy autonomy, but also shifting inflation risk to currency holders.
% ABSENT_VOICES: Advocates for strict monetary discipline and a return to a gold-backed system were increasingly excluded from policy debates, as the structural pressures and political will favored a more flexible, fiat-based system. Their arguments were seen as economically unfeasible.
% DISAPPEARANCE_RATIONALE: The gold standard's collapse fundamentally reshaped international finance, leading to floating exchange rates, increased capital mobility, and a new era of macroeconomic policy. Its disappearance was a major structural shift, not a minor adjustment.
% FOUNDING_PROBLEM: The gold standard was established to provide a stable, credible anchor for national currencies, prevent inflation, and facilitate international trade by fixing exchange rates.
% FOUNDING_PROBLEM_CORROBORATION: While some economists and political factions still advocate for a return to a gold standard, the consensus among mainstream economists and central bankers is that the original problems it solved are now addressed by flexible exchange rates and independent central banks, and that the gold standard itself created new, unmanageable problems (e.g., Triffin dilemma). Independent historical analysis corroborates the structural inevitability of its collapse.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.85) and suppression (0.90) metrics reflect the high cost of maintaining the gold standard against overwhelming structural forces. The system became increasingly extractive as it forced governments to choose between domestic policy goals and external convertibility, and required increasing suppression (e.g., capital controls, political pressure) to prevent gold outflows. The low resistance (0.05) indicates that by the late 1960s, the forces against the gold standard were so strong and widely acknowledged that active resistance to its collapse was minimal among key actors. Accessibility collapse (0.95) reflects the near-total closure of viable alternatives to abandoning the gold standard, given the confluence of pressures.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the systemic, multi-causal nature of the collapse, contrasting with readings that focus on a single event (punctuated_swap_reading) or a single dilemma (triffin_inevitability_reading). From the perspective of state fiscal capacity, the gold standard was an increasingly burdensome constraint; from the perspective of monetary discipline, its collapse was a loss of a vital anchor.
 *
 * DIRECTIONALITY LOGIC:
 *   State fiscal capacity and Keynesian policymakers were beneficiaries, as the constraint's removal granted them greater freedom. Monetary discipline and fixed exchange rate regimes were victims, as their structural integrity was undermined. International investors experienced a mixed outcome, facing new risks but also new opportunities. Gold standard advocates were excluded, as their preferred system became structurally untenable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_weight_of_pressures,
    'What was the precise relative weight of each structural pressure (Triffin dilemma, Vietnam War deficits, Keynesian consensus, capital mobility) in making the gold standard''s collapse inevitable?',
    'Counterfactual historical analysis or econometric modeling attempting to isolate the impact of each factor, though definitive resolution is likely impossible due to their interaction.',
    'A clearer understanding of relative weights would refine the narrative of inevitability, potentially highlighting one factor as more ''foundational'' than others, which could shift the classification towards a more ''mountain-like'' (single, irreducible cause) or ''snare-like'' (policy choice) interpretation depending on the dominant factor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relative_weight_of_pressures, empirical, 'Quantifying the contribution of each overdetermining factor.').

omega_variable(
    policy_choice_vs_structural_inevitability,
    'To what extent was the collapse a truly ''overdetermined'' structural inevitability, versus a series of policy choices that, while constrained, still offered alternative paths?',
    'Detailed historical counterfactuals exploring alternative policy responses to each pressure point. This is a conceptual debate about agency versus structure in historical outcomes.',
    'If significant policy choice is identified, the constraint might be reclassified as more ''snare-like'' (extraction by choice) or ''tangled_rope'' (coordination failure with alternatives), rather than a purely ''mountain-like'' (natural law) or ''tangled_rope'' (structural inevitability) outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_choice_vs_structural_inevitability, conceptual, 'The balance between structural inevitability and policy choice in the gold standard''s collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1958, 0.7).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1965, 0.8).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(mone_su_t1950, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1958, 0.75).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1965, 0.85).
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
% This constraint is one of three readings of the 'monetary_anchor_principle' kernel. This 'overdetermined composite' reading emphasizes the multi-causal, systemic inevitability of the gold standard's collapse, contrasting with readings that focus on a single event or dilemma.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
