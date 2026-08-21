% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity (Sovereignty Guarantor Reading)
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty guarantor' reading of
 *   the EU Council's unanimity rule. In this reading, the rule is a
 *   foundational protection against majoritarian coercion, ensuring that each
 *   member state, particularly smaller ones, retains a veto over collective
 *   actions that implicate its core sovereignty. The rule is seen as a
 *   legitimate exercise of sovereign rights, not as an extractive mechanism.
 *   The moderate extractiveness reflects the inherent coordination costs of
 *   achieving consensus among many sovereign actors, but not systematic
 *   extraction by any single party.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.3).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.1).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity (Sovereignty Guarantor Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, 'f27587f3-269e-454a-8f65-d669530fb8d4').
narrative_ontology:cs_kernel_codification('f27587f3-269e-454a-8f65-d669530fb8d4', fixed_text).
narrative_ontology:cs_authority_grounding('f27587f3-269e-454a-8f65-d669530fb8d4', lineage).
narrative_ontology:cs_interpretation_layer_present('f27587f3-269e-454a-8f65-d669530fb8d4').
narrative_ontology:cs_reading_relation('f27587f3-269e-454a-8f65-d669530fb8d4', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('f27587f3-269e-454a-8f65-d669530fb8d4', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('f27587f3-269e-454a-8f65-d669530fb8d4', foundational, national_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(national_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('f27587f3-269e-454a-8f65-d669530fb8d4', national_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('f27587f3-269e-454a-8f65-d669530fb8d4', foundational, majoritarian_coercion_is_illegitimate).
narrative_ontology:cs_axiom_status(majoritarian_coercion_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('f27587f3-269e-454a-8f65-d669530fb8d4', majoritarian_coercion_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('f27587f3-269e-454a-8f65-d669530fb8d4', founding_treaty_principles).
narrative_ontology:cs_drift_state('f27587f3-269e-454a-8f65-d669530fb8d4', contemporary_eu_expansion_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f27587f3-269e-454a-8f65-d669530fb8d4', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All member states benefit from the assurance that their core sovereign interests cannot be overridden by a simple majority, fostering trust in the collective decision-making process. They participate in the Council and can exercise their veto.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_member_states, beneficiary,
    institutional, generational, constrained, continental).

% These states disproportionately benefit from the unanimity rule, as it provides a critical safeguard against being outvoted by larger states on matters of national importance. Their power is amplified by the veto.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    institutional, generational, constrained, continental).

% The rotating presidency manages the Council's agenda and seeks consensus, but must respect the right of any member state to block proposals implicating sovereignty. They facilitate negotiation but cannot compel agreement.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_presidency, agenda_setter,
    institutional, immediate, constrained, continental).

% Proposes legislation and seeks to advance the Union's overall interests. While it benefits from stable decision-making, it must navigate the unanimity requirement, which can slow or block its initiatives. It observes the process and adapts proposals.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_commission, observer,
    institutional, biographical, constrained, continental).

% Benefit from the stability and perceived legitimacy of decisions that respect national sovereignty, reducing the risk of deep political crises or member state exits. They are indirectly affected by the pace of EU integration.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, beneficiary,
    moderate, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that collective action on sensitive issues (e.g., foreign policy, taxation, treaty changes) proceeds only with the explicit consent of all member states, preventing majoritarian overreach and preserving national sovereignty within the Union framework.
% TRANSFER_FUNCTION: Transfers decision-making power from a simple or qualified majority back to individual member states on specific, high-stakes issues, effectively granting each state a 'veto right' over such collective actions.
% ABSENT_VOICES: Advocates for deeper integration or a more efficient EU decision-making process might argue that the unanimity rule empowers individual states to hold the entire Union hostage, but their concerns are balanced against the foundational principle of sovereign equality.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight, especially for core sovereignty issues, many member states (particularly smaller ones) would likely feel their national interests were no longer adequately protected, leading to significant political instability, potential withdrawals, and a fundamental re-evaluation of the EU's constitutional order.
% FOUNDING_PROBLEM: The European Union was founded on the principle of sovereign equality among its member states, requiring a mechanism to protect national interests from being overridden by larger majorities, particularly after the historical experiences of inter-state conflict.
% FOUNDING_PROBLEM_CORROBORATION: All member states, especially smaller ones, consistently affirm the importance of the unanimity rule as a safeguard for their sovereignty. Treaty texts and historical negotiations corroborate this foundational intent, demonstrating that the rule was a deliberate design choice to ensure broad acceptance and stability within the Union.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.3) because the rule's primary function, in this reading, is to prevent extraction from member states by the majority, rather than to enable extraction by any single state. The costs are primarily coordination costs (time, negotiation effort). Suppression is low (0.1) as the rule is a right, not a coercive force; it suppresses majoritarian impulses but not member state agency. Theater ratio is negligible (0.05) as the rule genuinely functions as intended in this reading. The metrics reflect a constraint that is a genuine coordination mechanism, albeit one with inherent friction.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the protective function of unanimity. Other readings (e.g., 'veto trap') would highlight the potential for minoritarian extraction or gridlock. The engine's per-seat classification would reflect that from the perspective of a state exercising its veto, the rule is a pure benefit, while from the perspective of a state whose initiative is blocked, it might be seen as a cost. This story focuses on the structural benefit to all states from the existence of the safeguard.
 *
 * DIRECTIONALITY LOGIC:
 *   All EU member states, and especially small member states, are beneficiaries (d near 0.0) as the rule protects their sovereign interests. The EU Council Presidency and Commission are observers/agenda-setters who must work within the constraint, but do not directly benefit or pay in an extractive sense. There are no identifiable 'victims' in this reading, as blocking is seen as a legitimate exercise of a right, not an imposition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unanimity_as_veto_trap_ambiguity,
    'Is the unanimity rule primarily a legitimate defense of sovereignty, or does it function as a ''veto trap'' enabling minoritarian extraction through credible blocking threats?',
    'Empirical analysis of veto usage patterns: frequency of vetoes on non-sovereignty issues, correlation between veto use and specific national economic interests, and outcomes of negotiations following a veto.',
    'If vetoes are frequently used to extract concessions on non-sovereignty issues, the constraint would reclassify towards a Tangled Rope or Snare, with higher extractiveness and identifiable victims (the states forced to concede).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unanimity_as_veto_trap_ambiguity, empirical, 'Ambiguity between sovereignty protection and minoritarian extraction.').

omega_variable(
    coordination_cost_vs_extraction_boundary,
    'At what point do the legitimate coordination costs of achieving unanimity transition into an extractive burden imposed by one or more states?',
    'Economic modeling of negotiation deadlocks and their costs, combined with qualitative analysis of diplomatic statements and internal government documents regarding ''red lines'' and ''demands'' in Council negotiations.',
    'A clear identification of systematic costs imposed by specific states beyond reasonable negotiation friction would increase the measured extractiveness and potentially shift the classification towards Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_vs_extraction_boundary, conceptual, 'Distinguishing legitimate coordination costs from extractive demands.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(eu_c_tr_t50, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement(eu_c_be_t50, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 50, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(eu_c_su_t40, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(eu_c_su_t50, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_treaty_revision_process).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_common_foreign_security_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
