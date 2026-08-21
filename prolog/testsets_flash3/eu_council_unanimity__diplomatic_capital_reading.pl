% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity (Diplomatic Capital Reading)
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint describes the unanimity requirement in the EU Council
 *   from the perspective that it is a crucial mechanism for
 *   consensus-building, leading to stronger policy legitimacy and reduced
 *   defection. While it imposes significant negotiation costs, these are
 *   viewed as necessary investments in the long-term stability and
 *   effectiveness of EU policy. This reading emphasizes the positive
 *   coordination function, contrasting with views that highlight its
 *   potential for gridlock or extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.25).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.15).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity (Diplomatic Capital Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '311b81e4-1a97-4101-b8a1-febb543b77a9').
narrative_ontology:cs_kernel_codification('311b81e4-1a97-4101-b8a1-febb543b77a9', formalized).
narrative_ontology:cs_authority_grounding('311b81e4-1a97-4101-b8a1-febb543b77a9', lineage).
narrative_ontology:cs_interpretation_layer_present('311b81e4-1a97-4101-b8a1-febb543b77a9').
narrative_ontology:cs_reading_relation('311b81e4-1a97-4101-b8a1-febb543b77a9', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('311b81e4-1a97-4101-b8a1-febb543b77a9', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('311b81e4-1a97-4101-b8a1-febb543b77a9', foundational, consensus_enhances_legitimacy).
narrative_ontology:cs_axiom_status(consensus_enhances_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('311b81e4-1a97-4101-b8a1-febb543b77a9', consensus_enhances_legitimacy, conventional).
narrative_ontology:cs_axiom('311b81e4-1a97-4101-b8a1-febb543b77a9', foundational, negotiation_as_investment).
narrative_ontology:cs_axiom_status(negotiation_as_investment, holdable).
narrative_ontology:cs_axiom_grounding('311b81e4-1a97-4101-b8a1-febb543b77a9', negotiation_as_investment, instrumental).
narrative_ontology:cs_reference_frame('311b81e4-1a97-4101-b8a1-febb543b77a9', post_lisbon_treaty_consensus_model).
narrative_ontology:cs_drift_state('311b81e4-1a97-4101-b8a1-febb543b77a9', contemporary_geopolitical_challenges, gap(stable, minor, true)).
narrative_ontology:cs_created_at('311b81e4-1a97-4101-b8a1-febb543b77a9', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_policy_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_citizens).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, consensus_builds_legitimacy).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, iterative_negotiation_strengthens_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the enhanced legitimacy and durability of policies adopted through unanimity, as it ensures their core interests are addressed. They bear the cost of extended negotiation but gain long-term policy stability and reduced defection risk.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Responsible for brokering consensus among member states. They invest significant diplomatic capital and time in iterative negotiations to achieve unanimous decisions, which then carry greater political weight and are less prone to challenge.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_council_presidency, agenda_setter,
    institutional, immediate, constrained, continental).

% Benefit from more stable and legitimate EU policies, which are less likely to be undermined by member state non-compliance. They indirectly bear the costs of slower decision-making but gain from perceived fairness and broader buy-in.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_citizens, beneficiary,
    organized, biographical, mobile, continental).

% An abstract good that is strengthened by the unanimity requirement. Policies adopted with full consent are seen as more legitimate and robust, enhancing the overall authority and acceptance of EU governance.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_policy_legitimacy, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(eu_council_unanimity__diplomatic_capital_reading, eu_policy_legitimacy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that all EU member states' core interests are genuinely considered and integrated into policy, fostering collective ownership and reducing the likelihood of future non-compliance or defection from common policies.
% TRANSFER_FUNCTION: Transfers diplomatic capital and negotiation time from individual member states and the Council Presidency into increased policy legitimacy, durability, and collective buy-in across the EU.
% ABSENT_VOICES: While all member states are present, the voices of smaller states might be less influential in shaping the final consensus, even if their formal veto power is equal. Their 'absence' is in the disproportionate burden of negotiation rather than outright exclusion.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished overnight, decision-making would accelerate, but policies would likely face increased resistance, non-compliance, and challenges to their legitimacy from states whose interests were overridden by qualified majority voting. The political fabric of the EU would fundamentally shift.
% FOUNDING_PROBLEM: To ensure that decisions made by the European Council, particularly on sensitive issues, have the full political backing of all member states, preventing any single state from feeling coerced or marginalized.
% FOUNDING_PROBLEM_CORROBORATION: Diplomats and political scientists widely corroborate that the problem of ensuring member state buy-in and policy legitimacy remains central to EU governance, especially on foreign policy and security matters. The need for strong, unified external representation reinforces this.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the 'cost' is primarily negotiation time and diplomatic effort, which is offset by the benefit of enhanced policy legitimacy and durability. Suppression is low (0.15) as the constraint's persistence relies on the perceived value of consensus, not coercion. Theater ratio is negligible (0.05) because the negotiation process is genuinely aimed at achieving consensus, not merely performing it. The metrics reflect the view that the unanimity rule, while cumbersome, serves a vital coordination function.
 *
 * PERSPECTIVAL GAP:
 *   Other readings (e.g., the 'veto trap' reading) would emphasize the high costs and potential for minoritarian extraction, leading to a higher extractiveness score and a different classification. This 'diplomatic capital' reading focuses on the long-term benefits of consensus, framing negotiation costs as an investment rather than pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   EU member states are beneficiaries, as they gain from policy legitimacy and reduced defection, even if they bear negotiation costs. The Council Presidency, while an agenda-setter, also benefits from the enhanced authority of unanimous decisions. EU citizens are indirect beneficiaries of stable policy. There are no direct 'victims' in this reading, as the costs are seen as necessary for the collective good.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_benefit_balance,
    'Does the long-term legitimacy and durability gained from unanimity genuinely outweigh the short-term costs of negotiation and potential policy delays?',
    'Empirical studies comparing policy implementation rates, compliance levels, and public approval for unanimous vs. QMV decisions over time, controlling for policy area and political context.',
    'If costs consistently outweigh benefits, this reading''s low extractiveness would be challenged, potentially shifting the classification towards a Tangled Rope or even Snare, depending on who disproportionately bears the costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_balance, empirical, 'Assessing the true net benefit of unanimity in practice.').

omega_variable(
    alternative_framing_impact,
    'How would the classification of this constraint change if viewed through the ''veto_trap_reading'' or ''sovereignty_guarantor_reading''?',
    'Generate separate constraint stories for each sibling reading, with their own metrics and classifications, and compare the results.',
    'The ''veto_trap_reading'' would likely yield a higher extractiveness and suppression, potentially classifying as a Snare or Tangled Rope. The ''sovereignty_guarantor_reading'' might also be a Rope or Mountain, but with a different emphasis on the nature of the coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_framing_impact, conceptual, 'Impact of alternative interpretations of the unanimity rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(eu_c_tr_t5, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(eu_c_be_t5, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(eu_c_su_t5, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(eu_c_su_t15, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eu_council_unanimity' kernel. Other readings include 'sovereignty_guarantor_reading' and 'veto_trap_reading', which offer alternative structural interpretations of the same rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
