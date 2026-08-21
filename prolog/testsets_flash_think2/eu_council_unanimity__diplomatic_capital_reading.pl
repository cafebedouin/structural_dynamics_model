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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity: Diplomatic Capital Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'diplomatic capital' reading of the EU
 *   Council's unanimity rule, which views the requirement for unanimous
 *   consent on certain policy matters as a crucial mechanism for building
 *   consensus, ensuring broad member state buy-in, and strengthening the
 *   overall legitimacy and durability of EU policy. The costs associated with
 *   lengthy negotiations are interpreted as necessary coordination costs
 *   rather than extraction. This reading contrasts with others that might
 *   emphasize the protection of national sovereignty or the potential for a
 *   'veto trap'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.25).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.3).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity: Diplomatic Capital Reading").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__diplomatic_capital_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '378a5d88-2087-40af-9a57-a9207e7bcd23').
narrative_ontology:cs_kernel_codification('378a5d88-2087-40af-9a57-a9207e7bcd23', formalized).
narrative_ontology:cs_authority_grounding('378a5d88-2087-40af-9a57-a9207e7bcd23', lineage).
narrative_ontology:cs_interpretation_layer_present('378a5d88-2087-40af-9a57-a9207e7bcd23').
narrative_ontology:cs_reading_relation('378a5d88-2087-40af-9a57-a9207e7bcd23', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('378a5d88-2087-40af-9a57-a9207e7bcd23', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('378a5d88-2087-40af-9a57-a9207e7bcd23', foundational, consensus_builds_legitimacy).
narrative_ontology:cs_axiom_status(consensus_builds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('378a5d88-2087-40af-9a57-a9207e7bcd23', consensus_builds_legitimacy, conventional).
narrative_ontology:cs_axiom('378a5d88-2087-40af-9a57-a9207e7bcd23', foundational, policy_durability_requires_buy_in).
narrative_ontology:cs_axiom_status(policy_durability_requires_buy_in, holdable).
narrative_ontology:cs_axiom_grounding('378a5d88-2087-40af-9a57-a9207e7bcd23', policy_durability_requires_buy_in, empirically_contingent).
narrative_ontology:cs_reference_frame('378a5d88-2087-40af-9a57-a9207e7bcd23', post_maastricht_integration).
narrative_ontology:cs_drift_state('378a5d88-2087-40af-9a57-a9207e7bcd23', contemporary_eu_challenges, gap(stable, minor, false)).
narrative_ontology:cs_created_at('378a5d88-2087-40af-9a57-a9207e7bcd23', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As sovereign entities, they are required to reach unanimous agreement on certain policy areas. They invest diplomatic capital in iterative negotiations, which this reading frames as a necessary cost for achieving legitimate and durable collective policy outcomes. Exiting the EU is a high-cost, constrained option.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, beneficiary).

% Benefit from the enhanced legitimacy and stability of policies adopted through unanimity, which reduces the risk of member state non-compliance or defection. They facilitate the negotiation process.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_institutions, beneficiary,
    institutional, generational, constrained, continental).

% Indirectly benefit from policies that are perceived as more legitimate and stable due to broad member state buy-in, leading to more effective governance. Their exit options are tied to their national membership.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_citizens, beneficiary,
    organized, biographical, constrained, national).

% Study the effects of the unanimity rule on EU decision-making, policy legitimacy, and integration dynamics. They provide independent analysis of its costs and benefits.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, political_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that collective decisions, particularly on sensitive matters, are acceptable to all member states, thereby fostering broad buy-in, reducing the likelihood of non-compliance, and strengthening the overall legitimacy and durability of EU policy.
% TRANSFER_FUNCTION: Requires member states to invest significant diplomatic capital, time, and flexibility in iterative negotiation processes, transferring this effort into enhanced policy legitimacy and collective ownership.
% ABSENT_VOICES: This reading posits that the unanimity rule is designed to ensure all relevant voices (member states) are heard and accommodated, minimizing the existence of truly 'absent' voices within the decision-making body itself. However, external stakeholders not represented by member states might still be absent.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished overnight, decision-making in the EU Council might become faster, but policies would likely face increased resistance, non-compliance, and challenges to their legitimacy from dissenting states. This would fundamentally alter the political dynamics of the Union, potentially leading to fragmentation or a shift towards a more federal structure.
% FOUNDING_PROBLEM: To build a union of diverse sovereign states where collective decisions, especially those impacting national interests, are perceived as legitimate and binding by all members, thereby preventing a 'tyranny of the majority' and ensuring the long-term stability of the integration project.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists specializing in international organizations and EU integration, as well as many national diplomats and legal scholars, corroborate that the challenge of ensuring legitimacy, buy-in, and preventing majoritarian overreach in a diverse union remains a central and live problem for the EU.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.25) and suppression (0.30) reflect this reading's interpretation that the unanimity rule, while demanding, ultimately serves a genuine coordination function by forcing compromise and ensuring all parties feel their interests are adequately addressed. The costs are seen as investments in legitimacy and stability, not as rents extracted by one party from another. The theater ratio is low (0.10) because the negotiation process is considered genuine and functional, not merely performative. Resistance is low because the rule itself is accepted as a foundational principle for achieving consensus.
 *
 * PERSPECTIVAL GAP:
 *   While this reading emphasizes the collective benefits, other readings (e.g., the 'veto trap' reading) would highlight the costs and potential for minoritarian extraction. The engine's per-seat classification would reflect this divergence if those alternative framings were authored as separate constraints, showing how the same structural rule can be experienced differently depending on the interpretive lens.
 *
 * DIRECTIONALITY LOGIC:
 *   All EU member states are considered beneficiaries in this reading, as they gain from the enhanced legitimacy and durability of policies. EU institutions also benefit from a stable policy environment. There are no direct 'victims' in this framing, as the negotiation costs are viewed as shared investments in collective outcomes. The directionality for all involved parties is therefore skewed towards the beneficiary end.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negotiation_cost_vs_extraction,
    'Is the diplomatic capital invested in achieving unanimity always a legitimate coordination cost, or can it become a form of extraction when used by a minority to secure disproportionate concessions?',
    'Empirical analysis of specific negotiation outcomes: if concessions consistently exceed the proportional interest of the blocking party, it suggests extraction. This would require detailed case studies and economic modeling.',
    'If found to be extractive, the constraint''s effective extractiveness would be higher than currently assessed, potentially shifting its classification towards a Tangled Rope or Snare from the perspective of the majority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_cost_vs_extraction, empirical, 'Distinguishing legitimate coordination costs from minoritarian extraction within unanimity.').

omega_variable(
    legitimacy_vs_efficiency_tradeoff,
    'Does the enhanced legitimacy gained through unanimity always outweigh the potential costs of inefficiency, delayed decision-making, or lowest-common-denominator policies?',
    'Comparative studies with alternative decision-making rules (e.g., Qualified Majority Voting) across different policy domains, assessing both policy effectiveness and public perception of legitimacy over time.',
    'If the efficiency costs consistently outweigh legitimacy gains, the overall benefit of the constraint would be questioned, potentially leading to a re-evaluation of its ''Rope'' classification towards a more problematic type, or a shift in policy preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_efficiency_tradeoff, conceptual, 'Assessing the balance between legitimacy benefits and efficiency costs of unanimity.').

omega_variable(
    reading_divergence_from_veto_trap,
    'How does this ''diplomatic capital'' reading structurally differ from the ''veto trap'' reading of the same unanimity kernel, and what empirical signals would distinguish them?',
    'The ''veto trap'' reading would emphasize high extractiveness and suppression, with identifiable victims (the majority) and beneficiaries (the blocking minority). Empirical signals would include frequent blocking, disproportionate concessions, and policy paralysis. This reading, by contrast, emphasizes low extraction and diffuse benefits.',
    'If empirical evidence consistently aligns with the ''veto trap'' signals, this ''diplomatic capital'' reading would be challenged as an idealized or incomplete account, requiring a re-evaluation of its core metrics and claimed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_from_veto_trap, conceptual, 'Structural and empirical divergence from the ''veto trap'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eu_c_tr_t6, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(eu_c_tr_t18, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(eu_c_be_t6, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 12, 0.23).
narrative_ontology:measurement(eu_c_be_t18, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 18, 0.24).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(eu_c_su_t6, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 6, 0.29).
narrative_ontology:measurement(eu_c_su_t12, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(eu_c_su_t18, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 18, 0.3).
narrative_ontology:measurement(eu_c_su_t24, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 24, 0.3).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_common_foreign_security_policy).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_budget_negotiations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eu_council_unanimity' kernel, focusing on its role in building policy legitimacy through negotiation. It is linked to the 'sovereignty_guarantor_reading' and 'veto_trap_reading' which offer alternative interpretations of the same rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
