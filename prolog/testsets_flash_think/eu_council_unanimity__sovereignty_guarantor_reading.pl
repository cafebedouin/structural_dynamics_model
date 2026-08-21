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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity (Sovereignty Guarantor Reading)
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty guarantor' reading of
 *   EU Council unanimity, where the requirement for each state's consent in
 *   certain collective actions is seen as a foundational protection against
 *   majoritarian coercion. From this perspective, the veto power is a
 *   legitimate exercise of sovereignty, particularly beneficial for smaller
 *   member states, and the associated costs are acceptable coordination
 *   overhead, not systematic extraction. The constraint is claimed as a Rope,
 *   reflecting its primary function as a coordination mechanism that benefits
 *   all participants by safeguarding their autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.35).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.2).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity (Sovereignty Guarantor Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '589e5f05-fbdb-4ff8-ad55-fdbb4b798677').
narrative_ontology:cs_kernel_codification('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', formalized).
narrative_ontology:cs_authority_grounding('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', lineage).
narrative_ontology:cs_interpretation_layer_present('589e5f05-fbdb-4ff8-ad55-fdbb4b798677').
narrative_ontology:cs_reading_relation('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', foundational, national_sovereignty_is_indivisible).
narrative_ontology:cs_axiom_status(national_sovereignty_is_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', national_sovereignty_is_indivisible, deontological).
narrative_ontology:cs_axiom('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', foundational, consent_is_prerequisite_for_legitimate_collective_action).
narrative_ontology:cs_axiom_status(consent_is_prerequisite_for_legitimate_collective_action, holdable).
narrative_ontology:cs_axiom_grounding('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', consent_is_prerequisite_for_legitimate_collective_action, deontological).
narrative_ontology:cs_reference_frame('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', westphalian_sovereignty_principle).
narrative_ontology:cs_drift_state('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', contemporary_eu_integration_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('589e5f05-fbdb-4ff8-ad55-fdbb4b798677', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, all_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the assurance that their vital national interests cannot be overridden by a simple majority, fostering trust and continued participation in the EU project. They collectively bear the coordination costs of slower decision-making.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, all_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Are the primary beneficiaries of the unanimity rule, as it provides a disproportionate safeguard against the numerical dominance of larger states, ensuring their voice and sovereignty are respected in sensitive policy areas.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    organized, biographical, constrained, national).

% Bear the cost of slower and more difficult decision-making, as their preferred policies can be blocked by a single smaller state. From this reading, they accept this as a necessary cost for the stability and legitimacy of the Union.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer,
    powerful, biographical, constrained, national).

% Proposes legislation and manages the EU's executive functions. It must navigate the unanimity requirement in certain policy areas, which can slow down or alter its legislative agenda, but also ensures broader buy-in when consensus is achieved.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_commission, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from the stability and perceived legitimacy of decisions made with full national consent, which can prevent deep political crises. They indirectly bear the costs of delayed or compromised policy outcomes due to the unanimity requirement.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, payer).

% Study the institutional dynamics of the EU, analyzing the effects of unanimity on integration, sovereignty, and democratic legitimacy. They provide external analysis without direct participation in the constraint's operation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that collective action in sensitive areas has the full consent of all sovereign states, preventing majoritarian imposition and fostering trust in the integration process, particularly for smaller member states.
% TRANSFER_FUNCTION: Transfers decision-making power from a simple or qualified majority to each individual member state, effectively granting each state a veto over certain collective actions, thereby safeguarding national sovereignty.
% ABSENT_VOICES: From this reading's perspective, there are no absent voices, as the unanimity rule ensures every member state has a voice and the power to prevent collective action that implicates its sovereignty.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight, the EU's institutional balance would fundamentally shift. Decision-making in sensitive areas would become faster but potentially less consensual, leading to a perceived erosion of national sovereignty, especially for smaller states, and potentially triggering deep political crises or even exits from the Union.
% FOUNDING_PROBLEM: To protect the sovereignty of member states, particularly smaller ones, from being overridden by larger states in areas deemed vital to national interest, ensuring their voluntary and legitimate participation in the European integration project.
% FOUNDING_PROBLEM_CORROBORATION: Member states, particularly smaller ones, consistently invoke sovereignty concerns in debates over treaty changes or policy areas requiring unanimity. Legal scholars and political scientists outside the direct beneficiary group corroborate the historical intent and ongoing function of unanimity as a safeguard against majoritarian coercion.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness is set at a moderate 0.35, acknowledging that unanimity imposes real coordination costs (slower decision-making, need for extensive negotiation) but, from this reading, these costs are symmetric and necessary for the perceived benefit of sovereignty protection, not asymmetric extraction. Suppression is low (0.20) because the rule empowers, rather than suppresses, individual states. Accessibility collapse is higher (0.70) because the rule effectively collapses the alternative of majoritarian decision-making in sensitive areas, which is seen as a feature, not a bug, from this perspective. Theater ratio is low (0.10) as the mechanism is considered genuinely functional. The temporal measurements show a slight increase in extractiveness and suppression, reflecting the growing complexity and political friction within the EU over time, even when the core function is seen as stable.
 *
 * PERSPECTIVAL GAP:
 *   The 'sovereignty guarantor' reading emphasizes the protective function of unanimity, leading to a classification as a Rope. However, other readings (e.g., 'veto trap') would highlight the costs and potential for minoritarian extraction, leading to a different classification. The engine computes these divergences from the structural data, showing how the same mechanism can be experienced differently depending on an agent's position and interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   All member states, and especially small member states, are structural beneficiaries (d near 0.0) as the rule protects their sovereignty. Large member states are payers (d near 1.0) as they bear the costs of slower decision-making and potential blocking of their initiatives, though they also benefit from the overall stability of the Union. The EU Commission, as agenda-setter, navigates these dynamics. EU citizens are diffuse beneficiaries of stability but also indirect payers of coordination costs. No identifiable victims exist from this reading's perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by focusing on the foundational problem of sovereignty protection that unanimity was designed to solve. By acknowledging the ongoing 'live' status of this problem and the corroboration from external sources, it argues against the idea that the constraint's mandate has atrophied. The moderate extractiveness is attributed to inherent coordination costs, not a decay into pure rent-seeking, thus distinguishing it from a Piton or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unanimity_vs_efficiency_tradeoff,
    'Is the protection of national sovereignty afforded by unanimity genuinely worth the cost of slower decision-making and potential policy paralysis for the EU as a whole?',
    'Comparative analysis of policy outcomes and public satisfaction in areas governed by unanimity versus qualified majority voting, alongside surveys of national political elites regarding their perceived trade-offs.',
    'If the costs are widely perceived to outweigh the benefits, the constraint''s effective extractiveness would be higher, and its coordination function might be re-evaluated as less effective, potentially shifting its classification towards a Tangled Rope or even Snare if the costs are borne disproportionately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_vs_efficiency_tradeoff, preference, 'The normative trade-off between sovereignty protection and EU decision-making efficiency.').

omega_variable(
    veto_abuse_potential,
    'Is the veto power primarily used for legitimate defense of national sovereignty, or is it frequently leveraged for minoritarian extraction or unrelated national interests?',
    'Empirical study of veto instances: analysis of the stated reasons for vetoes, the policy areas involved, and the subsequent concessions or outcomes, distinguishing between genuine sovereignty concerns and strategic bargaining.',
    'If vetoes are frequently used for extraction, the constraint''s effective extractiveness would be significantly higher, and its classification would shift towards a Tangled Rope or Snare, as the coordination story would be revealed as cover for asymmetric gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_abuse_potential, empirical, 'Distinguishing legitimate sovereignty defense from strategic veto use for extraction.').

omega_variable(
    true_sovereignty_implication,
    'Does the unanimity rule truly protect national sovereignty, or does it merely shift the locus of power and negotiation, with sovereignty still being constrained by the need for consensus?',
    'Conceptual analysis of sovereignty in a multi-level governance system, examining whether the ''right to block'' constitutes full sovereignty or a modified form of autonomy within a shared framework.',
    'If unanimity is found to offer only a superficial protection of sovereignty, the perceived benefit for member states would decrease, potentially raising the effective extractiveness by highlighting the costs without the full promised benefit, pushing it towards a Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_sovereignty_implication, conceptual, 'The conceptual nature of sovereignty protection within the EU framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t2000, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(eu_c_tr_t2005, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2005, 0.06).
narrative_ontology:measurement(eu_c_tr_t2010, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2010, 0.07).
narrative_ontology:measurement(eu_c_tr_t2015, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(eu_c_tr_t2020, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2020, 0.09).
narrative_ontology:measurement(eu_c_tr_t2025, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(eu_c_tr_t2030, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t2000, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(eu_c_be_t2005, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(eu_c_be_t2010, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(eu_c_be_t2015, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2015, 0.32).
narrative_ontology:measurement(eu_c_be_t2020, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2020, 0.34).
narrative_ontology:measurement(eu_c_be_t2025, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2025, 0.35).
narrative_ontology:measurement(eu_c_be_t2030, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2030, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t2000, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(eu_c_su_t2005, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2005, 0.16).
narrative_ontology:measurement(eu_c_su_t2010, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2010, 0.17).
narrative_ontology:measurement(eu_c_su_t2015, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2015, 0.18).
narrative_ontology:measurement(eu_c_su_t2020, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2020, 0.19).
narrative_ontology:measurement(eu_c_su_t2025, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2025, 0.2).
narrative_ontology:measurement(eu_c_su_t2030, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2030, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eu_council_unanimity' kernel. This 'sovereignty_guarantor_reading' emphasizes the protective function of unanimity for national sovereignty, particularly for smaller states. It contrasts with the 'veto_trap_reading' (which focuses on minoritarian extraction) and the 'diplomatic_capital_reading' (which highlights consensus-building and legitimacy). Each reading yields a distinct structural constraint with different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
