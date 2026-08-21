% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Normative Illegitimacy of Total War (Post-1945)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the normative illegitimacy of total war, a
 *   reading that posits that while physically possible, total war became
 *   unacceptable through the development of international law (UN Charter
 *   Article 2(4)) and humanitarian norms post-1945. It is a Rope-class
 *   constraint, solving a coordination problem by establishing shared rules
 *   of engagement and non-aggression, benefiting global civilian populations
 *   and constraining revisionist powers. This is one reading of the
 *   'total_war_winnability_post1945' kernel, focusing on the ideational and
 *   legal shift.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.25).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.4).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.25).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Illegitimacy of Total War (Post-1945)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, 'ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8').
narrative_ontology:cs_kernel_codification('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', formalized).
narrative_ontology:cs_authority_grounding('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', lineage).
narrative_ontology:cs_interpretation_layer_present('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8').
narrative_ontology:cs_reading_relation('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', foundational, war_of_aggression_is_illegal).
narrative_ontology:cs_axiom_status(war_of_aggression_is_illegal, holdable).
narrative_ontology:cs_axiom_grounding('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', war_of_aggression_is_illegal, deontological).
narrative_ontology:cs_axiom('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', foundational, civilian_immunity_is_absolute).
narrative_ontology:cs_axiom_status(civilian_immunity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', civilian_immunity_is_absolute, deontological).
narrative_ontology:cs_reference_frame('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', post_un_charter_legal_order).
narrative_ontology:cs_drift_state('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', contemporary_geopolitical_challenges, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('ef54d2aa-fed5-48be-ae1a-1da4c9bac9a8', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_organizations).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, jus_ad_bellum_doctrine).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, jus_in_bello_doctrine).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, responsibility_to_protect_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the normative constraint against total war, which theoretically protects them from indiscriminate targeting and existential threats. Their 'exit' from the consequences of total war is non-existent, making the constraint a vital protection.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    organized, generational, trapped, global).

% Actively promote and enforce the norms against total war through advocacy, monitoring, and legal frameworks. They are beneficiaries of a more stable international order but also bear the cost of continuous vigilance and intervention.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_organizations, agenda_setter,
    institutional, generational, constrained, global).

% Are constrained by the normative illegitimacy of total war, limiting their strategic options and requiring them to justify military actions within established legal frameworks. They pay the cost of foregone strategic flexibility and international condemnation.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, global).

% The primary institutional body for upholding Article 2(4) of the UN Charter, which prohibits the threat or use of force against the territorial integrity or political independence of any state. Its members, particularly the P5, are both enforcers and subject to the norms.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, united_nations_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the evolution and effectiveness of international law and norms related to armed conflict. They contribute to the intellectual framework that underpins the normative constraint, but do not directly enforce it.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior by establishing a shared understanding of what constitutes legitimate and illegitimate use of force, thereby reducing the likelihood of states escalating conflicts to total war and protecting civilian populations.
% TRANSFER_FUNCTION: Transfers strategic flexibility and the option of total victory from states (especially revisionist powers) to the international community and global civilian populations, in exchange for a more stable and predictable international order.
% ABSENT_VOICES: Historical proponents of total war as a legitimate strategic option, or states that prioritize absolute sovereignty and unconstrained military action, are marginalized in contemporary international discourse. They would argue for the right to pursue national interests without external normative constraints.
% DISAPPEARANCE_RATIONALE: If the normative illegitimacy of total war vanished, states would revert to a more Hobbesian international system, increasing the risk of large-scale conflicts, indiscriminate targeting, and potentially existential threats to civilian populations. The international legal and humanitarian frameworks would collapse, leading to a dramatic reorganization of global security.
% FOUNDING_PROBLEM: The devastating human and material costs of two World Wars, particularly the targeting of civilian populations and the existential threat posed by modern weaponry, necessitated a new international legal and normative framework to prevent future total wars.
% FOUNDING_PROBLEM_CORROBORATION: The UN Charter, the Geneva Conventions, and subsequent humanitarian law developments, along with consistent diplomatic and academic discourse, corroborate the ongoing relevance of preventing total war. While some states challenge specific interpretations, the core problem of preventing catastrophic conflict remains live and widely acknowledged by international bodies and civil society organizations.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily coordinates behavior for collective benefit (peace, civilian protection) rather than extracting resources. Suppression is moderate (0.4) as it relies on international pressure, legal frameworks, and the threat of sanctions, rather than direct physical coercion. Theater ratio is low (0.1) because the commitment to these norms, while sometimes challenged, is largely genuine and functional in preventing overt declarations of total war. Accessibility collapse is moderate (0.6) as the normative framework significantly limits alternatives for states contemplating total war, but does not eliminate them entirely. Resistance is moderate (0.3) from states that occasionally challenge or circumvent these norms, but not to the extent of outright rejection.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of global civilian populations, this is a vital Rope, a necessary coordination mechanism for survival. From the perspective of revisionist powers, it is a constraint on their sovereignty and strategic freedom, potentially perceived as more extractive. The UN Security Council, as an agenda-setter, experiences it as a necessary framework for global governance, but also as a source of internal political friction.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations are the primary beneficiaries (d near 0.0) as the constraint directly protects their lives and livelihoods. International humanitarian organizations and the UN Security Council act as agenda-setters and beneficiaries, upholding the norms. Revisionist powers are the payers/targets (d near 1.0), as they bear the cost of restricted strategic options and international opprobrium for violating norms. International legal scholars are observers, analyzing the constraint's evolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing total war) remains live and highly relevant, as evidenced by ongoing conflicts and the potential for escalation. The classification as a Rope prevents mislabeling it as a Snare, which would imply the coordination story is mere cover for extraction, or a Piton, which would suggest its function has atrophied. The active enforcement and clear benefits to a broad population confirm its ongoing coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_structural_causation,
    'To what extent is the absence of total war due to normative illegitimacy (this reading) versus the structural constraint of nuclear deterrence (structural_contraction_reading)?',
    'Counterfactual analysis of historical crises where nuclear deterrence was present but normative constraints were also invoked, or analysis of conflicts between non-nuclear powers where total war was avoided due to normative pressure.',
    'If structural contraction is the dominant cause, this normative constraint''s effective extractiveness and suppression might be lower, as its ''work'' is already done by physical limits. If normative illegitimacy is dominant, its role as a Rope is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_vs_structural_causation, conceptual, 'Ambiguity in the primary causal mechanism preventing total war.').

omega_variable(
    normative_vs_strategic_culture_drift,
    'Is the normative illegitimacy of total war a distinct legal/ethical constraint, or primarily a reflection of a broader ideational shift in strategic culture (strategic_culture_drift)?',
    'Analysis of state rhetoric and policy documents: if legal arguments and humanitarian principles are explicitly invoked as constraints, it supports this reading. If the shift is primarily in ''acceptable'' strategic thought, it supports the strategic culture reading.',
    'If it''s primarily strategic culture, the constraint might be more fluid and less legally binding than this reading suggests, potentially impacting its long-term stability and enforcement mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_vs_strategic_culture_drift, conceptual, 'Distinguishing between formal normative constraint and informal strategic culture.').

omega_variable(
    enforcement_effectiveness_ambiguity,
    'How effective is the enforcement of these norms against powerful states, particularly those with veto power in the UN Security Council?',
    'Empirical analysis of instances where powerful states have violated or circumvented aspects of these norms, and the subsequent international response and consequences.',
    'If enforcement is consistently weak against powerful actors, the effective suppression and extractiveness for those actors would be lower, potentially reclassifying their seat towards a more ''mobile'' or ''arbitrage'' exit, even if the overall constraint remains a Rope for others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_ambiguity, empirical, 'Uncertainty regarding the practical enforcement of norms against all actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1965, 0.2).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1985, 0.25).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This reading focuses on the normative and legal shift making total war illegitimate. Sibling readings (structural_contraction_reading, strategic_culture_drift) offer alternative explanations for the absence of total war.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
