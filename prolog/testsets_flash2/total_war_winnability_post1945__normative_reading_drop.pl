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
 *   This constraint describes the normative illegitimacy of total war, as
 *   established by Article 2(4) of the UN Charter and the development of
 *   international humanitarian law post-1945. It is a 'normative reading' of
 *   the broader kernel 'total_war_winnability_post1945', focusing on the
 *   ideational shift that made total war unacceptable, even if physically
 *   possible. This reading posits a Rope-class constraint, solving a
 *   coordination problem (preventing global catastrophe) through shared legal
 *   and moral commitments, with global civilian populations as primary
 *   beneficiaries and revisionist powers as victims.
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
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Illegitimacy of Total War (Post-1945)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, 'd51cebec-3045-4e62-ade0-d22f24efbac7').
narrative_ontology:cs_kernel_codification('d51cebec-3045-4e62-ade0-d22f24efbac7', formalized).
narrative_ontology:cs_authority_grounding('d51cebec-3045-4e62-ade0-d22f24efbac7', lineage).
narrative_ontology:cs_interpretation_layer_present('d51cebec-3045-4e62-ade0-d22f24efbac7').
narrative_ontology:cs_reading_relation('d51cebec-3045-4e62-ade0-d22f24efbac7', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d51cebec-3045-4e62-ade0-d22f24efbac7', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('d51cebec-3045-4e62-ade0-d22f24efbac7', foundational, jus_ad_bellum_prohibits_total_war).
narrative_ontology:cs_axiom_status(jus_ad_bellum_prohibits_total_war, holdable).
narrative_ontology:cs_axiom_grounding('d51cebec-3045-4e62-ade0-d22f24efbac7', jus_ad_bellum_prohibits_total_war, deontological).
narrative_ontology:cs_axiom('d51cebec-3045-4e62-ade0-d22f24efbac7', foundational, humanitarian_law_limits_means_of_warfare).
narrative_ontology:cs_axiom_status(humanitarian_law_limits_means_of_warfare, holdable).
narrative_ontology:cs_axiom_grounding('d51cebec-3045-4e62-ade0-d22f24efbac7', humanitarian_law_limits_means_of_warfare, conventional).
narrative_ontology:cs_reference_frame('d51cebec-3045-4e62-ade0-d22f24efbac7', post_un_charter_legal_order).
narrative_ontology:cs_drift_state('d51cebec-3045-4e62-ade0-d22f24efbac7', contemporary_geopolitical_challenges, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('d51cebec-3045-4e62-ade0-d22f24efbac7', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, international_legal_order).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, status_quo_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the normative barrier against total war, which reduces the likelihood of widespread destruction and atrocities. They are the primary intended beneficiaries of humanitarian law and Article 2(4).
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    organized, generational, trapped, global).

% Embodies and enforces the normative constraint through treaties, customary law, and international institutions. Its legitimacy and function depend on the adherence to these norms, even if imperfectly observed.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_order, agenda_setter,
    institutional, civilizational, constrained, global).

% Are constrained by the normative illegitimacy of total war, limiting their strategic options and increasing the reputational and legal costs of aggressive action. They bear the cost of foregone strategic flexibility.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, national).

% Benefit from the stability and predictability that the normative constraint provides, reinforcing their existing positions and reducing the threat of existential conflict. They are aligned with the international legal order.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, status_quo_powers, beneficiary,
    institutional, generational, mobile, global).

% Monitor compliance with international humanitarian law, document violations, and advocate for stronger adherence to the norms against total war. They provide critical corroboration for the constraint's effectiveness and failures.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, humanitarian_organizations, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior by establishing a shared understanding of unacceptable warfare, thereby reducing the risk of escalation to total war and protecting civilian populations. It provides a common framework for legitimate use of force.
% TRANSFER_FUNCTION: Transfers strategic flexibility and the option of total victory from states (especially revisionist ones) to the global civilian population and the international legal order, in exchange for reduced existential risk.
% ABSENT_VOICES: Historical proponents of total war as a legitimate strategic option are absent from contemporary discourse, having been largely delegitimized by post-WWII normative shifts. Their arguments for unrestricted warfare are no longer considered valid within the mainstream international relations framework.
% DISAPPEARANCE_RATIONALE: If the normative illegitimacy of total war vanished, states would quickly re-evaluate their strategic doctrines, potentially leading to a return to more aggressive and unrestricted forms of conflict. The international legal order would lose a foundational pillar, and global security would significantly degrade.
% FOUNDING_PROBLEM: The catastrophic human cost and existential threat posed by two World Wars, particularly the indiscriminate targeting of civilians and the use of weapons of mass destruction, necessitated a new normative framework for warfare.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, humanitarian organizations, and the vast majority of UN member states consistently affirm the ongoing relevance and necessity of these norms, citing persistent threats of conflict and the need to prevent a return to the horrors of total war. This is corroborated by the continuous efforts in international diplomacy and law-making.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.25) is relatively low, representing the 'cost' of adherence for states that might otherwise pursue total victory, but it is offset by the collective benefit of avoiding global conflict. Suppression (0.4) reflects the diplomatic, legal, and reputational pressures on states to conform to these norms. Theater ratio (0.1) is low, as the commitment to these norms is largely genuine, though violations do occur. Accessibility collapse (0.7) is high because the normative framework significantly limits the perceived 'legitimate' options for warfare. Resistance (0.3) is present from states that challenge or violate these norms, but it is not widespread enough to dismantle the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of global civilian populations and status quo powers, this is a vital Rope that prevents catastrophe. From the perspective of revisionist powers, it is a constraint on their sovereignty and strategic freedom, potentially perceived as more extractive. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations are clear beneficiaries (d=0.0) as the norms protect them. The international legal order and status quo powers also benefit (d low) from the stability. Revisionist powers are targets (d high) as their strategic options are curtailed. Humanitarian organizations act as observers (d=0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_material_causation,
    'To what extent is the absence of total war due to normative illegitimacy (this reading) versus material constraints like nuclear deterrence (structural_contraction_reading)?',
    'Counterfactual historical analysis, comparative case studies of states with and without nuclear weapons, and analysis of state rhetoric and doctrine in non-nuclear contexts.',
    'If material constraints are dominant, this normative constraint''s effective extractiveness and suppression might be lower, as states are already deterred by other factors. If normative factors are primary, this constraint''s role is more significant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_vs_material_causation, conceptual, 'Distinguishing the causal weight of normative vs. material factors in preventing total war.').

omega_variable(
    norm_adherence_vs_strategic_culture,
    'Is the normative illegitimacy primarily driven by formal legal commitments (this reading) or by deeper, informal shifts in strategic culture and elite discourse (strategic_culture_drift)?',
    'Content analysis of strategic documents, elite interviews, and historical analysis of how legal norms were internalized (or not) into national strategic cultures.',
    'If strategic culture is the primary driver, the ''enforcement'' of this constraint might be more internalized and less reliant on external legal pressure, potentially lowering the effective suppression metric for some actors. If legal commitments are primary, the constraint is more robust against cultural shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_adherence_vs_strategic_culture, empirical, 'Assessing the relative influence of formal legal norms versus informal strategic culture.').

omega_variable(
    norm_erosion_risk,
    'Is the normative illegitimacy of total war eroding due to persistent violations, the rise of new great power competition, or the perceived ineffectiveness of international institutions?',
    'Longitudinal analysis of state behavior, rhetoric, and adherence to international humanitarian law, particularly in ongoing conflicts and crises.',
    'If erosion is substantial, the constraint''s effective extractiveness might decrease (as states feel less bound), but the risk of global conflict would increase. The classification might drift towards a Piton if the norms are maintained only theatrically, or a Snare if they are actively subverted for gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_erosion_risk, empirical, 'Evaluating the stability and potential erosion of the normative constraint over time.').


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
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This reading focuses on the normative illegitimacy established by international law, distinct from structural (nuclear) or cultural (strategic culture) explanations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
