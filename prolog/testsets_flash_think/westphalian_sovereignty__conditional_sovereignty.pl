% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty Doctrine (R2P)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'conditional sovereignty' reading of the
 *   Westphalian sovereignty kernel, asserting that state sovereignty is not
 *   absolute but entails a responsibility to protect populations from mass
 *   atrocities. Failure to uphold this responsibility can legitimately
 *   trigger external intervention. This reading emerged in response to
 *   humanitarian crises where strict non-interference led to inaction, and it
 *   directly challenges traditional notions of absolute state autonomy. The
 *   constraint operates as a snare, extracting autonomy from states that
 *   violate human rights, enforced by the threat or actuality of
 *   international intervention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.4).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.65).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty Doctrine (R2P)").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '24b09582-1f72-4bb8-8ca4-48df08f4a569').
narrative_ontology:cs_kernel_codification('24b09582-1f72-4bb8-8ca4-48df08f4a569', formalized).
narrative_ontology:cs_authority_grounding('24b09582-1f72-4bb8-8ca4-48df08f4a569', lineage).
narrative_ontology:cs_interpretation_layer_present('24b09582-1f72-4bb8-8ca4-48df08f4a569').
narrative_ontology:cs_reading_relation('24b09582-1f72-4bb8-8ca4-48df08f4a569', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('24b09582-1f72-4bb8-8ca4-48df08f4a569', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('24b09582-1f72-4bb8-8ca4-48df08f4a569', foundational, sovereignty_is_conditional_on_human_rights_protection).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional_on_human_rights_protection, holdable).
narrative_ontology:cs_axiom_grounding('24b09582-1f72-4bb8-8ca4-48df08f4a569', sovereignty_is_conditional_on_human_rights_protection, deontological).
narrative_ontology:cs_axiom('24b09582-1f72-4bb8-8ca4-48df08f4a569', foundational, international_community_has_right_to_intervene).
narrative_ontology:cs_axiom_status(international_community_has_right_to_intervene, holdable).
narrative_ontology:cs_axiom_grounding('24b09582-1f72-4bb8-8ca4-48df08f4a569', international_community_has_right_to_intervene, conventional).
narrative_ontology:cs_reference_frame('24b09582-1f72-4bb8-8ca4-48df08f4a569', post_atrocity_intervention_framework).
narrative_ontology:cs_drift_state('24b09582-1f72-4bb8-8ca4-48df08f4a569', contemporary_geopolitical_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('24b09582-1f72-4bb8-8ca4-48df08f4a569', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, human_rights_organizations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, sovereign_states_violating_human_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, victimized_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and lobby for the application of the conditional sovereignty doctrine, advocating for intervention when human rights violations meet the established thresholds. They benefit from the doctrine providing a legitimate basis for action.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from the doctrine as it provides a legal and moral framework to challenge state impunity and advocate for the protection of populations. They provide evidence of violations and push for international action.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, human_rights_organizations, beneficiary,
    organized, generational, constrained, global).

% Are the ultimate beneficiaries of successful intervention, as it aims to protect them from systematic human rights abuses by their own state. However, they are trapped by their circumstances and cannot directly influence the constraint's operation.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, victimized_populations, beneficiary,
    powerless, immediate, trapped, local).

% Bear the direct cost of this constraint, as their traditional right to non-interference is curtailed, and they face potential external intervention. Their autonomy is extracted when they fail to protect their own populations.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, sovereign_states_violating_human_rights, payer,
    institutional, biographical, constrained, national).

% Is the primary institutional body responsible for authorizing legitimate external intervention under international law. Its decisions are critical to the operationalization of conditional sovereignty, though its actions are often constrained by geopolitical interests.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Are states that generally uphold the principle of conditional sovereignty but may choose not to participate in interventions due to national interests, capacity limitations, or political considerations. They observe and influence the norm's evolution.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_intervening_states, observer,
    institutional, biographical, mobile, global).

% Are states or political factions that argue for an unconditional interpretation of state sovereignty, viewing any external intervention as illegitimate. Their perspective is marginalized by the conditional sovereignty doctrine, and they are excluded from the decision-making process regarding intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, absolute_sovereignty_proponents, excluded,
    organized, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, un_security_council).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a shared international understanding and legitimate framework for when state sovereignty can be overridden to protect populations from mass atrocities, thereby preventing arbitrary intervention while legitimizing necessary collective action.
% TRANSFER_FUNCTION: Transfers a portion of state autonomy and the principle of non-interference from states engaged in systematic human rights violations to the international community, enabling the latter to intervene.
% ABSENT_VOICES: States and political actors adhering to an absolute interpretation of Westphalian sovereignty are structurally excluded from the discourse that legitimizes intervention. They would object to any external interference, regardless of domestic conduct, but their views are not central to the operationalization of this doctrine.
% DISAPPEARANCE_RATIONALE: If the conditional sovereignty doctrine vanished overnight, the international community would lose a crucial legal and moral framework for responding to mass atrocities. This would likely lead to either more unchecked human rights violations within states or a return to arbitrary, less legitimate interventions, fundamentally reorganizing global governance around state-centric power rather than human protection.
% FOUNDING_PROBLEM: The failure of the international community to prevent or stop genocides and mass atrocities in the late 20th century (e.g., Rwanda, Srebrenica) due to strict interpretations of state sovereignty and the principle of non-interference.
% FOUNDING_PROBLEM_CORROBORATION: Numerous UN reports, human rights organizations, international legal scholars, and commissions of inquiry (all outside of states directly benefiting from non-intervention) corroborate the ongoing existence of mass atrocities and the need for a framework to address them, supporting the live status of the founding problem.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.40) because it directly curtails the traditional autonomy of states, but only under specific, high-threshold conditions. Suppression is moderate-high (0.65) as the doctrine relies on the coercive power of the international community (e.g., UN Security Council resolutions, military intervention) to enforce its principles against resistant states. Resistance is high (0.70) because states fiercely defend their sovereignty. Theater ratio is low (0.10) as interventions are high-stakes, costly, and rarely undertaken for purely performative reasons; the enforcement is real, even if selectively applied. Accessibility collapse is moderate (0.50) as states can attempt to deny violations, resist intervention, or seek diplomatic alternatives, but their options are significantly constrained once the threshold for intervention is met.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states accused of human rights violations, this doctrine is a direct infringement on their sovereign rights and an extractive mechanism. From the perspective of international intervention advocates and human rights organizations, it is a necessary coordination mechanism to protect vulnerable populations and uphold universal human rights norms. The engine's per-seat classification will reflect this divergence, with violating states experiencing it as a snare and advocates as a rope or scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   International intervention advocates and human rights organizations are beneficiaries (low d) as the doctrine empowers their mission and provides a legal basis for action. Victimized populations are also beneficiaries, as the constraint aims to protect them. Sovereign states violating human rights are the clear targets (high d), as the constraint extracts their autonomy and imposes costs. The UN Security Council acts as an agenda-setter, wielding the authority to operationalize the constraint, placing it closer to the beneficiary end, though its actions are often constrained by its own internal dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The conditional sovereignty doctrine is not experiencing mandatrophy; its founding problem (preventing mass atrocities) remains acutely live. The challenge lies in its consistent and equitable application, often hampered by geopolitical interests and the veto power of permanent UNSC members, rather than an atrophy of its core function. The classification as a snare reflects the coercive and extractive nature of its enforcement, even if its underlying intent is protective coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_legitimacy_threshold,
    'What constitutes ''systematic human rights violations'' and ''legitimate external intervention'' in practice, and how consistently are these thresholds applied?',
    'Analysis of UN Security Council resolutions, international legal precedents, and independent human rights reports across multiple cases to identify patterns of application and any biases.',
    'If thresholds are inconsistently applied or subject to geopolitical bias, the constraint''s effective extractiveness and suppression become more arbitrary, potentially shifting its classification closer to a pure snare for targeted states, regardless of their actual conduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_legitimacy_threshold, conceptual, 'Ambiguity in the criteria for triggering intervention.').

omega_variable(
    effectiveness_of_intervention,
    'Do external interventions, undertaken under the conditional sovereignty doctrine, consistently lead to improved human rights outcomes and long-term stability for the affected populations?',
    'Empirical studies comparing pre- and post-intervention human rights indicators, state stability, and civilian protection metrics, controlling for confounding factors.',
    'If interventions frequently fail to improve outcomes or exacerbate instability, the justification for the constraint weakens, potentially increasing its perceived extractiveness and reducing its coordination function, leading to a re-evaluation of its overall utility and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_intervention, empirical, 'Empirical efficacy of interventions in achieving stated goals.').

omega_variable(
    un_security_council_veto_power_impact,
    'To what extent does the veto power of permanent members of the UN Security Council undermine the consistent application and legitimacy of the conditional sovereignty doctrine?',
    'Case studies of blocked interventions, analysis of voting patterns, and diplomatic records to quantify the impact of vetoes on the doctrine''s operationalization.',
    'If veto power consistently prevents intervention in cases meeting the doctrine''s thresholds, it reveals a structural flaw that limits the constraint''s effective scope and enforcement, potentially leading to a higher theater ratio and reduced perceived legitimacy for the doctrine as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(un_security_council_veto_power_impact, empirical, 'Impact of UNSC vetoes on doctrine''s application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0, 0.1).
narrative_ontology:measurement(west_tr_t5, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 5, 0.1).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 10, 0.1).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 15, 0.1).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 20, 0.1).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(west_be_t5, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 25, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(west_su_t5, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalian_sovereignty' kernel. It focuses on the conditional nature of sovereignty, contrasting with 'absolute_sovereignty' and 'graduated_sovereignty' readings. Each reading has distinct ε values and structural implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
