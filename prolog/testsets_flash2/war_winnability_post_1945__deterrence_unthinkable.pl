% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear War Unwinnability (Deterrence Unthinkable Reading)
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence unthinkable' reading of the
 *   post-1945 nuclear reality: great-power total war is categorically
 *   unwinnable, rendering traditional victory planning incoherent. It is a
 *   structural feature of the international system, not a policy choice, and
 *   thus claimed as a Mountain. The extraction is low, reflecting the diffuse
 *   cost of mission incoherence for military establishments rather than
 *   direct rent collection. Suppression is high because the physical reality
 *   of nuclear weapons makes alternatives (i.e., winnable total war) almost
 *   completely inaccessible. This reading emphasizes the operational
 *   contraction of strategic options.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.15).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.95).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.15).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear War Unwinnability (Deterrence Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'c6b062fc-da00-4c6d-ada0-81e3ec6cef09').
narrative_ontology:cs_kernel_codification('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', implicit).
narrative_ontology:cs_authority_grounding('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', self_enforcing).
narrative_ontology:cs_reading_relation('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', foundational, mutual_assured_destruction_is_absolute).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', mutual_assured_destruction_is_absolute, empirically_contingent).
narrative_ontology:cs_axiom('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', foundational, total_war_is_existential_risk).
narrative_ontology:cs_axiom_status(total_war_is_existential_risk, holdable).
narrative_ontology:cs_axiom_grounding('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', total_war_is_existential_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', pre_nuclear_war_winnability).
narrative_ontology:cs_drift_state('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', post_nuclear_proliferation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c6b062fc-da00-4c6d-ada0-81e3ec6cef09', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, traditional_strategic_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the categorical unwinnability of total war, as it theoretically prevents their annihilation. They are trapped in the system but are the primary beneficiaries of its most extreme constraint.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Face a fundamental incoherence in their mission: how to plan for victory in a war that cannot be won. Their identity is tied to national defense and warfighting, which is undermined by this constraint.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, identity_locked, global).

% Their professional identity and career paths are built around developing war plans and strategies for victory. This constraint renders their core expertise obsolete or pushes it into highly abstract, non-operational domains.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, traditional_strategic_planners, payer,
    organized, biographical, identity_locked, global).

% Are the agents whose actions and arsenals instantiate this constraint. They are forced to manage a system where their ultimate weapons cannot be used for traditional victory, shifting their strategic focus to deterrence and non-proliferation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the implications of nuclear weapons for state behavior, international norms, and the nature of conflict. They interpret the structural changes imposed by this constraint on global politics.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, international_relations_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding among great powers that total war is self-defeating, thereby coordinating their strategic behavior towards mutual restraint and crisis management rather than escalation.
% TRANSFER_FUNCTION: Transfers the concept of 'winnability' out of the domain of great-power total war, shifting resources and intellectual effort from war-winning strategies to war-prevention and deterrence maintenance.
% ABSENT_VOICES: Pre-nuclear military strategists and proponents of conventional war as a viable policy tool are conceptually absent; their frameworks are rendered incoherent by the nuclear reality. They would argue for the continued relevance of traditional victory metrics.
% DISAPPEARANCE_RATIONALE: If the unwinnability of nuclear war vanished, strategic planning would immediately revert to traditional war-winning doctrines, military budgets would reorient, and the risk of great-power conflict would dramatically increase, fundamentally rearranging global security architecture.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons to human civilization, making traditional concepts of military victory in great-power conflict obsolete.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by every nuclear power's strategic doctrine, non-proliferation treaties, and the ongoing academic discourse in strategic studies, all of which operate under the premise of nuclear deterrence as a 'fact' of international relations. No credible external source disputes the problem's continued existence.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, ExtMetricName, E),
    domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that no single party directly 'profits' from the unwinnability of war; rather, it imposes a diffuse, structural cost on military institutions whose traditional mission is undermined. The high suppression (0.95) and accessibility collapse (0.98) are due to the physical reality of nuclear weapons, which fundamentally alters the strategic landscape, making alternatives to deterrence (like planning for victory in total war) physically and logically untenable. The theater ratio is very low (0.05) because the constraint is a genuine physical/logical limit, not a performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civilian populations, this constraint is a pure Mountain, ensuring their survival. From the perspective of military establishments, it is a Mountain that imposes a profound, identity-locked cost, forcing a redefinition of their purpose. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are beneficiaries (d=0.0) as they are spared total war. Military establishments and traditional strategic planners are targets (d=1.0) as their core functions are rendered incoherent. Nuclear powers, while instantiating the constraint, are also constrained by it, forced to manage a system where their ultimate weapons cannot achieve traditional victory.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Mountain, meaning its mandate is inherent to the physical reality of nuclear weapons. Mandatrophy is not applicable in the traditional sense, as its function (preventing total war) remains live and is a direct consequence of its existence. The classification prevents mislabeling a fundamental physical/logical limit as a human-constructed extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_rhetorical_contraction,
    'Is the unwinnability of total war a genuine operational contraction (as this reading claims), or primarily a rhetorical contraction where winnability remains operationally planned but unsayable (as the ''rhetorical_contraction'' sibling reading suggests)?',
    'Analysis of classified strategic planning documents and military exercises over time: if plans consistently show no path to victory, it supports operational contraction; if they show limited victory scenarios, it supports rhetorical contraction.',
    'If primarily rhetorical, the constraint''s ''emerges_naturally'' claim is weakened, and its ''theater_ratio'' would be higher, potentially reclassifying it as a Tangled Rope or Snare from the perspective of strategic planners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_rhetorical_contraction, empirical, 'Distinguishing between genuine operational limits and discursive suppression of strategic options.').

omega_variable(
    absolute_vs_constrained_unwinnability,
    'Is the unwinnability of total war truly categorical and absolute, or is it merely highly constrained, allowing for theoretical ''limited victory'' scenarios (as the ''countervailing_thinkable'' sibling reading suggests)?',
    'Further theoretical and simulation work on nuclear exchange scenarios, particularly focusing on the escalation dynamics and post-exchange societal collapse. If all paths lead to unacceptable outcomes, it supports absolute unwinnability.',
    'If limited victory is genuinely possible, the ''emerges_naturally'' claim is weakened, and the constraint''s extractiveness on military establishments might be lower, as their mission would retain some coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolute_vs_constrained_unwinnability, conceptual, 'The degree of unwinnability: absolute or merely highly constrained.').

omega_variable(
    beneficiary_status_of_civilian_populations,
    'Are civilian populations truly ''beneficiaries'' of this constraint, or are they merely ''survivors'' of a system that imposes existential risk, with the ''benefit'' being the absence of the worst outcome rather than a positive gain?',
    'Conceptual analysis of ''benefit'' in the context of existential threats. If ''benefit'' requires a positive gain beyond mere survival, then civilian populations are not beneficiaries.',
    'If civilian populations are not beneficiaries, the constraint''s coordination function is less clear, and its classification might shift towards a more purely structural Mountain without a clear ''good'' outcome for any party.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_status_of_civilian_populations, conceptual, 'The nature of ''benefit'' for populations under existential threat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1960, 0.95).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1980, 0.98).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2000, 0.97).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
