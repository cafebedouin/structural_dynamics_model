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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   This constraint represents the 'deterrence unthinkable' reading of
 *   nuclear weapons' impact on great-power total war. It posits that nuclear
 *   weapons fundamentally altered the nature of warfare, making victory in a
 *   total conflict impossible and rendering traditional strategic planning
 *   incoherent. This is presented as a 'natural law' (Mountain) arising from
 *   the physics of nuclear destruction, rather than a policy choice. The
 *   constraint's primary effect is to suppress the very concept of
 *   winnability, channeling strategic thought towards prevention and
 *   deterrence. This reading is one of several interpretations of the
 *   post-1945 strategic landscape.
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
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '121e1f9c-db16-44fa-a121-cd48fdf3d59e').
narrative_ontology:cs_kernel_codification('121e1f9c-db16-44fa-a121-cd48fdf3d59e', implicit).
narrative_ontology:cs_authority_grounding('121e1f9c-db16-44fa-a121-cd48fdf3d59e', diffuse_epistemic).
narrative_ontology:cs_reading_relation('121e1f9c-db16-44fa-a121-cd48fdf3d59e', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('121e1f9c-db16-44fa-a121-cd48fdf3d59e', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('121e1f9c-db16-44fa-a121-cd48fdf3d59e', foundational, mutual_assured_destruction_is_absolute).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('121e1f9c-db16-44fa-a121-cd48fdf3d59e', mutual_assured_destruction_is_absolute, empirically_contingent).
narrative_ontology:cs_axiom('121e1f9c-db16-44fa-a121-cd48fdf3d59e', foundational, total_war_is_existential_risk).
narrative_ontology:cs_axiom_status(total_war_is_existential_risk, holdable).
narrative_ontology:cs_axiom_grounding('121e1f9c-db16-44fa-a121-cd48fdf3d59e', total_war_is_existential_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('121e1f9c-db16-44fa-a121-cd48fdf3d59e', post_hiroshima_existential_dilemma).
narrative_ontology:cs_drift_state('121e1f9c-db16-44fa-a121-cd48fdf3d59e', contemporary_strategic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('121e1f9c-db16-44fa-a121-cd48fdf3d59e', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, traditional_strategic_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, arms_control_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the categorical unwinnability of total war, as it theoretically prevents their annihilation. They are trapped by the existence of nuclear weapons but are beneficiaries of the deterrence outcome.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Face a fundamental challenge to their traditional mission of achieving victory in great-power conflict. Their identity is tied to warfighting, which becomes incoherent under this constraint. Exit means a radical redefinition of purpose.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, identity_locked, global).

% Their professional expertise in planning for victory becomes obsolete. They are identity-locked by their training and career paths, making adaptation to a 'no-win' paradigm difficult.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, traditional_strategic_planners, payer,
    organized, biographical, identity_locked, global).

% Are forced to prioritize war prevention over warfighting, fundamentally altering their strategic calculus. They administer the nuclear arsenals but are constrained by the existential risk.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, political_leaders, agenda_setter,
    institutional, immediate, constrained, national).

% Their arguments for disarmament and non-proliferation are strengthened by the unwinnability thesis. They benefit from the intellectual and moral force this constraint provides to their agenda.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, arms_control_advocates, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates great-power behavior by establishing a shared understanding that total war is self-defeating, thereby channeling competition into non-nuclear domains and preventing escalation.
% TRANSFER_FUNCTION: Transfers the concept of 'victory' in total war from the realm of achievable outcomes to the realm of historical impossibility, shifting resources and intellectual effort from warfighting to deterrence and prevention.
% ABSENT_VOICES: Those who believe in the possibility of limited nuclear victory or who advocate for a return to traditional warfighting doctrines are marginalized or dismissed as unrealistic, their voices suppressed by the perceived 'natural law' of nuclear unwinnability.
% DISAPPEARANCE_RATIONALE: If nuclear unwinnability vanished, great powers would immediately re-engage in planning for total war, potentially leading to a rapid and catastrophic escalation of conflict. The entire international security architecture would collapse.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons to human civilization, making traditional military victory a pathway to mutual destruction.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on nuclear winter, the historical record of near-misses during the Cold War, and the ongoing existence of large nuclear arsenals corroborate that the problem remains live. This is attested by independent scientific bodies, former strategic planners, and international organizations, not just the benefiting parties.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.15) because the constraint primarily extracts from abstract concepts (winnability, traditional strategy) and institutional identities (military establishments' mission), rather than directly from human agents. Suppression is very high (0.95) because the physical reality of nuclear weapons makes the alternative (planning for victory) almost entirely inaccessible and irrational. Theater ratio is low (0.05) as the constraint is genuinely functional in preventing total war, with minimal performative elements. Accessibility collapse is near total (0.98) as the alternative of winning a total war is physically foreclosed. Resistance is low (0.1) because the physical reality is undeniable, though some strategic communities resist the full implications.
 *
 * PERSPECTIVAL GAP:
 *   Military establishments and traditional strategic planners experience this as a profound loss of mission and identity, a 'snare' on their professional purpose. Civilian populations, however, experience it as a 'mountain' that protects them from existential threat. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are beneficiaries (d=0.0) as the constraint protects them from annihilation. Military establishments and traditional strategic planners are victims (d=1.0) as their core mission and identity are undermined. Political leaders are agenda-setters (d=0.5) who must operate within this constraint, balancing deterrence with the impossibility of victory. Arms control advocates are beneficiaries (d=0.0) as their arguments are strengthened.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_strategic_choice,
    'Is the unwinnability of nuclear war a ''natural law'' (Mountain) derived from physics, or a ''strategic choice'' (Rope/Snare) maintained by doctrine and institutional inertia?',
    'Analysis of historical strategic debates and declassified planning documents: if significant factions consistently argued for winnability and were suppressed, it leans towards a constructed constraint. If the physical reality consistently foreclosed options, it leans towards natural law.',
    'If a constructed constraint, the extractiveness from military establishments is higher, and the classification shifts from Mountain to Tangled Rope or Snare, reflecting active enforcement of a chosen doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_strategic_choice, conceptual, 'Ambiguity between physical reality and strategic interpretation.').

omega_variable(
    operational_vs_rhetorical_contraction,
    'Did the space for war winnability truly contract operationally, or primarily rhetorically, with operational planning for victory continuing in secret?',
    'Declassification of Cold War and post-Cold War strategic plans, comparing stated doctrine with actual operational contingencies. Analysis of military procurement and training exercises for evidence of ''victory'' scenarios.',
    'If contraction was primarily rhetorical, the ''deterrence_unthinkable'' reading is a form of ''theater'' (Piton) or ''snare'' (Snare) for military establishments, masking continued pursuit of winnability. This would significantly increase the theater_ratio and extractiveness from military planners.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_vs_rhetorical_contraction, empirical, 'Distinction between stated policy and actual operational planning.').


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
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1980, 0.95).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nuclear_proliferation_treaty).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, arms_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'war_winnability_post_1945' kernel. Its unwinnability thesis influences the perceived legitimacy and operational space of other strategic constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
