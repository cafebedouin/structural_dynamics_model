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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Great-Power Total War is Unwinnable (Post-Nuclear Deterrence Reading)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'deterrence_unthinkable' reading
 *   of the 'war_winnability_post_1945' kernel. It posits that the advent of
 *   nuclear weapons fundamentally altered the nature of great-power total
 *   war, rendering it categorically unwinnable and making any strategic
 *   planning for victory incoherent. This reading emphasizes the physical and
 *   logical limits imposed by nuclear arsenals, shifting the focus of
 *   strategic thought from winning wars to preventing them. The constraint is
 *   presented as a Mountain, reflecting its status as an irreducible feature
 *   of the post-nuclear strategic landscape.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.15).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.9).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.15).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Great-Power Total War is Unwinnable (Post-Nuclear Deterrence Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '8f95a76e-b7be-43ce-ad44-1d30b0cd44b2').
narrative_ontology:cs_kernel_codification('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', formalized).
narrative_ontology:cs_authority_grounding('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', expertise).
narrative_ontology:cs_interpretation_layer_present('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2').
narrative_ontology:cs_reading_relation('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', foundational, mutual_assured_destruction_is_absolute).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', mutual_assured_destruction_is_absolute, empirically_contingent).
narrative_ontology:cs_axiom('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', foundational, escalation_is_uncontrollable).
narrative_ontology:cs_axiom_status(escalation_is_uncontrollable, holdable).
narrative_ontology:cs_axiom_grounding('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', escalation_is_uncontrollable, empirically_contingent).
narrative_ontology:cs_reference_frame('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', post_hiroshima_strategic_reality).
narrative_ontology:cs_drift_state('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8f95a76e-b7be-43ce-ad44-1d30b0cd44b2', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, strategic_planners).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, nuclear_revolution_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the categorical unwinnability of total war, as it prevents their annihilation. They are the ultimate beneficiaries of nuclear deterrence, even if they have no agency in its maintenance or conceptualization.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Bear the cost of mission incoherence; their traditional role of planning for and achieving victory in total war becomes meaningless. They are identity-locked to a strategic paradigm that nuclear weapons have rendered obsolete, leading to internal tension and redefinition of purpose.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, identity_locked, global).

% Must grapple with the intellectual and operational challenge of planning for a war that cannot be won. Their professional identity is challenged, and their work shifts from victory to war prevention, limited conflict management, and deterrence maintenance.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_planners, payer,
    organized, biographical, constrained, global).

% Responsible for managing the implications of unwinnable total war, including maintaining deterrence and preventing escalation. They must articulate a strategy that acknowledges this reality while still projecting strength and managing international relations.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, political_leaders, agenda_setter,
    institutional, immediate, constrained, global).

% Analyze the structural implications of nuclear weapons on international relations and the concept of war. They articulate and refine the 'unwinnability' thesis, often challenging conventional military thinking.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__deterrence_unthinkable, diffuse).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__deterrence_unthinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates great powers towards mutual restraint and war prevention by making total war a self-defeating endeavor for all parties, thereby establishing a new, albeit dangerous, form of strategic stability.
% TRANSFER_FUNCTION: Transfers the concept of 'victory' in great-power total war from military doctrine to the realm of impossibility, effectively transferring resources and strategic focus from offensive war-winning capabilities to deterrence, arms control, and crisis management.
% ABSENT_VOICES: Historical military strategists and political leaders who believed in decisive victory through overwhelming force; their doctrines are rendered obsolete by this constraint. Their voices are absent from contemporary strategic discourse on total war.
% DISAPPEARANCE_RATIONALE: If nuclear weapons ceased to exist, the fundamental premise of unwinnability would vanish, and great-power total war might once again be considered a viable, albeit catastrophic, option. This would lead to a complete reorientation of strategic thought, military planning, and international relations, potentially increasing the risk of large-scale conflict.
% FOUNDING_PROBLEM: The existential threat of mutual annihilation posed by the advent of nuclear weapons, which rendered traditional concepts of total war and victory obsolete and created an imperative for war prevention.
% FOUNDING_PROBLEM_CORROBORATION: The enduring absence of great-power total war since 1945, the continued existence of nuclear arsenals, and the consensus among most international relations theorists and arms control advocates, corroborated by historical analysis and game theory, support the live status of this problem. The constant threat of escalation reinforces this reality.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.15) reflects that the constraint primarily extracts the *concept* of victory from military doctrine, rather than directly extracting resources from active agents in a coercive manner. The high suppression (0.90) is due to the physical reality of nuclear weapons, which structurally suppresses the possibility of a winnable total war. Accessibility collapse is very high (0.95) because the alternative (winnable total war) is almost entirely foreclosed. Resistance (0.40) is moderate, as some strategic thinkers and military planners continue to explore scenarios for limited victory or escalation dominance, even if total victory is deemed impossible. The theater ratio is very low (0.05) because the unwinnability is a fundamental truth, not a performance.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between those who fully accept the categorical unwinnability of total war (e.g., many arms control advocates and some international relations theorists) and those within military establishments who, while acknowledging the dangers, still seek to define conditions for 'victory' or 'escalation control' in a nuclear conflict. The former see an absolute limit, the latter seek to find operational space within that limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are the primary beneficiaries (d near 0.0) as the constraint prevents their annihilation in a total war. Military establishments and strategic planners are the primary targets (d near 1.0) as their traditional mission of achieving victory in total war is rendered incoherent, forcing a fundamental redefinition of their purpose and methods. Political leaders operate as agenda-setters, navigating this new reality. Analytical observers provide the intellectual framework for understanding this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   As a Mountain, this constraint's 'mandate' (to prevent total war) is inherently tied to its existence as a physical and logical reality. It does not suffer from mandatrophy in the sense of an artificial construct whose function has atrophied. Instead, its persistence is guaranteed by the continued existence of nuclear weapons. The challenge is not its decay, but the resistance to fully accepting its implications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_consensus,
    'Is the categorical unwinnability of great-power total war a genuine Mountain of physical/logical reality, or a constructed consensus that benefits certain actors (e.g., those who profit from the nuclear industry''s ''stability'' or the academic field of deterrence theory)?',
    'Analysis of historical strategic discourse for evidence of deliberate framing or suppression of alternative views, alongside counterfactual analysis of strategic outcomes in a non-nuclear world.',
    'If primarily a constructed consensus, the constraint would reclassify towards a Tangled Rope or Snare, indicating an extractive element in its maintenance, despite its apparent naturalness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_consensus, conceptual, 'Ambiguity between natural law and constructed consensus regarding nuclear unwinnability.').

omega_variable(
    limited_war_vs_total_war_boundary,
    'Does the concept of ''limited nuclear war'' or ''escalation dominance'' truly challenge the categorical unwinnability of *total* war, or is it a form of strategic denial that ultimately leads back to the same unwinnable outcome?',
    'Further theoretical development and historical analysis of near-misses and crisis management, combined with game-theoretic modeling of escalation pathways, to determine if a stable ''limited'' nuclear conflict is genuinely possible without escalating to total war.',
    'If limited nuclear war is deemed genuinely possible and controllable, it would weaken the ''categorical unwinnability'' claim, potentially shifting the constraint''s suppression and accessibility collapse metrics downwards, and moving it away from a pure Mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(limited_war_vs_total_war_boundary, empirical, 'Ambiguity regarding the boundary between limited and total nuclear war and its implications for unwinnability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(war__tr_t1965, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(war__tr_t1985, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(war__tr_t2005, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(war__be_t1965, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(war__be_t1985, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1985, 0.14).
narrative_ontology:measurement(war__be_t2005, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.85).
narrative_ontology:measurement(war__su_t1965, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1965, 0.88).
narrative_ontology:measurement(war__su_t1985, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1985, 0.9).
narrative_ontology:measurement(war__su_t2005, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2005, 0.9).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nuclear_proliferation_regime).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, arms_control_treaties).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, conventional_military_buildup_limits).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('deterrence_unthinkable') of the 'war_winnability_post_1945' kernel. It posits that nuclear weapons made great-power total war categorically unwinnable. Sibling readings include 'countervailing_thinkable' (limited victory remains achievable) and 'rhetorical_contraction' (unwinnability is a discursive taboo, not an operational reality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
