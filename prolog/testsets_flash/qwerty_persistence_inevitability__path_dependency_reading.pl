% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Persistence (Path Dependency Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   as an outcome of accident-driven path dependency, where initial random
 *   events (e.g., early typewriter design choices to prevent jamming) created
 *   a self-reinforcing feedback loop. This reading posits that the efficiency
 *   loss from QWERTY's suboptimal design is a diffuse externality, not
 *   actively extracted by any strategic beneficiary. The constraint is
 *   treated as a technological inevitability given its historical trajectory
 *   and the high switching costs for users and manufacturers. This is one
 *   reading of the 'qwerty_persistence_inevitability' kernel, contrasting
 *   with a 'strategic_lock_in_reading' that would attribute persistence to
 *   deliberate manufacturer actions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.95).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96').
narrative_ontology:cs_kernel_codification('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', implicit).
narrative_ontology:cs_authority_grounding('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', practice).
narrative_ontology:cs_reading_relation('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', foundational, technological_evolution_is_path_dependent).
narrative_ontology:cs_axiom_status(technological_evolution_is_path_dependent, holdable).
narrative_ontology:cs_axiom_grounding('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', technological_evolution_is_path_dependent, empirically_contingent).
narrative_ontology:cs_axiom('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', foundational, efficiency_loss_is_diffuse_externality).
narrative_ontology:cs_axiom_status(efficiency_loss_is_diffuse_externality, holdable).
narrative_ontology:cs_axiom_grounding('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', efficiency_loss_is_diffuse_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', accidental_historical_contingency).
narrative_ontology:cs_drift_state('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('eaa3d4e7-f335-4308-8e5b-e7c0a6c56d96', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, typing_tutors_and_educators).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, keyboard_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users are accustomed to QWERTY through years of muscle memory and training. Switching to an alternative layout (e.g., Dvorak) requires significant retraining costs and is impractical given the ubiquity of QWERTY hardware and software. They bear the diffuse cost of suboptimal efficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_users, payer,
    powerless, biographical, identity_locked, global).

% Produce QWERTY keyboards because that is what the market demands. While they could theoretically produce alternative layouts, the lack of demand and high retooling costs make it economically unfeasible. They are constrained by the existing standard, not actively enforcing it for rent extraction in this reading.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, agenda_setter,
    organized, generational, constrained, global).

% Develop and advocate for more efficient keyboard layouts (e.g., Dvorak, Colemak). They face insurmountable barriers to market penetration due to the entrenched QWERTY standard, despite the potential for improved typing speed and ergonomics. Their innovations are effectively suppressed by the path dependency.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_developers, excluded,
    moderate, generational, trapped, global).

% Benefit from a stable, universal standard for keyboard instruction. Their curriculum is fixed, and they do not need to adapt to multiple layouts. While they could teach alternative layouts, the demand is negligible, and their primary function is to teach the dominant standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typing_tutors_and_educators, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the historical development and persistence of QWERTY, debating whether its dominance is due to accidental path dependency or strategic market manipulation. They provide the analytical framework for understanding the constraint.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, technology_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__path_dependency_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__path_dependency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, stable standard for human-computer interaction via keyboard input, allowing users to switch between devices and systems without retraining, and manufacturers to produce standardized hardware.
% TRANSFER_FUNCTION: This reading posits no active transfer of value from one party to another. Instead, it describes a diffuse, uncaptured efficiency loss borne by all users due to the suboptimal design of the entrenched standard.
% ABSENT_VOICES: Developers of alternative keyboard layouts are effectively excluded from the mainstream market; they would advocate for a more efficient, open standard but are marginalized by the inertia of the QWERTY system.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the entire digital world would grind to a halt. All hardware and software would be instantly incompatible with human input. A new standard would eventually emerge, but the transition would be catastrophic, demonstrating the deep structural dependence on the existing layout.
% FOUNDING_PROBLEM: Early typewriters faced mechanical jamming issues, leading to design choices (like separating common letter pairs) that prioritized mechanical function over typing speed. This created the initial QWERTY layout.
% FOUNDING_PROBLEM_CORROBORATION: The mechanical jamming problem is long dead with modern digital keyboards. Technology historians and ergonomic researchers (outside the keyboard manufacturing industry) corroborate that the original mechanical justification no longer applies, yet the layout persists due to network effects and switching costs. Keyboard manufacturers, while not directly benefiting from the 'dead' problem, continue to produce QWERTY due to market demand, not a live mechanical problem.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects the absence of a party actively collecting rents from QWERTY's inefficiency; the cost is borne diffusely by users as a suboptimal standard. High suppression (0.95) and accessibility collapse (0.9) reflect the near-impossibility of switching to alternative layouts due to network effects, ingrained muscle memory, and the installed base of hardware/software. Resistance is low (0.05) because collective action to change is futile against the inertia. The claimed type is Mountain because, from this reading, QWERTY's persistence is an irreducible structural feature of the technological landscape, not a human-maintained choice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a user, the QWERTY layout is an unchangeable fact of their interaction with computers. From the perspective of an alternative keyboard designer, it is a frustrating barrier. This reading emphasizes the 'natural law' aspect of path dependency, where no single agent benefits strategically, and the system's inertia is the primary force.
 *
 * DIRECTIONALITY LOGIC:
 *   Since no specific agent benefits from the 'extraction' (which is diffuse efficiency loss, not captured rent), and all users are equally subject to the path dependency, there are no identifiable beneficiaries or victims in the sense of active extraction. All agents are targets of the constraint's inertia, but none are specifically exploited by it. Manufacturers respond to demand for QWERTY, they do not create it in this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a diffuse, accident-driven technological inertia as a deliberate act of extraction. If the 'strategic_lock_in_reading' were true, the constraint would be a Snare, and the analysis would focus on identifying the beneficiaries and their mechanisms of suppression. This 'path_dependency_reading' argues that the mandate (to provide a functional keyboard) is still live, but the specific form (QWERTY) is an accidental, self-perpetuating outcome, not a designed one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_strategic_lock_in,
    'Is QWERTY''s persistence a result of accident-driven path dependency or strategic lock-in by manufacturers?',
    'Historical analysis of manufacturer actions, patent strategies, and lobbying efforts; counterfactual modeling of alternative keyboard layouts'' market penetration under different initial conditions.',
    'If strategic lock-in, the constraint would reclassify from Mountain to Snare or Tangled Rope, with identifiable beneficiaries (manufacturers) and victims (users, alternative layout developers). This reading (path_dependency_reading) asserts no strategic beneficiaries and diffuse, non-extracted efficiency loss.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_lock_in, empirical, 'Ambiguity between technological inevitability and engineered market control.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''path_dependency_reading'' of the ''qwerty_persistence_inevitability'' kernel. What would change if the ''strategic_lock_in_reading'' were adopted?',
    'Adoption of the ''strategic_lock_in_reading'' would introduce identifiable beneficiaries (keyboard manufacturers, training institutions) and victims (users, alternative layout developers), leading to a reclassification from Mountain to Snare or Tangled Rope.',
    'The entire structural analysis of extraction, suppression, and beneficiary/victim dynamics would shift, leading to a different constraint type and policy implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of adopting the sibling reading on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'QWERTY persistence inevitability' kernel. This 'path_dependency_reading' emphasizes accidental historical contingency and diffuse costs, while the 'strategic_lock_in_reading' (a sibling constraint) emphasizes deliberate manufacturer-driven market control and concentrated extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
