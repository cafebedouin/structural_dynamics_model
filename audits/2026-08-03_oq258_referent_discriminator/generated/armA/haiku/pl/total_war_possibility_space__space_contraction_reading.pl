% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Removed from Strategic Possibility Space (Space Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint asserts that nuclear weapons have done something
 *   structurally different from merely raising the cost of total war: they
 *   have removed total war from the set of strategically calculable options.
 *   Under this reading, the constraint is a categorical exclusion from the
 *   possibility space, not a high-cost deterrent that remains on the menu.
 *   The contrast is between 'very expensive' and 'logically incoherent.' A
 *   strategic planner cannot coherently game through a scenario that
 *   escalates to mutual nuclear annihilation and claim victory; the scenario
 *   contradicts the definition of strategy (achieving objectives). Therefore,
 *   total war has exited not just preference but thinkability. This generates
 *   institutional atrophy: war colleges stopped teaching total-war doctrine
 *   not because it was forbidden but because it became incoherent to teach.
 *
 * KEY AGENTS:
 *   - Strategic planners in nuclear-armed great powers: operate within a possibility space where total war is unthinkable
 *   - Military general staffs: experience atrophy of total-war mobilization apparatus; war-gaming focuses on limited conflicts
 *   - Strategic studies discipline: shifted empirical domain from total-war theory to deterrence and escalation control
 *   - Nuclear taboo advocates (excluded): argue the constraint is normative, not material
 *   - Deterrence equilibrium theorists (excluded): argue total war remains thinkable but deterred at high cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.18).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.05).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Removed from Strategic Possibility Space (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '3a812d5d-6fe0-4c7f-b3a2-40d6684756d2').
narrative_ontology:cs_kernel_codification('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', implicit).
narrative_ontology:cs_authority_grounding('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', practice).
narrative_ontology:cs_reading_relation('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', foundational, nuclear_escalation_logically_defeats_all_strategies).
narrative_ontology:cs_axiom_status(nuclear_escalation_logically_defeats_all_strategies, holdable).
narrative_ontology:cs_axiom_grounding('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', nuclear_escalation_logically_defeats_all_strategies, empirically_contingent).
narrative_ontology:cs_axiom('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', foundational, strategic_coherence_requires_winning_scenario).
narrative_ontology:cs_axiom_status(strategic_coherence_requires_winning_scenario, holdable).
narrative_ontology:cs_axiom_grounding('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', strategic_coherence_requires_winning_scenario, deontological).
narrative_ontology:cs_reference_frame('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', total_war_strategically_calculable).
narrative_ontology:cs_drift_state('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a812d5d-6fe0-4c7f-b3a2-40d6684756d2', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, humanity_in_post_nuclear_great_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, nuclear_armed_strategic_planners).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, military_general_staffs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Structural beneficiary of a strategic environment where total war has exited the planning space. The nuclear-armed great powers cannot rationally conceive total war as a thinkable option; this exerts aggregate gravitational pressure on the possibility space available to all strategists. The constraint is categorical — it is not that total war became very costly and thus disfavored, but that it became unthinkable within the logic of nuclear deterrence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, humanity_in_post_nuclear_great_powers, beneficiary,
    analytical, civilizational, analytical, global).

% Must operate within a collapsed planning space. The existence of nuclear weapons has not merely changed the cost-benefit calculation for total war; it has removed total war from the menu of strategically coherent options. Planners cannot think through total war without immediately encountering logical contradiction: any scenario that escalates to nuclear exchange is not a winning scenario by definition. The constraint operates as a hard categorical limit on war-gaming and doctrine.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_armed_strategic_planners, payer,
    institutional, generational, trapped, global).

% Experience institutional atrophy in total-war planning apparatus. War colleges stopped teaching total-war mobilization doctrine not because it was forbidden but because it became incoherent — there is no winning path through that scenario. General-staff exercises focus on limited war, escalation control, and sub-nuclear conflict. The atrophy is not performative; it reflects that the planning tradition itself has been structurally rewritten.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, military_general_staffs, payer,
    institutional, generational, constrained, national).

% The discipline's empirical domain has shifted. Nuclear deterrence theory, crisis stability, and escalation control are the live research questions; the theory of total-war mobilization is archived. The shift is not ideological but structural: scholars study what planners face, and planners no longer face a thinkable path to total war.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_discipline, observer,
    organized, generational, mobile, global).

% Would argue that total war is prohibited by constructed normative taboo, not by material logic. Under this reading, the space-contraction claim misattributes a normative prohibition to a strategic logic. They are excluded from the space-contraction account because the reading explicitly locates the constraint in the calculability problem, not in the norm.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_taboo_advocates, excluded,
    organized, generational, mobile, global).

% Would argue that total war remains strategically thinkable but is deterred by mutual vulnerability — war remains on the menu, just at prohibitive cost. Under the space-contraction reading, this mischaracterizes the constraint: total war is not on the menu at all; it is not a costly option but an impossible option within the logic of nuclear strategy.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_theorists, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination function. This is a structural constraint on the possibility space itself — it fixes what strategists can coherently think through, not what they coordinate on.
% TRANSFER_FUNCTION: No transfer. The constraint is categorical: it removes a strategic option from the calculable set. There is no exchange of costs and benefits; there is a boundary on what counts as a strategically coherent scenario.
% ABSENT_VOICES: Theorists of the deterrence-equilibrium reading and advocates of the nuclear-taboo reading are structurally excluded. The space-contraction reading asserts a material logic of calculability; the deterrence reading asserts continued thinkability at prohibitive cost; the taboo reading asserts normative prohibition. These are distinct claims about what the constraint is. Advocates of the other readings have a position that contradicts this reading's core premise.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared (i.e., if nuclear weapons suddenly ceased to exist or lost their destructive capacity), the strategic possibility space would expand and total war would re-enter planning scenarios. The world would not rearrange because the constraint has never been a coordination mechanism; it would rearrange because the logical constraint that removed total war from calculability would be gone. However, the very claim that the constraint is 'real' is contested: the deterrence and taboo readings dispute whether total war has actually exited the thinkable or merely become disfavored/prohibited.
% FOUNDING_PROBLEM: Total war in the industrial and post-industrial age threatened to consume all resources and all populations in pursuit of national strategic objectives, with no upper bound on destructiveness. The founding problem was: how to prevent great powers from mobilizing entire societies for wars of annihilation.
% FOUNDING_PROBLEM_CORROBORATION: Pre-nuclear strategic doctrine (Clausewitz, World War II mobilization, Cold War early planning) attests the founding problem was live: total war was thinkable and attempted. Post-nuclear strategists attest the problem is no longer urgent because total war is no longer thinkable (this reading's claim). However, deterrence theorists attest the problem remains live but is managed through mutual vulnerability rather than banishment from the possibility space. No corroboration exists outside the three reading camps; the disagreement is foundational and unresolved.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim is mountain: the constraint emerges from the material logic of nuclear weapons, not from human choice or preference. Extractiveness is low (0.18 final) because a mountain imposes no extraction — it is a boundary condition. Suppression is negligible (0.05) because no active enforcement is required; the constraint is self-enforcing through logical incoherence. Theater ratio is low (0.12) but non-zero: institutional practices around deterrence doctrine, nuclear stability, and crisis management perform the constraint's logic while appearing to manage deterrence. The measurement series shows slight decay in both extractiveness and theater over 80 time units as the institutional infrastructure fully internalizes the constraint — it becomes less novel, less actively maintained as a distinct proposition, and more just 'how strategy works.' This is consistent with a mountain: the constraint strengthens in the structural sense (becomes more fully embedded) while the apparent extraction diminishes (because it is never extracted — it is just the shape of the space).
 *
 * PERSPECTIVAL GAP:
 *   The space-contraction reading predicts uniform structural experience across all nuclear-armed great-power planners: the possibility space is collapsed for all of them identically. The deterrence reading would predict divergent experience based on power asymmetries (one side thinks it could win; the other does not). The taboo reading would predict divergent experience based on normative commitment (some states have stronger taboos than others). This reading predicts convergent structural experience: the logic of nuclear deterrence produces the same categorical boundary for all parties. The engine should compute this convergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides needed. The constraint has no extraction mechanism — it is a categorical boundary. The 'beneficiaries' are not seats that collect from the constraint; they are humanity in nuclear-armed great-power relationships, which benefits from the removal of total war from thinkability. This is not extraction; it is structural constraint. Planners are 'payers' only in the sense that they must operate within the collapsed space; they are not paying extraction to anyone.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has no mandate. It is not a governance arrangement or a rule system. It is a structural consequence of material facts (nuclear weapons) and logical facts (nuclear war cannot be won in the strategic sense). The founding problem — preventing total war — has not outlived its function; the function persists (total war remains absent from planning). No mandatrophy condition obtains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    calculability_vs_preference,
    'Is total war genuinely removed from the strategic possibility space (logically incoherent to pursue), or merely from strategic preference (would be catastrophic but remains thinkable)?',
    'Close examination of strategic doctrine, war-college curricula, and general-staff exercises across multiple nuclear-armed powers at different time periods. If total war never appears in war-gaming even as a boundary case or reductio, the space-contraction claim is stronger. If total war appears in scenarios as a reductio to which planners explicitly say ''we must avoid,'' it is still thinkable (merely disfavored).',
    'If thinkable but disfavored, the constraint is closer to snare or tangled_rope (high cost, enforced avoidance) than to mountain (logical impossibility). If truly unthinkable, the mountain classification is justified. This is the core empirical question for the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(calculability_vs_preference, empirical, 'Whether total war is logically incoherent or merely strategically undesirable.').

omega_variable(
    reading_substitution_ambiguity,
    'Is the space-contraction reading distinguishable from the deterrence-equilibrium reading, or are they the same structural claim stated differently?',
    'Isolate the core premises: space-contraction asserts ''total war is logically incoherent as a winning strategy''; deterrence asserts ''total war is thinkable but deterred by mutual vulnerability.'' These are logically distinct. However, if empirical observation cannot separate them (both predict the same institutional atrophy, the same absence of total-war planning), they may be describing the same constraint through different framings. A test: does deterrence allow for the possibility that deterrence might fail and total war would then become calculable? If yes, it is distinct from space-contraction. If deterrence means ''mutually assured destruction is built into any escalation path such that total war is never reachable even if deterrence fails,'' it collapses into space-contraction.',
    'If the readings are empirically indistinguishable, the space-contraction reading may be the more parsimonious statement of the same constraint. If they are distinguishable, they are genuinely different constraints and should remain separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_substitution_ambiguity, conceptual, 'Whether space-contraction and deterrence-equilibrium are distinct or alternative framings of the same constraint.').

omega_variable(
    institutional_atrophy_causation,
    'Does the atrophy of total-war planning apparatus follow from the space-contraction logic, or from deterrence cost-logic, or from something else (normative taboo, organizational path dependence)?',
    'Historical-institutional analysis: trace the timing of doctrine changes, war-college curriculum shifts, and strategic-analysis domain drift against the theoretical predictions of each reading. If atrophy occurred immediately after nuclear weapons emerged and persists regardless of deterrence stability, it supports space-contraction. If atrophy fluctuates with deterrence stability (Cold War relaxation periods see renewed total-war planning), it supports deterrence logic.',
    'Causation determines the reading. If the atrophy is truly path-dependent on space-contraction logic, the reading holds independently of what actually happens to deterrence. If the atrophy is contingent on sustained deterrence, the reading is narrower and more fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_atrophy_causation, empirical, 'Whether institutional atrophy of total-war doctrine is caused by space-contraction logic or by deterrence stability.').

omega_variable(
    kernel_contest_reconcilability,
    'Can a single framework hold all three readings of the total-war kernel, or are they genuinely exclusive?',
    'Formal analysis: map the premises of each reading and test for logical contradiction. Space-contraction vs. deterrence: does deterrence require that total war remain thinkable? If yes, they foreclose each other. Space-contraction vs. taboo: can a norm prohibit something that is logically impossible? If yes, they can coexist (one explains the material logic, one explains the norm). Deterrence vs. taboo: are mutual deterrence and normative taboo orthogonal or redundant?',
    'This determines the reading_relations classification in cs_structure. If the readings foreclose each other, use ''forecloses''. If they can coexist as different aspects of the same institutional reality, use ''coexists_with''. If one creates structural conditions for the other, use ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_reconcilability, conceptual, 'Logical relationship between space-contraction, deterrence, and taboo readings of the nuclear constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__space_contraction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t8, total_war_possibility_space__space_contraction_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(tota_tr_t8, observed).
narrative_ontology:measurement(tota_tr_t16, total_war_possibility_space__space_contraction_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(tota_tr_t16, observed).
narrative_ontology:measurement(tota_tr_t24, total_war_possibility_space__space_contraction_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(tota_tr_t24, observed).
narrative_ontology:measurement(tota_tr_t40, total_war_possibility_space__space_contraction_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(tota_tr_t40, observed).
narrative_ontology:measurement(tota_tr_t56, total_war_possibility_space__space_contraction_reading, theater_ratio, 56, 0.12).
narrative_ontology:measurement_basis(tota_tr_t56, observed).
narrative_ontology:measurement(tota_tr_t80, total_war_possibility_space__space_contraction_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement_basis(tota_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__space_contraction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t8, total_war_possibility_space__space_contraction_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(tota_be_t8, observed).
narrative_ontology:measurement(tota_be_t16, total_war_possibility_space__space_contraction_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement_basis(tota_be_t16, observed).
narrative_ontology:measurement(tota_be_t24, total_war_possibility_space__space_contraction_reading, base_extractiveness, 24, 0.19).
narrative_ontology:measurement_basis(tota_be_t24, observed).
narrative_ontology:measurement(tota_be_t40, total_war_possibility_space__space_contraction_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(tota_be_t40, observed).
narrative_ontology:measurement(tota_be_t56, total_war_possibility_space__space_contraction_reading, base_extractiveness, 56, 0.18).
narrative_ontology:measurement_basis(tota_be_t56, observed).
narrative_ontology:measurement(tota_be_t80, total_war_possibility_space__space_contraction_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement_basis(tota_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__space_contraction_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement_basis(tota_su_t0, observed).
narrative_ontology:measurement(tota_su_t8, total_war_possibility_space__space_contraction_reading, suppression_requirement, 8, 0.04).
narrative_ontology:measurement_basis(tota_su_t8, observed).
narrative_ontology:measurement(tota_su_t16, total_war_possibility_space__space_contraction_reading, suppression_requirement, 16, 0.05).
narrative_ontology:measurement_basis(tota_su_t16, observed).
narrative_ontology:measurement(tota_su_t24, total_war_possibility_space__space_contraction_reading, suppression_requirement, 24, 0.05).
narrative_ontology:measurement_basis(tota_su_t24, observed).
narrative_ontology:measurement(tota_su_t40, total_war_possibility_space__space_contraction_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement_basis(tota_su_t40, observed).
narrative_ontology:measurement(tota_su_t56, total_war_possibility_space__space_contraction_reading, suppression_requirement, 56, 0.05).
narrative_ontology:measurement_basis(tota_su_t56, observed).
narrative_ontology:measurement(tota_su_t80, total_war_possibility_space__space_contraction_reading, suppression_requirement, 80, 0.05).
narrative_ontology:measurement_basis(tota_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.15).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% The total_war_possibility_space kernel decomposes into three structurally distinct constraints: (1) space_contraction_reading claims nuclear weapons have made total war logically incoherent; (2) deterrence_equilibrium_reading claims total war remains thinkable but is deterred by mutual vulnerability; (3) nuclear_taboo_reading claims total war became normatively prohibited through constructed taboo. Each reading has a different ε (this one is low, as it asserts a categorical boundary rather than extraction), different beneficiary/victim structure, and different type. They share a kernel (the relationship between nuclear weapons and war calculability) but instantiate different constraints from that kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
