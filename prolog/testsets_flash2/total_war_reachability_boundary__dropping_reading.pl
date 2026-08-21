% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Probability Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint story instantiates the 'dropping_reading' of the
 *   'total_war_reachability_boundary' kernel. It posits that while the
 *   probability of total war has decreased, its reachability remains a live,
 *   if remote, possibility. Nuclear deterrence is viewed as a
 *   'tangled_rope'—a coordination mechanism that genuinely prevents
 *   large-scale conflict but also involves significant extraction
 *   (existential threat, resource diversion) and requires active enforcement
 *   (threats of retaliation, arms races). The claimed type 'tangled_rope'
 *   reflects the dual nature of deterrence as both a coordination solution
 *   and an extractive mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.45).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.6).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Probability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, 'a79ad956-65f7-470b-b237-b06a8f46c75e').
narrative_ontology:cs_kernel_codification('a79ad956-65f7-470b-b237-b06a8f46c75e', implicit).
narrative_ontology:cs_authority_grounding('a79ad956-65f7-470b-b237-b06a8f46c75e', extraction).
narrative_ontology:cs_interpretation_layer_present('a79ad956-65f7-470b-b237-b06a8f46c75e').
narrative_ontology:cs_reading_relation('a79ad956-65f7-470b-b237-b06a8f46c75e', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a79ad956-65f7-470b-b237-b06a8f46c75e', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('a79ad956-65f7-470b-b237-b06a8f46c75e', foundational, total_war_remains_feasible).
narrative_ontology:cs_axiom_status(total_war_remains_feasible, holdable).
narrative_ontology:cs_axiom_grounding('a79ad956-65f7-470b-b237-b06a8f46c75e', total_war_remains_feasible, empirically_contingent).
narrative_ontology:cs_axiom('a79ad956-65f7-470b-b237-b06a8f46c75e', foundational, deterrence_is_coordination_game).
narrative_ontology:cs_axiom_status(deterrence_is_coordination_game, holdable).
narrative_ontology:cs_axiom_grounding('a79ad956-65f7-470b-b237-b06a8f46c75e', deterrence_is_coordination_game, instrumental).
narrative_ontology:cs_reference_frame('a79ad956-65f7-470b-b237-b06a8f46c75e', cold_war_deterrence_equilibrium).
narrative_ontology:cs_drift_state('a79ad956-65f7-470b-b237-b06a8f46c75e', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a79ad956-65f7-470b-b237-b06a8f46c75e', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_establishments).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, global_population_under_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and deterrence postures, benefiting from the strategic stability (or coercive leverage) these provide. They actively enforce deterrence through threats of retaliation and arms control regimes. Exit from this system is constrained by perceived security needs.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the continued justification for large defense budgets, advanced weapons research, and strategic planning that nuclear deterrence entails. Their professional identity is deeply tied to maintaining this system.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_establishments, beneficiary,
    organized, biographical, identity_locked, national).

% Live under the constant, if low-probability, threat of nuclear annihilation. They bear the psychological and material costs of maintaining deterrence without direct agency in its operation. Exit is impossible.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, global_population_under_threat, payer,
    powerless, immediate, trapped, global).

% Are subject to the strategic dynamics set by nuclear powers, often facing pressure to align or seek protection. They bear the costs of proliferation risks and regional instability without the 'benefits' of nuclear deterrence. Exit options are limited to seeking alliances or developing their own deterrent.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Analyze the risks and benefits of nuclear deterrence, advocating for treaties and policies to reduce arsenals and prevent proliferation. They observe the system's dynamics and propose alternatives.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, arms_control_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the avoidance of direct military conflict between nuclear-armed states by establishing a mutual understanding that total war is unwinnable and catastrophic, thus incentivizing de-escalation.
% TRANSFER_FUNCTION: Transfers a sense of strategic stability (and the associated political leverage) to nuclear powers, while transferring existential risk and resource allocation burdens to the global population and non-nuclear states.
% ABSENT_VOICES: Future generations, who would bear the ultimate cost of deterrence failure, are absent. They would argue for immediate and complete disarmament, but their interests are represented only indirectly by advocacy groups.
% DISAPPEARANCE_RATIONALE: If the concept of total war reachability vanished, the strategic calculus of nuclear powers would fundamentally change. Deterrence postures would collapse, potentially leading to either rapid disarmament or increased conventional conflict, as the ultimate constraint on escalation would be removed. The global security architecture would be entirely reconfigured.
% FOUNDING_PROBLEM: The problem of preventing catastrophic, large-scale warfare between great powers, particularly after the advent of nuclear weapons, which made such conflicts potentially species-ending.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested as live by strategic analysts, international relations scholars, and non-proliferation experts across various institutions, not just by nuclear powers. The ongoing existence of nuclear arsenals and the continued focus on deterrence theory corroborate its persistence.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).
:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the ongoing costs of maintaining nuclear arsenals and the existential threat to populations, even if the probability of use is low. Suppression (0.6) is due to the active enforcement of deterrence through military postures and the suppression of alternatives to the nuclear order. The theater ratio (0.2) is relatively low, indicating that while some aspects of deterrence might be performative, the underlying threat is real. Accessibility collapse (0.4) is moderate, as alternatives to deterrence (e.g., global disarmament) are conceptually available but practically difficult to achieve. Resistance (0.3) is present from non-nuclear states and advocacy groups but is not strong enough to dismantle the system.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers perceive deterrence as a necessary 'rope' for global stability, a coordination mechanism that prevents a worse outcome. Populations under threat, however, experience it as a 'snare' due to the inherent existential risk and resource drain. This divergence is central to the 'tangled_rope' classification, where coordination and extraction are intertwined.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers and their defense establishments are beneficiaries/agenda-setters, as they control the system and derive strategic benefits. The global population and non-nuclear states are victims/payers, bearing the costs and risks without direct control. The 'identity_locked' exit option for defense establishments reflects their deep professional and institutional commitment to the deterrence framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing total war) is still live, preventing mandatrophy. However, the 'tangled_rope' classification highlights that the mechanism for achieving this mandate (deterrence) has become extractive, leveraging existential threat for strategic advantage. The classification prevents mislabeling it as a pure 'rope' by acknowledging the asymmetric costs and active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_vs_risk,
    'Is the current level of nuclear deterrence genuinely stable, or does it inherently carry an unacceptably high risk of accidental or intentional escalation?',
    'Long-term historical analysis of near-miss incidents, game-theoretic modeling of escalation dynamics, and expert consensus on the probability of deterrence failure.',
    'If the risk is deemed unacceptably high, the ''tangled_rope'' classification would shift closer to a ''snare'', emphasizing the extractive nature of the existential threat over the coordination function. If highly stable, it would lean more towards a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_vs_risk, empirical, 'Uncertainty regarding the true stability and risk profile of nuclear deterrence.').

omega_variable(
    alternative_security_regimes,
    'Are there viable alternative global security regimes that could achieve similar levels of conflict prevention without the extractive costs and risks of nuclear deterrence?',
    'Theoretical development and practical implementation of alternative security frameworks (e.g., global governance, comprehensive disarmament verification, robust conventional defense alliances) and their comparative analysis against deterrence.',
    'If viable alternatives exist and are suppressed, the ''suppression'' metric would be re-evaluated as higher, potentially pushing the classification further towards a ''snare''. If no viable alternatives exist, the ''tangled_rope'' classification is reinforced as the ''least bad'' option.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_security_regimes, preference, 'Uncertainty about the feasibility and desirability of alternatives to nuclear deterrence.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''contraction_reading'' (total war is no longer feasible) and ''contingent_reachability_reading'' (reachability is technology-dependent), which reading of the ''total_war_reachability_boundary'' kernel best describes the current strategic reality?',
    'Empirical evidence of strategic planning by nuclear powers, technological advancements in weaponry, and the observed frequency and intensity of near-miss incidents. Conceptual analysis of the logical coherence of each reading.',
    'If the ''contraction_reading'' were adopted, the constraint would shift towards a ''piton'' (atrophied threat) or even a ''mountain'' (physical impossibility), with significantly lower extractiveness and suppression. If the ''contingent_reachability_reading'' were adopted, the classification would be highly dynamic, shifting with technological change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Ambiguity in the fundamental nature of total war reachability, leading to different constraint classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(tota_tr_t1989, total_war_reachability_boundary__dropping_reading, theater_ratio, 1989, 0.25).
narrative_ontology:measurement(tota_tr_t2001, total_war_reachability_boundary__dropping_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(tota_tr_t2014, total_war_reachability_boundary__dropping_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__dropping_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.75).
narrative_ontology:measurement(tota_be_t1989, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1989, 0.55).
narrative_ontology:measurement(tota_be_t2001, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(tota_be_t2014, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2014, 0.48).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.85).
narrative_ontology:measurement(tota_su_t1989, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1989, 0.65).
narrative_ontology:measurement(tota_su_t2001, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(tota_su_t2014, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.1).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_proliferation_treaty).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, arms_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_reachability_boundary' kernel, which also includes the 'contraction_reading' and 'contingent_reachability_reading'. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
