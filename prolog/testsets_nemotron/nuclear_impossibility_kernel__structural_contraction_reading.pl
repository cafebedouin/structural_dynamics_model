% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Impossibility — Structural Contraction Reading
 *   domain: strategic/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   Nuclear weapons introduced a physical constraint on interstate war: above
 *   a threshold of destructive capacity, mutual annihilation becomes
 *   inevitable for any major power exchange. This reading treats that
 *   impossibility as a structural contraction of the reachable state-space —
 *   war in the classic Clausewitzian sense (a rational continuation of
 *   politics by other means) exits the set of available actions entirely.
 *   Proxy wars, limited conflicts, and coercive diplomacy are substitutions
 *   operating in the residual space, not continuations of the eliminated
 *   option. The constraint is Mountain-like because it derives from physics
 *   (energy release, fallout, climate effects) and game-theoretic logic (no
 *   winning terminal node in the payoff matrix), not from enforcement or
 *   institutional design. All state and non-state actors are beneficiaries in
 *   the negative sense — the constraint prevents a catastrophe that would
 *   harm everyone — but no actor extracts rents from its operation.
 *
 * KEY AGENTS:
 *   - major_nuclear_powers: Institutional beneficiaries (constrained but protected by the impossibility)
 *   - minor_nuclear_powers: Institutional beneficiaries (same structural position)
 *   - non_nuclear_states: Institutional beneficiaries (protected from major power war)
 *   - human_species: Ultimate beneficiary (survival preserved)
 *   - strategic_analysts: Observers (analytical seat, maps the contracted reachable set)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.02).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility — Structural Contraction Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic/international_relations/nuclear_deterrence").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '11644b84-9407-49e7-aa33-244d83221cf1').
narrative_ontology:cs_kernel_codification('11644b84-9407-49e7-aa33-244d83221cf1', implicit).
narrative_ontology:cs_authority_grounding('11644b84-9407-49e7-aa33-244d83221cf1', practice).
narrative_ontology:cs_interpretation_layer_present('11644b84-9407-49e7-aa33-244d83221cf1').
narrative_ontology:cs_reading_relation('11644b84-9407-49e7-aa33-244d83221cf1', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_reading_relation('11644b84-9407-49e7-aa33-244d83221cf1', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('11644b84-9407-49e7-aa33-244d83221cf1', foundational, war_physically_impossible_above_threshold).
narrative_ontology:cs_axiom_status(war_physically_impossible_above_threshold, holdable).
narrative_ontology:cs_axiom_grounding('11644b84-9407-49e7-aa33-244d83221cf1', war_physically_impossible_above_threshold, empirically_contingent).
narrative_ontology:cs_axiom('11644b84-9407-49e7-aa33-244d83221cf1', secondary, proxy_conflicts_are_substitution_not_continuation).
narrative_ontology:cs_axiom_status(proxy_conflicts_are_substitution_not_continuation, holdable).
narrative_ontology:cs_axiom_grounding('11644b84-9407-49e7-aa33-244d83221cf1', proxy_conflicts_are_substitution_not_continuation, conventional).
narrative_ontology:cs_reference_frame('11644b84-9407-49e7-aa33-244d83221cf1', pre_nuclear_unconstrained_war).
narrative_ontology:cs_drift_state('11644b84-9407-49e7-aa33-244d83221cf1', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11644b84-9407-49e7-aa33-244d83221cf1', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, all_state_actors).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, all_non_state_actors).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, human_species).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, major_nuclear_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, minor_nuclear_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, mutual_assured_destruction_inevitability).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, war_exit_reachable_set_contraction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess arsenals that make major war physically impossible. They cannot exit the constraint (no technology or policy can undo the physics), but they benefit from it — it prevents existential war. They perform doctrines and modernize forces (theater) but the underlying impossibility is unchanged regardless of their actions.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, major_nuclear_powers, beneficiary,
    institutional, generational, trapped, global).

% Possess smaller arsenals that create local impossibility zones. They benefit from the global constraint (no major power war) and their own regional deterrence. Like major powers, they are trapped in the constraint — cannot un-invent the physics — but are net beneficiaries.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, minor_nuclear_powers, beneficiary,
    institutional, generational, trapped, regional).

% Do not possess nuclear weapons but benefit from the impossibility of major power war. Their exit options are constrained — they cannot acquire nuclear weapons without triggering proliferation cascades, but they also cannot be targeted by major power nuclear war without triggering the impossibility that protects them.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, beneficiary,
    organized, generational, constrained, global).

% The ultimate beneficiary of the constraint. Nuclear impossibility prevents civilizational or species-level catastrophe. No exit exists — the species cannot leave the planetary system or un-invent the physics. Listed as non-agent for structural completeness; the constraint_vindicates mechanism captures this.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, human_species, beneficiary,
    analytical, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(nuclear_impossibility_kernel__structural_contraction_reading, human_species).

% Map the contracted reachable set. They neither collect nor pay; they observe and model. Their situation is cognitive: the constraint simplifies their analytical task (war is off the table) but complicates their explanatory task (must account for why war-like activities persist).
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great power war by making it physically impossible — solves the coordination problem of mutual restraint without requiring trust, enforcement, or communication. The physics enforces itself.
% TRANSFER_FUNCTION: Transfers nothing material. The constraint is negative: it removes an option (catastrophic war) from everyone's action set. No resources flow between parties. The 'gain' is the non-occurrence of a negative-sum outcome.
% ABSENT_VOICES: No voices are structurally absent — the constraint is universal and affects all humans. However, voices that *deny* the impossibility (advocates of limited nuclear war, counterforce theorists, disarmament skeptics who treat the constraint as policy-choice) are present in the discourse but argue against the constraint's reality. They are not excluded; they are dissenting from the physics.
% DISAPPEARANCE_RATIONALE: If the physical impossibility vanished (e.g., perfect missile defense, new physics, alien intervention), great power war would re-enter the reachable set. The entire post-1945 international order — built on the assumption that major war is unwinnable — would restructure. Proxy wars would become direct conflicts; deterrence architectures would collapse; the long peace would end.
% FOUNDING_PROBLEM: The constraint was not founded — it was discovered. The 'problem' it addresses (how to prevent great power war) was not solved by design; the physics of nuclear weapons made the problem insoluble by war. The founding_problem_status is therefore not applicable in the standard sense, but the kernel's genealogical question is: when did actors recognize the impossibility?
% FOUNDING_PROBLEM_CORROBORATION: The recognition of impossibility is contested: major powers' doctrinal documents (US Single Integrated Operational Plan, Soviet deep battle theory) continued to plan for war-fighting into the 1980s, treating the constraint as surmountable. Strategic analysts (Schelling, Kahn, Wohlstetter) debated whether the constraint was total or partial. No single corroborating source outside the analytical community attests to a moment of universal recognition — the recognition diffused unevenly and remains incomplete in doctrine.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because no party collects from the constraint's operation — it prevents a negative-sum outcome for all. Suppression is minimal (0.05) because the constraint does not require active enforcement to persist; the physics and logic are self-sustaining. Theater ratio (0.15) captures the performative maintenance of nuclear doctrines, posturing, and modernization programs that simulate continued relevance of war-fighting options the constraint has structurally eliminated. Accessibility collapse is near-total (0.98) — once the physics and logic are understood, no rational path to victory exists; alternatives (surrender, stalemate, proxy substitution) are the only remaining moves. Resistance is near-zero (0.02) because no actor can resist a physical impossibility; they can only deny, misrepresent, or ritualize it.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is minimal because the constraint is genuinely universal. The only meaningful gap is between actors who internalize the impossibility (accept war is off the table) and those who performatively deny it (maintain war-fighting doctrines, invest in counterforce capabilities). The latter experience higher theater_ratio from their seat because they must sustain the performance; the former experience it as a settled background fact. The engine computes this from the same structural data — the divergence emerges from different exit_options and time_horizon within the same power atom (institutional).
 *
 * DIRECTIONALITY LOGIC:
 *   All state actors are structural beneficiaries (d ≈ 0.0) — the constraint subsidizes their survival by eliminating a catastrophic option. No actor is a target or payer. The analytical observer seat (strategic theorists, historians) sits at d = 0.5 — symmetric, as they bear the cognitive cost of mapping the contracted space but collect no material transfer. Directionality derives entirely from the universal beneficiary structure: the impossibility protects everyone equally, and no exit is possible because the constraint is physics, not policy.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy applies — the constraint has no mandate that could atrophy. It is not a human arrangement with a founding purpose; it is a discovered physical/logical limit. The 'founding problem' (how to prevent great power war) was not solved by this constraint — the constraint *is* the reason the problem cannot be solved by war anymore. The arrangement persists because physics persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physics_vs_doctrine_boundary,
    'Where exactly does the physical impossibility end and the doctrinal performance begin? The 0.15 theater_ratio suggests non-zero performance, but is that performance sustaining the impossibility or obscuring it?',
    'Counterfactual analysis: if all performative doctrines (counterforce targeting, escalation ladders, limited nuclear war concepts) were abandoned overnight, would the physical impossibility remain intact? If yes, theater is pure obscurantism. If no, some performance is structurally necessary to maintain the credible threat that undergirds the impossibility (linking to credibility_paradox_reading).',
    'If theater is necessary for the impossibility to hold, the constraint is not a pure Mountain but a Mountain with a Rope component (credibility maintenance). This would shift claimed_type for the credibility_paradox_reading sibling and affect the reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physics_vs_doctrine_boundary, conceptual, 'Whether the performative layer is epiphenomenal or structurally necessary to the impossibility').

omega_variable(
    substitution_vs_continuation_ambiguity,
    'Are proxy wars and limited conflicts genuine continuations of war in a contracted space, or are they structurally distinct substitution activities that only resemble war?',
    'Compare the payoff matrices and terminal nodes of proxy conflicts vs. pre-nuclear great power wars. If proxy conflicts have fundamentally different win/loss conditions (no existential stakes, no mutual annihilation terminal), they are substitutions. If they preserve the same structural logic at lower intensity, they are continuations.',
    'If continuations, the M-set contraction is incomplete — war persists in a degraded form, making the constraint less than total Mountain. If substitutions, the contraction is total and the Mountain claim is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitution_vs_continuation_ambiguity, conceptual, 'Whether residual conflict forms are war-continuations or war-substitutions').

omega_variable(
    kernel_reading_boundary,
    'Does this reading (structural_contraction) logically foreclose the credibility_paradox_reading, or do they operate at different levels of description (physics vs. epistemology of threat)?',
    'Formal analysis: if war is physically impossible (this reading), does the credibility of the threat become a category error (threatening an impossible action)? If so, forecloses. If the paradox operates at the level of *perceived* credibility (adversary belief) independent of physical possibility, they coexist.',
    'Determines the reading_relation: forecloses vs. coexists_with. Affects whether the kernel has one Mountain constraint and two Tangled Rope constraints, or three distinct types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between physical impossibility and threat credibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement(nucl_tr_t1987, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1987, 0.14).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.01).
narrative_ontology:measurement(nucl_be_t1987, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1987, 0.02).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2024, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nuclear_impossibility_kernel__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_taboo_norm).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_architecture).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_regime).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the nuclear_impossibility_kernel. The kernel's colloquial label ('nuclear deterrence') conflates: (1) a physical impossibility (this reading, Mountain), (2) a credibility paradox (credibility_paradox_reading, Tangled Rope), and (3) a rational-cost barrier (rational_dropout_reading, Rope/Snare hybrid). Each has distinct ε, beneficiaries/victims, and enforcement structure. They are linked via affects_constraints. The structural_contraction reading is upstream — the physical impossibility creates the conditions for the paradox and the cost calculation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
