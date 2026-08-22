% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rationality Dropout: War Structurally Unwinnable
 *   domain: strategic/geopolitical/military
 *
 * SUMMARY:
 *   This constraint is ONE READING of the nuclear_impossibility_kernel. The
 *   kernel is the persisting commitment to the atomic age—the fact that
 *   nuclear weapons exist and cannot be un-invented. Different readings
 *   interpret what that kernel implies for rational action. The
 *   rational_dropout_reading asserts that nuclear weapons created a
 *   cost-benefit constraint: war remains in the logically possible set (it
 *   could be fought), but rational actors dropout from pursuing it because
 *   victory is not achievable at any acceptable cost. Victory could
 *   theoretically exist (territorial gain, political outcome achieved); it is
 *   the cost of achieving it (civilization-scale destruction, mutual
 *   retaliation) that makes the pursuit irrational. This reading differs from
 *   the structural_contraction reading (which claims victory is physically
 *   impossible, not merely irrational) and from the credibility_paradox
 *   reading (which claims the threat to use nuclear weapons is inherently
 *   self-defeating). All three readings operate on the same kernel (nuclear
 *   weapons exist and create mutual vulnerability), but each draws a
 *   different structural conclusion about what parties rationally do.
 *
 * KEY AGENTS:
 *   - Nuclear-armed state A (observer): Faces the rational-dropout constraint symmetrically with State B; both recognize nuclear war is unwinnable.
 *   - Nuclear-armed state B (observer): Subject to the identical constraint; neither can rationally pursue nuclear victory.
 *   - Strategic planning apparatus A (observer): Designs military strategy within the bounds the constraint establishes; nuclear escalation is off the table.
 *   - Strategic planning apparatus B (observer): Operates under the same rational limitation; both apparatuses coordinate de facto by accepting the constraint.
 *   - Human civilization (analytical beneficiary): The only entity that 'benefits' from the constraint, in the sense that civilization survives because parties accept the rational constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.0).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.0).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rationality Dropout: War Structurally Unwinnable").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic/geopolitical/military").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e').
narrative_ontology:cs_kernel_codification('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', distributed).
narrative_ontology:cs_authority_grounding('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', distributed).
narrative_ontology:cs_reading_relation('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', foundational, victory_unachievable_at_rational_cost).
narrative_ontology:cs_axiom_status(victory_unachievable_at_rational_cost, holdable).
narrative_ontology:cs_axiom_grounding('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', victory_unachievable_at_rational_cost, instrumental).
narrative_ontology:cs_axiom('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', secondary, rational_agents_dropout_from_nuclear_pursuit).
narrative_ontology:cs_axiom_status(rational_agents_dropout_from_nuclear_pursuit, holdable).
narrative_ontology:cs_axiom_grounding('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', rational_agents_dropout_from_nuclear_pursuit, empirically_contingent).
narrative_ontology:cs_reference_frame('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', mutual_vulnerability_rational_dropout).
narrative_ontology:cs_drift_state('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', contemporary_2025, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f1e9bf84-347f-4e9b-89e7-1f85c5c8bc0e', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, human_civilization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The constraint that large-scale nuclear exchange is rationally indefensible—victory is unachievable at any acceptable cost—applies uniformly to all actors. No actor benefits from the constraint in an extractive sense; all are equally subject to the rational mathematics it encodes. The constraint is a universal structural fact, not a mechanism one party uses against another.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, human_civilization, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(nuclear_impossibility_kernel__rational_dropout_reading, human_civilization).

% Possesses nuclear weapons. Recognizes that nuclear war cannot be 'won' in any meaningful sense—mutual destruction ensures that even a successful first strike results in unacceptable retaliation. The constraint operates symmetrically: both sides understand the rational calculation equally. Strategic posture is built around acknowledging this constraint rather than around winning a nuclear war.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_armed_state_a, observer,
    institutional, generational, trapped, global).

% Also possesses nuclear weapons. Subject to the identical rational-choice constraint. Can launch a first strike but cannot achieve victory—the cost calculus is identical to State A's. Both recognize that escalation beyond conventional limits is irrational, even when conventional war might be winnable.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_armed_state_b, observer,
    institutional, generational, trapped, global).

% Designs military strategy under the constraint that nuclear use is off the table as a rational choice. Plans around conventional deterrence, escalation control, and avoiding triggers that would force nuclear consideration. The constraint shapes the space of strategically coherent actions.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_planning_apparatus_a, observer,
    institutional, generational, constrained, national).

% Operates under the same rational constraint. Both planning apparatuses face an identical structural fact: nuclear war is not a policy option because it is not a winnable outcome. This shared constraint produces a de facto coordination: neither needs to threaten nuclear use because both know neither can rationally execute it.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_planning_apparatus_b, observer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rational-choice constraint does not coordinate action per se—it eliminates a class of action (nuclear war) from the strategic calculus of all parties symmetrically. The elimination is automatic from the cost-benefit mathematics: victory is not achievable, so rational agents do not pursue it. All parties are forced into the same conclusion independently.
% TRANSFER_FUNCTION: No transfer occurs. The constraint is not extractive—it does not move resources from one party to another. It is a universal constraint on all parties equally. The 'benefit' is not to any particular agent but to the human species: the constraint keeps large-scale nuclear war in the logically possible set but removes it from the rational-action set.
% ABSENT_VOICES: No voices are absent from this constraint. All nuclear-armed states and their strategic planners experience the same rational limitation. The constraint is not contested by any actor in the system—each independently arrives at the same conclusion: nuclear war is not winnable.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—i.e., if nuclear war could suddenly become winnable through a technological shift (e.g., perfect missile defense)—the world would reorganize dramatically: nuclear deterrence would collapse, first-strike strategies would become rational, and the international order would destabilize. But the constraint itself is not a constructed arrangement that could be 'removed.' It emerges from physical law (mutual vulnerability) and the rational logic of cost-benefit analysis. No actor could unilaterally remove it; it persists as long as second-strike nuclear capability exists.
% FOUNDING_PROBLEM: The founding problem was mutual vulnerability: once both sides possess thermonuclear weapons with no defensible countermeasure, the classical logic of military victory becomes logically incoherent. The problem emerged in ~1949-1962 as the Soviet Union developed survivable second-strike capability. The constraint solved the problem of how to prevent mutual destruction: by removing the pursuit of nuclear victory from the rational-action set.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (mutual vulnerability and the irrationality of nuclear war) is affirmed by the entire field of nuclear strategy scholarship (Schelling, Brodie, Jervis, Sagan, Waltz) and by declassified strategic documents from both superpowers recognizing that 'winning' a nuclear war is impossible. The US military's own doctrine (assured mutual destruction) and Russia's doctrine (no victor scenario) both encode the constraint. No authoritative voice outside the benefiting parties contests that nuclear war is rationally indefensible.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_unchanged).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because no party extracts from another via this constraint. The constraint is a universal fact that applies to all nuclear-armed actors identically. Suppression is zero because the constraint does not require coercion to maintain—each party arrives at the rational conclusion independently. Theater is zero because there is nothing performed; the constraint is continuously operative in the strategic calculus. Accessibility_collapse is high (0.92) because once the rational mathematics of mutual vulnerability is understood, there is no accessible alternative path to nuclear victory. The constraint is as close to a natural law as strategic facts get: given mutual nuclear vulnerability, no rational actor pursues nuclear war. Resistance is near-zero (0.08) because no party resists the constraint—all parties accept the rationality of the dropout. The minuscule non-zero value reflects the tail risk that some actor might reject the rationality assumption through ideological commitment or miscalculation, creating transient resistance (e.g., nuclear saber-rattling as political theater). The measurement series is flat across the interval (1962–2025) because the constraint itself is stable: as long as mutual vulnerability persists and actors remain rational, the constraint holds. No temporal drift is observed.
 *
 * PERSPECTIVAL GAP:
 *   Both nuclear-armed states perceive the constraint identically: nuclear war is irrational. There is no perspectival gap because the rational calculation is the same from all seats. State A's planners and State B's planners reach the same conclusion: escalation to nuclear use loses the calculation. This absence of perspectival gap is unusual for an extractive constraint and is a mark that the constraint is genuinely natural rather than constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no directionality in the traditional sense because it creates no extractive relationship. All parties are symmetrically constrained. The beneficiary is 'human civilization' (analytically), not any actor. No agent sits at d=1.0 (full target) because no agent bears costs imposed by another through this constraint. All agents sit at d=0.5 (symmetric: constrained equally by the same physics and rational logic). There is no asymmetry to model; the constraint is democratic in its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has no mandatrophy because its founding problem (mutual vulnerability creates unwinnable war) remains live and is not contested. The problem was to prevent escalation to mutual destruction; the constraint solves it by making nuclear war rationally indefensible. As long as mutual vulnerability exists, the constraint's mandate persists. There is no mandate obsolescence or substitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_strategic_doctrine,
    'Is the constraint that nuclear war is unwinnable a natural law emerging from physics and mutual vulnerability, or a strategic doctrine held by certain parties and potentially overridable by technological change (e.g., effective ABM or space-based defense)?',
    'Technological development that breaks mutual vulnerability (e.g., near-perfect ballistic missile defense, nuclear-powered space interceptors). If such a technology emerges and is deployed, the rational-choice constraint would shift: first-strike strategies would become mathematically defensible again.',
    'If the constraint is purely doctrinal and overridable, it is a snare or tangled rope masquerading as a mountain—parties agree to accept it, but the agreement is contingent on mutual technological limitation. If it is a genuine natural law (given current and plausible future physics), it is a true mountain. This distinction determines whether deterrence is stable or whether technological pressure could collapse it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_strategic_doctrine, empirical, 'Whether the unwinnable-war constraint is physically fundamental or technologically contingent.').

omega_variable(
    rationality_assumption_robustness,
    'Does the constraint depend on all parties being rational actors, or does it persist even if some actors behave irrationally (suicidally, ideologically, by miscalculation)?',
    'Historical examination of near-miss incidents (Cuban Missile Crisis, 1983 Soviet false alarm, Kargil crisis) to measure whether irrational actors or miscalculations have been confined by structural incentives or only by chance. Forward: observation of whether emerging nuclear-armed states (North Korea, Iran potential) accept the rational-dropout constraint or reject it.',
    'If the constraint is robust to irrationality, it is a mountain: even an irrational actor finds nuclear war unwinnable because the other side retaliates regardless of intent. If the constraint requires rationality (all parties must calculate and accept the cost-benefit math), then it is a snare: it works only as long as parties voluntarily accept the reasoning. Irrationality on either side could trigger escalation the constraint was supposed to prevent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_assumption_robustness, empirical, 'Whether the rational-dropout constraint is robust to actor irrationality.').

omega_variable(
    reading_vs_structural_contraction,
    'Is this reading (victory remains structurally possible but is rationally abandoned) coherently distinct from the structural_contraction_reading (no rational path to victory exists because mutual annihilation is guaranteed), or are they the same constraint viewed through different frames?',
    'Examine whether ''victory is impossible'' (structural reading) and ''victory is possible but irrational'' (this reading) produce different strategic predictions or policy recommendations. If they differ—e.g., in how they counsel planning for low-probability high-impact scenarios, or in how they frame first-strike vulnerability—then they are distinct constraints. If they produce identical strategic postures, the reading distinction is merely narrative.',
    'If the readings are distinct, each has its own ε and its own classification. This reading (rational dropout) has ε=0.0 because no extraction occurs—the constraint is universal. The structural reading would have higher ε if it framed vulnerability as an asymmetric risk imposed on one party. If they are the same constraint, one of the two readings is redundant and should be collapsed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_structural_contraction, conceptual, 'Whether the rational-dropout and structural-contraction readings are genuinely distinct constraints or the same constraint under different narrative frames.').

omega_variable(
    false_summit_candidate,
    'Is the constraint that nuclear war is irrational a genuine natural law of physics and rational choice, or does it benefit human civilization as a beneficiary in a way that conceals constructed institutional maintenance?',
    'Historical and counterfactual analysis: Did the constraint emerge naturally from the physics and mutual vulnerability (mountain), or did strategic elites construct it as a doctrine to prevent escalation they feared? Has institutional maintenance (doctrinal training, deterrence theory, strategic planning infrastructure) been necessary to keep the constraint stable, or does it persist without institutional effort?',
    'A true mountain persists because it is logically/physically inevitable. A false summit persists because beneficiaries (human civilization, avoiding annihilation) want it maintained and institutional actors enforce the doctrine. If maintenance is required, the constraint may be vulnerable to erosion or to parties that reject the rationality assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate, empirical, 'Whether the rational-dropout constraint is a natural law or a beneficiary-maintained doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1962, observed).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1980, observed).
narrative_ontology:measurement(nucl_tr_t2000, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t2000, observed).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2025, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.0).
narrative_ontology:measurement_basis(nucl_be_t1962, observed).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1980, 0.0).
narrative_ontology:measurement_basis(nucl_be_t1980, observed).
narrative_ontology:measurement(nucl_be_t2000, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(nucl_be_t2000, observed).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2025, 0.0).
narrative_ontology:measurement_basis(nucl_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nuclear_impossibility_kernel__rational_dropout_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__rational_dropout_reading, 0.0).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the nuclear_impossibility_kernel. The kernel represents the persisting atomic commitment: nuclear weapons exist and cannot be un-invented. Three sibling stories instantiate three distinct readings of what that kernel implies for rational action. The rational_dropout_reading claims that victory remains logically possible but is rationally abandoned due to cost-benefit mathematics. The structural_contraction_reading claims that no path to victory exists due to physical law (mutual annihilation is guaranteed). The credibility_paradox_reading claims that the threat to use nuclear weapons is self-defeating (no credible commitment to use them exists). All three readings operate on the same referent (the atomic age and mutual vulnerability), but each draws a different structural conclusion about strategic rationality. The ε values differ: rational_dropout has ε=0.0 (no extraction, universal constraint); the other readings may carry higher ε if they frame nuclear vulnerability as asymmetric risk. The readings are linked via network.affects_constraints to show the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
