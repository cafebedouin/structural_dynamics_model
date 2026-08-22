% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Nuclear Impossibility: Structural Contraction of War as Strategy
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   The structural contraction reading frames nuclear weapons as creating a
 *   physical impossibility: when both parties possess invulnerable
 *   second-strike arsenals, rational war between them ceases to exist as a
 *   strategy. War does not become more expensive or harder — it becomes
 *   unreachable. The constraint is not a human choice or institution; it is a
 *   mathematical fact about the payoff matrix. No party 'maintains' it; it
 *   persists because the physics of survivable arsenals persists. The reading
 *   asserts that large-scale interstate war between nuclear-armed powers
 *   dropped entirely from the reachable strategic set after mutual
 *   vulnerability was achieved. This distinguishes it sharply from the
 *   sibling readings: the rational_dropout_reading allows war to remain
 *   reachable but too costly; the credibility_paradox_reading focuses on the
 *   incoherence of the deterrent threat itself. The structural contraction
 *   reading is about the elimination of war as a strategic option, full stop.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: perceive the constraint as a physical limit — war is not a rational choice because victory is impossible
 *   - non_nuclear_states: operate under an umbrella constraint — their strategic freedom is bounded by the impossibility
 *   - military_strategists: must work around the constraint by constructing doctrines that deny it (limited war theory, escalation control) or by substituting proxy wars
 *   - analysts_external_to_defense_establishment: attest the structural contraction as an observable fact, independent of institutional defense interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.02).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility: Structural Contraction of War as Strategy").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '67030747-8940-450c-a7a1-36ddb1b226bd').
narrative_ontology:cs_kernel_codification('67030747-8940-450c-a7a1-36ddb1b226bd', formalized).
narrative_ontology:cs_authority_grounding('67030747-8940-450c-a7a1-36ddb1b226bd', expertise).
narrative_ontology:cs_interpretation_layer_present('67030747-8940-450c-a7a1-36ddb1b226bd').
narrative_ontology:cs_reading_relation('67030747-8940-450c-a7a1-36ddb1b226bd', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('67030747-8940-450c-a7a1-36ddb1b226bd', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('67030747-8940-450c-a7a1-36ddb1b226bd', foundational, mutual_annihilation_eliminates_victory).
narrative_ontology:cs_axiom_status(mutual_annihilation_eliminates_victory, holdable).
narrative_ontology:cs_axiom_grounding('67030747-8940-450c-a7a1-36ddb1b226bd', mutual_annihilation_eliminates_victory, empirically_contingent).
narrative_ontology:cs_axiom('67030747-8940-450c-a7a1-36ddb1b226bd', foundational, war_exits_reachable_strategy_set).
narrative_ontology:cs_axiom_status(war_exits_reachable_strategy_set, holdable).
narrative_ontology:cs_axiom_grounding('67030747-8940-450c-a7a1-36ddb1b226bd', war_exits_reachable_strategy_set, instrumental).
narrative_ontology:cs_reference_frame('67030747-8940-450c-a7a1-36ddb1b226bd', mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('67030747-8940-450c-a7a1-36ddb1b226bd', contemporary_2026, gap(stable, minor, true)).
narrative_ontology:cs_created_at('67030747-8940-450c-a7a1-36ddb1b226bd', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, humanity_as_collective).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The structural contraction reading treats 'humanity' not as an economic actor but as the referent of the physical constraint: the constraint is that rational war-fighting no longer exists as a reachable strategy for anyone, because both parties face mutual annihilation. This is not a beneficiary in the economic sense — it is not an agent collecting rents. It is the systemic outcome.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, humanity_as_collective, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(nuclear_impossibility_kernel__structural_contraction_reading, humanity_as_collective).

% Possess nuclear arsenals and confront the structural impossibility: direct war between them cannot yield victory because the cost is mutual annihilation. They may still choose war (irrationality, accident, miscalculation), but the choice is not rational in the strategic sense that military victory can be achieved. Their strategic doctrine must adapt to the fact that war is no longer a policy instrument.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_armed_states, observer,
    institutional, generational, analytical, global).

% Exist under the nuclear umbrella constraint: they cannot directly war against nuclear powers without triggering the mutual annihilation possibility, and they cannot fight each other at large scales without drawing nuclear powers into asymmetric intervention. Their strategic freedom is structurally bounded by the physics of the constraint.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, observer,
    organized, generational, analytical, global).

% Must plan for wars that the structural contraction reading declares impossible to win. They respond by constructing doctrine that denies the constraint (limited war theory, first-strike credibility, escalation ladders) or by substituting proxy wars and non-kinetic competition (cyber, economic). Their professional legitimacy rests on maintaining the illusion that strategic victory is possible.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, military_strategists, observer,
    powerful, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The structural contraction reading does not furnish a coordination function — it is not a coordination constraint at all. It is a physical/logical limit: if both parties possess survivable second-strike arsenals, the mathematical fact is that neither can achieve victory by war. The 'function' is not coordination but elimination: war drops out of the reachable strategic set entirely.
% TRANSFER_FUNCTION: No transfer: the structural contraction reading involves no movement of resources or benefits from one party to another. What is eliminated is the possibility of strategic victory itself. This distinguishes it from extraction constraints — nothing flows; the constraint operates by negation.
% ABSENT_VOICES: Voices that would dispute the physical constraint itself: those who believe first-strike can succeed, that escalation can be controlled sufficiently to achieve limited war objectives, that technological breakthrough (missile defense, hypersonics, AI-guided systems) can restore the possibility of victory. These voices are not structurally absent from the constraint — they exist in military planning and doctrine — but they are absent from the structural contraction reading's own frame, which takes the mutual-annihilation premise as fixed.
% DISAPPEARANCE_RATIONALE: If the structural contraction of war disappeared — if nuclear weapons were abolished and mutual annihilation were no longer guaranteed — the strategic landscape would reorganize: large-scale conventional war would again be rational, military planning would revert to victory-seeking doctrines, interstate competition would return to the form of territorial or resource war. The constraint's disappearance would not restore a previous state; it would enable a new one. But within the interval where the constraint is active, nothing external makes it disappear — it persists as a fact of physics and arsenal survivability.
% FOUNDING_PROBLEM: The founding problem was the achievement of mutual nuclear vulnerability: when both superpowers possessed invulnerable second-strike arsenals (submarine-based ICBMs, mobile launchers, redundant command-and-control), the mathematical fact emerged that neither could guarantee survival of a first strike. This was not a problem to be solved; it was the end state of arms competition.
% FOUNDING_PROBLEM_CORROBORATION: Independent nuclear strategists (Schelling, Brodie, Waltz, Sagan), physicists (Manhattan Project veterans), and declassified strategic analyses from both US and Soviet sources confirm that mutual vulnerability is the operational reality and that the elimination of war as a rational strategy followed from it. The constraint is corroborated by people outside the beneficiary frame — there is no beneficiary frame in this reading. The constraint is attested by technical analysts and game theorists who have no stake in its continuance; their corroboration is independent because the constraint creates no rents for them to defend.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is near-zero (0.05) because the constraint is not extractive in the economic sense — no party collects from it, no party bears a transfer. The 0.05 accounts for the minimal cost of maintaining strategic command-and-control and nuclear forces, which is necessary overhead for the constraint to persist. Suppression is near-zero (0.02) because the constraint does not suppress alternatives through coercion — alternatives are suppressed through mathematics. Once the payoff matrix is understood, no coercive force is required. Theater ratio is exactly zero because the constraint performs no performative function — war does not happen, not as a cover for something else. Accessibility_collapse is very high (0.95) because once the mutual-annihilation premise is accepted, no alternative to accepting the constraint exists — the strategic landscape is mathematically closed. Resistance is low (0.08) because the constraint meets almost no active resistance — parties may deny it rhetorically (limited-war theory, first-strike doctrine), but the physics does not require defeat of resistance. The measurement series are flat across the 64-year interval because the constraint's core property — mutual vulnerability and the impossibility of victory — has remained stable since the achievement of survivable second-strike forces.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality logic does not apply to this constraint in the standard way because the constraint is not an extraction mechanism. There are no 'payers' and 'beneficiaries' in the economic sense. The beneficiary list includes 'humanity_as_collective' with agent=false to indicate that the referent of the constraint is a systemic outcome, not an economic transfer. The constraint is a natural limit, and all parties are symmetrically positioned with respect to it: all nuclear-armed states face the same mathematical impossibility of victory.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this constraint. The founding problem (mutual vulnerability and the elimination of war as rational strategy) remains live — the constraint has not outlived its function. The physics of the constraint persists unchanged across the measurement interval. The constraint is not a degraded institution; it is a stable fact of the strategic landscape. There is no case where the constraint's mandate has become obsolete while the constraint persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_framing,
    'Is the structural contraction of war a fact of physics (mutual annihilation is guaranteed by the laws of nuclear physics) or a fact of human institutional choice (survival of nuclear arsenals is maintained by state policy and could be altered)?',
    'Trace the causal chain: does the constraint persist independently of any human choice to maintain it, or only because states choose not to disarm? If disarmament and arsenals reduction do not occur, is that because the physics forbids it or because politics prevents it?',
    'If the constraint is purely physical, it is a genuine mountain and persists regardless of human agency. If it depends on institutional choices (which states maintain arsenals, how command-and-control is organized), it is more accurately classified as an institutional Rope or Tangled Rope constraint. This affects downstream analysis of whether the constraint can be ''fixed'' or ''exited.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_framing, conceptual, 'Whether structural contraction is a fact of physics or institutional maintenance').

omega_variable(
    emergence_of_impossibility_vs_cost_escalation,
    'Does mutual annihilation create an absolute impossibility of strategic victory (the M-set contracts to zero), or does it create an extreme cost escalation such that victory remains reachable but prohibitively expensive?',
    'Game-theoretic analysis: does any strategy exist that yields positive payoff for both parties (or for the aggressor) when mutual annihilation is factored in? If no such strategy exists mathematically, the impossibility is absolute. If a strategy exists but yields negative expected value due to costs, the constraint is cost escalation, not impossibility.',
    'An absolute impossibility supports the structural contraction reading. An extreme cost escalation (where victory is reachable but irrational) would support the rational_dropout_reading instead. This determines the constraint''s type and its relationship to the sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_of_impossibility_vs_cost_escalation, empirical, 'Whether nuclear weapons create absolute impossibility or prohibitive cost').

omega_variable(
    proxy_war_as_continuation_vs_substitution,
    'Are proxy wars (wars between nuclear-armed powers fought through non-nuclear intermediaries) a continuation of direct strategic competition within the structural contraction, or a substitution for the direct war that the contraction forbids?',
    'Historical analysis of proxy conflicts: do they pursue the same strategic objectives as would direct war, or do they pursue separate regional/ideological objectives? Do parties treat proxy wars as equivalent to direct wars in terms of strategic payoff, or as inferior substitutes?',
    'If proxy wars are continuation, the contraction does not fully eliminate war as strategy — it merely redirects it through intermediaries. If proxy wars are substitution, the contraction stands: direct war between nuclear powers is eliminated, and proxy wars are a different phenomenon. This affects the interpretation of whether the constraint is truly eliminating war or merely compartmentalizing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_as_continuation_vs_substitution, empirical, 'Whether proxy wars represent continuation or substitution of war').

omega_variable(
    constraint_dependencies_on_technological_change,
    'Does the structural contraction depend on current arsenals and command-and-control technologies remaining stable? Would developments in missile defense, hypersonic systems, AI-guided countermeasures, or nuclear-hardened space-based systems alter the mutual-annihilation guarantee?',
    'Technical analysis: does a technological breakthrough exist that would allow one party to defend against a second strike, or to deliver a disarming first strike before retaliation? If such a breakthrough is possible, at what timeline and what probability?',
    'If the contraction depends on technological stability, a major breakthrough could restore war to the reachable strategic set and reclassify the constraint to a temporary state. If the contraction is robust to foreseeable technological change, it is more stable. This affects confidence in the mountain classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constraint_dependencies_on_technological_change, empirical, 'Technological robustness of the mutual-annihilation guarantee').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1962, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1962, observed).
narrative_ontology:measurement(nucl_tr_t1972, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1972, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1972, observed).
narrative_ontology:measurement(nucl_tr_t1982, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1982, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1982, observed).
narrative_ontology:measurement(nucl_tr_t1992, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1992, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1992, observed).
narrative_ontology:measurement(nucl_tr_t2002, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2002, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t2002, observed).
narrative_ontology:measurement(nucl_tr_t2026, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2026, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.05).
narrative_ontology:measurement_basis(nucl_be_t1962, observed).
narrative_ontology:measurement(nucl_be_t1972, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1972, 0.05).
narrative_ontology:measurement_basis(nucl_be_t1972, observed).
narrative_ontology:measurement(nucl_be_t1982, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1982, 0.05).
narrative_ontology:measurement_basis(nucl_be_t1982, observed).
narrative_ontology:measurement(nucl_be_t1992, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1992, 0.05).
narrative_ontology:measurement_basis(nucl_be_t1992, observed).
narrative_ontology:measurement(nucl_be_t2002, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2002, 0.05).
narrative_ontology:measurement_basis(nucl_be_t2002, observed).
narrative_ontology:measurement(nucl_be_t2026, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2026, 0.05).
narrative_ontology:measurement_basis(nucl_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1962, 0.02).
narrative_ontology:measurement_basis(nucl_su_t1962, observed).
narrative_ontology:measurement(nucl_su_t1972, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1972, 0.02).
narrative_ontology:measurement_basis(nucl_su_t1972, observed).
narrative_ontology:measurement(nucl_su_t1982, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1982, 0.02).
narrative_ontology:measurement_basis(nucl_su_t1982, observed).
narrative_ontology:measurement(nucl_su_t1992, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1992, 0.02).
narrative_ontology:measurement_basis(nucl_su_t1992, observed).
narrative_ontology:measurement(nucl_su_t2002, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2002, 0.02).
narrative_ontology:measurement_basis(nucl_su_t2002, observed).
narrative_ontology:measurement(nucl_su_t2026, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2026, 0.02).
narrative_ontology:measurement_basis(nucl_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__structural_contraction_reading, 0.02).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the nuclear_impossibility_kernel, a contested kernel in strategic studies. The structural_contraction_reading claims war is eliminated entirely from the reachable strategic set. The rational_dropout_reading (sibling) claims war remains reachable but too costly. The credibility_paradox_reading (sibling) claims deterrence itself is incoherent. Each reading instantiates a distinct constraint with different ε values, victim/beneficiary structures, and type classifications. They are linked via network.affects_constraints because they share a common domain (nuclear deterrence) and compete for interpretive authority over the same kernel (what nuclear weapons did to strategic rationality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
