% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rational-Dropout Constraint (Rational Choice Reading)
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   This constraint story captures the rational_dropout_reading of the
 *   nuclear_impossibility_kernel: nuclear weapons impose a rational-choice
 *   constraint where victory remains structurally possible (the reachable set
 *   includes war) but the expected costs exceed any conceivable benefit, so
 *   rational actors drop war from active consideration. The kernel is
 *   contested — sibling readings are credibility_paradox_reading (the threat
 *   to use is inherently incredible) and structural_contraction_reading
 *   (mutual annihilation makes victory physically impossible). This reading
 *   treats the constraint as a coordination mechanism (preventing great-power
 *   war) with an extraction component (risk transfer to non-nuclear states
 *   and future generations). It requires active enforcement (nonproliferation
 *   regimes, modernization, declaratory policy) and shows moderate extraction
 *   that has fluctuated with arms races and détente.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.12).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.35).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational-Dropout Constraint (Rational Choice Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'dda7d765-5456-47d5-a08c-83d2f07d726a').
narrative_ontology:cs_kernel_codification('dda7d765-5456-47d5-a08c-83d2f07d726a', fixed_text).
narrative_ontology:cs_authority_grounding('dda7d765-5456-47d5-a08c-83d2f07d726a', lineage).
narrative_ontology:cs_interpretation_layer_present('dda7d765-5456-47d5-a08c-83d2f07d726a').
narrative_ontology:cs_reading_relation('dda7d765-5456-47d5-a08c-83d2f07d726a', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_reading_relation('dda7d765-5456-47d5-a08c-83d2f07d726a', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('dda7d765-5456-47d5-a08c-83d2f07d726a', foundational, war_is_calculable_cost_benefit_decision).
narrative_ontology:cs_axiom_status(war_is_calculable_cost_benefit_decision, holdable).
narrative_ontology:cs_axiom_grounding('dda7d765-5456-47d5-a08c-83d2f07d726a', war_is_calculable_cost_benefit_decision, instrumental).
narrative_ontology:cs_axiom('dda7d765-5456-47d5-a08c-83d2f07d726a', foundational, rational_actor_drops_dominated_strategies).
narrative_ontology:cs_axiom_status(rational_actor_drops_dominated_strategies, holdable).
narrative_ontology:cs_axiom_grounding('dda7d765-5456-47d5-a08c-83d2f07d726a', rational_actor_drops_dominated_strategies, instrumental).
narrative_ontology:cs_reference_frame('dda7d765-5456-47d5-a08c-83d2f07d726a', post_hiroshima_rational_deterrence_framework).
narrative_ontology:cs_drift_state('dda7d765-5456-47d5-a08c-83d2f07d726a', contemporary_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dda7d765-5456-47d5-a08c-83d2f07d726a', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, strategic_analysts).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, allied_populations_under_extended_deterrence).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, future_generations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, disarmament_advocates).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, mutually_assured_destruction_logic).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, cost_benefit_war_avoidance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and set declaratory policy; they maintain the constraint by investing in modernization, signaling resolve, and suppressing proliferation. They benefit from the constraint's stabilization of great-power relations but bear the costs of arsenal maintenance and the risk of accidental or unauthorized use. Exit would mean unilateral disarmament — structurally possible but politically existential.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_armed_states, agenda_setter,
    institutional, generational, constrained, global).

% Live under the nuclear umbrella without possessing the deterrent; they pay the opportunity cost of forgoing independent deterrent capabilities and accept extended deterrence commitments that may not be credible. Their exit options are proliferation (blocked by NPT and supplier regimes) or reliance on security guarantees that could fail.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, global).

% Build careers, institutions, and intellectual capital around rational deterrence frameworks. The constraint's persistence validates their analytical tools (game theory, cost-benefit models) and sustains demand for their expertise. They can exit to other analytical domains but lose the specific prestige and policy access of nuclear strategy.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_analysts, beneficiary,
    organized, biographical, mobile, global).

% Receive security guarantees from nuclear patrons; their national identities and threat perceptions are fused with the protector relationship. Exit would mean redefining national security from first principles — a cognitive and political rupture few publics or elites can entertain. They bear residual risk of being the battlefield for a nuclear exchange they cannot control.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, allied_populations_under_extended_deterrence, beneficiary,
    powerless, generational, identity_locked, regional).

% Inherit the accumulated risk of arsenals, waste, and the possibility of deterrence failure without having consented to the arrangement. They cannot exit the constraint — they are born into it. The constraint's rational-choice framing treats their interests as a discounted variable, not a veto.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Argue that the rational-choice frame itself is the trap — that modeling nuclear war as a calculable cost-benefit problem normalizes the unacceptable. They are structurally excluded from the constraint's internal logic because the constraint defines rationality in a way that precludes their objection. Their exit is reframing the debate, which the constraint's institutional architecture resists.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, disarmament_advocates, excluded,
    moderate, biographical, constrained, global).

% Observes the full structure: the rational-choice reading is one of three coherent framings of the same kernel; it computes the constraint's operation without committing to its normative adequacy.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates great-power behavior by making large-scale war between nuclear-armed states irrational in expected-value terms; provides a stable framework for crisis management, arms control, and alliance politics.
% TRANSFER_FUNCTION: Transfers the risk of existential catastrophe onto non-nuclear states and future generations while concentrating the security benefits (deterrence credibility, alliance cohesion) on nuclear-armed states and their allies. Transfers analytical authority to rational-choice frameworks that marginalize moral and existential objections.
% ABSENT_VOICES: Populations of the Global South who bear disproportionate fallout and climatic risks from any nuclear exchange but have no seat in deterrence decision-making; indigenous communities affected by testing and uranium extraction; the dead of Hiroshima and Nagasaki whose experience is cited but not consulted.
% DISAPPEARANCE_RATIONALE: If the rational-choice constraint vanished — i.e., if leaders stopped treating nuclear war as a cost-benefit calculation — the institutional architecture of deterrence (arsenals, doctrines, alliances, verification regimes) would lose its legitimating logic. The world would not revert to pre-nuclear war patterns; it would enter a contested transition toward either structural contraction (physical impossibility framing) or credibility paradox (threat incredibility framing) as the new organizing logic.
% FOUNDING_PROBLEM: How to prevent great-power war after 1945 when the destructive capacity of new weapons made total war potentially species-ending. The rational-choice reading was built to solve this by reframing war as a calculable decision problem where the rational actor chooses not to fight.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing great-power war) is attested as live by nuclear-armed states and strategic analysts (the benefiting parties). It is attested as substantially solved but morphing into a new problem (deterrence stability vs. disarmament obligation) by arms control diplomats and legal scholars outside the beneficiary set — e.g., the 1996 ICJ Advisory Opinion on the Legality of Nuclear Weapons and the 2017 Treaty on the Prohibition of Nuclear Weapons negotiations. No consensus exists; the corroboration is split along the beneficiary/excluded line.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).
:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.12) is low but nonzero: the constraint coordinates by making war irrational, but it extracts by concentrating the residual risk on those who did not choose the arrangement. Suppression (0.35) reflects the active maintenance required — nonproliferation, extended deterrence credibility, arsenal modernization — not passive acceptance. Theater (0.18) captures the performative aspects of nuclear signaling (exercises, rhetoric, 'modernization' programs that sustain industrial bases more than deterrence). Accessibility collapse (0.68) is high: once the rational-choice frame is internalized, alternatives (disarmament, no-first-use, delegitimization) appear structurally irrational. Resistance (0.42) is moderate: disarmament movements, TPNW, and credibility critiques persist but have not shifted the constraint's core logic.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (nuclear-armed states), the constraint appears as a successful coordination mechanism — the 'long peace' is evidence of function. From the payer seats (non-nuclear states, future generations), it appears as risk imposition without consent. From the excluded seat (disarmament advocates), the constraint's rationality axiom is the extraction mechanism itself. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the author's judgment that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states are agenda_setters with constrained exit (d ~0.3): they benefit from coordination but bear maintenance costs and residual risk. Non-nuclear states are payers with constrained exit (d ~0.7): they absorb risk without the deterrent asset. Strategic analysts are beneficiaries with mobile exit (d ~0.15): they gain analytical authority. Allied populations are beneficiaries but identity_locked (d ~0.25): they gain security but cannot cognitively exit the protector relationship. Future generations are trapped payers (d ~0.95): they inherit all risk with zero voice. Disarmament advocates are excluded (d not computed): the constraint's rationality definition excludes their moral frame. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power war) is contested as live or solved. If solved, the constraint persists as mandatrophy — the coordination function has atrophied but the extraction (risk transfer, analytical capture) continues. The rational-choice frame itself may be the mandatrophy vehicle: it presents the constraint as a permanent feature of rationality rather than a contingent political arrangement, preventing sunset. The theater ratio rise in the 1980s and 2020s correlates with arms racing that serves industrial and bureaucratic interests more than marginal deterrence gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_framing_underdetermination,
    'Does the rational_dropout_reading accurately capture the kernel''s constraint, or does it smuggle in a normative theory of rationality that excludes the other readings?',
    'Compare the reachable-set topology implied by each reading: rational_dropout says war is reachable but dominated; structural_contraction says war is unreachable; credibility_paradox says the threat node is unstable. Empirical test: do leaders in crises act as if war is reachable (rational_dropout) or do they treat the threat as incredible (credibility_paradox)?',
    'If rational_dropout is a framing choice rather than a structural fact, the constraint''s claimed coordination function is a projection of rational-choice theory onto a kernel that structurally supports multiple readings. The extraction component (risk transfer) would then be theorist-enabled rather than arrangement-inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the three readings are structurally distinct constraints or theory-laden framings of one constraint.').

omega_variable(
    extraction_referent_stability,
    'Is the extraction measured here (risk transfer to non-nuclear states and future generations) invariant under the reading''s own lights, or does it shift if the referent arrangement is specified differently?',
    'Apply the ε-invariance test: hold the reading''s structural data fixed and ask whether a different observable (e.g., ''probability of use per crisis'' vs. ''expected value of war'') changes ε. If yes, the reading conflates multiple constraints.',
    'If ε shifts with observable choice, this story violates the ε-invariance principle and should be decomposed. The current ε=0.12 assumes the referent is ''the standing arrangement of nuclear deterrence as rational actors experience it.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_referent_stability, empirical, 'ε-invariance check for the rational_dropout_reading''s extraction metric.').

omega_variable(
    mandatrophy_timing,
    'When did the founding problem (preventing great-power war) cease to be the primary function, if it has? Is the constraint now maintained primarily by the extraction it enables (bureaucratic, industrial, analytical)?',
    'Trace the theater_ratio and suppression_requirement trajectories against great-power war frequency and arms control milestones. If theater/suppression rise while coordination need (measured by crisis frequency and severity) falls, mandatrophy is indicated.',
    'If mandatrophy is confirmed, the constraint''s claimed_type should shift toward piton or snare despite the coordination origin story. The current tangled_rope claim assumes both functions are live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_timing, empirical, 'Whether the constraint has undergone mandatrophy from coordination to extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement(nucl_tr_t1985, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1991, 0.18).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement(nucl_be_t1985, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1991, 0.14).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2010, 0.11).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.45).
narrative_ontology:measurement(nucl_su_t1985, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1991, 0.3).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__rational_dropout_reading, 0.08).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_nonproliferation_regime).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_architecture).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, arms_control_verification_systems).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the nuclear_impossibility_kernel. The credibility_paradox_reading treats the threat's incredibility as the constraint; the structural_contraction_reading treats physical annihilation as the constraint. This reading treats rational cost-benefit calculation as the constraint. All three share the same kernel but instantiate different constraints with different ε, beneficiaries, and victims. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, institutional, 0.3).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, powerless, 0.95).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
