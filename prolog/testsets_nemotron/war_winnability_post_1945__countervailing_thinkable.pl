% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Counterforce Winnability Under Nuclear Constraint
 *   domain: strategic/nuclear/political
 *
 * SUMMARY:
 *   This constraint story captures the countervailing_thinkable reading of
 *   the war_winnability_post_1945 kernel: the claim that nuclear weapons
 *   constrain but do not eliminate the possibility of limited victory through
 *   counterforce targeting. The constraint emerged from early Cold War
 *   efforts to preserve war's political utility (massive retaliation,
 *   flexible response, countervailing strategy) and persists in modern
 *   damage-limitation doctrines, prompt global strike programs, and low-yield
 *   warhead development. It operates as a tangled rope: genuine coordination
 *   (crisis signaling grammar, intra-war deterrence, termination bargaining)
 *   coexists with asymmetric extraction (risk transfer to non-participants,
 *   arms control undermining, institutional self-preservation). The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (coordination + extraction acknowledged) while the authored metrics
 *   describe substantial extraction (0.68) and active suppression (0.72) —
 *   the engine measures the divergence from a pure coordination reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.72).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Counterforce Winnability Under Nuclear Constraint").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic/nuclear/political").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2').
narrative_ontology:cs_kernel_codification('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', formalized).
narrative_ontology:cs_authority_grounding('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', extraction).
narrative_ontology:cs_interpretation_layer_present('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2').
narrative_ontology:cs_reading_relation('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', war_winnability_post_1945__deterrence_unthinkable, forecloses).
narrative_ontology:cs_reading_relation('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', foundational, counterforce_victory_achievable).
narrative_ontology:cs_axiom_status(counterforce_victory_achievable, holdable).
narrative_ontology:cs_axiom_grounding('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', counterforce_victory_achievable, instrumental).
narrative_ontology:cs_axiom('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', foundational, escalation_control_feasible).
narrative_ontology:cs_axiom_status(escalation_control_feasible, holdable).
narrative_ontology:cs_axiom_grounding('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', escalation_control_feasible, instrumental).
narrative_ontology:cs_reference_frame('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', bipolar_counterforce_parity).
narrative_ontology:cs_drift_state('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', post_cold_war_multipolar, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6812d2b5-e25f-46c3-a5ad-78c6e3b65bd2', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_command_structures).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_target_sets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives sustained procurement budgets, mission justification, and institutional relevance from ongoing counterforce planning and modernization programs. Shapes requirements and threat assessments that validate continued investment. Can pivot to adjacent domains (conventional prompt global strike, missile defense) if nuclear mission contracts.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, agenda_setter).

% Maintains operational relevance, career structures, and bureaucratic authority through continuous war planning for winnable nuclear scenarios. Professional identity fused to the mission of 'prevailing' in nuclear conflict. Exit requires abandoning the core professional self-concept and the institutional logic that justifies the command's existence.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_command_structures, agenda_setter,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, strategic_command_structures, beneficiary).

% Treaty frameworks (START, INF, NPT review cycles) are undermined when one party's doctrine assumes winnability — verification becomes asymmetric, trust erodes, and negotiated reductions stall because counterforce requirements drive force structure in directions treaties constrain. Cannot easily exit the diplomatic arena but operates from structural weakness when the counterparty plans for victory.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, constrained, global).

% Bear existential risk from counterforce targeting doctrines that plan for limited nuclear use without their consent or participation. No nuclear deterrent of their own, no seat at the planning table, no credible exit from the threat envelope. Their security is structurally hosted to the restraint of nuclear-armed states.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, non_nuclear_weapon_states, payer,
    moderate, generational, trapped, global).

% Populations in or near counterforce target complexes (silos, command centers, submarine pens, leadership bunkers) face prompt and fallout effects from any 'limited' nuclear exchange planned under this doctrine. No agency in the planning, no evacuation capability, no exit from the targeting logic.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_target_sets, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_target_sets).

% Scholarly and policy community holding the deterrence_unthinkable reading: nuclear use is self-defeating, escalation control is illusory, and planning for victory creates the very risks it claims to manage. Produces the intellectual counterweight that constrains operational doctrine from full deployment.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, deterrence_theorists_unthinkable, observer,
    analytical, civilizational, analytical, universal).

% Analysts documenting the gap between declaratory policy (no first use, sole purpose, 'unthinkable') and operational planning (counterforce targeting, damage limitation, escalation ladders). Their work exposes the rhetorical_contraction reading but does not directly alter the constraint's operation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, rhetorical_analysts_contraction, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared cognitive and operational framework for nuclear-armed states to plan military operations without triggering uncontrolled escalation — a grammar of restraint embedded in the very concept of 'limited' nuclear war, enabling crisis signaling, intra-war deterrence, and termination bargaining.
% TRANSFER_FUNCTION: Transfers existential risk from military-industrial budgets and strategic command prerogatives to arms control architectures, non-nuclear states, and civilian populations in target envelopes. The coordination benefit (crisis stability grammar) accrues to planners; the tail risk of plan execution accrues to those with no voice in the planning.
% ABSENT_VOICES: Non-nuclear weapon states and civilian populations in target sets are structurally excluded from the planning process that puts them at risk. Their objections would challenge the legitimacy of counterforce doctrine but they have no institutional pathway into the rooms where targeting plans are written and exercised.
% DISAPPEARANCE_RATIONALE: If the counterforce winnability constraint vanished overnight, strategic command structures would lose their core mission justification, procurement programs would face existential review, arms control negotiations would shift from verification of limits to verification of elimination, and the grammar of 'limited nuclear war' would cease to structure crisis signaling. The world would reorganize around either deterrence_unthinkable or rhetorical_contraction as the operative frame.
% FOUNDING_PROBLEM: Post-1945 U.S. strategic planners needed to preserve the utility of military force as a political instrument after the Soviet atomic test (1949) and thermonuclear breakout (1953) made total war self-annihilating. The founding problem: how to keep war 'thinkable' and winnable — and thus keep military institutions relevant — under a nuclear ceiling.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the military-industrial complex and strategic command structures (beneficiaries) as still live: 'the nuclear threat environment requires damage-limiting options.' It is attested as dead by arms control regimes and deterrence theorists (victims/observers): 'the Soviet Union is gone; the problem was bipolar parity, not nuclear weapons per se.' No neutral third-party corroboration exists — the dispute is structural.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the constraint transfers existential risk to populations and regimes with no say in the doctrine, while concentrating institutional benefits (budgets, missions, careers) in the military-industrial complex. Suppression (0.72) is higher still because the constraint's persistence depends on actively suppressing alternative framings (deterrence_unthinkable) and excluding rival voices (non-nuclear states, civil society) from the planning process. Theater ratio (0.28) is moderate: the coordination function (crisis grammar) is real but a declining share of the operational apparatus. The measurement series tracks three inflection points: Cuban Missile Crisis (escalation control grammar hardened), late Cold War (counterforce precision peaked), post-Cold War (mission drift without mission death).
 *
 * PERSPECTIVAL GAP:
 *   From the strategic command seat, the constraint is genuine coordination: it gives them a language for controlled escalation and a mission to plan for. From the arms control seat, the same structure is extraction: it makes treaties unverifiable and reductions impossible because counterforce requirements drive force structure. From the non-nuclear state seat, it is pure imposition: a grammar of restraint they never consented to, enforced by weapons they do not possess. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Military-industrial complex and strategic command structures are structural beneficiaries (d ~ 0.15-0.25): they collect budgets, missions, and professional identity from the constraint. Arms control regimes, non-nuclear states, and civilian populations are structural victims (d ~ 0.8-0.95): they bear the extracted risk with no exit. The derivation chain places institutional actors with arbitrage-grade exit (military-industrial) at the beneficiary end; identity-locked strategic commanders sit closer to target than their institutional power suggests because their professional self-concept is fused to the mission. Non-nuclear states are trapped by the global scope of the threat envelope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (keeping war thinkable under a nuclear ceiling) was real in 1949-1991. Post-1991, the bipolar parity problem dissolved but the institutional solution (counterforce planning apparatus) persisted — classic mandatrophy. The constraint now extracts more than it coordinates: the coordination benefit (crisis grammar) could be maintained by deterrence_unthinkable framing at lower extraction, but the institutional beneficiaries block that transition. The theater ratio rise (0.12 → 0.28) tracks the Goodhart drift: proxy metrics (target coverage, yield flexibility, promptness) replace the founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    escalation_control_viability,
    'Is escalation control in a counterforce exchange empirically viable, or is the ''limited nuclear war'' grammar a coordination myth that collapses on first use?',
    'Historical analysis of crisis simulations, wargame records, and the empirical record of nuclear crisis behavior (no counterforce exchanges have occurred). A natural experiment would require a counterforce strike that did not escalate — an event that by definition has not happened.',
    'If escalation control is illusory, the coordination function is a cover story and the constraint is a snare. If viable, the tangled_rope classification holds: genuine coordination coexists with extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_control_viability, conceptual, 'Whether the constraint''s coordination function survives contact with reality.').

omega_variable(
    counterforce_modernization_driver,
    'Is ongoing counterforce modernization (low-yield warheads, hypersonics, AI-enabled targeting) driven by genuine deterrence requirements or by institutional momentum of the military-industrial complex?',
    'Comparative analysis of requirement documents vs. threat assessments across administrations; tracing procurement logic to operational plans vs. budget protection.',
    'If driven by institutional momentum, extraction is higher than declared and the constraint is drifting toward snare. If threat-driven, the coordination function remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_modernization_driver, empirical, 'Whether the constraint''s current evolution is threat-responsive or institutionally self-sustaining.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the war_winnability_post_1945 kernel admit only these three readings, or is there a fourth structural position (e.g., ''managed vulnerability'' — winnability thinkable but deliberately not planned for, as a hedge)?',
    'Genealogical analysis of strategic doctrine documents 1945-present for positions that do not map cleanly to the three declared readings.',
    'If a fourth reading exists with distinct beneficiary/victim structure, the kernel decomposition is incomplete and this constraint story''s ε is not ε-invariant across the full framing space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel framings exhaust the structural possibilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1949, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1962, 0.18).
narrative_ontology:measurement(war__tr_t1974, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1974, 0.22).
narrative_ontology:measurement(war__tr_t1983, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1983, 0.28).
narrative_ontology:measurement(war__tr_t1991, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1991, 0.24).
narrative_ontology:measurement(war__tr_t2002, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2002, 0.26).
narrative_ontology:measurement(war__tr_t2018, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(war__be_t1949, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1962, 0.52).
narrative_ontology:measurement(war__be_t1974, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1974, 0.61).
narrative_ontology:measurement(war__be_t1983, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1983, 0.68).
narrative_ontology:measurement(war__be_t1991, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1991, 0.58).
narrative_ontology:measurement(war__be_t2002, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2002, 0.63).
narrative_ontology:measurement(war__be_t2018, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1949, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1949, 0.45).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1962, 0.68).
narrative_ontology:measurement(war__su_t1974, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1974, 0.72).
narrative_ontology:measurement(war__su_t1983, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1983, 0.78).
narrative_ontology:measurement(war__su_t1991, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement(war__su_t2002, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2002, 0.7).
narrative_ontology:measurement(war__su_t2018, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2018, 0.71).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__countervailing_thinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_modernization_imperative).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, arms_control_verification_architecture).

% DUAL FORMULATION NOTE:
% Part of the war_winnability_post_1945 constraint family. This reading (countervailing_thinkable) instantiates the operational persistence of winnability planning. The deterrence_unthinkable reading instantiates the logical foreclosure of victory. The rhetorical_contraction reading instantiates the declaratory-operational gap. The three readings share the kernel but have distinct ε values: countervailing_thinkable ε=0.68 (substantial extraction), deterrence_unthinkable ε≈0.05 (near-mountain), rhetorical_contraction ε≈0.45 (theater-dominated). The ε-invariance principle requires separate stories because the constraint's extraction profile depends on which reading's structural logic is instantiated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, institutional, 0.2).
constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, organized, 0.85).
constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
