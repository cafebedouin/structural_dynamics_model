% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Statehood Requires Recognition (Constitutive Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'constitutive' reading of statehood, where
 *   recognition by existing states is a prerequisite for an entity to be
 *   considered a state in international law. It is one reading of the broader
 *   'Montevideo Statehood Criteria' kernel. This reading places unrecognized
 *   polities in a victim role, as their legal existence and access to
 *   international forums are contingent on the political will of established
 *   powers. The metrics reflect high extraction and suppression due to the
 *   structural power imbalance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.85).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.92).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, snare).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Statehood Requires Recognition (Constitutive Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '54a8cbbb-6ebd-462e-9aaf-e675f4243f34').
narrative_ontology:cs_kernel_codification('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', formalized).
narrative_ontology:cs_authority_grounding('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', extraction).
narrative_ontology:cs_interpretation_layer_present('54a8cbbb-6ebd-462e-9aaf-e675f4243f34').
narrative_ontology:cs_reading_relation('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', montevideo_statehood_criteria__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', foundational, recognition_is_prerequisite_for_statehood).
narrative_ontology:cs_axiom_status(recognition_is_prerequisite_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', recognition_is_prerequisite_for_statehood, conventional).
narrative_ontology:cs_axiom('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', secondary, existing_states_hold_veto_on_new_state_creation).
narrative_ontology:cs_axiom_status(existing_states_hold_veto_on_new_state_creation, holdable).
narrative_ontology:cs_axiom_grounding('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', existing_states_hold_veto_on_new_state_creation, conventional).
narrative_ontology:cs_reference_frame('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', westphalian_state_system_sovereignty).
narrative_ontology:cs_drift_state('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', post_cold_war_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('54a8cbbb-6ebd-462e-9aaf-e675f4243f34', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, international_organizations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, populations_in_unrecognized_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the existing community of states, they collectively hold the power to grant or withhold recognition, thereby determining the legal existence and international personality of new entities. They benefit from maintaining control over the international system's membership.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Organizations like the UN, which operate based on the recognition of member states. They benefit from the clarity and stability provided by a constitutive approach, as it defines their membership and operational scope, even if it means excluding some entities.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_organizations, beneficiary,
    institutional, generational, constrained, global).

% Entities that meet objective criteria for statehood (territory, population, government, capacity to enter relations) but lack recognition from the international community. They are denied access to treaties, international aid, and full economic participation, effectively trapped in a state of legal limbo.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, generational, trapped, regional).

% Citizens of unrecognized polities who suffer the direct consequences of non-recognition, including limited travel rights, economic hardship, and lack of international protection. Their identity is often tied to the aspiration of statehood, making 'exit' from the polity unthinkable.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, populations_in_unrecognized_territories, payer,
    powerless, biographical, identity_locked, local).

% Academics and legal scholars who argue that statehood is an objective fact, not dependent on recognition. They analyze the structural consequences of the constitutive reading and advocate for alternative frameworks.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, declaratory_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit exclusive, mechanism for determining membership in the international community, which coordinates diplomatic relations, treaty obligations, and the application of international law among recognized entities.
% TRANSFER_FUNCTION: Transfers the power to define statehood from objective criteria to the subjective will of existing states, effectively granting a veto power over new state creation and denying international personality to unrecognized entities.
% ABSENT_VOICES: Unrecognized polities and their populations are structurally excluded from the decision-making process regarding their own statehood. They would argue for the primacy of self-determination and objective criteria, but their voices are marginalized by the very mechanism that denies them legal standing.
% DISAPPEARANCE_RATIONALE: If the constitutive requirement for statehood vanished, numerous unrecognized polities would immediately gain full international legal personality. This would fundamentally alter the UN's membership, redraw diplomatic maps, and reconfigure international trade and security alliances, leading to a significant rearrangement of the global order.
% FOUNDING_PROBLEM: To prevent chaotic proliferation of self-proclaimed states and ensure a stable, manageable international system where new entities are integrated in an orderly fashion.
% FOUNDING_PROBLEM_CORROBORATION: Existing states and major international organizations consistently assert that the problem of maintaining international order and preventing fragmentation remains live, justifying the need for a recognition-based system. Declaratory theorists, while disagreeing with the solution, acknowledge the historical problem of potential chaos without some form of gatekeeping.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constitutive reading allows existing states to leverage their recognition power for political or economic gain, denying full international personality to entities that objectively meet statehood criteria. Suppression is also high, as unrecognized polities have extremely limited avenues to overcome non-recognition, often facing diplomatic isolation, economic sanctions, and military threats if they attempt to assert full sovereignty without external approval. Theater ratio is low because the mechanism is genuinely functional in maintaining the existing state system, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of existing states, this constraint is a necessary mechanism for international order (a 'rope' or even 'mountain' of international relations). From the perspective of unrecognized polities, it is a 'snare' that traps them in a subordinate status, denying their inherent right to self-determination. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing states and international organizations are beneficiaries, as they control the system's membership and benefit from its stability. Unrecognized polities and their populations are victims, bearing the full cost of non-recognition, including lack of legal standing and international protection. Declaratory theorists are observers, analyzing the system without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_as_political_tool,
    'To what extent is recognition withheld or granted based on political expediency rather than objective criteria or genuine concerns for international order?',
    'Comparative case studies of unrecognized polities, analyzing the political and economic interests of states that grant or withhold recognition, and correlating these with the objective status of the polity.',
    'If recognition is primarily a political tool, it strengthens the ''snare'' classification by highlighting the instrumental use of the constraint for extraction. If it''s consistently tied to objective criteria, it might suggest a ''tangled_rope'' with a genuine, albeit flawed, coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_as_political_tool, empirical, 'Assesses the political vs. objective basis of recognition decisions.').

omega_variable(
    self_determination_vs_state_sovereignty,
    'How does the constitutive reading balance the principle of self-determination of peoples against the principle of state sovereignty and territorial integrity?',
    'Legal and philosophical analysis of international jurisprudence and UN resolutions concerning self-determination, particularly in cases of secession or decolonization, to identify the prevailing legal hierarchy.',
    'If self-determination is consistently subordinated, it reinforces the extractive nature of the constitutive reading. If a balance is found, it might suggest a more complex ''tangled_rope'' where competing principles are imperfectly coordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_vs_state_sovereignty, conceptual, 'Examines the conceptual tension between self-determination and state sovereignty within the constitutive framework.').

omega_variable(
    declaratory_constitutive_ambiguity,
    'Is the constitutive reading truly distinct from the declaratory reading, or do they represent different emphases within a single, ambiguous framework?',
    'Analysis of state practice and judicial decisions to determine if the objective criteria of statehood (declaratory) are ever truly sufficient without some form of recognition (constitutive), or if recognition is always implicitly required.',
    'If the distinction is blurred in practice, it suggests the ''constitutive'' aspect is a more powerful, underlying structural reality, even when states claim to adhere to declaratory principles. This would reinforce the ''snare'' classification by showing how the constitutive element dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_constitutive_ambiguity, conceptual, 'Explores the practical and theoretical overlap between constitutive and declaratory theories of statehood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mont_tr_t10, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(mont_tr_t20, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(mont_tr_t40, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(mont_tr_t50, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(mont_tr_t60, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(mont_tr_t70, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 70, 0.1).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(mont_be_t10, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(mont_be_t20, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(mont_be_t40, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(mont_be_t50, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 50, 0.84).
narrative_ontology:measurement(mont_be_t60, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(mont_be_t70, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 70, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(mont_su_t10, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(mont_su_t20, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(mont_su_t40, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(mont_su_t50, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 50, 0.91).
narrative_ontology:measurement(mont_su_t60, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 60, 0.92).
narrative_ontology:measurement(mont_su_t70, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 70, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, un_membership_criteria).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, international_treaty_access).

% DUAL FORMULATION NOTE:
% This constraint is the 'constitutive' reading of the Montevideo Statehood Criteria. It emphasizes that recognition by existing states is a prerequisite for statehood, in contrast to the 'declaratory' reading (statehood is an objective fact) and the 'hybrid' reading (objective criteria plus normative legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
