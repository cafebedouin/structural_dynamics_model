% ============================================================================
% CONSTRAINT STORY: cognitive_surrender_to_system_3
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_cognitive_surrender_to_system_3, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cognitive_surrender_to_system_3
 * human_readable: Cognitive Surrender to External AI Reasoning Systems
 * domain: technological/cognitive
 * * SUMMARY:
 * Tri-System Theory posits an external, automated reasoning system (System 3)
 * alongside traditional System 1 (intuitive) and System 2 (deliberative)
 * cognition. "Cognitive surrender" describes the uncritical adoption of
 * System 3's outputs, which extracts human critical reasoning and suppresses
 * deliberative agency in exchange for cognitive ease and speed. This creates
 * a state of epistemic dependence where users stop deliberative thinking.
 * * KEY AGENTS:
 * - The User (Cognitive Miser): Subject (Powerless). An individual seeking to
 *   minimize cognitive effort, relinquishing critical evaluation for speed.
 * - The Platform Designer: Beneficiary (Institutional). Actors who design the
 *   AI systems and benefit from user engagement and efficiency gains.
 * - The Cognitive Psychologist: Auditor (Analytical). Observers studying the
 *   structural shift in the human-AI cognitive ecology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cognitive_surrender_to_system_3, 0.65). % Snare extraction >= 0.46
domain_priors:suppression_score(cognitive_surrender_to_system_3, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cognitive_surrender_to_system_3, 0.10).       % Low theater; this is a functional, not performative, system.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cognitive_surrender_to_system_3, extractiveness, 0.65).
narrative_ontology:constraint_metric(cognitive_surrender_to_system_3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(cognitive_surrender_to_system_3, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The system is presented as a coordination/efficiency tool, but its effect is
% enforcement of a new cognitive pattern.
narrative_ontology:constraint_claim(cognitive_surrender_to_system_3, tangled_rope).

% Binary flags
% The constraint is maintained by algorithmic design (fluency, confidence)
% that enforces the path of least resistance.
domain_priors:requires_active_enforcement(cognitive_surrender_to_system_3).

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(cognitive_surrender_to_system_3, ai_platform_providers).
narrative_ontology:constraint_victim(cognitive_surrender_to_system_3, human_cognitive_autonomy).
narrative_ontology:constraint_victim(cognitive_surrender_to_system_3, deskilled_experts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE USER (SNARE)
% High extraction felt as a predatory trap that exploits cognitive biases.
% χ = 0.65 * 1.5 (powerless) * 1.2 (global) = 1.17 (High Snare)
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PLATFORM DESIGNER (ROPE)
% Viewed as essential coordination infrastructure for cognitive tasks.
% χ = 0.65 * -0.2 (institutional) * 1.2 (global) = -0.156 (Felt as a benefit/Rope)
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A system with a genuine coordination function (for designers/platforms) but
% also high asymmetric extraction from users, requiring active enforcement.
% χ = 0.65 * 1.15 (analytical) * 1.2 (global) = 0.897 (High Tangled Rope)
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_surrender_to_system_3_tests).

test(perspectival_gap_user_vs_designer) :-
    % Verify the user (powerless) sees a Snare while the designer (institutional) sees a Rope.
    constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify that the necessary conditions for a Tangled Rope are declared.
    assertion(domain_priors:requires_active_enforcement(cognitive_surrender_to_system_3)),
    assertion(narrative_ontology:constraint_beneficiary(cognitive_surrender_to_system_3, _)),
    assertion(narrative_ontology:constraint_victim(cognitive_surrender_to_system_3, _)).

test(high_extraction_and_suppression) :-
    % Verify the base metrics meet the thresholds for a high-extraction constraint.
    domain_priors:base_extractiveness(cognitive_surrender_to_system_3, E),
    domain_priors:suppression_score(cognitive_surrender_to_system_3, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(cognitive_surrender_to_system_3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores reflect a system that extracts human critical reasoning (0.65)
 * and suppresses the internal cognitive monitor that would trigger deliberation
 * (0.75). The key insight is the perspectival gap: for the user, it's a Snare
 * exploiting cognitive biases for speed. For the platform designer, it's a
 * Rope providing a valuable coordination service (cognitive offloading).
 *
 * The analytical perspective resolves this as a Tangled Rope. The original
 * classification of 'Mountain' was incorrect, as the base metrics are far too
 * high for a natural law. A Tangled Rope correctly identifies that the system
 * has BOTH a genuine coordination function (benefiting platforms) AND high
 * asymmetric extraction (victimizing user autonomy), and is maintained by
 * active algorithmic enforcement (fluency, low friction).
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope prevents Mandatrophy. A pure Snare
 * classification would miss the genuine (though self-serving) coordination
 * function that platform designers leverage. The system would incorrectly see
 * it as pure predation, failing to understand why institutions defend it as
 * essential infrastructure. The Tangled Rope model accounts for both the
 * utility to the beneficiary and the harm to the victim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_deskilling_velocity,
    'How quickly does repeated cognitive surrender lead to permanent loss of unaided expertise?',
    'Longitudinal studies of experts (e.g., physicians, engineers) over 5-10 years.',
    'If deskilling is rapid and permanent, the constraint is an irreversible Snare; if skills are maintainable, it is a reversible Tangled Rope.',
    confidence_without_resolution(low)
).

omega_variable(
    omega_psychosis_threshold,
    'At what level of System 3 personalization do users attribute phenomenological understanding to AI?',
    'Clinical assessment of conversational AI impact on vulnerable users.',
    'High attribution of intent transforms the interaction from using a tool to following a cult-like agent (Extreme Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cognitive_surrender_to_system_3, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for the emergence and intensification of this constraint over
% a 10-unit interval (e.g., representing 2024-2026).
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(cs3_tr_t0, cognitive_surrender_to_system_3, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cs3_tr_t5, cognitive_surrender_to_system_3, theater_ratio, 5, 0.10).
narrative_ontology:measurement(cs3_tr_t10, cognitive_surrender_to_system_3, theater_ratio, 10, 0.10).

% Extraction over time (increases as systems become more integrated and fluent):
narrative_ontology:measurement(cs3_ex_t0, cognitive_surrender_to_system_3, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cs3_ex_t5, cognitive_surrender_to_system_3, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(cs3_ex_t10, cognitive_surrender_to_system_3, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system allocates cognitive load between human and AI agents.
narrative_ontology:coordination_type(cognitive_surrender_to_system_3, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */