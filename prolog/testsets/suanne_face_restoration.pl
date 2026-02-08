% ============================================================================
% CONSTRAINT STORY: suanne_face_restoration
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_suanne_face_restoration, []).

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
 * * constraint_id: suanne_face_restoration
 * human_readable: The Coup of Positive Politeness
 * domain: social
 * * SUMMARY:
 * This constraint models the social dynamic during a 1988 basketball game where SuAnne Marie Big Crow, a Lakota player, faced a hostile, taunting crowd. The crowd's mockery was a "Face-Threatening Act" (FTA) that extracted social dignity from her team. SuAnne's response—performing a traditional Lakota dance—functioned as a "Positive Politeness" strategy, a coordination mechanism that restored her team's "face" and reframed the social contract, creating a shared identity with the crowd.
 * * KEY AGENTS:
 * - The Basketball Team: Subject (Powerless), victim of the face-threatening act.
 * - School Administration / Athletic Association: Beneficiary (Institutional), benefits from the de-escalation and restoration of sportsmanship.
 * - Social Systems Analyst: Auditor (Analytical), observes the interplay of extraction and coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(suanne_face_restoration, 0.75). % The initial face-attack liquidates the team's social status and dignity. High extraction.
domain_priors:suppression_score(suanne_face_restoration, 0.65).   % The mockery suppresses the team's authentic identity behind caricatures.
domain_priors:theater_ratio(suanne_face_restoration, 0.10).       % SuAnne's action is highly functional, not performative for its own sake.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(suanne_face_restoration, extractiveness, 0.75).
narrative_ontology:constraint_metric(suanne_face_restoration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(suanne_face_restoration, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The dynamic is a constructed social enforcement mechanism, not a natural state.
narrative_ontology:constraint_claim(suanne_face_restoration, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(suanne_face_restoration). % Required for Tangled Rope. The social pressure requires active participation.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(suanne_face_restoration, school_administration). % Derives has_coordination_function/1
narrative_ontology:constraint_victim(suanne_face_restoration, targeted_basketball_team). % Derives has_asymmetric_extraction/1

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The team experiences the mockery as a pure trap, extracting their dignity.
% χ = 0.75 * π(powerless, 1.5) * σ(local, 0.8) = 0.90. This is a clear Snare.
constraint_indexing:constraint_classification(suanne_face_restoration, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The school administration sees SuAnne's act as a pure coordination tool that
% resolves a dangerous conflict and restores sportsmanship.
% χ = 0.75 * π(institutional, -0.2) * σ(regional, 0.9) = -0.135. This is a clear Rope.
constraint_indexing:constraint_classification(suanne_face_restoration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the high asymmetric extraction (Snare aspect) and the
% brilliant coordination function (Rope aspect). It requires active enforcement
% and has clear victims and beneficiaries. This is the canonical Tangled Rope.
% χ = 0.75 * π(analytical, 1.15) * σ(global, 1.2) = 1.035.
constraint_indexing:constraint_classification(suanne_face_restoration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suanne_face_restoration_tests).

test(perspectival_gap) :-
    % Verify the gap between the team (powerless) and the administration (institutional).
    constraint_indexing:constraint_classification(suanne_face_restoration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suanne_face_restoration, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The canonical classification for this structure should be Tangled Rope.
    constraint_indexing:constraint_classification(suanne_face_restoration, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(suanne_face_restoration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint is a classic Tangled Rope. The initial situation is a Snare from the perspective of the targeted team, where their social dignity ("face") is being extracted to fuel the hostile crowd's sense of superiority. SuAnne's intervention introduces a powerful coordination function (a Rope) that de-escalates the conflict and creates a new, shared identity.
 *
 * The Perspectival Gap is stark:
 * - For the team (powerless), it is a pure Snare (χ=0.90).
 * - For the administration (institutional), it is a beneficial Rope (χ=-0.135), a tool for conflict resolution.
 * - The analytical view must account for both realities simultaneously. The structure has a clear coordination function (beneficiary exists), asymmetric extraction (victim exists), and requires active enforcement. This makes it a canonical Tangled Rope.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system correctly avoids classifying this as a pure Snare by recognizing the powerful coordination function introduced by SuAnne. The Tangled Rope classification captures the dual nature of the event: a resolution to a problem that should not have existed in the first place. The high extraction is real, but so is the coordination that resolves it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_suanne_face_restoration,
    "Was the crowd's subsequent silence a biological recognition of authenticity (Mountain), or a temporary, strategic confusion allowing recalibration without losing face (Rope)?",
    "Longitudinal study of community social dynamics in Lead, SD, post-event, compared to control communities without such an intervention.",
    "If Mountain, authenticity is a social law with predictable effects. If Rope, the act was a high-risk gamble that may not be replicable.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(suanne_face_restoration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This event is a snapshot, but the underlying potential for social extraction
% was constant during the period. The measurements reflect the state of the
% social dynamic during the game. Required because base_extractiveness > 0.46.

% Theater ratio over time (consistently low and functional):
narrative_ontology:measurement(suanne_face_restoration_tr_t0, suanne_face_restoration, theater_ratio, 0, 0.10).
narrative_ontology:measurement(suanne_face_restoration_tr_t5, suanne_face_restoration, theater_ratio, 5, 0.10).
narrative_ontology:measurement(suanne_face_restoration_tr_t10, suanne_face_restoration, theater_ratio, 10, 0.10).

% Extraction over time (consistently high potential for social harm):
narrative_ontology:measurement(suanne_face_restoration_ex_t0, suanne_face_restoration, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(suanne_face_restoration_ex_t5, suanne_face_restoration, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(suanne_face_restoration_ex_t10, suanne_face_restoration, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% SuAnne's dance enforces a new norm of mutual respect.
narrative_ontology:coordination_type(suanne_face_restoration, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */