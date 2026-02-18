% ============================================================================
% CONSTRAINT STORY: poetic_verse_and_past
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_poetic_verse_and_past, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: poetic_verse_and_past
 * human_readable: The Inescapable Tale of Institutional Expectation
 * domain: social/cultural
 * * SUMMARY:
 * This constraint models the rigid institutional and familial expectations at a preparatory school, as depicted in "Dead Poets Society". The system demands conformity to a narrow set of "noble pursuits" (medicine, law, business), actively suppressing individual expression and alternative life paths. This creates a system that coordinates students towards socially-approved roles while extracting their individuality and autonomy.
 * * KEY AGENTS:
 * - The Students (e.g., Neil Perry): Subject (Powerless)
 * - The School Administration & Parents: Beneficiary (Institutional)
 * - Systems Theorist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(poetic_verse_and_past, 0.75). % The system extracts individual identity, autonomy, and in the extreme, life itself.
domain_priors:suppression_score(poetic_verse_and_past, 0.60).   % Parental authority and school rules actively punish deviation from the prescribed path.
domain_priors:theater_ratio(poetic_verse_and_past, 0.15).       % The enforcement is functional, not performative. The goals are real.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(poetic_verse_and_past, extractiveness, 0.75).
narrative_ontology:constraint_metric(poetic_verse_and_past, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(poetic_verse_and_past, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% The institution claims its rules are for coordination towards a successful future.
narrative_ontology:constraint_claim(poetic_verse_and_past, tangled_rope).
narrative_ontology:human_readable(poetic_verse_and_past, "The Inescapable Tale of Institutional Expectation").
narrative_ontology:topic_domain(poetic_verse_and_past, "social/cultural").

% Binary flags
domain_priors:requires_active_enforcement(poetic_verse_and_past). % Requires active enforcement by parents and the school board.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(poetic_verse_and_past, institutional_order).
narrative_ontology:constraint_victim(poetic_verse_and_past, individual_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a student like Neil Perry, the constraint is a lethal trap.
% χ = 0.75 * 1.5 (powerless) * 0.8 (local) = 0.90. This is a clear Snare.
constraint_indexing:constraint_classification(poetic_verse_and_past, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The school administration sees the rules as a necessary tool for coordination.
% χ = 0.75 * -0.2 (institutional) * 0.9 (regional) = -0.135. This is a clear Rope.
constraint_indexing:constraint_classification(poetic_verse_and_past, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the coordination function and the asymmetric extraction.
% χ = 0.75 * 1.15 (analytical) * 1.2 (global) = 1.035.
% With high extraction, high suppression, enforcement, and both beneficiaries
% and victims, the structure is a textbook Tangled Rope.
constraint_indexing:constraint_classification(poetic_verse_and_past, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(poetic_verse_and_past_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(poetic_verse_and_past, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(poetic_verse_and_past, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope structure.
    constraint_indexing:constraint_classification(poetic_verse_and_past, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % High-extraction constraints must meet the Snare/Tangled Rope threshold.
    domain_priors:base_extractiveness(poetic_verse_and_past, E),
    E >= 0.46.

:- end_tests(poetic_verse_and_past_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint is a classic example of a Tangled Rope. The base extractiveness (0.75) is severe, representing the theft of individual futures. The suppression score (0.60) reflects the active, coercive measures used to enforce conformity.
 *
 * The Perspectival Gap is profound:
 * - For the institution (school, parents), the system is a 'Rope' that coordinates students into stable, high-status professions, ensuring the continuation of their legacy. The negative extraction felt by this index (-0.135) shows they perceive it as a net benefit.
 * - For the student (Neil), the same system is a 'Snare' with no viable exit, leading to the ultimate tragedy. The high effective extraction (0.90) reflects this crushing weight.
 * - The analytical observer, seeing both the coordination function for the beneficiaries and the severe asymmetric extraction imposed on the victims, correctly classifies it as a 'Tangled Rope'.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system avoids misclassifying this as a pure Snare by acknowledging its genuine (though coercive) coordination function for the institutional beneficiaries. The Tangled Rope classification correctly captures this duality: a mechanism that is simultaneously a tool of order for one group and a tool of destruction for another. The tragedy arises precisely from this entanglement, not from simple malice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_poetic_verse_and_past,
    "Is the institutional pressure a necessary evil for societal stability (a harsh but functional Tangled Rope) or a purely destructive force that creates no value (a pure Snare disguised as coordination)?",
    "Longitudinal studies comparing life outcomes of students from such institutions versus those from more liberal systems, controlling for socioeconomic background.",
    "If necessary evil, the classification remains Tangled Rope. If purely destructive, the coordination claim is false, and it should be re-classified as a Snare from all non-beneficiary perspectives.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(poetic_verse_and_past, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint. The model assumes the
% institutional pressure intensified over the school's history.
%
% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(poetic_verse_and_past_tr_t0, poetic_verse_and_past, theater_ratio, 0, 0.10).
narrative_ontology:measurement(poetic_verse_and_past_tr_t5, poetic_verse_and_past, theater_ratio, 5, 0.12).
narrative_ontology:measurement(poetic_verse_and_past_tr_t10, poetic_verse_and_past, theater_ratio, 10, 0.15).

% Extraction over time (intensifies as standards become more rigid):
narrative_ontology:measurement(poetic_verse_and_past_ex_t0, poetic_verse_and_past, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(poetic_verse_and_past_ex_t5, poetic_verse_and_past, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(poetic_verse_and_past_ex_t10, poetic_verse_and_past, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint uses rules and authority to enforce a specific life path.
narrative_ontology:coordination_type(poetic_verse_and_past, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */