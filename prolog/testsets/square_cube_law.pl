% ============================================================================
% CONSTRAINT STORY: square_cube_law
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_square_cube_law, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: square_cube_law
 * human_readable: The Square-Cube Law
 * domain: technological/biological
 * * SUMMARY:
 * The Square-Cube Law, a principle of geometry, states that as an object grows in size, its surface area increases by the square of the multiplier, while its volume (and mass) increases by the cube. This represents a fundamental scaling limit where properties dependent on surface area (like structural strength or heat dissipation) fail to keep pace with properties dependent on volume (like weight or heat generation). It is an immutable feature of three-dimensional space.
 * * KEY AGENTS:
 * - The Evolutionary Biologist: An analytical observer mapping the "Mountain" of physical limits on animal size.
 * - The Aircraft Designer: An institutional agent who must operate within the fixed boundaries of the law.
 * - The Mega-Fauna / Giant: A powerless subject whose physical integrity is bound by the law, making gargantuan size impossible.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: As a fundamental law of geometry, the square-cube law has near-zero extractiveness and suppression. It doesn't "extract" value in a socio-economic sense; it defines the fixed, neutral landscape of physical possibility.
domain_priors:base_extractiveness(square_cube_law, 0.01). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(square_cube_law, 0.01).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(square_cube_law, 0.0).       % Piton detection (>= 0.70). A physical law has no performative aspect.

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(square_cube_law, extractiveness, 0.01).
narrative_ontology:constraint_metric(square_cube_law, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(square_cube_law, theater_ratio, 0.0).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(square_cube_law, mountain).

% Binary flags
% (none apply)

% Structural property derivation hooks:
% (none apply, as this is a Mountain with no beneficiaries/victims)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% UNIFORM-TYPE CONSTRAINT: As a natural law, this constraint is a Mountain
% from all perspectives. The classification is invariant. We include multiple
% perspectives to demonstrate this invariance.

% PERSPECTIVE 1: THE ANALYTICAL OBSERVER (BIOLOGIST)
% Views the law as an unchangeable feature of the universe's geometric "hardware."
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE INSTITUTIONAL ACTOR (ENGINEER)
% Views the law as a fixed boundary condition for design and construction.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile), % Can choose different materials, but not a different law.
            spatial_scope(global))).

% PERSPECTIVE 3: THE POWERLESS SUBJECT (GIANT)
% Experiences the law as an absolute physical limit on their existence.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(square_cube_law_tests).

test(perspectival_invariance) :-
    % Verify that the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(square_cube_law, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(square_cube_law, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(square_cube_law, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation_mountain) :-
    % Verify the metrics are within the canonical range for a Mountain.
    narrative_ontology:constraint_metric(square_cube_law, extractiveness, E),
    narrative_ontology:constraint_metric(square_cube_law, suppression_requirement, S),
    E =< 0.15,
    S =< 0.05.

:- end_tests(square_cube_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This file models a canonical Mountain: a physical/mathematical law. The original file's high extraction (0.5) and suppression (0.7) scores were incorrect, as they confused the *consequences* of the law (e.g., a giant's bones breaking) with the nature of the law itself. A physical law is not extractive; it is a neutral, fixed feature of reality.
 *
 * The scores have been corrected to near-zero (E=0.01, S=0.01) to reflect this. Consequently, the perspectival variance has been removed. The law is a Mountain for the powerless giant, the institutional engineer, and the analytical biologist alike. Their *strategies* for dealing with the law differ, but their classification of the law itself is uniform. This demonstrates the "uniform-type" exception for natural laws.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Mandatrophy is not applicable here. As a pure Mountain with near-zero extraction, there is no coordination function to degrade or asymmetric extraction to disguise. The constraint's claim of being a `natural_law` is verifiably true, posing no risk of misclassification as a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_square_cube_law,
    'Is the law a feature of spacetime geometry itself, or an emergent property of particle interactions within that geometry?',
    'A unified theory of quantum gravity or experimental evidence of non-Euclidean geometry at micro-scales.',
    'If geometry itself, the Mountain is absolute. If emergent, there may be exotic physics (e.g., inside a neutron star) where the law is bypassed, changing its scope from universal to conditional.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(square_cube_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.01) is below the 0.46 threshold for
% mandatory temporal tracking. As a physical law, its properties do not drift.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No coordination function is present, so coordination_type is not applicable.
% No known structural dependencies on other constraints in the corpus.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (square_cube_law)
% ============================================================================
constraint_beneficiary(square_cube_law, engineering_disciplines).
constraint_victim(square_cube_law, none).
