% ============================================================================
% CONSTRAINT STORY: material_tensile_strength
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_material_tensile_strength, []).

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
 * * constraint_id: material_tensile_strength
 * human_readable: Ultimate Tensile Strength (UTS)
 * domain: technological
 * * SUMMARY:
 * Tensile strength is the maximum stress that a material can withstand while
 * being stretched or pulled before breaking. It represents the fundamental
 * cohesive limit of atomic bonding within a solid, dictating the maximum load
 * a structure can carry per unit of cross-sectional area. This is a physical
 * law, not a social construct.
 * * KEY AGENTS:
 * - The Material Scientist: An analytical observer measuring a fixed property.
 * - The Civil Engineer: An institutional agent designing structures around this fixed limit.
 * - The Overloaded Component: A physical object subject to the limit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: As a fundamental physical property, tensile strength has near-zero
% extractiveness. It doesn't "take" anything; it is simply a limit.
% Suppression is also minimal; it doesn't suppress alternatives, it defines the
% physical possibility space.
domain_priors:base_extractiveness(material_tensile_strength, 0.02). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(material_tensile_strength, 0.01).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(material_tensile_strength, 0.0).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(material_tensile_strength, extractiveness, 0.02).
narrative_ontology:constraint_metric(material_tensile_strength, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(material_tensile_strength, theater_ratio, 0.0).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(material_tensile_strength, mountain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% UNIFORM-TYPE CONSTRAINT: This is a natural law, which classifies as a
% Mountain from all perspectives. The perspectival minimum is relaxed.

% PERSPECTIVE 1: THE MATERIAL SCIENTIST (MOUNTAIN)
% To the scientist, tensile strength is a Mountain. It is an unchangeable
% feature of the material's atomic "hardware."
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE STRUCTURAL ENGINEER (MOUNTAIN)
% For the engineer, the limit is still a Mountain. They don't change the limit;
% they design around it. It is a fixed feature of the landscape they must build on.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage), % Can choose different materials (mountains)
            spatial_scope(regional))).

% PERSPECTIVE 3: THE OVERLOADED COMPONENT (MOUNTAIN)
% For a cable under load, the limit is the point of failure. It is not a trap
% (Snare) but an immutable physical boundary. Reaching the boundary results in
% failure, but the boundary itself is a fixed, non-extractive Mountain.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(material_tensile_strength_tests).

test(perspectival_invariance) :-
    % Verify that as a natural law, it is a Mountain from all perspectives.
    constraint_indexing:constraint_classification(material_tensile_strength, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(material_tensile_strength, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(material_tensile_strength, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation_mountain) :-
    % Verify the metrics adhere to the Mountain classification thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(material_tensile_strength, ExtMetricName, E),
    narrative_ontology:constraint_metric(material_tensile_strength, SuppMetricName, S),
    E =< 0.15,
    S =< 0.05.

:- end_tests(material_tensile_strength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint was refactored to correctly model a physical law. The original
 * version incorrectly assigned high extraction (0.5) and perspectival variance
 * (Rope, Snare). This is a category error. A physical law like tensile strength
 * is the canonical example of a Mountain: it is a fixed, unchangeable feature
 * of reality with effectively zero base extractiveness or suppression.
 *
 * The scores were adjusted to E=0.02 and S=0.01 to fall well within the Mountain
 * thresholds (E<=0.15, S<=0.05). Consequently, all perspectives correctly
 * classify the constraint as a Mountain. An engineer using the law for
 * coordination does not change the law into a Rope; they are simply using a
 * known Mountain as a reference point. A component failing does not make the
 * law a Snare; it simply means a physical limit was reached.
 *
 * As a low-extraction constraint, it does not require Omega variables or
 * temporal lifecycle data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Not required for low-extraction constraints.

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(material_tensile_strength, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints. The properties of a physical
% law are constant over the interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No coordination function or network relationships are applicable for a
% fundamental physical constant.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (material_tensile_strength)
% ============================================================================
constraint_beneficiary(material_tensile_strength, structural_engineers).
constraint_victim(material_tensile_strength, none).

omega_variable(
    omega_metamaterial_bypass,
    "Could engineered metamaterials or nano-scale structures effectively bypass the classical tensile strength limits of bulk materials?",
    "Laboratory testing of carbon nanotube composites and graphene structures at macro-scale loads.",
    "If bypassed: The Mountain is conditional on material scale. If not: The physical law remains absolute.",
    confidence_without_resolution(medium)
).
