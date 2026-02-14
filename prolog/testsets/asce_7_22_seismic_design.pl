% ============================================================================
% CONSTRAINT STORY: asce_7_22_seismic_design
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_asce_7_22_seismic_design, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: asce_7_22_seismic_design
 *   human_readable: ASCE 7-22 Seismic Design Requirements
 *   domain: technological/legal
 *
 * SUMMARY:
 *   ASCE 7-22 is a building code standard that dictates the minimum loads,
 *   including seismic forces, that structures in the United States must be
 *   designed to withstand. It functions as a coordination mechanism for public
 *   safety but imposes significant costs and suppresses alternative design
 *   methodologies through its prescriptive requirements and the need for
 *   regulatory approval.
 *
 * KEY AGENTS (by structural relationship):
 *   - Junior Structural Engineer (powerless/trapped): Primary target of enforcement, must comply to practice.
 *   - Property Developer (moderate/constrained): Bears the direct financial cost of increased structural requirements.
 *   - ASCE 7 Code Committee (institutional/arbitrage): Primary beneficiary, defines the standard and benefits from its adoption.
 *   - Building Occupants (powerless/trapped): Secondary beneficiaries who receive safety benefits.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Increased from 0.2 to 0.35. While the goal is safety, the direct
% cost of compliance (materials, labor, complex engineering) represents a
% significant transfer of capital from developers to the construction/materials
% sector, justifying a Tangled Rope classification.
domain_priors:base_extractiveness(asce_7_22_seismic_design, 0.35).
% Rationale: High. Alternatives like Performance-Based Design exist but are
% costly, require specialized expertise, and face high scrutiny from building
% officials, effectively suppressing them for most projects.
domain_priors:suppression_score(asce_7_22_seismic_design, 0.70).
domain_priors:theater_ratio(asce_7_22_seismic_design, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asce_7_22_seismic_design, extractiveness, 0.35).
narrative_ontology:constraint_metric(asce_7_22_seismic_design, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(asce_7_22_seismic_design, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(asce_7_22_seismic_design, tangled_rope).

% --- Binary flags ---
% Required for Tangled Rope classification. The code is enforced by local
% building departments (Authorities Having Jurisdiction).
domain_priors:requires_active_enforcement(asce_7_22_seismic_design).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, building_occupants).
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, insurance_underwriters).
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, asce_code_committee).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(asce_7_22_seismic_design, property_developers).
narrative_ontology:constraint_victim(asce_7_22_seismic_design, junior_structural_engineers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE JUNIOR STRUCTURAL ENGINEER (SNARE)
% For the practicing engineer, the code is not negotiable. It is a set of
% rules with high suppression of alternatives that must be followed to avoid
% liability and get permits. This traps them in a specific methodology,
% which feels like a Mountain but is structurally a Snare due to the high
% suppression and required enforcement.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ASCE 7 CODE COMMITTEE (ROPE)
% For the committee that writes the standard, it is a pure coordination tool.
% They have arbitrage exit (they can change the rules in the next cycle) and
% see it as a mechanism to align engineering practice with safety research.
% They are the primary institutional beneficiary.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PROPERTY DEVELOPER (SNARE)
% For a developer, especially on marginal projects, the unyielding and costly
% requirements can make a project financially non-viable. The high suppression
% of cheaper alternatives and mandatory enforcement makes it a classic Snare.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the genuine coordination function (public safety,
% shared standards) and the asymmetric extraction (costs imposed on developers)
% and high suppression. This dual nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asce_7_22_seismic_design_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target, beneficiary, and analyst.
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, TypeTarget,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, TypeBeneficiary,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, TypeAnalyst,
        context(agent_power(analytical), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeAnalyst == tangled_rope.

test(tangled_rope_threshold_validation) :-
    % Verify metrics are consistent with Tangled Rope classification.
    narrative_ontology:constraint_metric(asce_7_22_seismic_design, extractiveness, E),
    narrative_ontology:constraint_metric(asce_7_22_seismic_design, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(asce_7_22_seismic_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original classification of this constraint as a Mountain was incorrect
 * because the suppression score (0.70) is far too high for the Mountain gate
 * (<= 0.05). A Mountain is a limit of physics or logic; a building code is a
 * human system with high enforcement and suppression of alternatives.
 *
 * The analytical classification is Tangled Rope. It has a clear coordination
 * function (beneficiary: building_occupants) providing public safety, but also
 * imposes asymmetric costs (victim: property_developers) and requires active
 * enforcement to maintain its dominance over alternatives. The base
 * extractiveness was raised to 0.35 to meet the Tangled Rope threshold (>= 0.30),
 * reflecting the significant material and engineering costs.
 *
 * PERSPECTIVAL GAP:
 * - The engineer (powerless, trapped) experiences the code as a Snare. While it
 *   feels like an unchangeable law (Mountain), its high suppression and
 *   enforcement make it structurally a Snare from their perspective.
 * - The code committee (institutional, arbitrage) sees it as a Rope, a tool
 *   for coordination that they control and benefit from.
 * - The developer (moderate, constrained) also sees a Snare, as the costs can
 *   render projects non-viable.
 *
 * MANDATROPHY ANALYSIS:
 * This re-classification from Mountain to Tangled Rope is a core example of
 * preventing mandatrophy. Labeling a man-made, high-suppression regulatory
 * system as a "law of nature" (Mountain) obscures the real economic trade-offs
 * and power dynamics at play. The Tangled Rope classification correctly
 * identifies both its utility and its extractive/coercive components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_asce_7_22_seismic_design,
    'Do the code''s seismic load requirements accurately reflect physical reality, or are they overly conservative due to risk aversion and industry influence?',
    'Post-earthquake analysis of buildings designed to the ASCE 7-22 standard vs. older standards after a major seismic event.',
    'If accurate, the constraint is a high-friction Rope. If overly conservative, it has a significant Snare component, extracting unnecessary capital.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asce_7_22_seismic_design, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.35) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(asce_7_22_seismic_design, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality for
% the key agents in this system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */