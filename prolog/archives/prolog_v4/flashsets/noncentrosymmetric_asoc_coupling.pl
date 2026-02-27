% ============================================================================
% CONSTRAINT STORY: noncentrosymmetric_asoc_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_noncentrosymmetric_asoc_coupling, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: noncentrosymmetric_asoc_coupling
 *   human_readable: Noncentrosymmetric Antisymmetric Spin-Orbit Coupling
 *   domain: condensed_matter_physics/superconductivity/quantum_materials
 *
 * SUMMARY:
 *   In noncentrosymmetric superconductors, the absence of spatial inversion
 *   symmetry in the crystal structure enables antisymmetric spin-orbit
 *   coupling (ASOC). This ASOC leads to a mixing of spin-singlet and
 *   spin-triplet pairing states, resulting in unconventional superconducting
 *   properties such as upper critical field enhancement and the possible
 *   emergence of topological superconductivity. From a materials perspective,
 *   it enables the formation of novel quantum states and functionalities.
 *
 * KEY AGENTS:
 *   - Condensed Matter Researchers: Primary beneficiary (institutional/analytical) — benefits from the novel physics and potential applications.
 *   - Materials Science Community: Secondary beneficiary (organized/mobile) — benefits from access to new materials with unique properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(noncentrosymmetric_asoc_coupling, 0.3).
domain_priors:suppression_score(noncentrosymmetric_asoc_coupling, 0.2).
domain_priors:theater_ratio(noncentrosymmetric_asoc_coupling, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, extractiveness, 0.3).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(noncentrosymmetric_asoc_coupling, rope).
narrative_ontology:human_readable(noncentrosymmetric_asoc_coupling, "Noncentrosymmetric Antisymmetric Spin-Orbit Coupling").
narrative_ontology:topic_domain(noncentrosymmetric_asoc_coupling, "condensed_matter_physics/superconductivity/quantum_materials").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(noncentrosymmetric_asoc_coupling, condensed_matter_researchers).
narrative_ontology:constraint_beneficiary(noncentrosymmetric_asoc_coupling, materials_science_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The analytical observer sees the ASOC as a fundamental physical property enabling novel quantum states and functionalities.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% Experimental physicists can move to other research areas. The ASOC is a useful property for discovering new physics.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Theoretical physicists can develop models and theories around ASOC and its effects.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(noncentrosymmetric_asoc_coupling_tests).
:- end_tests(noncentrosymmetric_asoc_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.30): Low. ASOC is a physical property, not a mechanism that extracts value. It provides a basis for exploring new physics, but its existence doesn't inherently take from any group. Suppression (0.20): Low. The existence of ASOC does not suppress alternatives. Researchers are free to study centrosymmetric materials or other phenomena. Theater Ratio (0.10): Low. The study of ASOC is driven by genuine scientific inquiry, not performative activities.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap as all perspectives see ASOC as enabling new physics. The institutional observer sees ASOC as a fundamental property; the physicists see it as a useful tool.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiaries are condensed matter researchers and the materials science community who benefit from the novel physics and potential applications arising from ASOC. Since no significant extraction or suppression occurs, all relevant agents experience a net benefit, leading to their classification as Rope from their respective viewpoints.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not pose a mandatrophy risk, as ASOC is a physical property of materials and not a social arrangement or institution that could be misclassified as a tool of extraction or coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(noncentrosymmetric_asoc_coupling, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(noncentrosymmetric_asoc_coupling, information_standard).
narrative_ontology:affects_constraint(noncentrosymmetric_asoc_coupling, superconductivity_pairing_mechanism).
narrative_ontology:affects_constraint(noncentrosymmetric_asoc_coupling, topological_superconductivity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
