% ============================================================================
% CONSTRAINT STORY: noncentrosymmetric_asoc_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-05
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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
 *   coupling (ASOC). This is a direct consequence of fundamental symmetry
 *   principles in quantum mechanics: when the Hamiltonian lacks inversion
 *   symmetry, the Pauli exclusion principle and spin-orbit interaction
 *   combine to produce an antisymmetric component in the coupling tensor.
 *   This mixing of singlet and triplet pairing channels is not a matter of
 *   experimental technique, material purity, or theoretical interpretation —
 *   it is a mathematical necessity following from the crystal's point group
 *   symmetry. The constraint is verified through three independent
 *   observables: (1) crystallographic determination of the space group
 *   showing lack of inversion center, (2) band structure calculations
 *   revealing the ASOC strength from first principles, and (3) experimental
 *   measurement of parity mixing in the superconducting gap via spectroscopic
 *   probes. All three observables converge on the same structural claim with
 *   ε ≈ 0.08, representing only residual uncertainty in measurement precision
 *   and computational approximations, not ambiguity about the underlying
 *   physics.
 *
 * KEY AGENTS:
 *   - Quantum Theorist: Analytical observer (analytical/analytical) — derives constraint from symmetry principles
 *   - Experimental Physicist: Institutional researcher (institutional/analytical) — measures ASOC effects in real materials
 *   - Materials Engineer: Applied researcher (powerful/analytical) — must work within the constraint when designing quantum devices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(noncentrosymmetric_asoc_coupling, 0.08).
domain_priors:suppression_score(noncentrosymmetric_asoc_coupling, 0.02).
domain_priors:theater_ratio(noncentrosymmetric_asoc_coupling, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, extractiveness, 0.08).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(noncentrosymmetric_asoc_coupling, mountain).
narrative_ontology:human_readable(noncentrosymmetric_asoc_coupling, "Noncentrosymmetric Antisymmetric Spin-Orbit Coupling").
narrative_ontology:topic_domain(noncentrosymmetric_asoc_coupling, "condensed_matter_physics/superconductivity/quantum_materials").

domain_priors:emerges_naturally(noncentrosymmetric_asoc_coupling).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE QUANTUM THEORIST (MOUNTAIN) — Universal physical constraint from symmetry principles
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE EXPERIMENTAL PHYSICIST (MOUNTAIN) — Directly observable in crystal structure and band calculations
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MATERIALS ENGINEER (MOUNTAIN) — Irreducible design constraint in quantum materials
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(noncentrosymmetric_asoc_coupling_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, ExtMetricName, E),
    domain_priors:suppression_score(noncentrosymmetric_asoc_coupling, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(noncentrosymmetric_asoc_coupling),
    narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(noncentrosymmetric_asoc_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ε = 0.08 reflects only the irreducible measurement uncertainty in determining exact ASOC strength and the computational approximations in band structure methods (DFT exchange-correlation functionals, basis set convergence). The physical principle itself — that lack of inversion symmetry enables ASOC — has zero extractiveness; the small residual comes from translating the abstract symmetry constraint into quantitative predictions for specific materials. Suppression σ = 0.02 is near-zero because no alternative formulation exists: once the crystal structure is determined, the presence or absence of ASOC follows deterministically from group theory. Accessibility collapse A = 0.92 is high because the constraint becomes apparent through multiple independent routes (crystallography, band theory, spectroscopy) that all converge. Resistance R = 0.08 is low because attempts to engineer around the constraint (e.g., artificially imposing inversion symmetry through heterostructures) only confirm the underlying principle by showing that ASOC vanishes when symmetry is restored.
 *
 * PERSPECTIVAL GAP:
 *   There is no meaningful perspectival gap. All observers — from the pure theorist deriving selection rules from representation theory, to the experimentalist measuring Rashba splitting in ARPES data, to the engineer designing topological superconducting qubits — classify this as Mountain. The constraint is invariant across power levels, time horizons, and spatial scales because it derives from the mathematical structure of quantum mechanics applied to crystals. The only variation is in practical impact: the theorist sees an elegant consequence of symmetry breaking, the experimentalist sees a measurable signature in their data, and the engineer sees a design constraint (or opportunity, for topological applications). But all agree it is irreducible.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim structure exists. This is a constraint on the space of possible physical states, not a relationship between agents. The crystal structure either has inversion symmetry or it doesn't; if it doesn't, ASOC is present. No agent extracts from another through this mechanism. Materials engineers may benefit from ASOC (enabling topological superconductivity) or be constrained by it (complicating pairing symmetry analysis), but these are consequences of choosing to work with noncentrosymmetric materials, not extraction relationships. The constraint itself is neutral — a feature of the quantum mechanical landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable. With ε = 0.08, this constraint is far below the mandatrophy threshold (ε > 0.70). There is no risk of mislabeling coordination as extraction or vice versa. The constraint is a pure Mountain: it emerges from fundamental physical law (Pauli exclusion + spin-orbit interaction + broken inversion symmetry), has no coordination function (it is not solving a collective action problem), and involves no extraction (no agent is coerced or exploited). The classification is unambiguous across all perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(noncentrosymmetric_asoc_coupling, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is part of the broader family of symmetry-derived selection rules in quantum materials. Related constraints include time-reversal symmetry breaking (enabling Majorana modes), mirror symmetry constraints (determining nodal structure), and point group selection rules (restricting pairing channels). Each is a separate Mountain with its own ε value, but they share the common structure of deriving irreducible physical consequences from crystallographic symmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
