% ============================================================================
% CONSTRAINT STORY: noncentrosymmetric_asoc_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
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
 *   Noncentrosymmetric antisymmetric spin-orbit coupling (ASOC) is a
 *   structural constraint arising from the absence of spatial inversion
 *   symmetry in the crystal lattice combined with relativistic effects in the
 *   electronic Hamiltonian. This is a pure Mountain constraint — a natural
 *   law in the domain of condensed matter physics. The constraint is
 *   irreducible: if a crystal structure lacks inversion symmetry,
 *   relativistic quantum mechanics guarantees the presence of antisymmetric
 *   spin-orbit coupling in the low-energy electronic structure. The
 *   constraint does not extract value from any agent, does not suppress
 *   alternatives (scientists can choose different materials with different
 *   symmetries), and does not require enforcement. It simply is a consequence
 *   of the fundamental laws of physics applied to a specific structural
 *   configuration. All perspectives — crystallographic, theoretical,
 *   experimental, and institutional — converge on the same classification
 *   because the constraint's status as a natural law is invariant across all
 *   observation contexts.
 *
 * KEY AGENTS:
 *   - The Crystal Lattice: Not an agent but the physical substrate. The inversion symmetry (or lack thereof) is an objective property of the material structure.
 *   - The Relativistic Electron: The quantum degree of freedom experiencing the spin-orbit coupling. Not an agent in the DR sense; the coupling acts on the electron's spinor structure.
 *   - Theoretical Physicists: Agents who understand the constraint through first-principles calculations. They derive coupling strength from crystal structure and relativistic corrections. d≈0.50 (mobile within the constraint—choose materials, don't choose coupling).
 *   - Experimental Materials Scientists: Agents who measure and characterize ASOC through ARPES, magneto-transport, and other probes. d≈0.50 (constrained by the materials they choose, but free to choose different materials).
 *   - Research Program Directors: Institutional agents funding materials discovery and characterization. d≈0.05 (beneficiary—the constraint enables noncentrosymmetric superconductivity as a research field).
 *   - The Analytical Observer: Universal/civilizational perspective. d≈0.72, but the mountain classification is not perspective-dependent in this case—it holds across all perspectives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(noncentrosymmetric_asoc_coupling, 0.12).
domain_priors:suppression_score(noncentrosymmetric_asoc_coupling, 0.03).
domain_priors:theater_ratio(noncentrosymmetric_asoc_coupling, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, extractiveness, 0.12).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, theater_ratio, 0.15).

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

% PERSPECTIVE 1: CRYSTALLOGRAPHIC OBSERVER (MOUNTAIN) — The absence of inversion symmetry is a structural property of the crystal lattice itself. No observer, actor, or agent can negotiate this symmetry absence into existence or suppress it. ε=0.12, accessibility_collapse=0.92. This is a geometric fact of the material structure.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THEORETICAL PHYSICIST (MOUNTAIN) — From the Hamiltonian level, antisymmetric spin-orbit coupling is a consequence of relativistic quantum mechanics applied to a non-centrosymmetric lattice. The constraint derives from first principles: relativistic corrections to the Dirac equation in a symmetry-broken potential. No experiment can refute the mathematical relationship; only confirm or refute whether specific materials realize the predicted coupling strength. ε=0.12, resistance=0.08. The constraint is logically irreducible.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: EXPERIMENTAL MATERIALS SCIENTIST (MOUNTAIN) — Even when designing materials, one cannot escape this constraint. Choosing a non-centrosymmetric crystal structure automatically invokes antisymmetric spin-orbit coupling. One can choose different materials or symmetries, but the coupling is intrinsic to the choice made. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08. The scientist has exit at the material design level, but at each chosen material, the constraint is absolute.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RESEARCH PROGRAM DIRECTOR (MOUNTAIN) — From an institutional view, the constraint enables research directions (noncentrosymmetric superconductivity as a field) but the fundamental constraint itself is not negotiable. One can fund research on topological superconductors, unconventional pairing mechanisms, or synthetic materials that exploit ASOC, but the underlying relationship between symmetry breaking and spin-orbit coupling persists. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.01. Institutional actors benefit from research programs enabled by the constraint.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

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
 *   Base extractiveness (ε = 0.12): Very low. The constraint does not extract resources, wealth, or opportunity from any agent. No one is worse off because ASOC exists. Scientists can choose materials with or without the coupling. The low ε reflects that this is a physical relationship, not a rent-extraction mechanism. Suppression (0.03): Negligible. There are no alternatives being suppressed. Inversion-symmetric materials exist as alternatives; noncentrosymmetric materials have different properties and different applications, but neither is 'suppressed' by the constraint. Theater ratio (0.15): Low. While there is some performative language in how ASOC effects are discussed (e.g., framing all interesting superconductivity as arising from ASOC when it may be secondary), the core constraint is mathematically rigorous and experimentally testable. The theater is minimal — mostly the inevitable scientific framing and presentation. Accessibility collapse (0.92): Very high. The constraint is accessible to any observer with knowledge of crystal structures and quantum mechanics. There are no hidden dimensions or exceptional cases. The relationship between inversion symmetry and ASOC is fully transparent. Resistance (0.08): Very low. The constraint faces no resistance from any agent or mechanism. It cannot be negotiated with, evaded, or mitigated.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on the same classification: Mountain. This is not a perspectival gap but a perspectival convergence — a signature of a true natural law. The crystallographer sees it as a structural fact. The theorist sees it as a mathematical consequence. The experimentalist sees it as an inescapable empirical property of chosen materials. The director sees it as enabling a research field. The analytical observer sees it as a universally invariant constraint. There is no disagreement because there is no extractive mechanism, no suppression of alternatives, and no asymmetry in who bears costs. This is the ideal case for mountain classification: the same base properties and logical structure yield the same type across all (P, T, E, S) tuples.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraints do not require directionality analysis because they do not extract value or suppress alternatives. All agents have the same structural relationship to the constraint: they encounter it as an irreducible physical fact. The experimental scientist could choose a centrosymmetric material and avoid ASOC entirely, making their exit option 'constrained' only at the level of 'if you choose this material, this property follows.' At the level of material selection, exit is mobile. But within the set of noncentrosymmetric materials, the coupling is universal and inescapable. This creates a special case: the scientist's power is moderate (they choose the material class) and their exit is constrained (within the chosen class, ASOC is unavoidable), yet no extraction occurs because the constraint is purely a structural consequence of the choice, not a mechanism that benefits someone at the scientist's expense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asoc_strength_measurement,
    'Is the measured magnitude of antisymmetric spin-orbit coupling consistent with relativistic predictions from first principles, or do material-specific effects systematically enhance or suppress it?',
    'Comparative analysis across material families (CePt3Si, Li2Pt3B, Li2Pd3B, etc.); ab initio calculations vs experimental spin splitting measurements (from ARPES, magneto-transport, muon spin rotation); systematic comparison of predicted vs observed λ_ASOC',
    'If consistent with relativistic theory: confirms mountain classification at all perspectives. If systematic deviations: may reveal a constraint family (ASOC as relativistic limit + material-specific modulation as separate constraint with higher ε)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asoc_strength_measurement, empirical, 'Consistency of measured ASOC magnitude with relativistic predictions').

omega_variable(
    centrosymmetry_breaking_mechanism,
    'Is the loss of inversion symmetry a fundamental structural property or a low-energy effective description of a more complex structure?',
    'High-resolution X-ray crystallography, transmission electron microscopy at atomic resolution, comparison of nominal and refined crystal structures; test for hidden symmetries or weak ordering that preserves inversion at higher temperatures',
    'If fundamental: strengthens mountain classification; symmetry is irreducible. If effective: suggests a constraint family where true centrosymmetry + symmetry-breaking phase transition constitute separate constraints with different ε values and timescales',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(centrosymmetry_breaking_mechanism, empirical, 'Whether loss of inversion symmetry is fundamental or emergent').

omega_variable(
    asoc_observability_boundary,
    'What is the minimum coupling strength λ_ASOC below which experimental signatures become indistinguishable from noise and finite-size effects?',
    'Systematic study of materials with predicted ASOC values spanning 2-3 orders of magnitude; measurement sensitivity limits across ARPES, magneto-transport, and susceptibility probes; statistical power analysis of existing datasets',
    'If boundary is above the weak-coupling regime: ASOC effects are always observable in principle. If boundary overlaps with calculated values for some materials: creates a domain where ASOC is theoretically present but experimentally inaccessible — potential false-summit scenario for mountain classification',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asoc_observability_boundary, empirical, 'Observability threshold for antisymmetric spin-orbit coupling effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(noncentrosymmetric_asoc_coupling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nasoc_tr_t0, noncentrosymmetric_asoc_coupling, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nasoc_tr_t10, noncentrosymmetric_asoc_coupling, theater_ratio, 10, 0.15).
narrative_ontology:measurement(nasoc_tr_t20, noncentrosymmetric_asoc_coupling, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(nasoc_be_t0, noncentrosymmetric_asoc_coupling, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(nasoc_be_t10, noncentrosymmetric_asoc_coupling, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(nasoc_be_t20, noncentrosymmetric_asoc_coupling, base_extractiveness, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(noncentrosymmetric_asoc_coupling, information_standard).
narrative_ontology:affects_constraint(noncentrosymmetric_asoc_coupling, inverse_spin_valve_signature).
narrative_ontology:affects_constraint(noncentrosymmetric_asoc_coupling, verification_bottleneck).

% DUAL FORMULATION NOTE:
% ASOC in noncentrosymmetric superconductors forms a constraint family with upper and lower bounds. (1) The fundamental constraint (this story) is the mathematical and relativistic relationship between inversion symmetry breaking and the presence of antisymmetric spin-orbit coupling in the Hamiltonian—a pure mountain. (2) The observability of ASOC effects in specific materials may depend on coupling strength and measurement sensitivity; if observability becomes the constraint, a separate story would capture the experimental verification challenge (higher ε, potentially Tangled Rope). (3) The interpretation of ASOC as the mechanism for exotic superconducting properties (nodal gaps, topological features) invokes a third constraint story with its own ε (currently categorized under topological_superconductivity_claims). This story addresses only the fundamental constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
