% ============================================================================
% CONSTRAINT STORY: fundamental_material_limits_nbre
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_material_limits_nbre, []).

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
 *   constraint_id: fundamental_material_limits_nbre
 *   human_readable: Fundamental Material Limits of Niobium-Rhenium (NbRe)
 *   domain: physics/materials_science
 *
 * SUMMARY:
 *   Niobium-Rhenium (NbRe) is a superconducting alloy with a critical
 *   temperature (Tc) of approximately 7-9 Kelvin and weak spin-triplet pair
 *   admixture. The fundamental material limits imposed by this alloy's
 *   intrinsic quantum mechanical properties — the electron-phonon coupling
 *   strength, electronic density of states at the Fermi level, and the
 *   resulting superconducting gap structure — create an absolute ceiling on
 *   performance in applications requiring higher critical temperatures or
 *   stronger paramagnetic pair-breaking resistance. These limits are not
 *   engineering constraints that can be overcome through processing,
 *   composition refinement, or application of external fields within the NbRe
 *   alloy system. They are immutable consequences of the fundamental physics
 *   governing superconductivity in this material. The constraint exhibits no
 *   dependence on the observer's power level, exit options, or time
 *   perspective — the critical temperature is what it is, measured in the
 *   same way by every observer, unaffected by institutional resources,
 *   economic incentive, or measurement methodology variation.
 *
 * KEY AGENTS:
 *   - Materials Engineers: Powerless/Trapped — tasked with designing superconducting devices; face absolute Tc ceiling within NbRe system
 *   - Applied Physicists: Powerful/Arbitrage — institutional resources do not overcome the intrinsic limit; arbitrage exit (switching to alternative materials) is real but outside the NbRe domain
 *   - Fundamental Physicists: Analytical/Analytical — observe the constraint as a direct consequence of BCS theory and quantum mechanics; universal across all measurement contexts
 *   - Device Manufacturers: Institutional/Arbitrage — can choose alternative superconductors (Nb3Sn, Nb-Ti) but cannot push NbRe beyond its natural ceiling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_material_limits_nbre, 0.12).
domain_priors:suppression_score(fundamental_material_limits_nbre, 0.02).
domain_priors:theater_ratio(fundamental_material_limits_nbre, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, extractiveness, 0.12).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_material_limits_nbre, mountain).
narrative_ontology:human_readable(fundamental_material_limits_nbre, "Fundamental Material Limits of Niobium-Rhenium (NbRe)").
narrative_ontology:topic_domain(fundamental_material_limits_nbre, "physics/materials_science").

domain_priors:emerges_naturally(fundamental_material_limits_nbre).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATERIALS ENGINEER (MOUNTAIN) — Cannot exceed Tc~8K or meaningfully alter the spin-triplet pair admixture through composition or processing within the NbRe alloy system. The constraint is absolute: fundamental quantum mechanical and thermodynamic limits of the electron-phonon coupling and superconducting gap. No exit, no workaround, no negotiation.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED PHYSICIST (MOUNTAIN) — Even with institutional resources and funding leverage, cannot overcome the intrinsic critical temperature ceiling. The constraint is immutable across all experimental regimes and measurement methodologies. Arbitrage exit is illusory — no alternative alloy composition can solve the same design problem at higher Tc within the NbRe system boundaries.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a fundamental physics perspective, the critical temperature and spin-triplet pair admixture are direct consequences of the Bardeen-Cooper-Schrieffer (BCS) gap equation and the alloy electronic structure. These emerge naturally from quantum mechanics and electromagnetism, with zero degrees of freedom for manipulation. Universal classification across all observation contexts.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_material_limits_nbre_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fundamental_material_limits_nbre, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_material_limits_nbre, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fundamental_material_limits_nbre, ExtMetricName, E),
    domain_priors:suppression_score(fundamental_material_limits_nbre, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fundamental_material_limits_nbre),
    narrative_ontology:constraint_metric(fundamental_material_limits_nbre, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fundamental_material_limits_nbre, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fundamental_material_limits_nbre_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint is purely a physical limit, not an extraction mechanism. No agent benefits from the Tc ceiling; it is a universal asymmetry imposed by nature, not by institutional design. The low value reflects that there is no extraction happening — no actor is harvesting value from the constraint. Suppression (0.02): Negligible. There are no coercive mechanisms suppressing alternatives; the critical temperature is simply observable and immutable. Agents are free to choose different materials or applications outside NbRe. Theater ratio (0.15): Low. Measurement of critical temperature is straightforward, reproducible, and transparent. There is no performative activity masking a degraded function — the property is directly and simply measured. The small theater reflects only the legitimate ambiguity inherent in defining precise experimental conditions (field, frequency, detection criterion) but this is minimal and shared across all superconducting materials.
 *
 * PERSPECTIVAL GAP:
 *   There is no meaningful perspectival gap. All three perspectives classify as Mountain because the constraint is genuinely universal — the critical temperature of NbRe is the same regardless of who measures it, what resources they command, or how long they want to study it. The materials engineer, the applied physicist, and the analytical observer all encounter the same immutable ceiling. This is the defining feature of a true natural law constraint: invariance across power levels, time horizons, and spatial scopes. The perspectival uniformity itself is evidence of the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraints have no directionality. There are no beneficiaries or victims, no extraction flow, no asymmetry in structural position. The critical temperature is not higher for one agent and lower for another. The constraint is observer-independent. In the standard DR framework, this means d is undefined (or equivalently, the chi formula does not apply). The mountain's immutability does not depend on any agent's power level, exit options, or time perspective — it is equally binding on all.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spin_triplet_admixture_mechanism,
    'Is the observed spin-triplet pair admixture in NbRe a fundamental property of the electron-phonon interaction, or an artifact of disorder and defect scattering?',
    'High-purity sample synthesis with controlled defect levels; measurement of gap symmetry via tunneling spectroscopy, nodal quasiparticle excitations, and spin susceptibility as a function of purity',
    'If fundamental: Tc ceiling is harder than calculated by standard BCS theory. If artifact: Tc may be tunable via enhanced purity, though experimental evidence so far contradicts this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spin_triplet_admixture_mechanism, empirical, 'Whether spin-triplet admixture is intrinsic or disorder-dependent').

omega_variable(
    upper_critical_field_scaling,
    'Does the upper critical field (Hc2) follow the theoretical prediction Hc2 ~ sqrt(Tc) for weak-coupling BCS superconductors, or is there evidence of non-standard enhancement mechanisms?',
    'Precision measurements of Hc2 as a function of temperature and composition; comparison with Bardeen-Cooper-Schrieffer predictions and paramagnetic limiting; evaluation of orbital vs. paramagnetic contributions',
    'If standard BCS behavior: Tc ceiling is truly immutable. If anomalous enhancement: potential for engineering higher effective Hc2 without raising Tc, but this does not circumvent the Tc limit itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(upper_critical_field_scaling, empirical, 'Whether Hc2 scaling follows standard BCS prediction').

omega_variable(
    alternative_nbre_phases,
    'Are there alternative crystal phases or stoichiometric ratios of the NbRe system that exhibit higher critical temperature than the currently studied phases?',
    'Phase diagram exploration via solid-state synthesis, X-ray diffraction, and characterization of Tc across the full Nb-Re composition range and crystallographic variants',
    'If higher-Tc phases exist: the ''fundamental limit'' would apply to the current known phase, but the NbRe material class itself would not be fundamentally capped. If no higher-Tc phases: the constraint is binding across the entire NbRe material space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_nbre_phases, empirical, 'Existence of alternative NbRe phases with higher critical temperature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_material_limits_nbre, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nbre_tr_t0, fundamental_material_limits_nbre, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nbre_tr_t5, fundamental_material_limits_nbre, theater_ratio, 5, 0.15).
narrative_ontology:measurement(nbre_tr_t10, fundamental_material_limits_nbre, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(nbre_be_t0, fundamental_material_limits_nbre, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(nbre_be_t5, fundamental_material_limits_nbre, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(nbre_be_t10, fundamental_material_limits_nbre, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_material_limits_nbre, information_standard).
narrative_ontology:affects_constraint(fundamental_material_limits_nbre, quantum_critical_point_superconductivity).
narrative_ontology:affects_constraint(fundamental_material_limits_nbre, majorana_fermion_engineering_constraints).

% DUAL FORMULATION NOTE:
% The fundamental material limits of NbRe are upstream constraints that affect device engineering and alternative material selection downstream. NbRe's Tc ceiling may be superseded by higher-Tc materials (Nb3Sn, MgB2) in specific applications, but within the NbRe material space itself, the constraint is immutable. Downstream constraints on Majorana fermion engineering and quantum critical point physics depend on the availability of suitable superconductors; NbRe's limitations restrict which downstream applications are feasible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
