% ============================================================================
% CONSTRAINT STORY: fundamental_material_limits_nbre
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   Niobium-Rhenium (NbRe) is a phonon-mediated superconductor with a
 *   critical temperature Tc of approximately 7-9 Kelvin. This Tc represents a
 *   fundamental material limit imposed by the electron-phonon coupling
 *   strength (λ≈0.78) and the Debye cutoff energy (ω_D≈275 K) of the alloy
 *   system. The constraint arises from first-principles BCS theory: Tc is
 *   exponentially sensitive to the electron-phonon coupling integral, and the
 *   material composition determines this integral completely. Unlike
 *   coordination problems or extractive systems, there is no agent
 *   relationship, no beneficiary or victim, and no exit option. The Tc
 *   ceiling is a natural law boundary condition. The constraint exemplifies
 *   the 'mountain' category: it emerges naturally from the electronic
 *   structure of the material, exhibits near-zero suppression (no coercion
 *   mechanism exists), and zero degrees of freedom for all indices. The
 *   'extractiveness' value of 0.12 reflects the theoretical confidence in BCS
 *   predictions for this material rather than any extraction mechanism — it
 *   quantifies residual uncertainty in whether unconventional pairing
 *   contributions might modestly elevate Tc. The theater ratio of 0.15
 *   indicates minimal performative activity: measurement of Tc is a
 *   straightforward experimental procedure (resistive transition) with direct
 *   physical meaning. The weak spin-triplet pair admixture observed in NbRe
 *   adds a minor omega variable: whether triplet mechanisms could be
 *   amplified through engineering.
 *
 * KEY AGENTS:
 *   - Device Engineers: Powerless actors (trapped/civilizational) — face immutable Tc ceiling in cryogenic applications, cannot negotiate with material physics
 *   - Materials Scientists: Organized research community (constrained/civilizational) — collective research has not and cannot lift the Tc threshold; electron-phonon coupling is structural
 *   - Condensed Matter Theorists: Analytical observers (analytical/analytical) — see the constraint as a natural law predicted by BCS theory with high confidence
 *   - Funding Institutions: Institutional allocators (institutional/analytical) — treat Tc as a scientific boundary condition, not a policy constraint; no funding mechanism changes thermodynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_material_limits_nbre, 0.12).
domain_priors:suppression_score(fundamental_material_limits_nbre, 0.03).
domain_priors:theater_ratio(fundamental_material_limits_nbre, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, extractiveness, 0.12).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, accessibility_collapse, 0.92).
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

% PERSPECTIVE 1: DEVICE ENGINEER (MOUNTAIN) — Cannot escape the Tc=7-9K ceiling regardless of doping, strain engineering, or interface modification strategies. All phonon-mediated superconductivity in NbRe is fundamentally bound by its electron-phonon coupling strength and Debye cutoff. No substitution, pressure cycle, or heterostructure architecture can lift this bound. d≈1.00, f(d)≈1.42, σ=1.0 → χ≈0.17. The constraint is experienced as immutable physical reality.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CONDENSED MATTER THEORIST (MOUNTAIN) — From first principles: BCS theory predicts Tc∝ω_D exp(-1/λ). The electron-phonon coupling constant λ for NbRe is ~0.78 (measured). The Debye cutoff ω_D is ~275 K (from neutron data). These values are structural properties of the elemental composition and crystal lattice. No observer, measurement protocol, or application pressure can change the underlying electron-phonon matrix element. This is natural law for a specific material system. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATERIALS SCIENCE COMMUNITY (MOUNTAIN) — Organized actors (superconductor research consortia, device labs) collectively face the same immutable ceiling. Collaborative research, resource concentration, and technique advancement have not lifted the Tc ceiling in any A15 superconductor. The constraint appears equally immutable whether approached individually or collectively. d≈0.60, f(d)≈0.80, σ=1.2 → χ≈0.10. Community agency cannot negotiate with thermodynamics.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: FUNDING INSTITUTION (MOUNTAIN) — Institutional actors (DOE, NSF, international research councils) allocate resources based on performance expectations. NbRe's 7-9K Tc is a scientific constraint, not a policy constraint. No funding mechanism, priority setting, or collaborative framework can change electron-phonon coupling strength. The institution experiences the constraint as a boundary condition for resource allocation, not a coordination problem. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.01. Negative effective extraction = pure scientific boundary condition.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(institutional),
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
 *   Extractiveness (0.12): Very low. The BCS prediction of Tc from electron-phonon coupling is robust and widely verified across A15 superconductors. The 0.12 value captures residual theoretical uncertainty: whether strong-coupling corrections, unconventional pairing channels, or proximity effects might modestly modify the singlet-only BCS limit. However, no measurement to date has found Tc in NbRe higher than ~9K. The low value reflects high confidence in the fundamental material limit. Suppression (0.03): Negligible. There is no coercive mechanism, alternative suppression, or strategic barrier in the Tc ceiling — it is pure physics. Experimental determination of Tc is transparent, and the constraint is equally visible to all observers. Theater ratio (0.15): Very low. Measurement of superconducting transition temperature is a direct, unambiguous experimental protocol. Resistive transitions show sharp drops; specific heat exhibits the characteristic jump. There is minimal performative activity — the measurement outcome is what it is. Accessibility collapse (0.92): Very high. The constraint is maximally accessible: Tc is the defining property of the superconductor, measured in every characterization campaign. No agent can be ignorant of it. Resistance (0.08): Very low. No meaningful resistance to accepting the Tc ceiling exists — it is observed in every NbRe sample above a certain purity threshold. The constraint cannot be negotiated.
 *
 * PERSPECTIVAL GAP:
 *   This mountain constraint exhibits remarkable perspectival convergence: all four perspectives agree on classification. The device engineer experiences Tc as an immutable ceiling (mountain). The theorist derives Tc from first principles and finds no degree of freedom (mountain). The materials community, collectively approaching the problem, finds the ceiling equally immutable (mountain). The funding institution allocates resources knowing the ceiling is a scientific fact, not a policy lever (mountain). This convergence is the hallmark of true natural law constraints. No agent sees a coordination problem (rope), extraction mechanism (snare), or temporary barrier (scaffold). The constraint is structurally the same from all viewpoints.
 *
 * DIRECTIONALITY LOGIC:
 *   NbRe's fundamental limits constraint has no agent directionality because it has no agent extraction. Device engineers are not 'targets' of the Tc ceiling — the ceiling simply does not apply to them in a predatory sense. They are constrained observers, not victims. Theoretical physicists are not 'beneficiaries' — BCS theory simply explains the limit correctly. The constraint is not extracted FROM engineers TO theorists. All agents occupy the same structural position: they are all observers of a natural boundary condition. The d values assigned (0.5-1.0 across perspectives) reflect the distribution of observational certainty, not directionality. This is correctly handled by assigning d based on the analytical framework (organized community has slightly higher d than institutional, both lower than powerless engineers facing hard constraints in applications). But the key point is: directionality derivation does not apply here, because there is no extraction or coordination function. The mountain's d values are perspectival uncertainty measures, not asymmetric extraction measures.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strong_coupling_regime,
    'Does NbRe exhibit weak-coupling BCS behavior or does strong electron-phonon coupling modify Tc predictions in ways not captured by standard BCS theory?',
    'Detailed spectroscopic measurement: tunneling spectroscopy, photoemission (ARPES), inelastic neutron scattering to extract full Eliashberg function α²F(ω). Comparison of measured Tc with BCS prediction using measured λ and ω_D.',
    'If weak-coupling BCS holds: Tc=7-9K is fully explained and represents a fundamental material limit with no adjustable parameters. If strong-coupling effects dominate: BCS underestimates Tc; there may be exotic pairing mechanisms (spin-orbit coupling enhancement, spin-triplet admixture) that could be engineered. Tc ceiling may not be as rigid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strong_coupling_regime, empirical, 'Whether NbRe exhibits weak-coupling BCS behavior or strong-coupling modifications').

omega_variable(
    spin_triplet_enhancement,
    'Can the weak spin-triplet pair admixture in NbRe be amplified through strain engineering, doping, or proximity effects to increase Tc above the singlet-only prediction?',
    'Measurement of spin-triplet pairing fraction via upper critical field anisotropy (H_c2 parallel vs perpendicular to film axis), Josephson junction critical current modulation under spin-flip scattering, proximity coupling to ferromagnetic contacts. Theoretical calculation of triplet enhancement under proposed strain geometries.',
    'If triplet contribution is negligible (< 1% pairing fraction): singlet-only limit holds, Tc is fundamentally fixed. If triplet can be enhanced to > 5% through engineering: Tc could potentially be lifted by 0.5-2K through triplet mechanism. This would reframe the constraint from immutable mountain to engineerable rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spin_triplet_enhancement, empirical, 'Whether spin-triplet enhancement can raise Tc above singlet-only limits').

omega_variable(
    unconventional_pairing_alternative,
    'Is NbRe strictly a conventional BCS singlet superconductor, or do unconventional pairing mechanisms (charge-density-wave pairing, excitonic mechanisms, or hidden order) contribute to superconductivity?',
    'Symmetry tests: measurement of gap anisotropy via directional tunneling spectroscopy, heat capacity at multiple fields and angles, thermal transport under rotation. Comparison with NbTi (pure BCS) and materials showing unconventional signatures (e.g., heavy fermions). Search for anomalies in specific heat jump, penetration depth, or magnetic susceptibility inconsistent with s-wave symmetry.',
    'If strictly BCS: mountain classification is fully justified. If unconventional mechanisms present: Tc ceiling could be raised by alternative pairing symmetries (d-wave, p-wave nodes might support higher Tc in different geometries). This would decompose the constraint into separate stories: one for singlet limit (mountain), one for achievable superconductivity including multiplet channels (tangled rope or rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unconventional_pairing_alternative, empirical, 'Whether NbRe is strictly conventional BCS or involves unconventional pairing').

omega_variable(
    isotope_effect_anomaly,
    'Does the isotope effect in NbRe follow the standard BCS prediction (Tc∝M^(-1/2)) or does it deviate, suggesting competing pairing mechanisms?',
    'Measure Tc of ⁹²Nb/¹⁸⁵Re and ⁹⁴Nb/¹⁸⁷Re isotopologue samples. Calculate the isotope exponent α = d(ln Tc)/d(ln M). Standard BCS predicts α≈0.5. Deviation > ±0.1 indicates unconventional contributions.',
    'If α≈0.5: purely electron-phonon mechanism confirmed, BCS framework holds, mountain classification robust. If α significantly deviates: multiple pairing mechanisms compete, Tc may not be as rigidly fixed as BCS predicts. Constraint relaxes from mountain to tangled rope (mixed mechanism, some engineering leverage).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(isotope_effect_anomaly, empirical, 'Isotope effect exponent consistency with BCS predictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_material_limits_nbre, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nbre_tr_t0, fundamental_material_limits_nbre, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nbre_tr_t50, fundamental_material_limits_nbre, theater_ratio, 50, 0.15).
narrative_ontology:measurement(nbre_tr_t100, fundamental_material_limits_nbre, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(nbre_be_t0, fundamental_material_limits_nbre, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nbre_be_t50, fundamental_material_limits_nbre, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(nbre_be_t100, fundamental_material_limits_nbre, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_material_limits_nbre, information_standard).
narrative_ontology:affects_constraint(fundamental_material_limits_nbre, superconductor_cryogenic_cost_barrier).
narrative_ontology:affects_constraint(fundamental_material_limits_nbre, niobium_alloy_tunability).

% DUAL FORMULATION NOTE:
% The Tc ceiling for NbRe is an upstream constraint in the superconductor application family. It directly bounds the parameter space available for device engineers and sets the thermodynamic cost floor for any cryogenic application using NbRe. Downstream constraints (cryogenic cost barrier, alloy tunability) inherit this Tc limit as a structural given.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
