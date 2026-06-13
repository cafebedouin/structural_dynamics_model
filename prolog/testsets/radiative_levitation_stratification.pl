% ============================================================================
% CONSTRAINT STORY: radiative_levitation_stratification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_radiative_levitation_stratification, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: radiative_levitation_stratification
 *   human_readable: Radiative Levitation Atmospheric Stratification in Magnetic Chemically Peculiar Stars
 *   domain: astrophysics/stellar_spectroscopy/nuclear_physics
 *
 * SUMMARY:
 *   Radiative levitation is the physical mechanism by which radiation
 *   pressure from a star's photon field, combined with gravitational settling
 *   and magnetic field geometry, stratifies atmospheric ions by atomic number
 *   in slowly rotating magnetic chemically peculiar (CP) stars. The mechanism
 *   was proposed in the 1970s to explain extreme abundance anomalies observed
 *   spectroscopically and has been validated across hundreds of CP stars.
 *   This story models the stratification mechanism itself as a Mountain
 *   constraint — a structural feature of stellar atmospheres under specific
 *   conditions (strong magnetic field, slow rotation, sufficient radiation
 *   pressure). The actinide replenishment question (how short-lived
 *   radioactive elements persist in a billion-year-old star) is a SEPARATE
 *   constraint with contested readings; this story does NOT adjudicate that
 *   question. It models only the levitation mechanism that concentrates
 *   whatever elements are present into observable layers. KEY AGENTS (by
 *   structural relationship): - Stellar evolution theorists
 *   (organized/mobile): Beneficiaries — use validated mechanism in atmosphere
 *   models - Atomic physics laboratories (institutional/mobile):
 *   Beneficiaries — gain empirical validation of radiative force calculations
 *   - High-resolution spectroscopy facilities (institutional/mobile):
 *   Beneficiaries — justify instrument development through observability -
 *   Observational astronomers (organized/analytical): Observers — document
 *   the mechanism's operation - Alternative mechanism proponents
 *   (moderate/mobile): Excluded — evidentially, not structurally
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(radiative_levitation_stratification, 0.03).
domain_priors:suppression_score(radiative_levitation_stratification, 0.02).
domain_priors:theater_ratio(radiative_levitation_stratification, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(radiative_levitation_stratification, extractiveness, 0.03).
narrative_ontology:constraint_metric(radiative_levitation_stratification, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(radiative_levitation_stratification, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(radiative_levitation_stratification, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(radiative_levitation_stratification, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(radiative_levitation_stratification, mountain).
narrative_ontology:human_readable(radiative_levitation_stratification, "Radiative Levitation Atmospheric Stratification in Magnetic Chemically Peculiar Stars").
narrative_ontology:topic_domain(radiative_levitation_stratification, "astrophysics/stellar_spectroscopy/nuclear_physics").

domain_priors:emerges_naturally(radiative_levitation_stratification).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(radiative_levitation_stratification, '65f3484b-e759-409e-b809-9568272d0c5e').
narrative_ontology:cs_kernel_codification('65f3484b-e759-409e-b809-9568272d0c5e', formalized).
narrative_ontology:cs_authority_grounding('65f3484b-e759-409e-b809-9568272d0c5e', expertise).
narrative_ontology:cs_interpretation_layer_present('65f3484b-e759-409e-b809-9568272d0c5e').
narrative_ontology:cs_axiom('65f3484b-e759-409e-b809-9568272d0c5e', foundational, photon_momentum_transfer_to_ions).
narrative_ontology:cs_axiom_status(photon_momentum_transfer_to_ions, holdable).
narrative_ontology:cs_axiom_grounding('65f3484b-e759-409e-b809-9568272d0c5e', photon_momentum_transfer_to_ions, empirically_contingent).
narrative_ontology:cs_axiom('65f3484b-e759-409e-b809-9568272d0c5e', foundational, opacity_dependent_radiative_force).
narrative_ontology:cs_axiom_status(opacity_dependent_radiative_force, holdable).
narrative_ontology:cs_axiom_grounding('65f3484b-e759-409e-b809-9568272d0c5e', opacity_dependent_radiative_force, empirically_contingent).
narrative_ontology:cs_axiom('65f3484b-e759-409e-b809-9568272d0c5e', secondary, magnetic_field_channels_stratification).
narrative_ontology:cs_axiom_status(magnetic_field_channels_stratification, holdable).
narrative_ontology:cs_axiom_grounding('65f3484b-e759-409e-b809-9568272d0c5e', magnetic_field_channels_stratification, empirically_contingent).
narrative_ontology:cs_reference_frame('65f3484b-e759-409e-b809-9568272d0c5e', radiative_pressure_equilibrium_framework).
narrative_ontology:cs_drift_state('65f3484b-e759-409e-b809-9568272d0c5e', contemporary_high_resolution_spectroscopy_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('65f3484b-e759-409e-b809-9568272d0c5e', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(radiative_levitation_stratification, stellar_evolution_theorists).
narrative_ontology:constraint_beneficiary(radiative_levitation_stratification, atomic_physics_laboratories).
narrative_ontology:constraint_beneficiary(radiative_levitation_stratification, high_resolution_spectroscopy_facilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use radiative levitation as a validated mechanism in stellar atmosphere models. The constraint's operation vindicates their theoretical framework for chemically peculiar stars and provides a natural laboratory for testing atomic physics predictions. They benefit from the constraint's predictive power without extracting rents from its operation.
narrative_ontology:constraint_stakeholder(radiative_levitation_stratification, stellar_evolution_theorists, beneficiary,
    organized, generational, mobile, global).

% Gain empirical validation of atomic transition probabilities and radiative force calculations from stellar observations. The stratification provides a natural experiment at conditions unattainable in terrestrial labs. Their theoretical predictions are tested against observed abundance patterns, refining atomic data tables used across physics.
narrative_ontology:constraint_stakeholder(radiative_levitation_stratification, atomic_physics_laboratories, beneficiary,
    institutional, generational, mobile, global).

% Justify continued funding and instrument development by demonstrating capability to resolve fine spectral features that reveal stratification. The constraint's observability drives demand for higher resolution and precision, but facilities compete on technical merit rather than gatekeeping access to the phenomenon.
narrative_ontology:constraint_stakeholder(radiative_levitation_stratification, high_resolution_spectroscopy_facilities, beneficiary,
    institutional, biographical, mobile, global).

% Measure spectral line strengths, magnetic field configurations, and rotational velocities to map atmospheric composition. They document the constraint's operation through systematic observation campaigns, providing the empirical foundation for theoretical models without being constrained by the mechanism itself.
narrative_ontology:constraint_stakeholder(radiative_levitation_stratification, observational_astronomers, observer,
    organized, biographical, analytical, global).

% Propose competing explanations for observed abundance anomalies (diffusion without radiation pressure, mass loss, binary mass transfer). They are not structurally excluded from the discourse but find their mechanisms insufficient to explain the full pattern of stratification, particularly the correlation with magnetic field strength and slow rotation.
narrative_ontology:constraint_stakeholder(radiative_levitation_stratification, alternative_mechanism_proponents, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this is a physical mechanism, not a coordination arrangement. Radiation pressure from stellar photons, combined with gravitational settling and magnetic field geometry, naturally stratifies atmospheric ions by their radiative opacity.
% TRANSFER_FUNCTION: Photon momentum transfers to ions with strong spectral lines at the local radiation field wavelengths, levitating them against gravity. Ions with weaker opacity sink. The transfer is photon-to-ion momentum, not economic or social.
% ABSENT_VOICES: No voices are structurally absent — the mechanism is accessible to any observer with spectroscopic capability. Alternative mechanism proponents participate fully in the discourse; their exclusion is evidential, not structural.
% DISAPPEARANCE_RATIONALE: If the theoretical framework describing radiative levitation disappeared, the physical stratification would continue unchanged. Stars would still exhibit the same abundance patterns, magnetic fields would still channel radiation pressure, and ions would still sort by atomic number. Observers would rediscover the mechanism from the same spectroscopic data.
% FOUNDING_PROBLEM: Early 20th century spectroscopy revealed chemically peculiar stars with extreme abundance anomalies (factors of 100-10,000 above solar) that violated assumptions of well-mixed stellar atmospheres. The founding problem was explaining how these anomalies arise and persist.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live because new chemically peculiar stars are continuously discovered and each requires detailed modeling. Independent corroboration comes from atomic physics laboratories (confirming radiative force calculations), stellar evolution modelers outside the CP star community (incorporating diffusion into broader stellar models), and high-energy physics (validating atomic data through accelerator experiments). No party benefits from the problem's persistence — it is a genuine open question in stellar physics.
narrative_ontology:disappearance_verdict(radiative_levitation_stratification, world_unchanged).
narrative_ontology:founding_problem_status(radiative_levitation_stratification, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(radiative_levitation_stratification, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(radiative_levitation_stratification, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(radiative_levitation_stratification_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(radiative_levitation_stratification, ExtMetricName, E),
    domain_priors:suppression_score(radiative_levitation_stratification, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(radiative_levitation_stratification),
    narrative_ontology:constraint_metric(radiative_levitation_stratification, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(radiative_levitation_stratification, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(radiative_levitation_stratification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.03) because the mechanism operates independently of human institutional arrangements. The minimal extraction reflects only the concentration of research funding toward facilities capable of observing the effect, which is a second-order consequence, not the mechanism itself. Suppression is near-zero (0.02) because alternative explanations are tested openly and fail on empirical grounds, not institutional gatekeeping. Theater ratio is minimal (0.05) because the mechanism is directly observable through spectral line ratios and magnetic field measurements — there is negligible performative maintenance. Accessibility collapse is very high (0.92) because once the physics of radiative pressure, atomic opacity, and magnetic field geometry is understood, alternative explanations for the observed stratification patterns collapse nearly completely. Resistance is very low (0.04) because the mechanism is not defended against challengers — it is simply measured. The measurements are flat across the interval because the physical mechanism is time-invariant; the constraint's operation does not drift.
 *
 * PERSPECTIVAL GAP:
 *   There is no meaningful perspectival gap because all seats observe the same physical mechanism. The beneficiary seats (theorists, atomic physicists, facilities) experience the constraint as a validated natural law that enables their work. The observer seat documents it empirically. The excluded seat (alternative mechanism proponents) experiences evidential exclusion, not structural extraction — their mechanisms fail to match observations. The engine should compute Mountain classification from all seats because the structural data (near-zero extraction, near-zero suppression, very high accessibility collapse, very low resistance) is invariant across perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All stakeholders sit near the beneficiary end of the directionality spectrum. Theorists, atomic physicists, and spectroscopy facilities benefit from the constraint's predictive power and observability without being constrained by it. Observational astronomers occupy the analytical seat — they measure the mechanism without being subject to it. Alternative mechanism proponents are mobile — they can propose competing explanations and test them empirically. No seat is trapped or identity-locked because the mechanism is a feature of stellar physics, not a social arrangement. The engine should compute very low effective extraction for all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply because this is a physical mechanism, not a coordination arrangement with a founding mandate. The 'founding problem' (explaining chemically peculiar star abundances) remains live because new CP stars are continuously discovered and each requires detailed modeling. The mechanism's persistence is not institutional inertia — it is the continued operation of radiation pressure in stellar atmospheres. The constraint cannot outlive its function because its function IS its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_structure_ambiguity,
    'Does declaring beneficiaries on a Mountain constraint (stellar evolution theorists, atomic physics labs, spectroscopy facilities) constitute false summit authoring, or is this a genuine natural law where beneficiaries exist but do not shape the constraint''s operation?',
    'Examine whether the identified beneficiaries have any structural influence over the constraint''s operation or persistence. If research funding, facility access, or theoretical consensus could alter the physical mechanism of radiative levitation, it is a false summit. If the mechanism operates independently of all human institutional arrangements, beneficiaries exist but do not compromise Mountain status.',
    'If beneficiaries structurally influence the constraint, FSM triggers and the engine reclassifies to tangled_rope. If beneficiaries exist but have zero structural influence, the constraint remains Mountain and the beneficiary declarations document second-order effects (funding concentration, research agenda) without compromising the natural law claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, conceptual, 'Whether beneficiary presence on a physical mechanism constitutes false summit or documents genuine but non-extractive benefit.').

omega_variable(
    actinide_replenishment_decomposition,
    'Is the actinide replenishment question (neutron star bombardment vs. superheavy decay vs. artifact disposal) part of THIS constraint or a separate constraint that should be decomposed per the ε-invariance principle?',
    'Apply the ε-invariance test: if measuring the constraint via ''stratification mechanism'' gives negligible extraction but measuring via ''actinide source'' gives contested/extractive readings, they are two constraints. The stratification mechanism (how ions sort by atomic number) is distinct from the replenishment mechanism (how short-lived isotopes persist). Decompose into separate stories linked by network.affects_constraints.',
    'If decomposed, this story models only the levitation mechanism (Mountain, negligible extraction) and a sibling story models the actinide source question (likely contested, higher extraction if readings are institutionally defended). If not decomposed, the single story must handle observable-dependent classification, violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actinide_replenishment_decomposition, conceptual, 'Whether actinide replenishment is part of this constraint or requires decomposition.').

omega_variable(
    magnetic_field_necessity,
    'Is the strong magnetic field (kG-scale) strictly necessary for radiative levitation stratification, or does it only enhance an effect that would occur (more weakly) in non-magnetic stars?',
    'Systematic comparison of abundance anomalies in magnetic vs. non-magnetic slowly rotating A-type stars. If non-magnetic slow rotators show similar (but weaker) stratification, the magnetic field is an amplifier. If they show no stratification, the field is necessary.',
    'If the field is necessary, the constraint applies only to magnetic CP stars (narrower scope, higher accessibility collapse). If it is an amplifier, the constraint applies more broadly but with variable strength (wider scope, lower accessibility collapse). This affects the scope modifier in the effective extraction calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magnetic_field_necessity, empirical, 'Whether magnetic field is necessary condition or amplifying factor for stratification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(radiative_levitation_stratification, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(radi_tr_t0, radiative_levitation_stratification, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(radi_tr_t0, observed).
narrative_ontology:measurement(radi_tr_t20, radiative_levitation_stratification, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(radi_tr_t20, observed).
narrative_ontology:measurement(radi_tr_t40, radiative_levitation_stratification, theater_ratio, 40, 0.05).
narrative_ontology:measurement_basis(radi_tr_t40, observed).
narrative_ontology:measurement(radi_tr_t60, radiative_levitation_stratification, theater_ratio, 60, 0.05).
narrative_ontology:measurement_basis(radi_tr_t60, observed).
narrative_ontology:measurement(radi_tr_t80, radiative_levitation_stratification, theater_ratio, 80, 0.05).
narrative_ontology:measurement_basis(radi_tr_t80, observed).
narrative_ontology:measurement(radi_tr_t100, radiative_levitation_stratification, theater_ratio, 100, 0.05).
narrative_ontology:measurement_basis(radi_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(radi_be_t0, radiative_levitation_stratification, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(radi_be_t0, observed).
narrative_ontology:measurement(radi_be_t20, radiative_levitation_stratification, base_extractiveness, 20, 0.03).
narrative_ontology:measurement_basis(radi_be_t20, observed).
narrative_ontology:measurement(radi_be_t40, radiative_levitation_stratification, base_extractiveness, 40, 0.03).
narrative_ontology:measurement_basis(radi_be_t40, observed).
narrative_ontology:measurement(radi_be_t60, radiative_levitation_stratification, base_extractiveness, 60, 0.03).
narrative_ontology:measurement_basis(radi_be_t60, observed).
narrative_ontology:measurement(radi_be_t80, radiative_levitation_stratification, base_extractiveness, 80, 0.03).
narrative_ontology:measurement_basis(radi_be_t80, observed).
narrative_ontology:measurement(radi_be_t100, radiative_levitation_stratification, base_extractiveness, 100, 0.03).
narrative_ontology:measurement_basis(radi_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(radi_su_t0, radiative_levitation_stratification, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(radi_su_t0, observed).
narrative_ontology:measurement(radi_su_t20, radiative_levitation_stratification, suppression_requirement, 20, 0.02).
narrative_ontology:measurement_basis(radi_su_t20, observed).
narrative_ontology:measurement(radi_su_t40, radiative_levitation_stratification, suppression_requirement, 40, 0.02).
narrative_ontology:measurement_basis(radi_su_t40, observed).
narrative_ontology:measurement(radi_su_t60, radiative_levitation_stratification, suppression_requirement, 60, 0.02).
narrative_ontology:measurement_basis(radi_su_t60, observed).
narrative_ontology:measurement(radi_su_t80, radiative_levitation_stratification, suppression_requirement, 80, 0.02).
narrative_ontology:measurement_basis(radi_su_t80, observed).
narrative_ontology:measurement(radi_su_t100, radiative_levitation_stratification, suppression_requirement, 100, 0.02).
narrative_ontology:measurement_basis(radi_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(radiative_levitation_stratification, information_standard).
narrative_ontology:boltzmann_floor_override(radiative_levitation_stratification, 0.01).
narrative_ontology:affects_constraint(radiative_levitation_stratification, actinide_replenishment_mechanism).

% DUAL FORMULATION NOTE:
% This story models the radiative levitation stratification mechanism itself (how atmospheric ions sort by atomic number given radiation pressure, gravity, and magnetic field geometry). The actinide replenishment question (how short-lived radioactive isotopes persist in a billion-year-old star) is a SEPARATE constraint that DEPENDS ON this one (the replenishment mechanism must explain how actinides reach the stratified layers where they are observed). Per the ε-invariance principle, these are distinct constraints: the stratification mechanism has negligible extraction (physical law), while the replenishment mechanism has contested readings with potentially higher extraction (institutional defense of competing explanations). Link via network.affects_constraints rather than folding both into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
