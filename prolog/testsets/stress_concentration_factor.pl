% ============================================================================
% CONSTRAINT STORY: stress_concentration_factor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stress_concentration_factor, []).

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
 *   constraint_id: stress_concentration_factor
 *   human_readable: Stress Concentration Factor in Material Mechanics
 *   domain: mechanical_engineering/materials_science
 *
 * SUMMARY:
 *   Stress concentration factor (K_t) represents the amplification of nominal
 *   stress at geometric discontinuities relative to the far-field stress
 *   value. When a smooth bar under tension is replaced with a notched bar of
 *   the same cross-sectional area, the peak stress at the notch root exceeds
 *   the nominal stress by a factor K_t that depends purely on the geometry of
 *   the discontinuity and the material's elastic properties. This is not a
 *   regulatory artifact, design choice, or institutional arrangement — it is
 *   a direct consequence of how elastic fields behave around singularities.
 *   The constraint is invariant across all engineering disciplines
 *   (mechanical, civil, aerospace), all material systems (metals, composites,
 *   ceramics), and all time scales within the elasticity regime. It emerges
 *   naturally from the Navier equations and cannot be engineered away without
 *   eliminating the discontinuity.
 *
 * KEY AGENTS:
 *   - Material Scientists: Powerful/mobile — work within this constraint; accept it as a fixed feature of the design landscape and focus on material selection to tolerate concentration rather than eliminating it
 *   - Design Engineers: Institutional/arbitrage — codify stress concentration factors in iterative design processes; benefit from standardized K_t values that enable rapid preliminary calculations
 *   - Standards Bodies (ASME, ISO, DNV): Institutional/arbitrage — document and validate stress concentration factors empirically; derive authority from accuracy of documentation, not from creating the constraint
 *   - Analytical Observer: Analytical/analytical — recognizes stress concentration as a universal consequence of elastic mechanics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stress_concentration_factor, 0.12).
domain_priors:suppression_score(stress_concentration_factor, 0.03).
domain_priors:theater_ratio(stress_concentration_factor, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stress_concentration_factor, extractiveness, 0.12).
narrative_ontology:constraint_metric(stress_concentration_factor, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(stress_concentration_factor, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stress_concentration_factor, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(stress_concentration_factor, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stress_concentration_factor, mountain).
narrative_ontology:human_readable(stress_concentration_factor, "Stress Concentration Factor in Material Mechanics").
narrative_ontology:topic_domain(stress_concentration_factor, "mechanical_engineering/materials_science").

domain_priors:emerges_naturally(stress_concentration_factor).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Stress concentration factor is an irreducible physical consequence of elastic field theory. Geometric discontinuities (notches, holes, fillets) create stress singularities that cannot be eliminated through design revision without removing the discontinuity itself. The stress amplification follows deterministically from the Navier equations. Zero degrees of freedom across all indices.
constraint_indexing:constraint_classification(stress_concentration_factor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of materials innovation, stress concentration is an immutable constraint that must be worked around rather than eliminated. Different material systems (ductile vs brittle, composite vs monolithic) respond differently to stress concentration, but the mathematical relationship itself does not change. Every material engineer accepts this as a fixed feature of the design landscape.
constraint_indexing:constraint_classification(stress_concentration_factor, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Stress concentration factors are codified in ASME, ISO, and DNV standards not as regulatory choices but as factual descriptions of physical phenomena. The standards encode experimental and theoretical data about stress concentration; they do not create the concentration. Standards bodies cannot change this constraint through policy — they can only document it more accurately.
constraint_indexing:constraint_classification(stress_concentration_factor, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stress_concentration_factor_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(stress_concentration_factor, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stress_concentration_factor, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(stress_concentration_factor, ExtMetricName, E),
    domain_priors:suppression_score(stress_concentration_factor, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(stress_concentration_factor),
    narrative_ontology:constraint_metric(stress_concentration_factor, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(stress_concentration_factor, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(stress_concentration_factor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Stress concentration represents a loss of load-carrying capacity — nominal stress values underestimate actual peak stresses, so designers must apply safety factors or use lower nominal stresses than the nominal cross-section would permit. The 'extraction' is the difference between theoretical and actual capacity, but this is not extraction in the sense of one agent benefiting at another's expense. It is a pure loss inherent to geometry. Suppression (0.03): Minimal. The constraint permits complete exit through design revision: eliminate the discontinuity, use tapered transitions, or employ stress-relief geometry. No agent is locked into accepting stress concentration — it is simply the consequence of particular design choices. Theater ratio (0.15): Very low. Stress concentration calculations are entirely functional — K_t values are measured, tabulated, and applied deterministically without ritual or performance. The slight non-zero value (0.15 vs 0.0) reflects that empirical validation of K_t involves test standards and documentation practices, which have minor performative components, but the core function is purely technical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits remarkable uniformity across all perspectives — all three declare Mountain classification because the binding mechanism is identical from all structural positions. The universal applicability is diagnostic evidence that this is a genuine natural law within elasticity theory, not a contingent institutional arrangement. Even the beneficiary perspective (standards bodies that derive authority from documenting K_t) does not argue that the constraint is anything other than a fixed fact. The absence of perspectival gap — the fact that beneficiaries, victims, and analysts all agree on the classification — is itself the signature of a true Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint involves no extraction in the technical sense because it does not flow from one agent to another. Stress concentration is a loss relative to theoretical capacity, but no specific agent captures that loss as a benefit. Standards bodies document K_t but do not benefit from the concentration itself — they benefit from the authority of accurate documentation. Material scientists must design around the concentration but view it as a fixed parameter, not as a transferred resource. The absence of directional flow (no identifiable beneficiary extracting from identifiable victims) is consistent with the Mountain classification — natural laws do not have directionality in the extraction sense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elastic_vs_elastoplastic_regime,
    'Does stress concentration factor K_t (elastic) meaningfully predict fatigue life under elastoplastic deformation?',
    'Correlation analysis between elastic stress concentration predictions and observed fatigue crack initiation in ductile materials across yield limits',
    'If elastic K_t remains predictive: Mountain classification confirmed across material regimes. If elastoplastic redistribution makes K_t non-predictive: The constraint''s binding force shifts from elastic mechanics to material plasticity — potentially opening degrees of freedom in the constraint structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elastic_vs_elastoplastic_regime, empirical, 'Whether elastic stress concentration governs elastoplastic fatigue behavior').

omega_variable(
    gradient_effects_and_scale_dependence,
    'Is stress concentration factor truly scale-invariant, or do gradient effects at microscale and manufacturing tolerance effects introduce functional degrees of freedom?',
    'Systematic study of stress concentration factors across length scales (1mm to 1μm); investigation of how manufacturing tolerances on fillet radius affect measured stress concentration',
    'If truly scale-invariant: Mountain classification holds. If scale-dependent: The constraint becomes technically bounded but practically mobile — design can exploit scale effects to reduce functional concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gradient_effects_and_scale_dependence, empirical, 'Scale and gradient dependence of stress concentration factor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stress_concentration_factor, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scf_tr_t0, stress_concentration_factor, theater_ratio, 0, 0.12).
narrative_ontology:measurement(scf_tr_t25, stress_concentration_factor, theater_ratio, 25, 0.14).
narrative_ontology:measurement(scf_tr_t50, stress_concentration_factor, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(scf_be_t0, stress_concentration_factor, base_extractiveness, 0, 0.11).
narrative_ontology:measurement(scf_be_t25, stress_concentration_factor, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(scf_be_t50, stress_concentration_factor, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stress_concentration_factor, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
