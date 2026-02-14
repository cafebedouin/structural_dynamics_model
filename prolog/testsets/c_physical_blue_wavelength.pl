% ============================================================================
% CONSTRAINT STORY: c_physical_blue_wavelength
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-01
% ============================================================================

:- module(constraint_c_physical_blue_wavelength, []).

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: c_physical_blue_wavelength
 *   human_readable: The Physical Wavelength of Blue Light
 *   domain: scientific/physical
 *
 * SUMMARY:
 *   The perception of "blue" is constrained by the physical properties of the
 *   electromagnetic spectrum, specifically light with a wavelength of ~450-495nm.
 *   This is a fundamental, unchangeable property of the universe as we know it.
 *   It is a classic example of a Mountain: a natural law with zero degrees of
 *   freedom for all agents, regardless of their power or perspective.
 *
 * KEY AGENTS (by structural relationship):
 *   - Human Observer: Any agent subject to the laws of physics (powerless/trapped)
 *   - Display Manufacturer: Institutional agent building standards upon the law (institutional/mobile)
 *   - Physicist: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Mountains have near-zero extraction (it's a law, not a tax) and low suppression.
domain_priors:base_extractiveness(c_physical_blue_wavelength, 0.02).
domain_priors:suppression_score(c_physical_blue_wavelength, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(c_physical_blue_wavelength, 0.0).        % Purely functional physical law, zero theater.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(c_physical_blue_wavelength, extractiveness, 0.02).
narrative_ontology:constraint_metric(c_physical_blue_wavelength, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(c_physical_blue_wavelength, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification, and the mountain metric gate will not fire.
narrative_ontology:constraint_metric(c_physical_blue_wavelength, accessibility_collapse, 1.0). % Alternatives are inconceivable.
narrative_ontology:constraint_metric(c_physical_blue_wavelength, resistance, 0.0). % Resistance is incoherent.

% --- Constraint claim (must match analytical perspective type) ---
% A physical law is the canonical example of a Mountain claim.
narrative_ontology:constraint_claim(c_physical_blue_wavelength, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the laws of physics without human design.
domain_priors:emerges_naturally(c_physical_blue_wavelength).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% This constraint is a physical law; it has no beneficiaries or victims in the
% Deferential Realism sense, as there is no asymmetric extraction. It also
% requires no active enforcement. These facts are omitted as they are not applicable.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   This is a uniform-type constraint (Mountain-only). The classification is
   invariant across all perspectives because the base extractiveness and
   suppression are negligible and the constraint is a physical law.
   ========================================================================== */

% PERSPECTIVE 1: THE HUMAN OBSERVER (MOUNTAIN)
% To the observer, the sky is blue due to Rayleigh scattering; this cannot be
% changed or negotiated.
constraint_indexing:constraint_classification(c_physical_blue_wavelength, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE INSTITUTION (MOUNTAIN)
% A standards body (e.g., defining sRGB) cannot change the physics of blue light.
% They can only build a coordination standard (a Rope) that REFERENCES this Mountain.
% From their perspective, the physical law itself remains a Mountain.
constraint_indexing:constraint_classification(c_physical_blue_wavelength, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The physicist observes a fundamental, non-negotiable law of nature.
constraint_indexing:constraint_classification(c_physical_blue_wavelength, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(c_physical_blue_wavelength_tests).

test(uniformity_is_mountain_as_expected) :-
    % Verify this is a uniform-type constraint, classifying as Mountain from all key perspectives.
    constraint_indexing:constraint_classification(c_physical_blue_wavelength, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(c_physical_blue_wavelength, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(c_physical_blue_wavelength, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the constraint adheres to the canonical Mountain thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(c_physical_blue_wavelength, ExtMetricName, E),
    narrative_ontology:constraint_metric(c_physical_blue_wavelength, SuppMetricName, S),
    E =< 0.25, % Mountain extraction threshold
    S =< 0.05. % Mountain suppression ceiling

test(nl_profile_is_present_and_correct) :-
    % Verify the Natural Law profile metrics are present for this Mountain constraint.
    narrative_ontology:constraint_metric(c_physical_blue_wavelength, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(c_physical_blue_wavelength, resistance, R),
    domain_priors:emerges_naturally(c_physical_blue_wavelength),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(c_physical_blue_wavelength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a uniform-type Mountain. A physical law is the canonical
 *   example of a Mountain; it has negligible extraction (ε=0.02) and its
 *   suppression of alternatives is total and requires no enforcement. The
 *   suppression score (S=0.01) is low because the metric measures the *cost*
 *   of suppressing alternatives, which is zero for a physical law as no
 *   alternatives exist. The file includes the full Natural Law profile
 *   (accessibility_collapse=1.0, resistance=0.0, emerges_naturally) required
 *   for the engine to certify it as a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The constraint is a Mountain from all perspectives,
 *   as no agent can alter a law of physics. This is a uniform-type constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable. As a Mountain, the constraint has no
 *   beneficiaries or victims. The system will fall back to canonical `d` values
 *   for each power atom, but since ε is near zero, the effective extraction χ
 *   remains negligible for all agents.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This constraint is a Mountain. Mandatrophy, the misidentification
 *   of extraction as coordination, is not a risk here because the base extractiveness (0.02)
 *   is well below any threshold for a Snare or Tangled Rope. The system correctly
 *   identifies this as a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_blue_qualia,
    'Is the subjective experience of blue (qualia) a physical Mountain derived from this constraint, or a constructed Snare of neuro-linguistic programming?',
    'Advances in the hard problem of consciousness; empirical studies on qualia across different linguistic groups.',
    'If qualia is a Mountain, color perception is universal. If a Snare, it implies perception can be manipulated or is culturally relative.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(c_physical_blue_wavelength, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is included for completeness, but as a physical law, its
% properties are static over human timescales. Extraction is below the 0.46
% threshold requiring this data, but its inclusion demonstrates stability.

% Theater ratio remains zero (Physical laws do not perform "theater")
narrative_ontology:measurement(blue_tr_t0, c_physical_blue_wavelength, theater_ratio, 0, 0.0).
narrative_ontology:measurement(blue_tr_t5, c_physical_blue_wavelength, theater_ratio, 5, 0.0).
narrative_ontology:measurement(blue_tr_t10, c_physical_blue_wavelength, theater_ratio, 10, 0.0).

% Extraction remains negligible (Nature does not collect rent)
narrative_ontology:measurement(blue_ex_t0, c_physical_blue_wavelength, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(blue_ex_t5, c_physical_blue_wavelength, base_extractiveness, 5, 0.02).
narrative_ontology:measurement(blue_ex_t10, c_physical_blue_wavelength, base_extractiveness, 10, 0.02).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% As a physical law, this constraint has no coordination function itself, so
% coordination_type is not declared. However, it serves as a foundation for
% other, human-constructed constraints.

% Network relationships (structural influence edges)
% This physical law (Mountain) enables the creation of technological standards (Ropes).
narrative_ontology:affects_constraint(c_physical_blue_wavelength, c_tech_srgb_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No enrichment or overrides needed — Mountain (physical constant).
% A physical law has no beneficiary/victim asymmetry; suppression is total
% and extraction is negligible. No agent benefits at the expense of another.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */