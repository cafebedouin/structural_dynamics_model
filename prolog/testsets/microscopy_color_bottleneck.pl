% ============================================================================
% CONSTRAINT STORY: microscopy_color_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_microscopy_color_bottleneck, []).

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
 *   constraint_id: microscopy_color_bottleneck
 *   human_readable: Fluorescence Microscopy Color Channel Limit
 *   domain: computational_biology/spatial_proteomics/optical_physics
 *
 * SUMMARY:
 *   The fluorescence microscopy color channel limit represents a hard
 *   physical boundary set by quantum mechanics and the finite extent of the
 *   visible/near-infrared electromagnetic spectrum. Fluorescent proteins and
 *   organic dyes emit light in specific wavelength bands determined by their
 *   electronic structure. The fundamental constraint is that two fluorophores
 *   with overlapping emission spectra cannot be simultaneously imaged without
 *   crosstalk — light from one dye contaminates the detection channel of the
 *   other. This constraint is not institutional, not negotiable, and not
 *   subject to enforcement because it flows directly from the quantized
 *   nature of photon energy and the limited spectral window available to
 *   biology. The constraint does not change based on researcher intent,
 *   funding level, or technological ingenuity — it can only be worked around
 *   (spectral unmixing, temporal separation, frequency shifting) or accepted
 *   (fewer proteins imaged per experiment). From the perspective of quantum
 *   optics and photon physics, this is a mountain: unchangeable, immutable,
 *   and independent of observer position.
 *
 * KEY AGENTS:
 *   - Quantum Optics: The underlying principle — photon wavelength quantization and spectral overlap are immutable features of electromagnetic radiation
 *   - Systems Biology Researchers: Agents constrained by the limit (powerless/trapped) — must design experiments within the 4-6 simultaneous channel envelope
 *   - Instrument Manufacturers: Powerful agents (powerful/mobile) — can engineer better filters and detectors but cannot alter the fundamental spectral overlap physics
 *   - Computational Biology Community: Organized agents (organized/constrained) — develop workarounds (spectral unmixing, machine learning) that reduce but do not eliminate the constraint
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees the constraint as a natural law flowing from quantum mechanics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(microscopy_color_bottleneck, 0.18).
domain_priors:suppression_score(microscopy_color_bottleneck, 0.04).
domain_priors:theater_ratio(microscopy_color_bottleneck, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microscopy_color_bottleneck, extractiveness, 0.18).
narrative_ontology:constraint_metric(microscopy_color_bottleneck, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(microscopy_color_bottleneck, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(microscopy_color_bottleneck, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(microscopy_color_bottleneck, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(microscopy_color_bottleneck, mountain).
narrative_ontology:human_readable(microscopy_color_bottleneck, "Fluorescence Microscopy Color Channel Limit").
narrative_ontology:topic_domain(microscopy_color_bottleneck, "computational_biology/spatial_proteomics/optical_physics").

domain_priors:emerges_naturally(microscopy_color_bottleneck).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPTICAL PHYSICIST / NATURAL LAW (MOUNTAIN) — The photon wavelength quantization and spectral overlap constraint is a direct consequence of quantum mechanics and the finite width of the electromagnetic spectrum. No technology or technique changes the fact that fluorophore emission spectra overlap in the visible/near-IR window. This is immutable physics.
constraint_indexing:constraint_classification(microscopy_color_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: INSTRUMENT MANUFACTURER (MOUNTAIN) — Even with superior optical design, spectral filter sets, and detector technology, the underlying photon wavelength constraints remain fixed. Manufacturers can improve signal-to-noise and reduce crosstalk through engineering, but cannot overcome the fundamental spectral overlap limit. The constraint is immutable regardless of market leverage or R&D investment.
constraint_indexing:constraint_classification(microscopy_color_bottleneck, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: SYSTEMS BIOLOGY RESEARCHER (MOUNTAIN) — Researchers studying 8+ protein complexes face the hard limit of simultaneous spectral imaging. Experimental design must accommodate channel limits: sequential scanning, spectral unmixing, or sacrifice of protein targets. The constraint is unchangeable from their temporal horizon and resource position. No career advancement, funding, or organizational pressure alters the physics.
constraint_indexing:constraint_classification(microscopy_color_bottleneck, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPUTATIONAL BIOLOGY COMMUNITY (ROPE) — This community perceives the color limit as a coordination problem solvable through spectral unmixing algorithms, machine-learning deconvolution, and multiplexed time-resolved imaging. From a generational view, workarounds emerge that make the fundamental constraint less binding. The community benefits from having a well-defined problem to solve collectively.
constraint_indexing:constraint_classification(microscopy_color_bottleneck, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microscopy_color_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(microscopy_color_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(microscopy_color_bottleneck, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(microscopy_color_bottleneck, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(microscopy_color_bottleneck, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(microscopy_color_bottleneck, ExtMetricName, E),
    domain_priors:suppression_score(microscopy_color_bottleneck, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(microscopy_color_bottleneck),
    narrative_ontology:constraint_metric(microscopy_color_bottleneck, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(microscopy_color_bottleneck, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(microscopy_color_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. This constraint does not extract value from anyone — it is a neutral physical boundary. The slight non-zero value (rather than exactly 0.0) reflects that the constraint does absorb research effort: designing around it costs time and resources. But this is coordination cost, not extraction. Suppression (0.04): Very low. The constraint does not suppress alternatives through coercion or institutional enforcement — it is simply impossible to detect simultaneous light from spectrally overlapping sources without crosstalk. This is a physical impossibility, not a suppressed option. Theater ratio (0.12): Very low. The constraint requires no performative enforcement. The experiments either work or they don't based on physics, not on institutional ritual or theatrical compliance. The slight non-zero value reflects documentation and statistical reporting, which are minimal. Accessibility collapse (0.92): Very high. Every researcher faces the same limit regardless of resources, institutional position, or technical sophistication. The constraint is completely universal. Resistance (0.08): Very low. The constraint is not resisted because it is not an institution that can be pushed back against. It is simply the state of the world. The small non-zero value reflects the distributed effort in workarounds and spectral unmixing, but these are adaptations, not resistance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is minimal because the constraint is genuinely invariant across observer positions. The optical physicist, the manufacturer, the researcher, and the community all agree: this is how photons work. The rope perspective from the computational biology community is not a disagreement about the constraint's type — it is agreement that the constraint exists, combined with optimism about the community's ability to develop workarounds that reduce (but do not eliminate) the binding effect. This is not a gap in classification; it is a gap in temporal horizon and agency assessment. The researcher sees the constraint as immediately binding; the community sees it as something their collective effort can partially overcome within a generational timescale.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not meaningful for mountain constraints because the constraint does not extract directionally from any agent. No beneficiary gains from the constraint's existence; no victim bears extraction through it. The constraint is symmetric — it applies uniformly to all researchers regardless of power level. The analytical context's d-value would be canonical 0.73 (analytical observer), yielding low negative f(d) ≈ 1.15, which scales ε to χ ≈ 0.21. But this is a measurement artifact of the formula's application to natural laws, not a reflection of actual directionality. The constraint has no directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spectral_unmixing_sufficiency,
    'Do advanced spectral unmixing and machine-learning deconvolution methods genuinely overcome the color-channel limit, or do they only redistribute the constraint (trading spectral resolution for algorithmic coupling)?',
    'Quantitative assessment of unmixing error rates as a function of channel number; comparison of information-theoretic channel capacity (Shannon limit) vs. achieved capacity with unmixing; analysis of whether unmixing introduces systematic bias in downstream quantification',
    'If unmixing is truly sufficient: the constraint is less absolute than the mountain classification suggests — classification might shift toward rope at the community level. If unmixing trades one constraint for another: the mountain holds but the nature of the constraint shifts (from spectral overlap to algorithmic coupling).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spectral_unmixing_sufficiency, empirical, 'Whether spectral unmixing overcomes or redistributes the color-channel constraint').

omega_variable(
    natural_law_vs_instrumental_artifact,
    'Is the color-channel limit a fundamental law of quantum optics, or is it an artifact of current detection modalities that future technologies (single-photon sources, quantum entanglement) might bypass?',
    'Analysis of whether the limit derives from quantized photon properties or from technical constraints of fluorophore emission and detector design; investigation of quantum-enabled imaging pathways (quantum illumination, entangled photon pairs); assessment of whether any proposed technology would violate photon statistics',
    'If fundamental quantum property: mountain classification is robust across all time horizons. If technical artifact: classification might degrade toward rope or scaffold at longer time horizons as quantum technologies mature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_instrumental_artifact, empirical, 'Whether the color limit is a fundamental law or a technical artifact').

omega_variable(
    beneficiary_asymmetry_absence,
    'Are there identifiable beneficiaries who profit from the existence of the color-channel limit, or is this a genuine physical constraint with no extractive layer?',
    'Structural analysis of who benefits from the constraint persisting: do instrument manufacturers benefit from forced upgrades? Do software vendors profit from spectral unmixing tools? Do research grant agencies benefit from extended project timelines? Comparison with known extraction mechanisms.',
    'If beneficiaries exist: the constraint may be subject to FSM evaluation (false summit detection) and reclassification as tangled_rope. If no beneficiaries: the mountain classification holds as a pure natural law with no extractive overlay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_asymmetry_absence, conceptual, 'Whether the constraint has extractive beneficiaries or is pure natural law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(microscopy_color_bottleneck, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(microcol_tr_t0, microscopy_color_bottleneck, theater_ratio, 0, 0.1).
narrative_ontology:measurement(microcol_tr_t10, microscopy_color_bottleneck, theater_ratio, 10, 0.12).
narrative_ontology:measurement(microcol_tr_t20, microscopy_color_bottleneck, theater_ratio, 20, 0.13).

% Extraction over time
narrative_ontology:measurement(microcol_be_t0, microscopy_color_bottleneck, base_extractiveness, 0, 0.17).
narrative_ontology:measurement(microcol_be_t10, microscopy_color_bottleneck, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(microcol_be_t20, microscopy_color_bottleneck, base_extractiveness, 20, 0.19).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(microscopy_color_bottleneck, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
