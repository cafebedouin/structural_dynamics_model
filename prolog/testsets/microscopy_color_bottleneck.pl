% ============================================================================
% CONSTRAINT STORY: microscopy_color_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
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
 *   The fluorescence microscopy color channel bottleneck is a genuine
 *   physical constraint arising from the quantized nature of photon
 *   wavelengths and the limited spectral range of visible and near-infrared
 *   light. Fluorescent proteins and dyes emit light in specific wavelength
 *   bands, and the overlap between emission spectra limits how many distinct
 *   signals can be simultaneously detected without crosstalk. Conventional
 *   widefield and confocal microscopy can reliably distinguish approximately
 *   4 channels (typically DAPI/blue, GFP/green, RFP/red, far-red). This
 *   creates a fundamental mismatch with spatial proteomics goals: the human
 *   proteome contains ~13,000 proteins that can be detected in typical cell
 *   types, but only 4 can be visualized simultaneously in a single imaging
 *   pass. The constraint is not institutional,
 *   technological-in-the-contingent-sense, or extractive — it is a direct
 *   consequence of electromagnetic spectrum physics. Workarounds exist
 *   (cyclic imaging with photobleaching between rounds, mass spectrometry
 *   imaging, DNA barcoding methods like MERFISH/seqFISH, expansion
 *   microscopy) but these are adaptations that accept the constraint rather
 *   than removals of it. The 4-channel limit remains invariant across all
 *   observer positions.
 *
 * KEY AGENTS:
 *   - Experimental Biologist: Powerless/trapped — cannot exit the constraint; must design experiments within the 4-channel limit or adopt complex workarounds
 *   - Microscopy Core Facility: Institutional/arbitrage — has access to multiple imaging modalities but faces the same physical limit in each
 *   - Spatial Proteomics Consortium: Organized/constrained — developing cyclic imaging and barcoding methods that work around rather than remove the constraint
 *   - Analytical Observer: Analytical/analytical — recognizes the constraint as a consequence of photon physics and spectral overlap mathematics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(microscopy_color_bottleneck, 0.08).
domain_priors:suppression_score(microscopy_color_bottleneck, 0.03).
domain_priors:theater_ratio(microscopy_color_bottleneck, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microscopy_color_bottleneck, extractiveness, 0.08).
narrative_ontology:constraint_metric(microscopy_color_bottleneck, suppression_requirement, 0.03).
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

% PERSPECTIVE 1: EXPERIMENTAL BIOLOGIST (MOUNTAIN) — Faces the spectral overlap constraint as an immutable physical law. Cannot simultaneously visualize more than ~4 proteins with conventional fluorescence microscopy due to photon wavelength physics. No exit option — this is the fundamental limit of the measurement technology.
constraint_indexing:constraint_classification(microscopy_color_bottleneck, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MICROSCOPY CORE FACILITY (MOUNTAIN) — Institutional actor with access to cutting-edge equipment still faces the same 4-channel limit. Arbitrage options (switching between techniques, sequential imaging, multiplexing protocols) do not remove the constraint — they work around it. The physical limit remains invariant.
constraint_indexing:constraint_classification(microscopy_color_bottleneck, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SPATIAL PROTEOMICS CONSORTIUM (MOUNTAIN) — Organized research community developing workarounds (cyclic imaging, mass spectrometry imaging, DNA barcoding) but these are adaptations to the constraint, not removals of it. The 4-channel simultaneous limit persists as a physical boundary that shapes all protocol design.
constraint_indexing:constraint_classification(microscopy_color_bottleneck, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The constraint derives from photon wavelength quantization and spectral overlap in the visible/near-IR range. This is a consequence of quantum mechanics and the electromagnetic spectrum structure. No observer position changes this classification — it is a genuine natural law constraint on fluorescence-based imaging.
constraint_indexing:constraint_classification(microscopy_color_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microscopy_color_bottleneck_tests).

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
 *   Extractiveness (0.08): Very low. The constraint extracts minimal value from agents — it is a coordination cost (researchers must choose which 4 proteins to visualize, must run multiple experiments, must adopt sequential imaging protocols) but not an asymmetric extraction mechanism. The small non-zero value reflects the opportunity cost of experiments not performed and the resource cost of workarounds. Suppression (0.03): Very low. The constraint does not suppress alternatives through coercion — it is a physical boundary. Alternatives exist (mass spec imaging, electron microscopy, biochemical fractionation) but they measure different observables. The suppression value reflects only the narrow sense in which the constraint 'suppresses' simultaneous multi-protein fluorescence imaging by making it physically impossible. Theater ratio (0.12): Very low. There is minimal performative content. The constraint is directly measurable (spectral overlap can be quantified, crosstalk can be demonstrated), and the 4-channel limit is consistently observed across labs and equipment. The small non-zero value reflects that some imaging claims push the boundary (5-6 channels with careful spectral unmixing) but these are marginal cases, not theater. Accessibility collapse (0.92): Very high. All agents, regardless of resources or position, face the same constraint. A well-funded core facility with a $2M microscope has the same 4-channel limit as a researcher with a basic widefield system. Resistance (0.08): Very low. The constraint has persisted unchanged since the introduction of multi-color fluorescence microscopy in the 1990s. Incremental improvements (better filters, spectral unmixing algorithms, brighter dyes) have not changed the fundamental ~4 channel limit.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in classification — all four perspectives return mountain. This is the signature of a genuine natural law: the constraint is invariant across power levels, time horizons, exit options, and spatial scopes. The experimental biologist with no resources, the institutional core facility with cutting-edge equipment, the organized research consortium developing workarounds, and the analytical observer with civilizational perspective all encounter the same immutable limit. The gap that does exist is between this constraint and the institutional/technological constraints often confused with it. For example, the high cost of super-resolution microscopy is NOT a mountain — that is an institutional constraint (snare or tangled_rope depending on perspective). The 4-channel spectral limit is a mountain. Conflating these produces false summits.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because this is a genuine natural law constraint. The constraint does not extract asymmetrically — all agents face the same physical limit. The small extractiveness value (0.08) represents the symmetric coordination cost of working within the constraint, not asymmetric extraction from victims to beneficiaries. Directionality values are not computed because there is no extraction flow to measure. All perspectives classify as mountain because the constraint is invariant across observer positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mountain classification in its purest form: a physical law that admits no perspectival variation. The mandatrophy question 'Is this coordination or extraction?' is resolved immediately — it is neither. The constraint is a boundary condition on what is physically possible. The small extractiveness value (0.08) reflects coordination costs (researchers must coordinate their experimental design with the constraint, must choose which proteins to visualize, must sequence experiments) but this is not extraction in the DR sense. No agent benefits from the constraint's existence. No agent is targeted by it. It simply is. The resistance to mandatrophy confusion comes from the constraint's empirical accessibility: spectral overlap is directly measurable, the 4-channel limit is consistently reproduced, and the physics (photon wavelength quantization, detector sensitivity curves, emission spectrum overlap) is well-understood. There is no institutional narrative that could reframe this as coordination or extraction without immediately colliding with experimental evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(microscopy_color_bottleneck, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(microscopy_color_bottleneck, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is often conflated with institutional constraints in spatial proteomics (e.g., equipment access barriers, reagent costs, expertise requirements). Those are separate constraints with their own extractiveness values. The 4-channel spectral limit is the physical boundary; the institutional barriers are the social structures built around it. Decomposition: microscopy_color_bottleneck (ε=0.08, mountain) is the physical limit. A separate story for microscopy_equipment_access (ε≈0.55, tangled_rope or snare depending on institution) would capture the institutional extraction. A third story for cyclic_imaging_protocol_complexity (ε≈0.30, tangled_rope) would capture the coordination costs of workarounds. These are linked via network.affects_constraints but must not be merged into a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
