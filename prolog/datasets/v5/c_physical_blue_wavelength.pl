% ============================================================================
% CONSTRAINT STORY: c_physical_blue_wavelength
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-30
% ============================================================================

:- module(constraint_blue_wavelength, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: c_physical_blue_wavelength
 * human_readable: The Blue Wavelength Constraint (Rayleigh Scattering)
 * domain: technological/scientific
 * * SUMMARY:
 * The phenomenon of "blue" is constrained by the physical properties of the 
 * electromagnetic spectrum (~450-495nm). For a subject, it is an immutable 
 * physical law; for an institution, it is a tool for atmospheric signaling.
 * * KEY AGENTS:
 * - Human Observer: Subject (Powerless against physics)
 * - Display Manufacturer: Beneficiary (Uses standards to coordinate hardware)
 * - Physicist: Auditor (Analyzes the scattering mechanics)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Mountains have near-zero extraction (it's a law, not a tax)
domain_priors:base_extractiveness(c_physical_blue_wavelength, 0.02). 
domain_priors:suppression_score(c_physical_blue_wavelength, 0.95).   % High suppression of alternatives (physics)
domain_priors:theater_ratio(c_physical_blue_wavelength, 0.05).       % Purely functional physical law

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(c_physical_blue_wavelength, extractiveness, 0.02).
narrative_ontology:constraint_metric(c_physical_blue_wavelength, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(c_physical_blue_wavelength, theater_ratio, 0.05).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the observer, the sky is blue due to Rayleigh scattering; this cannot be voted away.
constraint_indexing:constraint_classification(c_physical_blue_wavelength, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(universal))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Standards bodies (sRGB/Rec.709) use the "blue" primary as a coordination tool for displays.
constraint_indexing:constraint_classification(c_physical_blue_wavelength, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the transition where "natural blue" is co-opted by "digital blue" standards.
constraint_indexing:constraint_classification(c_physical_blue_wavelength, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(c_physical_blue_wavelength, E), E < 0.10.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blue_wavelength_tests).

test(perspectival_gap) :-
    % Verify the physical limit is a Mountain for the individual.
    constraint_indexing:constraint_classification(c_physical_blue_wavelength, mountain, context(agent_power(powerless), _, _, _)),
    % Verify it acts as a Rope (coordination) for institutions (standardization).
    constraint_indexing:constraint_classification(c_physical_blue_wavelength, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(c_physical_blue_wavelength, E),

    E =< 0.05. % Validates Mountain status.

:- end_tests(blue_wavelength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Color Blue" is modeled as a Mountain because its physical basis (Rayleigh 
 * scattering and the EM spectrum) is independent of human agency. The Perspectival 
 * Gap arises because while a human cannot change the sky (Mountain), a display 
 * manufacturer "uses" the constraint to coordinate global manufacturing (Rope).
 *
 * MANDATROPHY ANALYSIS:
 * Low extraction (0.02) ensures this is not misidentified as a Snare. The high 
 * suppression score reflects the impossibility of "opting out" of light physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_blue_qualia,
    'Is the subjective experience of blue (qualia) a physical Mountain or a neural Snare?',
    '[Neural mapping and hard-problem consciousness studies]',
    '[Intersubjective blue vs individual chromatic variation]',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(c_physical_blue_wavelength, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio remains low (Physical laws do not perform "theater")
narrative_ontology:measurement(blue_tr_t0, c_physical_blue_wavelength, theater_ratio, 0, 0.05).
narrative_ontology:measurement(blue_tr_t5, c_physical_blue_wavelength, theater_ratio, 5, 0.05).
narrative_ontology:measurement(blue_tr_t10, c_physical_blue_wavelength, theater_ratio, 10, 0.05).

% Extraction remains low (Nature does not collect rent)
narrative_ontology:measurement(blue_ex_t0, c_physical_blue_wavelength, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(blue_ex_t5, c_physical_blue_wavelength, base_extractiveness, 5, 0.02).
narrative_ontology:measurement(blue_ex_t10, c_physical_blue_wavelength, base_extractiveness, 10, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
