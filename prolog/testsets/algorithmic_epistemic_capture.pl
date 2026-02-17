% ============================================================================
% CONSTRAINT STORY: algorithmic_epistemic_capture
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(algorithmic_epistemic_capture, []).

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
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: algorithmic_epistemic_capture
 * human_readable: The Feedback Loop Blindfold
 * domain: technological/social/cognitive
 * * SUMMARY:
 * A scenario where an individual's worldview is entirely mediated by 
 * recommendation algorithms that optimize for engagement rather than 
 * accuracy. This "Rope" for information discovery becomes a "Snare" 
 * as it siphons the subject's cognitive agency into a self-reinforcing 
 * bubble, liquidating their ability to perceive counter-signals or 
 * external reality.
 * * KEY AGENTS:
 * - Digital Consumer: Subject (Powerless)
 * - Attention Platform: Beneficiary (Institutional)
 * - Information Ecologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% High extraction (0.85) because the capture liquidates the subject's 
% primary epistemic agency to feed the institutional platform's ad-revenue.
domain_priors:base_extractiveness(algorithmic_epistemic_capture, 0.85). 
domain_priors:suppression_score(algorithmic_epistemic_capture, 0.74). 
domain_priors:theater_ratio(algorithmic_epistemic_capture, 0.82). 

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, extractiveness, 0.85).
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, theater_ratio, 0.82).

% Constraint classification claim
narrative_ontology:constraint_claim(algorithmic_epistemic_capture, piton).
narrative_ontology:human_readable(algorithmic_epistemic_capture, "The Feedback Loop Blindfold").

% Mandatory keys for classification engine v3.4
domain_priors:requires_active_enforcement(algorithmic_epistemic_capture).

% Multifile declarations for Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(algorithmic_epistemic_capture, attention_platforms).
narrative_ontology:constraint_victim(algorithmic_epistemic_capture, digital_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: their "choices" are pre-filtered by an engine 
% they do not control, creating a predatory cognitive lock-in.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the capture as a Rope—the only way to coordinate 
% millions of users' attention and manage the infinite noise of the internet.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.82) > 0.70 triggers Piton: the "Discovery" feature 
% is an inertial spike; it no longer finds new things, but merely 
% deepens the existing rut.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.85) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(algorithmic_epistemic_capture, E), E >= 0.50,
    domain_priors:suppression_score(algorithmic_epistemic_capture, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_epistemic_capture_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless consumer vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(algorithmic_epistemic_capture, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_epistemic_capture, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.82) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(algorithmic_epistemic_capture, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.85) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(algorithmic_epistemic_capture, E),
    E > 0.70.

:- end_tests(algorithmic_epistemic_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the 
 * "coordination" of social attention has effectively consumed the 
 * cognitive diversity of the subject population.
 * 
 * * PERSPECTIVAL GAP:
 * The Digital Consumer feels a Snare because their worldview is 
 * systematically restricted to whatever maintains their "dwell time." 
 * The Platform sees a Rope because the capture is the only way to 
 * coordinate stable user bases for ad-driven revenue at scale.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "discovery" engine is no longer functional (Theater 0.82); 
 * it is an inert spike siphoning 0.85 of the species' collective agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_epistemic_escape_velocity,
    'Can a user break the capture through manual "drift", or is the algorithm faster (Snare vs Mountain)?',
    'Tracking the success rate of deliberate interest-swerving against algorithmic re-centering.',
    'If swerving persists: Snare of current tech. If re-centering wins: Mountain of Cognitive Vulnerability.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(algorithmic_epistemic_capture, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional personalization (0.30) to 
% inertial "Discovery Theater" (0.82) as the feedback loop hardens.
narrative_ontology:measurement(aec_tr_t0, algorithmic_epistemic_capture, theater_ratio, 0, 0.30).
narrative_ontology:measurement(aec_tr_t5, algorithmic_epistemic_capture, theater_ratio, 5, 0.62).
narrative_ontology:measurement(aec_tr_t10, algorithmic_epistemic_capture, theater_ratio, 10, 0.82).

% Extraction: Progressive liquidation of the user's cognitive agency into 
% terminal algorithmic dependency and engagement-harvesting.
narrative_ontology:measurement(aec_ex_t0, algorithmic_epistemic_capture, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(aec_ex_t5, algorithmic_epistemic_capture, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(aec_ex_t10, algorithmic_epistemic_capture, base_extractiveness, 10, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
