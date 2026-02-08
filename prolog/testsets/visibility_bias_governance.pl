% ============================================================================
% CONSTRAINT STORY: visibility_bias_governance
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_visibility_bias, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Updated for v3.4) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: visibility_bias_governance
 * human_readable: The Dashboard Delusion
 * domain: political/institutional
 * * SUMMARY:
 * A governance failure where policy is driven exclusively by high-visibility 
 * metrics (e.g., stock indices, surface-level crime stats) while ignoring 
 * low-visibility systemic decay (e.g., infrastructure entropy, trust decay). 
 * It creates a "Theater of Progress."
 * * KEY AGENTS:
 * - The Resident: Subject (Powerless) - Suffering from the unmeasured decay.
 * - The Policymaker: Beneficiary (Institutional) - Rewarded for "improving" visible metrics.
 * - The Complexity Scientist: Auditor (Analytical) - Mapping the invisible debt.
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(visibility_bias_governance, 0.54). 
domain_priors:suppression_score(visibility_bias_governance, 0.68).   
domain_priors:theater_ratio(visibility_bias_governance, 0.78).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(visibility_bias_governance, extractiveness, 0.54).
narrative_ontology:constraint_metric(visibility_bias_governance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(visibility_bias_governance, theater_ratio, 0.78).

% Constraint classification claim
narrative_ontology:constraint_claim(visibility_bias_governance, tangled_rope).

% Mandatory keys for classification engine v3.4
domain_priors:requires_active_enforcement(visibility_bias_governance).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(visibility_bias_governance, policymaker).
narrative_ontology:constraint_victim(visibility_bias_governance, resident).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped by a system that claims everything is fine (based 
% on metrics) while their actual environment is failing.
constraint_indexing:constraint_classification(visibility_bias_governance, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTION (ROPE)
% To the state, the dashboard is a Rope—the only way to coordinate 
% macro-level policy across a massive population.
constraint_indexing:constraint_classification(visibility_bias_governance, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid: The metrics provide some coordination (Rope), but the 
% selection bias extracts future stability (Snare/Piton).
constraint_indexing:constraint_classification(visibility_bias_governance, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(visibility_bias_governance, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(visibility_bias_tests).

test(piton_analysis_trigger) :-
    % Verify that the analytical observer identifies the Piton status via theater_ratio.
    constraint_indexing:constraint_classification(visibility_bias_governance, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % Extraction > 0.46 requires Omega validation in section 6.
    domain_priors:base_extractiveness(visibility_bias_governance, E), E > 0.46.

:- end_tests(visibility_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction (0.54) is non-obvious; it is the "extraction of attention." 
 * Resources are pulled away from critical invisible nodes to serve high-visibility 
 * political signals.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by identifying that Visibility Bias is a 
 * "Tangled Rope" of coordination. It is not an unavoidable "Mountain" of 
 * human cognitive limits, but a choice of which tools (metrics) to prioritize. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_metric_gaming,
    'Are the visible metrics accurate, or have they become "Theater" to hide systemic failure?',
    'Audit of internal raw data vs. published governance dashboards.',
    'If gamed: Pure Piton; If accurate: A high-tension Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(visibility_bias_governance, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional metric coordination (0.20) 
% to high-theater "Dashboard Delusion" (0.78) as systemic decay is ignored.
narrative_ontology:measurement(vis_tr_t0, visibility_bias_governance, theater_ratio, 0, 0.20).
narrative_ontology:measurement(vis_tr_t5, visibility_bias_governance, theater_ratio, 5, 0.48).
narrative_ontology:measurement(vis_tr_t10, visibility_bias_governance, theater_ratio, 10, 0.78).

% Extraction: Progressive accumulation of the "Opportunity Cost" of ignored 
% systemic risks as resources are pulled toward visible political signals.
narrative_ontology:measurement(vis_ex_t0, visibility_bias_governance, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(vis_ex_t5, visibility_bias_governance, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(vis_ex_t10, visibility_bias_governance, base_extractiveness, 10, 0.54).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
