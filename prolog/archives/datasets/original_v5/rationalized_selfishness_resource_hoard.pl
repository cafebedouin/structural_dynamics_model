% ============================================================================
% CONSTRAINT STORY: global_hoarding_scaling_laws
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_rationalized_selfishness, []).

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
 * * constraint_id: global_hoarding_scaling_laws
 * human_readable: The Planetary Siphon
 * domain: economic/political
 * * SUMMARY:
 * An expansion of localized selfishness into a globalized extraction system. 
 * As the spatial scope increases, the "suppression score" scales due to the 
 * increased distance between the beneficiary and the victims, making the 
 * constraint appear as an immutable "Mountain" to the powerless.
 * * KEY AGENTS:
 * - The Global Precariat: Subject (Powerless) - Distributed victims.
 * - The Sovereign Fund: Beneficiary (Institutional) - The global hoarder.
 * - The Forensic Economist: Auditor (Analytical) - Mapping the flow of value.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction remains high, but Suppression is scaled by global scope.
domain_priors:base_extractiveness(global_hoarding_scaling_laws, 0.85). 
domain_priors:suppression_score(global_hoarding_scaling_laws, 0.90).   
domain_priors:theater_ratio(global_hoarding_scaling_laws, 0.60).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, extractiveness, 0.85).
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, theater_ratio, 0.6).

% Binary flags
domain_priors:requires_active_enforcement(global_hoarding_scaling_laws).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% At a global scale with zero exit options, extraction is felt as natural law.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, mountain, 
    context(agent_power(powerless), 
            time_horizon(generational), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Sovereign Fund views the system as the only viable global coordination.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, rope, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Detects that the global infrastructure is now primarily theatrical/inertial.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(global_hoarding_scaling_laws, TR), TR > 0.50.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_scaling_tests).

test(scope_scaling_impact) :-
    % Verify that global scope shifts a Snare toward a Mountain classification.
    constraint_indexing:constraint_classification(global_hoarding_scaling_laws, mountain, context(_, _, _, global)).

test(extraction_is_severe) :-
    domain_priors:base_extractiveness(global_hoarding_scaling_laws, E),

    E > 0.80.

:- end_tests(global_scaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * When spatial_scope moves to 'global', the suppression_score ($0.90$) creates 
 * a "Scaling Law" effect. To the powerless, the constraint is no longer a 
 * "Snare" they can struggle against—it becomes a "Mountain," an environmental 
 * fact of life. 
 * * [RESOLVED MANDATROPHY]:
 * The extreme extraction ($0.85$) is sustained by the fact that the Beneficiary 
 * is mobile across the global scope while the Subject is trapped locally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_global_coalition,
    'Can the Precariat achieve "organized" power at a global scale?',
    'Detection of cross-border labor/resource coordination events.',
    'If True: The Mountain reverts to a Snare. If False: Permanent Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_hoarding_scaling_laws, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Institutionalization of global justifications)
narrative_ontology:measurement(ghs_tr_t0, global_hoarding_scaling_laws, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ghs_tr_t5, global_hoarding_scaling_laws, theater_ratio, 5, 0.45).
narrative_ontology:measurement(ghs_tr_t10, global_hoarding_scaling_laws, theater_ratio, 10, 0.60).

% Extraction over time (The "Siphon" effect intensifying)
narrative_ontology:measurement(ghs_ex_t0, global_hoarding_scaling_laws, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ghs_ex_t5, global_hoarding_scaling_laws, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(ghs_ex_t10, global_hoarding_scaling_laws, base_extractiveness, 10, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
