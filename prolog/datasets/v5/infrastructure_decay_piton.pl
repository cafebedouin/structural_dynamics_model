% ============================================================================
% CONSTRAINT STORY: infrastructure_decay_piton
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_infrastructure_piton, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: infrastructure_decay_piton
 * human_readable: The Painted Ruins
 * domain: technological/political
 * * SUMMARY:
 * A state of physical infrastructure where superficial maintenance is 
 * prioritized to satisfy public visibility, while internal structural 
 * integrity is neglected. This is a 
 * Piton: a legacy constraint maintained by the theater of appearance 
 * despite functional failure.
 * * KEY AGENTS:
 * - The Commuter: Subject (Powerless) - Crossing a bridge they assume is safe.
 * - The Public Works Director: Beneficiary (Institutional) - Keeping metrics green.
 * - The Structural Engineer: Auditor (Analytical) - Warning of the invisible rot.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(infrastructure_decay_piton, 0.65). % High: Theft of future safety for present budget optics.
domain_priors:suppression_score(infrastructure_decay_piton, 0.85).   % High: Commuters often have no alternative route.
domain_priors:theater_ratio(infrastructure_decay_piton, 0.92).       % Extreme: The "maintenance" is almost entirely aesthetic.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(infrastructure_decay_piton, extractiveness, 0.65).
narrative_ontology:constraint_metric(infrastructure_decay_piton, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(infrastructure_decay_piton, theater_ratio, 0.92).

% Binary flags
domain_priors:requires_active_enforcement(infrastructure_decay_piton).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the commuter, the bridge is a Snare—a mandatory route that extracts 
% taxes and risks their life without providing the promised safety.
constraint_indexing:constraint_classification(infrastructure_decay_piton, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE INSTITUTION (ROPE)
% To the department, the painting program is a Rope—it coordinates public 
% approval and prevents political "friction" regarding the budget.
constraint_indexing:constraint_classification(infrastructure_decay_piton, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Detection of "Piton" status: The theater_ratio (0.92) confirms the 
% structure is maintained by habit and appearance, not utility.
constraint_indexing:constraint_classification(infrastructure_decay_piton, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(infrastructure_decay_piton, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infra_piton_tests).

test(piton_identity) :-
    % Verify the auditor identifies the Piton via high theater_ratio.
    constraint_indexing:constraint_classification(infrastructure_decay_piton, piton, context(agent_power(analytical), _, _, _)).

test(mandatrophy_resolution) :-
    % Extraction > 0.7 requires explicit resolution.
    domain_priors:base_extractiveness(infrastructure_decay_piton, E), E > 0.70 -> true; 

    (re_match("\\[RESOLVED MANDATROPHY\\]", _)).

:- end_tests(infra_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The theater_ratio (0.92) is diagnostic of the "Visibility Bias." The 
 * institution extracts the "Maintenance Tax" but spends it only on what 
 * can be seen from a moving car.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by identifying that the "Rope" (safety) 
 * has snapped, but the "Piton" (the physical bridge and the budget for 
 * it) remains hammered into the system. This is not a "Mountain" of 
 * natural decay, but a "Snare" of misallocated coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_structural_threshold,
    'When does the invisible rot exceed the load-bearing capacity of the "Theater"?',
    'Comparison of official inspection reports vs. non-destructive testing (NDT) data.',
    'If threshold is reached: Sudden Snare-collapse; If not: Continued Piton-decay.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(infrastructure_decay_piton, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
