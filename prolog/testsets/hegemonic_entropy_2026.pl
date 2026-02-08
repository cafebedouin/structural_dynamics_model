% ============================================================================
% CONSTRAINT STORY: hegemonic_entropy_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-02
% Scenario: The asymmetric fragility of global order vs. internal rot.
% ============================================================================

:- module(constraint_hegemonic_entropy_2026, []).

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
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: hegemonic_entropy_2026
 * human_readable: The Asymmetry of Hegemonic Decay
 * domain: economic/political
 * * SUMMARY:
 * The global order, once a tool for broad coordination, has shifted toward 
 * asymmetric extraction via Cantillon effects and tax havens. This creates 
 * a "Piton" effect where institutions maintain the theater of stability 
 * while the underlying social contract rots, leading to populism and fragmentation.
 * * KEY AGENTS:
 * - The Fragmented Middle Class: Subject (Powerless) - Viewing the system as a Snare.
 * - The Cantillon Insiders: Beneficiary (Institutional) - Viewing the system as a Rope.
 * - The Macro-Historian: Auditor (Analytical) - Detecting the Piton/Inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.72) due to wealth concentration at the top.
domain_priors:base_extractiveness(hegemonic_entropy_2026, 0.72). 

% Suppression of alternatives is moderate but high-impact at national scales.
domain_priors:suppression_score(hegemonic_entropy_2026, 0.55).   

% Theater ratio is high (0.78), indicating institutional inertia (Piton).
domain_priors:theater_ratio(hegemonic_entropy_2026, 0.78).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(hegemonic_entropy_2026, extractiveness, 0.72).
narrative_ontology:constraint_metric(hegemonic_entropy_2026, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hegemonic_entropy_2026, theater_ratio, 0.78).

% Constraint classification claim
narrative_ontology:constraint_claim(hegemonic_entropy_2026, piton).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The powerless middle class experiences the "King Dollar" siege as a predatory trap.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutional elites see frictionless shipping and tax havens as essential coordination.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Analytical detection of a non-functional, inertial constraint.
% [RESOLVED MANDATROPHY]: The theater ratio > 0.70 confirms the Piton classification.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(hegemonic_entropy_2026, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hegemonic_entropy_2026_tests).

test(perspectival_gap) :-
    % Verify the gap between the victim (Snare) and the insider (Rope).
    constraint_indexing:constraint_classification(hegemonic_entropy_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hegemonic_entropy_2026, rope, context(agent_power(institutional), _, _, _)).

test(piton_verification) :-
    % Ensure the high theater ratio triggers Piton status for auditors.
    constraint_indexing:constraint_classification(hegemonic_entropy_2026, piton, context(agent_power(analytical), _, _, _)).

:- end_tests(hegemonic_entropy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Perspectival Gap" here is driven by the Cantillon Effect. For those near 
 * the money spigot, the constraint is a "Rope" that facilitates global 
 * commerce. For the periphery, it is a "Snare" because the benefits of 
 * coordination are stripped away by extraction.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]: The extraction score (0.72) is high enough to 
 * suggest the system should collapse, yet it persists through "Theater." 
 * The Piton classification prevents us from assuming the system is 
 * still providing genuine coordination to the majority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46)
omega_variable(
    omega_entropy_reversibility,
    'Is the stagnation of King Dollar a reversible policy error or a civilizational Mountain?',
    'Observation of US internal political realignment vs. global fragmentation rate.',
    'Reversible = Tangled Rope; Irreversible = civilizational Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hegemonic_entropy_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the transition from distributed wealth to top-heavy extraction.
% Required for Extraction > 0.46. 

% Theater ratio over time (Goodhart drift: institutions becoming more performative).
narrative_ontology:measurement(he_tr_t0, hegemonic_entropy_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(he_tr_t5, hegemonic_entropy_2026, theater_ratio, 5, 0.55).
narrative_ontology:measurement(he_tr_t10, hegemonic_entropy_2026, theater_ratio, 10, 0.78).

% Extraction over time (Extraction accumulation: wealth concentrating at the top).
narrative_ontology:measurement(he_ex_t0, hegemonic_entropy_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(he_ex_t5, hegemonic_entropy_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(he_ex_t10, hegemonic_entropy_2026, base_extractiveness, 10, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
