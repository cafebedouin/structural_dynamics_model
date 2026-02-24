% ============================================================================
% CONSTRAINT STORY: spain_digital_offensive_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_spain_digital, []).

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
 * * constraint_id: spain_digital_offensive_2026
 * human_readable: Spanish Five-Point Digital Offensive
 * domain: political/technological/legal
 * * SUMMARY:
 * Spanish PM Pedro Sánchez has declared social media a "failed state". 
 * Spain is implementing a sweeping regulatory push including a ban for minors 
 * under 16, criminalizing algorithmic manipulation, and holding executives 
 * personally liable to curb hate and disinformation.
 * * KEY AGENTS:
 * - [Minors/Platform Users]: Subject (Powerless) - Restricted from access and 
 * algorithmic "amplification of illegal content".
 * - [Spanish State]: Beneficiary (Institutional) - Reasserting sovereign 
 * control over digital "failed states".
 * - [Coalition of the Digitally Willing]: Auditor (Analytical) - Coordinating 
 * cross-border enforcement across Europe.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.72) due to criminal penalties for algorithms and executive liability.
domain_priors:base_extractiveness(spain_digital_offensive_2026, 0.72). 

% High suppression (0.80); requires "real barriers" and age verification, not just checkboxes.
domain_priors:suppression_score(spain_digital_offensive_2026, 0.80).   

% Low-moderate theater (0.40); intended as "enforcement-first" rather than mere rhetoric.
domain_priors:theater_ratio(spain_digital_offensive_2026, 0.40).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(spain_digital_offensive_2026, extractiveness, 0.72).
narrative_ontology:constraint_metric(spain_digital_offensive_2026, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(spain_digital_offensive_2026, theater_ratio, 0.4).

% Requires active enforcement by public prosecutors and age-verification systems.
domain_priors:requires_active_enforcement(spain_digital_offensive_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Minors under 16 are effectively "trapped" out of the digital commons by 
% state-mandated barriers.
constraint_indexing:constraint_classification(spain_digital_offensive_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The State views this as essential coordination ("Coalition of the Willing") 
% to prevent social "polarization".
constraint_indexing:constraint_classification(spain_digital_offensive_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid of genuine safety coordination and aggressive 
% state extraction of corporate power.
constraint_indexing:constraint_classification(spain_digital_offensive_2026, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(continental))) :-
    domain_priors:base_extractiveness(spain_digital_offensive_2026, E), E >= 0.50,
    domain_priors:suppression_score(spain_digital_offensive_2026, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spain_digital_tests).

test(perspectival_gap) :-
    % Verify the State sees coordination (Rope) while the Subject sees a Snare.
    constraint_indexing:constraint_classification(spain_digital_offensive_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(spain_digital_offensive_2026, snare, context(agent_power(powerless), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(spain_digital_offensive_2026, E),

    E > 0.46. % Triggers temporal data requirement for high-extraction constraints.

:- end_tests(spain_digital_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.72) reflects the move toward "criminalizing algorithmic 
 * practices" and personal executive liability.
 * The Perspectival Gap is driven by the state's framing of a "failed state" vs. 
 * the platforms' and users' view of a "closed digital border".
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is vital as it acknowledges the coordination 
 * intent (protecting minors) while highlighting the extraction of sovereignty 
 * from global tech giants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_effective_verification,
    'Can "real barriers" be implemented without creating a universal identity snare?',
    'Technical audit of age-verification deployments in Spain vs. Australia.',
    'If privacy fails, it is a Snare; if it holds, it is a Scaffold for safety.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spain_digital_offensive_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the escalation from Feb 2026 (T=0) through the 10-interval horizon.

% Theater ratio: Initially high as laws are passed, but drops as 
% prosecutorial action against Grok/TikTok begins.
narrative_ontology:measurement(spain_tr_t0, spain_digital_offensive_2026, theater_ratio, 0, 0.65).
narrative_ontology:measurement(spain_tr_t5, spain_digital_offensive_2026, theater_ratio, 5, 0.50).
narrative_ontology:measurement(spain_tr_t10, spain_digital_offensive_2026, theater_ratio, 10, 0.40).

% Extraction: Increases as criminal offenses for algorithms are codified 
% and executive liability is enforced.
narrative_ontology:measurement(spain_ex_t0, spain_digital_offensive_2026, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(spain_ex_t5, spain_digital_offensive_2026, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(spain_ex_t10, spain_digital_offensive_2026, base_extractiveness, 10, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
