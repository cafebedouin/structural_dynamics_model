% ============================================================================
% CONSTRAINT STORY: other_peoples_troubles_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_other_troubles, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: other_peoples_troubles_2026
 * human_readable: The Asymmetry of Vicarious Resilience
 * domain: social/psychological
 * * SUMMARY:
 * This constraint maps the psychological distance that allows observers to 
 * "bear" the suffering of others with ease. This "strength" functions as a 
 * high-extraction filter that suppresses the urgency of the victim's distress 
 * to maintain the observer's emotional or social stability.
 * * KEY AGENTS:
 * - [The Sufferer]: Subject (Powerless) - Experiencing the actual "trouble" 
 * as a physical or economic snare.
 * - [The Observer]: Beneficiary (Institutional) - Possesses the "strength" 
 * (indifference) to bear the situation without personal cost.
 * - [The Moral Philosopher]: Auditor (Analytical) - Identifies the 
 * performative nature of this stoicism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.75); the observer's "strength" extracts the social 
% impetus for aid, leaving the sufferer isolated.
domain_priors:base_extractiveness(other_peoples_troubles_2026, 0.75). 

% High suppression (0.80); the comfort of the observer suppresses the 
% signal of the sufferer's crisis.
domain_priors:suppression_score(other_peoples_troubles_2026, 0.80).   

% High theater ratio (0.85); vicarious "strength" is often a performative 
% stoicism that masks a lack of functional empathy.
domain_priors:theater_ratio(other_peoples_troubles_2026, 0.85).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(other_peoples_troubles_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(other_peoples_troubles_2026, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(other_peoples_troubles_2026, theater_ratio, 0.85).

% Constraint classification claim
narrative_ontology:constraint_claim(other_peoples_troubles_2026, piton).
narrative_ontology:human_readable(other_peoples_troubles_2026, "The Asymmetry of Vicarious Resilience").

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the one in trouble, the situation is a snare: they are trapped in 
% suffering while the "strength" of others offers no functional escape.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the observer, their own "strength" is a rope: it coordinates their 
% emotional stability and prevents the "trouble" from disrupting their life.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% The observer identifies a Piton: a non-functional, inertial indifference 
% that maintains the status quo through the theater of "resilience."
constraint_indexing:constraint_classification(other_peoples_troubles_2026, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(other_peoples_troubles_2026, TR), TR > 0.70.
domain_priors:requires_active_enforcement(other_peoples_troubles_2026).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(other_troubles_tests).

test(perspectival_gap) :-
    % Subject feels a Snare; Institutional observer maintains a Rope.
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, rope, context(agent_power(institutional), _, _, _)).

test(piton_detection) :-
    % Verify the Piton classification is active for high-theater observers.
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, piton, _).

:- end_tests(other_troubles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) reflects how "bearing" others' troubles 
 * essentially consumes the urgency of the problem to fund the observer's 
 * peace of mind. The theater_ratio (0.85) is high because the "strength" 
 * described by La Rochefoucauld is not an act of support, but a performative 
 * detachment.
 *
 * [RESOLVED MANDATROPHY]
 * The Piton classification prevents the system from mislabeling this 
 * indifference as a functional coordination mechanism (Rope) for 
 * societal stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_empathy_limit,
    'Is vicarious strength a fundamental Mountain of human cognition?',
    'Analysis of empathy-fatigue data vs. active-aid intervention rates.',
    'If aid scales with strength, it is a Rope; if it stays flat, it is a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(other_peoples_troubles_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the drift from "Potential Empathy" (T=0) to "Inertial Indifference" (T=10).

% Theater ratio: Increases as the "strength" to bear others' troubles 
% transitions from silent observation to performative stoicism.
narrative_ontology:measurement(ot_tr_t0, other_peoples_troubles_2026, theater_ratio, 0, 0.40).
narrative_ontology:measurement(ot_tr_t5, other_peoples_troubles_2026, theater_ratio, 5, 0.65).
narrative_ontology:measurement(ot_tr_t10, other_peoples_troubles_2026, theater_ratio, 10, 0.85).

% Extraction: Increases as the observer's refusal to act effectively 
% "extracts" the time and hope of the sufferer.
narrative_ontology:measurement(ot_ex_t0, other_peoples_troubles_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ot_ex_t5, other_peoples_troubles_2026, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(ot_ex_t10, other_peoples_troubles_2026, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2.

% --- Structural properties (missing from base properties) ---
% Beneficiary: The observer who maintains emotional stability through
% psychological distance from the sufferer's distress (Section 1 narrative).
narrative_ontology:constraint_beneficiary(other_peoples_troubles_2026, detached_observers).

% Victim: The sufferer whose crisis signal is suppressed by the observer's
% performative stoicism (Section 1: "Experiencing the actual trouble").
narrative_ontology:constraint_victim(other_peoples_troubles_2026, isolated_sufferers).

% NOTE: requires_active_enforcement is NOT declared because this constraint
% emerges naturally from cognitive distance and empathy limits — it does not
% dissolve without institutional enforcement. The asymmetry is sustained by
% psychological architecture, not by an enforcing institution.
