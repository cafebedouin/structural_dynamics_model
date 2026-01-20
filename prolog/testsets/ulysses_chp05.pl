% ============================================================================
% CONSTRAINT STORY: bloom_secret_correspondence
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 5 (Lotus Eaters)
% ============================================================================

:- module(bloom_secret_correspondence, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: bloom_secret_correspondence
 * human_readable: The Flower/Martha Secret Correspondence
 * domain: social/emotional
 * temporal_scope: June 16, 1904 (Dublin morning)
 * spatial_scope: Westland Row post office and surrounding streets
 * * SUMMARY:
 * Leopold Bloom maintains a clandestine, eroticized correspondence with Martha Clifford 
 * under the pseudonym "Henry Flower". This constraint functions as a 
 * psychological "closet" that Bloom navigates through elaborate physical and social 
 * suppression techniques.
 * * KEY AGENTS:
 * - Leopold Bloom (Henry Flower): Moderate agent seeking narcotic emotional relief (Rope).
 * - Martha Clifford: Powerless correspondent trapped in a one-sided dynamic (Noose).
 * - The Catholic Church (All Hallows): Institutional observer viewing morality as law (Mountain).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Standardized interval and claim (Fixed: changed 'social_boundary' to 'noose')
narrative_ontology:interval(bloom_secret_correspondence, 0, 10).
narrative_ontology:constraint_claim(bloom_secret_correspondence, noose).

% Metrics: Extractiveness (0.4) and Suppression (0.8)
domain_priors:base_extractiveness(bloom_secret_correspondence, 0.4).
domain_priors:suppression_score(bloom_secret_correspondence, 0.8).
domain_priors:requires_active_enforcement(bloom_secret_correspondence).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(bloom_secret_correspondence, extractiveness, 0.4).
narrative_ontology:constraint_metric(bloom_secret_correspondence, suppression_requirement, 0.8).

% Beneficiaries & Victims
constraint_beneficiary(bloom_secret_correspondence, bloom_leopold).
constraint_victim(bloom_secret_correspondence, bloom_marion_molly).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom views the secrecy as a coordination tool to manage his domestic 
   unrest without destroying his primary marriage. 
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_secret_correspondence,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MARTHA CLIFFORD - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - Martha is socially constrained and "longing".
   WHY: She is trapped in an asymmetric dynamic where Bloom controls the 
   pseudonym, the rhythm, and the physical meeting.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_secret_correspondence,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CHURCH (All Hallows) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "Wonderful organisation... goes like clockwork".
   WHY: The Church views moral law as immutable; Bloom's "secret" is already 
   mapped into their theology of sin and confession.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_secret_correspondence,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(bloom_secret_correspondence_tests).

test(multi_perspective_conflict) :-
    constraint_indexing:constraint_classification(bloom_secret_correspondence, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(bloom_secret_correspondence, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 = rope, T2 = noose.

test(time_immutability_shift) :-
    % Institutional view sees morality as a Mountain over historical time
    constraint_indexing:constraint_classification(bloom_secret_correspondence, mountain, context(agent_power(institutional), time_horizon(historical), _, _)).

:- end_tests(bloom_secret_correspondence_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Standardizing the claim as 'noose' allows the Audit Suite to flag the 
 * perspectival gap between Bloom's 'Rope' and Martha's 'Noose'. 
 * This resolves the previous schema mismatch.
 */

omega_variable(
    martha_clifford_authenticity,
    "Is Martha a real person or a strategic fraud targeting lonely men?",
    resolution_mechanism("Verification of her physical address vs P.O. Box"),
    impact("If fraud: Rope (Bloom is being coordinated). If real: Noose (asymmetry)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Open Marital Reconciliation: Rejected due to active suppression of domestic 
 * friction and the 'Lotus' effect of the secret.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp05, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp05, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp05, noose, agent_power(individual_powerless)).
