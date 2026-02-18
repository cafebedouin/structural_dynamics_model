% ============================================================================
% CONSTRAINT STORY: ulysses_cyclops_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_cyclops, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ulysses_cyclops_1904
 * human_readable: The Cyclopean Snare (Barney Kiernan's Pub)
 * domain: social/political/nationalist
 * * SUMMARY:
 * Leopold Bloom enters Barney Kiernan's pub, a site of fierce nationalist 
 * fervor led by "The Citizen." The social environment functions as a Rope 
 * of coordination for the "true men," but becomes a Snare of xenophobic 
 * extraction for Bloom, culminating in a literal assault with a biscuit 
 * tin. The narrative is defined by "Gigantism"—hyperbolic parodies that 
 * create a peak Theater Ratio.
 * * KEY AGENTS:
 * - Leopold Bloom: Subject (Powerless) - The "walking jew" in the lion's den.
 * - The Citizen: Beneficiary (Institutional) - The "huge asphodel" of Irish nationalism.
 * - The Nameless Narrator: Auditor (Analytical) - The cynical debt collector 
 * observing the "bloody big foxy thief."
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_cyclops_1904, 0.54). % Highest extraction via physical threat.
domain_priors:suppression_score(ulysses_cyclops_1904, 0.88).   % Mono-vision (Cyclops) of nationalism.
domain_priors:theater_ratio(ulysses_cyclops_1904, 0.92).      % Peak "Gigantism" and parodic theater.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_cyclops_1904, extractiveness, 0.54).
narrative_ontology:constraint_metric(ulysses_cyclops_1904, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(ulysses_cyclops_1904, theater_ratio, 0.92).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_cyclops_1904, piton).
narrative_ontology:human_readable(ulysses_cyclops_1904, "The Cyclopean Snare (Barney Kiernan's Pub)").
narrative_ontology:topic_domain(ulysses_cyclops_1904, "social/political/nationalist").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_cyclops_1904, irish_nationalism).
narrative_ontology:constraint_victim(ulysses_cyclops_1904, leopold_bloom).
domain_priors:requires_active_enforcement(ulysses_cyclops_1904). % The Citizen's shouting and Garryowen's growling.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: LEOPOLD BLOOM (SNARE)
% Effective Extraction: 0.54 * 1.5 (powerless) * 0.8 (local) = 0.648.
% Perceived as a predatory trap of "malediction" and the "shouts of the mob."
constraint_indexing:constraint_classification(ulysses_cyclops_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE CITIZEN (ROPE)
% Effective Extraction: 0.54 * -0.2 (institutional) * 0.8 = -0.0864.
% Viewed as the essential coordination of "Sinn Fein" and "Ireland for the Irish."
constraint_indexing:constraint_classification(ulysses_cyclops_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.92) indicates a Piton of "epic" mummery.
constraint_indexing:constraint_classification(ulysses_cyclops_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_cyclops_1904_tests).

test(perspectival_gap) :-
    % Verify shift from Snare (Bloom) to Rope (Nationalists).
    constraint_indexing:constraint_classification(ulysses_cyclops_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_cyclops_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_cyclops_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_cyclops_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Leopold Bloom experiences the pub as a violent Snare—a site where his 
 * identity extracts physical and social safety. The high Theater Ratio 
 * (0.92) is the defining property of the "Cyclops" mode; the chapter is 
 * interrupted by massive, parodic interpolations of legal, scientific, 
 * and medieval styles that create a spectacular performance masking 
 * the atrophied function of the drunken "heroes." The Citizen’s 
 * biscuit tin assault is the final "Rope" of exclusionary 
 * coordination that forces Bloom’s "Elijah-like" exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_elijah_transfiguration,
    "Is Bloom's exit a Rope of spiritual transcendence or a Snare of panicked flight?",
    "Review of the 'great brightness' vs the 'furious driving' of the jarvey.",
    "Transcendence confirms a Rope; flight hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_cyclops_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking during the 'Gigantism' lists and the final transfiguration)
narrative_ontology:measurement(uc_tr_t0, ulysses_cyclops_1904, theater_ratio, 0, 0.70).
narrative_ontology:measurement(uc_tr_t5, ulysses_cyclops_1904, theater_ratio, 5, 0.95).
narrative_ontology:measurement(uc_tr_t10, ulysses_cyclops_1904, theater_ratio, 10, 0.92).

% Extraction (Increasing as the nationalist rhetoric turns toward physical threat)
narrative_ontology:measurement(uc_ex_t0, ulysses_cyclops_1904, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(uc_ex_t5, ulysses_cyclops_1904, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(uc_ex_t10, ulysses_cyclops_1904, base_extractiveness, 10, 0.54).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
