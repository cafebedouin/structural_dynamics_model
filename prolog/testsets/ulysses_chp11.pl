% ============================================================================
% CONSTRAINT STORY: ulysses_sirens_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_sirens, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ulysses_sirens_1904
 * human_readable: The Auditory Lure (Ormond Hotel)
 * domain: social/artistic/biological
 * * SUMMARY:
 * Chapter 11 models the Ormond Hotel bar as a complex auditory system. 
 * The music and the flirtatious "Sirens" act as a Rope of coordination 
 * for the bar patrons, while for Leopold Bloom, they form a Snare 
 * of "longindying" calls and psychological extraction as he dwells 
 * on his loneliness and Molly's infidelity.
 * * KEY AGENTS:
 * - Leopold Bloom: Subject (Powerless) - "So lonely blooming" while eating grease. 
 * - Lydia Douce & Mina Kennedy: Beneficiary (Institutional) - The trilling barmaids. 
 * - The Stripling: Auditor (Analytical) - The unseeing presence tapping at the door. 
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds [cite: 12]
domain_priors:base_extractiveness(ulysses_sirens_1904, 0.53). % High psychological cost.
domain_priors:suppression_score(ulysses_sirens_1904, 0.82).   % Auditory seduce-and-suppress.
domain_priors:theater_ratio(ulysses_sirens_1904, 0.89).      % Peak "mummery" of sound.

% Primary keys for classification engine [cite: 12]
narrative_ontology:constraint_metric(ulysses_sirens_1904, extractiveness, 0.53).
narrative_ontology:constraint_metric(ulysses_sirens_1904, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(ulysses_sirens_1904, theater_ratio, 0.89).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_sirens_1904, piton).

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_sirens_1904, the_sirens). % Lydia and Mina trilling.
narrative_ontology:constraint_victim(ulysses_sirens_1904, leopold_bloom).   % Trapped in sad blooming.
domain_priors:requires_active_enforcement(ulysses_sirens_1904). % The piano's "War! War!". 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE LONELY SUBJECT (SNARE)
% Effective Extraction: 0.53 * 1.5 (powerless) * 0.8 (local) = 0.636.
% Bloom perceives the music as a "decoy" and a trap of "lost" notes. 
constraint_indexing:constraint_classification(ulysses_sirens_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE BARMAID (ROPE)
% Effective Extraction: 0.53 * -0.2 (institutional) * 0.8 = -0.0848.
% Viewed as the "clinking" coordination of service and "peepofgold". 
constraint_indexing:constraint_classification(ulysses_sirens_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.89) indicates a Piton of atrophied social function.
constraint_indexing:constraint_classification(ulysses_sirens_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_sirens_1904_tests).

test(perspectival_gap) :-
    % Verify shift from Snare (Bloom) to Rope (Sirens). [cite: 13]
    constraint_indexing:constraint_classification(ulysses_sirens_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_sirens_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70. [cite: 13]
    domain_priors:theater_ratio(ulysses_sirens_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_sirens_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Leopold Bloom experiences the Ormond bar as an auditory Snare—a site 
 * where the "throb" of the music and the "trilling" of barmaids extracts 
 * his emotional equilibrium. The high Theater Ratio (0.89) is the 
 * defining property; the chapter is a "fugue" of onomatopoeia 
 * ("Imperthnthn thnthnthn") that creates a spectacular performance  
 * masking the atrophied function of genuine human connection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_auditory_entrapment,
    "Is the 'pure, long and throbbing' call a Rope for beauty or a Snare for the soul?",
    "Analysis of the 'decoy' effect vs the 'last rose of summer' resolution.",
    "Beauty confirms a Rope; 'alluring' entrapment hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_sirens_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking as the music and trilling intensify toward the end) [cite: 12]
narrative_ontology:measurement(us_tr_t0, ulysses_sirens_1904, theater_ratio, 0, 0.75).
narrative_ontology:measurement(us_tr_t5, ulysses_sirens_1904, theater_ratio, 5, 0.92).
narrative_ontology:measurement(us_tr_t10, ulysses_sirens_1904, theater_ratio, 10, 0.89).

% Extraction (Bloom's psychological fatigue accumulating with the piano chords) [cite: 12]
narrative_ontology:measurement(us_ex_t0, ulysses_sirens_1904, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(us_ex_t5, ulysses_sirens_1904, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(us_ex_t10, ulysses_sirens_1904, base_extractiveness, 10, 0.53).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
