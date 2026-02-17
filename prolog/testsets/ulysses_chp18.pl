% ============================================================================
% CONSTRAINT STORY: ulysses_penelope_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_penelope, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ulysses_penelope_1904
 * human_readable: The Penelopean Affirmation (7 Eccles Street)
 * domain: social/psychological/biological
 * * SUMMARY:
 * Chapter 18 models the "stream of consciousness" of Molly Bloom as she lies 
 * in bed, unpunctuated and flowing toward a final "Yes". 
 * While her domestic labor and Leopold’s requests act as a Snare, her 
 * connection to the "flower of the mountain" and the earth’s fertility 
 * functions as an irreducible biological Mountain.
 * * KEY AGENTS:
 * - Molly Bloom: Subject (Powerless) - Lying in bed, "thinking of so many 
 * things".
 * - Leopold Bloom: Beneficiary (Institutional) - Asking for "breakfast in 
 * bed with a couple of eggs".
 * - The Universe (Gea-Tellus): Auditor (Analytical) - Witnessing the 
 * "yes I said yes I will Yes".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_penelope_1904, 0.50). % High emotional and physical labor of "yes".
domain_priors:suppression_score(ulysses_penelope_1904, 0.96).   % The ineluctable flow of the monologue.
domain_priors:theater_ratio(ulysses_penelope_1904, 0.97).      % Peak linguistic mummery: no punctuation.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_penelope_1904, extractiveness, 0.50).
narrative_ontology:constraint_metric(ulysses_penelope_1904, suppression_requirement, 0.96).
narrative_ontology:constraint_metric(ulysses_penelope_1904, theater_ratio, 0.97).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_penelope_1904, piton).
narrative_ontology:human_readable(ulysses_penelope_1904, "The Penelopean Affirmation (7 Eccles Street)").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_penelope_1904, biological_affirmation).
narrative_ontology:constraint_victim(ulysses_penelope_1904, molly_bloom).
domain_priors:requires_active_enforcement(ulysses_penelope_1904). % The "throb" and "mad" heart.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DOMESTIC SUBJECT (SNARE)
% Effective Extraction: 0.50 * 1.5 (powerless) * 0.8 (local) = 0.60.
% Perceived as a trap of "breakfast in bed" and "sick voice" pretenses 
% .
constraint_indexing:constraint_classification(ulysses_penelope_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: GEA-TELLUS (MOUNTAIN)
% Effective Extraction: 0.50 * -0.2 (institutional) * 1.2 (universal) = -0.12.
% χ < 0.05 check: The "flower of the mountain" is a fixed topological floor 
% .
constraint_indexing:constraint_classification(ulysses_penelope_1904, mountain, 
    context(agent_power(institutional), 
            time_horizon(civilizational), 
            exit_options(mobile), 
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.97) indicates a Piton of "unpunctuated" mummery 
% .
constraint_indexing:constraint_classification(ulysses_penelope_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_penelope_1904_tests).

test(perspectival_gap) :-
    % Verify shift from the Snare of domestic labor to the Mountain of "Yes".
    constraint_indexing:constraint_classification(ulysses_penelope_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_penelope_1904, mountain, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_penelope_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_penelope_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Penelope" chapter models the final "Affirmation" of the novel. 
 * While Molly's reality is a domestic Snare—extracting her labor and 
 * patience—her interiority creates a peak Theater Ratio (0.97) through its 
 * unpunctuated stream. This stylistic 
 * "mummery" masks the atrophied function of linear narrative, rendering the 
 * chapter a Piton for the observer.
 * MANDATROPHY ANALYSIS:
 * The final "Yes" resolves the Mandatrophy of the day; all "short circuits" 
 * collapse into the irreducible Mountain of life’s "crimson" and 
 * "glorious" suns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_penelopean_affirmation,
    "Is the 'Yes' a Rope for human connection or a Snare of cyclical repetition?",
    "Review of the 'flower of the mountain' vs the 'breakfast in bed' labor.",
    "Affirmation confirms a Rope; repetition hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_penelope_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking as the stream of consciousness reaches its final unpunctuated flow)
narrative_ontology:measurement(up_tr_t0, ulysses_penelope_1904, theater_ratio, 0, 0.80).
narrative_ontology:measurement(up_tr_t5, ulysses_penelope_1904, theater_ratio, 5, 0.95).
narrative_ontology:measurement(up_tr_t10, ulysses_penelope_1904, theater_ratio, 10, 0.97).

% Extraction (Accumulating with Molly's long night of memory and Bloom's requests)
narrative_ontology:measurement(up_ex_t0, ulysses_penelope_1904, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(up_ex_t5, ulysses_penelope_1904, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(up_ex_t10, ulysses_penelope_1904, base_extractiveness, 10, 0.50).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
