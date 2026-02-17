% ============================================================================
% CONSTRAINT STORY: ulysses_lestrygonians_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_lestrygonians, []).

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
 * * constraint_id: ulysses_lestrygonians_1904
 * human_readable: The Lestrygonian Food Chain (Lunchtime Dublin)
 * domain: social/economic/biological
 * * SUMMARY:
 * Leopold Bloom wanders Dublin at lunchtime, contemplating the "predatory" 
 * nature of existence. He views the social and biological requirement of 
 * eating as a Snare where the "Burton" diners wolf down food like animals, 
 * while institutions use the "Blood of the Lamb" and "Plumtree's Potted Meat" 
 * as Ropes of social and religious coordination.
 * * KEY AGENTS:
 * - Leopold Bloom: Subject (Powerless) - Avoiding the "worst man in Dublin".
 * - The Lestrygonians (Burton Diners): Beneficiary (Institutional) - Wolfing down "gory meat".
 * - The Y.M.C.A. Man: Auditor (Analytical) - Distributing "Blood of the Lamb" throwaways.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_lestrygonians_1904, 0.50). % High predatory extraction.
domain_priors:suppression_score(ulysses_lestrygonians_1904, 0.80).   % Biological necessity of hunger.
domain_priors:theater_ratio(ulysses_lestrygonians_1904, 0.75).      % High social mummery/advertising.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_lestrygonians_1904, extractiveness, 0.50).
narrative_ontology:constraint_metric(ulysses_lestrygonians_1904, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(ulysses_lestrygonians_1904, theater_ratio, 0.75).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_lestrygonians_1904, piton).
narrative_ontology:human_readable(ulysses_lestrygonians_1904, "The Lestrygonian Food Chain (Lunchtime Dublin)").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_lestrygonians_1904, social_hierarchy).
narrative_ontology:constraint_victim(ulysses_lestrygonians_1904, leopold_bloom).
domain_priors:requires_active_enforcement(ulysses_lestrygonians_1904). % The "enforced" manners of the Burton.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE HUNGRY SUBJECT (SNARE)
% Effective Extraction: 0.50 * 1.5 (powerless) * 0.8 (local) = 0.60.
% Bloom perceives the Burton restaurant as a predatory trap of "stinking" food.
constraint_indexing:constraint_classification(ulysses_lestrygonians_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL DINER (ROPE)
% Effective Extraction: 0.50 * -0.2 (institutional) * 0.8 = -0.08.
% Viewed as the essential "nourishment" and coordination of the "Great Daily Organ".
constraint_indexing:constraint_classification(ulysses_lestrygonians_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.75) indicates a Piton of atrophied social "respectability."
constraint_indexing:constraint_classification(ulysses_lestrygonians_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_lestrygonians_1904_tests).

test(perspectival_gap) :-
    % Verify the shift from Snare (Bloom's disgust) to Rope (Social function).
    constraint_indexing:constraint_classification(ulysses_lestrygonians_1904, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_lestrygonians_1904, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_lestrygonians_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_lestrygonians_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Leopold Bloom experiences lunchtime as the "Lestrygonian Snare." While 
 * the city coordinates around the lunch hour (Rope), Bloom sees the 
 * "cannibalistic" extraction of the Burton diners—"men, men, men" bolting 
 * down "gory meat". The high Theater Ratio (0.75) reflects the 
 * performative nature of social dining and advertisements like 
 * "Plumtree's Potted Meat" which mask the irreducible Mountain of 
 * biological hunger and death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_biological_snare,
    "Is 'social dining' a Rope of civilizational progress or a Snare of animal regression?",
    "Review of the 'Burton' vs 'Davy Byrne's' dining modalities.",
    "Temperance confirms a Rope; 'wolfing' hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_lestrygonians_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking as social etiquette is abandoned at the Burton)
narrative_ontology:measurement(ul_tr_t0, ulysses_lestrygonians_1904, theater_ratio, 0, 0.60).
narrative_ontology:measurement(ul_tr_t5, ulysses_lestrygonians_1904, theater_ratio, 5, 0.85).
narrative_ontology:measurement(ul_tr_t10, ulysses_lestrygonians_1904, theater_ratio, 10, 0.75).

% Extraction (Increasing as the "predatory" lunch hour intensifies)
narrative_ontology:measurement(ul_ex_t0, ulysses_lestrygonians_1904, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ul_ex_t5, ulysses_lestrygonians_1904, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ul_ex_t10, ulysses_lestrygonians_1904, base_extractiveness, 10, 0.50).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
