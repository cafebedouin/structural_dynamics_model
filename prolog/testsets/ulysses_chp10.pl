% ============================================================================
% CONSTRAINT STORY: ulysses_rocks_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_rocks, []).

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
 * * constraint_id: ulysses_rocks_1904
 * human_readable: The Dublin Simultaneity (Wandering Rocks)
 * domain: social/political/religious
 * * SUMMARY:
 * Chapter 10 models Dublin as a complex system of simultaneous movements and 
 * near-misses. The city acts as a Tangled Rope of coordination, where the 
 * progress of the Viceregal cavalcade and Father Conmee provides a 
 * structural floor for the diverse inhabitants, even as it extracts 
 * their attention and social deference through a high theater of 
 * colonial and religious "pomp".
 * * KEY AGENTS:
 * - Dublin Inhabitants: Subject (Powerless) - Navigating the city's 
 * "short circuits" and coincidences.
 * - The Viceregal Cavalcade: Beneficiary (Institutional) - Representing 
 * the imperial "pomp" and coordination of the state.
 * - Father Conmee: Auditor (Analytical) - Observing and blessing 
 * the city's movements while managing his own errands.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_rocks_1904, 0.48). 
domain_priors:suppression_score(ulysses_rocks_1904, 0.75).   
domain_priors:theater_ratio(ulysses_rocks_1904, 0.82). % High "pomp": viceregal salutes and blessings.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_rocks_1904, extractiveness, 0.48).
narrative_ontology:constraint_metric(ulysses_rocks_1904, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ulysses_rocks_1904, theater_ratio, 0.82).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_rocks_1904, piton).
narrative_ontology:human_readable(ulysses_rocks_1904, "The Dublin Simultaneity (Wandering Rocks)").
narrative_ontology:topic_domain(ulysses_rocks_1904, "social/political/religious").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_rocks_1904, colonial_and_church_authorities).
narrative_ontology:constraint_victim(ulysses_rocks_1904, dublin_citizens).
domain_priors:requires_active_enforcement(ulysses_rocks_1904). % The "timekeeper" and "bawled" tram departures.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DUBLIN CITIZEN (SNARE)
% Effective Extraction: 0.48 * 1.5 (powerless) * 0.8 (local) = 0.576.
% Perceived as a "short circuit" trap of coincidences and "stale" social debts.
constraint_indexing:constraint_classification(ulysses_rocks_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE VICEREGAL AUTHORITY (ROPE)
% Effective Extraction: 0.48 * -0.2 (institutional) * 0.8 = -0.0768.
% Viewed as the essential coordination of the "inauguration" and city "order".
constraint_indexing:constraint_classification(ulysses_rocks_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.82) indicates a Piton of "punctual salutes" and mummery.
constraint_indexing:constraint_classification(ulysses_rocks_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_rocks_1904_tests).

test(perspectival_gap) :-
    % Verify shift from Snare (Citizen) to Rope (Authority).
    constraint_indexing:constraint_classification(ulysses_rocks_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_rocks_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_rocks_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_rocks_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Dubliners experience the city's simultaneity as a Tangled Rope—it 
 * coordinates their disparate lives through the "Great Daily Organ" of 
 * shared space, yet it extracts their social deference through the "pomp" 
 * of authority. The high Theater Ratio (0.82) is driven by the 
 * "punctual salutes" of Master Patrick Aloysius Dignam and the "very 
 * reverend" John Conmee's blessings. These performative Ropes 
 * mask the atrophied function of the "becalmed" short circuits 
 * where citizens like the "onelegged sailor" are "pauperized".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_city_simultaneity,
    "Is the 'shifting' coordination of the city a Rope for social unity or a Snare of surveillance?",
    "Review of the 'blind stripling' vs the 'cavalcade' progress.",
    "Unity confirms a Rope; 'short circuit' surveillance hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_rocks_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking as the viceroy acknowledges salutes through the city)
narrative_ontology:measurement(ur_tr_t0, ulysses_rocks_1904, theater_ratio, 0, 0.65).
narrative_ontology:measurement(ur_tr_t5, ulysses_rocks_1904, theater_ratio, 5, 0.88).
narrative_ontology:measurement(ur_tr_t10, ulysses_rocks_1904, theater_ratio, 10, 0.82).

% Extraction (Increasing as the "short circuit" of urban coincidences accumulates)
narrative_ontology:measurement(ur_ex_t0, ulysses_rocks_1904, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ur_ex_t5, ulysses_rocks_1904, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ur_ex_t10, ulysses_rocks_1904, base_extractiveness, 10, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
