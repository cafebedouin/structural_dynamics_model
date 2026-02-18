% ============================================================================
% CONSTRAINT STORY: ulysses_hades_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_hades, []).

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
 * * constraint_id: ulysses_hades_1904
 * human_readable: The Funerary Mountain (Prospect Cemetery)
 * domain: social/religious/technological
 * * SUMMARY:
 * Leopold Bloom attends the funeral of Paddy Dignam, navigating the communal 
 * Ropes of mourning while confronting the absolute Mountain of death. 
 * The ritual coordination of the carriage ride and the interment is 
 * punctuated by the high theater of Father Coffey’s Latin and the 
 * "ineluctable" reality of decomposition and forgotten names.
 * * KEY AGENTS:
 * - Leopold Bloom: Subject (Powerless) - Counting the bared heads (thirteen).
 * - Father Coffey: Beneficiary (Institutional) - Balancing a book against his "toad's belly".
 * - John O'Connell: Auditor (Analytical) - The caretaker puzzling keys at his back.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_hades_1904, 0.49). % High due to biological decay and grief.
domain_priors:suppression_score(ulysses_hades_1904, 0.95).   % Death as an absolute "Fixed Point".
domain_priors:theater_ratio(ulysses_hades_1904, 0.88).      % The "Pomp of death" and Father Coffey's "fluent croak".

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_hades_1904, extractiveness, 0.49).
narrative_ontology:constraint_metric(ulysses_hades_1904, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(ulysses_hades_1904, theater_ratio, 0.88).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_hades_1904, piton).
narrative_ontology:human_readable(ulysses_hades_1904, "The Funerary Mountain (Prospect Cemetery)").
narrative_ontology:topic_domain(ulysses_hades_1904, "social/religious/technological").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_hades_1904, burial_societies). % Paying the penny a week for turf.
narrative_ontology:constraint_victim(ulysses_hades_1904, paddy_dignam).          % The "stiff" in the dealer box.
domain_priors:requires_active_enforcement(ulysses_hades_1904). % Gravediggers and caretakers enforcing "rest".

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DEPARTED (MOUNTAIN)
% Effective Extraction: 0.49 * 1.5 (powerless) * 1.2 (universal) = 0.882.
% χ < 0.05 logic check: In death, extraction ceases because agency is zero.
% However, for the survivor Bloom, it is a Mountain of "Inertial Limit".
constraint_indexing:constraint_classification(ulysses_hades_1904, mountain, 
    context(agent_power(powerless), 
            time_horizon(civilizational), 
            exit_options(trapped), 
            spatial_scope(universal))).

% PERSPECTIVE 2: THE CARETAKER (ROPE)
% Effective Extraction: 0.49 * -0.2 (institutional) * 0.8 (local) = -0.0784.
% Viewed as the essential coordination of "cheering a fellow up" and "keys".
constraint_indexing:constraint_classification(ulysses_hades_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.88) indicates a Piton of "vanished crowds" and "mummery".
constraint_indexing:constraint_classification(ulysses_hades_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_hades_1904_tests).

test(perspectival_gap) :-
    % Verify shift from the Mountain of death to the Rope of the caretaker.
    constraint_indexing:constraint_classification(ulysses_hades_1904, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_hades_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_hades_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_hades_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Leopold Bloom experiences Prospect Cemetery as a site of the Funerary 
 * Mountain—an irreducible "Fixed Point" where "once you are dead you are 
 * dead"[cite: 4007]. While burial societies and caretakers provide a Rope 
 * of coordination (managing "twenty or thirty funerals every day" [cite: 3911]), 
 * the high Theater Ratio (0.88) reflects the "Pomp of death". 
 * Bloom sees through this theater, noting that Father Coffey’s Latin 
 * "stupefies them first" [cite: 3520] and that the "resurrection and the 
 * life" is merely a "pump" that eventually "gets bunged up"[cite: 4005, 4006].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_mortality_theater,
    "Is the 'last day idea' a Rope for social hope or a Snare for religious extraction?",
    "Review of 'waiting for it to melt in their stomachs' vs 'safe in the arms of kingdom come'.",
    "Hope-coordination confirms a Rope; 'pious fraud' hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_hades_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking during the mortuary chapel service and Father Coffey's croak)
narrative_ontology:measurement(uh_tr_t0, ulysses_hades_1904, theater_ratio, 0, 0.75).
narrative_ontology:measurement(uh_tr_t5, ulysses_hades_1904, theater_ratio, 5, 0.92).
narrative_ontology:measurement(uh_tr_t10, ulysses_hades_1904, theater_ratio, 10, 0.88).

% Extraction (The weight of the "dead weight" coffin and Bloom's thoughts on his own plot)
narrative_ontology:measurement(uh_ex_t0, ulysses_hades_1904, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(uh_ex_t5, ulysses_hades_1904, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(uh_ex_t10, ulysses_hades_1904, base_extractiveness, 10, 0.49).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
