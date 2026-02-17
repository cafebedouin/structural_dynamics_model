% ============================================================================
% CONSTRAINT STORY: ulysses_scylla_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_scylla, []).

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
 * * constraint_id: ulysses_scylla_1904
 * human_readable: The Hamlet Algebra (National Library)
 * domain: social/religious/philosophical
 * * SUMMARY:
 * Stephen Dedalus performs his elaborate "Hamlet" theory in the National 
 * Library, navigating between the Scylla of Aristotelian dogmatic realism 
 * and the Charybdis of Platonic mysticism[cite: 7009, 6807]. The library 
 * functions as a Piton of atrophied intellectualism, while Stephen’s theory 
 * serves as a personal Scaffold for his artistic identity, even as his 
 * auditors perceive it as a Snare of "perverted ingenuity"[cite: 7948].
 * * KEY AGENTS:
 * - Stephen Dedalus: Subject (Powerless) - The "horrible example of free thought"[cite: 6719].
 * - John Eglinton: Beneficiary (Institutional) - The "elder's gall" challenging the bard[cite: 7948].
 * - The Quaker Librarian: Auditor (Analytical) - Purring "urbane" comfort[cite: 7948].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_scylla_1904, 0.50). % High spiritual/intellectual labor[cite: 6525].
domain_priors:suppression_score(ulysses_scylla_1904, 0.70).   % The "mummery" of letters and tradition.
domain_priors:theater_ratio(ulysses_scylla_1904, 0.90).      % Peak performance: "Shakespeare's ghost".

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_scylla_1904, extractiveness, 0.50).
narrative_ontology:constraint_metric(ulysses_scylla_1904, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(ulysses_scylla_1904, theater_ratio, 0.90).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_scylla_1904, piton).
narrative_ontology:human_readable(ulysses_scylla_1904, "The Hamlet Algebra (National Library)").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_scylla_1904, dublin_intelligentsia). % Coordination of culture[cite: 7948].
narrative_ontology:constraint_victim(ulysses_scylla_1904, stephen_dedalus).           % The rejected "bard"[cite: 6480].
domain_priors:requires_active_enforcement(ulysses_scylla_1904). % The "gall" of John Eglinton[cite: 7948].
narrative_ontology:has_sunset_clause(ulysses_scylla_1904).     % Stephen's theory is a temporary "movement".

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE)
% Effective Extraction: 0.50 * 1.5 (powerless) * 0.8 (local) = 0.60.
% Perceived as a trap of "toothless terrors" and social mockery[cite: 6998, 6429].
constraint_indexing:constraint_classification(ulysses_scylla_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE LITERARY CIRCLE (ROPE)
% Effective Extraction: 0.50 * -0.2 (institutional) * 0.8 = -0.08.
% Viewed as the essential coordination of "priceless pages" and debate[cite: 7948].
constraint_indexing:constraint_classification(ulysses_scylla_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.90) indicates a Piton of "vanished crowds" and mummery[cite: 6927].
constraint_indexing:constraint_classification(ulysses_scylla_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE CREATIVE ARCHITECT (SCAFFOLD)
% The "Hamlet" theory is a temporary support for artistic birth.
constraint_indexing:constraint_classification(ulysses_scylla_1904, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(ulysses_scylla_1904).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_scylla_1904_tests).

test(perspectival_gap) :-
    % Verify shift from the Snare of Stephen's isolation to the Rope of culture.
    constraint_indexing:constraint_classification(ulysses_scylla_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_scylla_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_validation) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_scylla_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_scylla_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Stephen Dedalus experiences the National Library as a Scylla-and-Charybdis 
 * Snare—a predatory trap of intellectual "mummery" where his theory is 
 * used to extract his spiritual energy for the entertainment of "swine"[cite: 6494]. 
 * Conversely, the librarians perceive it as a Rope of coordination, 
 * a "sea of troubles" navigated through "priceless pages" of Meister[cite: 7948]. 
 * The high Theater Ratio (0.90) is the defining feature; Stephen proves 
 * by "algebra" that Shakespeare is his own father's ghost, a performance 
 * that anchors his identity (Scaffold) while serving as a spectacular 
 * atrophied function (Piton) for the Dublin circle[cite: 6847, 6927].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_intellectual_paternity,
    "Is Stephen's theory a Rope for his own artistic birth or a Snare of solipsism?",
    "Review of the 'word known to all men' vs the 'mummery of letters'[cite: 7258, 6849].",
    "Creative birth confirms a Rope; 'phantasmal mirth' hardens the Snare[cite: 6545].",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_scylla_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking during the climax of the Shakespearean proof)
narrative_ontology:measurement(us_tr_t0, ulysses_scylla_1904, theater_ratio, 0, 0.75).
narrative_ontology:measurement(us_tr_t5, ulysses_scylla_1904, theater_ratio, 5, 0.95).
narrative_ontology:measurement(us_tr_t10, ulysses_scylla_1904, theater_ratio, 10, 0.90).

% Extraction (The weight of Stephen's spiritual "debt" and creative labor)
narrative_ontology:measurement(us_ex_t0, ulysses_scylla_1904, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(us_ex_t5, ulysses_scylla_1904, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(us_ex_t10, ulysses_scylla_1904, base_extractiveness, 10, 0.50).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
