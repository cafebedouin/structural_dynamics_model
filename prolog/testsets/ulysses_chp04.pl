% ============================================================================
% CONSTRAINT STORY: ulysses_calypso_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_calypso, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ulysses_calypso_1904
 * human_readable: The Domestic Calypso (7 Eccles Street)
 * domain: social/economic/religious
 * * SUMMARY:
 * Leopold Bloom navigates the "loose brass quoits" of his domestic life. 
 * While the morning routine functions as a Rope of coordination (preparing 
 * Molly's tea and kidney), Bloom is caught in a Snare of looming 
 * marital infidelity (the letter from Blazes Boylan) and the "grey sunken 
 * cunt of the world" of Zionist/Orientalist desolation.
 * * KEY AGENTS:
 * - Leopold Bloom: Subject (Powerless) - Moving on "quietly creaky boots".
 * - Molly Bloom: Beneficiary (Institutional) - Ruling from her "ample bedwarmed flesh".
 * - Blazes Boylan: Auditor (Analytical) - The "bold hand" on the morning letters.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_calypso_1904, 0.47). % High due to marital anxiety and debt symbols.
domain_priors:suppression_score(ulysses_calypso_1904, 0.60).   % Domestic habits and "ineluctable" hunger.
domain_priors:theater_ratio(ulysses_calypso_1904, 0.75).      % High theater: "metempsychosis", Orientalist daydreams, and Plasto's hat labels.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_calypso_1904, extractiveness, 0.47).
narrative_ontology:constraint_metric(ulysses_calypso_1904, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(ulysses_calypso_1904, theater_ratio, 0.75).

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_calypso_1904, marion_bloom). % Receiving breakfast in bed.
narrative_ontology:constraint_victim(ulysses_calypso_1904, leopold_bloom).    % The "servant" of the house.
domain_priors:requires_active_enforcement(ulysses_calypso_1904). % Molly's calls: "Poldy! Scald the teapot."

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: LEOPOLD BLOOM (SNARE)
% Effective Extraction: 0.47 * 1.5 (powerless) * 0.8 (local) = 0.564.
% Perceived as a trap of "useless" affection and looming "destiny" (Milly/Molly).
constraint_indexing:constraint_classification(ulysses_calypso_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: MOLLY BLOOM (ROPE)
% Effective Extraction: 0.47 * -0.2 (institutional) * 0.8 = -0.0752.
% Viewed as the essential coordination of being "parched" and needing tea.
constraint_indexing:constraint_classification(ulysses_calypso_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Theater Ratio (0.75) indicates a Piton of "Agendath Netaim" and "metempsychosis".
constraint_indexing:constraint_classification(ulysses_calypso_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_calypso_1904_tests).

test(perspectival_gap) :-
    % Verify the shift from Snare (Bloom's anxiety) to Rope (Molly's breakfast).
    constraint_indexing:constraint_classification(ulysses_calypso_1904, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_calypso_1904, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_calypso_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_calypso_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Leopold Bloom perceives his domestic life as a Snare—a predatory trap of 
 * marital insecurity ("Bold hand. Mrs Marion") and the "grey horror" of 
 * a "dead land"[cite: 1024]. Conversely, Molly perceives the morning as a Rope 
 * of coordination, where Bloom is the "server" bringing her "Love's Old 
 * Sweet Song" and tea[cite: 1041, 1069]. The high Theater Ratio (0.75) 
 * is driven by Bloom's performative fantasies about "Oriental" Jaffa 
 * [cite: 1002] and the "mummery" of explaining Greek terms like 
 * "metempsychosis" to mask his visceral "desolation"[cite: 1024, 1080].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_metempsychosis,
    "Is Bloom's life a unique Rope of new experience or a Snare of reincarnation?",
    "Review of the 'word known to all men' vs the 'smudge of the dead' in Titbits.",
    "Reincarnation confirms the Snare; agency confirms the Rope.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_calypso_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Bloom's daydreaming peaking during his walk to Dlugacz's)
narrative_ontology:measurement(uc_tr_t0, ulysses_calypso_1904, theater_ratio, 0, 0.65).
narrative_ontology:measurement(uc_tr_t5, ulysses_calypso_1904, theater_ratio, 5, 0.80).
narrative_ontology:measurement(uc_tr_t10, ulysses_calypso_1904, theater_ratio, 10, 0.75).

% Extraction (Bloom's psychological cost rising as he reads the letters)
narrative_ontology:measurement(uc_ex_t0, ulysses_calypso_1904, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uc_ex_t5, ulysses_calypso_1904, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(uc_ex_t10, ulysses_calypso_1904, base_extractiveness, 10, 0.47).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
