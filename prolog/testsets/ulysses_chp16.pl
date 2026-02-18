% ============================================================================
% CONSTRAINT STORY: ulysses_eumaeus_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_eumaeus, []).

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
 * * constraint_id: ulysses_eumaeus_1904
 * human_readable: The Exhausted Coordination (Cabman's Shelter)
 * domain: social/economic/linguistic
 * * SUMMARY:
 * Leopold Bloom and Stephen Dedalus seek refuge in a cabman's shelter near 
 * Butt bridge. The social environment 
 * functions as a Rope of "orthodox Samaritan fashion" coordination, 
 * providing beverage and rest for the "e.d.ed" Stephen. 
 * However, the narrative style acts as a Piton of atrophied function, 
 * where clichés and circumlocution create an immense Theater Ratio.
 * * KEY AGENTS:
 * - Stephen Dedalus: Subject (Powerless) - "Mind... a bit unsteady" and 
 * physically exhausted.
 * - Leopold Bloom: Beneficiary (Institutional) - Coordinating the "conveyance" 
 * and "propriety" of the shelter.
 * - The Shelter Inhabitants: Auditor (Analytical) - Watching the two 
 * figures "both black" walk toward the bridge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_eumaeus_1904, 0.47). % High due to extreme fatigue and social "rub".
domain_priors:suppression_score(ulysses_eumaeus_1904, 0.70).   % The inertia of the late night hour.
domain_priors:theater_ratio(ulysses_eumaeus_1904, 0.85).      % High circumlocution and "orthodoxy".

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_eumaeus_1904, extractiveness, 0.47).
narrative_ontology:constraint_metric(ulysses_eumaeus_1904, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(ulysses_eumaeus_1904, theater_ratio, 0.85).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_eumaeus_1904, piton).
narrative_ontology:human_readable(ulysses_eumaeus_1904, "The Exhausted Coordination (Cabman's Shelter)").
narrative_ontology:topic_domain(ulysses_eumaeus_1904, "social/economic/linguistic").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_eumaeus_1904, social_charity).
narrative_ontology:constraint_victim(ulysses_eumaeus_1904, stephen_dedalus).
domain_priors:requires_active_enforcement(ulysses_eumaeus_1904). % Bloom's "duty" to take measures.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE)
% Effective Extraction: 0.47 * 1.5 (powerless) * 0.8 (local) = 0.564.
% Perceived as a trap of "unsteady" mind and the "rub" of finding beverage.
constraint_indexing:constraint_classification(ulysses_eumaeus_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: LEOPOLD BLOOM (ROPE)
% Effective Extraction: 0.47 * -0.2 (institutional) * 0.8 = -0.0752.
% Viewed as the essential coordination of "suitable ways and means".
constraint_indexing:constraint_classification(ulysses_eumaeus_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.85) indicates a Piton of "preparatory" mummery.
constraint_indexing:constraint_classification(ulysses_eumaeus_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_eumaeus_1904_tests).

test(perspectival_gap) :-
    % Verify shift from the Snare of Stephen's fatigue to the Rope of Bloom's coordination.
    constraint_indexing:constraint_classification(ulysses_eumaeus_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_eumaeus_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_eumaeus_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_eumaeus_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Eumaeus" chapter models the fatigue of late-night coordination. 
 * While the cabman's shelter serves as a Rope—a "Samaritan" structure 
 * providing safety—Stephen perceives it as a Snare of spiritual and physical 
 * exhaustion. The high Theater Ratio (0.85) 
 * is the chapter's defining feature; the circumlocutory style acts as a 
 * Piton, where the function of direct storytelling is atrophied into a 
 * "mummery" of clichés and "orthodox" preliminaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_samaritan_intent,
    "Is Bloom's 'orthodox Samaritan' coordination a Rope for recovery or a Snare of patronization?",
    "Analysis of the 'expedient' vs the 'unsteady' response of the subject.",
    "Recovery confirms a Rope; patronization hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_eumaeus_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking as the circumlocutory style intensifies through the walk)
narrative_ontology:measurement(ue_tr_t0, ulysses_eumaeus_1904, theater_ratio, 0, 0.70).
narrative_ontology:measurement(ue_tr_t5, ulysses_eumaeus_1904, theater_ratio, 5, 0.88).
narrative_ontology:measurement(ue_tr_t10, ulysses_eumaeus_1904, theater_ratio, 10, 0.85).

% Extraction (Accumulating with the subjects' profound fatigue and the "rub" of the hour)
narrative_ontology:measurement(ue_ex_t0, ulysses_eumaeus_1904, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ue_ex_t5, ulysses_eumaeus_1904, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(ue_ex_t10, ulysses_eumaeus_1904, base_extractiveness, 10, 0.47).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
