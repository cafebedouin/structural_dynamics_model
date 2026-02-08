% ============================================================================
% CONSTRAINT STORY: ulysses_proteus_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_proteus, []).

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
 * * constraint_id: ulysses_proteus_1904
 * human_readable: The Ineluctable Modality (Sandymount Strand)
 * domain: philosophical/social/technological
 * * SUMMARY:
 * Stephen Dedalus navigates Sandymount Strand, bound by the "ineluctable 
 * modality of the visible". This perceptual Mountain forces him 
 * to read the "signatures of all things". For Stephen, the 
 * experience is a Snare of internal brooding, where memories of Paris and 
 * family decay act as "heavy sands" of language silted by the past.
 * * KEY AGENTS:
 * - Stephen Dedalus: Subject (Powerless) - Walking through "short times of space"[cite: 603].
 * - The Physical Universe: Beneficiary (Institutional) - Enforcing the "lex eterna".
 * - The Reader/Observer: Auditor (Analytical) - Witnessing the "mummer" of thought[cite: 832].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_proteus_1904, 0.52). % High due to "Agenbite of inwit"[cite: 237, 836].
domain_priors:suppression_score(ulysses_proteus_1904, 0.85).   % The "ineluctable" nature of vision and sound[cite: 595, 604].
domain_priors:theater_ratio(ulysses_proteus_1904, 0.72).      % Internal "mummery" and "pretenders"[cite: 773, 831].

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_proteus_1904, extractiveness, 0.52).
narrative_ontology:constraint_metric(ulysses_proteus_1904, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ulysses_proteus_1904, theater_ratio, 0.72).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_proteus_1904, piton).

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_proteus_1904, physical_reality).
narrative_ontology:constraint_victim(ulysses_proteus_1904, stephen_dedalus).
domain_priors:requires_active_enforcement(ulysses_proteus_1904). % Memory and guilt as active "tyrants"[cite: 443].

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE)
% Effective Extraction: 0.52 * 1.5 (powerless) * 0.8 (local) = 0.624.
% Perceived as a trap of "unwholesome sandflats" and "sewage breath".
constraint_indexing:constraint_classification(ulysses_proteus_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE PHYSICAL UNIVERSE (MOUNTAIN)
% Effective Extraction: 0.52 * -0.2 (institutional) * 1.0 (national/universal) = -0.104.
% The "lex eterna"  functions as a fixed topological floor.
constraint_indexing:constraint_classification(ulysses_proteus_1904, mountain, 
    context(agent_power(institutional), 
            time_horizon(civilizational), 
            exit_options(mobile), 
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Theater Ratio (0.72) suggests a Piton of "vanished crowds" and "mummery"[cite: 513, 831].
constraint_indexing:constraint_classification(ulysses_proteus_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_proteus_1904_tests).

test(perspectival_gap) :-
    % Verify the shift from Snare (Subjective guilt) to Mountain (Objective physics).
    constraint_indexing:constraint_classification(ulysses_proteus_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_proteus_1904, mountain, context(agent_power(institutional), _, _, _)).

test(piton_theater_validation) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_proteus_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_proteus_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Stephen’s walk is a meditation on the Mountain of the "Ineluctable". 
 * While physics provides a stable floor (the "nacheinander" of time)[cite: 603], 
 * his psychological state is a Snare. The "heavy sands" of his past—Paris 
 * poverty [cite: 689], his mother’s death[cite: 702], and uncle Richie’s 
 * "houses of decay" [cite: 649]—extract spiritual energy, leaving him 
 * "lonely here"[cite: 843]. The high Theater Ratio (0.72) reflects his 
 * "mummer" nature, where internal monologues on Aristotle and 
 * Berkeley mask his social "dispossession"[cite: 694].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_protean_identity,
    "Is the 'other me' [cite: 693] a stable Rope of identity or a shifting Snare of solipsism?",
    "Review of the 'word known to all men'  vs 'phantasmal mirth'[cite: 131].",
    "Word-known confirms a Rope; phantasmal-mirth hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_proteus_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking as Stephen's internal mummery becomes more abstract)
narrative_ontology:measurement(up_tr_t0, ulysses_proteus_1904, theater_ratio, 0, 0.60).
narrative_ontology:measurement(up_tr_t5, ulysses_proteus_1904, theater_ratio, 5, 0.75).
narrative_ontology:measurement(up_tr_t10, ulysses_proteus_1904, theater_ratio, 10, 0.72).

% Extraction (The increasing weight of "Agenbite of inwit" and hunger)
narrative_ontology:measurement(up_ex_t0, ulysses_proteus_1904, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(up_ex_t5, ulysses_proteus_1904, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(up_ex_t10, ulysses_proteus_1904, base_extractiveness, 10, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
