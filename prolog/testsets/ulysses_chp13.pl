% ============================================================================
% CONSTRAINT STORY: ulysses_nausicaa_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_nausicaa, []).

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
 * * constraint_id: ulysses_nausicaa_1904
 * human_readable: The Sentimental Snare (Sandymount Shore)
 * domain: social/artistic/psychological
 * * SUMMARY:
 * Chapter 13 models the encounter between Gerty MacDowell and Leopold Bloom 
 * on Sandymount Strand. For Gerty, the social requirement of "maidenly 
 * grace" and romantic sentimentality functions as a Snare, extracting 
 * her self-worth through cheap novelette ideals. For Bloom, the 
 * scene is a Piton of voyeuristic mummery, ending with the mocking 
 * "Cuckoo" of a clock that marks his cuckoldry.
 * * KEY AGENTS:
 * - Gerty MacDowell: Subject (Powerless) - Trapped in her "limp" and romantic "dream"[cite: 7013, 7015].
 * - Leopold Bloom: Auditor (Analytical) - The "foreign gentleman" watching from the rocks[cite: 7013, 7015].
 * - The Church: Beneficiary (Institutional) - The "Star of the Sea" providing a background of "pure radiance".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_nausicaa_1904, 0.49). % High emotional extraction/delusion.
domain_priors:suppression_score(ulysses_nausicaa_1904, 0.75).   % Social mores and Gerty's physical "secret".
domain_priors:theater_ratio(ulysses_nausicaa_1904, 0.88).      % Peak sentimentality: "mysterious embrace" and "lovingly".

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_nausicaa_1904, extractiveness, 0.49).
narrative_ontology:constraint_metric(ulysses_nausicaa_1904, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ulysses_nausicaa_1904, theater_ratio, 0.88).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_nausicaa_1904, piton).

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_nausicaa_1904, sentimental_press). % Novelettes and magazines.
narrative_ontology:constraint_victim(ulysses_nausicaa_1904, gerty_macdowell).        % The "girl friend" in the "favourite nook".
domain_priors:requires_active_enforcement(ulysses_nausicaa_1904). % The "cuckoo" clock and social shame.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: GERTY MACDOWELL (SNARE)
% Effective Extraction: 0.49 * 1.5 (powerless) * 0.8 (local) = 0.588.
% Perceived as a "mysterious embrace" that is actually a trap of physical and social limitation.
constraint_indexing:constraint_classification(ulysses_nausicaa_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE CHURCH/ST. MARY'S (ROPE)
% Effective Extraction: 0.49 * -0.2 (institutional) * 0.8 = -0.0784.
% Viewed as the essential coordination of "the voice of prayer" and a "beacon" for man.
constraint_indexing:constraint_classification(ulysses_nausicaa_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.88) indicates a Piton of atrophied romantic function.
constraint_indexing:constraint_classification(ulysses_nausicaa_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_nausicaa_1904_tests).

test(perspectival_gap) :-
    % Verify the shift from Gerty's Snare to the Church's Rope.
    constraint_indexing:constraint_classification(ulysses_nausicaa_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_nausicaa_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_validation) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_nausicaa_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_nausicaa_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Gerty MacDowell experiences the shore as a Sentimental Snare—a site 
 * where the "mysterious embrace" of the evening masks her social and physical 
 * "trapped" status[cite: 7013, 7015]. The high Theater Ratio (0.88) is 
 * driven by the first half of the chapter's "mummery" of flowery prose, which 
 * mimics the novelettes Gerty reads. The final "Cuckoo" of the 
 * clock  serves as the Piton's chime, signaling the atrophied 
 * reality of Bloom's marriage and Gerty's romantic hopes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_sentimental_truth,
    "Is Gerty's 'dream' a Rope for social hope or a Snare of self-deception?",
    "Review of the 'foreign gentleman's' gaze vs her physical 'limp'.",
    "Hope-coordination confirms a Rope; deception hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_nausicaa_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking during Gerty's internal monologue and the fireworks)
narrative_ontology:measurement(un_tr_t0, ulysses_nausicaa_1904, theater_ratio, 0, 0.65).
narrative_ontology:measurement(un_tr_t5, ulysses_nausicaa_1904, theater_ratio, 5, 0.90).
narrative_ontology:measurement(un_tr_t10, ulysses_nausicaa_1904, theater_ratio, 10, 0.88).

% Extraction (Increasing as the "sentimentality" gives way to Bloom's earthier reality)
narrative_ontology:measurement(un_ex_t0, ulysses_nausicaa_1904, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(un_ex_t5, ulysses_nausicaa_1904, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(un_ex_t10, ulysses_nausicaa_1904, base_extractiveness, 10, 0.49).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
