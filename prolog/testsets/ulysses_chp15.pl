% ============================================================================
% CONSTRAINT STORY: ulysses_circe_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_circe, []).

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
 * * constraint_id: ulysses_circe_1904
 * human_readable: The Nighttown Phantasmagoria (Circe)
 * domain: social/psychological/religious
 * * SUMMARY:
 * Chapter 15 models the "Nighttown" red-light district as a site of extreme 
 * hallucinatory extraction. The environment functions as a predatory Snare 
 * where the guilt and desires of Bloom and Stephen are externalized into a 
 * "mummery" of shifting forms. Amidst 
 * this theater, Bloom’s vision of his dead son, Rudy, acts as an 
 * irreducible psychological Mountain.
 * * KEY AGENTS:
 * - Leopold Bloom: Subject (Powerless) - Standing "on guard" as a 
 * "secret master".
 * - Stephen Dedalus: Subject (Powerless) - Trembling at his "soul's cry" 
 *.
 * - The Nighttown Inhabitants: Beneficiary (Institutional) - Managing the 
 * "grimy houses" and "ice gondolas".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_circe_1904, 0.58). % Extreme spiritual and moral extraction.
domain_priors:suppression_score(ulysses_circe_1904, 0.90).   % The hallucinatory "entombing" of the subjects.
domain_priors:theater_ratio(ulysses_circe_1904, 0.99).      % Maximal mummery: the entire chapter is a play script.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_circe_1904, extractiveness, 0.58).
narrative_ontology:constraint_metric(ulysses_circe_1904, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(ulysses_circe_1904, theater_ratio, 0.99).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_circe_1904, piton).
narrative_ontology:human_readable(ulysses_circe_1904, "The Nighttown Phantasmagoria (Circe)").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_circe_1904, nighttown_economy). % Prostitution and "ice gondolas".
narrative_ontology:constraint_victim(ulysses_circe_1904, leopold_bloom).          % Subject to hallucinations and mockery.
domain_priors:requires_active_enforcement(ulysses_circe_1904). % Whistles, calls, and answers.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE HALLUCINATING SUBJECT (SNARE)
% Effective Extraction: 0.58 * 1.5 (powerless) * 0.8 (local) = 0.696.
% Perceived as a trap of "skeleton tracks" and "danger signals".
constraint_indexing:constraint_classification(ulysses_circe_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE PARENTAL BOND (MOUNTAIN)
% Effective Extraction: 0.58 * -0.2 (institutional/fixed) * 1.2 = -0.139.
% χ < 0.05 check: The vision of Rudy is an irreducible fixed point of 
% the psychological landscape.
constraint_indexing:constraint_classification(ulysses_circe_1904, mountain, 
    context(agent_power(institutional), 
            time_horizon(civilizational), 
            exit_options(mobile), 
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.99) indicates a Piton of maximal mummery 
% and atrophied narrative reality.
constraint_indexing:constraint_classification(ulysses_circe_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_circe_1904_tests).

test(perspectival_gap) :-
    % Verify shift from the Snare of the district to the Mountain of Rudy.
    constraint_indexing:constraint_classification(ulysses_circe_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_circe_1904, mountain, context(agent_power(institutional), _, _, _)).

test(piton_theater_validation) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_circe_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_circe_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Circe" chapter represents the ultimate "Theater" of the novel. 
 * While the setting is a social Snare of predatory extraction, the 
 * hallucinatory style creates a near-total Theater Ratio (0.99), 
 * rendering the narrative a Piton—a structure where direct function 
 * is replaced by "mummery". 
 * MANDATROPHY ANALYSIS:
 * The "Rudy" vision at the end resolves the Mandatrophy of the 
 * chapter; the chaos of Nighttown collapses into the irreducible 
 * Mountain of Bloom's paternal love and grief.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_nighttown_reality,
    "Is the phantasmagoria a Rope for psychological catharsis or a Snare for the soul?",
    "Review of the 'Rudy' vision vs the 'ghoul' hallucinations.",
    "Catharsis confirms a Rope; persistent entrapment hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_circe_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Escalating as the hallucinations become more complex and surreal)
narrative_ontology:measurement(uc_tr_t0, ulysses_circe_1904, theater_ratio, 0, 0.85).
narrative_ontology:measurement(uc_tr_t5, ulysses_circe_1904, theater_ratio, 5, 0.95).
narrative_ontology:measurement(uc_tr_t10, ulysses_circe_1904, theater_ratio, 10, 0.99).

% Extraction (Accumulating with the subjects' fatigue, drunkenness, and spiritual labor)
narrative_ontology:measurement(uc_ex_t0, ulysses_circe_1904, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(uc_ex_t5, ulysses_circe_1904, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(uc_ex_t10, ulysses_circe_1904, base_extractiveness, 10, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
