% ============================================================================
% CONSTRAINT STORY: ulysses_ithaca_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_ithaca, []).

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
 * * constraint_id: ulysses_ithaca_1904
 * human_readable: The Mathematical Catechism (7 Eccles Street)
 * domain: technological/scientific/social
 * * SUMMARY:
 * Chapter 17 models the return of Bloom and Stephen to a state of "rest" 
 * through a cold, scientific catechism. 
 * Human experience is reduced to "parallel courses" and the "perpetual 
 * motion of the earth," creating an immutable biological and physical 
 * Mountain. The domestic coordination 
 * of the "duumvirate" functions as a Rope, while the clinical style 
 * acts as a Piton of atrophied emotion.
 * * KEY AGENTS:
 * - Stephen Dedalus: Subject (Powerless) - The "childman weary".
 * - Leopold Bloom: Beneficiary (Institutional) - Coordinating the "itinerary" 
 * and "budget".
 * - The Physical Universe: Auditor (Analytical) - Enforcing "everchanging 
 * tracks of neverchanging space".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_ithaca_1904, 0.46). % High due to the "weary" labor of the long day.
domain_priors:suppression_score(ulysses_ithaca_1904, 0.95).   % Determinism of physics and geometry.
domain_priors:theater_ratio(ulysses_ithaca_1904, 0.95).      % Maximal "catechism" theater.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_ithaca_1904, extractiveness, 0.46).
narrative_ontology:constraint_metric(ulysses_ithaca_1904, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(ulysses_ithaca_1904, theater_ratio, 0.95).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_ithaca_1904, piton).
narrative_ontology:human_readable(ulysses_ithaca_1904, "The Mathematical Catechism (7 Eccles Street)").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_ithaca_1904, scientific_determinism).
narrative_ontology:constraint_victim(ulysses_ithaca_1904, human_emotion).
domain_priors:requires_active_enforcement(ulysses_ithaca_1904). % The "proper perpetual motion of the earth".

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE WEARY SUBJECT (SNARE)
% Effective Extraction: 0.46 * 1.5 (powerless) * 1.2 (global/universal) = 0.828.
% Perceived as a trap of "everchanging tracks" and "neverchanging space".
constraint_indexing:constraint_classification(ulysses_ithaca_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE DOMESTIC DUUMVIRATE (ROPE)
% Effective Extraction: 0.46 * -0.2 (institutional) * 1.2 = -0.11.
% Viewed as the essential "united" coordination of "parallel courses".
constraint_indexing:constraint_classification(ulysses_ithaca_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.95) indicates a Piton of "mathematical" mummery.
constraint_indexing:constraint_classification(ulysses_ithaca_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_ithaca_1904_tests).

test(perspectival_gap) :-
    % Verify shift from the Snare of the weary subject to the Rope of domestic rest.
    constraint_indexing:constraint_classification(ulysses_ithaca_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_ithaca_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_validation) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_ithaca_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_ithaca_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Ithaca" chapter represents the final "Rest" of the novel. While 
 * the setting is a domestic Rope—a site of shared "parallel courses" and 
 * cocoa—the clinical, catechistic style creates a near-total Theater 
 * Ratio (0.95). This "mummery" of 
 * mathematical precision masks the atrophied function of human narrative, 
 * rendering the chapter a Piton for the analytical observer.
 * MANDATROPHY ANALYSIS:
 * The "state of rest" at the end resolves the Mandatrophy of the long day; 
 * the characters collapse into the irreducible Mountain of the earth's 
 * "proper perpetual motion".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ithacan_rest,
    "Is the 'state of rest' a Rope for recovery or a Snare of spiritual exhaustion?",
    "Review of the 'manchild in the womb' vs 'childman weary'.",
    "Recovery confirms a Rope; persistent inertia hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ulysses_ithaca_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Escalating as the questions and answers become more abstract and clinical)
narrative_ontology:measurement(ui_tr_t0, ulysses_ithaca_1904, theater_ratio, 0, 0.70).
narrative_ontology:measurement(ui_tr_t5, ulysses_ithaca_1904, theater_ratio, 5, 0.85).
narrative_ontology:measurement(ui_tr_t10, ulysses_ithaca_1904, theater_ratio, 10, 0.95).

% Extraction (Accumulating with the profound weariness and the "perpetual motion" of the day)
narrative_ontology:measurement(ui_ex_t0, ulysses_ithaca_1904, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ui_ex_t5, ulysses_ithaca_1904, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ui_ex_t10, ulysses_ithaca_1904, base_extractiveness, 10, 0.46).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
