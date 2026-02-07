% ============================================================================
% CONSTRAINT STORY: ulysses_aeolus_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_aeolus, []).

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
 * * constraint_id: ulysses_aeolus_1904
 * human_readable: The Rhetorical Press (Aeolus)
 * domain: technological/social/political
 * * SUMMARY:
 * Leopold Bloom navigates the "clanking drums" of the Freeman's Journal office, 
 * attempting to coordinate a simple advertisement renewal. He is caught 
 * between the irreducible Mountain of the machines and the high-theater 
 * Snare of rhetorical bombast from Myles Crawford and the "windbag" orators.
 * * KEY AGENTS:
 * - Leopold Bloom: Subject (Powerless) - Rebuffed by the "K.M.R.I.A." response.
 * - Myles Crawford: Beneficiary (Institutional) - Controlling the "pressgang".
 * - Professor MacHugh: Auditor (Analytical) - Observing the "divine afflatus".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_aeolus_1904, 0.51). % High due to Bloom's rejected labor.
domain_priors:suppression_score(ulysses_aeolus_1904, 0.75).   % The "nightmare" of mechanical rhythm.
domain_priors:theater_ratio(ulysses_aeolus_1904, 0.85).      % High bombast: "divine afflatus" and headlines.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_aeolus_1904, extractiveness, 0.51).
narrative_ontology:constraint_metric(ulysses_aeolus_1904, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ulysses_aeolus_1904, theater_ratio, 0.85).

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_aeolus_1904, press_institution). % Daily circulation.
narrative_ontology:constraint_victim(ulysses_aeolus_1904, leopold_bloom).          % The rejected canvasser.
domain_priors:requires_active_enforcement(ulysses_aeolus_1904). % The editor's "Begone!" and yelling newsboys.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: LEOPOLD BLOOM (SNARE)
% Effective Extraction: 0.51 * 1.5 (powerless) * 1.0 (national) = 0.765.
% Perceived as a "hurricane" of noise and personal humiliation.
constraint_indexing:constraint_classification(ulysses_aeolus_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE EDITOR (ROPE)
% Effective Extraction: 0.51 * -0.2 (institutional) * 1.0 = -0.102.
% Viewed as the essential "workaday worker tack" of a great daily organ.
constraint_indexing:constraint_classification(ulysses_aeolus_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.85) suggests a Piton of "lost causes" and "cloaquae".
constraint_indexing:constraint_classification(ulysses_aeolus_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_aeolus_1904_tests).

test(perspectival_gap) :-
    % Verify shift from the Snare of rejection to the Rope of institutional function.
    constraint_indexing:constraint_classification(ulysses_aeolus_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_aeolus_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_validation) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_aeolus_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_aeolus_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Leopold Bloom experiences the press as a Tangled Rope—it coordinates the 
 * " Hibernian Metropolis" through its trams and mailcars, yet it extracts 
 * his social sovereignty[cite: 4718, 4722]. The high Theater Ratio (0.85) 
 * is driven by the "mummery" of letters and the "inflated windbag" oratory 
 * of Dan Dawson and John F. Taylor[cite: 4867, 5085]. Crawford’s 
 * "K.M.R.I.A." (Kiss My Royal Irish Arse) is the definitive marker of 
 * asymmetric extraction felt by the powerless agent[cite: 5174].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_aeolian_agency,
    "Is the editor's rejection a Rope of editorial standards or a Snare of personal malice?",
    "Review of the 'Canada swindle' and 'Ontario' references.",
    "Editorial standard confirms a Rope; malice hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_aeolus_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking during the 'Pisgah Sight of Palestine' and Taylor's speech)
narrative_ontology:measurement(ua_tr_t0, ulysses_aeolus_1904, theater_ratio, 0, 0.72).
narrative_ontology:measurement(ua_tr_t5, ulysses_aeolus_1904, theater_ratio, 5, 0.90).
narrative_ontology:measurement(ua_tr_t10, ulysses_aeolus_1904, theater_ratio, 10, 0.85).

% Extraction (Increasing as Bloom is chased by newsboys and rebuffed by Crawford)
narrative_ontology:measurement(ua_ex_t0, ulysses_aeolus_1904, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ua_ex_t5, ulysses_aeolus_1904, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ua_ex_t10, ulysses_aeolus_1904, base_extractiveness, 10, 0.51).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
