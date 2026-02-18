% ============================================================================
% CONSTRAINT STORY: ulysses_school_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_school, []).

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
 * * constraint_id: ulysses_school_1904
 * human_readable: The Nightmare of History (Dalkey School)
 * domain: economic/social/political
 * * SUMMARY:
 * For Stephen Dedalus, the school at Dalkey is a Snare defined by the 
 * "nightmare of history" and the futility of teaching privileged boys who 
 * lack innocence[cite: 377, 555]. The educational structure is a Tangled 
 * Rope for Mr. Deasy, who uses it to enforce a "generous but just" financial 
 * discipline while maintaining a high theater of anti-Semitic and 
 * pro-Unionist rhetoric[cite: 492, 537].
 * * KEY AGENTS:
 * - Stephen Dedalus: Subject (Powerless) - Trapped in "three nooses" of debt and role[cite: 475, 488].
 * - Mr. Deasy: Beneficiary (Institutional) - Paying his way and managing the "Strongroom"[cite: 464, 486].
 * - The Students: Auditor (Analytical) - Observing Stephen's "lack of rule"[cite: 374].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_school_1904, 0.49). % High due to Stephen's list of debts[cite: 488].
domain_priors:suppression_score(ulysses_school_1904, 0.72).   % History as a "branded" and "fettered" room.
domain_priors:theater_ratio(ulysses_school_1904, 0.78).      % Deasy's performative "wisdom" and savings-box prodding[cite: 467, 512].

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_school_1904, extractiveness, 0.49).
narrative_ontology:constraint_metric(ulysses_school_1904, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ulysses_school_1904, theater_ratio, 0.78).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_school_1904, piton).
narrative_ontology:human_readable(ulysses_school_1904, "The Nightmare of History (Dalkey School)").
narrative_ontology:topic_domain(ulysses_school_1904, "economic/social/political").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_school_1904, english_empire). % History moving toward a manifest goal[cite: 558].
narrative_ontology:constraint_victim(ulysses_school_1904, stephen_dedalus).
domain_priors:requires_active_enforcement(ulysses_school_1904). % Deasy's prodding and strict "rules"[cite: 374, 512].

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE)
% Effective Extraction: 0.49 * 1.5 (powerless) * 1.0 (national) = 0.735.
% Perceived as a nightmare from which he is trying to awake.
constraint_indexing:constraint_classification(ulysses_school_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: MR. DEASY (ROPE)
% Effective Extraction: 0.49 * -0.2 (institutional) * 1.0 = -0.098.
% Viewed as the essential coordination of "paying one's way" and justice[cite: 486, 492].
constraint_indexing:constraint_classification(ulysses_school_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.78) suggests the atrophied function of "toothless terrors"[cite: 584].
constraint_indexing:constraint_classification(ulysses_school_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_school_1904_tests).

test(perspectival_gap) :-
    % Verify the shift from Snare (Stephen) to Rope (Deasy).
    constraint_indexing:constraint_classification(ulysses_school_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_school_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_validation) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_school_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_school_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Stephen identifies the "three nooses" of his financial and social reality 
 * as a Snare—a predatory trap of accumulated debt and listless labor[cite: 475, 488]. 
 * Conversely, Mr. Deasy views the system as a Rope of coordination, 
 * maintaining the "proudest boast" of the Englishman: "I paid my way"[cite: 486]. 
 * The high Theater Ratio (0.78) comes from the "mummery" of Deasy’s symbols—
 * the savings-box, the Stuart coins, and his insistence on anti-Semitic 
 * conspiracy theories that mask the "nightmare" of Irish history[cite: 435, 457, 537].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_history_as_nightmare,
    "Is history truly a trapped room of ousted possibilities, or a joust moving toward God? [cite: 386, 558]",
    "Tracking the manifestation of the 'great goal' vs the 'shout in the street'[cite: 558, 560].",
    "God-goal confirms a Rope; street-shout hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_school_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Deasy's rhetorical intensity rising toward the anti-Semitic climax)
narrative_ontology:measurement(us_tr_t0, ulysses_school_1904, theater_ratio, 0, 0.65).
narrative_ontology:measurement(us_tr_t5, ulysses_school_1904, theater_ratio, 5, 0.75).
narrative_ontology:measurement(us_tr_t10, ulysses_school_1904, theater_ratio, 10, 0.78).

% Extraction (Accumulation of Stephen's biographical and spiritual debt [cite: 488])
narrative_ontology:measurement(us_ex_t0, ulysses_school_1904, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(us_ex_t5, ulysses_school_1904, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(us_ex_t10, ulysses_school_1904, base_extractiveness, 10, 0.49).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
