% ============================================================================
% CONSTRAINT STORY: ulysses_tower_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_tower, []).

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
 * * constraint_id: ulysses_tower_1904
 * human_readable: The Martello Tower Usurpation
 * domain: social/political/religious
 * * SUMMARY:
 * The Martello tower, originally built as a defensive Rope against French 
 * invasion, has become a Snare for Stephen Dedalus. Despite paying the 
 * twelve quid rent to the British state, Stephen is socially and 
 * psychologically "usurped" by Buck Mulligan's performative dominance and 
 * Haines's colonial presence.
 * * KEY AGENTS:
 * - Stephen Dedalus: Subject (Powerless) - The "server of a servant".
 * - Buck Mulligan: Beneficiary (Institutional) - Controlling the social theater.
 * - Haines: Auditor (Analytical) - The English presence collecting "sayings".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_tower_1904, 0.48). 
domain_priors:suppression_score(ulysses_tower_1904, 0.65).   
domain_priors:theater_ratio(ulysses_tower_1904, 0.82). % Mulligan's high mockery

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_tower_1904, extractiveness, 0.48).
narrative_ontology:constraint_metric(ulysses_tower_1904, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ulysses_tower_1904, theater_ratio, 0.82).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_tower_1904, piton).
narrative_ontology:human_readable(ulysses_tower_1904, "The Martello Tower Usurpation").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_tower_1904, colonial_subjects).
narrative_ontology:constraint_victim(ulysses_tower_1904, stephen_dedalus).
domain_priors:requires_active_enforcement(ulysses_tower_1904).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE)
% Effective Extraction: 0.48 * 1.5 (powerless) * 0.8 (local) = 0.576.
% Perceived as a predatory trap of "Agenbite of inwit" and unpaid debts.
constraint_indexing:constraint_classification(ulysses_tower_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: BUCK MULLIGAN (ROPE)
% Effective Extraction: 0.48 * -0.2 (institutional) * 0.8 = -0.0768.
% Viewed as a blithe coordination for breakfast, swimming, and "Hellenizing".
constraint_indexing:constraint_classification(ulysses_tower_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.82) indicates a Piton of atrophied function.
constraint_indexing:constraint_classification(ulysses_tower_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_tower_1904_tests).

test(perspectival_gap) :-
    % Verify the shift from Snare (Stephen) to Rope (Mulligan).
    constraint_indexing:constraint_classification(ulysses_tower_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_tower_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_tower_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_tower_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Stephen perceives the tower as a Snare because his payment of the twelve 
 * quid rent to the British state does not grant him social sovereignty. 
 * Mulligan's constant mockery (TR: 0.82) transforms the living arrangement 
 * into a Piton for analytical observers—it is a structure of atrophied 
 * defense now serving as a stage for "mummers".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_usurpation,
    "Will Stephen successfully withhold the key and the rent?",
    "Review of the final encounter at the foot of the ladder.",
    "Withholding confirms a Rope; surrender hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_tower_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Mulligan's performative blasphemy peaking during breakfast)
narrative_ontology:measurement(ut_tr_t0, ulysses_tower_1904, theater_ratio, 0, 0.70).
narrative_ontology:measurement(ut_tr_t5, ulysses_tower_1904, theater_ratio, 5, 0.85).
narrative_ontology:measurement(ut_tr_t10, ulysses_tower_1904, theater_ratio, 10, 0.82).

% Extraction (Rent payments and social debts accumulating)
narrative_ontology:measurement(ut_ex_t0, ulysses_tower_1904, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ut_ex_t5, ulysses_tower_1904, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ut_ex_t10, ulysses_tower_1904, base_extractiveness, 10, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
