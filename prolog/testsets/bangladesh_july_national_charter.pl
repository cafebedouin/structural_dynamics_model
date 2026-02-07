% ============================================================================
% CONSTRAINT STORY: bangladesh_july_national_charter
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_bangladesh_july_national_charter, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Updated for v3.4) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: bangladesh_july_national_charter
 * human_readable: The July National Charter Referendum
 * domain: political
 * SUMMARY:
 * A bundle of 80 constitutional reforms proposed by the Muhammad Yunus-led 
 * interim government, presented as a binary "Yes/No" referendum alongside 
 * the February 12, 2026, general elections. While intended to dismantle 
 * the "two-party" autocracy cycle, the referendum acts as a take-it-or-leave-it 
 * constraint that forces systemic change through a singular electoral event.
 * KEY AGENTS:
 * - Ordinary Voter: Subject (Powerless) - Faces a binary choice on 80 disparate reforms with no line-item veto.
 * - Interim Government/Jamaat-NCP: Beneficiary (Institutional) - Utilize the "clean slate" window to lock in bicameralism and term limits.
 * - Constitutional Auditor: Auditor (Analytical) - Evaluates the legality of a non-elected government resetting the national charter.
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(bangladesh_july_national_charter, 0.55). 
domain_priors:suppression_score(bangladesh_july_national_charter, 0.78).   
domain_priors:theater_ratio(bangladesh_july_national_charter, 0.32). % Primarily functional.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(bangladesh_july_national_charter, extractiveness, 0.55).
narrative_ontology:constraint_metric(bangladesh_july_national_charter, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(bangladesh_july_national_charter, theater_ratio, 0.32).

% Mandatory keys for classification engine v3.4
% [RESOLVED MISSING_SUNSET_CLAUSE]: Required for Scaffold classification.
% The Charter is a temporary scaffold until the 13th Jatiya Sangsad is seated.
narrative_ontology:has_sunset_clause(bangladesh_july_national_charter).

domain_priors:requires_active_enforcement(bangladesh_july_national_charter).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(bangladesh_july_national_charter, interim_government).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, ordinary_voter).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISENFRANCHISED VOTER (SNARE)
% For supporters of the banned Awami League or those wary of the "Yes" campaign, 
% the charter is a Snare that traps the nation's future in an un-vetted document.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE REFORM ALLIANCE (ROPE)
% For the Jamaat-e-Islami and NCP, the Charter is a Rope (coordination) that 
% prevents the return of "neo-fascism" and the BNP/Awami League duopoly.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE HISTORICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid: It coordinates a new democratic structure but extracts 
% political agency from those who do not subscribe to the interim "consensus."
constraint_indexing:constraint_classification(bangladesh_july_national_charter, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(bangladesh_july_national_charter, E), E >= 0.50,
    domain_priors:suppression_score(bangladesh_july_national_charter, S), S > 0.40.

% PERSPECTIVE 4: THE INTERIM GOVERNMENT (SCAFFOLD)
% The referendum is viewed as a temporary Scaffold to support the state 
% during its transition from the 2024 crisis to the 13th Jatiya Sangsad.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bangladesh_july_national_charter_tests).

test(perspectival_gap) :-
    % Verify the Charter is a Snare for the powerless but a Rope for the institutional players.
    constraint_indexing:constraint_classification(bangladesh_july_national_charter, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bangladesh_july_national_charter, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(bangladesh_july_national_charter, E),
    E >= 0.46. % Confirms it is a high-extraction Snare/Tangled signature.

:- end_tests(bangladesh_july_national_charter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The July National Charter exhibits a high suppression score (0.78) due to the 
 * absence of pluralistic alternatives—the electorate is presented with the 
 * "Yunus Draft" or a return to the pre-2024 constitutional void. 
 * The Perspectival Gap arises because the Institutional agents see this as 
 * "Essential Coordination" (Rope) to prevent future autocracy, while the 
 * Subject (the voter) experiences it as a "Forced Bundle" (Snare) where 
 * 80 changes are swallowed as a single pill.
 * * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification (Analytical perspective) is critical. It 
 * acknowledges that while the Charter provides genuine coordination (e.g., 
 * anti-defection provisions, term limits), it extracts "foundational 
 * sovereignty" from the 13th Jatiya Sangsad before it is even seated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_charter_permanence,
    'Will the 13th Jatiya Sangsad (if dominated by the BNP) respect the "anti-unilateral change" provision, or is it a paper-thin Rope that will snap upon the first majority vote?',
    'Post-election legislative analysis of the first 100 days of the new parliament.',
    'If the BNP slow-pedals or ignores the Charter, the Scaffold collapses into a Piton (non-functional constraint).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing: The transition window is 2024-2026.
narrative_ontology:interval(bangladesh_july_national_charter, 2024, 2026). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Modeling the drift from functional post-2024 reform (0.10) 
% toward the high-stakes "New Dawn" rhetoric of the 2026 campaign (0.32).
narrative_ontology:measurement(charter_tr_t0, bangladesh_july_national_charter, theater_ratio, 0, 0.10).
narrative_ontology:measurement(charter_tr_t5, bangladesh_july_national_charter, theater_ratio, 5, 0.24).
narrative_ontology:measurement(charter_tr_t10, bangladesh_july_national_charter, theater_ratio, 10, 0.32).

% Extraction: Tracking how the "Forced Bundle" progressively liquidates 
% foundational sovereignty as the referendum date approaches.
narrative_ontology:measurement(charter_ex_t0, bangladesh_july_national_charter, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(charter_ex_t5, bangladesh_july_national_charter, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(charter_ex_t10, bangladesh_july_national_charter, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
