% ============================================================================
% CONSTRAINT STORY: israel_surplus_vote_agreements
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_israel_surplus_vote_agreements, []).

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
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: israel_surplus_vote_agreements
 * human_readable: Surplus-Vote Agreements (Bader-Ofer Method)
 * domain: political
 * SUMMARY:
 * A system where two parties sign a pre-election agreement to be treated as a 
 * single list for the purpose of allocating "surplus" or leftover seats[cite: 62, 588]. 
 * While it prevents vote wastage, the mathematical implementation (Bader-Ofer) 
 * slightly favors larger lists, often resulting in an "extraction" of value 
 * from the smaller partner to the larger one within the alliance[cite: 63, 585].
 * KEY AGENTS:
 * - Small Partner Party: Subject (Powerless) - Often provides the votes that secure the extra seat but doesn't receive it.
 * - Large Partner Party: Beneficiary (Institutional) - Statistically more likely to receive the allocated leftover seat[cite: 63].
 * - Central Elections Committee: Auditor (Analytical) - Manages the complex calculations to ensure legal adherence[cite: 198, 604].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Moderate extraction: The Bader-Ofer method favors larger lists, meaning surplus 
% votes from a smaller party often "graduate" to seat a candidate for the larger party[cite: 63].
domain_priors:base_extractiveness(israel_surplus_vote_agreements, 0.47). 

% Moderate suppression: Parties are not forced to sign these, but the "waste" 
% alternative (losing surplus votes) makes it a semi-coercive strategic necessity.
domain_priors:suppression_score(israel_surplus_vote_agreements, 0.42).   

domain_priors:theater_ratio(israel_surplus_vote_agreements, 0.10). % Highly functional mathematical logic.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, extractiveness, 0.47).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, theater_ratio, 0.1).

% No sunset clause; these agreements are a standard feature of the electoral law[cite: 588].
% narrative_ontology:has_sunset_clause(israel_surplus_vote_agreements).

domain_priors:requires_active_enforcement(israel_surplus_vote_agreements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a smaller party that provides 0.9 seats' worth of surplus only for the 
% larger partner to "capture" the seat, this feels like a predatory Snare.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the larger party and the overall bloc, it is a Rope (coordination) 
% that ensures no votes are "wasted" in the fight for a majority[cite: 235, 402].
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, rope, 
    context(agent_power(institutional), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: it coordinate blocs to reach 61 seats but 
% extracts small-party influence to empower large-party stability.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(israel_surplus_vote_agreements, E), E >= 0.46,
    domain_priors:suppression_score(israel_surplus_vote_agreements, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_surplus_vote_agreements_tests).

test(perspectival_gap) :-
    % Verify it acts as a coordination tool for institutions but a snare for the small partner.
    constraint_indexing:constraint_classification(israel_surplus_vote_agreements, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_surplus_vote_agreements, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(israel_surplus_vote_agreements, E),

    E >= 0.46. % Correctly identifies high extraction of the Bader-Ofer bias.

:- end_tests(israel_surplus_vote_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Bader-Ofer method is a "Tangled Rope" because its coordination function 
 * (bloc consolidation) is inextricably linked to its extractive function 
 * (favoring larger lists) [cite: 63, 64]. In 2022, the failure of Arab parties 
 * to sign these deals was cited as a key factor giving the Netanyahu bloc 
 * a mathematical edge[cite: 402]. This demonstrates that the "coordination" 
 * is not just mathematical, but deeply tactical.
 * * MANDATROPHY ANALYSIS:
 * By classifying this as a Tangled Rope, we acknowledge that while it 
 * "extracts" seats from the small to the large, it prevents the "Snare" of 
 * total vote wastage that occurs when parties run in total isolation[cite: 235, 384].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_bader_ofer_bias,
    'At what specific seat-count delta does the "benefit" of the alliance for a small party turn into a net loss of representational power?',
    'Stochastic modeling of surplus seat distribution across multiple election cycles.',
    'Determines if the constraint is a Scaffold (helping small parties survive) or a Snare (cannibalizing them).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(israel_surplus_vote_agreements, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
