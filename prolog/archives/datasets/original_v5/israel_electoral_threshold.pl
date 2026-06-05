% ============================================================================
% CONSTRAINT STORY: israel_electoral_threshold
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_israel_electoral_threshold, []).

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
 * constraint_id: israel_electoral_threshold
 * human_readable: The 3.25% Knesset Electoral Threshold
 * domain: political
 * SUMMARY:
 * A statutory requirement that a political party must receive at least 3.25% 
 * of the national vote to be allocated seats in the 120-member Knesset[cite: 12, 564, 639]. 
 * While intended to prevent extreme fragmentation and stabilize coalitions[cite: 639, 640], 
 * it creates a high-stakes barrier where "wasted votes" for parties narrowly 
 * missing the mark can fundamentally shift the balance of power[cite: 232, 235].
 * KEY AGENTS:
 * - Small Minority Parties: Subject (Powerless) - Face binary outcome of representation or total exclusion[cite: 40, 232].
 * - Major Party Blocs: Beneficiary (Institutional) - Benefit from the consolidation of votes and the elimination of small rivals[cite: 13, 232, 236].
 * - Electoral Auditor: Auditor (Analytical) - Evaluates the trade-off between governance stability and representational fidelity[cite: 587, 639].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% High extraction (wasted votes) in the 2022 context, where 289,000 votes for parties 
% like Meretz (3.16%) and Balad (2.91%) were discarded[cite: 202, 235].
domain_priors:base_extractiveness(israel_electoral_threshold, 0.48). 

% Suppression score is high because the law mandates a specific closed-list 
% proportional representation system with no alternative mechanism for 
% small parties to gain seats unless they form alliances[cite: 583, 584, 586].
domain_priors:suppression_score(israel_electoral_threshold, 0.65).   

domain_priors:theater_ratio(israel_electoral_threshold, 0.15).       % Functional, not theatrical.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(israel_electoral_threshold, extractiveness, 0.48).
narrative_ontology:constraint_metric(israel_electoral_threshold, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(israel_electoral_threshold, theater_ratio, 0.15).

% The threshold does not have a sunset clause; it is a permanent part of the law[cite: 564, 639].
% narrative_ontology:has_sunset_clause(israel_electoral_threshold). 

domain_priors:requires_active_enforcement(israel_electoral_threshold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For small parties (powerless) over a biographical/election cycle horizon, 
% the threshold acts as a Snare that captures votes without providing representation[cite: 232, 235].
constraint_indexing:constraint_classification(israel_electoral_threshold, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the governing institutions, it is a Rope (coordination) intended to solve 
% the "collective action problem" of hyper-fragmented, unstable coalitions[cite: 639, 640].
constraint_indexing:constraint_classification(israel_electoral_threshold, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% From a historical view, it is a Tangled Rope: it provides coordination (stability) 
% but does so through the asymmetric extraction of political voice from 
% fringe or minority demographics[cite: 235, 237, 587].
constraint_indexing:constraint_classification(israel_israel_electoral_threshold, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(israel_electoral_threshold, E), E >= 0.46,
    domain_priors:suppression_score(israel_electoral_threshold, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_electoral_threshold_tests).

test(perspectival_gap) :-
    % Verify the threshold is a Snare for small parties but a Rope for the State.
    constraint_indexing:constraint_classification(israel_electoral_threshold, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_electoral_threshold, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(israel_electoral_threshold, E),

    E >= 0.46. % Confirms it is classified as a high-extraction constraint.

:- end_tests(israel_electoral_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 3.25% threshold represents a classic "Perspectival Gap." To the institutional 
 * designer, it is an essential "Rope" for governance—without it, the 24th Knesset's 
 * narrow 61-seat majority might have been even more precarious[cite: 8, 10, 639]. 
 * However, to parties like Meretz or Balad in 2022, it was a "Snare": they received 
 * significant vote shares (3.16% and 2.91%) but were mathematically erased from 
 * the legislature, leading to a "wasted" 49.5% block for the anti-Netanyahu camp 
 * despite near-parity in raw votes[cite: 202, 235, 236].
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is vital here. It prevents the system from 
 * seeing the threshold as *purely* extractive (Snare). The "coordination" 
 * dividend is the reduction of tiny, single-issue parties that can hold 
 * entire governments hostage—a persistent issue in Israeli political history[cite: 566, 629, 639].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_israel_electoral_threshold,
    'Does the 3.25% threshold actually increase coalition stability, or merely increase the frequency of snap elections by raising the stakes of defection?',
    'Comparative analysis of coalition longevity before and after the 2014 threshold hike[cite: 639, 640].',
    'If it fails to stabilize, the Rope classification collapses into a Piton (inertial non-functional constraint).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing: The threshold has been historically 
% adjusted (1% to 1.5% to 2% to 3.25%)[cite: 637, 638, 639].
narrative_ontology:interval(israel_electoral_threshold, 1, 4). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
