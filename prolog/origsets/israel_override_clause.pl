% ============================================================================
% CONSTRAINT STORY: israel_override_clause
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_israel_override_clause, []).

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
 * constraint_id: israel_override_clause
 * human_readable: The 61-Member Judicial Override Clause
 * domain: political
 * SUMMARY:
 * A proposed mechanism to allow the Knesset, by a majority of 61 out of 120 MKs, 
 * to bypass Supreme Court rulings that invalidate legislation for violating 
 * Basic Laws. This effectively shifts the final word on constitutionality from 
 * the judiciary to the ruling coalition.
 * KEY AGENTS:
 * - Minority Groups: Subject (Powerless) - Vulnerable to majoritarian legislation without judicial recourse.
 * - Ruling Coalition: Beneficiary (Institutional) - Gains the ability to implement policies (e.g., draft exemptions, land regularization) previously blocked by the court.
 * - Supreme Court: Auditor (Analytical) - Historically the sole balancing mechanism in the Israeli system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% [RESOLVED MANDATROPHY]
% Extraction is high (0.75) because it extracts the protected status of 
% constitutional rights (equality, dignity) and reclaims them as political 
% assets for the majority.
domain_priors:base_extractiveness(israel_override_clause, 0.75). 

% Suppression is very high (0.85). The clause is designed specifically to 
% suppress the "alternative" of judicial review for specific laws.
domain_priors:suppression_score(israel_override_clause, 0.85).   

% Theater ratio is significant (0.35) as the proposal is frequently framed 
% in the theatrical language of "pure democracy" and "sovereignty".
domain_priors:theater_ratio(israel_override_clause, 0.35).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(israel_override_clause, extractiveness, 0.75).
narrative_ontology:constraint_metric(israel_override_clause, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(israel_override_clause, theater_ratio, 0.35).

domain_priors:requires_active_enforcement(israel_override_clause).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VULNERABLE MINORITY (SNARE)
% For groups relying on judicial protection (e.g., women in public spaces, 
% Arab citizens, secular draft advocates), the clause is a Snare that 
% traps them under the absolute will of a 61-seat majority.
constraint_indexing:constraint_classification(israel_override_clause, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE COALITION GOVERNMENT (ROPE)
% Viewed by proponents as a Rope (coordination) that allows the government 
% to fulfill its democratic mandate and solve governance paralysis caused 
% by "judicial overreach".
constraint_indexing:constraint_classification(israel_override_clause, rope, 
    context(agent_power(institutional), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE HISTORICAL ANALYST (TANGLED ROPE)
% Viewed as a Tangled Rope: it provides coordination for the ruling bloc 
% but does so by extracting the fundamental "checks and balances" required 
% for long-term democratic stability.
constraint_indexing:constraint_classification(israel_override_clause, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(israel_override_clause, E), E > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_override_clause_tests).

test(perspectival_gap) :-
    % Verify it is a Rope for institutions but a Snare for the powerless.
    constraint_indexing:constraint_classification(israel_override_clause, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(israel_override_clause, snare, context(agent_power(powerless), _, _, _)).

test(mandatrophy_check) :-
    domain_priors:base_extractiveness(israel_override_clause, E),

    E > 0.7. % Confirms the high-extraction status requiring resolution.

:- end_tests(israel_override_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Override Clause represents the ultimate "Perspectival Gap." The 
 * institutional logic (Rope) argues that a 61-seat majority *is* the 
 * democratic coordinate. However, because Israel lacks a formal written 
 * constitution, a second house, or regional representation, the analytical 
 * view (Tangled Rope) sees this coordination as inextricably linked to 
 * the extraction of minority protections.
 * * * MANDATROPHY ANALYSIS:
 * The high extractiveness (0.75) is resolved by the "Tangled Rope" 
 * classification, which acknowledges that the "coordination" is not 
 * just for efficiency, but for the specific extraction of judicial veto 
 * power to enable previously "illegal" policies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_override_majority,
    'Would a higher majority (e.g., 70 or 80 MKs) convert the clause from a Snare into a genuine Rope of broad consensus?',
    'Comparative analysis of legislative consensus levels in countries with similar override mechanisms (e.g., Canada).',
    'High majority requirements might preserve the "Rope" (coordination) without the "Snare" (predatory extraction).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% The intensity of this constraint reached its peak in late 2022/early 2023.
narrative_ontology:interval(israel_override_clause, 2022, 2024). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
