% ============================================================================
% CONSTRAINT STORY: isa_education_scaffold
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_isa_scaffold, []).

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
 * * constraint_id: isa_education_scaffold
 * human_readable: Income Share Agreement (ISA) Funding
 * domain: economic/educational
 * * SUMMARY:
 * An ISA allows students to access education with zero upfront cost in 
 * exchange for a fixed percentage of future earnings. It acts as a 
 * Scaffold (temporary support) that enables upward mobility but creates 
 * a localized Snare (income extraction) during the repayment window.
 * * KEY AGENTS:
 * - The Student: Subject (Powerless). Capital-constrained but high potential.
 * - The ISA Provider: Beneficiary (Institutional). Underwrites the risk for 
 * a share of the upside.
 * - The Regulator: Auditor (Analytical). Ensures the Sunset Clause is robust.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.42) is moderate. While it takes a 
% portion of income, it only does so if the student succeeds (alignment), 
% distinguishing it from fixed-debt "Snares."
domain_priors:base_extractiveness(isa_education_scaffold, 0.42). 
domain_priors:suppression_score(isa_education_scaffold, 0.45).   % Moderate: Traditional loans or self-funding are alternatives.
domain_priors:theater_ratio(isa_education_scaffold, 0.20).      % Low: The contract is functional and direct.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(isa_education_scaffold, extractiveness, 0.42).
narrative_ontology:constraint_metric(isa_education_scaffold, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(isa_education_scaffold, theater_ratio, 0.2).
domain_priors:requires_active_enforcement(isa_education_scaffold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REPAYING GRADUATE (SNARE)
% During the 5-10 year repayment window, the student feels the 15% income 
% hit as a Snare—a trap that limits their "Biographical" mobility and savings.
constraint_indexing:constraint_classification(isa_education_scaffold, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE UNIVERSITY/FUNDER (ROPE)
% To the institution, this is a Rope. It coordinates the talent of the 
% student with the capital of the funder, diversifying risk across a cohort.
constraint_indexing:constraint_classification(isa_education_scaffold, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE POLICY ANALYST (SCAFFOLD)
% Because the constraint has a defined end-date and total-payment cap, 
% it is classified as a Scaffold—a temporary bridge to a higher state.
constraint_indexing:constraint_classification(isa_education_scaffold, scaffold, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(isa_education_scaffold).

% PERSPECTIVE 4: THE MARKET OBSERVER (TANGLED ROPE)
% If the "Sunset Clause" is ambiguous or the "Effective Extraction" exceeds 
% the value of the education, the system is viewed as a Tangled Rope.
constraint_indexing:constraint_classification(isa_education_scaffold, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(isa_education_scaffold, E), E > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(isa_scaffold_tests).

test(sunset_logic_validation) :-
    % Ensure the constraint is only a Scaffold if a sunset clause is present.
    narrative_ontology:has_sunset_clause(isa_education_scaffold),
    constraint_indexing:constraint_classification(isa_education_scaffold, scaffold, _).

test(perspectival_gap) :-
    % Verify the Snare/Rope tension.
    constraint_indexing:constraint_classification(isa_education_scaffold, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(isa_education_scaffold, rope, context(agent_power(institutional), _, _, _)).

:- end_tests(isa_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The ISA is the archetype of the Scaffold. Its "Extraction" (0.42) is 
 * high enough to be felt as a Snare, but its "Coordination" function 
 * (enabling education for the powerless) is its primary justification. 
 * Without the Sunset Clause ($has\_sunset\_clause$), the ISA would 
 * degrade into permanent indentured servitude (a Snare).
 *
 * MANDATROPHY ANALYSIS:
 * Mandatrophy in ISAs occurs when the "Support" (the education) ceases 
 * to provide value, but the "Extraction" (the income share) remains 
 * active. By enforcing the Scaffold classification, we ensure the 
 * system monitors the expiration of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_upside_extraction_limit,
    'At what level of income does 15% extraction become a net-negative for social mobility?',
    'Comparative analysis of high-earner consumption patterns with vs without ISA debt.',
    'If extraction > mobility gain, the Scaffold is actually a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for Scaffold classification
narrative_ontology:has_sunset_clause(isa_education_scaffold). 
narrative_ontology:interval(isa_education_scaffold, 0, 10). % Years until expiry.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
