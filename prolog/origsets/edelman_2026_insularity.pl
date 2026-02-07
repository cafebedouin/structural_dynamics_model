% ============================================================================
% CONSTRAINT STORY: edelman_2026_insularity
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_edelman_2026_insularity, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: edelman_2026_insularity
 * human_readable: The Insular Trust Mindset
 * domain: social/economic
 * SUMMARY:
 * A global condition where 70% of individuals are unwilling or hesitant to trust 
 * those with different values, sources, or backgrounds. This insularity 
 * acts as a "Snare" for the powerless by limiting economic mobility and 
 * as a "Rope" for institutions attempting to broker local stability.
 * KEY AGENTS:
 * - Low-Income Citizen: Subject (Powerless) - Faces a 15pt trust gap vs high income[cite: 263].
 * - Multi-national Employer: Beneficiary (Institutional) - Seen as the most trusted 
 * entity to bridge divides[cite: 826, 972].
 * - Edelman Analyst: Auditor (Analytical) - Measures the erosion of shared reality[cite: 72].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.55) reflecting lost productivity (34%) and workplace conflict[cite: 624, 630].
domain_priors:base_extractiveness(edelman_2026_insularity, 0.55). 
% High suppression (0.65) as 71% of low-income fear being left behind by AI[cite: 408, 434].
domain_priors:suppression_score(edelman_2026_insularity, 0.65).   
% Theater ratio (0.42) reflecting significant but not yet dominant performative "Trust Brokering"[cite: 835].
domain_priors:theater_ratio(edelman_2026_insularity, 0.42).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(edelman_2026_insularity, extractiveness, 0.55).
narrative_ontology:constraint_metric(edelman_2026_insularity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(edelman_2026_insularity, theater_ratio, 0.42).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The low-income subject feels trapped by a system they believe favors the rich[cite: 667, 1385].
constraint_indexing:constraint_classification(edelman_2026_insularity, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Employers view the constraint as a coordination challenge to be managed via 
% "Trust Brokering" and shared identity[cite: 778, 890].
constraint_indexing:constraint_classification(edelman_2026_insularity, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature of insularity: it provides local safety but extracts 
% global progress[cite: 584, 595].
constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(edelman_2026_insularity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(edelman_2026_insularity, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(edelman_2026_insularity, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(edelman_2026_insularity, E),

    E >= 0.46.

:- end_tests(edelman_2026_insularity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.55 extraction score is grounded in the "Loss of Productivity" metric (34%) 
 * where employees put less effort into teams led by those with different beliefs[cite: 624, 630]. 
 * The Perspectival Gap exists because institutions see insularity as a problem 
 * to be solved through "polynational" models[cite: 965], while the powerless 
 * experience it as a "rigged system"[cite: 132].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_edelman_2026,
    'Is insularity a psychological defense (Mountain) or a manufactured state (Snare)?',
    'Analysis of the 11pt rise in fears of foreign disinformation[cite: 465, 478].',
    'If defense, trust brokering succeeds; if manufactured, insularity is an extraction tool.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(edelman_2026_insularity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time: Rising as "Trust Brokering" becomes a corporate mandate[cite: 826].
narrative_ontology:measurement(ed_tr_t0, edelman_2026_insularity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ed_tr_t5, edelman_2026_insularity, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ed_tr_t10, edelman_2026_insularity, theater_ratio, 10, 0.42).

% Extraction over time: Increasing as shared reality erodes over 20 years[cite: 72].
narrative_ontology:measurement(ed_ex_t0, edelman_2026_insularity, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ed_ex_t5, edelman_2026_insularity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ed_ex_t10, edelman_2026_insularity, base_extractiveness, 10, 0.55).

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================
