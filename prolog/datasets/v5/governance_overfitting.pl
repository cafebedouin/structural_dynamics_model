% ============================================================================
% CONSTRAINT STORY: governance_overfitting
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(governance_overfitting, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: governance_overfitting
 * human_readable: Hyper-Specific Compliance Lock-in
 * domain: political/technological
 * * SUMMARY:
 * This constraint occurs when a governance framework is so precisely tuned to 
 * past edge cases that it becomes a rigid barrier to novel coordination. 
 * It functions as a 'Mountain' of red tape for innovators while acting as 
 * a protective 'Rope' for entrenched incumbents.
 * * KEY AGENTS:
 * - Startup Founder: Subject (Powerless)
 * - Industry Incumbent: Beneficiary (Institutional)
 * - Regulatory Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.74) because the "protection" is actually asymmetric rent.
domain_priors:base_extractiveness(governance_overfitting, 0.74). 
domain_priors:suppression_score(governance_overfitting, 0.68).
domain_priors:theater_ratio(governance_overfitting, 0.45).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(governance_overfitting, extractiveness, 0.74).
narrative_ontology:constraint_metric(governance_overfitting, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(governance_overfitting, theater_ratio, 0.45).

% Overfitting is often permanent unless a sunset clause is forced.
% narrative_ontology:has_sunset_clause(governance_overfitting). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the innovator, the hyper-specific rules are a predatory snare designed to stop growth.
constraint_indexing:constraint_classification(governance_overfitting, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Incumbents view the overfitting as a necessary 'Rope' that ensures safety and standards.
constraint_indexing:constraint_classification(governance_overfitting, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction/suppression triggers the hybrid 'Tangled Rope' classification.
constraint_indexing:constraint_classification(governance_overfitting, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(governance_overfitting, E), E >= 0.50,
    domain_priors:suppression_score(governance_overfitting, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance_overfitting_tests).

test(perspectival_gap) :-
    % Verify Snare for powerless vs Rope for institutional.
    constraint_indexing:constraint_classification(governance_overfitting, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance_overfitting, rope, 
        context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction meets the linter's v3.4 core limits.
    domain_priors:base_extractiveness(governance_overfitting, E),

    (E =< 0.05 ; E >= 0.46).

:- end_tests(governance_overfitting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.74) reflects the 'Mandatrophy' threshold where 
 * the regulatory cost creates more drag than the stability provides value.
 * * PERSPECTIVAL GAP:
 * The innovator feels a Snare because they cannot bypass the rules; the 
 * incumbent feels a Rope because they helped write the rules to suit their 
 * existing infrastructure.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the 'Tangled Rope' observer perspective. By identifying 
 * both the genuine coordination (safety) and the asymmetric extraction 
 * (incumbency protection), we prevent misclassification as a simple Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_regulatory_capture,
    'Is the overfitting a result of genuine edge-case safety or deliberate capture?',
    'Comparative analysis of compliance costs vs. lobbying expenditures.',
    'If capture-linked: Snare. If safety-linked: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(governance_overfitting, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
