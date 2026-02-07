% ============================================================================
% CONSTRAINT STORY: complexity_debt
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(complexity_debt, []).

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
 * * constraint_id: complexity_debt
 * human_readable: The Cumulative Fragility Surcharge
 * domain: technological/organizational/economic
 * * SUMMARY:
 * This constraint represents the accrued cost of "quick-fix" solutions and 
 * layered abstractions within a system. Over time, the maintenance of this 
 * historical complexity consumes a majority of the system's energy, siphoning 
 * resources away from innovation and toward the preservation of an 
 * increasingly brittle status quo. It functions as a Snare for the current 
 * maintainer while having served as a Rope (scaffold) for the original builder.
 * * KEY AGENTS:
 * - Maintenance Engineer: Subject (Powerless)
 * - Legacy System Owner: Beneficiary (Institutional)
 * - Systems Archeologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the "interest" on complexity debt liquidates 
% the system's adaptive capacity, forcing participants to pay a perpetual 
% tax in labor and cognitive load.
domain_priors:base_extractiveness(complexity_debt, 0.84). 
domain_priors:suppression_score(complexity_debt, 0.76). % Alternatives are blocked by the cost of re-architecture.
domain_priors:theater_ratio(complexity_debt, 0.45).    % Moderate theater; legacy stability is often used as a justification.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(complexity_debt, extractiveness, 0.84).
narrative_ontology:constraint_metric(complexity_debt, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(complexity_debt, theater_ratio, 0.45).

% This is an entropic property of systems, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(complexity_debt). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the maintainer, the debt is a snare: they cannot move without triggering 
% a regression in a legacy component they don't fully understand.
constraint_indexing:constraint_classification(complexity_debt, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the legacy stack as a Rope—it is the coordination 
% substrate that currently generates all revenue, despite its fragility.
constraint_indexing:constraint_classification(complexity_debt, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of essential coordination (Ropes) 
% entangled with massive, deferred extraction (Snare).
constraint_indexing:constraint_classification(complexity_debt, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(complexity_debt, E), E >= 0.50,
    domain_priors:suppression_score(complexity_debt, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complexity_debt_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional owner.
    constraint_indexing:constraint_classification(complexity_debt, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complexity_debt, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(complexity_debt, E),

    E > 0.70.

:- end_tests(complexity_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "interest" on technical debt siphons all productive agency from the subject.
 * 
 * * PERSPECTIVAL GAP:
 * The Maintenance Engineer feels a Snare because they are blamed for 
 * "slowness" that is actually a physical consequence of the debt. The 
 * System Owner sees a Rope because the legacy code allows for continued 
 * coordination and market presence without the immediate cost of a rewrite.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This identifies that the 
 * system's coordination is currently subsidized by the "tangled" extraction 
 * of the maintainer's cognitive and temporal resources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_complexity_insolvency,
    'At what point does the cost of maintenance exceed the system utility (Mountain or Snare)?',
    'Tracking the ratio of bug-fix hours to feature-development hours.',
    'If utility < cost: Mountain of Ruin. If utility > cost: Snare of Rent-Seeking.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(complexity_debt, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
