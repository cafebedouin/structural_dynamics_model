% ============================================================================
% CONSTRAINT STORY: cognitive_energy_budget
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(cognitive_energy_budget, []).

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
 * * constraint_id: cognitive_energy_budget
 * human_readable: The Attention Exhaustion Trap
 * domain: cognitive/technological/social
 * * SUMMARY:
 * This constraint represents the finite biological limit of an individual's 
 * daily cognitive energy. In a hyper-mediated environment, institutional 
 * systems compete to "tax" this budget through notification loops and dark 
 * patterns. This creates a Snare for the individual, whose ability to 
 * exercise agency declines as the budget is depleted, while serving as a 
 * Rope for platforms seeking predictable user engagement.
 * * KEY AGENTS:
 * - Information Worker: Subject (Powerless)
 * - Attention Economy Platform: Beneficiary (Institutional)
 * - Neuro-Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.81) as the platform siphons the subject's primary 
% cognitive resource, leaving insufficient energy for autonomous exit.
domain_priors:base_extractiveness(cognitive_energy_budget, 0.81). 
domain_priors:suppression_score(cognitive_energy_budget, 0.68). 
domain_priors:theater_ratio(cognitive_energy_budget, 0.45). % Moderate theater: notifications presented as "helpful."

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cognitive_energy_budget, extractiveness, 0.81).
narrative_ontology:constraint_metric(cognitive_energy_budget, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cognitive_energy_budget, theater_ratio, 0.45).

% This is an irreducible biological limit, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(cognitive_energy_budget). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The worker is trapped: the more energy they spend on the system, 
% the less they have to plan or execute an exit strategy.
constraint_indexing:constraint_classification(cognitive_energy_budget, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the budget management as a Rope—a way to coordinate 
% user behavior and ensure high-fidelity data streams.
constraint_indexing:constraint_classification(cognitive_energy_budget, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a neuro-biological perspective, the finite nature of ATP 
% in the prefrontal cortex is an immutable Mountain.
constraint_indexing:constraint_classification(cognitive_energy_budget, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% High extraction (0.81) and suppression (0.68) trigger the hybrid Tangled Rope signature.
constraint_indexing:constraint_classification(cognitive_energy_budget, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(cognitive_energy_budget, E), E >= 0.50,
    domain_priors:suppression_score(cognitive_energy_budget, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_energy_budget_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless worker vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(cognitive_energy_budget, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_energy_budget, rope, 
        context(agent_power(institutional), _, _, _)).

test(mountain_physics) :-
    % Verify that analytical observers correctly identify the biological limit as a Mountain.
    constraint_indexing:constraint_classification(cognitive_energy_budget, mountain, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(cognitive_energy_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.81) reflects a "Mandatrophy" state where the 
 * subject's executive function is systematically depleted by external design.
 * 
 * * PERSPECTIVAL GAP:
 * The Information Worker feels a Snare because their exhaustion is 
 * weaponized against their own agency. The Platform sees a Rope 
 * because user "engagement" is the primary coordination metric for their market.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Tangled Rope classification. This recognizes that while 
 * the platform provides coordination (social connectivity, news), the 
 * high extraction score flags the predatory nature of "attention mining."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_cognitive_augmentation,
    'Can exogenous chemical or digital augmentation expand the budget (Mountain or Snare)?',
    'Long-term study of "nootropic" users vs control in high-frequency notification environments.',
    'If budget expands: Snare (policy lockout). If budget crashes: Permanent Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(cognitive_energy_budget, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
