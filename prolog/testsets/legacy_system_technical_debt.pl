% ============================================================================
% CONSTRAINT STORY: legacy_system_technical_debt
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2026-02-08
% ============================================================================

:- module(constraint_legacy_system_technical_debt, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: legacy_system_technical_debt
 * human_readable: Cumulative Technical Debt in Legacy Monoliths
 * domain: technological/economic
 * * SUMMARY:
 * Technical debt is the implied cost of future refactoring caused by choosing an 
 * easy, limited solution now instead of a better approach. In legacy systems, 
 * this debt ossifies into a constraint that limits innovation. What begins as 
 * a strategic shortcut (Rope) becomes an unmovable reality (Mountain) for 
 * new hires, and eventually an inertial burden (Piton) that consumes 
 * resources via performative maintenance.
 * * KEY AGENTS:
 * - Junior Developer: Subject (Powerless), navigating a brittle codebase.
 * - CTO/VPE: Beneficiary (Institutional), balancing velocity vs. stability.
 * - System Architect/Auditor: Auditor (Analytical), identifying structural decay.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Base extraction is set low (0.03) as the cost is opportunity loss, not direct theft.
domain_priors:base_extractiveness(legacy_system_technical_debt, 0.03). 
domain_priors:suppression_score(legacy_system_technical_debt, 0.04).   
domain_priors:theater_ratio(legacy_system_technical_debt, 0.85).       

% Primary keys for the classification engine
narrative_ontology:constraint_metric(legacy_system_technical_debt, extractiveness, 0.03).
narrative_ontology:constraint_metric(legacy_system_technical_debt, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(legacy_system_technical_debt, theater_ratio, 0.85).

% Constraint self-claim
narrative_ontology:constraint_claim(legacy_system_technical_debt, piton).

% Coordination Type: Technical debt is a failed resource allocation strategy.
narrative_ontology:coordination_type(legacy_system_technical_debt, resource_allocation).

% Technical debt does not require "active enforcement" in the coercive sense;
% it is an emergent property of the system's state.
narrative_ontology:requires_active_enforcement(legacy_system_technical_debt) :- fail.

% Structural property derivation hooks
narrative_ontology:constraint_beneficiary(legacy_system_technical_debt, short_term_profit_margins).
narrative_ontology:constraint_victim(legacy_system_technical_debt, engineering_morale).
narrative_ontology:constraint_victim(legacy_system_technical_debt, long_term_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE JUNIOR DEVELOPER (MOUNTAIN)
% χ = 0.03 * 1.5 (powerless) * 0.8 (local) = 0.036 (<= 0.05) -> Mountain.
constraint_indexing:constraint_classification(legacy_system_technical_debt, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CTO (ROPE)
% Viewed as a manageable coordination tool for scaling.
constraint_indexing:constraint_classification(legacy_system_technical_debt, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Classified as Piton due to high theater_ratio (0.85).
constraint_indexing:constraint_classification(legacy_system_technical_debt, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_system_technical_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legacy_system_technical_debt, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_system_technical_debt, rope, context(agent_power(institutional), _, _, _)),
    !.

test(piton_signature) :-
    narrative_ontology:constraint_metric(legacy_system_technical_debt, theater_ratio, TR),
    TR >= 0.70.

test(mountain_threshold) :-
    domain_priors:base_extractiveness(legacy_system_technical_debt, E),
    E =< 0.05.

:- end_tests(legacy_system_technical_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.03 base extraction reflects that technical debt doesn't "steal" value, 
 * but instead creates a "Mountain" of friction (0.036 effective extraction 
 * for juniors). The high Theater Ratio (0.85) is the primary diagnostic for 
 * a Piton, where maintenance is performative (endless refactoring meetings) 
 * without addressing the core rot.
 *
 * * MANDATROPHY ANALYSIS:
 * The Piton classification prevents the system from mislabeling the lack of 
 * progress as a Snare (malicious intent). It correctly identifies the 
 * constraint as an inertial failure of a formerly useful Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_legacy_system_technical_debt,
    'Is the monolith coupling so deep it requires a total rewrite?',
    'Dependency graph analysis and cyclomatic complexity metrics.',
    'Total rewrite necessity converts the Mountain into a Snare (high exit cost).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_system_technical_debt, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Drift from low-theater Rope to high-theater Piton.
narrative_ontology:measurement(legacy_system_debt_tr_t0, legacy_system_technical_debt, theater_ratio, 0, 0.10).
narrative_ontology:measurement(legacy_system_debt_tr_t5, legacy_system_technical_debt, theater_ratio, 5, 0.50).
narrative_ontology:measurement(legacy_system_debt_tr_t10, legacy_system_technical_debt, theater_ratio, 10, 0.85).

narrative_ontology:measurement(legacy_system_debt_ex_t0, legacy_system_technical_debt, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(legacy_system_debt_ex_t5, legacy_system_technical_debt, base_extractiveness, 5, 0.03).
narrative_ontology:measurement(legacy_system_debt_ex_t10, legacy_system_technical_debt, base_extractiveness, 10, 0.03).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
