% ============================================================================
% CONSTRAINT STORY: epistemic_free_rider_problem
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_free_rider_problem, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: epistemic_free_rider_problem
 *   human_readable: The Truth-Mining Exhaustion
 *   domain: informational/social/economic
 *
 * SUMMARY:
 *   A scenario where the cost of producing verified, grounded information is
 *   borne by a shrinking pool of 'truth-miners,' while the majority of the
 *   population consumes low-cost, unverified synthetic derivatives. This
 *   creates an epistemic free-rider problem, where the value of verified
 *   information is undermined by the proliferation of cheaper, less reliable
 *   alternatives.
 *
 * KEY AGENTS:
 *   - truth_miners: Primary target (powerless/trapped) — bears cost of producing verified information
 *   - synthetic_content_consumers: Primary beneficiary (institutional/arbitrage) — benefits from low-cost information, regardless of veracity
 *   - analytical_observer: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_free_rider_problem, 0.6).
domain_priors:suppression_score(epistemic_free_rider_problem, 0.4).
domain_priors:theater_ratio(epistemic_free_rider_problem, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_free_rider_problem, extractiveness, 0.6).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_free_rider_problem, tangled_rope).
narrative_ontology:human_readable(epistemic_free_rider_problem, "The Truth-Mining Exhaustion").
narrative_ontology:topic_domain(epistemic_free_rider_problem, "informational/social/economic").

domain_priors:requires_active_enforcement(epistemic_free_rider_problem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_free_rider_problem, synthetic_content_consumers).
narrative_ontology:constraint_victim(epistemic_free_rider_problem, truth_miners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual truth-miner, facing increasing costs to produce verified information, has no ability to exit the system without sacrificing their livelihood and reputation.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The consumer of synthetic content, able to arbitrage information sources, sees the system as beneficial. They obtain information at low cost, regardless of its veracity.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer recognizes both the coordination aspect (information dissemination) and the extraction aspect (exhaustion of truth-miners).
constraint_indexing:constraint_classification(epistemic_free_rider_problem, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_free_rider_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_free_rider_problem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_free_rider_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. Truth-miners are increasingly burdened by the cost of verification. Suppression (0.4): Moderate. Alternative sources of information exist, but they are often less reliable. Theater ratio (0.3): Low. The system still has some functional verification, but it is declining.
 *
 * PERSPECTIVAL GAP:
 *   Truth-miners experience the system as a Snare, as they are trapped and bear the costs. Consumers experience the system as a Rope, as they benefit from low-cost information. The analytical observer recognizes the Tangled Rope dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural position. Truth-miners (victims) have low power and no exit, leading to high d. Consumers (beneficiaries) have high power and arbitrage options, leading to low d.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_content_veracity,
    'What is the correlation between the cost of producing information and its veracity?',
    'Empirical analysis of various information sources, correlating production cost with error rates and bias.',
    'If low-cost information is consistently inaccurate, the problem is a pure Snare. If some low-cost information is accurate, the problem is Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_content_veracity, empirical, 'Correlation between production cost and veracity').

omega_variable(
    truth_miner_agency,
    'Can truth-miners effectively organize to demand compensation for their services?',
    'Analysis of collective action attempts by journalists, researchers, and fact-checkers.',
    'If truth-miners can organize, their perspective shifts from Snare to Tangled Rope, as they gain some agency. If they cannot organize, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_miner_agency, conceptual, 'Capacity for collective action among truth-miners').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_free_rider_problem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t0, epistemic_free_rider_problem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(epis_tr_t5, epistemic_free_rider_problem, theater_ratio, 5, 0.2).
narrative_ontology:measurement(epis_tr_t10, epistemic_free_rider_problem, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t0, epistemic_free_rider_problem, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(epis_be_t5, epistemic_free_rider_problem, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(epis_be_t10, epistemic_free_rider_problem, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
