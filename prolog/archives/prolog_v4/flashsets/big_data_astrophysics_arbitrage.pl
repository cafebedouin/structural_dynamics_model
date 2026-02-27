% ============================================================================
% CONSTRAINT STORY: big_data_astrophysics_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_big_data_astrophysics_arbitrage, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: big_data_astrophysics_arbitrage
 *   human_readable: Big Data Arbitrage in Modern Astrophysics
 *   domain: technological
 *
 * SUMMARY:
 *   As astronomy shifts from spatial mapping to time-domain analysis, the
 *   vast data streams (e.g., 1.6 petabytes from Pan-STARRS) create a new form
 *   of arbitrage. Researchers with early access to these data can exploit
 *   this advantage to publish findings before others, leading to career and
 *   funding benefits. This creates a structural tension between the
 *   incentives for early publication and the need for independent
 *   verification, producing a tangled rope.
 *
 * KEY AGENTS:
 *   - Early Access Researchers: Primary beneficiary (institutional/arbitrage) – gains career and funding benefits from early publications.
 *   - Independent Verification Teams: Primary victim (powerless/trapped) – struggles to replicate findings due to data access limitations.
 *   - Public Access Astronomy: Secondary victim (moderate/constrained) – suffers from delayed or restricted access to discoveries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, 0.55).
domain_priors:suppression_score(big_data_astrophysics_arbitrage, 0.4).
domain_priors:theater_ratio(big_data_astrophysics_arbitrage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, extractiveness, 0.55).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(big_data_astrophysics_arbitrage, tangled_rope).
narrative_ontology:human_readable(big_data_astrophysics_arbitrage, "Big Data Arbitrage in Modern Astrophysics").
narrative_ontology:topic_domain(big_data_astrophysics_arbitrage, "technological").

domain_priors:requires_active_enforcement(big_data_astrophysics_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(big_data_astrophysics_arbitrage, early_access_researchers).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, independent_verification_teams).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, public_access_astronomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Teams lacking early access and substantial computational resources find themselves trapped in a snare. They are unable to independently verify findings derived from proprietary datasets or analyses due to the high costs of replicating the initial analysis.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Public access to astronomical data and discoveries suffers. Delayed or restricted access to data hinders broad participation and education. Benefits are the continued refinement of complex models, but at the cost of widespread accessibility. Constrained as the public requires the access the research group allows or publishes.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Researchers with early or exclusive access to large datasets can arbitrage this advantage for publications, funding, and career advancement, thus experiencing a rope. They benefit from a head start and the ability to shape the initial interpretation of the data.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% The observer sees a tangled rope: a system that both enables rapid discovery and reinforces existing power structures. Early access advantages drive the field forward but also create barriers to entry and independent verification.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(big_data_astrophysics_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(big_data_astrophysics_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55. Early access advantages extract career and funding benefits from those without such access. Suppression: 0.40. Data access policies and computational resource constraints limit independent verification. Theater Ratio: 0.30. While performative aspects exist (e.g., hype around discoveries), the core function remains scientific discovery.
 *
 * PERSPECTIVAL GAP:
 *   The early access researchers see a rope; independent verification teams see a snare; public access views the situation as a tangle; the analytical observer seees the tangled rope of power.
 *
 * DIRECTIONALITY LOGIC:
 *   The early access group benefits; the independent groups are hurt. Public access is hurt but could gain from discoveries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_access_mandates,
    'To what extent can open access mandates mitigate the arbitrage opportunity?',
    'Analysis of publication and citation rates before and after implementation of open access policies.',
    'If effective, the arbitrage opportunity decreases, shifting the system towards a rope. If ineffective, the tangled rope or snare classification remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_access_mandates, empirical, 'Whether open access mandates reduce arbitrage.').

omega_variable(
    computational_resource_availability,
    'How does the increasing availability of cloud computing resources affect the ability of independent teams to verify results?',
    'Survey of computational resource usage by research teams and correlation with publication rates.',
    'If cloud resources level the playing field, the snare effect diminishes. If access remains unequal due to cost or expertise, the snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_resource_availability, empirical, 'Effect of cloud computing on verification ability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(big_data_astrophysics_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(big__tr_t0, big_data_astrophysics_arbitrage, theater_ratio, 0, 0.1).
narrative_ontology:measurement(big__tr_t5, big_data_astrophysics_arbitrage, theater_ratio, 5, 0.2).
narrative_ontology:measurement(big__tr_t10, big_data_astrophysics_arbitrage, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(big__be_t0, big_data_astrophysics_arbitrage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(big__be_t5, big_data_astrophysics_arbitrage, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(big__be_t10, big_data_astrophysics_arbitrage, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(big_data_astrophysics_arbitrage, information_standard).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, data_access_policies_astronomy).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, funding_priorities_astronomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
