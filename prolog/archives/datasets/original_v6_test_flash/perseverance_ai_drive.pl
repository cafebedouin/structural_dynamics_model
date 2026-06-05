% ============================================================================
% CONSTRAINT STORY: perseverance_ai_drive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perseverance_ai_drive, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perseverance_ai_drive
 *   human_readable: AI-Driven Martian Rover Autonomy
 *   domain: technological
 *
 * SUMMARY:
 *   NASA's Perseverance rover successfully completed its first AI-planned
 *   drive on Mars, autonomously navigating a 200-foot (61-meter) path. This
 *   represents a significant step forward in rover autonomy, enabling more
 *   efficient exploration and scientific discovery. The constraint is
 *   characterized by the balance between the benefits of AI-driven navigation
 *   and the potential risks and limitations of relying on autonomous systems.
 *
 * KEY AGENTS:
 *   - NASA Mission Team: Primary beneficiary (institutional/arbitrage) - Gains efficiency and reduces workload.
 *   - Scientific Community: Secondary beneficiary (analytical/analytical) - Accesses more data and discoveries.
 *   - Rover Engineering Team: Supporting role (powerful/constrained) - Involved in development and maintenance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perseverance_ai_drive, 0.35).
domain_priors:suppression_score(perseverance_ai_drive, 0.2).
domain_priors:theater_ratio(perseverance_ai_drive, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perseverance_ai_drive, extractiveness, 0.35).
narrative_ontology:constraint_metric(perseverance_ai_drive, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(perseverance_ai_drive, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perseverance_ai_drive, rope).
narrative_ontology:human_readable(perseverance_ai_drive, "AI-Driven Martian Rover Autonomy").
narrative_ontology:topic_domain(perseverance_ai_drive, "technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, nasa_mission_team).
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, scientific_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: NASA Mission Team (Rope) - Benefits from increased efficiency and reduced workload, enabling more focused scientific investigations. They have arbitrage options because they can always revert to manual control if the AI system fails or does not meet requirements.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(universal))).

% Perspective 2: Scientific Community (Rope) - Gains access to more data and discoveries due to faster and more efficient rover operations. Analytical perspective allows assessment of long-term impact on scientific knowledge.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 3: Rover Engineering Team (Scaffold) - Initially experience increased workload during the AI system development and integration phase. This is a temporary burden that decreases over time as the AI system becomes more reliable and requires less maintenance. Exit options are constrained as their primary responsibility is rover operation, but they gain new skills through the project.
constraint_indexing:constraint_classification(perseverance_ai_drive, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perseverance_ai_drive_tests).
:- end_tests(perseverance_ai_drive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low to moderate. The AI system requires development and maintenance efforts, but the long-term benefits outweigh the initial costs. The extractiveness is primarily related to the resources required for the AI system development. Suppression (0.20): Low. The AI system does not significantly suppress alternative navigation methods. Manual control remains an option, and other autonomous navigation techniques can be explored. Theater ratio (0.10): Very low. The primary focus is on the functional performance of the AI system, with minimal emphasis on performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between the NASA Mission Team and the Scientific Community. The mission team directly experiences the increased efficiency and reduced workload, while the scientific community benefits from the increased data and discoveries. The rover engineering team experiences a scaffold effect during the initial development phases.
 *
 * DIRECTIONALITY LOGIC:
 *   NASA benefits from increased efficiency. The scientific community benefits from more data. The engineering team contributes to the effort.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction because the AI-driven autonomy demonstrably improves rover efficiency and data collection, benefiting both the mission team and the scientific community.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perseverance_ai_drive, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perseverance_ai_drive, resource_allocation).
narrative_ontology:affects_constraint(perseverance_ai_drive, mars_sample_return_mission).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
