% ============================================================================
% CONSTRAINT STORY: guinea_worm_eradication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guinea_worm_eradication, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: guinea_worm_eradication
 *   human_readable: Global Guinea Worm Eradication Program
 *   domain: social
 *
 * SUMMARY:
 *   The Global Guinea Worm Eradication Program, led by The Carter Center,
 *   aims to eradicate Guinea worm disease through providing safe drinking
 *   water sources and health education, reducing human suffering in endemic
 *   regions. The program involves collaboration with local communities,
 *   national governments, and international organizations, creating a
 *   sustainable and equitable solution.
 *
 * KEY AGENTS:
 *   - Endemic Communities: Primary beneficiaries (powerless/mobile) - Gain access to safe drinking water and health education, improving their overall health and well-being.
 *   - The Carter Center: Program coordinator (institutional/arbitrage) - Enhances its reputation and attracts funding for other global health initiatives.
 *   - National Governments: Collaborators (organized/mobile) - Strengthen their public health infrastructure and improve their international standing.
 *   - Analytical Observer: Global perspective (analytical/analytical) - Assesses the program's impact and effectiveness in eradicating Guinea worm disease.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guinea_worm_eradication, 0.2).
domain_priors:suppression_score(guinea_worm_eradication, 0.1).
domain_priors:theater_ratio(guinea_worm_eradication, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guinea_worm_eradication, extractiveness, 0.2).
narrative_ontology:constraint_metric(guinea_worm_eradication, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(guinea_worm_eradication, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guinea_worm_eradication, rope).
narrative_ontology:human_readable(guinea_worm_eradication, "Global Guinea Worm Eradication Program").
narrative_ontology:topic_domain(guinea_worm_eradication, "social").

domain_priors:requires_active_enforcement(guinea_worm_eradication).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guinea_worm_eradication, endemic_communities).
narrative_ontology:constraint_beneficiary(guinea_worm_eradication, carter_center).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Endemic communities benefit from the program through improved health and access to safe drinking water, allowing them to escape the cycle of infection.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% The Carter Center coordinates the program and enhances its reputation as a global health leader, allowing it to attract funding and support for other initiatives.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% National governments in endemic countries collaborate with the program, strengthening their public health infrastructure and improving their international standing.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% From a global perspective, the program represents a successful international coordination effort to eradicate a debilitating disease, showcasing the potential for collaborative global health initiatives.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guinea_worm_eradication_tests).
:- end_tests(guinea_worm_eradication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.20): Low. The program primarily focuses on providing aid and resources to endemic communities, with minimal extraction involved. Suppression (0.10): Low. The program empowers communities through education and provides alternative water sources, reducing reliance on contaminated water sources.
 *
 * PERSPECTIVAL GAP:
 *   The program is largely seen as a beneficial coordination effort, with all perspectives classifying it as a Rope. The different perspectives highlight the various stakeholders involved and their respective benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the relationship between the agents and the program. Endemic communities benefit directly, while The Carter Center and national governments experience mutual benefits through collaboration and improved health outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The program's focus on providing aid and resources, rather than extracting from communities, ensures that it is classified as a Rope rather than a Snare. The program's goals align with improving the health and well-being of endemic populations, preventing any mislabeling of coordination as pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guinea_worm_eradication, 1986, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guinea_worm_eradication, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
