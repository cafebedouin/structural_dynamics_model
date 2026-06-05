% ============================================================================
% CONSTRAINT STORY: permissive_software_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_software_licensing, []).

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
 *   constraint_id: permissive_software_licensing
 *   human_readable: Permissive Software Licenses (e.g., MIT, Apache)
 *   domain: technological/legal/economic
 *
 * SUMMARY:
 *   Permissive licenses (like MIT or Apache) allow users to do almost
 *   anything with source code—copy, modify, and redistribute it—even as part
 *   of proprietary, closed-source software. This fosters code reuse and
 *   innovation. The primary purpose is to enable broad adoption and
 *   commercialization of software.
 *
 * KEY AGENTS:
 *   - Software Developers: Primary beneficiaries (moderate/mobile) — Can use and modify the code as they wish.
 *   - Commercial Enterprises: Secondary beneficiaries (institutional/arbitrage) — Can incorporate the code into proprietary products.
 *   - Analytical Observer: Sees overall benefits (analytical/analytical) — Observes increased adoption and overall growth of ecosystem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_software_licensing, 0.35).
domain_priors:suppression_score(permissive_software_licensing, 0.2).
domain_priors:theater_ratio(permissive_software_licensing, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_software_licensing, extractiveness, 0.35).
narrative_ontology:constraint_metric(permissive_software_licensing, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(permissive_software_licensing, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_software_licensing, rope).
narrative_ontology:human_readable(permissive_software_licensing, "Permissive Software Licenses (e.g., MIT, Apache)").
narrative_ontology:topic_domain(permissive_software_licensing, "technological/legal/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_software_licensing, software_developers).
narrative_ontology:constraint_beneficiary(permissive_software_licensing, commercial_enterprises).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Commercial enterprises benefit from the permissive nature, allowing them to integrate open-source code into proprietary products without the restrictions of copyleft licenses. This fosters innovation and reduces development costs.
constraint_indexing:constraint_classification(permissive_software_licensing, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Individual developers benefit from the widespread adoption of permissive licenses, as it encourages code sharing, collaboration, and the creation of a vibrant ecosystem of reusable components. They can also choose to use the code in their own projects, whether open-source or proprietary.
constraint_indexing:constraint_classification(permissive_software_licensing, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% From an analytical perspective, permissive licenses facilitate the growth of the software industry by promoting code reuse, innovation, and collaboration. The lack of restrictions encourages wider adoption and accelerates technological progress.
constraint_indexing:constraint_classification(permissive_software_licensing, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_software_licensing_tests).
:- end_tests(permissive_software_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low. While permissive licenses do extract a degree of control from the original authors by allowing modification and redistribution without attribution, the overall impact is to promote collaboration and widespread use. The license minimizes restrictions. Suppression (0.20): Low. The presence of alternative licensing schemes (e.g., copyleft licenses) ensures that developers have choices and are not forced to use permissive licenses if they prefer greater control over their code. Theater ratio (0.10): Low. Very little performative activity involved.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because almost all the agents benefit from this license. It makes the code more readily accessible, which is highly valued and coordinated.
 *
 * DIRECTIONALITY LOGIC:
 *   The agents derive significant benefit from the software being freely available to do what they please with.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable, as this is classified as a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_software_licensing, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_software_licensing, information_standard).
narrative_ontology:affects_constraint(permissive_software_licensing, copyleft_licensing).

% DUAL FORMULATION NOTE:
% Permissive licensing represents a dual formulation to copyleft licensing, with differing control tradeoffs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
