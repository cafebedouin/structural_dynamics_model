% ============================================================================
% CONSTRAINT STORY: wikipedia_crowdsourcing_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wikipedia_crowdsourcing_2026, []).

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
 *   constraint_id: wikipedia_crowdsourcing_2026
 *   human_readable: Wikipedia Crowdsourced Knowledge Governance
 *   domain: technological/social/political
 *
 * SUMMARY:
 *   Wikipedia operates as a massive, open-access knowledge repository that
 *   functions through radical collaboration rather than typical internet
 *   hostility. It serves as a model of crowdsourced knowledge governance,
 *   leveraging the collective intelligence of a global community of editors
 *   to create and maintain a vast encyclopedia. While disputes and biases
 *   exist, the system's overall performance demonstrates a functional,
 *   globally accessible resource.
 *
 * KEY AGENTS:
 *   - Wikipedia Foundation: Institutional support and maintenance of the platform.
 *   - Content Editors: Voluntary contributors who create and curate content.
 *   - Knowledge Seekers: Users who access and consume information on Wikipedia.
 *   - Analytical Observer: Researchers and analysts studying the dynamics of Wikipedia's governance model.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_crowdsourcing_2026, 0.35).
domain_priors:suppression_score(wikipedia_crowdsourcing_2026, 0.25).
domain_priors:theater_ratio(wikipedia_crowdsourcing_2026, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_crowdsourcing_2026, extractiveness, 0.35).
narrative_ontology:constraint_metric(wikipedia_crowdsourcing_2026, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(wikipedia_crowdsourcing_2026, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wikipedia_crowdsourcing_2026, rope).
narrative_ontology:human_readable(wikipedia_crowdsourcing_2026, "Wikipedia Crowdsourced Knowledge Governance").
narrative_ontology:topic_domain(wikipedia_crowdsourcing_2026, "technological/social/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wikipedia_crowdsourcing_2026, knowledge_seekers).
narrative_ontology:constraint_beneficiary(wikipedia_crowdsourcing_2026, content_editors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Wikipedia Foundation benefits by maintaining a valuable resource that enhances its reputation and mission.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Content Editors benefit by contributing to a vast knowledge base and gaining recognition for their expertise, can leave easily
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Knowledge seekers benefit by accessing a wide range of information easily, can seek other sources if needed.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% Analytical observers view the system as a successful coordination mechanism for global knowledge sharing.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_crowdsourcing_2026_tests).
:- end_tests(wikipedia_crowdsourcing_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Represents the effort required to contribute and maintain content, as well as occasional disputes and edit wars. Suppression (0.25): Represents the moderation policies and community norms that discourage vandalism and misinformation, but also can suppress legitimate dissenting views. Theater Ratio (0.15): Represents the minimal amount of performative activity relative to its actual role as a large knowledge base.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives view the system as beneficial, but some editors may experience suppression more acutely than others. The Foundation's arbitrage exit is due to its ability to adapt the rules and system as needed. The Knowledge Seeker's mobile exit represents the capacity to simply use other sources. The Analytical Observer has full awareness of these tradeoffs. The different levels of power and different exit options give slightly different χ values.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries include knowledge seekers and content editors, who gain access to and contribute to a global knowledge base, respectively. The directionality is derived from the benefit these agents receive from participating in the system, creating a coordination effect.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wikipedia_crowdsourcing_2026, 2001, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wikipedia_crowdsourcing_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
