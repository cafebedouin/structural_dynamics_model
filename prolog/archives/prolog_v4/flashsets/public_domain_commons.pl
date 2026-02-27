% ============================================================================
% CONSTRAINT STORY: public_domain_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_domain_commons, []).

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
 *   constraint_id: public_domain_commons
 *   human_readable: The Public Domain as a Cultural Commons
 *   domain: legal/economic/social
 *
 * SUMMARY:
 *   The public domain serves as a cultural commons, providing a foundation
 *   for creativity, education, and research by ensuring that certain works
 *   are freely available for use by anyone. It's a collection of works that
 *   are not protected by intellectual property law, either because the term
 *   of copyright has expired, or because the works were never eligible for
 *   copyright protection. The public domain is essential for preserving
 *   cultural heritage and fostering innovation.
 *
 * KEY AGENTS:
 *   - Artists: Benefit from the ability to freely use and build upon existing works.
 *   - Educators: Benefit from the ability to freely use and adapt materials for teaching.
 *   - Researchers: Benefit from the ability to freely access and analyze data and information.
 *   - General Public: Benefit from access to a wide range of cultural and informational resources.
 *   - Legal Scholars: Study and advocate for the public domain's role in balancing creators' rights and the public interest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_domain_commons, 0.35).
domain_priors:suppression_score(public_domain_commons, 0.25).
domain_priors:theater_ratio(public_domain_commons, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_domain_commons, extractiveness, 0.35).
narrative_ontology:constraint_metric(public_domain_commons, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(public_domain_commons, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_domain_commons, rope).
narrative_ontology:human_readable(public_domain_commons, "The Public Domain as a Cultural Commons").
narrative_ontology:topic_domain(public_domain_commons, "legal/economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_domain_commons, artists).
narrative_ontology:constraint_beneficiary(public_domain_commons, educators).
narrative_ontology:constraint_beneficiary(public_domain_commons, researchers).
narrative_ontology:constraint_beneficiary(public_domain_commons, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Legal scholars view the public domain as a crucial mechanism for balancing creators' rights with the public interest, fostering innovation and cultural progress.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Artists benefit from the public domain by being able to freely incorporate existing works into their own, building upon a shared cultural heritage.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Educators benefit from the public domain by being able to freely use and adapt materials for teaching purposes, promoting access to knowledge.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Researchers benefit from the public domain by being able to freely access and analyze data and information, fostering scientific discovery.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The general public benefits from the public domain by having access to a wealth of cultural and informational resources, enriching their lives and fostering civic engagement.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Creative Commons organizations see the Public Domain as a foundation for expanding open access to creative works, however, they still need to actively enforce licenses to protect from copyright infringement.
constraint_indexing:constraint_classification(public_domain_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_domain_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_domain_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_domain_commons, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(public_domain_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because, although there is a cost associated with maintaining and curating the public domain, the benefits far outweigh the costs. The suppression is low as well, as the public domain fosters freedom and access to information. The theater ratio is low because this is functional and not performative.
 *
 * PERSPECTIVAL GAP:
 *   The different perspectives all classify the public domain as a rope, but their reasons for doing so vary. Legal scholars emphasize the legal and policy aspects, artists focus on the creative opportunities, educators emphasize the educational benefits, researchers highlight the research potential, and the general public values the accessibility of information.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the extent to which each group benefits from the public domain. Legal scholars, artists, educators, researchers, and the general public all benefit from the public domain, so they all have a low directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_domain_commons, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_domain_commons, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
