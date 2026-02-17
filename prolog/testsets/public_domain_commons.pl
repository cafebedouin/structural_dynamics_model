% ============================================================================
% CONSTRAINT STORY: public_domain_commons
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

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
 *   The Public Domain consists of all creative work to which no exclusive
 *   intellectual property rights apply. These works are free for anyone to use,
 *   remix, and build upon. It represents the default state of information
 *   before or after the artificial constraint of Copyright is applied. This
 *   constraint is the existence and defense of that commons.
 *
 * KEY AGENTS (by structural relationship):
 *   - Commons Participants (students, artists, developers): Primary beneficiary (powerless/moderate/mobile) — uses the commons for education, creation, and innovation.
 *   - Legacy Rights-Holders (large media corporations): Institutional actor (institutional/constrained) — views the commons as a boundary condition that limits their ability to extract rent from intellectual property.
 *   - Analytical Observer: Sees the full structure as a pure coordination mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% By definition, the public domain is non-extractive. ε=0.0.
domain_priors:base_extractiveness(public_domain_commons, 0.0).
% While legally free, works can be suppressed by "copyfraud" (false copyright
% claims) or made difficult to access, requiring active defense.
domain_priors:suppression_score(public_domain_commons, 0.3).
% The public domain is purely functional; there is no theatrical component.
domain_priors:theater_ratio(public_domain_commons, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_domain_commons, extractiveness, 0.0).
narrative_ontology:constraint_metric(public_domain_commons, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(public_domain_commons, theater_ratio, 0.08).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(public_domain_commons, rope).
narrative_ontology:human_readable(public_domain_commons, "The Public Domain as a Cultural Commons").

% --- Binary flags ---
% The existence of the commons must be actively defended against encroachment
% by rights-holders seeking term extensions and through legal challenges to
% copyfraud. This flag is critical to prevent misclassification as a Scaffold.
domain_priors:requires_active_enforcement(public_domain_commons).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(public_domain_commons, commons_participants).
%
% Who bears disproportionate cost?
% No victim group. The public domain is a pure coordination good. The perceived
% "cost" to rights-holders is an opportunity cost, not a direct extraction.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE STUDENT / ARTIST (ROPE)
% Agent who uses the commons as a resource. With ε=0.0, χ is always 0.0.
% This is a pure coordination mechanism (Rope) for accessing knowledge and
% raw material for new creations.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE LEGACY RIGHTS-HOLDER (MOUNTAIN)
% From the perspective of an entity whose business model is rent-seeking on
% intellectual property, the public domain is not a Snare that extracts from
% them, but an unchangeable feature of the legal landscape that limits their
% ability to extract. It is a boundary condition, a Mountain.
constraint_indexing:constraint_classification(public_domain_commons, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% The analytical view sees a pure coordination mechanism with ε=0.0. The
% conflict surrounding it (e.g., copyright term extension) belongs to a
% separate, extractive constraint (copyright_law), not the commons itself.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_domain_commons_tests).

test(perspectival_gap) :-
    % Verify the key perspectival gap between users and rights-holders.
    constraint_indexing:constraint_classification(public_domain_commons, TypeUser, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_domain_commons, TypeHolder, context(agent_power(institutional), _, _, _)),
    TypeUser == rope,
    TypeHolder == mountain,
    TypeUser \= TypeHolder.

test(non_extractive_nature) :-
    % The public domain must be non-extractive.
    domain_priors:base_extractiveness(public_domain_commons, E),
    E < 0.1.

:- end_tests(public_domain_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.0): The public domain is the conceptual baseline
 *     for a non-extractive information system. It does not take; it provides.
 *   - Suppression (0.3): This score reflects the real-world friction from
 *     copyfraud and legislative efforts to shrink the commons, which require
 *     active defense by organizations like the EFF and Creative Commons.
 *   - requires_active_enforcement: This flag is crucial. It correctly models
 *     that the commons is not a passive, natural state but a legal space that
 *     must be actively defended to exist.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between users and legacy rights-holders.
 *   - For a user (student, artist), it's a Rope: a pure coordination tool for
 *     learning and creating. Since ε=0.0, χ is always 0, classifying as Rope.
 *   - For a rights-holder, it's a Mountain: an immutable boundary condition of
 *     the legal environment that prevents perpetual rent extraction. It doesn't
 *     extract *from* them; it simply *is*, limiting their field of action.
 *     The prior classification of Snare was a category error, confusing an
 *     opportunity cost with active extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint benefits all `commons_participants` by providing a free
 *   resource. There is no victim group, as the constraint itself does not
 *   impose costs. The "costs" perceived by rights-holders are an artifact of
 *   their business model clashing with this boundary condition.
 *
 * MANDATROPHY ANALYSIS:
 *   This model resolves a key ambiguity. The original file was in the
 *   "Scaffold Danger Zone" (low ε, no enforcement, beneficiary data), risking
 *   misclassification as a temporary Scaffold. By correctly identifying that
 *   the public domain `requires_active_enforcement` to defend its boundaries,
 *   we structurally distinguish it from a Scaffold and correctly classify it
 *   as a permanent Rope. This prevents mislabeling a foundational commons as
 *   a temporary support structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_public_domain_commons,
    'Will AI models trained on public domain data be able to compete with those trained on vast copyrighted datasets, or will the latter create an insurmountable advantage?',
    'Benchmarking performance of open-weights models trained on public data vs. closed-weights models trained on proprietary data over time.',
    'If public data is sufficient, the commons (Rope) becomes the primary foundation for future tech development. If not, the commons becomes a quaint but irrelevant starting point, and access to proprietary data becomes a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(public_domain_commons, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal data required as base_extractiveness (0.0) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The public domain is a shared pool of informational resources.
narrative_ontology:coordination_type(public_domain_commons, resource_allocation).

% Network relationships (structural influence edges)
% The public domain is defined in opposition to copyright law. Changes in
% copyright (e.g., term extensions) directly affect the scope of the commons.
narrative_ontology:affects_constraint(copyright_law, public_domain_commons).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary status
% and exit options correctly models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */