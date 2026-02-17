% ============================================================================
% CONSTRAINT STORY: wikipedia_notability_requirement_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_wikipedia_notability_requirement_2026, []).

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
 *   constraint_id: wikipedia_notability_requirement_2026
 *   human_readable: Wikipedia Notability Requirement (2026)
 *   domain: social/technological
 *
 * SUMMARY:
 *   The notability requirement is the core gatekeeping policy determining which
 *   subjects deserve a dedicated article on Wikipedia. It aims to foster
 *   mass-scale trust by ensuring Wikipedia remains a repository of verifiable
 *   information from reliable sources, rather than a fragmented collection of
 *   unverified data. This creates a structural tension between ensuring quality
 *   and including marginalized or niche knowledge.
 *
 * KEY AGENTS (by structural relationship):
 *   - Niche Contributors: Primary target (powerless/trapped) — bear the cost of exclusion and wasted labor.
 *   - Admin/Power Editors: Primary beneficiary (institutional/arbitrage) — benefit from a clear, enforceable rule that maintains site integrity.
 *   - Information Analyst: Analytical observer — sees the dual coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_notability_requirement_2026, 0.40).
domain_priors:suppression_score(wikipedia_notability_requirement_2026, 0.70).
domain_priors:theater_ratio(wikipedia_notability_requirement_2026, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, extractiveness, 0.40).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, theater_ratio, 0.06).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(wikipedia_notability_requirement_2026, tangled_rope).
narrative_ontology:human_readable(wikipedia_notability_requirement_2026, "Wikipedia Notability Requirement (2026)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(wikipedia_notability_requirement_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, institutional_editors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, marginalized_knowledge_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE NICHE CONTRIBUTOR (SNARE)
% For those trying to document marginalized knowledge, the rule is a barrier
% that extracts their labor (research, writing) for no gain, as the article
% is ultimately deleted. Their exit is trapped as there is no comparable alternative platform.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ESTABLISHED ADMIN (ROPE)
% For institutional editors, the rule is a pure coordination device. It provides
% a clear, objective standard for content decisions, reducing conflict and
% maintaining the encyclopedia's quality and reputation. They benefit directly.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the essential coordination function (Rope) and the
% asymmetric extraction (Snare). The system requires active enforcement to
% maintain this balance, fitting the Tangled Rope definition.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_notability_requirement_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation_for_tangled_rope) :-
    % Verify that the metrics support a Tangled Rope classification.
    narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, extractiveness, E),
    narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

test(structural_data_for_tangled_rope) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, _),
    narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, _),
    domain_priors:requires_active_enforcement(wikipedia_notability_requirement_2026).

:- end_tests(wikipedia_notability_requirement_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics were chosen to reflect a system with a genuine coordination
 *   function that simultaneously imposes asymmetric costs.
 *   - Base Extractiveness (ε=0.40): Represents the significant but not purely
 *     extractive labor cost imposed on all contributors to meet sourcing standards.
 *   - Suppression Score (S=0.70): Reflects the strict, high-suppression
 *     gatekeeping function where non-compliant content is actively deleted.
 *   - Analytical Classification: The combination of a coordination function
 *     (beneficiary exists), asymmetric extraction (victim exists), and active
 *     enforcement makes this a canonical Tangled Rope. The original file's
 *     'mountain' claim was incorrect as the metrics violate mountain thresholds.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Institutional editors see a pure coordination tool (Rope)
 *   that enables the project's success. Niche contributors whose topics are
 *   deemed "non-notable" experience a purely extractive barrier (Snare) that
 *   invalidates their knowledge and labor.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `institutional_editors` who wield the rule to maintain
 *     order and quality, reinforcing the platform's authority and their role within it.
 *   - Victims: `marginalized_knowledge_producers` whose subjects lack coverage
 *     in "reliable sources" (often a lagging indicator of importance), leading
 *     to their systematic exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial for avoiding mandatrophy. It
 *   prevents mislabeling the notability rule as a pure Rope (ignoring its
 *   exclusionary effects) or a pure Snare (ignoring its necessary function in
 *   maintaining a reliable, large-scale knowledge base). It correctly identifies
 *   the system as one that both coordinates and extracts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_wikipedia_notability_requirement_2026,
    "Is 'notability' a neutral filter for reliability or a systemic filter for ideological/cultural exclusion?",
    "An audit of deletion rates comparing topics from marginalized cultures vs. topics from dominant Western cultures, controlling for source availability.",
    "If primarily exclusionary, the constraint is a high-risk Tangled Rope, bordering on a Snare even from an analytical view. If neutral, it is a lower-risk, functional Tangled Rope.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(wikipedia_notability_requirement_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is not > 0.46, so temporal data is not required by the linter.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(wikipedia_notability_requirement_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */