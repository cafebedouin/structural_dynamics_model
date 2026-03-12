% ============================================================================
% CONSTRAINT STORY: mediated_knowledge_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mediated_knowledge_dependency, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mediated_knowledge_dependency
 *   human_readable: Mediated Knowledge Dependency
 *   domain: epistemology/cognitive_science/information_theory
 *
 * SUMMARY:
 *   Mediated knowledge dependency is the structural constraint that most
 *   knowledge claims accepted by any agent arrive through instruments,
 *   records, and testimony rather than direct sensory experience. This is not
 *   an institutional artifact or a social construction — it emerges from the
 *   interaction between finite cognitive capacity and the exponential growth
 *   of knowledge. A contemporary individual accepts millions of knowledge
 *   claims (historical events, scientific findings, geographic facts,
 *   mathematical theorems, technological specifications) on mediation. Direct
 *   verification of even a tiny fraction would exceed a human lifespan. The
 *   constraint is scale-invariant: it applies to laypeople, domain experts,
 *   and institutions. Even a physicist who directly verifies one experimental
 *   result relies on mediated knowledge for the instrument calibration,
 *   background theory, mathematical foundations, and thousands of other
 *   experimental results that contextualize the finding. The constraint's
 *   extractiveness (0.08) reflects the minimal overhead of coordination
 *   mechanisms (language, notation, institutional certification) required to
 *   make testimony reliable. The suppression (0.02) reflects that agents are
 *   not coerced into accepting mediated knowledge — they do so because direct
 *   verification is informationally impossible. The accessibility collapse
 *   (0.95) reflects that no alternative epistemic strategy is available: an
 *   agent who refuses all mediated knowledge cannot function in any domain.
 *   The resistance (0.08) reflects the small margin where direct verification
 *   is possible (immediate sensory experience, simple experiments, basic
 *   logical proofs) but this margin cannot be expanded to cover the knowledge
 *   base as a whole.
 *
 * KEY AGENTS:
 *   - Individual Knower: Any epistemic agent (powerless/trapped at immediate/local scale) — cannot directly verify most accepted knowledge within biographical constraints
 *   - Specialist Researcher: Domain expert (moderate/constrained) — has tools and training to verify some claims directly but still relies overwhelmingly on mediated knowledge even within specialty
 *   - Knowledge Institution: University, research institute, scientific society (institutional/arbitrage) — coordinates testimony and certification but cannot eliminate underlying dependency
 *   - Verification Coalition: Organized replication efforts, open science movements (organized/mobile) — can shift the margin of direct verification but cannot escape mediation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as information-theoretic limit, not social arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mediated_knowledge_dependency, 0.08).
domain_priors:suppression_score(mediated_knowledge_dependency, 0.02).
domain_priors:theater_ratio(mediated_knowledge_dependency, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mediated_knowledge_dependency, extractiveness, 0.08).
narrative_ontology:constraint_metric(mediated_knowledge_dependency, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(mediated_knowledge_dependency, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mediated_knowledge_dependency, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(mediated_knowledge_dependency, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mediated_knowledge_dependency, mountain).
narrative_ontology:human_readable(mediated_knowledge_dependency, "Mediated Knowledge Dependency").
narrative_ontology:topic_domain(mediated_knowledge_dependency, "epistemology/cognitive_science/information_theory").

domain_priors:emerges_naturally(mediated_knowledge_dependency).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL KNOWER (MOUNTAIN) — Cannot directly verify most accepted knowledge claims within biographical timescales. The ratio of directly witnessed facts to total accepted knowledge is structurally constrained by cognitive bandwidth, lifespan, and physical access. This is not institutional gatekeeping but informational physics.
constraint_indexing:constraint_classification(mediated_knowledge_dependency, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SPECIALIST RESEARCHER (MOUNTAIN) — Even domain experts rely overwhelmingly on mediated knowledge. A physicist accepts thousands of experimental results, mathematical proofs, and theoretical frameworks on testimony and institutional certification. Direct replication of foundational work is economically and temporally impossible. The constraint is structural, not social.
constraint_indexing:constraint_classification(mediated_knowledge_dependency, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KNOWLEDGE INSTITUTION (MOUNTAIN) — Universities, research institutes, and scientific societies exist precisely because direct verification is impossible at scale. The institution coordinates testimony and certification but cannot eliminate the underlying dependency. Even institutions rely on other institutions' mediated knowledge. The dependency is not created by institutions; institutions are created by the dependency.
constraint_indexing:constraint_classification(mediated_knowledge_dependency, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The constraint emerges from information-theoretic and cognitive limits. Human working memory (7±2 items), lifespan (~10^9 seconds), sensory bandwidth (~10^7 bits/second), and the exponential growth of knowledge create an unbridgeable gap between what can be directly verified and what must be accepted on mediation. This is a structural feature of finite agents in an information-rich universe, invariant across institutional arrangements and cultural contexts.
constraint_indexing:constraint_classification(mediated_knowledge_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: VERIFICATION COALITION (MOUNTAIN) — Organized efforts to increase direct verification (replication studies, open data, citizen science) can shift the margin but cannot eliminate the dependency. Even maximally transparent systems require agents to accept instrument calibration, measurement protocols, and background theory on testimony. The coalition's mobility allows them to choose which mediations to trust, but not to escape mediation itself.
constraint_indexing:constraint_classification(mediated_knowledge_dependency, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mediated_knowledge_dependency_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(mediated_knowledge_dependency, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mediated_knowledge_dependency, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(mediated_knowledge_dependency, ExtMetricName, E),
    domain_priors:suppression_score(mediated_knowledge_dependency, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(mediated_knowledge_dependency),
    narrative_ontology:constraint_metric(mediated_knowledge_dependency, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(mediated_knowledge_dependency, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(mediated_knowledge_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal overhead beyond the inherent cost of communication and coordination. Language, notation systems, and institutional certification mechanisms have small but non-zero costs (learning, standardization, maintenance). These costs are coordination mechanisms, not extraction. The value reflects that mediated knowledge is not a rent-seeking arrangement but a solution to an information-theoretic problem. Suppression (0.02): Minimal. Agents are not coerced into accepting mediated knowledge. The constraint emerges from cognitive and physical limits, not from enforcement. An agent can choose to reject mediated knowledge, but doing so eliminates functional participation in any knowledge domain. The tiny suppression value reflects the opportunity cost of the coordination mechanisms (time spent learning notation, institutional certification requirements) but these are not coercive barriers. Theater ratio (0.15): Low. Most mediated knowledge transmission is functional rather than performative. Peer review, citation practices, and institutional certification have some theatrical elements (status signaling, credentialism) but the core function (enabling testimony to be reliable enough for knowledge accumulation) is genuine. The theater ratio is higher than the extractiveness because some coordination overhead is performative (journal prestige hierarchies, credential inflation) even though the underlying dependency is not extractive. Accessibility collapse (0.95): Very high. No alternative epistemic strategy is available at scale. An agent who attempts to verify all knowledge claims directly cannot progress beyond trivial domains. Direct verification is possible only at the margin (immediate sensory experience, simple replication) and this margin cannot be expanded to cover the knowledge base. Resistance (0.08): Very low. The constraint is not imposed by any actor and cannot be resisted through collective action, institutional reform, or technological innovation. Improvements in verification technology (better instruments, open data, distributed replication) shift the margin but do not eliminate the dependency. The small resistance value reflects that agents can choose which specific mediations to trust (epistemic autonomy at the object level) but cannot escape mediation at the structural level.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap — all agents classify it as mountain regardless of power, time horizon, exit options, or scope. The individual knower at immediate/local scale, the specialist researcher at biographical/national scale, the knowledge institution at generational/global scale, the verification coalition at generational/continental scale, and the analytical observer at civilizational/universal scale all experience the same structural dependency. This uniformity is diagnostic of a genuine natural law constraint. The constraint is invariant across observables: whether measured by ratio of directly verified claims, reliance on testimony, dependence on instruments, or acceptance of institutional certification, the structural dependency remains. The lack of perspectival gap distinguishes this constraint from institutional arrangements that appear as mountains from some perspectives but reveal extraction from others (false summits). Mediated knowledge dependency is not a naturalized social arrangement — it is a structural feature of finite agents in an information-rich universe.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a uniform-type mountain constraint with no beneficiaries or victims. All agents experience the same structural dependency regardless of power, exit options, or scope. The constraint is not a flow of extraction from one group to another but a universal informational limit. Directionality values are not applicable — the constraint has no asymmetric extraction to measure. The low extractiveness (0.08) represents coordination cost, not rent extraction, and this cost is distributed symmetrically across all epistemic agents. Institutions that coordinate testimony (universities, journals, certification bodies) are not beneficiaries extracting from individuals — they are coordination mechanisms that all agents rely on, including the institutions themselves. The analytical observer perspective confirms this: the constraint emerges from information theory and cognitive science, not from institutional arrangements that could be reformed or resisted.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint demonstrates how genuine natural law constraints differ from false summits (naturalized institutional arrangements). The mandatrophy question is: 'Is this an immutable limit or a contingent arrangement that benefits some agents at others' expense?' The resolution: (1) No beneficiary group exists — all agents including institutions experience the same dependency. (2) The constraint emerges from information-theoretic and cognitive limits, not from enforcement or institutional design. (3) Resistance is structurally impossible — no alternative epistemic strategy is available at scale. (4) The constraint is invariant across institutional arrangements, cultural contexts, and technological regimes. (5) Improvements in verification technology (better instruments, open data, replication infrastructure) shift the margin but cannot eliminate the dependency — the exponential growth of knowledge outpaces any linear increase in verification capacity. (6) The extractiveness (0.08) represents coordination cost, not rent extraction, and is symmetrically distributed. This is not a case where 'experts claim it's a mountain to protect their status' — even experts experience the constraint and would benefit from its elimination if that were possible. The constraint is a genuine mountain, not a snare disguised as one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mediated_knowledge_dependency, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mediated_knowledge_dependency, information_standard).

% DUAL FORMULATION NOTE:
% Mediated knowledge dependency is a foundational constraint that underlies many institutional and epistemic constraints but is not decomposable into multiple stories with different epsilon values. The constraint is invariant across measurement methodologies and observables. Institutional arrangements for managing testimony (peer review, certification, replication norms) are separate constraints downstream of this one, each with their own extractiveness values reflecting the specific institutional mechanisms rather than the underlying informational dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
