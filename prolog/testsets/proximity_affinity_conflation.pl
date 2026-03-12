% ============================================================================
% CONSTRAINT STORY: proximity_affinity_conflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proximity_affinity_conflation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proximity_affinity_conflation
 *   human_readable: Proximity-Affinity Conflation in Relationship Formation
 *   domain: moral_philosophy/social_psychology/virtue_ethics
 *
 * SUMMARY:
 *   Proximity-affinity conflation is the cognitive mechanism whereby spatial
 *   or temporal proximity is misread as elective affinity or shared values.
 *   Humans treat neighbors, coworkers, classmates, and family members as if
 *   proximity itself signals value alignment, when in fact proximity is often
 *   circumstantial (inherited geography, employer assignment, birth family).
 *   This constraint appears uniformly as rope across all perspectives because
 *   the cognitive heuristic serves a genuine coordination function: it
 *   enables efficient social bond formation by reducing search costs and
 *   providing a default mechanism for trust allocation. The extractiveness is
 *   low (0.18) because the heuristic works well enough in most contexts —
 *   proximity-based relationships often DO develop genuine affinity over time
 *   through repeated interaction, shared experience, and mutual adaptation.
 *   The constraint coordinates social life with minimal coercive overhead.
 *
 * KEY AGENTS:
 *   - Geographic Stable Individual: Beneficiary (moderate/mobile) — uses proximity heuristic to build local community efficiently
 *   - Mobile Professional: Beneficiary (moderate/mobile) — leverages heuristic to integrate rapidly after relocation
 *   - Intentional Community Builder: Beneficiary (organized/mobile) — deliberately uses proximity to generate affinity
 *   - Evolutionary Psychology Perspective: Institutional observer (institutional/arbitrage) — sees heuristic as adaptive coordination mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes low-extraction coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proximity_affinity_conflation, 0.18).
domain_priors:suppression_score(proximity_affinity_conflation, 0.22).
domain_priors:theater_ratio(proximity_affinity_conflation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proximity_affinity_conflation, extractiveness, 0.18).
narrative_ontology:constraint_metric(proximity_affinity_conflation, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(proximity_affinity_conflation, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proximity_affinity_conflation, rope).
narrative_ontology:human_readable(proximity_affinity_conflation, "Proximity-Affinity Conflation in Relationship Formation").
narrative_ontology:topic_domain(proximity_affinity_conflation, "moral_philosophy/social_psychology/virtue_ethics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proximity_affinity_conflation, proximity_based_communities).
narrative_ontology:constraint_beneficiary(proximity_affinity_conflation, inherited_relationship_networks).
narrative_ontology:constraint_beneficiary(proximity_affinity_conflation, geographic_stability_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GEOGRAPHIC STABLE INDIVIDUAL (ROPE) — Experiences proximity-based relationship formation as efficient coordination. Proximity provides low-cost access to social bonds, shared resources, and mutual aid networks. The cognitive heuristic (proximity → affinity) reduces search costs and enables community formation. Minimal extraction — the constraint coordinates genuine social needs.
constraint_indexing:constraint_classification(proximity_affinity_conflation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: MOBILE PROFESSIONAL (ROPE) — Sees proximity-affinity conflation as a useful social heuristic that enables rapid community integration after relocation. The cognitive shortcut (treating neighbors/coworkers as potential friends) accelerates social capital formation in new environments. Low extraction — the mechanism serves genuine coordination function even when the agent is aware of the heuristic nature.
constraint_indexing:constraint_classification(proximity_affinity_conflation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTENTIONAL COMMUNITY BUILDER (ROPE) — Organized agents (cohousing movements, ecovillages, religious communities) deliberately leverage proximity to build affinity. They recognize the heuristic and use it instrumentally: shared physical space creates conditions for value alignment to emerge. The conflation is not a bug but a feature — proximity is chosen as a mechanism to generate genuine affinity over time.
constraint_indexing:constraint_classification(proximity_affinity_conflation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EVOLUTIONARY PSYCHOLOGY PERSPECTIVE (ROPE) — Institutional/academic view sees proximity-affinity conflation as an adaptive heuristic from ancestral environments where proximity WAS a reliable signal of shared fate and values (small-scale societies, limited mobility). The cognitive mechanism coordinates social bond formation efficiently in environments where the heuristic remains valid. Low extraction — the constraint reflects genuine coordination logic, though the match between heuristic and environment has degraded in high-mobility modern contexts.
constraint_indexing:constraint_classification(proximity_affinity_conflation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, proximity-affinity conflation is a low-extraction coordination mechanism. The cognitive heuristic solves a real problem (how to allocate limited social attention and trust) with minimal coercive overhead. Relationship retention rates vs stated value alignment show that proximity-based relationships often DO develop genuine affinity over time through repeated interaction and shared experience. The constraint is not extractive — it coordinates social bond formation through a cognitively efficient shortcut that works well enough in most contexts.
constraint_indexing:constraint_classification(proximity_affinity_conflation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proximity_affinity_conflation_tests).
:- end_tests(proximity_affinity_conflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The proximity-affinity heuristic does create some mismatch — agents invest in relationships based on circumstantial proximity rather than deep value alignment — but the mismatch is small because proximity genuinely predicts shared experience and mutual adaptation over time. The cognitive shortcut is not a trap; it is an efficient coordination mechanism. Suppression (0.22): Low. Agents are free to exit proximity-based relationships and seek value-aligned connections elsewhere. The constraint does not coerce relationship maintenance. The slight suppression reflects social costs of exit (neighborhood norms, workplace dynamics) but these are mild. Theater ratio (0.15): Very low. The constraint is not performative — proximity-based relationship formation is functional, not theatrical. Agents genuinely form bonds, share resources, and coordinate mutual aid through proximity networks. The measurements show slight increases over the interval, reflecting modest degradation as mobility increases and the heuristic's match to environment weakens, but the constraint remains low-extraction coordination throughout.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all perspectives classify as rope because the structural reality is uniform. The proximity-affinity heuristic is a genuine coordination mechanism with low extraction regardless of the observer's position. The slight variation in experienced extraction comes from differences in mobility and intentionality (organized agents use the heuristic more deliberately than moderate agents) but these differences do not change the classification type. The uniformity is diagnostically significant: it demonstrates that not all constraints produce perspectival disagreement. When a cognitive mechanism serves a genuine coordination function with minimal coercion, all observers see rope.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives are beneficiaries of the proximity-affinity heuristic. Geographic stable individuals benefit from efficient local community formation. Mobile professionals benefit from rapid integration mechanisms. Intentional community builders benefit from a tool they can use deliberately. The evolutionary psychology perspective sees an adaptive coordination mechanism. The analytical observer sees low-extraction coordination. There are no victims in this constraint — the cognitive heuristic serves all agents by reducing the search costs and cognitive load of relationship formation. Directionality values are uniformly low (beneficiary range), producing low or negative effective extraction across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that low-extraction coordination (rope) is a real structural category, not just an absence of extraction. The proximity-affinity heuristic could be framed as extractive ('people are trapped in circumstantial relationships based on false signals of value alignment') but the empirical data contradicts this framing: relationship retention rates show that proximity-based relationships have comparable satisfaction and longevity to choice-based relationships, and value alignment often emerges over time through shared experience. The constraint coordinates social bond formation efficiently, and the cognitive conflation (treating proximity as affinity) is not a bug but a feature — it enables trust and cooperation to develop in contexts where deep value alignment would be too costly to verify in advance. The rope classification is not a euphemism for hidden extraction; it is an accurate description of low-overhead coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    heuristic_validity_threshold,
    'At what mobility level does the proximity-affinity heuristic shift from adaptive coordination to maladaptive mismatch?',
    'Longitudinal studies comparing relationship satisfaction and value alignment in high-mobility vs low-mobility populations; measurement of heuristic accuracy (proximity predicting affinity) across different mobility contexts',
    'If threshold is low (frequent relocation breaks heuristic): constraint becomes extractive in modern contexts, misclassifying as rope when it should be tangled_rope for mobile populations. If threshold is high (heuristic remains valid even with mobility): rope classification holds across contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heuristic_validity_threshold, empirical, 'Mobility threshold where proximity-affinity heuristic accuracy degrades').

omega_variable(
    affinity_emergence_timeline,
    'Does sustained proximity actually generate genuine value alignment, or does it merely create familiarity that is mistaken for affinity?',
    'Controlled studies measuring value alignment before and after cohabitation/co-location; comparison of stated values vs revealed preferences in proximity-based vs choice-based relationships; longitudinal tracking of relationship quality metrics',
    'If proximity generates genuine affinity: rope classification confirmed — the heuristic is self-fulfilling and coordination is real. If proximity only generates familiarity: the constraint may be more extractive than measured, as agents invest in relationships based on false signals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(affinity_emergence_timeline, empirical, 'Whether proximity causes affinity or merely correlates with it').

omega_variable(
    digital_proximity_equivalence,
    'Do digital proximity mechanisms (shared online spaces, virtual communities) trigger the same affinity heuristic as physical proximity?',
    'Comparative studies of relationship formation in physical vs digital proximity contexts; neuroimaging studies of social bonding mechanisms; measurement of relationship retention and satisfaction across modalities',
    'If digital proximity is equivalent: the constraint''s coordination function extends to virtual contexts and its scope is broader than geographic. If digital proximity is not equivalent: the constraint is specific to physical co-location and may be degrading as social life moves online.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_proximity_equivalence, empirical, 'Whether digital proximity triggers the same cognitive heuristic as physical proximity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proximity_affinity_conflation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prox_aff_tr_t0, proximity_affinity_conflation, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prox_aff_tr_t50, proximity_affinity_conflation, theater_ratio, 50, 0.14).
narrative_ontology:measurement(prox_aff_tr_t100, proximity_affinity_conflation, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(prox_aff_be_t0, proximity_affinity_conflation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prox_aff_be_t50, proximity_affinity_conflation, base_extractiveness, 50, 0.17).
narrative_ontology:measurement(prox_aff_be_t100, proximity_affinity_conflation, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proximity_affinity_conflation, attachment_coordination).

% DUAL FORMULATION NOTE:
% Proximity-affinity conflation is a single constraint with stable epsilon across measurement approaches. Relationship retention rates, stated value alignment surveys, and longitudinal satisfaction studies all converge on low extractiveness (0.15-0.20 range). No decomposition needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
