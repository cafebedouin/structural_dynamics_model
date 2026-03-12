% ============================================================================
% CONSTRAINT STORY: selective_attention_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_selective_attention_constraint, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: selective_attention_constraint
 *   human_readable: Selective Attention Constraint in Moral Relationships
 *   domain: moral_philosophy/social_psychology/economic_anthropology
 *
 * SUMMARY:
 *   The selective attention constraint describes the structural limit that
 *   quality of moral attention degrades with distribution breadth due to
 *   finite cognitive and emotional resources. This is the empirical
 *   foundation underlying Dunbar's number (approximately 150 stable
 *   relationships, with nested circles of 5 intimates, 15 close friends, 50
 *   friends, 150 meaningful contacts). The constraint is not a social norm or
 *   institutional arrangement but a property of embodied cognition:
 *   maintaining high-quality relationships requires time, emotional labor,
 *   and cognitive tracking that scale linearly with relationship count, while
 *   available resources remain fixed. Cross-cultural anthropological research
 *   shows the same nested circle structure across societies with radically
 *   different social technologies, suggesting the constraint is invariant to
 *   institutional form. The constraint is downstream of both
 *   temporal_asymmetry_of_obligation (which creates the need for relationship
 *   maintenance over time) and generosity_as_bond_mechanism (which
 *   establishes what quality attention consists of in relationship
 *   maintenance). Unlike those constraints, which have extractive or
 *   coordinative social components, selective attention is a pure cognitive
 *   limit.
 *
 * KEY AGENTS:
 *   - Individual Agent: Any person attempting to maintain relationships (powerless/trapped) — cannot escape finite cognitive resources
 *   - Social Institution: Organizations coordinating relationship maintenance (institutional/arbitrage) — face the same structural limit despite coordination capacity
 *   - Community Organizer: Agents building solidarity networks (organized/mobile) — must navigate quality-breadth tradeoff in coalition-building
 *   - Relationship Participant: Person in specific relationships (moderate/constrained) — experiences tradeoff as practical impossibility of equal deep attention
 *   - Analytical Observer: Cross-cultural researcher (analytical/analytical) — observes invariance of nested circle structure across societies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(selective_attention_constraint, 0.08).
domain_priors:suppression_score(selective_attention_constraint, 0.03).
domain_priors:theater_ratio(selective_attention_constraint, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(selective_attention_constraint, extractiveness, 0.08).
narrative_ontology:constraint_metric(selective_attention_constraint, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(selective_attention_constraint, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(selective_attention_constraint, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(selective_attention_constraint, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(selective_attention_constraint, mountain).
narrative_ontology:human_readable(selective_attention_constraint, "Selective Attention Constraint in Moral Relationships").
narrative_ontology:topic_domain(selective_attention_constraint, "moral_philosophy/social_psychology/economic_anthropology").

domain_priors:emerges_naturally(selective_attention_constraint).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL AGENT (MOUNTAIN) — Cannot escape finite cognitive and emotional resources. The constraint that attention quality degrades with distribution breadth is experienced as an unchangeable limit. No amount of effort allows maintaining 150 intimate relationships with the same depth as 5.
constraint_indexing:constraint_classification(selective_attention_constraint, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: SOCIAL INSTITUTION (MOUNTAIN) — Institutions that coordinate relationship maintenance (religious communities, professional networks, kinship systems) face the same structural limit. Technology can extend reach but cannot eliminate the quality-breadth tradeoff. The constraint is invariant across institutional forms.
constraint_indexing:constraint_classification(selective_attention_constraint, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMMUNITY ORGANIZER (MOUNTAIN) — Organized agents attempting to build solidarity movements face the constraint directly: expanding the circle of moral concern dilutes the quality of attention available to each member. Coalition-building strategies must navigate this tradeoff, not transcend it.
constraint_indexing:constraint_classification(selective_attention_constraint, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational perspective, the constraint appears as a structural feature of embodied cognition. Dunbar number research, time-budget studies, and cross-cultural relationship maintenance patterns all converge on the same limit: finite cognitive resources impose a quality-breadth tradeoff that no social technology has eliminated.
constraint_indexing:constraint_classification(selective_attention_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: RELATIONSHIP PARTICIPANT (MOUNTAIN) — At the immediate/local scale, the constraint is experienced as the practical impossibility of giving equal deep attention to all relationships. Choosing to deepen one relationship necessarily means less attention available for others. The tradeoff is felt as an unchangeable fact of social life.
constraint_indexing:constraint_classification(selective_attention_constraint, mountain,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(selective_attention_constraint_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(selective_attention_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(selective_attention_constraint, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(selective_attention_constraint, ExtMetricName, E),
    domain_priors:suppression_score(selective_attention_constraint, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(selective_attention_constraint),
    narrative_ontology:constraint_metric(selective_attention_constraint, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(selective_attention_constraint, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(selective_attention_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes costs (cannot maintain unlimited deep relationships) but these costs are not asymmetrically distributed — everyone faces the same limit. The small non-zero value reflects that relationship maintenance does involve resource expenditure (time, emotional labor), but this is coordination cost rather than extraction. Suppression (0.03): Minimal. The constraint does not suppress alternatives through coercion — it is a structural limit of embodied cognition. The small non-zero value reflects that the limit does constrain choice (cannot choose to maintain 500 intimate relationships), but this is not suppression in the extractive sense. Theater ratio (0.12): Very low. There is minimal performative content. Social norms about relationship obligations exist, but the underlying constraint (finite cognitive resources) is not theatrical. The small non-zero value reflects that some relationship maintenance rituals are performative, but the core constraint is functional. Accessibility collapse (0.92): Very high. The constraint is highly accessible to direct observation — people immediately recognize they cannot maintain unlimited deep relationships. Time-budget studies and Dunbar number research make the limit empirically measurable. Resistance (0.08): Very low. The constraint shows minimal resistance to observation or measurement. Cross-cultural studies consistently find the same nested circle structure. No social technology has eliminated the quality-breadth tradeoff, suggesting the constraint is robust.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all five perspectives classify as mountain. The unanimity reflects that the constraint is a genuine natural law: a structural property of embodied cognition that is invariant across power levels, time horizons, exit options, and spatial scopes. The individual agent with no exit options and the institutional actor with arbitrage capacity face the same limit. The immediate/local perspective and the civilizational/universal perspective observe the same phenomenon. This is the signature of a true mountain: classification invariance across all indexical positions. The constraint is downstream of social constraints (temporal_asymmetry_of_obligation, generosity_as_bond_mechanism) that do show perspectival gaps, but selective attention itself is the cognitive bedrock those social constraints rest on.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims — the structural limit applies universally. All perspectives derive d from their power atom's canonical fallback value, but because the constraint is mountain-classified from all perspectives, the effective extraction chi is negligible regardless of d. The constraint does not extract asymmetrically; it imposes a symmetric limit on all agents. The individual agent (powerless/trapped) and the relationship participant (moderate/constrained) experience the constraint as an unchangeable personal limit. The social institution (institutional/arbitrage) and community organizer (organized/mobile) experience it as an unchangeable coordination limit. The analytical observer (analytical/analytical) sees it as an invariant structural feature of embodied cognition. No perspective sees the constraint as changeable or as extracting asymmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint demonstrates what a genuine natural law looks like in the DR framework. It passes all mountain gates: (1) very low extractiveness (ε = 0.08 ≤ 0.25), (2) very low suppression (0.03 ≤ 0.05), (3) emerges naturally without enforcement, (4) high accessibility collapse (0.92 ≥ 0.85), (5) low resistance to observation (0.08 ≤ 0.15), and (6) classification invariance across all perspectives. The constraint is not a social norm that could be changed by institutional reform, not a coordination mechanism that could be improved by better technology, and not an extraction mechanism that benefits some at the expense of others. It is a structural limit of embodied cognition. The mandatrophy resolution is straightforward: this is not coordination mislabeled as natural law (the false summit pattern) — it is an actual natural law. The Dunbar number research provides empirical grounding: the nested circle structure (5/15/50/150) appears across cultures with different social technologies, different kinship systems, and different moral frameworks. The quality-breadth tradeoff is not eliminated by social media, not bypassed by institutional coordination, and not overcome by moral philosophy. It is a constraint on what is possible for embodied cognitive agents with finite resources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(selective_attention_constraint, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is downstream of temporal_asymmetry_of_obligation and generosity_as_bond_mechanism. Those constraints describe social/moral structures that create relationship maintenance obligations and define what quality attention consists of. Selective attention describes the cognitive limit that makes those obligations costly: finite resources mean distributing attention more broadly necessarily reduces quality per relationship. The upstream constraints have extractive or coordinative components; selective attention is a pure cognitive limit with minimal extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
