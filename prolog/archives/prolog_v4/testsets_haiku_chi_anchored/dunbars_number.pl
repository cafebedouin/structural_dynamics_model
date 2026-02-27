% ============================================================================
% CONSTRAINT STORY: dunbars_number
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dunbars_number, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dunbars_number
 *   human_readable: Dunbar's Number (Cognitive Limit)
 *   domain: social/biological
 *
 * SUMMARY:
 *   Dunbar's number represents a cognitive limit on the number of stable
 *   social relationships an individual can maintain, approximately 150
 *   people. Proposed by anthropologist Robin Dunbar based on neocortex ratio
 *   analysis across primates, this constraint has become the canonical
 *   example of a natural law in social science. Unlike sociopolitical
 *   constraints that can be negotiated or overcome, Dunbar's number emerges
 *   directly from brain architecture: the neocortex's volume relative to body
 *   size determines how much social information an individual can track
 *   simultaneously. The constraint appears identically across all measured
 *   human societies — from hunter-gatherers to modern nation-states —
 *   regardless of technology, wealth, or cultural values. However, different
 *   observers experience it very differently. Small communities below 150
 *   experience it as pure coordination (rope). Nation-states experience it as
 *   an organizing principle for bureaucratic hierarchies, extracting control
 *   benefits through tier-based delegation (tangled rope). Digital platforms
 *   claim to overcome it through scale, but their scale is theatrical — users
 *   maintain dunbar-limited emotional investment despite thousands of online
 *   connections (piton). The constraint demonstrates how a single natural law
 *   can manifest as coordination in one context, extraction in another, and
 *   theater in a third, depending on the observer's structural position.
 *
 * KEY AGENTS:
 *   - Individual Human: Primary agent (powerless/trapped) — subject to neocortical limit; cannot exceed ~150 stable relationships regardless of effort
 *   - Small Community: Beneficiary (moderate/mobile) — natural group size aligns with cognitive optimum; experiences pure coordination with no extraction
 *   - Nation-State Bureaucracy: Primary beneficiary (institutional/constrained) — exploits dunbar limit to create hierarchical information aggregation; extracts control advantages
 *   - Social Media Platform: Institutional actor (powerful/arbitrage) — claims to overcome limit through digital scaling; captures value from the illusion of scaled connection
 *   - Evolutionary Biologist: Analytical observer (analytical/analytical) — recognizes constraint as emergent from neocortex ratio; immutable across deep time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dunbars_number, 0.12).
domain_priors:suppression_score(dunbars_number, 0.03).
domain_priors:theater_ratio(dunbars_number, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dunbars_number, extractiveness, 0.12).
narrative_ontology:constraint_metric(dunbars_number, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(dunbars_number, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dunbars_number, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dunbars_number, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dunbars_number, mountain).
narrative_ontology:human_readable(dunbars_number, "Dunbar's Number (Cognitive Limit)").
narrative_ontology:topic_domain(dunbars_number, "social/biological").

domain_priors:emerges_naturally(dunbars_number).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL SOCIAL AGENT (MOUNTAIN) — Every human faces the same neocortical constraint regardless of effort or resources. Cannot maintain stable relationships with more than ~150 people simultaneously due to brain architecture. This is not enforced; it emerges from cognitive limits. d≈1.00, f(d)≈1.42, σ=1.0 → χ≈0.17. Appears as an immutable law of human social cognition.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: LARGE-SCALE ORGANIZATIONS (MOUNTAIN) — Even organizations with hierarchical structure and formal communication systems experience the dunbar ceiling. Subgroups that exceed ~150-200 members experience communication breakdowns, loss of informal trust, and need for formal rules. The constraint manifests identically: d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.10. Organizational structure cannot overcome the underlying cognitive limit.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / EVOLUTIONARY BIOLOGY (MOUNTAIN) — From civilizational timescale, Dunbar's number reflects a hard constraint on primate neocortex size relative to group size, refined over millions of years of human evolution. The number (~150) holds across all measured human societies regardless of technology, wealth, or cultural practices. No civilization has overcome it; all hierarchies and scaling mechanisms work around it rather than through it. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14. This is a law of human neurobiology as immutable as physical laws.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: SMALL COMMUNITY / PRE-INDUSTRIAL (ROPE) — In settings where community size naturally clusters below 150 (villages, clans, bands), Dunbar's number functions as pure coordination: the cognitive limit aligns with the coordination optimum. No extraction occurs because the limit IS the optimal group size for maintaining trust through direct relationships. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.06. Pure coordination with no asymmetric benefits.
constraint_indexing:constraint_classification(dunbars_number, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: NATION-STATE / LARGE-SCALE BUREAUCRACY (TANGLED ROPE) — States exploit Dunbar's cognitive limit by creating hierarchical tiers (villages → districts → provinces → national) that organize humans into groups capped at ~150-200 per tier. This creates a genuine coordination function (organizing scale) BUT also asymmetric extraction: the state captures information aggregation advantages, tax collection efficiency, and control over trust-network bottlenecks. Requires active enforcement (rules, bureaucrats). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.09. Mixed coordination + extraction.
constraint_indexing:constraint_classification(dunbars_number, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SOCIAL MEDIA / DIGITAL SCALING ILLUSION (PITON) — Platforms claim to overcome Dunbar's limit through digital connections (follower counts, friend lists can exceed 1000+). But theater ratio ≈0.78: these digital connections are hollow. Users maintain stable emotional investment in ~150 people even with thousands of followers. The theatrical performance of scale (broadcast presence, notification metrics) masks the cognitive reality that attention and trust remain dunbar-limited. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.06. Degraded institution maintaining the illusion of scaled social connection.
constraint_indexing:constraint_classification(dunbars_number, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: NEUROBIOLOGY / NEOCORTEX RATIO (MOUNTAIN) — At the analytical civilizational level, Dunbar's number emerges directly from neocortex volume relative to body size. The ratio (neocortex ratio ≈ 4.1 for humans) scales with group size across all primates. Humans cannot evolve larger neocortices without massive developmental and birth costs. The constraint is not enforced; it emerges from evolutionary trade-offs baked into human physiology. This is as immutable as the speed of light. ε≈0.08, suppression≈0.02, accessibility_collapse≈0.95, resistance≈0.05. Perfect natural law signature.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dunbars_number_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dunbars_number, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dunbars_number, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(dunbars_number, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dunbars_number, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dunbars_number, ExtMetricName, E),
    domain_priors:suppression_score(dunbars_number, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dunbars_number),
    narrative_ontology:constraint_metric(dunbars_number, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dunbars_number, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(piton_threshold) :-
    domain_priors:theater_ratio(dunbars_number, TR),
    TR >= 0.70.

:- end_tests(dunbars_number_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. Dunbar's number is not primarily an extraction mechanism; it is a cognitive limit that emerges from brain architecture. The moderate value reflects that some institutional actors (states, platforms) can extract secondary benefits by exploiting knowledge of the limit, but the primary constraint is structural, not economic. Suppression (0.03): Negligible. The constraint is not maintained through coercion; it emerges naturally from neurobiology. Individuals cannot suppress the cognitive limit through effort or policy. Theater ratio (0.15): Low. The constraint has minimal theatrical content — it is what it is, a hard neurobiological fact. The only theater appears in digital platforms that claim to overcome it. Accessibility collapse (0.92): Very high. Alternative group organizations (digital connection, institutional hierarchy, broadcasting) cannot fundamentally change the underlying cognitive limit. The accessibility to escape dunbar's number is nearly zero. Resistance (0.08): Low. The constraint is not resisted because resistance is impossible — no known intervention can increase neocortex capacity in adulthood or reduce the social information processing load. This low resistance confirms the mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is not disagreement about whether the constraint exists, but about whether it is a problem or a feature. The individual social agent and evolutionary biologist both see it as immutable natural law (mountain). The small community below 150 sees it as the optimal group size for coordination (rope). The nation-state sees it as an organizing principle to manage scale through hierarchy (tangled rope). The digital platform sees it as a limitation to overcome through technology (piton, because the overcome attempt fails). The different perspectives reveal that dunbar's number is not intrinsically good or bad — it simply IS. The evaluation of whether it constrains or enables depends entirely on the observer's structural position and goals.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual social agent: Trapped + no exit → d≈1.00, f(d)≈1.42. No choice but to live within the constraint. Small community: Mobile + no extraction → d≈0.50, f(d)≈0.65. Symmetric cost/benefit; the limit IS the benefit. Nation-state: Constrained + institutional → d≈0.55, f(d)≈0.75. Mixed experience: the limit enables hierarchical organization (beneficial) but also constrains direct control (costly). Social media platform: Arbitrage + powerful → d≈0.20, f(d)≈0.08. Tries to transcend the limit; benefits from appearing to overcome it even though the appearance is theatrical. Evolutionary biologist: Analytical → d≈0.72, f(d)≈1.15. Observer position neutral; sees the constraint as a fact of biology, not as extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. Dunbar's number resolves mandatrophy by being a pure mountain — a natural law with zero degrees of freedom. There is no risk of mislabeling it as a snare (extraction masquerading as limit) because the mechanism is neurobiological, not economic. There is no risk of mislabeling it as mere coordination because it is not negotiable. The piton perspective (social media claiming to overcome it) is correctly identified as theatrical — the platform performs scale-overcoming without achieving it. The tangled rope perspective (nation-state hierarchy) correctly identifies mixed coordination + extraction. The constraint's true type is invariant across all these perspectives because ε and suppression values are so low (0.12 and 0.03) that no amount of power-level variation can change the classification. The mountain type is robust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dunbar_number_exact_value,
    'Is Dunbar''s number exactly 150, or does it vary by individual cognitive capacity and cultural context?',
    'Longitudinal social network analysis across diverse populations (hunter-gatherers, agricultural societies, industrial societies); measurement of maximum stable relationship count as function of neocortex size; correlation between reported group size and cognitive load metrics',
    'If exactly 150: immutable natural law across all humans. If varies by neocortex size: individual variation within species-level constraint. If varies by cultural context: partially culturally constructed (would require decomposition into separate constraints).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dunbar_number_exact_value, empirical, 'Precision and universality of Dunbar''s number across populations').

omega_variable(
    digital_connection_substitution,
    'Can digital communication create stable social bonds at scales exceeding Dunbar''s cognitive limit, or does online presence mask unchanged underlying dunbar-limited relationships?',
    'Measurement of emotional investment and reciprocal obligation in digital-only relationships; comparison of online friend/follower count to reported close relationships; analysis of whether digital communication increases or substitutes for face-to-face dunbar-network maintenance',
    'If substitution fails: dunbar remains a hard limit; digital platforms are pure theater (piton). If substitution succeeds: dunbar''s cognitive limit could be partially overcome (constraint would shift from mountain to rope/tangled_rope). If partial: some digital bonds form but at reduced stability (constraint remains mountain but with nuanced perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_connection_substitution, empirical, 'Whether digital communication can overcome dunbar cognitive limit').

omega_variable(
    neocortex_ratio_causal_mechanism,
    'Does neocortex ratio CAUSE group size limits, or is the correlation merely correlational and the true cause is something else (e.g., energy budget, reproductive strategy)?',
    'Comparative anatomy of extinct hominins; computational modeling of information integration capacity vs group size; evolution of neocortex ratio in domesticated vs wild populations; cross-species validation in non-primate social animals',
    'If causal: dunbar''s number is a direct consequence of neocortex architecture (mountain). If correlational: the true constraint might be energy budget or reproductive timing; dunbar number would be a symptom of a deeper constraint (would require decomposition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neocortex_ratio_causal_mechanism, empirical, 'Whether neocortex ratio causally determines group size limits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dunbars_number, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dunbar_tr_t0, dunbars_number, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dunbar_tr_t50, dunbars_number, theater_ratio, 50, 0.15).
narrative_ontology:measurement(dunbar_tr_t100, dunbars_number, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(dunbar_be_t0, dunbars_number, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(dunbar_be_t50, dunbars_number, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(dunbar_be_t100, dunbars_number, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dunbars_number, information_standard).
narrative_ontology:affects_constraint(dunbars_number, organizational_scaling_limit).
narrative_ontology:affects_constraint(dunbars_number, bureaucratic_hierarchy_formation).
narrative_ontology:affects_constraint(dunbars_number, digital_connection_authenticity).

% DUAL FORMULATION NOTE:
% Dunbar's number is a foundational biological constraint that affects the structure of all larger social constraints. Related constraints (organizational scaling, bureaucratic hierarchy) depend on dunbar's cognitive limit but have their own ε values reflecting the institutional/economic arrangements built atop the biological substrate. This is the upstream constraint in the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
