% ============================================================================
% CONSTRAINT STORY: frame_mismatch_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_frame_mismatch_friction, []).

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
 *   constraint_id: frame_mismatch_friction
 *   human_readable: Frame Mismatch Friction in LLM Interface Design
 *   domain: human_computer_interaction/cognitive_ergonomics
 *
 * SUMMARY:
 *   LLM interfaces default to a messaging-app surface optimized for casual,
 *   ephemeral queries: single-turn or short-session interactions with minimal
 *   state persistence, no role differentiation, and implicit temporal
 *   context. This design choice maximizes accessibility and funnel width —
 *   the interface is familiar, low-friction, and serves the largest user
 *   segment effectively. However, a subset of users attempts
 *   capability-extension work through the same interface: stateful workflows
 *   requiring persistent context, role-differentiated interaction (user as
 *   orchestrator, LLM as specialized tool), and explicit temporal sequencing.
 *   The frame mismatch produces friction: context loss across sessions, role
 *   confusion when the LLM defaults to conversational assistant mode rather
 *   than tool mode, and cognitive overhead from manual state management.
 *   Providers maintain the simple surface to preserve funnel width while
 *   offering alternative interfaces (Claude Code, Projects, custom GPTs) and
 *   API access for power users. The constraint exhibits tangled rope
 *   structure: genuine coordination (wide accessibility) coexists with
 *   asymmetric extraction (cognitive overhead for capability-extension users
 *   lacking scaffolding). The theater ratio (0.38) reflects moderate
 *   performative content: interface updates often address surface aesthetics
 *   or conversational polish rather than structural support for stateful
 *   interaction. The open-source agent framework community is building
 *   alternative pathways with explicit state/role/time primitives, suggesting
 *   a scaffold dynamic with potential sunset as these tools mature.
 *
 * KEY AGENTS:
 *   - Capability Extension Users: Primary victim (powerless/constrained) — attempting stateful, persistent, role-differentiated work through an interface designed for ephemeral queries; experience frame mismatch as unreliability and cognitive overhead
 *   - Workflow Integration Developers: Secondary victim (moderate/mobile) — building on API or developing agent frameworks; benefit from wide funnel but bear extraction through interface instability and missing primitives
 *   - Providers Maintaining Wide Funnel: Primary beneficiary (institutional/arbitrage) — messaging-app surface maximizes accessibility and user acquisition; frame mismatch for power users is acceptable trade-off for funnel width
 *   - Casual Query Users: Secondary beneficiary (powerless/mobile) — interface serves their use case effectively; no frame mismatch because mental model aligns with affordances
 *   - Open-Source Agent Framework Community: Organized agents (organized/mobile) — building alternative interfaces with explicit state/role/time support; see frame mismatch as temporary coordination failure with sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes irreducible hybrid of coordination (accessibility) and extraction (cognitive overhead for capability-extension users)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(frame_mismatch_friction, 0.48).
domain_priors:suppression_score(frame_mismatch_friction, 0.52).
domain_priors:theater_ratio(frame_mismatch_friction, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(frame_mismatch_friction, extractiveness, 0.48).
narrative_ontology:constraint_metric(frame_mismatch_friction, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(frame_mismatch_friction, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(frame_mismatch_friction, tangled_rope).
narrative_ontology:human_readable(frame_mismatch_friction, "Frame Mismatch Friction in LLM Interface Design").
narrative_ontology:topic_domain(frame_mismatch_friction, "human_computer_interaction/cognitive_ergonomics").

domain_priors:requires_active_enforcement(frame_mismatch_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(frame_mismatch_friction, providers_maintaining_wide_funnel).
narrative_ontology:constraint_beneficiary(frame_mismatch_friction, casual_query_users).
narrative_ontology:constraint_victim(frame_mismatch_friction, capability_extension_users_lacking_scaffolding).
narrative_ontology:constraint_victim(frame_mismatch_friction, workflow_integration_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPABILITY EXTENSION USER (SNARE) — Attempting stateful, persistent, role-differentiated interaction through an interface designed for ephemeral queries. Experiences the mismatch as unreliability: context loss, role confusion, session collapse. Exit options are constrained rather than trapped — alternative interfaces exist (Claude Code, agent frameworks) but require technical sophistication and workflow migration costs. High extraction: the interface design extracts cognitive overhead (manual state management, repeated context provision, workaround development) while providing minimal scaffolding for the intended use case.
constraint_indexing:constraint_classification(frame_mismatch_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKFLOW INTEGRATION DEVELOPER (TANGLED ROPE) — Building on the API or developing agent frameworks. Benefits from the wide user funnel (large market, diverse use cases to learn from) but bears extraction through interface instability: the messaging-app surface changes frequently, breaking integrations; stateful patterns require reverse-engineering undocumented behavior; role differentiation must be implemented client-side. Mixed experience: genuine coordination (API access, model capability) alongside asymmetric extraction (interface churn, missing primitives for state/role/time). Mobile exit: can switch providers or build alternative interfaces, but at significant migration cost.
constraint_indexing:constraint_classification(frame_mismatch_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: PROVIDER MAINTAINING WIDE FUNNEL (ROPE) — The messaging-app interface defaults maximize accessibility: low barrier to entry, familiar interaction pattern, minimal cognitive load for casual queries. From the provider's perspective, this is pure coordination: serving the largest user base with the simplest viable interface. The frame mismatch for capability-extension users is an acceptable trade-off to maintain funnel width. Arbitrage exit: providers can experiment with alternative interfaces (Code, Projects, custom GPTs) without abandoning the core messaging surface. Net beneficiary: extraction flows toward this agent through user lock-in and data accumulation, not away from them.
constraint_indexing:constraint_classification(frame_mismatch_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CASUAL QUERY USER (ROPE) — Using the interface for its designed purpose: ephemeral, stateless, single-turn or short-session queries. Experiences the constraint as coordination: the messaging-app surface is familiar, low-friction, and well-suited to their use case. No frame mismatch because their mental model aligns with the interface affordances. Mobile exit: can switch to alternative AI assistants with minimal cost. Low extraction: the interface serves their needs without imposing significant overhead.
constraint_indexing:constraint_classification(frame_mismatch_friction, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: OPEN-SOURCE AGENT FRAMEWORK COMMUNITY (SCAFFOLD) — Building alternative interfaces (LangChain, AutoGPT, agent orchestration layers) that provide explicit state management, role differentiation, and temporal persistence. Sees the frame mismatch as a temporary coordination failure with a sunset: as agent frameworks mature and providers adopt richer interface primitives (function calling, structured outputs, memory APIs), the messaging-app surface will either evolve to support stateful interaction or be bypassed by specialized tooling. Organized agents with mobile exit: can build on any provider's API and migrate between backends. Low effective extraction because the coalition has agency and sees a clear path to resolution.
constraint_indexing:constraint_classification(frame_mismatch_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes the frame mismatch as a structural consequence of interface design choices optimizing for different use cases. The messaging-app surface genuinely coordinates casual query access (rope function) while simultaneously extracting cognitive overhead from capability-extension users who lack scaffolding (snare function). The constraint is not purely extractive — the wide funnel serves a real coordination purpose — but the asymmetry is structural: casual users benefit from simplicity; capability-extension users bear the cost of missing primitives. Tangled rope classification reflects the irreducible hybrid: both coordination and extraction are present, and neither can be eliminated without sacrificing the other.
constraint_indexing:constraint_classification(frame_mismatch_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frame_mismatch_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(frame_mismatch_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(frame_mismatch_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(frame_mismatch_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(frame_mismatch_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Capability-extension users bear significant cognitive overhead from manual state management, repeated context provision, and workaround development. The extraction is not as severe as pure snares because alternative interfaces exist (constrained exit rather than trapped), and some users successfully adapt their workflows. The value reflects real but bounded extraction: the interface design imposes costs on a user segment while serving another segment effectively. Suppression (0.52): Moderate. Barriers to stateful interaction include lack of native state persistence, implicit role defaults (conversational assistant rather than tool), session length limits, and context window constraints. However, suppression is not total: users can adopt alternative interfaces (Code, agent frameworks), use API access, or develop workarounds (external state management, prompt engineering). The suppression is structural (missing interface primitives) rather than coercive (active prevention). Theater ratio (0.38): Moderate. Interface updates often prioritize conversational polish, aesthetic refinement, and feature announcements over structural support for stateful interaction. Some theater is present (marketing emphasis on capabilities that require scaffolding the interface doesn't provide), but the ratio is lower than pure pitons because the messaging surface genuinely serves its designed use case (casual queries). The theater has increased over the interval as the gap between marketed capabilities and interface affordances has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how interface design choices create perspectival divergence across user segments. Casual query users experience pure coordination (Rope) — the messaging-app surface is well-suited to their use case, familiar, and low-friction. Providers see coordination (Rope) — the simple interface maximizes accessibility and serves the largest user base. Capability-extension users experience extraction (Snare) — the frame mismatch imposes cognitive overhead and unreliability for stateful workflows. Workflow integration developers experience mixed coordination and extraction (Tangled Rope) — they benefit from the wide funnel but bear costs from interface instability and missing primitives. The open-source agent framework community sees a temporary problem with a sunset (Scaffold) — alternative interfaces are maturing and will either replace or augment the messaging surface. The analytical observer recognizes the irreducible hybrid (Tangled Rope) — both coordination and extraction are structural features of the design choice, and neither can be eliminated without sacrificing the other. The perspectival gap reveals that 'unreliability' complaints from capability-extension users are not about model capability but about interface affordances: the same system appears reliable to casual users and unreliable to power users because their mental models and use cases differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Capability-extension users are victims with constrained exit options. The engine derives high directionality (d ≈ 0.85) from victim status + constrained exit, producing high effective extraction. They bear the cognitive overhead of frame mismatch without adequate scaffolding. Workflow integration developers are victims with mobile exit options. The engine derives moderate-high directionality (d ≈ 0.65) from victim status + mobile exit, producing moderate effective extraction. They bear interface instability and missing primitives but can migrate to alternative providers or build their own tooling. Providers maintaining wide funnel are beneficiaries with arbitrage exit options. The engine derives low directionality (d ≈ 0.05) from beneficiary status + arbitrage exit, producing negative effective extraction. They capture user lock-in, data accumulation, and market share through the accessible interface. Casual query users are beneficiaries with mobile exit options. The engine derives low directionality (d ≈ 0.15) from beneficiary status + mobile exit, producing near-zero effective extraction. The interface serves their needs effectively. The open-source agent framework community are organized agents with mobile exit options and mixed beneficiary/victim status (benefit from wide funnel, bear extraction through missing primitives). The engine derives moderate directionality (d ≈ 0.50) from mixed status + mobile exit, producing moderate effective extraction. The analytical observer uses canonical analytical directionality (d ≈ 0.72), producing moderate-high effective extraction, reflecting the structural visibility of both coordination and extraction functions.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by recognizing that the frame mismatch serves a genuine coordination function (accessibility for casual users) while simultaneously extracting from capability-extension users (cognitive overhead from missing scaffolding). This is not a rope misclassified as a snare, nor a snare misclassified as a rope — it is an irreducible hybrid where both functions coexist. The coordination function is real: the messaging-app surface lowers barriers to entry and serves the largest user segment effectively. The extraction function is also real: capability-extension users bear costs (manual state management, context re-provision, workaround development) that could be reduced with richer interface primitives. The asymmetry is structural: providers optimize for funnel width (casual users) at the expense of power users, who lack the market power to demand interface changes. The scaffold perspective (open-source agent frameworks) suggests a potential resolution pathway: as alternative interfaces mature, the frame mismatch may sunset, either through provider adoption of richer primitives or through user migration to specialized tooling. The tangled rope classification captures the current state: both coordination and extraction are present, and the constraint's resolution depends on whether the interface evolution trajectory converges (scaffold) or diverges (persistent tangled rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interface_evolution_trajectory,
    'Will LLM interfaces converge toward richer state/role/time primitives, or will stateful interaction remain the domain of specialized agent frameworks?',
    'Longitudinal tracking of provider interface updates; adoption rates of Projects, Code, custom GPTs vs continued reliance on messaging surface; market share of agent frameworks vs direct provider interfaces',
    'If convergence: scaffold perspective confirmed — the frame mismatch is temporary. If divergence: tangled rope persists — providers maintain simple surface for funnel width while power users migrate to frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interface_evolution_trajectory, empirical, 'Whether provider interfaces will natively support stateful interaction').

omega_variable(
    cognitive_overhead_quantification,
    'What is the measurable cognitive overhead imposed by manual state management, context re-provision, and workaround development for capability-extension users?',
    'User studies comparing task completion time, error rates, and subjective cognitive load between messaging-app surface and scaffolded interfaces (Code, agent frameworks) for equivalent stateful tasks',
    'If overhead < 20%: frame mismatch is minor friction, not extraction. If overhead > 50%: frame mismatch constitutes significant extraction from capability-extension users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_overhead_quantification, empirical, 'Magnitude of cognitive overhead from frame mismatch').

omega_variable(
    funnel_width_necessity,
    'Is the messaging-app surface''s simplicity necessary to maintain funnel width, or could richer primitives be introduced without sacrificing accessibility?',
    'A/B testing of interface variants with progressive disclosure of state/role/time features; adoption and retention metrics across user segments; comparison to other tools that successfully serve both casual and power users',
    'If simplicity is necessary: tangled rope is structural trade-off. If richer primitives are compatible with accessibility: current design is extractive choice, not coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funnel_width_necessity, empirical, 'Whether interface simplicity is necessary for accessibility').

omega_variable(
    alternative_interface_adoption_threshold,
    'At what point does capability-extension user migration to alternative interfaces (Code, agent frameworks) constitute exit from the constraint vs adoption of a complementary tool?',
    'Usage pattern analysis: do users abandon the messaging surface entirely, or do they use both interfaces for different tasks? Retention and session frequency metrics across interface types.',
    'If migration is exit: constrained exit option is real, and extraction is bounded. If migration is complementary: users remain locked into the messaging surface for some tasks, and extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_interface_adoption_threshold, empirical, 'Whether alternative interfaces constitute exit or complementary use').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(frame_mismatch_friction, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, frame_mismatch_friction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_midpoint, frame_mismatch_friction, theater_ratio, 12, 0.32).
narrative_ontology:measurement(theater_current, frame_mismatch_friction, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(extract_initial, frame_mismatch_friction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_midpoint, frame_mismatch_friction, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(extract_current, frame_mismatch_friction, base_extractiveness, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(frame_mismatch_friction, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of state_role_time_collapse (the mountain constraint that LLM interfaces lack native primitives for state persistence, role differentiation, and temporal sequencing). The upstream constraint describes the structural absence; this constraint describes the friction produced when users attempt capability-extension work through an interface lacking those primitives. The upstream constraint has low extractiveness (mountain — the absence is a design choice, not inherently extractive); this constraint has moderate extractiveness (tangled rope — the friction imposes real costs on a user segment while serving another segment effectively).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
