% ============================================================================
% CONSTRAINT STORY: gpt_store_marketplace
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_gpt_store_marketplace, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpt_store_marketplace
 *   human_readable: The OpenAI GPT Store Marketplace
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI's GPT Store creates a centralized marketplace for custom AI
 *   applications ("GPTs"). This structure provides a coordination function
 *   by connecting developers with users and establishing a standard for
 *   AI app creation. However, it also creates a dependency on OpenAI's
 *   proprietary models and infrastructure, allowing the platform owner to
 *   extract significant value through revenue sharing and control over the
 *   ecosystem's rules.
 *
 * KEY AGENTS (by structural relationship):
 *   - GPT Developers: Primary target (moderate/constrained) — bear extraction via revenue sharing and platform dependency, but also benefit from the market access.
 *   - OpenAI (Platform Owner): Primary beneficiary (institutional/arbitrage) — benefits from revenue, data, and ecosystem control.
 *   - End Users: Secondary target (powerless/trapped) — benefit from access to tools but are subject to the platform's mediation, data policies, and limited choice architecture.
 *   - Analytical Observer: Analytical observer — sees the full dual-function structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction represents revenue share, data capture, and dependency lock-in.
% A value of 0.48 is analogous to the 15-30% cut of traditional app stores,
% plus the implicit value of platform control.
domain_priors:base_extractiveness(gpt_store_marketplace, 0.48).

% Suppression is high due to the immense capital and research cost of creating
% a competing foundational model and ecosystem.
domain_priors:suppression_score(gpt_store_marketplace, 0.75).   % Structural property (raw, unscaled).

% At launch, the store is highly functional, not merely performative.
domain_priors:theater_ratio(gpt_store_marketplace, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpt_store_marketplace, extractiveness, 0.48).
narrative_ontology:constraint_metric(gpt_store_marketplace, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpt_store_marketplace, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
% The structure has both a genuine coordination function and asymmetric extraction.
narrative_ontology:constraint_claim(gpt_store_marketplace, tangled_rope).
narrative_ontology:human_readable(gpt_store_marketplace, "The OpenAI GPT Store Marketplace").
narrative_ontology:topic_domain(gpt_store_marketplace, "technological/economic").

% --- Binary flags ---
% The platform's rules (content moderation, revenue splits, API access)
% require active enforcement by OpenAI. This is a required flag for Tangled Rope.
domain_priors:requires_active_enforcement(gpt_store_marketplace).

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a designed, artificial construct.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(gpt_store_marketplace, openai_platform_owner).
narrative_ontology:constraint_beneficiary(gpt_store_marketplace, gpt_developers). % They gain access to a market.

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(gpt_store_marketplace, gpt_developers). % They are subject to revenue share and platform risk.
narrative_ontology:constraint_victim(gpt_store_marketplace, end_users). % Data extraction, attention capture, ecosystem lock-in.

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE GPT DEVELOPER (PRIMARY TARGET)
% As a member of the 'victim' group with 'trapped' exit (cannot port a GPT
% easily to a competing ecosystem), the derived directionality 'd' is high.
% d ≈ 0.95 → f(d) ≈ 1.42.
% High ε, high suppression, and high f(d) result in a high effective
% extraction (χ), classifying the constraint as a Snare from this view.
constraint_indexing:constraint_classification(gpt_store_marketplace, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPENAI (PRIMARY BENEFICIARY)
% As a member of the 'beneficiary' group with 'arbitrage' exit, the derived
% directionality 'd' is low.
% d ≈ 0.05 → f(d) ≈ -0.12.
% This negative multiplier makes effective extraction (χ) negative, meaning the
% platform views the constraint as a pure subsidy/coordination mechanism. This is a Rope.
constraint_indexing:constraint_classification(gpt_store_marketplace, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical perspective sees both the coordination function (beneficiaries exist)
% and the asymmetric extraction (victims exist). The high ε and suppression,
% combined with the existence of both functions, leads to the Tangled Rope
% classification, which matches the constraint_claim.
constraint_indexing:constraint_classification(gpt_store_marketplace, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SOPHISTICATED DEVELOPER / SMALL BUSINESS
% An organized developer has more power and mobility than a powerless one, but is
% still constrained by the platform. Their directionality is lower than the powerless
% developer's, reducing effective extraction. They perceive the trade-off more
% clearly and see a Tangled Rope.
constraint_indexing:constraint_classification(gpt_store_marketplace, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpt_store_marketplace_tests).

test(perspectival_gap_developer_vs_platform) :-
    % Verify the core perspectival gap between the developer and OpenAI.
    constraint_indexing:constraint_classification(gpt_store_marketplace, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(gpt_store_marketplace, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    % The analytical claim must be Tangled Rope.
    constraint_indexing:constraint_classification(gpt_store_marketplace, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gates_are_met) :-
    % Verify that all structural requirements for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(gpt_store_marketplace, _),
    narrative_ontology:constraint_victim(gpt_store_marketplace, _),
    domain_priors:requires_active_enforcement(gpt_store_marketplace).

:- end_tests(gpt_store_marketplace_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value represents the significant value capture
 *     by the platform, analogous to the 15-30% fees in mobile app stores, compounded
 *     by the strategic value of data and ecosystem control.
 *   - Suppression (0.75): This reflects the monumental barrier to entry for
 *     creating a competing foundational model and marketplace, effectively locking
 *     developers into the existing ecosystem and suppressing alternatives.
 *   - The combination of a clear coordination function (a marketplace) with high
 *     extraction and suppression makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. OpenAI (institutional beneficiary) sees a Rope: a coordination
 *   tool that enables a vibrant ecosystem and generates revenue. Their arbitrage
 *   position means the rules are a benefit, not a cost. A developer (powerless
 *   target), however, sees a Snare. They are trapped by the high cost of switching
 *   and subject to the platform's extractive terms (revenue share, risk of being
 *   de-platformed or having their functionality Sherlocked). The same set of rules
 *   is experienced as enabling from one side and coercive from the other.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `openai_platform_owner` benefits directly from revenue and control.
 *     `gpt_developers` are also listed as beneficiaries because they gain access to a
 *     vast user base and standardized tools they couldn't create alone. This is the
 *     coordination aspect of the Tangled Rope.
 *   - Victims: `gpt_developers` are simultaneously victims because they bear the cost of
 *     extraction and platform risk. This dual role is characteristic of participants
 *     in extractive two-sided markets. `end_users` are secondary victims of data
 *     extraction and choice architecture limitations.
 *   - The engine uses this dual membership of developers to calculate directionality.
 *     When indexed as powerless/trapped, their victim status dominates, leading to a
 *     high 'd' value. When indexed as organized/constrained, the d value is lower,
 *     reflecting their partial agency.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the platform. A simpler
 *   analysis might label it a pure Snare (focusing only on extraction) or a pure Rope
 *   (focusing only on market creation). The Tangled Rope classification is crucial
 *   because it acknowledges that the system's stability comes from its genuine
 *   coordination function. The extraction is parasitic upon a genuinely useful
 *   service. This prevents mislabeling the coordination as fraudulent while also
 *   refusing to ignore the asymmetric power dynamics and value capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_gpt_store_marketplace,
    'Will the platform governance evolve to be more extractive (higher take rates, more aggressive "Sherlocking") or more cooperative (lower take rates, stronger developer protections) over time?',
    'Observing OpenAI''s policy changes regarding revenue sharing, API access, and competition with third-party GPTs over a 3-5 year period.',
    'If it becomes more extractive, ε will increase, and the analytical classification could shift from Tangled Rope to Snare. If more cooperative, ε would decrease, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gpt_store_marketplace, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction (ε=0.48 > 0.46) constraint.
% This models a common platform lifecycle: initial focus on function gives
% way to increasing rent-seeking and performative governance as the market matures.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(gpt_store_tr_t0, gpt_store_marketplace, theater_ratio, 0, 0.10).
narrative_ontology:measurement(gpt_store_tr_t5, gpt_store_marketplace, theater_ratio, 5, 0.12).
narrative_ontology:measurement(gpt_store_tr_t10, gpt_store_marketplace, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(gpt_store_ex_t0, gpt_store_marketplace, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gpt_store_ex_t5, gpt_store_marketplace, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gpt_store_ex_t10, gpt_store_marketplace, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It's a two-sided market that allocates user attention
% and provides a standard for building applications.
narrative_ontology:coordination_type(gpt_store_marketplace, resource_allocation).

% Network relationships: The GPT store's existence and rules are structurally
% dependent on the underlying proprietary foundational models.
narrative_ontology:affects_constraint(foundational_llm_dependency, gpt_store_marketplace).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation based on
% beneficiary/victim declarations and exit options accurately models the
% power dynamics between the platform owner and its developers.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */