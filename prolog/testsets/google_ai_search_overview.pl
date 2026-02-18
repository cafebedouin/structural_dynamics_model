% ============================================================================
% CONSTRAINT STORY: google_ai_search_overview
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-20
% ============================================================================

:- module(constraint_google_ai_search_overview, []).

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
 *   constraint_id: google_ai_search_overview
 *   human_readable: Google's AI-Powered Search Summary Layer (Gemini)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Google is integrating generative AI summaries ("AI Overviews") at the top
 *   of its search results page. This provides users with a direct, synthesized
 *   answer, but it abstracts away the original source material (news sites,
 *   blogs, publishers). This fundamentally alters the flow of traffic and
 *   value on the open web, shifting it from content creators to the search platform.
 *
 * KEY AGENTS (by structural relationship):
 *   - Google LLC: Primary beneficiary (institutional/arbitrage) — benefits from increased user retention and control over the information interface.
 *   - Online Publishers & Content Creators: Primary target (organized/constrained) — bear the cost of lost referral traffic and ad revenue.
 *   - General Search Users: Secondary target (powerless/constrained) — gain convenience but lose source transparency and are exposed to AI-generated errors.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(google_ai_search_overview, 0.55).
domain_priors:suppression_score(google_ai_search_overview, 0.85).   % Structural property (raw, unscaled). High due to Google's market dominance.
domain_priors:theater_ratio(google_ai_search_overview, 0.10).       % Piton detection (>= 0.70). This is a functional, not theatrical, change.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(google_ai_search_overview, extractiveness, 0.55).
narrative_ontology:constraint_metric(google_ai_search_overview, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(google_ai_search_overview, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(google_ai_search_overview, tangled_rope).
narrative_ontology:human_readable(google_ai_search_overview, "Google's AI-Powered Search Summary Layer (Gemini)").
narrative_ontology:topic_domain(google_ai_search_overview, "technological/economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(google_ai_search_overview). % The AI layer is an actively maintained algorithmic choice.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(google_ai_search_overview, google_llc).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(google_ai_search_overview, online_publishers).
narrative_ontology:constraint_victim(google_ai_search_overview, general_search_users).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (ONLINE PUBLISHERS)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + constrained exit → d ≈ 0.85 → f(d) ≈ 1.15 → high χ
% This is a classic Snare from their perspective.
constraint_indexing:constraint_classification(google_ai_search_overview, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (GOOGLE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% From Google's institutional view, it's a pure coordination improvement.
constraint_indexing:constraint_classification(google_ai_search_overview, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees both the coordination function and the
% asymmetric extraction. Required for Tangled Rope detection.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(google_ai_search_overview, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: GENERAL SEARCH USER
% Experiences convenience but loses source diversity and agency, becoming
% dependent on a single, potentially flawed AI-generated answer.
% Engine derives d from: victim membership + constrained exit -> high χ
constraint_indexing:constraint_classification(google_ai_search_overview, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(google_ai_search_overview_tests).

test(perspectival_gap_beneficiary_victim, [nondet]) :-
    % Verify the core perspectival gap between the institutional beneficiary and the organized victim.
    constraint_indexing:constraint_classification(google_ai_search_overview, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(google_ai_search_overview, snare, context(agent_power(organized), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical observer must see the full structure as a Tangled Rope.
    constraint_indexing:constraint_classification(google_ai_search_overview, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(google_ai_search_overview, _),
    narrative_ontology:constraint_victim(google_ai_search_overview, _),
    domain_priors:requires_active_enforcement(google_ai_search_overview).

:- end_tests(google_ai_search_overview_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. The system directly appropriates the value of publisher-created content by summarizing it, thereby capturing the user's attention and preventing the click-through that publishers rely on for revenue. This is a significant value transfer.
 *   - Suppression (0.85): Very high. Google's dominance in search (~90% market share) means publishers have no meaningful alternative for referral traffic. Opting out is not a viable strategy for most. This lack of exit options is the primary enforcement mechanism.
 *   - The analytical classification is Tangled Rope because the constraint possesses both a genuine coordination function (providing users with fast, convenient answers) and a highly extractive, asymmetric component (defunding the open web's content layer).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Google (beneficiary) sees this as a Rope—a technical innovation to better coordinate user queries with information, improving their product. Publishers (victims) experience it as a Snare—a coercive system that extracts their primary asset (content) without fair compensation, threatening their existence. The user sees a convenience that masks a loss of agency and source diversity, also qualifying as a Snare over a biographical timescale.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `google_llc`. Google's institutional power and arbitrage exit options (they can change the algorithm at will) give them a very low directionality `d`, resulting in a negative effective extraction (`χ`). The constraint serves their interests.
 *   - Victim: `online_publishers` and `general_search_users`. As the primary targets of the value extraction and loss of agency, their `constrained` exit options and victim status yield a high `d`, resulting in a high `χ`. The constraint extracts from them.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It is not a pure Snare, because the AI summaries provide a real, albeit costly, coordination benefit to users. It is also not a pure Rope, because this coordination is achieved through a deeply asymmetric extraction from publishers. The Tangled Rope classification captures this dual nature, identifying a system where a coordination claim is used to legitimize an extractive architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_google_ai_search_overview,
    'Will the long-term degradation of source quality (as publishers lose funding) undermine the utility of the AI summaries themselves, creating a negative feedback loop?',
    'Longitudinal analysis of AI Overview quality vs. publisher health metrics over a 5-10 year period.',
    'If true, the constraint is self-defeating and may transition to a Piton. If false, it represents a stable, new extractive equilibrium.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(google_ai_search_overview, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a new constraint, so we model its projected lifecycle drift.
% It starts with a lower perceived extraction during its rollout phase and
% intensifies as it becomes the default search experience.

% Theater ratio over time (starts higher during PR rollout, then drops):
narrative_ontology:measurement(google_ai_search_overview_tr_t0, google_ai_search_overview, theater_ratio, 0, 0.20).
narrative_ontology:measurement(google_ai_search_overview_tr_t5, google_ai_search_overview, theater_ratio, 5, 0.15).
narrative_ontology:measurement(google_ai_search_overview_tr_t10, google_ai_search_overview, theater_ratio, 10, 0.10).

% Extraction over time (increases as the system displaces traditional links):
narrative_ontology:measurement(google_ai_search_overview_ex_t0, google_ai_search_overview, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(google_ai_search_overview_ex_t5, google_ai_search_overview, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(google_ai_search_overview_ex_t10, google_ai_search_overview, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It establishes a new default method for information retrieval.
narrative_ontology:coordination_type(google_ai_search_overview, information_standard).

% Network relationships: This change structurally impacts the entire digital publishing ecosystem.
narrative_ontology:affects_constraint(google_ai_search_overview, publisher_ad_revenue_model).
narrative_ontology:affects_constraint(large_language_model_data_scraping, google_ai_search_overview).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships between Google, publishers, and users.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */