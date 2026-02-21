% ============================================================================
% CONSTRAINT STORY: fb_creator_monetization_indonesia
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_fb_creator_monetization_indonesia, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fb_creator_monetization_indonesia
 *   human_readable: Facebook/Meta's "Performance Bonus" Monetization Program for Indonesian Creators
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Meta's "Performance Bonus" program offers Indonesian content creators a
 *   pathway to monetization based on content engagement. However, the system is
 *   characterized by opaque rules, unpredictable payouts, and unilateral
 *   changes by the platform. This creates a dynamic where creators invest
 *   significant labor for uncertain, often minimal, returns, while the platform
 *   benefits from a massive influx of engagement-driving content.
 *
 * KEY AGENTS (by structural relationship):
 *   - Indonesian Creators: Primary target (powerless/trapped) — bear the cost of content production for unpredictable and often low rewards.
 *   - Meta Platform: Primary beneficiary (institutional/arbitrage) — benefits from the vast quantity of user-generated content that drives platform engagement and ad revenue.
 *   - Analytical Observer: Analytical perspective — sees both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fb_creator_monetization_indonesia, 0.68). % High extraction of value (content/engagement) vs. payout.
domain_priors:suppression_score(fb_creator_monetization_indonesia, 0.75).   % High due to network effects; difficult for creators to move audiences.
domain_priors:theater_ratio(fb_creator_monetization_indonesia, 0.40).       % Significant performance of "supporting creators" but not pure theater.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, extractiveness, 0.68).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fb_creator_monetization_indonesia, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(fb_creator_monetization_indonesia). % Algorithmic management, policy updates, payment processing.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, meta_platform).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, indonesian_creators).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (INDONESIAN CREATOR)
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.68 * 1.42 * 1.0 (national) ≈ 0.96. This is well into Snare territory (>= 0.66).
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (META PLATFORM)
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.68 * -0.12 * 1.2 (global) ≈ -0.10. A negative chi indicates a subsidy. This is a Rope.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.68 * 1.15 * 1.2 (global) ≈ 0.94.
% With ε=0.68, suppression=0.75, and a valid coordination function (beneficiary declared),
% this meets the Tangled Rope criteria (0.40 ≤ χ ≤ 0.90 is a soft guide; the structural gates matter more).
% The high χ value pushes it to the upper bound, reflecting its highly extractive nature.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fb_creator_monetization_indonesia_tests).

test(perspectival_gap_creator_vs_platform) :-
    % Verify the creator sees a Snare while the platform sees a Rope.
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_met) :-
    % Verify that the structural requirements for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, _),
    narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, _),
    domain_priors:requires_active_enforcement(fb_creator_monetization_indonesia).

:- end_tests(fb_creator_monetization_indonesia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): High. The value creators provide in terms of
 *     engagement, content, and data for the platform far exceeds the unreliable
 *     and often minimal payouts they receive.
 *   - Suppression (0.75): High. Facebook/Instagram's powerful network effects
 *     create high switching costs. Creators cannot easily port their audience
 *     to a rival platform, making them dependent on Meta's ecosystem.
 *   - This constraint is a canonical Tangled Rope because it possesses both a
 *     genuine coordination function (it provides a mechanism, however flawed, for
 *     monetization) and a powerful, asymmetric extraction mechanism (the opaque,
 *     unilateral control over that mechanism).
 *
 * PERSPECTIVAL GAP:
 *   - For an Indonesian creator (powerless, trapped), the system is a Snare.
 *     They are lured by the promise of income but are trapped by opaque rules
 *     and high exit costs, with their labor being extracted for uncertain gain.
 *     The high derived directionality (d≈0.95) leads to a very high effective
 *     extraction (χ), confirming the Snare classification.
 *   - For Meta (institutional, arbitrage), the system is a Rope. It's a highly
 *     efficient tool for coordinating content generation at a massive scale,
 *     subsidizing its own operations with creator labor. The low derived
 *     directionality (d≈0.05) results in a negative effective extraction (χ),
 *     meaning the constraint benefits them directly.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `meta_platform`. The platform gains the primary benefit: a
 *     constant stream of low-cost, high-engagement content that powers its ad
 *     revenue model.
 *   - Victim: `indonesian_creators`. They bear the costs of production, time, and
 *     creative labor, while being subject to the platform's algorithmic whims and
 *     unpredictable payment schedules. The system extracts this value from them.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids two potential errors.
 *   It is not a pure Snare, because it does solve a real coordination problem
 *   (connecting creators to a monetization stream). Labeling it a Snare would
 *   ignore this function. It is not a pure Rope, because the terms are grossly
 *   asymmetric and extractive. The Tangled Rope classification captures this
 *   hybrid nature: a coordination tool that has been weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_fb_creator_monetization_indonesia,
    'Is the opacity of the bonus program a deliberate design choice to maximize extraction, or is it an unavoidable byproduct of managing a complex global algorithmic system?',
    'Internal Meta documentation on the design philosophy and goals of the program, or A/B testing data showing how transparency affects creator output vs. platform revenue.',
    'If deliberate, it confirms a highly cynical extractive strategy. If a byproduct, it suggests a problem of institutional incompetence or scale-out-of-control, which is still extractive but not by malign design.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_fb_creator_monetization_indonesia, empirical, 'Whether the system`s extractive opacity is by design or emergent complexity.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fb_creator_monetization_indonesia, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.68 > 0.46), so temporal data is required.
% This models the likely trajectory: the program started as more generous and
% transparent to attract creators, then became more extractive and opaque over time
% as creators became locked into the platform (extraction_accumulation).

% Theater ratio over time (the narrative of "supporting creators" grew):
narrative_ontology:measurement(fb_creator_monetization_indonesia_tr_t0, fb_creator_monetization_indonesia, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fb_creator_monetization_indonesia_tr_t5, fb_creator_monetization_indonesia, theater_ratio, 5, 0.25).
narrative_ontology:measurement(fb_creator_monetization_indonesia_tr_t10, fb_creator_monetization_indonesia, theater_ratio, 10, 0.40).

% Extraction over time (the terms became less favorable for creators):
narrative_ontology:measurement(fb_creator_monetization_indonesia_ex_t0, fb_creator_monetization_indonesia, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fb_creator_monetization_indonesia_ex_t5, fb_creator_monetization_indonesia, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(fb_creator_monetization_indonesia_ex_t10, fb_creator_monetization_indonesia, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The program is a mechanism for allocating bonus payments.
narrative_ontology:coordination_type(fb_creator_monetization_indonesia, resource_allocation).

% Network relationships (structural influence edges)
% The monetization system is structurally dependent on the content ranking algorithm.
% A change in one directly impacts the function of the other.
narrative_ontology:affects_constraint(platform_algorithmic_feed_ranking, fb_creator_monetization_indonesia).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationship between the platform and its creators.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */