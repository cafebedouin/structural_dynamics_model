% ============================================================================
% CONSTRAINT STORY: sk_newtro_aesthetic
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_sk_newtro_aesthetic, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sk_newtro_aesthetic
 *   human_readable: South Korean 'Newtro' Aesthetic Commercialization
 *   domain: social/economic
 *
 * SUMMARY:
 *   The "Newtro" (New + Retro) trend in South Korea involves the reinterpretation
 *   of traditional and vintage aesthetics by younger generations. This constraint
 *   models the market pressure that commercializes this trend, compelling
 *   producers to adopt the Newtro aesthetic to remain relevant and competitive.
 *   While it serves a coordination function between producers and consumers,
 *   it asymmetrically extracts cultural capital from original sources for
 *   the benefit of large-scale commercial entities.
 *
 * KEY AGENTS (by structural relationship):
 *   - Independent Artisans & Small Businesses: Primary target (powerless/trapped) — Their authentic crafts and aesthetics are the raw material for the trend, but they are forced to compete on commercial terms or risk being overshadowed. They bear the cost of cultural extraction.
 *   - Corporate Producers (F&B, fashion, media): Primary beneficiary (institutional/arbitrage) — They leverage the trend to reduce market uncertainty and launch scalable, profitable product lines, capturing the majority of the value.
 *   - Cultural Influencers: Secondary beneficiary (organized/mobile) — They build personal brands and gain audience by amplifying and curating the Newtro aesthetic, acting as key nodes in the enforcement network.
 *   - Analytical Observer: The cultural critic or system analyst who sees both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_newtro_aesthetic, 0.48).
domain_priors:suppression_score(sk_newtro_aesthetic, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(sk_newtro_aesthetic, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_newtro_aesthetic, extractiveness, 0.48).
narrative_ontology:constraint_metric(sk_newtro_aesthetic, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sk_newtro_aesthetic, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sk_newtro_aesthetic, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(sk_newtro_aesthetic). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, corporate_producers).
narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, cultural_influencers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sk_newtro_aesthetic, independent_artisans).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (INDEPENDENT ARTISAN)
% Experiences the trend as a coercive market force that extracts their
% cultural capital while suppressing non-conforming aesthetics. The high
% suppression and extraction, combined with a trapped exit, makes this a Snare.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ ≈ 0.48 * 1.42 * 1.0 (national scope) ≈ 0.68. This χ ≥ 0.66 classifies as a Snare.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (CORPORATE PRODUCER)
% Experiences the trend as a highly effective coordination mechanism. It
% provides a clear, pre-validated aesthetic to guide product development,
% reducing market risk and creating reliable revenue streams.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% χ ≈ 0.48 * -0.12 * 1.0 ≈ -0.06. This is a clear Rope classification.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes both the genuine coordination function (aligning consumer and
% producer preferences) and the significant asymmetric extraction (value flow
% from artisans to corporations). This dual nature is the hallmark of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE CULTURAL INFLUENCER
% A secondary beneficiary who operates within the system. For them, it is a
% coordination device (Rope) that provides a reliable path to audience growth.
% Their exit is mobile, not arbitrage, as shifting aesthetics carries brand risk.
% Engine derives d for a mobile beneficiary ≈ 0.15 → f(d) ≈ -0.01.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_newtro_aesthetic_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the powerless artisan and institutional corporation.
    constraint_indexing:constraint_classification(sk_newtro_aesthetic, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(sk_newtro_aesthetic, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(tangled_rope_conditions_met) :-
    % Verify that the structural conditions for a Tangled Rope classification are met.
    narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, _),
    narrative_ontology:constraint_victim(sk_newtro_aesthetic, _),
    domain_priors:requires_active_enforcement(sk_newtro_aesthetic).

test(analytical_claim_matches_type) :-
    % The system's final claim should match the analytical perspective.
    narrative_ontology:constraint_claim(sk_newtro_aesthetic, Type),
    constraint_indexing:constraint_classification(sk_newtro_aesthetic, Type, context(agent_power(analytical), _, _, _)).

:- end_tests(sk_newtro_aesthetic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (ε=0.48) is high because the model focuses on the
 *   commercialization of the trend, where significant cultural capital is
 *   appropriated from authentic sources and repackaged for mass-market profit
 *   with little compensation to the originators.
 *   Suppression Score (0.65) is high because the trend's dominance on social
 *   media and in retail makes it difficult for alternative or non-conforming
 *   aesthetics to gain traction with the youth demographic. It is not enforced
 *   by law, but by powerful market and algorithmic pressures.
 *   This is a canonical Tangled Rope because it possesses both a genuine
 *   coordination function and a strong extractive dynamic.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the 'corporate_producer' (beneficiary), Newtro is a
 *   Rope—a brilliant coordination tool that de-risks investment. For the
 *   'independent_artisan' (victim), it is a Snare—a coercive pressure to conform
 *   or become invisible, where their cultural identity is extracted for others' profit.
 *   The beneficiary sees a signal; the victim sees a demand.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs value and attention.
 *   - BENEFICIARIES: 'corporate_producers' benefit from scalable profits and reduced
 *     market uncertainty. 'cultural_influencers' benefit from audience engagement
 *     and monetization. The directionality `d` is low for them.
 *   - VICTIMS: 'independent_artisans' bear the cost. Their work provides the
 *     aesthetic foundation, but they are often priced out or overshadowed by
 *     larger players who can produce at scale. The directionality `d` is high for them.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. First, it avoids
 *   labeling the trend as a pure Rope, which would ignore the clear extractive
 *   harm done to originators. Second, it avoids calling it a pure Snare from an
 *   analytical view, which would miss the very real coordination function that
 *   makes the trend so powerful and appealing to both producers and consumers.
 *   The Tangled Rope classification captures this essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_sk_newtro_aesthetic,
    'Is the Newtro trend a temporary commercial fad (Snare-like) or a durable cultural shift that will eventually lead to a broader appreciation and economic support for traditional crafts (Scaffold-like)?',
    'Longitudinal analysis of revenue streams for independent artisans vs. corporate producers over a 10-year period; tracking the persistence of Newtro aesthetics after peak commercial hype.',
    'If it proves durable and supportive, the constraint could be reclassified as a Scaffold that transitions society toward a new equilibrium. If it is a fad, its extractive properties dominate, reinforcing the Snare/Tangled Rope analysis.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sk_newtro_aesthetic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is > 0.46, so temporal data is required.
% This models the lifecycle of the trend from nascent cultural movement to
% full-blown commercial phenomenon.

% Theater ratio over time:
narrative_ontology:measurement(sk_newtro_tr_t0, sk_newtro_aesthetic, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sk_newtro_tr_t5, sk_newtro_aesthetic, theater_ratio, 5, 0.20).
narrative_ontology:measurement(sk_newtro_tr_t10, sk_newtro_aesthetic, theater_ratio, 10, 0.30).

% Extraction over time:
narrative_ontology:measurement(sk_newtro_ex_t0, sk_newtro_aesthetic, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(sk_newtro_ex_t5, sk_newtro_aesthetic, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(sk_newtro_ex_t10, sk_newtro_aesthetic, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: Newtro functions as an information standard, providing a
% shared aesthetic language for producers and consumers.
narrative_ontology:coordination_type(sk_newtro_aesthetic, information_standard).

% DUAL FORMULATION NOTE:
% This constraint models the *commercialization* of Newtro (ε=0.48, Tangled Rope).
% A separate constraint, 'sk_newtro_preservation', could model the trend as a
% cultural preservation effort, which would have a much lower ε (e.g., 0.15)
% and classify as a Rope. The commercialization constraint affects the
% preservation constraint by commodifying its outputs.
%
% narrative_ontology:affects_constraint(sk_newtro_aesthetic, sk_newtro_preservation).
% narrative_ontology:affects_constraint(sk_newtro_aesthetic, global_k_culture_exports).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */