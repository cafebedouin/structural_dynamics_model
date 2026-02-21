% ============================================================================
% CONSTRAINT STORY: lp_pikachu_illustrator
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_lp_pikachu_illustrator, []).

:- use_module(library(plunit)).
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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lp_pikachu_illustrator
 *   human_readable: "Artificial Value Creation in High-End Collectibles Market"
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the system of grading, authenticating, and
 *   marketing high-value collectibles, exemplified by Logan Paul's purchase
 *   and promotion of a "PSA Grade 10" Pikachu Illustrator Pokémon card. The
 *   system establishes a consensus on value (a coordination function) but
 *   enables massive, asymmetric value extraction by leveraging celebrity,
 *   media spectacle, and induced belief in artificial scarcity to create
 *   speculative bubbles.
 *
 * KEY AGENTS (by structural relationship):
 *   - Retail Collectors: Primary target (powerless/trapped) — Their belief
 *     and financial participation underpin the market's speculative value,
 *     making them vulnerable to hype-driven price manipulation.
 *   - Celebrity Influencers & Grading Companies: Primary beneficiaries
 *     (institutional/arbitrage) — They create and capture the vast majority
 *     of the speculative value, using the grading system as a tool for
 *     legitimation and hype generation.
 *   - Analytical Observer: Sees the full structure of both coordination
 *     and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lp_pikachu_illustrator, 0.68).
domain_priors:suppression_score(lp_pikachu_illustrator, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(lp_pikachu_illustrator, 0.80).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lp_pikachu_illustrator, extractiveness, 0.68).
narrative_ontology:constraint_metric(lp_pikachu_illustrator, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(lp_pikachu_illustrator, theater_ratio, 0.80).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lp_pikachu_illustrator, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(lp_pikachu_illustrator). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(lp_pikachu_illustrator, celebrity_influencers).
narrative_ontology:constraint_beneficiary(lp_pikachu_illustrator, collectibles_graders).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(lp_pikachu_illustrator, retail_collectors).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (RETAIL COLLECTORS)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% The high ε (0.68) and high f(d) result in a χ > 0.66, classifying it as a Snare.
constraint_indexing:constraint_classification(lp_pikachu_illustrator, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (INFLUENCERS/GRADERS)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% This classifies the system as a highly efficient coordination mechanism (Rope).
constraint_indexing:constraint_classification(lp_pikachu_illustrator, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees both the coordination function and the
% severe asymmetric extraction. The high base extraction (ε=0.68), high
% suppression (0.85), and presence of both beneficiaries and victims, plus
% active enforcement, meets all criteria for a Tangled Rope.
constraint_indexing:constraint_classification(lp_pikachu_illustrator, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lp_pikachu_illustrator_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(lp_pikachu_illustrator, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(lp_pikachu_illustrator, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(lp_pikachu_illustrator, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    domain_priors:requires_active_enforcement(lp_pikachu_illustrator),
    narrative_ontology:constraint_beneficiary(lp_pikachu_illustrator, _),
    narrative_ontology:constraint_victim(lp_pikachu_illustrator, _).

test(high_extraction_and_suppression_scores) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lp_pikachu_illustrator, ExtMetricName, E),
    E >= 0.46,
    config:param(suppression_metric_name, SupMetricName),
    narrative_ontology:constraint_metric(lp_pikachu_illustrator, SupMetricName, S),
    S >= 0.60.

:- end_tests(lp_pikachu_illustrator_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): High. While there is a coordination function
 *     (agreeing on card condition), the vast majority of the $5M+ valuation is
 *     speculative value generated and extracted through hype, not related to
 *     any intrinsic property of the card. ε represents the ratio of this
 *     extracted, artificial value to the base coordination utility.
 *   - Suppression (0.85): High. To participate in the high-end market, using
 *     a top-tier grading service like PSA is de facto mandatory. Alternatives
 *     lack legitimacy, effectively suppressing competition and locking participants
 *     into the established system.
 *   - Theater Ratio (0.80): Very High. The public spectacle of the purchase,
 *     the WWE entrance, the diamond-encrusted pendant, and the creation of a
 *     related NFT are all performative acts designed to inflate perceived value,
 *     far outweighing the simple function of a transaction.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For a beneficiary like Logan Paul, the system is a
 *   perfect Rope—a tool for coordinating audience attention to generate immense
 *   wealth. For a retail collector (the target), it is a Snare—a coercive
 *   system where they are induced to buy into a market whose values are
 *   manipulated by powerful, unaccountable actors. They are trapped by the
 *   fear of missing out and the belief in the system's legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The beneficiaries are `celebrity_influencers`
 *   and `collectibles_graders`. They architect and profit from the system. The
 *   victims are `retail_collectors`, who provide the liquidity and belief that
 *   fuels the speculative bubble, bearing the financial risk. The engine
 *   correctly derives a low `d` for beneficiaries (low χ) and a high `d`
 *   for victims (high χ).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual-nature of the collectibles
 *   market. A simplistic analysis might label it either as "free market
 *   coordination" (Rope) or a "total fraud" (Snare). The Tangled Rope
 *   classification from the analytical perspective is more accurate: it
 *   acknowledges the real coordination function (providing a standard for
 *   quality) while correctly identifying that this function has been co-opted
 *   for severe, asymmetric extraction and requires active, theatrical
 *   enforcement to maintain its structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_lp_pikachu,
    'To what extent is the value of high-end collectibles based on genuine, sustainable collector demand versus manufactured, celebrity-driven speculative hype?',
    'Analysis of market liquidity, price stability, and sales volume for similar high-end collectibles in the years following the peak of the hype cycle.',
    'If demand is sustainable, the base extractiveness (ε) might be slightly lower. If it collapses, the ε=0.68 is confirmed or even understated.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lp_pikachu_illustrator, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over the 2010s, exploding during the pandemic.
% This data models the rise in both performative theater and extraction.
% T=0: Early modern collectible market. T=5: Pandemic boom. T=10: Logan Paul peak.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(lp_pikachu_tr_t0, lp_pikachu_illustrator, theater_ratio, 0, 0.20).
narrative_ontology:measurement(lp_pikachu_tr_t5, lp_pikachu_illustrator, theater_ratio, 5, 0.50).
narrative_ontology:measurement(lp_pikachu_tr_t10, lp_pikachu_illustrator, theater_ratio, 10, 0.80).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(lp_pikachu_ex_t0, lp_pikachu_illustrator, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(lp_pikachu_ex_t5, lp_pikachu_illustrator, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(lp_pikachu_ex_t10, lp_pikachu_illustrator, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The grading system acts as a standard for information about asset quality.
narrative_ontology:coordination_type(lp_pikachu_illustrator, information_standard).

% Network relationships (structural influence edges)
% The spectacle was used to launch an NFT, linking the logic of physical
% collectible speculation directly to the digital speculative bubble.
narrative_ontology:affects_constraint(lp_pikachu_illustrator, nft_speculative_bubble).
narrative_ontology:affects_constraint(influencer_monetization_models, lp_pikachu_illustrator).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the directionality of extraction.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */