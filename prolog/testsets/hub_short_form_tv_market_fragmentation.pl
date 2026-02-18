% ============================================================================
% CONSTRAINT STORY: hub_short_form_tv_market_fragmentation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_hub_short_form_tv_market_fragmentation, []).

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
 *   constraint_id: hub_short_form_tv_market_fragmentation
 *   human_readable: Short-Form Video's Impact on Traditional TV Market Fragmentation
 *   domain: economic
 *
 * SUMMARY:
 *   The rise of short-form video platforms (e.g., TikTok, YouTube Shorts) fragments the traditional TV and movie market by diverting consumer attention and ad revenue. This creates a dynamic where traditional media companies face increased competition and need to adapt their strategies to remain competitive. This dynamic increases market uncertainty for investors. The ultimate source of extraction is the attention of end consumers, which is captured via algorithmic feeds.
 *
 * KEY AGENTS (by structural relationship):
 *   - End Consumers: Primary target (powerless/trapped) — their attention is the extracted resource.
 *   - Traditional Media Companies: Institutional victim (institutional/constrained) — bears extraction of attention and revenue.
 *   - Short-Form Video Platforms: Primary beneficiary (institutional/arbitrage) — benefits from increased user engagement and ad revenue.
 *   - Advertisers: Secondary actor (institutional/mobile) - increasingly shifting budget to short-form video.
 *   - Investors: Analytical observer (analytical/analytical) — sees full market shift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hub_short_form_tv_market_fragmentation, 0.55).
domain_priors:suppression_score(hub_short_form_tv_market_fragmentation, 0.65).   % Structural property (raw, unscaled). Algorithmic feeds and network effects suppress alternatives.
domain_priors:theater_ratio(hub_short_form_tv_market_fragmentation, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, extractiveness, 0.55).
narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hub_short_form_tv_market_fragmentation, tangled_rope).
narrative_ontology:human_readable(hub_short_form_tv_market_fragmentation, "Short-Form Video's Impact on Traditional TV Market Fragmentation").
narrative_ontology:topic_domain(hub_short_form_tv_market_fragmentation, "economic").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(hub_short_form_tv_market_fragmentation).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(hub_short_form_tv_market_fragmentation). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(hub_short_form_tv_market_fragmentation).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hub_short_form_tv_market_fragmentation, short_form_video_platforms).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hub_short_form_tv_market_fragmentation, traditional_media_companies).
narrative_ontology:constraint_victim(hub_short_form_tv_market_fragmentation, end_consumers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The end consumer whose attention is captured by algorithmic feeds.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint operates between two institutional actors with different
% structural relationships and exit options.

% PERSPECTIVE 3A: The Institutional Victim (Tangled Rope)
% Traditional media companies, who lose revenue and market share.
% Their exit is constrained, not fully trapped, as they can attempt to adapt.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3B: The Primary Beneficiary (Rope)
% Short-form video platforms who benefit from the attention shift.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hub_short_form_tv_market_fragmentation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(hub_short_form_tv_market_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is 0.55, representing the significant diversion of attention and revenue from traditional media. The suppression score is increased to 0.65 (from 0.50) to reflect the coercive nature of algorithmic feeds and strong network effects, which actively suppress alternatives and make it difficult for users to disengage, thus meeting the threshold for a Snare classification from the powerless perspective. The theater ratio is low (0.30) as the market competition is functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   - End Consumers (powerless/trapped) perceive a Snare. Their attention is captured by systems designed to maximize engagement, making exit difficult.
 *   - Traditional Media Companies (institutional/constrained) perceive a Tangled Rope. They face significant extraction but also see it as a market coordination problem they must adapt to.
 *   - Short-Form Video Platforms (institutional/arbitrage) perceive a Rope. They are coordinating a new form of content consumption and benefit immensely from the network effects and ad revenue.
 *   - The Analytical observer sees a Tangled Rope, recognizing both the genuine coordination function and the asymmetric extraction from multiple victim groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Short-form video platforms are the beneficiaries, attracting users and advertisers. Traditional media companies are institutional victims, losing market share. End consumers are the primary victims, as their attention is the fundamental resource being extracted. This multi-victim structure is key to the Tangled Rope classification at the analytical level.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids mislabeling the market shift as a pure Snare by acknowledging the genuine coordination function short-form video provides (a new content format). It also avoids mislabeling it as a pure Rope by recognizing the significant, asymmetric extraction of value (attention, revenue) from both consumers and traditional media companies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hub_short_form_tv_market_fragmentation,
    'Will traditional media companies successfully adapt to the short-form video market?',
    'Historical analysis of media companies adapting to new technologies and platforms, market share data.',
    'If yes, the extraction will diminish, potentially shifting the classification towards a more balanced state. If no, the extraction will intensify, possibly leading to a more extreme Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hub_short_form_tv_market_fragmentation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(hub_short_form_tv_market_fragmentation_tr_t0, hub_short_form_tv_market_fragmentation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(hub_short_form_tv_market_fragmentation_tr_t5, hub_short_form_tv_market_fragmentation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(hub_short_form_tv_market_fragmentation_tr_t10, hub_short_form_tv_market_fragmentation, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(hub_short_form_tv_market_fragmentation_ex_t0, hub_short_form_tv_market_fragmentation, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(hub_short_form_tv_market_fragmentation_ex_t5, hub_short_form_tv_market_fragmentation, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(hub_short_form_tv_market_fragmentation_ex_t10, hub_short_form_tv_market_fragmentation, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(hub_short_form_tv_market_fragmentation, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(hub_short_form_tv_market_fragmentation, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(hub_short_form_tv_market_fragmentation, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(hub_short_form_tv_market_fragmentation, [sibling_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(hub_short_form_tv_market_fragmentation, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */