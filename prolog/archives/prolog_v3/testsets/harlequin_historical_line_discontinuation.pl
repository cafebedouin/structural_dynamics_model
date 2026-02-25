% ============================================================================
% CONSTRAINT STORY: harlequin_historical_line_discontinuation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-27
% ============================================================================

:- module(constraint_harlequin_historical_line_discontinuation, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: harlequin_historical_line_discontinuation
 *   human_readable: Harlequin's Discontinuation of its Dedicated Historical Romance Imprint
 *   domain: economic
 *
 * SUMMARY:
 *   In 2025, major romance publisher Harlequin (owned by HarperCollins) announced
 *   the discontinuation of its dedicated Harlequin Historical imprint. While the
 *   company stated it would continue to publish historical romance, this would
 *   happen within its other, more generalized imprints. This corporate decision
 *   acts as a constraint that centralizes resources and eliminates a specialized,
 *   prestigious, and reliable channel for authors in that subgenre.
 *
 * KEY AGENTS (by structural relationship):
 *   - historical_romance_authors: Primary target (powerless/trapped) — lose a key market, income stream, and career path.
 *   - harlequin_corporate: Primary beneficiary (institutional/arbitrage) — streamlines operations and reallocates capital to higher-margin genres.
 *   - literary_agents: Secondary actor (moderate/constrained) — must find new venues for their clients.
 *   - analytical_observer: Analytical observer — sees the full structure of capital reallocation and market consolidation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harlequin_historical_line_discontinuation, 0.75).
domain_priors:suppression_score(harlequin_historical_line_discontinuation, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(harlequin_historical_line_discontinuation, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, extractiveness, 0.75).
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(harlequin_historical_line_discontinuation, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(harlequin_historical_line_discontinuation). % Enforced by editorial and corporate policy.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(harlequin_historical_line_discontinuation, harlequin_corporate).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, historical_romance_authors).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
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
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% The observer sees a highly extractive mechanism with no true coordination
% function for the overall system, classifying it as a Snare.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harlequin_historical_line_discontinuation_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_snare) :-
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
        context(agent_power(analytical), _, _, _)).

test(snare_thresholds_met) :-
    domain_priors:base_extractiveness(harlequin_historical_line_discontinuation, E),
    domain_priors:suppression_score(harlequin_historical_line_discontinuation, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(harlequin_historical_line_discontinuation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε = 0.75): This score reflects a significant, one-sided
 *     reallocation of value. Harlequin captures the capital, editorial resources,
 *     and market position previously dedicated to the historical line and redirects
 *     it to genres it deems more profitable. The authors lose their primary,
 *     specialized platform, representing a direct extraction of opportunity and
 *     potential income.
 *   - Suppression Score (S = 0.80): The decision has a high suppression effect.
 *     Harlequin is a market leader in category romance. Eliminating a flagship
 *     imprint for a specific subgenre severely curtails the options for authors
 *     specializing in it. While other publishers exist, they do not offer an
 *     identical platform, forcing authors to adapt, rebrand, or accept less
 *     favorable terms elsewhere.
 *   - Theater Ratio (T = 0.30): The public statements about "evolving reader
 *     preferences" and continuing to publish historicals in other lines are a
 *     form of PR theater to soften the blow. However, the underlying action—the
 *     closure of the dedicated line—is a concrete, non-theatrical business
 *     decision with material consequences.
 *
 * PERSPECTIVAL GAP:
 *   - From the `historical_romance_authors`' perspective (powerless, trapped),
 *     the decision is a Snare. It abruptly closes off a vital career path they
 *     are invested in, with high costs to exit or adapt. The 'trap' is their
 *     specialization and reliance on this market structure.
 *   - From `harlequin_corporate`'s perspective (institutional, arbitrage), the
 *     same decision is a Rope. It is a pure coordination tool for efficiently
 *     managing its portfolio of assets (imprints) and allocating capital to
 *     maximize returns. For the corporation, this is a rational, beneficial act
 *     of internal organization with negative effective extraction (χ < 0).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. `harlequin_corporate` is the sole
 *   beneficiary, gaining financial flexibility and operational efficiency.
 *   `historical_romance_authors` are the clear victims, bearing the full cost
 *   of the decision through lost opportunities and career disruption. The
 *   engine's derivation of d (low for the beneficiary, high for the victim)
 *   from these structural roles is accurate and requires no overrides.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This classification correctly identifies the coercive nature of a top-down
 *   corporate mandate that is framed as a neutral business decision. A naive
 *   analysis might accept the "market efficiency" argument and label this a
 *   coordination mechanism (Rope). However, by indexing to the powerless agent
 *   (the authors), the Deferential Realism framework reveals the high extractive
 *   cost and high suppression, correctly identifying the constraint as a Snare
 *   for those most affected by it. It disambiguates corporate resource management
 *   (a Rope for insiders) from market foreclosure (a Snare for outsiders). The high
 *   extraction score (0.75) is justified as it represents a direct, material
 *   foreclosure of a specialized market channel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_harlequin_discontinuation,
    'Is the publisher''s claim of "evolving reader preferences" an accurate reflection of market data, or a post-hoc justification for a purely profit-driven consolidation?',
    'Access to Harlequin''s internal sales data and independent, long-term market analysis of the historical romance subgenre.',
    'If true, the base extractiveness might be slightly lower, as the publisher is responding to an external reality. If false, the action is purely extractive and the ε=0.75 is fully justified.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The reporting engine reads narrative_ontology:omega_variable/3 with structure
% (ID, TypeClass, Description) where TypeClass is one of:
%   empirical   — resolvable by gathering more data
%   conceptual  — depends on definitional or theoretical framing
%   preference  — depends on value judgments or policy choices
% The /3 form is what the engine reads; /5 provides narrative context.
narrative_ontology:omega_variable(omega_harlequin_discontinuation, empirical, 'Whether the publisher''s justification is based on actual market data or is a post-hoc rationalization.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(harlequin_historical_line_discontinuation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction > 0.46 requires temporal data. This models the internal
% pressure to consolidate resources building over time, culminating in the
% decision to close the line.

% Theater ratio over time: Initially low, spikes with the public announcement.
narrative_ontology:measurement(hhld_tr_t0, harlequin_historical_line_discontinuation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(hhld_tr_t5, harlequin_historical_line_discontinuation, theater_ratio, 5, 0.15).
narrative_ontology:measurement(hhld_tr_t10, harlequin_historical_line_discontinuation, theater_ratio, 10, 0.30).

% Extraction over time: Increases as the line is deprioritized and finally cut.
narrative_ontology:measurement(hhld_ex_t0, harlequin_historical_line_discontinuation, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(hhld_ex_t5, harlequin_historical_line_discontinuation, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(hhld_ex_t10, harlequin_historical_line_discontinuation, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: From the perspective of the beneficiary, this is a
% resource allocation mechanism.
narrative_ontology:coordination_type(harlequin_historical_line_discontinuation, resource_allocation).

% Network relationships: This decision structurally influences the diversity
% of the publishing market and the financial stability of authors.
narrative_ontology:affects_constraint(harlequin_historical_line_discontinuation, genre_diversity_in_publishing).
narrative_ontology:affects_constraint(harlequin_historical_line_discontinuation, author_income_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% based on the declared beneficiary/victim groups and their exit options
% accurately models the power dynamics and directionality of the constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */