% ============================================================================
% CONSTRAINT STORY: uk_artist_resale_right
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_uk_artist_resale_right, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_artist_resale_right
 *   human_readable: UK Artist's Resale Right (ARR) Legislation
 *   domain: economic/legal
 *
 * SUMMARY:
 *   The UK's Artist's Resale Right (ARR), enacted in 2006, is a legal
 *   mandate requiring a royalty payment to the original artist (or their
 *   estate) upon the resale of their work by an art market professional.
 *   The constraint is a resource allocation mechanism intended to support
 *   artists, but it imposes administrative and financial costs on the art market,
 *   which are managed by collecting societies that take a commission.
 *
 * KEY AGENTS (by structural relationship):
 *   - Estate of an Obscure Artist: Powerless beneficiary (powerless/trapped) — nominally benefits but experiences the system as extractive.
 *   - Living Artists & Estates: Primary beneficiary (moderate/constrained) — receives royalties.
 *   - UK Art Market Sellers: Primary target (organized/constrained) — bears the cost and administrative burden.
 *   - Collecting Societies (e.g., DACS): Institutional beneficiary (institutional/arbitrage) — administers the system and takes a commission.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_artist_resale_right, 0.48). % Represents administrative overhead, commission, and asymmetric benefit concentration.
domain_priors:suppression_score(uk_artist_resale_right, 0.85).   % Structural property (raw, unscaled). High due to legal mandate.
domain_priors:theater_ratio(uk_artist_resale_right, 0.20).       % Piton detection (< 0.70); the system is functional.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_artist_resale_right, extractiveness, 0.48).
narrative_ontology:constraint_metric(uk_artist_resale_right, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(uk_artist_resale_right, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_artist_resale_right, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(uk_artist_resale_right). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(uk_artist_resale_right, living_artists_and_estates).
narrative_ontology:constraint_beneficiary(uk_artist_resale_right, art_collecting_societies).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(uk_artist_resale_right, uk_art_market_sellers).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (UK ART MARKET SELLERS)
% Organized actors (galleries, auction houses) who see the ARR as a coercive
% tax and administrative burden.
% Engine derives d from: victim membership + constrained exit -> d ≈ 0.8 -> f(d) ≈ 1.29 -> high χ
constraint_indexing:constraint_classification(uk_artist_resale_right, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ARTISTS)
% Artists and their estates who see the ARR as a fair, beneficial coordination
% mechanism that secures them ongoing income from their work's success.
% Engine derives d from: beneficiary membership + constrained exit -> d ≈ 0.3 -> f(d) ≈ 0.16 -> low χ
constraint_indexing:constraint_classification(uk_artist_resale_right, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INSTITUTIONAL BENEFICIARY (COLLECTING SOCIETIES)
% The societies administering the scheme profit from its existence via commission.
% For them, it is a pure coordination system they operate.
% Engine derives d from: beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ
constraint_indexing:constraint_classification(uk_artist_resale_right, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Recognizes both the genuine coordination function (supporting artists) and the
% asymmetric extraction (costs on sellers, rent-seeking by administrators).
% The high suppression and extraction, coupled with a coordination goal, define a Tangled Rope.
constraint_indexing:constraint_classification(uk_artist_resale_right, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE POWERLESS BENEFICIARY (ESTATE OF AN OBSCURE ARTIST)
% For the estate of a lesser-known artist, the system is experienced as a trap.
% While they are nominal beneficiaries, the administrative hurdles, the collecting
% society's commission, and their lack of resources to pursue claims make the
% system feel extractive. The benefit is theoretical, but the cost is real.
% Engine derives d from: powerless + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ
constraint_indexing:constraint_classification(uk_artist_resale_right, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_artist_resale_right_tests).

test(perspectival_gap_seller_vs_artist) :-
    constraint_indexing:constraint_classification(uk_artist_resale_right, snare, context(agent_power(organized), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(uk_artist_resale_right, rope, context(agent_power(moderate), _, exit_options(constrained), _)),
    format('Seller/Target sees Snare, Artist/Beneficiary sees Rope. Gap confirmed.~n').

test(perspectival_gap_powerless_vs_institutional) :-
    constraint_indexing:constraint_classification(uk_artist_resale_right, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_artist_resale_right, rope, context(agent_power(institutional), _, _, _)),
    format('Powerless sees Snare, Institutional sees Rope. Gap confirmed.~n').

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(uk_artist_resale_right, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(uk_artist_resale_right, _),
    narrative_ontology:constraint_victim(uk_artist_resale_right, _),
    domain_priors:requires_active_enforcement(uk_artist_resale_right).

:- end_tests(uk_artist_resale_right_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value represents the combined systemic costs, not just the royalty transfer. It includes the 15% commission taken by collecting societies, the administrative overhead for galleries and auction houses to track and process payments, and the economic friction introduced. The high concentration of payouts to a few superstar artists also contributes to the extractive nature for the median artist and the market as a whole.
 *   - Suppression (0.85): This is a legal requirement. Non-compliance has legal consequences. Alternatives are strongly suppressed.
 *   - This combination of genuine coordination (a resource transfer to artists) and high extraction/suppression makes it a canonical Tangled Rope from an analytical view.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For an art seller (e.g., a gallery), the law is a pure cost with no benefit—a coercive extraction they must comply with to operate. This makes it a Snare. For a successful artist, it's a system that coordinates the market to provide them with fair compensation, a clear Rope. The collecting society also sees a Rope, as it's the mechanism that funds their operations. The most telling gap is for the powerless beneficiary (the estate of an obscure artist), who is supposed to benefit but experiences the system's administrative friction and costs as a Snare, revealing how a system's intent can diverge from its effect on the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `living_artists_and_estates` and `art_collecting_societies`. The engine assigns them low directionality (d), resulting in low or negative effective extraction (χ).
 *   - Victim: `uk_art_market_sellers`. This group bears the direct financial and administrative costs. The engine assigns them a high directionality (d), resulting in high effective extraction (χ), hence the Snare classification from their perspective.
 *   The structural declarations correctly model the flow of value and cost within the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This case demonstrates the value of the Tangled Rope classification. A simplistic analysis might label ARR as either pure coordination ("it helps artists," a Rope) or pure extraction ("it's a tax on the market," a Snare). The Tangled Rope category correctly identifies that it is *both* simultaneously. It has a legitimate, designed-in coordination function but achieves it through high-coercion enforcement and creates opportunities for rent-seeking (the administrative cut), leading to significant extractive properties. The framework avoids mislabeling by acknowledging this hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_uk_artist_resale_right,
    'Does the ARR create a net positive economic benefit for the UK art ecosystem, or does it drive high-value sales to jurisdictions without such a right (e.g., the US, Switzerland), thus harming the market it intends to regulate?',
    'Comparative longitudinal analysis of high-value art sales volumes in London vs. New York/Geneva/Hong Kong, controlling for other market factors, pre- and post-2006.',
    'If net positive, the base extractiveness (ε) might be modeled as lower. If net negative, ε is likely higher as the deadweight loss is greater than modeled.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_uk_artist_resale_right, empirical, 'Net economic impact on UK art market vs. international competitors.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Represents the 20-year period from 2006-2026, scaled to a 10-point interval.
narrative_ontology:interval(uk_artist_resale_right, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As the system matured, administrative
% overhead may have become more entrenched. Required since ε > 0.46.

% Theater ratio over time:
narrative_ontology:measurement(arr_tr_t0, uk_artist_resale_right, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arr_tr_t5, uk_artist_resale_right, theater_ratio, 5, 0.18).
narrative_ontology:measurement(arr_tr_t10, uk_artist_resale_right, theater_ratio, 10, 0.20).

% Extraction over time:
narrative_ontology:measurement(arr_ex_t0, uk_artist_resale_right, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(arr_ex_t5, uk_artist_resale_right, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(arr_ex_t10, uk_artist_resale_right, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(uk_artist_resale_right, resource_allocation).

% Network relationships (structural influence edges)
% The ARR is a specific implementation within the broader domain of copyright law.
narrative_ontology:affects_constraint(uk_copyright_law, uk_artist_resale_right).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the relationships between artists, sellers, and administrators.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */