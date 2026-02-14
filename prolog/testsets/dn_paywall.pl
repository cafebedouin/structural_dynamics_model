% ============================================================================
% CONSTRAINT STORY: dn_paywall
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_dn_paywall, []).

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
 *   constraint_id: dn_paywall
 *   human_readable: Dagens Nyheter Digital Subscription Paywall
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The digital paywall implemented by the Swedish newspaper Dagens Nyheter (DN).
 *   It restricts access to online articles to paying subscribers, acting as a
 *   revenue-generation mechanism to fund journalism in an era of declining
 *   print and advertising income. This creates a direct trade-off between
 *   the financial sustainability of a high-quality news organization and the
 *   public's free access to information.
 *
 * KEY AGENTS (by structural relationship):
 *   - non_subscribing_readers: Primary target (powerless/trapped) — blocked from information or must pay the fee.
 *   - bonnier_news_ab: Primary beneficiary (institutional/arbitrage) — receives subscription revenue to fund operations.
 *   - public_at_large: Secondary victim/beneficiary — loses broad access to information but benefits from the existence of a funded, independent press.
 *   - media_analyst: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dn_paywall, 0.52).
domain_priors:suppression_score(dn_paywall, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(dn_paywall, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dn_paywall, extractiveness, 0.52).
narrative_ontology:constraint_metric(dn_paywall, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dn_paywall, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(dn_paywall, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(dn_paywall). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(dn_paywall, bonnier_news_ab).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(dn_paywall, non_subscribing_readers).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% Calculation: χ = 0.52 * 1.42 * 1.0 (national scope) ≈ 0.738
% Since χ >= 0.66 and suppression >= 0.60, this is a Snare.
constraint_indexing:constraint_classification(dn_paywall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% Calculation: χ = 0.52 * -0.12 * 1.0 (national scope) ≈ -0.06
% Since χ <= 0.35, this is a Rope.
constraint_indexing:constraint_classification(dn_paywall, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% The analyst sees both the coordination (funding) and extraction (fees).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% Calculation: χ = 0.52 * 1.15 * 1.2 (global scope) ≈ 0.717
% The combination of a genuine coordination function and high effective
% extraction (0.40 <= χ <= 0.90) classifies it as a Tangled Rope.
constraint_indexing:constraint_classification(dn_paywall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dn_paywall_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:classify(dn_paywall, context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(national)), snare),
    constraint_indexing:classify(dn_paywall, context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(national)), rope).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:classify(dn_paywall, context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)), tangled_rope).

test(threshold_validation_high_extraction_and_suppression) :-
    domain_priors:base_extractiveness(dn_paywall, E), E >= 0.46,
    domain_priors:suppression_score(dn_paywall, S), S >= 0.60.

:- end_tests(dn_paywall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): Represents the non-trivial financial cost of a yearly subscription, which is a direct extraction of value from the reader.
 *   - Suppression Score (0.75): High. DN holds a strong position in Swedish quality journalism. For certain political analyses, cultural reporting, or investigative pieces, there are few-to-no freely available substitutes of equivalent quality and depth. This lack of alternatives is a form of suppression. Suppression is a raw structural property, not scaled by perspective.
 *   - Theater Ratio (0.10): Low. The paywall is a blunt, functional mechanism, not a performative or ceremonial one. Its purpose is transparently economic.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a non-subscribing reader ('powerless', 'trapped'), the paywall is a pure barrier (Snare) that extracts money or withholds information. For the publisher ('institutional', 'arbitrage'), it is a coordination mechanism (Rope) essential for funding the creation of the very information the reader desires. The publisher sees it as a tool for survival and quality; the excluded reader sees it as a coercive toll.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived from the structural relationships.
 *   - `bonnier_news_ab` is declared the `constraint_beneficiary`. Combined with `arbitrage` exit, the engine assigns a very low `d`, resulting in negative effective extraction (χ) and a Rope classification. The constraint benefits them.
 *   - `non_subscribing_readers` are declared the `constraint_victim`. Combined with `trapped` exit, the engine assigns a very high `d`, resulting in high effective extraction (χ) and a Snare classification. The constraint targets them.
 *   This automatic derivation accurately models the opposing experiences of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This model prevents the two most common mischaracterizations of paywalls.
 *   1. It avoids labeling the paywall as a pure Snare, which would ignore its genuine and critical coordination function of funding professional journalism (a public good). The analytical `tangled_rope` classification explicitly acknowledges this function.
 *   2. It avoids labeling the paywall as a pure Rope, which would ignore the significant coercive extraction and the creation of an information-underclass who cannot afford access. The `powerless` perspective's Snare classification makes this exclusion visible and measurable.
 *   The `tangled_rope` classification is therefore the most accurate, capturing the necessary but costly nature of the solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_dn_paywall,
    'Is the paywall model a necessary evil for funding quality journalism, or an opportunistic extraction model that suppresses superior, less-extractive alternatives (e.g., public funding, non-profit models)?',
    'Comparative analysis of financial viability and journalistic quality in markets with different funding models (e.g., Swedish paywall vs. German public media vs. UK Guardian non-profit model).',
    'If necessary, it remains a Tangled Rope. If opportunistic, its suppression score is understated and it leans closer to a pure Snare, as it actively blocks better coordination solutions.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dn_paywall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the paywall model over the last decade,
% showing "extraction accumulation" as the model matured from experimental to standard.
% Required because base_extractiveness > 0.46.

% Theater ratio over time (declined as the model became normalized):
narrative_ontology:measurement(dn_paywall_tr_t0, dn_paywall, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dn_paywall_tr_t5, dn_paywall, theater_ratio, 5, 0.15).
narrative_ontology:measurement(dn_paywall_tr_t10, dn_paywall, theater_ratio, 10, 0.10).

% Extraction over time (increased with price hikes and removal of leaky access):
narrative_ontology:measurement(dn_paywall_ex_t0, dn_paywall, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(dn_paywall_ex_t5, dn_paywall, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dn_paywall_ex_t10, dn_paywall, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The paywall coordinates funding to produce a resource (journalism).
narrative_ontology:coordination_type(dn_paywall, resource_allocation).

% Network relationships (structural influence edges)
% Hard paywalls at major outlets increase the importance and political
% pressure on publicly funded media.
narrative_ontology:affects_constraint(dn_paywall, public_service_media_funding).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately captures
% the perspectival differences.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */