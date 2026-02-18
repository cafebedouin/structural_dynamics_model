% ============================================================================
% CONSTRAINT STORY: berkshire_compounding_culture
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_berkshire_compounding_culture, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: berkshire_compounding_culture
 *   human_readable: The Berkshire Hathaway Culture of Compounding
 *   domain: economic
 *
 * SUMMARY:
 *   A corporate culture predicated on foregoing dividends and electing to
 *   reinvest earnings rather than consume them. This constraint functions as a
 *   coordination mechanism between passive long-term investors and management
 *   to create a "mixture of a sustained culture of savings, combined with the
 *   magic of long-term compounding." While highly effective for capital growth,
 *   it creates structural constraints for management (due to scale) and for
 *   investors who require current income.
 *
 * KEY AGENTS (by structural relationship):
 *   - Income-Focused Investors: Primary target (powerless/trapped) — bears the cost of deferred consumption and tax friction on exit.
 *   - Long-Term Shareholders: Primary beneficiary (powerless/arbitrage) — benefits from the compounding mechanism.
 *   - Berkshire Management: Institutional actor (institutional/trapped) — constrained by the inertia and scale the culture creates.
 *   - U.S. Treasury: Secondary beneficiary (analytical/arbitrage) — benefits from massive corporate tax payments.
 *   - Analytical Observer: Sees the full structure of coordination, extraction, and inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Management compensation is heavily tied to investment performance,
% aligning interests with owners. The system is designed for value creation
% for posterity, not immediate extraction.
domain_priors:base_extractiveness(berkshire_compounding_culture, 0.10).

% Rationale: Investment is voluntary, and marketable equities allow for exit.
% However, the tax implications of selling create a moderate suppression effect
% for long-term holders.
domain_priors:suppression_score(berkshire_compounding_culture, 0.20).

% Rationale: From the CEO's perspective, adherence to the "Berkshire creed"
% can become performative due to the immense scale of the company, which removes
% flexibility and makes certain actions (like large acquisitions) difficult.
% The culture must be theatrically maintained.
domain_priors:theater_ratio(berkshire_compounding_culture, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(berkshire_compounding_culture, extractiveness, 0.10).
narrative_ontology:constraint_metric(berkshire_compounding_culture, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(berkshire_compounding_culture, theater_ratio, 0.75).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(berkshire_compounding_culture, rope).
narrative_ontology:human_readable(berkshire_compounding_culture, "The Berkshire Hathaway Culture of Compounding").
narrative_ontology:topic_domain(berkshire_compounding_culture, "economic").

% --- Binary flags ---
% Rationale: Maintaining the culture requires "wisdom and vigilance" against
% entropic decay and changes in fiscal policy.
domain_priors:requires_active_enforcement(berkshire_compounding_culture).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(berkshire_compounding_culture, long_term_shareholders).
narrative_ontology:constraint_beneficiary(berkshire_compounding_culture, us_treasury).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(berkshire_compounding_culture, income_focused_investors).

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

% PERSPECTIVE 1: THE LONG-TERM SHAREHOLDER (ROPE)
% For the investor focused on capital growth, the culture is a pure Rope: a
% functional coordination mechanism that allows small savings to compound over
% decades. Exit is mobile via public markets.
constraint_indexing:constraint_classification(berkshire_compounding_culture, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile), % 'arbitrage' might imply more active trading
            spatial_scope(global))).

% PERSPECTIVE 2: THE INCOME-FOCUSED INVESTOR (SNARE)
% For an investor needing current income, the "forego dividends" rule is a Snare.
% They are trapped because the only exit is to sell shares, triggering capital
% gains taxes. This friction suppresses liquidity in favor of the reinvestment creed.
constraint_indexing:constraint_classification(berkshire_compounding_culture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE BERKSHIRE CEO (PITON)
% For the CEO, the culture's success has created immense scale, which removes
% flexibility ("we can't come and go on a dime"). The "creed" becomes a form of
% institutional inertia that must be performatively maintained, even when it
% constrains strategic options. This is a classic Piton.
constraint_indexing:constraint_classification(berkshire_compounding_culture, piton,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER / U.S. TREASURY (ROPE)
% From a systemic, analytical view, the culture is a highly effective Rope for
% allocating capital and generating societal value (and tax revenue). Its
% primary function is coordination.
constraint_indexing:constraint_classification(berkshire_compounding_culture, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(berkshire_compounding_culture_tests).

test(perspectival_gap_powerless) :-
    % Verify that a powerless investor's classification depends on their exit options.
    constraint_indexing:constraint_classification(berkshire_compounding_culture, rope, context(agent_power(powerless), _, exit_options(mobile), _)),
    constraint_indexing:constraint_classification(berkshire_compounding_culture, snare, context(agent_power(powerless), _, exit_options(trapped), _)).

test(institutional_inertia) :-
    % Institutional CEO sees a Piton due to scale and cultural inertia.
    constraint_indexing:constraint_classification(berkshire_compounding_culture, piton, context(agent_power(institutional), _, _, _)).

test(low_extraction_and_high_theater) :-
    % Verify the core metrics for a Rope that has degraded to a Piton for some actors.
    narrative_ontology:constraint_metric(berkshire_compounding_culture, extractiveness, E),
    narrative_ontology:constraint_metric(berkshire_compounding_culture, theater_ratio, T),
    E < 0.25,
    T >= 0.70.

:- end_tests(berkshire_compounding_culture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.10) is low because the system's primary design
 *   is capital compounding, not management enrichment. The suppression score
 *   (0.20) is non-trivial due to the tax friction on exit, but still low as
 *   investment is voluntary. The high theater ratio (0.75) is key to the Piton
 *   classification for management, reflecting the performative aspect of
 *   maintaining a "creed" that now limits flexibility due to the organization's
 *   massive scale.
 *
 * PERSPECTIVAL GAP:
 *   The core gap exists for the 'powerless' investor. For one seeking long-term
 *   growth with mobile exit, it's a Rope. For one needing income, the lack of
 *   dividends and tax cost of exit makes it a Snare. This demonstrates how exit
 *   options critically alter perception. A second gap exists between the
 *   analytical view (Rope) and the institutional actor (Piton), where the
 *   successful coordination mechanism has generated so much scale that it has
 *   become an inertial constraint on its own managers.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'long_term_shareholders' directly benefit from compounding.
 *     The 'us_treasury' is a major secondary beneficiary via tax revenue.
 *   - Victims: 'income_focused_investors' bear the cost of deferred consumption
 *     and are structurally disadvantaged by the "no dividends" policy.
 *   This clear beneficiary/victim structure allows the directionality engine
 *   to derive appropriate `d` values, leading to low/negative chi for
 *   beneficiaries and high chi for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This story shows how a pure Rope can generate second-order effects that are
 *   Snare-like or Piton-like for different actors. The low base extraction
 *   prevents misclassifying the entire system as a Snare, while the high
 *   theater ratio correctly identifies the inertial trap experienced by
 *   management. The framework successfully distinguishes the primary
 *   coordination function from its emergent, constraining side effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_berkshire_talent,
    'Is the success of the compounding culture attributable to an innate, irreplaceable talent (Mountain) or a replicable system (Rope)?',
    'Longitudinal study of post-Buffett performance and analysis of capital allocation decisions made by successors.',
    'If innate, the system is fragile and person-dependent. If a replicable system, it is a robust coordination technology.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_berkshire_talent, empirical, 'Distinguishing innate talent vs. replicable system in long-term performance.').

% /5 form: narrative detail for story context
omega_variable(
    omega_berkshire_currency,
    'Is the long-term value of compounding vulnerable to the degradation of fiat currency (a background Mountain/Snare)?',
    'Monitoring of U.S. fiscal policy, currency stability, and inflation-adjusted returns over decades.',
    'If currency degrades, the compounding Rope may simply be preserving wealth against a Snare rather than creating new value.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_berkshire_currency, empirical, 'Assessing the impact of fiat currency degradation on real returns.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(berkshire_compounding_culture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.10) < 0.46.
% The following is a template for if the constraint were high-extraction.
%
% narrative_ontology:measurement(berkshire_compounding_culture_tr_t0, berkshire_compounding_culture, theater_ratio, 0, 0.1).
% narrative_ontology:measurement(berkshire_compounding_culture_tr_t5, berkshire_compounding_culture, theater_ratio, 5, 0.4).
% narrative_ontology:measurement(berkshire_compounding_culture_tr_t10, berkshire_compounding_culture, theater_ratio, 10, 0.75).
%
% narrative_ontology:measurement(berkshire_compounding_culture_ex_t0, berkshire_compounding_culture, base_extractiveness, 0, 0.1).
% narrative_ontology:measurement(berkshire_compounding_culture_ex_t5, berkshire_compounding_culture, base_extractiveness, 5, 0.1).
% narrative_ontology:measurement(berkshire_compounding_culture_ex_t10, berkshire_compounding_culture, base_extractiveness, 10, 0.1).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The culture is a mechanism for allocating capital resources over long time horizons.
narrative_ontology:coordination_type(berkshire_compounding_culture, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately captures
% the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */