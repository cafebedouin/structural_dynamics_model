% ============================================================================
% CONSTRAINT STORY: help_to_buy_uk
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_help_to_buy_uk, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: help_to_buy_uk
 *   human_readable: UK 'Help to Buy' Equity Loan Scheme
 *   domain: economic
 *
 * SUMMARY:
 *   The UK's "Help to Buy" scheme (2013-2023) offered first-time buyers an
 *   equity loan from the government to reduce their mortgage deposit. While
 *   enabling home purchases, a House of Lords report found it inflated house
 *   prices, primarily benefiting large housebuilders. Buyers now face rising
 *   interest payments and the risk of negative equity, while the government
 *   anticipates a net loss on the program.
 *
 * KEY AGENTS (by structural relationship):
 *   - First-time buyers: Primary target (powerless/trapped) — bear extraction via price inflation and loan terms.
 *   - Large housebuilders: Primary beneficiary (institutional/arbitrage) — benefit from increased sales and inflated prices.
 *   - UK Government: Architect/secondary beneficiary (institutional/constrained) — benefits politically but is financially exposed to losses.
 *   - Analytical observer: Sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(help_to_buy_uk, 0.52).
domain_priors:suppression_score(help_to_buy_uk, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(help_to_buy_uk, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(help_to_buy_uk, extractiveness, 0.52).
narrative_ontology:constraint_metric(help_to_buy_uk, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(help_to_buy_uk, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(help_to_buy_uk, tangled_rope).
narrative_ontology:human_readable(help_to_buy_uk, "UK 'Help to Buy' Equity Loan Scheme").

% --- Binary flags ---
domain_priors:requires_active_enforcement(help_to_buy_uk). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(help_to_buy_uk, housebuilders).
narrative_ontology:constraint_beneficiary(help_to_buy_uk, uk_government).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(help_to_buy_uk, first_time_buyers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: national=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (FIRST-TIME BUYER)
% Bears the cost of price inflation and risky loan terms. Once in the scheme,
% they are trapped by the loan conditions and market dynamics.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(help_to_buy_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (HOUSEBUILDER)
% Benefits from guaranteed demand and higher sale prices. Has arbitrage exit
% as they are not dependent on this single scheme for survival.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(help_to_buy_uk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the genuine coordination function (helping some people buy houses)
% and the severe asymmetric extraction (price inflation benefiting builders).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(help_to_buy_uk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The UK Government is an institutional actor, but unlike the housebuilders, its
% exit is constrained. It is politically and financially responsible for the
% scheme's outcome, including the projected £2bn loss.
% Engine derives d from: beneficiary + constrained exit → d ≈ 0.25 → f(d) ≈ 0.13
% This results in a higher (but still low) effective extraction than the
% arbitrage beneficiary, correctly capturing its risk exposure.
constraint_indexing:constraint_classification(help_to_buy_uk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(help_to_buy_uk_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between buyer (target) and builder (beneficiary).
    constraint_indexing:constraint_classification(help_to_buy_uk, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(help_to_buy_uk, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap [Snare vs Rope] validated.~n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(help_to_buy_uk, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('... Analytical classification as Tangled Rope validated.~n').

test(tangled_rope_gate_conditions_met) :-
    narrative_ontology:constraint_beneficiary(help_to_buy_uk, _),
    narrative_ontology:constraint_victim(help_to_buy_uk, _),
    domain_priors:requires_active_enforcement(help_to_buy_uk),
    format('... Tangled Rope gate conditions (beneficiary, victim, enforcement) validated.~n').

:- end_tests(help_to_buy_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.52): High. This value captures not just the direct
 *     loan terms but the systemic price inflation effect reported by the House
 *     of Lords, which transferred wealth from the entire class of first-time
 *     buyers to housebuilders. The structure of the loan, with interest rates
 *     rising sharply after 5 years, is also inherently extractive.
 *   - Suppression (0.75): High. The scheme was limited to new-build properties,
 *     channeling a huge pool of subsidized demand directly to a few large
 *     developers and suppressing the market for existing homes. For many buyers,
 *     it created the perception of being the "only way" onto the property ladder.
 *   - Classification (Tangled Rope): The scheme is not a pure Snare because it
 *     had a genuine coordination function: it matched buyers who lacked a full
 *     deposit with sellers (builders), enabling transactions that might not have
 *     otherwise occurred. However, this coordination was fused with a severe
 *     asymmetric extraction mechanism, making it a classic Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a first-time buyer (powerless, trapped), the scheme
 *   morphs into a Snare as interest rates rise and house prices stagnate or
 *   fall, leaving them with an expensive loan and potentially negative equity.
 *   For a housebuilder (institutional, arbitrage), it was a perfect Rope: a
 *   low-risk coordination mechanism that boosted sales volume and profit margins.
 *   The builder extracts the value upfront and has no exposure to the buyer's
 *   long-term risk.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'housebuilders' are declared beneficiaries, as they captured
 *     the upside of price inflation. The 'uk_government' is also a beneficiary due
 *     to the political capital gained from being seen to "help" buyers.
 *   - Victims: 'first_time_buyers' are the victims, as they bear the long-term
 *     financial risk, higher purchase prices, and escalating loan costs.
 *   These declarations correctly drive the directionality `d`, making `χ` high for
 *   buyers and low/negative for builders.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the two institutional actors. The housebuilders
 *   have `arbitrage` exit, reflecting their ability to move capital and adapt if
 *   the scheme ends. The UK Government has `constrained` exit; it cannot simply
 *   walk away from the financial and political fallout of a failing public
 *   program, including a projected £2bn loss. This structural difference is
 *   captured by their different exit options, resulting in a higher `d` and `χ`
 *   for the government, accurately reflecting its greater risk exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It is not a pure
 *   Rope, which would ignore the immense extraction via price inflation. It is
 *   not a pure Snare, which would deny its (at least initially) functional role
 *   in coordinating buyers and sellers. The Tangled Rope classification captures
 *   the essential duality: a policy with a coordination mandate that was captured
 *   by an extractive imperative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_help_to_buy_uk,
    'Was the house price inflation an intended (but unstated) feature to stimulate construction, or an unforeseen negative side-effect of the coordination attempt?',
    'Internal government communications and minutes from policy design meetings from 2012-2013.',
    'If intended, the constraint is closer to a pure Snare with theatrical coordination. If unintended, the Tangled Rope classification is robust.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(help_to_buy_uk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the scheme's lifecycle from 2013 (T=0) to 2023 (T=10).
% Base extractiveness was high (>0.46), so temporal data is required.

% Theater ratio over time (starts high with political launch, fades as issues appear):
narrative_ontology:measurement(htb_tr_t0, help_to_buy_uk, theater_ratio, 0, 0.50).
narrative_ontology:measurement(htb_tr_t5, help_to_buy_uk, theater_ratio, 5, 0.45).
narrative_ontology:measurement(htb_tr_t10, help_to_buy_uk, theater_ratio, 10, 0.40).

% Extraction over time (ramps up as price inflation takes hold):
narrative_ontology:measurement(htb_ex_t0, help_to_buy_uk, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(htb_ex_t5, help_to_buy_uk, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(htb_ex_t10, help_to_buy_uk, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The scheme is a mechanism for allocating capital resources.
narrative_ontology:coordination_type(help_to_buy_uk, resource_allocation).

% Network relationships: The scheme's effectiveness and impact are structurally
% dependent on the pre-existing constraint of housing undersupply.
narrative_ontology:affects_constraint(uk_housing_supply_shortage, help_to_buy_uk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options (trapped, constrained, arbitrage)
% accurately models the power dynamics and risk distribution of the scheme.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */