% ============================================================================
% CONSTRAINT STORY: uk_help_to_buy_scheme
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_uk_help_to_buy_scheme, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_help_to_buy_scheme
 *   human_readable: UK 'Help to Buy' Equity Loan Scheme
 *   domain: economic
 *
 * SUMMARY:
 *   The UK's 'Help to Buy' scheme (2013-2023) offered government equity loans
 *   to help first-time buyers purchase new-build homes with small deposits.
 *   While framed as a coordination mechanism to improve housing access, the
 *   scheme created a state-subsidized market that inflated new-build prices,
 *   generating outsized profits for property developers. Borrowers are now
 *   facing steep increases in loan interest payments and risk being trapped
 *   in negative equity, revealing the scheme's highly extractive nature.
 *
 * KEY AGENTS (by structural relationship):
 *   - Help-to-Buy Borrowers: Primary target (powerless/trapped) — bear the extraction via inflated prices and escalating loan payments.
 *   - UK Housebuilders: Primary beneficiary (institutional/arbitrage) — benefit from increased sales, higher prices, and boosted profits.
 *   - UK Government (Homes England): Architect/Enforcer (institutional/constrained) — created and managed the scheme, bearing political and financial risk.
 *   - Analytical Observer: Policy analyst/auditor (analytical/analytical) — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_help_to_buy_scheme, 0.55).
domain_priors:suppression_score(uk_help_to_buy_scheme, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(uk_help_to_buy_scheme, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, extractiveness, 0.55).
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_help_to_buy_scheme, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(uk_help_to_buy_scheme). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, uk_housebuilders).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, help_to_buy_borrowers).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, uk_taxpayers).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)
%   Snare:        victim required (met)

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
% The borrower is a victim with trapped exit (negative equity, unable to remortgage).
% Engine derives d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.0 (national scope) ≈ 0.78. This high effective
% extraction, combined with high suppression, classifies as a Snare.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Housebuilders are beneficiaries with arbitrage exit (they can build other
% homes or lobby for new schemes).
% Engine derives d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * 1.0 ≈ -0.066. The negative effective extraction
% indicates a subsidy, classifying as a pure Rope.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees the full structure: the coordination function (getting buyers into
% homes) and the asymmetric extraction (inflated prices, risky loans).
% The metrics ε=0.55, suppression=0.65 and structural data (beneficiary,
% victim, enforcement) meet the Tangled Rope criteria.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SCHEME ARCHITECT (INTER-INSTITUTIONAL)
% The UK Government is an institutional actor, but with constrained exit.
% It cannot easily abandon the scheme without political and financial fallout.
% It is aware of both the coordination goals and the extractive outcomes.
% From this viewpoint, it's a tool of policy with mixed results.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_help_to_buy_scheme_tests).

test(perspectival_gap_target_beneficiary, [nondet]) :-
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_analytical_classification, [nondet]) :-
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_conditions_met) :-
    narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, _),
    narrative_ontology:constraint_victim(uk_help_to_buy_scheme, _),
    domain_priors:requires_active_enforcement(uk_help_to_buy_scheme).

:- end_tests(uk_help_to_buy_scheme_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High, reflecting the degree to which the
 *     scheme inflated new-build prices (estimated at up to 6% by some studies)
 *     and transferred wealth from buyers/taxpayers to developers. The loan
 *     structure itself becomes extractive as interest payments begin.
 *   - Suppression (0.65): High. For the target demographic (first-time buyers
 *     with low deposits), the scheme was often marketed as the only viable
 *     path to owning a new-build, suppressing alternative market solutions.
 *   - The combination of a real coordination function (enabling purchases) with
 *     high, asymmetric extraction makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - For a Help-to-Buy borrower, the scheme is a Snare. They are trapped by
 *     terms they may not have understood, facing rising costs, and have
 *     limited ability to exit without significant financial loss. The initial
 *     "help" has transformed into a coercive financial instrument.
 *   - For a housebuilder, the scheme is a Rope. It is a pure coordination
 *     mechanism that solves their problem of finding qualified buyers,
 *     effectively providing a state-sponsored subsidy that guarantees demand
 *     and inflates their profit margins. There is no extraction from them;
 *     in fact, it's a net benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `uk_housebuilders`. They received a direct subsidy in the form
 *     of a larger, more motivated customer base, allowing them to raise prices
 *     and profits. This maps to a low directionality `d`.
 *   - Victim: `help_to_buy_borrowers`. They bear the costs through inflated asset
 *     prices and long-term, risky debt. This maps to a high `d`. The UK taxpayer
 *     is also a victim, bearing the ultimate financial risk of the loan book.
 *   - The stark difference in `d` between these groups, derived from their
 *     structural roles, creates the Rope/Snare perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the policy,
 *   preventing a simplistic analysis. Calling it a pure Snare would ignore that
 *   it did, in fact, help over 300,000 people buy homes (a coordination success).
 *   Calling it a pure Rope would ignore the immense, asymmetric transfer of
 *   wealth to developers and the financial distress now faced by borrowers
 *   (the extractive failure). The Tangled Rope classification captures this
 *   essential conflict: a coordination mechanism co-opted for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_uk_help_to_buy_scheme,
    'Was the price inflation and wealth transfer to developers an intended (but unstated) goal of the policy, or an unforeseen (but tolerated) side effect of the coordination attempt?',
    'Internal government communications from the policy design phase; econometric analysis isolating the scheme''s price effect from other market factors.',
    'If intended, the constraint is closer to a cleverly disguised Snare. If unforeseen, it is a classic Tangled Rope where the coordination function was genuine but the design was flawed.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(uk_help_to_buy_scheme, 0, 10). % Represents 2013-2023

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the scheme's lifecycle from its launch (2013) to the
% present day as its negative consequences manifest.
% T=0 is 2013, T=5 is 2018, T=10 is 2023.

% Theater ratio over time (stable messaging):
narrative_ontology:measurement(uk_htb_tr_t0, uk_help_to_buy_scheme, theater_ratio, 0, 0.30).
narrative_ontology:measurement(uk_htb_tr_t5, uk_help_to_buy_scheme, theater_ratio, 5, 0.30).
narrative_ontology:measurement(uk_htb_tr_t10, uk_help_to_buy_scheme, theater_ratio, 10, 0.30).

% Extraction over time (extraction_accumulation as price inflation took hold):
narrative_ontology:measurement(uk_htb_ex_t0, uk_help_to_buy_scheme, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uk_htb_ex_t5, uk_help_to_buy_scheme, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(uk_htb_ex_t10, uk_help_to_buy_scheme, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The scheme allocates capital to a specific group for a specific purpose.
narrative_ontology:coordination_type(uk_help_to_buy_scheme, resource_allocation).

% Network relationships (structural influence edges)
% The scheme directly impacts housing affordability and mortgage market structures.
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, uk_housing_affordability).
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, uk_mortgage_market_stability).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */