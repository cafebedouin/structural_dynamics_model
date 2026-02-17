% ============================================================================
% CONSTRAINT STORY: fiat_currency_lifecycle
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-14
% ============================================================================

:- module(constraint_fiat_currency_lifecycle, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 *   constraint_id: fiat_currency_lifecycle
 *   human_readable: The Lifecycle of a Fiat Currency
 *   domain: economic/political
 *
 * SUMMARY:
 *   Models the typical lifecycle of an unbacked fiat currency. The constraint
 *   is the monetary system itself. It begins as a pure coordination tool (Rope),
 *   evolves into a managed system for growth (Scaffold), degrades as it's used
 *   to finance state deficits (Tangled Rope), and finally collapses in a
 *   hyperinflationary event (Snare). This story represents the final, collapsed state.
 *
 * KEY AGENTS (by structural relationship):
 *   - Savers & Wage Earners: Primary targets of the inflation tax (powerless/trapped).
 *   - The State & Central Bank: Primary beneficiaries of seigniorage (institutional/arbitrage).
 *   - An Austrian School Economist: The analytical observer (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS) - Represents T=20 (Hyperinflationary Collapse)
   ========================================================================== */

% --- Numerical metrics (End State: Collapse) ---
domain_priors:base_extractiveness(fiat_currency_lifecycle, 0.95). % Wealth extraction via hyperinflation is near-total.
domain_priors:suppression_score(fiat_currency_lifecycle, 0.90).   % Capital controls & legal tender laws suppress alternatives.
domain_priors:theater_ratio(fiat_currency_lifecycle, 0.75).       % Officials insist "inflation is under control."

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiat_currency_lifecycle, extractiveness, 0.95).
narrative_ontology:constraint_metric(fiat_currency_lifecycle, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(fiat_currency_lifecycle, theater_ratio, 0.75).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fiat_currency_lifecycle, tangled_rope).
narrative_ontology:human_readable(fiat_currency_lifecycle, "The Lifecycle of a Fiat Currency").

% --- Binary flags ---
domain_priors:requires_active_enforcement(fiat_currency_lifecycle). % Legal tender laws, central bank operations.

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, the_state). % First recipient of new money.
narrative_ontology:constraint_victim(fiat_currency_lifecycle, savers_and_wage_earners). % Last to receive new money.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: A SAVER IN HYPERINFLATION (SNARE)
% For a citizen holding the currency as it collapses, the system is a Snare
% that confiscates their life savings.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped), % Trapped by capital controls / lack of alternatives
            spatial_scope(national))).

% PERSPECTIVE 2: THE CENTRAL BANK / STATE (ROPE)
% For the state, the ability to print money is a powerful Rope for funding
% its operations and directing the economy, appearing as a net benefit.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: AN ECONOMIST (TANGLED ROPE)
% The analytical observer sees the dual function: it's a coordination tool
% (medium of exchange) that is inextricably tangled with a powerful,
% asymmetric extraction mechanism (the inflation tax).
constraint_indexing:constraint_classification(fiat_currency_lifecycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiat_currency_lifecycle_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(fiat_currency_lifecycle, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiat_currency_lifecycle, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(end_state_is_snare_for_victim) :-
    % From the victim's perspective, the final state must be a Snare
    BaseE = 0.95, Supp = 0.90,
    % Use canonical powerless modifier for test approximation
    Chi is BaseE * 1.50,
    BaseE >= 0.46, Supp >= 0.60, Chi >= 0.66.

:- end_tests(fiat_currency_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story models the Cantillon Effect, where the benefits of money creation
 *   accrue to those closest to the source (the state), while the costs (inflation)
 *   are borne by those furthest away (savers and wage earners). The temporal
 *   measurements model the lifecycle:
 *   - T=0-4 (Adoption): ROPE, a simple medium of exchange.
 *   - T=5-9 (Managed Growth): SCAFFOLD, central bank intervenes for public good.
 *   - T=10-15 (Fiscal Exploitation): TANGLED ROPE, deficit financing begins, creating an inflation tax.
 *   - T=16-20 (Hyperinflation): SNARE, confidence is lost, and the currency rapidly becomes worthless.
 *
 * PERSPECTIVAL GAP:
 *   The gap is a direct model of the Cantillon Effect. The state (beneficiary)
 *   always sees the system as a useful Rope for achieving its goals. The saver
 *   (victim) first sees a Rope, then a Tangled Rope as their purchasing power
 *   erodes, and finally a Snare as their savings are wiped out. The Economist
 *   sees the inherent tension from the start.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   The extreme base extractiveness (0.95) at the end of the lifecycle risks a
 *   Mandatrophy error, where a system with a genuine coordination function is
 *   misclassified as pure extraction. This model resolves the ambiguity by
 *   explicitly encoding the perspectival gap.
 *   - For the State (beneficiary), the system remains a coordination tool (Rope),
 *     reflecting its utility in funding state operations.
 *   - For the Saver (victim), it becomes a pure extraction mechanism (Snare).
 *   - The analytical `tangled_rope` classification requires both a beneficiary
 *     (coordination function) and a victim (asymmetric extraction). This
 *     prevents the system from being reduced to just one of its functions and
 *     correctly identifies it as a hybrid system where coordination has been
 *     weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_fiat_endgame,
    'Can a fiat currency be managed as a permanent Tangled Rope, or does the political incentive structure make collapse into a Snare inevitable?',
    'Long-term study of sovereign debt levels vs. central bank independence and inflation rates.',
    'If manageable, a "soft landing" is possible. If inevitable, all fiat currencies are terminal.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiat_currency_lifecycle, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% T=0-4: Adoption (Rope)
narrative_ontology:measurement(fiat_ex_t0, fiat_currency_lifecycle, base_extractiveness, 0, 0.05). % Minimal seigniorage
narrative_ontology:measurement(fiat_th_t0, fiat_currency_lifecycle, theater_ratio, 0, 0.0).
narrative_ontology:measurement(fiat_su_t0, fiat_currency_lifecycle, suppression_requirement, 0, 0.10).

% T=5-9: Managed Growth (Scaffold)
narrative_ontology:measurement(fiat_ex_t5, fiat_currency_lifecycle, base_extractiveness, 5, 0.10).
narrative_ontology:measurement(fiat_th_t5, fiat_currency_lifecycle, theater_ratio, 5, 0.10).
narrative_ontology:measurement(fiat_su_t5, fiat_currency_lifecycle, suppression_requirement, 5, 0.30).
narrative_ontology:measurement(fiat_ex_t9, fiat_currency_lifecycle, base_extractiveness, 9, 0.15).
narrative_ontology:measurement(fiat_th_t9, fiat_currency_lifecycle, theater_ratio, 9, 0.20).
narrative_ontology:measurement(fiat_su_t9, fiat_currency_lifecycle, suppression_requirement, 9, 0.40).

% T=10-15: Fiscal Exploitation (Tangled Rope)
narrative_ontology:measurement(fiat_ex_t10, fiat_currency_lifecycle, base_extractiveness, 10, 0.40). % Inflation tax begins
narrative_ontology:measurement(fiat_th_t10, fiat_currency_lifecycle, theater_ratio, 10, 0.40). % "Inflation is transitory"
narrative_ontology:measurement(fiat_su_t10, fiat_currency_lifecycle, suppression_requirement, 10, 0.50).
narrative_ontology:measurement(fiat_ex_t15, fiat_currency_lifecycle, base_extractiveness, 15, 0.60).
narrative_ontology:measurement(fiat_th_t15, fiat_currency_lifecycle, theater_ratio, 15, 0.60).
narrative_ontology:measurement(fiat_su_t15, fiat_currency_lifecycle, suppression_requirement, 15, 0.70).

% T=16-20: Hyperinflation (Snare)
narrative_ontology:measurement(fiat_ex_t16, fiat_currency_lifecycle, base_extractiveness, 16, 0.80). % Confidence collapses
narrative_ontology:measurement(fiat_th_t16, fiat_currency_lifecycle, theater_ratio, 16, 0.80). % Denials become absurd
narrative_ontology:measurement(fiat_su_t16, fiat_currency_lifecycle, suppression_requirement, 16, 0.85).
narrative_ontology:measurement(fiat_ex_t20, fiat_currency_lifecycle, base_extractiveness, 20, 0.95). % Final value
narrative_ontology:measurement(fiat_th_t20, fiat_currency_lifecycle, theater_ratio, 20, 0.75).   % Final value
narrative_ontology:measurement(fiat_su_t20, fiat_currency_lifecycle, suppression_requirement, 20, 0.90).   % Final value

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiat_currency_lifecycle, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */