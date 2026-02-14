% ============================================================================
% CONSTRAINT STORY: israel_egypt_gas_deal
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_israel_egypt_gas_deal, []).

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
 *   constraint_id: israel_egypt_gas_deal
 *   human_readable: Geopolitical Gas Supply Agreement between Israel and Egypt
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   A bilateral agreement where Israel supplies natural gas to Egypt. While this
 *   solves Egypt's immediate domestic energy shortages and risk of civil
 *   unrest, it creates a long-term strategic dependency. The deal provides
 *   Israel with significant economic revenue and geopolitical leverage over a
 *   key regional power, creating an asymmetric power dynamic under the guise
 *   of a standard commercial transaction.
 *
 * KEY AGENTS (by structural relationship):
 *   - Egyptian Populace: Primary target (powerless/trapped) — bears the costs of dependency and potential price manipulation.
 *   - Israeli Government & Energy Sector: Primary beneficiary (institutional/arbitrage) — reaps economic and geopolitical rewards.
 *   - Egyptian Government: Inter-institutional actor (institutional/constrained) — secures domestic stability at the cost of strategic autonomy.
 *   - Geopolitical Analyst: Analytical observer — sees the full structure of coercive coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_egypt_gas_deal, 0.48).
domain_priors:suppression_score(israel_egypt_gas_deal, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(israel_egypt_gas_deal, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_egypt_gas_deal, extractiveness, 0.48).
narrative_ontology:constraint_metric(israel_egypt_gas_deal, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(israel_egypt_gas_deal, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this Tangled Rope.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(israel_egypt_gas_deal, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(israel_egypt_gas_deal). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
narrative_ontology:constraint_beneficiary(israel_egypt_gas_deal, israeli_government_and_energy_sector).
narrative_ontology:constraint_victim(israel_egypt_gas_deal, egyptian_state_and_populace).

% Gate requirements check for Tangled Rope:
%   [X] beneficiary declared
%   [X] victim declared
%   [X] requires_active_enforcement declared
% All conditions met.

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

% PERSPECTIVE 1: THE EGYPTIAN POPULACE (SNARE)
% Bears the cost of dependency and is powerless to change the terms.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% χ = 0.48 * 1.42 * 1.0 (national scope) ≈ 0.68. This is ≥ 0.66, classifying as a Snare.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ISRAELI GOVERNMENT (ROPE)
% Benefits economically and strategically.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
% χ = 0.48 * -0.12 * 1.0 ≈ -0.06. Negative extraction classifies as a Rope.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.48 * 1.15 * 1.2 (global scope) ≈ 0.66. The structural data (beneficiary,
% victim, enforcement) forces a Tangled Rope classification.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE EGYPTIAN GOVERNMENT (TANGLED ROPE)
% An institutional actor forced into the deal. Recognizes the extraction but
% accepts it to maintain domestic stability.
% Engine derives d from: victim membership + constrained exit → d ≈ 0.75-0.85
% f(d) ≈ 1.25. χ = 0.48 * 1.25 * 1.0 ≈ 0.60.
% This χ is in the Tangled Rope range (0.40 ≤ χ ≤ 0.90), reflecting
% a coercive but necessary coordination mechanism.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_egypt_gas_deal_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verifies the two institutional actors have different classifications/experiences.
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(israel_egypt_gas_deal, _),
    narrative_ontology:constraint_victim(israel_egypt_gas_deal, _),
    domain_priors:requires_active_enforcement(israel_egypt_gas_deal).

:- end_tests(israel_egypt_gas_deal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value is high, reflecting that the "price" of the gas is not just monetary but also includes significant geopolitical leverage and a transfer of strategic autonomy from Egypt to Israel. It's not pure theft (ε near 1.0) because Egypt receives a vital commodity, but it is a highly asymmetric value exchange.
 *   - Suppression Score (0.65): Egypt's alternatives are poor. Domestic energy shortages pose a direct threat to regime stability, making the Israeli supply a forced choice. This high suppression score is critical for the Snare and Tangled Rope classifications.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the Israeli government (beneficiary with arbitrage exit), the deal is a pure coordination win (Rope), generating revenue and influence. For the Egyptian populace (victim with trapped exit), it's a Snare, creating a dependency that can be exploited. This divergence is the core signature of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `israeli_government_and_energy_sector`. They gain revenue and, more importantly, geopolitical leverage over a critical neighbor. This maps to a low directionality `d` and negative effective extraction `χ`.
 *   - Victim: `egyptian_state_and_populace`. They lose strategic independence and become vulnerable to supply disruptions or price coercion. This maps to a high `d` and high `χ`.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional dynamics. Both the Israeli and Egyptian governments are `institutional` actors, but their relationship to the constraint is fundamentally different, captured by their `exit_options`. Israel has `arbitrage` (it can sell its gas to other markets), making it a true beneficiary. The Egyptian government has `constrained` exit; its primary alternative is widespread domestic unrest, a catastrophic option. Therefore, it perceives the deal as a Tangled Rope—a necessary act of coordination that comes with heavy, unavoidable extractive costs. The Deferential Realism framework correctly distinguishes these two institutional perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. It does not mistake the deal for a simple, mutually beneficial trade (a pure Rope), which would ignore the coercive element and geopolitical extraction. It also does not label it as pure predation (a pure Snare), which would ignore the genuine coordination function of supplying desperately needed energy. The `Tangled Rope` classification correctly identifies the hybrid nature of the constraint: it solves a coordination problem, but does so in a way that creates and exploits a power imbalance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_israel_egypt_gas_deal,
    'To what extent is the Egypt-Israel gas deal a product of mutual economic benefit versus Israeli geopolitical coercion exploiting Egyptian domestic instability?',
    'Access to the classified pricing terms of the agreement, internal Egyptian government memos on energy security vs. strategic autonomy, and a counterfactual analysis of alternative energy suppliers for Egypt.',
    'If primarily mutual benefit, ε would be lower (~0.25) and the constraint would be a Rope from most perspectives. If primarily coercion, ε would be higher (~0.60) and the analytical classification would be a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_egypt_gas_deal, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required as base_extractiveness > 0.46.
% Models a scenario where Egypt's dependency deepens over time,
% increasing Israel's leverage and thus the extractive potential of the deal.

% Theater ratio over time (remains low and stable):
narrative_ontology:measurement(israel_egypt_gas_deal_tr_t0, israel_egypt_gas_deal, theater_ratio, 0, 0.10).
narrative_ontology:measurement(israel_egypt_gas_deal_tr_t5, israel_egypt_gas_deal, theater_ratio, 5, 0.12).
narrative_ontology:measurement(israel_egypt_gas_deal_tr_t10, israel_egypt_gas_deal, theater_ratio, 10, 0.15).

% Extraction over time (increases as dependency is locked in):
narrative_ontology:measurement(israel_egypt_gas_deal_ex_t0, israel_egypt_gas_deal, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(israel_egypt_gas_deal_ex_t5, israel_egypt_gas_deal, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(israel_egypt_gas_deal_ex_t10, israel_egypt_gas_deal, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(israel_egypt_gas_deal, resource_allocation).

% Network relationships: This deal affects regional stability.
narrative_ontology:affects_constraint(israel_egypt_gas_deal, east_med_geopolitical_stability).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain,
% using beneficiary/victim declarations combined with the distinct `exit_options`
% for the two institutional actors (`arbitrage` vs. `constrained`), accurately
% captures the directionality and power asymmetry of the constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */