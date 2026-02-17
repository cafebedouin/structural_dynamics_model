% ============================================================================
% CONSTRAINT STORY: eu_affordable_housing_initiative
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_eu_affordable_housing_initiative, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_affordable_housing_initiative
 *   human_readable: "EU Affordable Housing Initiative (2025)"
 *   domain: economic/political
 *
 * SUMMARY:
 *   The European Commission's plan to promote affordable housing across
 *   member states. The initiative establishes a platform for sharing best
 *   practices and coordinates financial support through EU funds. While it
 *   serves a genuine coordination function, its structure (complex funding
 *   rules, reliance on large-scale development) creates significant
 *   opportunities for value capture by large corporate actors, effectively
 *   extracting value from the system intended for low-income households.
 *
 * KEY AGENTS (by structural relationship):
 *   - low_income_households: Primary target (powerless/trapped) — bear costs of price inflation and policy inefficiency.
 *   - large_construction_firms_and_investors: Primary beneficiary (institutional/arbitrage) — benefit from subsidies and centralized contracts.
 *   - eu_commission_and_member_states: Institutional architect (institutional/constrained) — benefit from political mandate but constrained by policy implementation and capture.
 *   - analytical_observer: Analytical observer — sees the full coordination/extraction duality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_affordable_housing_initiative, 0.48). % Significant value capture by non-target beneficiaries.
domain_priors:suppression_score(eu_affordable_housing_initiative, 0.65).   % Becomes the dominant channel for housing policy, crowding out alternatives.
domain_priors:theater_ratio(eu_affordable_housing_initiative, 0.25).       % A real program, but with a significant PR component. Not a Piton.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, extractiveness, 0.48).
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, theater_ratio, 0.25).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_affordable_housing_initiative, tangled_rope).
narrative_ontology:human_readable(eu_affordable_housing_initiative, "EU Affordable Housing Initiative (2025)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_affordable_housing_initiative). % Required for Tangled Rope: EU must monitor fund allocation and compliance.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, large_construction_firms_and_investors).
narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, eu_commission_and_member_states). % Political benefit from "acting" on housing.

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, low_income_households). % Intended beneficiaries who bear costs of inefficiency/inflation.
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, small_local_developers). % Suppressed by complexity favoring large firms.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% low_income_households are victims with trapped exit. The engine derives a
% high d (~0.95), leading to high effective extraction (χ). With high
% suppression, this appears as a Snare: a system that promises help but
% traps them in a high-cost environment.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% large_construction_firms_and_investors are beneficiaries with arbitrage exit.
% The engine derives a low d (~0.05), leading to negative effective
% extraction (χ). For them, the policy is a pure coordination mechanism that
% provides subsidies and predictable contracts.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical observer sees both the coordination function (beneficiaries exist)
% and the asymmetric extraction (victims exist). The high ε and suppression,
% combined with the structural data, lead to a Tangled Rope classification.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INSTITUTIONAL ARCHITECT (ROPE)
% The EU Commission and Member States are institutional actors, but with
% constrained exit—they cannot easily abandon their own flagship policy.
% As both beneficiary (political win) and victim (of capture/failure), their
% derived directionality 'd' is moderate (~0.5). This results in a positive but
% low χ, classifying the constraint as a Rope from their viewpoint, as they
% focus on the coordination function and are structurally incentivized to
% downplay the extraction.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_affordable_housing_initiative_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Passed: Target (snare) and Beneficiary (rope) have different classifications.~n').

test(perspectival_gap_architect_vs_target) :-
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, rope, context(agent_power(institutional), _, exit_options(constrained), _)),
    format('Passed: Target (snare) and Architect (rope) have different classifications.~n').

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('Passed: Analytical observer correctly identifies Tangled Rope.~n').

test(tangled_rope_structural_data_present) :-
    narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, _),
    narrative_ontology:constraint_victim(eu_affordable_housing_initiative, _),
    domain_priors:requires_active_enforcement(eu_affordable_housing_initiative),
    format('Passed: All three structural requirements for Tangled Rope are declared.~n').

:- end_tests(eu_affordable_housing_initiative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε) is set to 0.48 to model significant value leakage
 *   to developers and investors through subsidies, favorable zoning changes, and
 *   complex procurement processes that favor large incumbents. Suppression is
 *   high (0.65) because such a large-scale EU initiative centralizes policy,
 *   making it the "only game in town" and marginalizing local, non-corporate,
 *   or more radical housing solutions (like co-ops or land value taxes).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For low-income households (the target), the program is a
 *   Snare; it may inflate local land/rent prices and deliver housing that is
 *   "affordable" only by a metric that doesn't match their reality. For large
 *   developers (the beneficiary), it's a Rope; a beneficial coordination
 *   mechanism reducing risk and providing capital. For the EU Commission
 *   (the architect), it is also a Rope; they are institutionally blind to the
 *   extraction, focusing on the political win of implementing a large-scale
 *   coordination program.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the explicit beneficiary/victim declarations.
 *   `large_construction_firms_and_investors` are beneficiaries; they gain
 *   subsidized contracts. `low_income_households` are victims; they are the
 *   putative target but bear the externalized costs. The engine maps these
 *   roles plus their exit options (arbitrage vs. trapped) to extreme `d` values
 *   (low for beneficiary, high for victim), which drives the classification gap.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the difference between the primary corporate beneficiary and
 *   the institutional architect. The EU Commission has `constrained` exit; it's
 *   politically difficult to admit failure or dismantle their own initiative.
 *   This, combined with their dual beneficiary/victim status (political wins vs.
 *   risk of capture), results in a moderate directionality. They perceive the
 *   program as coordination (Rope), unlike the corporate beneficiaries who can
 *   exploit it with `arbitrage` exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. A naive analysis might label the
 *   initiative a Rope based on its stated goals. A cynical analysis might label
 *   it a Snare based on its likely outcomes. The Tangled Rope classification,
 *   from the analytical perspective, correctly identifies it as a hybrid system
 *   with both a genuine coordination function AND severe asymmetric extraction,
 *   which is the defining feature of many large-scale public-private partnerships.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_eu_affordable_housing,
    'Is the extractive potential of the initiative a bug (inefficient implementation) or a feature (a deliberate subsidy to the construction sector disguised as social policy)?',
    'Forensic accounting of fund flows to developers vs. final housing cost reduction; analysis of lobbying records during policy formation.',
    'If a bug, the system might be reformable towards a Scaffold or Rope. If a feature, it is a durable Tangled Rope, bordering on a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_affordable_housing_initiative, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.48 > 0.46), so temporal data is
% required to track its lifecycle. The data models an initial idealistic
% phase followed by "extraction accumulation" as the system is captured by
% vested interests over its 10-year interval.

% Theater ratio over time:
narrative_ontology:measurement(eu_housing_tr_t0, eu_affordable_housing_initiative, theater_ratio, 0, 0.40).
narrative_ontology:measurement(eu_housing_tr_t5, eu_affordable_housing_initiative, theater_ratio, 5, 0.30).
narrative_ontology:measurement(eu_housing_tr_t10, eu_affordable_housing_initiative, theater_ratio, 10, 0.25).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(eu_housing_ex_t0, eu_affordable_housing_initiative, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(eu_housing_ex_t5, eu_affordable_housing_initiative, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(eu_housing_ex_t10, eu_affordable_housing_initiative, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(eu_affordable_housing_initiative, resource_allocation).

% Network relationships (structural influence edges)
% This EU-level policy will directly influence national-level regulations.
narrative_ontology:affects_constraint(eu_affordable_housing_initiative, german_rent_control_laws).
narrative_ontology:affects_constraint(eu_affordable_housing_initiative, french_social_housing_subsidies).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are used for this constraint. The structural derivation chain,
% based on the rich beneficiary/victim data and differentiated exit options
% (trapped, constrained, arbitrage), is sufficient to accurately model the
% directionality for each key agent. The resulting classifications (Snare,
% Rope, Tangled Rope) reflect the expected perspectival gaps without needing
% manual adjustment.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */