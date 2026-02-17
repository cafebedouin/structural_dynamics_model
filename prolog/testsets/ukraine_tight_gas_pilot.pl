% ============================================================================
% CONSTRAINT STORY: ukraine_tight_gas_pilot
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-25
% ============================================================================

:- module(constraint_ukraine_tight_gas_pilot, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ukraine_tight_gas_pilot
 *   human_readable: Ukraine Tight Gas Pilot Project Framework
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   A state-backed, exclusive partnership between Ukraine's national gas
 *   company (Naftogaz) and a foreign expert firm (Expert Petroleum) to
 *   develop previously inaccessible "tight gas" reserves. The framework
 *   provides exclusive rights and investment incentives to unlock this
 *   resource, with the stated goal of enhancing national energy security.
 *   This constraint represents the legal, financial, and regulatory
 *   structure of this specific pilot project.
 *
 * KEY AGENTS (by structural relationship):
 *   - Naftogaz_Group & Expert_Petroleum: Primary beneficiaries (institutional/arbitrage) — gain profits and access to new resources.
 *   - Future_Local_Communities: Primary target (powerless/trapped) — bear the externalized environmental and social risks of extraction.
 *   - Potential_Domestic_Competitors: Secondary target (moderate/constrained) - locked out of the opportunity by the exclusive agreement.
 *   - Ukrainian_State: Secondary beneficiary (institutional/constrained) — gains energy security and tax revenue, but has fewer exit options than its corporate partner.
 *   - Analytical_Observer: Sees the full structure, including both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukraine_tight_gas_pilot, 0.48).
domain_priors:suppression_score(ukraine_tight_gas_pilot, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ukraine_tight_gas_pilot, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, extractiveness, 0.48).
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ukraine_tight_gas_pilot, tangled_rope).
narrative_ontology:human_readable(ukraine_tight_gas_pilot, "Ukraine Tight Gas Pilot Project Framework").

% --- Binary flags ---
domain_priors:requires_active_enforcement(ukraine_tight_gas_pilot). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, naftogaz_group).
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, expert_petroleum).
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, ukrainian_state).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, future_local_communities).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, potential_domestic_competitors).

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
% Future local communities who bear environmental risks without direct compensation.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARIES (ROPE)
% The corporate partners who designed and operate the project.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the genuine coordination function (energy production) and the
% asymmetric extraction (profit, externalized costs).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The Ukrainian state as a beneficiary, but with fewer options than its partner.
% The engine differentiates via directionality: constrained exit + beneficiary status
% yields a higher d (closer to neutral) than arbitrage exit.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukraine_tight_gas_pilot_tests).

test(perspectival_gap) :-
    % Verify the gap between the target (local communities) and beneficiary (companies).
    constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap confirmed: Snare (powerless) vs Rope (institutional)~n', []).

test(tangled_rope_conditions_met) :-
    % Verify that the base metrics and structural flags support Tangled Rope classification.
    narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, extractiveness, E), E >= 0.30,
    narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, suppression_requirement, S), S >= 0.40,
    domain_priors:requires_active_enforcement(ukraine_tight_gas_pilot),
    narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, _),
    narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, _),
    format('Tangled Rope structural requirements met.~n', []).

:- end_tests(ukraine_tight_gas_pilot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value reflects a project with a significant,
 *     genuine coordination function (unlocking a strategic national resource) that
 *     is coupled with substantial asymmetric extraction. The extraction includes
 *     the profit generated for the corporate partners from a public resource and,
 *     critically, the externalized environmental and social costs imposed on local
 *     communities who are not party to the agreement.
 *   - Suppression (0.65): The score is high because the pilot project is an exclusive
 *     agreement. It inherently suppresses alternative development models, smaller
 *     domestic competitors, and other potential land uses in the affected area.
 *   - Type: The combination of a real coordination function (beneficiary declared),
 *     asymmetric extraction (victim declared), high suppression, and required
 *     enforcement makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the corporate beneficiaries (Naftogaz, Expert Petroleum),
 *   the framework is a Rope—a pure coordination tool to align capital, technology,
 *   and legal rights to achieve a valuable goal. For the local communities, it is
 *   a Snare—an externally imposed system that extracts value (by risking their
 *   environment and quality of life) for the benefit of distant actors, with no
 *   meaningful recourse or alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the clear division of costs and benefits.
 *   `naftogaz_group`, `expert_petroleum`, and the `ukrainian_state` are declared
 *   beneficiaries, pushing their derived `d` value lower. `future_local_communities`
 *   and `potential_domestic_competitors` are victims, pushing their `d` value higher.
 *   This structural data allows the engine to automatically compute the perspectival
 *   classifications.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   A key nuance is the difference between the corporate partners and the Ukrainian
 *   state. Both are institutional beneficiaries. However, the international partner has
 *   `arbitrage` exit options (it can invest in many countries), while the state has
 *   `constrained` exit (it cannot easily abandon its own energy security policy). The
 *   engine reflects this by deriving a slightly higher directionality `d` for the
 *   state, correctly modeling that its stake is less flexible and more symmetric
 *   than its partner's.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids two common errors. A naive
 *   pro-development analysis might label it a pure Rope, ignoring the externalized
 *   costs and suppressed competition. A naive anti-corporate analysis might label it
 *   a pure Snare, ignoring the genuine strategic value of increased energy
 *   independence. The Tangled Rope classification acknowledges both truths simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ukraine_tight_gas_pilot,
    'Will the long-term environmental/social costs borne by local communities outweigh the realized national energy security and economic benefits?',
    'Longitudinal (10-20 year) empirical studies tracking water quality, seismic activity, local health outcomes, and the project''s actual contribution to Ukraine''s energy grid vs. initial projections.',
    'If benefits strongly outweigh costs, the constraint is a justifiable Tangled Rope. If costs equal or exceed benefits, it is a destructive Snare that was mis-sold as a coordination mechanism.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukraine_tight_gas_pilot, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε > 0.46), so temporal data is required.
% The model assumes the project becomes slightly more extractive over time as
% efficiencies are found and more costs are externalized, while theater remains
% low but non-zero for public relations.

% Theater ratio over time:
narrative_ontology:measurement(ukraine_tight_gas_pilot_tr_t0, ukraine_tight_gas_pilot, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ukraine_tight_gas_pilot_tr_t5, ukraine_tight_gas_pilot, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ukraine_tight_gas_pilot_tr_t10, ukraine_tight_gas_pilot, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(ukraine_tight_gas_pilot_ex_t0, ukraine_tight_gas_pilot, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(ukraine_tight_gas_pilot_ex_t5, ukraine_tight_gas_pilot, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ukraine_tight_gas_pilot_ex_t10, ukraine_tight_gas_pilot, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(ukraine_tight_gas_pilot, resource_allocation).

% Network relationships (structural influence edges)
% This project directly impacts Ukraine's energy dependence, a separate and
% highly significant geopolitical constraint.
narrative_ontology:affects_constraint(ukraine_tight_gas_pilot, ukraine_energy_dependence).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */