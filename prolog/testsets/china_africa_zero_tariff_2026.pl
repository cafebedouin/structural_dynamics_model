% ============================================================================
% CONSTRAINT STORY: china_africa_zero_tariff_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-15
% ============================================================================

:- module(constraint_china_africa_zero_tariff_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: china_africa_zero_tariff_2026
 *   human_readable: China-Africa Zero-Tariff Trade Framework
 *   domain: economic
 *
 * SUMMARY:
 *   China implements zero-tariff treatment for 53 African nations to expand
 *   market access. While nominally a coordination mechanism (Rope) for trade,
 *   the inclusion of "green channels" and "joint economic partnership pacts"
 *   introduces structural extraction through standard-setting and diplomatic
 *   alignment.
 *
 * KEY AGENTS (by structural relationship):
 *   - smallholder_african_farmers: Primary target (powerless/trapped) — bears highest compliance costs, lacks capital to meet standards.
 *   - organized_african_exporters: Secondary target (moderate/constrained) — can meet standards but at a significant cost.
 *   - china_state_actors: Primary beneficiary (institutional/arbitrage) — gains geopolitical alignment and resource security.
 *   - focac_administrators: Analytical observer — monitors the Forum on China-Africa Cooperation (FOCAC) outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Base extraction reflects the administrative/political cost of "Partnership Pacts".
domain_priors:base_extractiveness(china_africa_zero_tariff_2026, 0.32).
domain_priors:suppression_score(china_africa_zero_tariff_2026, 0.45).
domain_priors:theater_ratio(china_africa_zero_tariff_2026, 0.20).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, extractiveness, 0.32).
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(china_africa_zero_tariff_2026, tangled_rope).
narrative_ontology:human_readable(china_africa_zero_tariff_2026, "China-Africa Zero-Tariff Trade Framework").
narrative_ontology:topic_domain(china_africa_zero_tariff_2026, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(china_africa_zero_tariff_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, china_state_actors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, african_producers_and_exporters).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% Smallholder farmers who lack the capital to meet 'Green Channel' standards.
% Engine derives d from: victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Chinese state actors who view the framework as pure coordination.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ORGANIZED EXPORTER (TANGLED ROPE)
% Larger exporters who can comply but still face high costs and standard-setting asymmetry.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Default analytical context.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_africa_zero_tariff_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_gate) :-
    narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, _),
    narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, _),
    domain_priors:requires_active_enforcement(china_africa_zero_tariff_2026).

:- end_tests(china_africa_zero_tariff_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.32) is set above the mountain floor but below the
 *   pure snare threshold because the removal of tariffs is a genuine subsidy
 *   to trade. However, the requirement for "diplomatic relations" and
 *   "partnership pacts" creates a suppression score (0.45) representing the
 *   foreclosure of political non-alignment.
 *
 * PERSPECTIVAL GAP:
 *   China (Institutional) sees a 'Rope' because the system lowers their
 *   import costs (negative chi). African producers (Powerless/Moderate) see a
 *   'Tangled Rope' because the 'Zero Tariff' benefit is tied to 'Green Channel'
 *   compliance—a standard-setting mechanism they do not control. The gap
 *   between the powerless farmer and the organized exporter is small, as both
 *   face the same asymmetric standard.
 *
 * DIRECTIONALITY LOGIC:
 *   African producers and exporters are victims of standard-setting asymmetry
 *   (d ≈ 0.85-0.95), while China is the architect and beneficiary of the
 *   resulting resource-flow stability (d ≈ 0.05).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as 'Pure Rope' by
 *   highlighting the 'Active Enforcement' required to maintain the
 *   diplomatic and technical standards of the Green Channel, which is the
 *   primary vector of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_china_africa_2026,
    'The degree to which "Green Channel" standards function as non-tariff barriers.',
    'Review of 2027 export rejection rates for African agricultural goods.',
    'If standards are prohibitive, ε shifts from 0.32 to > 0.46 (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_china_africa_2026, empirical, 'Non-tariff barrier impact of green channel standards').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(china_africa_zero_tariff_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Anticipated rise as pacts become more performative)
narrative_ontology:measurement(catz_tr_t0, china_africa_zero_tariff_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(catz_tr_t5, china_africa_zero_tariff_2026, theater_ratio, 5, 0.15).
narrative_ontology:measurement(catz_tr_t10, china_africa_zero_tariff_2026, theater_ratio, 10, 0.20).

% Extraction over time (Drift as "Green Channel" standards tighten)
narrative_ontology:measurement(catz_ex_t0, china_africa_zero_tariff_2026, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(catz_ex_t5, china_africa_zero_tariff_2026, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(catz_ex_t10, china_africa_zero_tariff_2026, base_extractiveness, 10, 0.32).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(china_africa_zero_tariff_2026, resource_allocation).

% Network: Influences the broader African Continental Free Trade Area (AfCFTA)
% narrative_ontology:affects_constraint(china_africa_zero_tariff_2026, afcfta_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; structural derivation from beneficiary/victim + exit
% options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */