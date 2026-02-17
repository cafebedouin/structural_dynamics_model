% ============================================================================
% CONSTRAINT STORY: climate_event_attribution
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_climate_event_attribution, []).

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
 *   constraint_id: climate_event_attribution
 *   human_readable: "Scientific methodology for attributing extreme weather to climate change"
 *   domain: scientific/political
 *
 * SUMMARY:
 *   This constraint is the scientific and social process of attributing specific
 *   extreme weather events (heatwaves, floods, droughts) to anthropogenic
 *   climate change. Led by groups like World Weather Attribution (WWA), this
 *   methodology uses climate models to quantify the increased likelihood and
 *   intensity of an event due to human-caused greenhouse gas emissions. While
 *   serving a crucial coordination function for scientific consensus and
 *   policy-making, it also functions as a mechanism of extraction against
 *   fossil fuel industries by undermining their social license to operate and
 *   creating grounds for regulation and litigation.
 *
 * KEY AGENTS (by structural relationship):
 *   - fossil_fuel_industry: Primary target (powerful/constrained) — bears the cost of delegitimization, increased regulation, and legal risk.
 *   - climate_science_community: Primary beneficiary (organized/arbitrage) — gains research funding, policy relevance, and institutional authority.
 *   - intergovernmental_policy_bodies: Institutional beneficiary (institutional/arbitrage) — uses the science for policy coordination and justification.
 *   - vulnerable_nations: Secondary beneficiary (powerless/trapped) — gains leverage for "loss and damage" climate finance and international aid.
 *   - analytical_observer: Analytical observer — sees the dual nature of the constraint as both a coordination tool and an extractive mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_event_attribution, 0.55).
domain_priors:suppression_score(climate_event_attribution, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(climate_event_attribution, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_event_attribution, extractiveness, 0.55).
narrative_ontology:constraint_metric(climate_event_attribution, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_event_attribution, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this Tangled Rope constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(climate_event_attribution, tangled_rope).
narrative_ontology:human_readable(climate_event_attribution, "Scientific methodology for attributing extreme weather to climate change").

% --- Binary flags ---
% Required for Tangled Rope: enforcement of methodological consensus in science.
domain_priors:requires_active_enforcement(climate_event_attribution).

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a human-constructed methodology.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(climate_event_attribution, climate_science_community).
narrative_ontology:constraint_beneficiary(climate_event_attribution, vulnerable_nations).
narrative_ontology:constraint_beneficiary(climate_event_attribution, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(climate_event_attribution, intergovernmental_policy_bodies).


% Who bears disproportionate cost?
narrative_ontology:constraint_victim(climate_event_attribution, fossil_fuel_industry).

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
% The fossil fuel industry is powerful but constrained by the scientific consensus.
% As the designated victim, the engine derives a high d, leading to a high χ.
% From their view, this is a targeted mechanism of extraction.
constraint_indexing:constraint_classification(climate_event_attribution, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The climate science community benefits from increased authority and funding.
% As beneficiaries with arbitrage exit (can pivot research), the engine derives a
% low d, leading to low/negative χ. For them, it is a pure coordination tool.
constraint_indexing:constraint_classification(climate_event_attribution, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view sees both the coordination function and the asymmetric
% extraction. The high ε and suppression, combined with both beneficiary and
% victim groups, makes this a classic Tangled Rope.
constraint_indexing:constraint_classification(climate_event_attribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: SECONDARY BENEFICIARY (ROPE)
% Vulnerable nations are powerless and trapped by geography, but this constraint
% is a pure benefit to them, providing leverage for climate finance. This shows
% that being 'powerless' does not automatically imply being a victim of every constraint.
constraint_indexing:constraint_classification(climate_event_attribution, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: THE POLICY-MAKING INSTITUTION (ROPE)
% Intergovernmental bodies (e.g., IPCC) and national regulators use attribution
% science as a coordination mechanism to build consensus and justify policy.
% As institutional beneficiaries with policy arbitrage, they see a Rope.
constraint_indexing:constraint_classification(climate_event_attribution, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_event_attribution_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target (fossil fuel industry) and beneficiary (science community).
    constraint_indexing:constraint_classification(climate_event_attribution, snare, context(agent_power(powerful), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(climate_event_attribution, rope, context(agent_power(organized), _, exit_options(arbitrage), _)),
    true. % The presence of both clauses is the test.

test(analytical_view_matches_claim) :-
    narrative_ontology:constraint_claim(climate_event_attribution, ClaimedType),
    constraint_indexing:constraint_classification(climate_event_attribution, ClaimedType, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(climate_event_attribution, _),
    narrative_ontology:constraint_victim(climate_event_attribution, _),
    domain_priors:requires_active_enforcement(climate_event_attribution).

:- end_tests(climate_event_attribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This represents the significant cost imposed on the fossil fuel industry, measured in terms of lost social license, increased regulatory burden, stranded asset risk, and potential legal liability stemming from attribution claims. It is not a financial transfer but a structural delegitimization.
 *   - Suppression (0.75): The scientific consensus around attribution models is very strong, making it extremely difficult for counter-narratives to gain traction in policy or legal arenas. The methodology actively suppresses alternative explanations for extreme weather by framing them as unscientific.
 *   - Theater (0.15): The scientific work is genuine and methodologically rigorous, not merely performative. The low theater ratio reflects its high functional content.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the climate science community, policy institutions, and vulnerable nations, this is a Rope—a tool that coordinates scientific understanding, focuses policy, and provides leverage for aid. For the fossil fuel industry, it is a Snare—a targeted mechanism that extracts their political capital and economic future, constructed by an institution from which they cannot easily exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The constraint extracts from the `fossil_fuel_industry` (victim) and transfers benefits (authority, funding, political leverage) to the `climate_science_community`, `intergovernmental_policy_bodies`, and `vulnerable_nations` (beneficiaries). The engine's derivation of `d` from these structural roles is critical: the industry's victim status and constrained exit options result in a high `d` (d ≈ 0.8-0.9), amplifying extraction. The beneficiaries' status and arbitrage/trapped exit options result in a low `d` (d ≈ 0.1-0.2), creating a coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the constraint, avoiding two common errors. It is not a pure Snare, because it possesses a genuine and powerful coordination function for scientific progress and international policy. It is not a pure Rope, because it imposes severe, asymmetric costs on a clearly defined group. The Tangled Rope classification captures this duality, which is essential for understanding the political conflict surrounding climate science.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_climate_event_attribution,
    'To what extent does political pressure for conclusive results bias attribution models beyond what the underlying climate data can robustly support?',
    'Independent, adversarial audits of WWA models and source code by hostile but technically proficient third parties.',
    'If bias is low, the Tangled Rope classification holds. If bias is high, the theater ratio would increase and the constraint would degrade towards a Piton, where scientific performance masks a political goal.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(climate_event_attribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction (ε=0.55) constraint.
% The interval [0, 10] maps roughly to [2004-2024], as the science matured.
% Extraction increased as the methodology gained confidence and political weight.

% Theater ratio over time (slight increase as science became more politicized):
narrative_ontology:measurement(climate_event_attribution_tr_t0, climate_event_attribution, theater_ratio, 0, 0.05).
narrative_ontology:measurement(climate_event_attribution_tr_t5, climate_event_attribution, theater_ratio, 5, 0.10).
narrative_ontology:measurement(climate_event_attribution_tr_t10, climate_event_attribution, theater_ratio, 10, 0.15).

% Extraction over time (growing impact on the fossil fuel industry):
narrative_ontology:measurement(climate_event_attribution_ex_t0, climate_event_attribution, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(climate_event_attribution_ex_t5, climate_event_attribution, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(climate_event_attribution_ex_t10, climate_event_attribution, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(climate_event_attribution, information_standard).

% --- Network Decomposition (Constraint Families) ---
% The methodology of climate attribution is built upon the foundation of
% fundamental climate physics. We model this as a dependency.

% DUAL FORMULATION NOTE:
% This constraint is one of 2+ stories decomposed from the general concept of "climate change".
% Decomposed because ε differs across observables (ε-invariance principle).
% This story models the socio-political methodology (Tangled Rope). The underlying
% physical law is a separate, Mountain-class constraint.
% Related stories:
%   - ghg_forcing_thermodynamics (ε=0.02, Mountain)
%   - fossil_fuel_divestment_campaigns (ε=0.60, Snare)

narrative_ontology:affects_constraint(ghg_forcing_thermodynamics, climate_event_attribution).
narrative_ontology:affects_constraint(climate_event_attribution, fossil_fuel_divestment_campaigns).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural declarations of
% beneficiary/victim combined with the distinct exit options (constrained vs.
% arbitrage) are sufficient for the engine to derive accurate and
% differentiated directionality (d) values for the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */