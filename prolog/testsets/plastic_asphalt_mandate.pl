% ============================================================================
% CONSTRAINT STORY: plastic_asphalt_mandate
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_plastic_asphalt_mandate, []).

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
 *   constraint_id: plastic_asphalt_mandate
 *   human_readable: Government Mandate for Plastic-Infused Asphalt
 *   domain: economic/environmental/political
 *
 * SUMMARY:
 *   A government mandate requires that all new asphalt road construction and repair projects incorporate a specific percentage of recycled plastic. This is intended to reduce plastic waste and improve road durability. However, it also creates a captive market for specific plastic suppliers and increases costs for road construction, disproportionately affecting smaller operators.
 *
 * KEY AGENTS (by structural relationship):
 *   - small_contractors_and_municipalities: Primary target (powerless/trapped) — bears full extraction with no recourse.
 *   - road_construction_companies: Secondary target (moderate/constrained) — bears extraction but has some capacity to adapt or lobby.
 *   - plastic_recycling_companies: Primary beneficiary (institutional/arbitrage) — benefits from guaranteed demand.
 *   - government_regulators: Enforcer (institutional/constrained) — tasked with enforcing the mandate.
 *   - analytical_observer: Analytical observer (analytical/analytical) — sees full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plastic_asphalt_mandate, 0.48). % Medium extractiveness - compliance costs, potentially higher material costs.
domain_priors:suppression_score(plastic_asphalt_mandate, 0.55).   % Moderate suppression - limits choices of asphalt composition, potential for regulatory capture limiting alternatives.
domain_priors:theater_ratio(plastic_asphalt_mandate, 0.30).       % Relatively low theater - the primary goal is material substitution, not window dressing.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plastic_asphalt_mandate, extractiveness, 0.48).
narrative_ontology:constraint_metric(plastic_asphalt_mandate, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(plastic_asphalt_mandate, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(plastic_asphalt_mandate, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(plastic_asphalt_mandate, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(plastic_asphalt_mandate, tangled_rope).
narrative_ontology:human_readable(plastic_asphalt_mandate, "Government Mandate for Plastic-Infused Asphalt").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(plastic_asphalt_mandate).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(plastic_asphalt_mandate). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(plastic_asphalt_mandate).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, plastic_recycling_companies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(plastic_asphalt_mandate, road_construction_companies).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, small_contractors_and_municipalities).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: THE POWERLESS TARGET (SNARE)
% Small contractors or municipalities with no lobbying power or ability to absorb costs.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE MODERATE TARGET (SNARE)
% Larger road construction companies who are constrained but not entirely trapped.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (ROPE)
% Large recycling companies who benefit from the created market.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context, recognizing both coordination and extraction.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ENFORCER (ROPE)
% The government regulator, constrained by the mandate they must enforce.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plastic_asphalt_mandate_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(plastic_asphalt_mandate, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plastic_asphalt_mandate, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(plastic_asphalt_mandate, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(plastic_asphalt_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The mandate has a medium level of base extractiveness (0.48) because road construction companies face potential cost increases and compliance burdens. The suppression score (0.55) is medium because while companies are technically free to choose their suppliers, the mandate effectively reduces their degrees of freedom in asphalt composition. The low theater ratio (0.30) reflects that the mandate is primarily driven by practical goals (waste reduction and road improvement), not performative ones.
 *
 * PERSPECTIVAL GAP:
 *   Small contractors (powerless/trapped) experience the mandate as a pure Snare because it imposes costs and compliance burdens they cannot avoid or absorb. Plastic recycling companies (institutional/arbitrage), however, view it as a Rope—a beneficial coordination mechanism that creates a stable, guaranteed market for their product. The analytical view resolves this as a Tangled Rope, acknowledging both the genuine coordination goal (waste reduction) and the asymmetric extraction from construction firms.
 *
 * DIRECTIONALITY LOGIC:
 *   Plastic recycling companies are the beneficiaries because the mandate increases the demand for their product. Road construction companies and smaller municipal operators are the victims because they are forced to use a specific material and potentially incur higher costs, or face penalties for non-compliance. Government regulators are both beneficiaries (fulfilling policy goals) and targets (increased oversight responsibilities).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *  The regulator perspective is included to account for the role of governmental bodies in both pushing for and overseeing the mandate's enactment. While the regulator benefits from enacting this policy, it is also constrained by political pressures from lobbying groups and faces scrutiny from affected road construction companies, hence the `constrained` exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this as pure extraction (Snare) from an analytical view because there is a genuine coordination function – reducing plastic waste and improving road durability. It also avoids mislabeling it as pure coordination (Rope) because road construction companies bear a disproportionate cost, making this a hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_plastic_effectiveness,
    'Is plastic-infused asphalt significantly more durable and cost-effective than traditional asphalt in the long run?',
    'Long-term field studies comparing performance of both types of asphalt in various climates.',
    'If true: the mandate becomes a more legitimate coordination mechanism (closer to Rope). If false: the mandate is primarily extractive and wasteful (closer to Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(plastic_asphalt_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(plastic_asphalt_mandate_tr_t0, plastic_asphalt_mandate, theater_ratio, 0, 0.25).
narrative_ontology:measurement(plastic_asphalt_mandate_tr_t5, plastic_asphalt_mandate, theater_ratio, 5, 0.35).
narrative_ontology:measurement(plastic_asphalt_mandate_tr_t10, plastic_asphalt_mandate, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(plastic_asphalt_mandate_ex_t0, plastic_asphalt_mandate, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(plastic_asphalt_mandate_ex_t5, plastic_asphalt_mandate, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(plastic_asphalt_mandate_ex_t10, plastic_asphalt_mandate, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(plastic_asphalt_mandate, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(plastic_asphalt_mandate, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(plastic_asphalt_mandate, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(plastic_asphalt_mandate, [sibling_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(plastic_asphalt_mandate, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */