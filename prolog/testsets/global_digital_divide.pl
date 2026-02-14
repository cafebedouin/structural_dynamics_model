% ============================================================================
% CONSTRAINT STORY: global_digital_divide
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-29
% ============================================================================

:- module(constraint_global_digital_divide, []).

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
 *   constraint_id: global_digital_divide
 *   human_readable: Global Digital Divide
 *   domain: economic
 *
 * SUMMARY:
 *   The global digital divide represents the unequal access to digital technologies and internet connectivity based on geographic location, economic status, and socio-political factors. This divide creates disparities in information access, education, economic opportunities, and civic participation, perpetuating existing inequalities and hindering inclusive development.
 *
 * KEY AGENTS (by structural relationship):
 *   - Disconnected populations: Primary target (powerless/trapped) — bears extraction
 *   - Tech companies: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Governments/Regulators: Secondary actor (institutional/constrained)
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_digital_divide, 0.55).
domain_priors:suppression_score(global_digital_divide, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(global_digital_divide, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_digital_divide, extractiveness, 0.55).
narrative_ontology:constraint_metric(global_digital_divide, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(global_digital_divide, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(global_digital_divide, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(global_digital_divide, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(global_digital_divide, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(global_digital_divide).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(global_digital_divide). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(global_digital_divide).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(global_digital_divide, tech_companies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(global_digital_divide, disconnected_populations).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(global_digital_divide, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(global_digital_divide, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(global_digital_divide, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE CONSTRAINED REGULATOR (TANGLED ROPE)
% An institutional actor (e.g., government) whose exit options are constrained
% by political or economic factors, leading them to experience the constraint
% more extractively than the primary beneficiary.
constraint_indexing:constraint_classification(global_digital_divide, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_digital_divide_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(global_digital_divide, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_digital_divide, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(global_digital_divide, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(global_digital_divide_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.55, reflecting the significant economic value captured by tech companies and service providers from the existing digital infrastructure, while large populations remain excluded from these benefits. Suppression is high (0.70) due to the immense capital costs of building alternative global infrastructure, coupled with regulatory capture and patent thickets that prevent new entrants. The theater ratio is low (0.30) because while there are many public initiatives to "bridge the gap," their functional impact is limited compared to the scale of the problem; they are not the dominant activity.
 *
 * PERSPECTIVAL GAP:
 *   For disconnected populations (powerless, trapped), the divide is a Snare, locking them out of economic, educational, and civic opportunities available to the connected world. For tech companies (institutional, arbitrage), the existing infrastructure is a Rope, a coordination mechanism that enables their global business models. The analytical view reveals a Tangled Rope: a system with a genuine coordination function (the internet works for those who can access it) but which also produces severe, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are tech companies, ISPs, and hardware manufacturers who profit from the current state of digital infrastructure. The victims are populations in low-income countries, rural areas, and marginalized communities who bear the opportunity cost of non-participation. This structural relationship directly informs the directionality calculation.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   Governments and international regulators often find themselves in a 'constrained' position. They are institutionally powerful but lack the resources or political will to build public alternatives, making them dependent on the private sector. This constrained exit leads them to perceive the system as a Tangled Rope, acknowledging both its function and its extractive nature, unlike the unconstrained beneficiaries who see it as a pure Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the digital divide. It is not a pure Snare, because the internet provides immense coordination benefits. However, classifying it as a Rope would ignore the massive, structurally-enforced extraction. The Tangled Rope classification captures this essential conflict, preventing mislabeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_global_digital_divide,
    'Is the digital divide a temporary market failure correctable with investment, or a permanent structural feature of a centralized internet architecture?',
    'Comparative analysis of state-led vs. market-led connectivity initiatives and the adoption rate of decentralized network protocols.',
    'If a temporary failure, it is a Scaffold that can be dismantled. If a permanent feature, it is a durable Tangled Rope requiring continuous regulation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(global_digital_divide, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(global_digital_divide_tr_t0, global_digital_divide, theater_ratio, 0, 0.20).
narrative_ontology:measurement(global_digital_divide_tr_t5, global_digital_divide, theater_ratio, 5, 0.25).
narrative_ontology:measurement(global_digital_divide_tr_t10, global_digital_divide, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(global_digital_divide_ex_t0, global_digital_divide, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(global_digital_divide_ex_t5, global_digital_divide, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(global_digital_divide_ex_t10, global_digital_divide, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(global_digital_divide, global_infrastructure).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(global_digital_divide, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(global_digital_divide, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */