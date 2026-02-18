% ============================================================================
% CONSTRAINT STORY: international_seabed_mining_regime
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_international_seabed_mining_regime, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: international_seabed_mining_regime
 *   human_readable: International Seabed Mining Regime
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The International Seabed Authority (ISA) is mandated to regulate seabed
 *   mining in international waters. This regime is intended to balance
 *   commercial exploitation with environmental protection and benefit sharing.
 *   However, it faces challenges in effectively preventing environmental damage
 *   and ensuring equitable distribution of resources, leading to a structure
 *   with both coordination and extraction functions.
 *
 * KEY AGENTS (by structural relationship):
 *   - Developing Nations: Primary target (powerless/trapped) — bear environmental and potentially economic costs.
 *   - Mining Companies: Primary beneficiary (institutional/arbitrage) — benefit from resource extraction.
 *   - ISA Secretariat: Secondary actor (institutional/constrained) — supposed to act as regulator, but potentially subject to capture.
 *   - Analytical Community: Analytical observer (analytical/analytical) — sees full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_seabed_mining_regime, 0.55).
domain_priors:suppression_score(international_seabed_mining_regime, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(international_seabed_mining_regime, 0.40).       % Moderate theater, not high enough for Piton.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_seabed_mining_regime, extractiveness, 0.55).
narrative_ontology:constraint_metric(international_seabed_mining_regime, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(international_seabed_mining_regime, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(international_seabed_mining_regime, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(international_seabed_mining_regime, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(international_seabed_mining_regime, tangled_rope).
narrative_ontology:human_readable(international_seabed_mining_regime, "International Seabed Mining Regime").
narrative_ontology:topic_domain(international_seabed_mining_regime, "geopolitical").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(international_seabed_mining_regime).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(international_seabed_mining_regime). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(international_seabed_mining_regime).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(international_seabed_mining_regime, mining_companies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(international_seabed_mining_regime, developing_nations).
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
constraint_indexing:constraint_classification(international_seabed_mining_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(international_seabed_mining_regime, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(international_seabed_mining_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.

% Perspective 4A: ISA Secretariat (institutional, constrained exit, subject to capture)
% The regulator's function is compromised, serving both coordination and extraction.
% This is a classic tangled_rope, not a piton, as the function is subverted, not inert.
constraint_indexing:constraint_classification(international_seabed_mining_regime, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 4B: Mining Companies (institutional, arbitrage exit, beneficiary)
% This is the same as Perspective 2, included for clarity in the inter-institutional analysis.
constraint_indexing:constraint_classification(international_seabed_mining_regime, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_seabed_mining_regime_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(international_seabed_mining_regime, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_seabed_mining_regime, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(international_seabed_mining_regime, ExtMetricName, E),
    E >= 0.46. % High-extraction Tangled Rope.

test(inter_institutional_gap) :-
    % Verify gap between the captured regulator and the beneficiary company.
    constraint_indexing:constraint_classification(international_seabed_mining_regime, TypeRegulator, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(international_seabed_mining_regime, TypeCompany, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeRegulator == tangled_rope,
    TypeCompany == rope.

:- end_tests(international_seabed_mining_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This regime aims to enable seabed mining under international oversight.
 *   However, the current structure poses risks of exploitation with limited
 *   benefit sharing and potential for environmental damage. The base
 *   extractiveness score (0.55) reflects significant resource extraction potential.
 *   The suppression score (0.70) indicates limited alternatives for developing
 *   nations and environmental groups to effectively oppose mining activities
 *   given the ISA's structure and potential for regulatory capture. The theater
 *   ratio is moderate (0.40), suggesting that while there are performative
 *   elements (e.g., environmental impact assessments), the core function of
 *   enabling mining remains dominant. This value is too low for a Piton.
 *
 * PERSPECTIVAL GAP:
 *   Developing nations perceive the regime as a snare due to the potential
 *   for environmental damage and inequitable benefit sharing. Mining
 *   companies see it as a rope that provides a coordinated, legal framework
 *   to facilitate access to resources. The analytical observer sees both
 *   functions—coordination and extraction—and classifies it as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Mining companies are the primary beneficiaries, gaining access to seabed
 *   resources. Developing nations, particularly those without the capacity
 *   to engage in mining themselves, are the primary victims, bearing the
 *   environmental risks and receiving a disproportionately small share of
 *   the benefits.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The ISA Secretariat's perspective differs from that of the mining companies.
 *   While both are institutional actors, the ISA has constrained exit and is
 *   subject to capture. This means it simultaneously enables a coordination
 *   function (for miners) while facilitating asymmetric extraction (from the
 *   global commons), making its structural position a tangled rope, not a
 *   simple rope or a purely theatrical piton. The mining companies, with
 *   arbitrage exit, experience the regime as a pure coordination rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling this as pure
 *   extraction (Snare) because there is a genuine coordination function:
 *   establishing a global regulatory framework. It also prevents mislabeling
 *   it as pure coordination (Rope) because the environmental and economic
 *   risks borne by developing nations constitute a significant and asymmetric
 *   extraction component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_seabed,
    'To what extent will the ISA effectively prevent environmental damage and ensure equitable benefit sharing?',
    'Long-term monitoring of mining impacts on deep-sea ecosystems and audits of financial flows.',
    'If ineffective: Snare becomes the dominant classification. If effective: moves closer to Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(international_seabed_mining_regime, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(international_seabed_mining_regime_tr_t0, international_seabed_mining_regime, theater_ratio, 0, 0.25).
narrative_ontology:measurement(international_seabed_mining_regime_tr_t5, international_seabed_mining_regime, theater_ratio, 5, 0.35).
narrative_ontology:measurement(international_seabed_mining_regime_tr_t10, international_seabed_mining_regime, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(international_seabed_mining_regime_ex_t0, international_seabed_mining_regime, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(international_seabed_mining_regime_ex_t5, international_seabed_mining_regime, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(international_seabed_mining_regime_ex_t10, international_seabed_mining_regime, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(international_seabed_mining_regime, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(international_seabed_mining_regime, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(international_seabed_mining_regime, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% This override models the ISA's compromised position. As an institutional
% actor subject to capture, its directionality is higher (more victim-like)
% than a pure beneficiary, but not as high as a pure victim. d=0.30 reflects
% this intermediate state. Without this, the engine would derive a d value
% near 0.15 based on its institutional power and beneficiary status, failing
% to capture the capture dynamic.
constraint_indexing:directionality_override(international_seabed_mining_regime, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */