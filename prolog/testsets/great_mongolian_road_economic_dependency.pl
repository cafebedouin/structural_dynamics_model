% ============================================================================
% CONSTRAINT STORY: great_mongolian_road_economic_dependency
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_great_mongolian_road_economic_dependency, []).

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
 *   constraint_id: great_mongolian_road_economic_dependency
 *   human_readable: Economic Dependency via Infrastructure Development
 *   domain: economic
 *
 * SUMMARY:
 *   The Great Mongolian Road project, spearheaded by Japan, aims to improve Mongolia's infrastructure. However, this development comes with the risk of increased economic dependency on Japan, creating a constraint on Mongolia's economic sovereignty and potentially exacerbating existing power imbalances. This story focuses on that potential dynamic.
 *
 * KEY AGENTS (by structural relationship):
 *   - Local Mongolian communities: Most vulnerable target (powerless/trapped) — face displacement and loss of livelihood with no recourse.
 *   - Mongolia (as a state): Primary target (moderate/constrained) — faces increased economic dependency.
 *   - Japan: Primary beneficiary (institutional/arbitrage) — gains economic influence and resource access.
 *   - China: Secondary actor (institutional/mobile) — influenced by the infrastructure development.
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(great_mongolian_road_economic_dependency, 0.59).
domain_priors:suppression_score(great_mongolian_road_economic_dependency, 0.62).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(great_mongolian_road_economic_dependency, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, extractiveness, 0.59).
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(great_mongolian_road_economic_dependency, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(great_mongolian_road_economic_dependency).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(great_mongolian_road_economic_dependency). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(great_mongolian_road_economic_dependency).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(great_mongolian_road_economic_dependency, japan).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(great_mongolian_road_economic_dependency, mongolia).
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

% PERSPECTIVE 1: THE MOST VULNERABLE TARGET (SNARE)
% Local communities who are powerless and trapped. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (TANGLED ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% NOTE: Classification is Tangled Rope, not Rope, because base extractiveness (ε=0.59)
% is too high for the Rope gate (ε ≤ 0.45). The classification reflects the
% underlying structure, while the negative χ reflects the beneficiary's experience.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE NATION-STATE TARGET (SNARE)
% Mongolia as a state actor. Victim membership + constrained exit options
% result in a high derived directionality, pushing χ into the snare range.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: A COMPETING INSTITUTION (TANGLED ROPE)
% China's perspective. The road could benefit China (coordination) but also
% reduce its regional influence (extraction). This ambiguity is characteristic
% of a Tangled Rope. The classification is not Rope because ε is too high.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE ORGANIZED TARGET (TANGLED ROPE)
% Mongolia with strong coalition power. Organization provides leverage to
% negotiate terms and mitigate the worst extraction, shifting the perception
% from an inescapable Snare to a negotiable Tangled Rope.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(great_mongolian_road_economic_dependency_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the most vulnerable target and the beneficiary.
    constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(great_mongolian_road_economic_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε) is set high at 0.59 and suppression at 0.62. These values are chosen to reflect the significant potential for the project to create a coercive dependency trap, which would qualify as a Snare for those with the least power to negotiate or exit. The theater ratio is low (0.20) as the infrastructure project has a clear, primary function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For powerless local communities and even the Mongolian state (moderate power), the high extraction and suppression create a Snare. They experience the constraint as purely coercive. For the beneficiary, Japan, the constraint is a Tangled Rope. The high base extraction (ε > 0.45) makes a 'Rope' classification impossible; the classification must reflect the underlying extractive structure. Japan experiences this structure as beneficial (negative effective extraction χ), but the system correctly identifies the structure's dual nature. Observers and organized actors also see the Tangled Rope, acknowledging both the coordination benefits and the extractive risks.
 *
 * DIRECTIONALITY LOGIC:
 *   Japan is the clear beneficiary, gaining economic influence and resource access. Mongolia is the victim, bearing the risk of dependency and loss of sovereignty. These declarations drive the directionality calculation, leading to a negative χ for Japan (beneficiary with arbitrage) and a high positive χ for Mongolia (victim with constrained/trapped exit).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the constraint as a Tangled Rope from the beneficiary and analytical perspectives is crucial. It prevents mislabeling a high-extraction project as a purely coordinative Rope, correctly identifying that even when one party benefits, the structure contains inherent, asymmetric extraction. The Snare classification for the powerless correctly identifies that for those without agency, the coordination function is irrelevant, and the experience is one of pure coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_great_mongolian_road,
    'Will Mongolia be able to diversify its economy and avoid excessive dependence on Japan due to the Great Mongolian Road?',
    'Monitoring Mongolia''s economic policies and trade diversification efforts over the next decade.',
    'If True: The constraint will diminish over time, becoming less of a snare. If False: The constraint will intensify, potentially solidifying as a snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(great_mongolian_road_economic_dependency, 0, 10).

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
narrative_ontology:measurement(great_mongolian_road_tr_t0, great_mongolian_road_economic_dependency, theater_ratio, 0, 0.15).
narrative_ontology:measurement(great_mongolian_road_tr_t5, great_mongolian_road_economic_dependency, theater_ratio, 5, 0.18).
narrative_ontology:measurement(great_mongolian_road_tr_t10, great_mongolian_road_economic_dependency, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(great_mongolian_road_ex_t0, great_mongolian_road_economic_dependency, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(great_mongolian_road_ex_t5, great_mongolian_road_economic_dependency, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(great_mongolian_road_ex_t10, great_mongolian_road_economic_dependency, base_extractiveness, 10, 0.59).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(great_mongolian_road_economic_dependency, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(great_mongolian_road_economic_dependency, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(great_mongolian_road_economic_dependency, [other_constraint_id]).

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
% narrative_ontology:affects_constraint(great_mongolian_road_economic_dependency, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(great_mongolian_road_economic_dependency, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */