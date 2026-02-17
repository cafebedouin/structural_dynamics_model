% ============================================================================
% CONSTRAINT STORY: guano_wealth_extraction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-14
% ============================================================================

:- module(constraint_guano_wealth_extraction, []).

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
 *   constraint_id: guano_wealth_extraction
 *   human_readable: Guano-based Wealth Extraction
 *   domain: economic
 *
 * SUMMARY:
 *   The Chincha Islands guano deposits enabled the Chincha culture to thrive from 800 AD to 1400 AD. However, this natural resource also created a vulnerability to external extraction when the Inca Empire conquered the region, centralizing control over guano distribution and essentially turning the Chincha people into suppliers. The shift from local control to centralized Inca rule transformed a resource that once fueled local prosperity into a tool for imperial wealth extraction.
 *
 * KEY AGENTS (by structural relationship):
 *   - Chincha People: Primary target (powerless/trapped) — bears extraction
 *   - Inca Empire: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guano_wealth_extraction, 0.55).
domain_priors:suppression_score(guano_wealth_extraction, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(guano_wealth_extraction, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guano_wealth_extraction, extractiveness, 0.55).
narrative_ontology:constraint_metric(guano_wealth_extraction, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(guano_wealth_extraction, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(guano_wealth_extraction, tangled_rope).
narrative_ontology:human_readable(guano_wealth_extraction, "Guano-based Wealth Extraction").

% --- Binary flags ---
domain_priors:requires_active_enforcement(guano_wealth_extraction). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(guano_wealth_extraction, inca_empire).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(guano_wealth_extraction, chincha_people).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(guano_wealth_extraction, snare,
    context(agent_power(powerless),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(guano_wealth_extraction, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(guano_wealth_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guano_wealth_extraction_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(guano_wealth_extraction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guano_wealth_extraction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(guano_wealth_extraction, ExtMetricName, E),
    E >= 0.46. % High-extraction Snare/Tangled.

:- end_tests(guano_wealth_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The guano wealth extraction represents a classic case of a resource-rich region being exploited by a more powerful entity. The base extractiveness is set to 0.55, reflecting the substantial transfer of wealth from the Chincha people to the Inca Empire. The suppression score is 0.70, indicating that the Inca Empire actively limited the Chincha people's autonomy and control over the resource.
 *
 * PERSPECTIVAL GAP:
 *   The Chincha people experience this constraint as a snare, as they are powerless to prevent the Inca's extraction of guano. The Inca Empire, on the other hand, views it as a rope, representing a coordinated effort to distribute resources across the empire, albeit one that disproportionately benefits them. The analytical observer recognizes the hybrid nature of the constraint as a tangled rope, acknowledging both the coordination function (resource distribution) and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Chincha people are victims, as they bear the costs of the Inca's resource extraction. The Inca Empire benefits, as it gains access to a valuable fertilizer that enhances agricultural productivity across its territories. The beneficiary/victim declarations map to the structural relationship between the two groups: the Inca Empire exerted political and military control over the Chincha people, enabling them to extract guano with minimal resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a tangled rope prevents mislabeling the situation as pure extraction (a snare) or pure coordination (a rope). While the Inca's actions involved a degree of coordination in distributing the guano, the core dynamic was one of asymmetric extraction, with the Chincha people bearing a disproportionate cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_guano_enforcement_level,
    'To what extent did the Inca actively *enforce* the guano extraction, versus relying on pre-existing tribute systems?',
    'Archaeological evidence, historical records of Inca administration and military presence.',
    'High enforcement increases the suppression score, potentially shifting the analytical classification towards a more extractive snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(guano_wealth_extraction, 0, 10).

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
narrative_ontology:measurement(guano_wealth_extraction_tr_t0, guano_wealth_extraction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(guano_wealth_extraction_tr_t5, guano_wealth_extraction, theater_ratio, 5, 0.20).
narrative_ontology:measurement(guano_wealth_extraction_tr_t10, guano_wealth_extraction, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(guano_wealth_extraction_ex_t0, guano_wealth_extraction, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(guano_wealth_extraction_ex_t5, guano_wealth_extraction, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(guano_wealth_extraction_ex_t10, guano_wealth_extraction, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(guano_wealth_extraction, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */