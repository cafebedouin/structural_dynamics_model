% ============================================================================
% CONSTRAINT STORY: p_g_golden_pear_surveillance
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-31
% ============================================================================

:- module(constraint_p_g_golden_pear_surveillance, []).

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
 *   constraint_id: p_g_golden_pear_surveillance
 *   human_readable: Procter & Gamble's Golden Pear Microcontent Surveillance
 *   domain: economic
 *
 * SUMMARY:
 *   Procter & Gamble (P&G) is creating microcontent "soap operas" called "The Golden Pear Affair" to maintain brand relevance and capture consumer attention. This involves collecting consumer data through engagement with the microcontent, creating a surveillance system to optimize product placement and marketing strategies. This constraint focuses on the inherent power asymmetry in data collection for targeted marketing.
 *
 * KEY AGENTS (by structural relationship):
 *   - Consumers: Primary target (powerless/trapped) — bears extraction of attention and data.
 *   - Procter & Gamble: Primary beneficiary (institutional/arbitrage) — benefits from data collection and targeted marketing.
 *   - Advertising Ecosystem: Secondary actor (organized/mobile) - benefits from the data flow and targeted advertising.
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(p_g_golden_pear_surveillance, 0.55).
domain_priors:suppression_score(p_g_golden_pear_surveillance, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(p_g_golden_pear_surveillance, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(p_g_golden_pear_surveillance, extractiveness, 0.55).
narrative_ontology:constraint_metric(p_g_golden_pear_surveillance, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(p_g_golden_pear_surveillance, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(p_g_golden_pear_surveillance, snare).
narrative_ontology:human_readable(p_g_golden_pear_surveillance, "Procter & Gamble's Golden Pear Microcontent Surveillance").
narrative_ontology:topic_domain(p_g_golden_pear_surveillance, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(p_g_golden_pear_surveillance). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(p_g_golden_pear_surveillance, procter_gamble).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(p_g_golden_pear_surveillance, consumers).
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
constraint_indexing:constraint_classification(p_g_golden_pear_surveillance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(p_g_golden_pear_surveillance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(p_g_golden_pear_surveillance, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(p_g_golden_pear_surveillance_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(p_g_golden_pear_surveillance, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(p_g_golden_pear_surveillance, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(p_g_golden_pear_surveillance, ExtMetricName, E),
    E >= 0.46. % Mountain or high-extraction Snare/Tangled.

:- end_tests(p_g_golden_pear_surveillance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores are assigned based on the extractiveness of consumer data and attention, and the suppression of alternative marketing strategies due to the effectiveness of targeted advertising. The theater ratio is relatively low as the microcontent serves a genuine marketing purpose beyond pure performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   Consumers perceive this as a snare due to the feeling of being tracked and manipulated for advertising purposes. Procter & Gamble views it as a rope, a way to efficiently coordinate marketing efforts and maintain brand relevance.
 *
 * DIRECTIONALITY LOGIC:
 *   Procter & Gamble benefits by gaining insights into consumer behavior and optimizing product placement, resulting in increased sales. Consumers bear the cost of having their data and attention extracted and used for targeted advertising, which can feel intrusive and manipulative.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a snare prevents mislabeling this as a purely coordinated marketing effort. While P&G gains efficiency, the extraction from consumers outweighs the coordination benefits from the consumer perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_p_g_surveillance,
    'To what extent do consumers actively consent to data collection for personalized advertising?',
    'Longitudinal studies of consumer awareness and attitudes towards data privacy.',
    'If true (high consent), the constraint might be better described as a Tangled Rope. If false (low consent), the constraint is more definitively a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(p_g_golden_pear_surveillance, 0, 10).

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
narrative_ontology:measurement(p_g_golden_pear_surveillance_tr_t0, p_g_golden_pear_surveillance, theater_ratio, 0, 0.10).
narrative_ontology:measurement(p_g_golden_pear_surveillance_tr_t5, p_g_golden_pear_surveillance, theater_ratio, 5, 0.15).
narrative_ontology:measurement(p_g_golden_pear_surveillance_tr_t10, p_g_golden_pear_surveillance, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(p_g_golden_pear_surveillance_ex_t0, p_g_golden_pear_surveillance, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(p_g_golden_pear_surveillance_ex_t5, p_g_golden_pear_surveillance, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(p_g_golden_pear_surveillance_ex_t10, p_g_golden_pear_surveillance, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(p_g_golden_pear_surveillance, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */