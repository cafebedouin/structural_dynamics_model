% ============================================================================
% CONSTRAINT STORY: ny_private_school_discount
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_ny_private_school_discount, []).

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
 *   constraint_id: ny_private_school_discount
 *   human_readable: Discount-for-Data Scheme in Private Schools
 *   domain: economic
 *
 * SUMMARY:
 *   A new private school startup offers a significant tuition discount (42%)
 *   in exchange for parents providing extensive data about their children.
 *   This creates a constraint on families needing affordable private education,
 *   as they must sacrifice data privacy for access.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-Income Parents: Primary target (powerless/trapped) — bears extraction
 *   - School Startup: Primary beneficiary (institutional/arbitrage) — benefits from data & tuition
 *   - Wealthy Parents: Alternative option (powerful/mobile)
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ny_private_school_discount, 0.48). % Moderate, as the choice is presented as a positive discount rather than an unavoidable fee
domain_priors:suppression_score(ny_private_school_discount, 0.55). % Moderate, alternative schools exist, but the discount is significant enough to suppress other options.
domain_priors:theater_ratio(ny_private_school_discount, 0.20).       % Low, the data collected directly informs pedagogical choices.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ny_private_school_discount, extractiveness, 0.48).
narrative_ontology:constraint_metric(ny_private_school_discount, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ny_private_school_discount, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ny_private_school_discount, tangled_rope).
narrative_ontology:human_readable(ny_private_school_discount, "Discount-for-Data Scheme in Private Schools").

% --- Binary flags ---
domain_priors:requires_active_enforcement(ny_private_school_discount). % Active enforcement is required through the contractual agreement and data collection infrastructure.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ny_private_school_discount, school_startup).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ny_private_school_discount, low_income_parents).

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
constraint_indexing:constraint_classification(ny_private_school_discount, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(ny_private_school_discount, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(ny_private_school_discount, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ny_private_school_discount_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(ny_private_school_discount, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ny_private_school_discount, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ny_private_school_discount, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(ny_private_school_discount_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score is relatively moderate, as it reflects a choice
 *   presented as a positive (discount) rather than an unavoidable cost.
 *   Suppression reflects that other options exist (public schools,
 *   other private schools), but the discount creates a significant incentive
 *   that reduces the accessibility of alternatives. The theater_ratio is
 *   low because the data collection is intended to improve the educational
 *   experience, not just for marketing or PR purposes.
 *
 * PERSPECTIVAL GAP:
 *   Low-income parents experience this as a snare because they may feel
 *   compelled to sacrifice their children's data privacy to afford private
 *   education. The school startup views it as a rope because they believe
 *   they are creating a beneficial coordination mechanism: affordable
 *   education in exchange for valuable data. The analytical observer sees
 *   the hybrid of coordination and asymmetric extraction, hence Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The school startup benefits by gaining access to valuable data, which
 *   can be used to improve their educational programs and attract more
 *   students. They have an arbitrage exit option as they can always refine
 *   their data practices. Low-income parents bear the cost of sacrificing
 *   data privacy for affordability. Their exit option is limited,
 *   putting them in a trapped position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint can prevent mislabeling pure coordination by requiring the school to actively use the collected data to improve the education, and be transparent with the parents how the data is being used. It also enforces a sunset clause to limit the use of data to the specific needs of the education.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ny_private_school_discount,
    'How valuable is the collected data to the school and the long-term privacy implications for the children?',
    'Longitudinal study on data usage and tracking impact on student outcomes.',
    'Snare classification if data is not actually used and privacy risks are high; Tangled Rope if data is used ethically and effectively.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ny_private_school_discount, 0, 10).

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
narrative_ontology:measurement(ny_private_school_discount_tr_t0, ny_private_school_discount, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ny_private_school_discount_tr_t5, ny_private_school_discount, theater_ratio, 5, 0.20).
narrative_ontology:measurement(ny_private_school_discount_tr_t10, ny_private_school_discount, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ny_private_school_discount_ex_t0, ny_private_school_discount, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ny_private_school_discount_ex_t5, ny_private_school_discount, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ny_private_school_discount_ex_t10, ny_private_school_discount, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(ny_private_school_discount, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */