% ============================================================================
% CONSTRAINT STORY: fiscal_equalization_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiscal_equalization_friction, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fiscal_equalization_friction
 *   human_readable: The Equalization Conflict (Net Transfer Friction)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Canadian fiscal equalization program is designed to reduce fiscal
 *   disparities among provinces, ensuring that all provinces can provide
 *   reasonably comparable levels of public services. This mechanism involves
 *   transferring funds from wealthier "contributing" provinces to less
 *   wealthy "receiving" provinces. While intended to foster national unity
 *   and social equity, the equalization program generates significant
 *   interprovincial conflict and economic friction. Contributing provinces
 *   often perceive the program as unfair extraction, while receiving
 *   provinces may become dependent on the payments, creating a tangled web of
 *   political and economic incentives.
 *
 * KEY AGENTS:
 *   - Contributing Provinces: Primary victims (institutional/constrained) — bear the cost of equalization transfers.
 *   - Receiving Provinces: Primary beneficiaries (institutional/constrained) — benefit from equalization transfers.
 *   - Federal Government: Intermediary (institutional/constrained) — manages the equalization program, balancing national unity with provincial interests.
 *   - Interprovincial Harmony: Abstract collective (powerless/trapped) — suffers when equalization exacerbates interprovincial tensions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiscal_equalization_friction, 0.55).
domain_priors:suppression_score(fiscal_equalization_friction, 0.4).
domain_priors:theater_ratio(fiscal_equalization_friction, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiscal_equalization_friction, extractiveness, 0.55).
narrative_ontology:constraint_metric(fiscal_equalization_friction, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fiscal_equalization_friction, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiscal_equalization_friction, tangled_rope).
narrative_ontology:human_readable(fiscal_equalization_friction, "The Equalization Conflict (Net Transfer Friction)").
narrative_ontology:topic_domain(fiscal_equalization_friction, "economic/political").

domain_priors:requires_active_enforcement(fiscal_equalization_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiscal_equalization_friction, receiving_provinces).
narrative_ontology:constraint_beneficiary(fiscal_equalization_friction, federal_government_legitimacy).
narrative_ontology:constraint_victim(fiscal_equalization_friction, contributing_provinces).
narrative_ontology:constraint_victim(fiscal_equalization_friction, interprovincial_harmony).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Taxpayers in contributing provinces experience this as a snare. They are trapped within the national fiscal framework and are subject to continuous extraction with little direct benefit. Their taxes are redistributed with limited control over the outcome.
constraint_indexing:constraint_classification(fiscal_equalization_friction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The governments of receiving provinces view the equalization payments as a rope, a coordination mechanism that allows them to provide comparable public services. They are structurally dependent on these funds and have some influence through federal negotiations, leading to a rope-like relationship.
constraint_indexing:constraint_classification(fiscal_equalization_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The federal government experiences a tangled rope: it benefits from the perceived legitimacy of the equalization program (political coordination), but also faces political costs and constraints managing interprovincial tensions and ensuring compliance with the program's rules. The government is constrained by needing to maintain national unity while extracting from some provinces to give to others.
constraint_indexing:constraint_classification(fiscal_equalization_friction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observer sees the mixed coordination/extraction: Equalization attempts to coordinate fiscal capacity across provinces but generates political friction and economic distortions as some provinces effectively subsidize others. Active enforcement is required due to the inherent tension between contributing and receiving provinces. Extraction is offset by the coordination function of national unity.
constraint_indexing:constraint_classification(fiscal_equalization_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_equalization_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fiscal_equalization_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_equalization_friction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fiscal_equalization_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fiscal_equalization_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The equalization program involves a significant transfer of wealth from contributing provinces to receiving provinces. While not purely extractive (due to the coordination function of national unity), the net flow is substantial, leading to a moderate level of base extractiveness. Suppression (0.40): Contributing provinces have limited exit options (constrained by the constitutional framework) and political avenues to challenge the equalization formula. This creates a moderate degree of suppression. Theater Ratio (0.20): While there is some performative element in federal-provincial negotiations, the equalization program is largely functional, with relatively low theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ based on the economic and political position of each province. Contributing provinces view the program as an unfair extraction (snare), while receiving provinces see it as a necessary coordination mechanism (rope). The federal government attempts to balance these competing interests (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the structural relationship of each actor to the equalization program. Contributing provinces are victims, so their d value is high, leading to a high effective extraction (chi). Receiving provinces are beneficiaries, so their d value is low, resulting in a low (or even negative) effective extraction. The federal government is an intermediary, so its d value is moderate, reflecting the mixed nature of its relationship to the program.
 *
 * MANDATROPHY ANALYSIS:
 *   The equalization program is not purely extractive because it serves a coordination function: it aims to ensure that all provinces can provide a minimum level of public services, thereby contributing to national unity and social equity. This coordination function prevents the program from being classified as a pure snare. However, the program also involves a significant transfer of wealth from contributing provinces to receiving provinces, which generates political friction and economic distortions. This extraction prevents the program from being classified as a pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revenue_standardization_methodology,
    'Is the current methodology for standardizing provincial revenues accurately reflecting fiscal capacity?',
    'Comparative analysis of alternative revenue standardization methodologies; econometric modeling of provincial fiscal capacity under various assumptions.',
    'If the methodology is flawed, equalization payments may be misdirected, exacerbating interprovincial tensions and undermining the program''s legitimacy. A refined methodology may shift the burdens and benefits of the program.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_standardization_methodology, empirical, 'Validity of the revenue standardization methodology.').

omega_variable(
    fiscal_dependency_incentives,
    'Does the equalization program create perverse incentives for receiving provinces to underperform economically?',
    'Econometric modeling of the relationship between equalization payments and provincial economic performance; case studies of specific provinces'' economic policies.',
    'If fiscal dependency is significant, reforms to the equalization formula may be necessary to encourage economic growth in receiving provinces. Identifying specific disincentives could allow tailored policy interventions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_dependency_incentives, empirical, 'Incentive effects on provincial economic performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiscal_equalization_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisc_tr_t0, fiscal_equalization_friction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fisc_tr_t5, fiscal_equalization_friction, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fisc_tr_t10, fiscal_equalization_friction, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(fisc_be_t0, fiscal_equalization_friction, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fisc_be_t5, fiscal_equalization_friction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fisc_be_t10, fiscal_equalization_friction, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiscal_equalization_friction, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
