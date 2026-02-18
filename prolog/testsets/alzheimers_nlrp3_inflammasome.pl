% ============================================================================
% CONSTRAINT STORY: alzheimers_nlrp3_inflammasome
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_alzheimers_nlrp3_inflammasome, []).

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
 *   constraint_id: alzheimers_nlrp3_inflammasome
 *   human_readable: Alzheimer's Disease Pathogenesis via NLRP3 Inflammasome
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint models the biological mechanism of Alzheimer's-related
 *   cognitive decline as driven by the NLRP3 inflammasome pathway. The
 *   pathway, when pathologically activated, extracts cognitive function,
 *   memory, and quality of life from individuals. The constraint is the
 *   biological reality that must be overcome by therapeutic intervention,
 *   as described in recent research on reversing its effects in mice.
 *
 * KEY AGENTS (by structural relationship):
 *   - alzheimers_patients: Primary target (powerless/trapped) — bear the full
 *     extraction of cognitive function.
 *   - pharma_and_care_industry: Primary beneficiary (institutional/arbitrage) —
 *     benefits from the status quo of managing an incurable disease with
 *     existing palliative treatments and long-term care services.
 *   - research_community: Analytical observer — sees the full biological
 *     structure and its devastating effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alzheimers_nlrp3_inflammasome, 0.85).
domain_priors:suppression_score(alzheimers_nlrp3_inflammasome, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(alzheimers_nlrp3_inflammasome, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alzheimers_nlrp3_inflammasome, extractiveness, 0.85).
narrative_ontology:constraint_metric(alzheimers_nlrp3_inflammasome, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(alzheimers_nlrp3_inflammasome, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A. While a "natural" biological process, its high extraction and
% suppression disqualify it from being a Mountain. A disease is not a
% fundamental, neutral law of physics; it is an extractive process.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(alzheimers_nlrp3_inflammasome, snare).
narrative_ontology:human_readable(alzheimers_nlrp3_inflammasome, "Alzheimer's Disease Pathogenesis via NLRP3 Inflammasome").
narrative_ontology:topic_domain(alzheimers_nlrp3_inflammasome, "technological").

% --- Binary flags ---
% N/A: No sunset clause, no active human enforcement needed.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(alzheimers_nlrp3_inflammasome, pharma_and_care_industry).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(alzheimers_nlrp3_inflammasome, alzheimers_patients).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (THE PATIENT)
% The patient is powerless and trapped within the biological progression of
% the disease. The high extraction and suppression make it a perfect Snare.
% Engine derives d from: victim + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))). % Disease is experienced locally by the individual.

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE INDUSTRY)
% The pharmaceutical and long-term care industries have business models built
% around managing the chronic, incurable nature of the disease. From this
% perspective, the disease's existence is a stable "coordination" point for
% a massive market.
% Engine derives d from: beneficiary + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (THE RESEARCH COMMUNITY)
% Researchers see the underlying biology, the immense extraction from patients,
% and the industrial incentives. They recognize the process is not a functional
% coordination but a pathological extraction of life and function.
% The classification aligns with the victim's experience: it's a Snare.
constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alzheimers_nlrp3_inflammasome_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap between patient and industry.
    constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_snare, [nondet]) :-
    % Verify the analytical view matches the constraint claim.
    constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, snare, context(agent_power(analytical), _, _, _)).

test(snare_threshold_validation) :-
    % Verify that the base metrics meet the criteria for a Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SupMetricName),
    narrative_ontology:constraint_metric(alzheimers_nlrp3_inflammasome, ExtMetricName, E),
    narrative_ontology:constraint_metric(alzheimers_nlrp3_inflammasome, SupMetricName, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(alzheimers_nlrp3_inflammasome_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.85): Extremely high, representing the near-total
 *     destruction of cognitive function, memory, and quality of life over the
 *     course of the disease.
 *   - Suppression Score (0.90): High, as the pathological inflammatory cascade
 *     actively suppresses normal neural function and is biologically
 *     inescapable without a targeted intervention.
 *   - Theater Ratio (0.05): Very low. The biological process itself has no
 *     performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the patient (powerless/trapped), the disease is a
 *   Snare that consumes their identity and future. For the incumbent industry
 *   (institutional/arbitrage), the disease's intractability creates a stable,
 *   predictable market for palliative drugs and long-term care services, making
 *   the status quo a Rope that coordinates their economic activity. The discovery
 *   of a potential cure threatens this Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Victim: 'alzheimers_patients' are the unambiguous victims. They bear 100% of
 *     the biological cost. The engine assigns them a high directionality (d),
 *     leading to a high effective extraction (χ) and a Snare classification.
 *   - Beneficiary: 'pharma_and_care_industry' benefits economically from the
 *     existence of a large patient population requiring management, not cure. The
 *     engine assigns them a low directionality (d), a negative χ, and a Rope
 *     classification. This correctly models their structural incentive to maintain
 *     the system.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This model correctly avoids mislabeling a devastating
 *   disease as a "natural" or "neutral" phenomenon (a Mountain). By focusing
 *   on the extractive and suppressive metrics from the victim's perspective,
 *   it classifies the disease's mechanism as a Snare. It simultaneously
 *   captures why an institutional actor would perceive this same reality as a
 *   Rope of pure coordination, revealing the structural incentives that can
 *   create inertia against disruptive cures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_alzheimers_nlrp3,
    'Is the NLRP3 inflammasome pathway a primary causal driver of Alzheimer`s, or a significant but secondary downstream effect of another process (like amyloid plaques)?',
    'Longitudinal human clinical trials of NLRP3 inhibitors, measuring both cognitive outcomes and other biomarkers like amyloid/tau levels.',
    'If causal, this constraint is the core Snare to be dismantled. If secondary, dismantling it may only provide partial relief, and the primary Snare remains elsewhere.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents disease progression.
narrative_ontology:interval(alzheimers_nlrp3_inflammasome, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required as base_extractiveness > 0.46.
% Models the progression of the disease within a patient's biographical timeline.
% T=0 is early stage, T=10 is late stage.

% Theater ratio over time (remains negligible):
narrative_ontology:measurement(az_tr_t0, alzheimers_nlrp3_inflammasome, theater_ratio, 0, 0.05).
narrative_ontology:measurement(az_tr_t5, alzheimers_nlrp3_inflammasome, theater_ratio, 5, 0.05).
narrative_ontology:measurement(az_tr_t10, alzheimers_nlrp3_inflammasome, theater_ratio, 10, 0.05).

% Extraction over time (increases as disease worsens):
narrative_ontology:measurement(az_ex_t0, alzheimers_nlrp3_inflammasome, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(az_ex_t5, alzheimers_nlrp3_inflammasome, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(az_ex_t10, alzheimers_nlrp3_inflammasome, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The pathological immune response can be seen as a misapplication of biological
% resources, hence 'resource_allocation'.
narrative_ontology:coordination_type(alzheimers_nlrp3_inflammasome, resource_allocation).

% --- Network Decomposition (Constraint Families) ---
% Alzheimer's is a complex disease. This constraint story focuses on one
% specific mechanism (NLRP3). It is part of a larger family of constraints
% that collectively constitute the disease.

% DUAL FORMULATION NOTE:
% This constraint is one of several stories decomposed from the colloquial label
% "Alzheimer's Disease". Decomposed because ε differs across observables
% (e.g., measuring cognitive decline vs. measuring plaque density).
%
% Related stories:
%   - alzheimers_amyloid_plaque (ε≈0.55, Tangled Rope) - The classic hypothesis.
%   - alzheimers_tau_tangles (ε≈0.65, Snare) - Another key pathological feature.

narrative_ontology:affects_constraint(alzheimers_amyloid_plaque, alzheimers_nlrp3_inflammasome).
narrative_ontology:affects_constraint(alzheimers_nlrp3_inflammasome, alzheimers_tau_tangles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The automatic derivation from beneficiary/victim groups
% and exit options accurately captures the structural relationships between
% patients and the healthcare/pharma industry in this context.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */