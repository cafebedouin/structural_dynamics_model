% ============================================================================
% CONSTRAINT STORY: complexity_debt
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_complexity_debt, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: complexity_debt
 * human_readable: The Cumulative Fragility Surcharge
 * domain: technological/organizational
 * * SUMMARY:
 * This constraint represents the accrued cost of "quick-fix" solutions and
 * layered abstractions within a system. Over time, the maintenance of this
 * historical complexity consumes a majority of the system's energy, siphoning
 * resources away from innovation and toward the preservation of an
 * increasingly brittle status quo. It functions as a Snare for the current
 * maintainer while having served as a Rope for the original builder.
 * * KEY AGENTS:
 * - Maintenance Engineer: Subject (Powerless)
 * - Legacy System Owner: Beneficiary (Institutional)
 * - Systems Archeologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the "interest" on complexity debt liquidates
% the system's adaptive capacity, forcing participants to pay a perpetual
% tax in labor and cognitive load.
domain_priors:base_extractiveness(complexity_debt, 0.84).
domain_priors:suppression_score(complexity_debt, 0.76).   % Alternatives are blocked by the prohibitive cost of re-architecture.
domain_priors:theater_ratio(complexity_debt, 0.45).       % Moderate theater; legacy stability is often used as a justification.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(complexity_debt, extractiveness, 0.84).
narrative_ontology:constraint_metric(complexity_debt, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(complexity_debt, theater_ratio, 0.45).

% Constraint self-claim: The system claims to be essential for coordination.
narrative_ontology:constraint_claim(complexity_debt, tangled_rope).
narrative_ontology:human_readable(complexity_debt, "The Cumulative Fragility Surcharge").
narrative_ontology:topic_domain(complexity_debt, "technological/organizational").

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(complexity_debt).
narrative_ontology:constraint_beneficiary(complexity_debt, legacy_system_owner).
narrative_ontology:constraint_victim(complexity_debt, maintenance_engineer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the maintainer, the debt is a snare: they cannot move without triggering
% a regression in a legacy component they don't fully understand.
constraint_indexing:constraint_classification(complexity_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the legacy stack as a Rope—it is the coordination
% substrate that currently generates all revenue, despite its fragility.
constraint_indexing:constraint_classification(complexity_debt, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of essential coordination (Rope)
% entangled with massive, deferred extraction (Snare).
constraint_indexing:constraint_classification(complexity_debt, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complexity_debt_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional owner.
    constraint_indexing:constraint_classification(complexity_debt, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complexity_debt, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(complexity_debt, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction (0.84) is in the high-extraction range.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(complexity_debt, ExtMetricName, E),
    E >= 0.46.

:- end_tests(complexity_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a state where the "interest" on technical debt siphons all productive agency from the subject. The suppression score (0.76) represents the prohibitive cost and risk of re-architecting the system, effectively trapping maintainers. The Tangled Rope classification is critical because the system is not purely extractive; it provides a genuine, albeit fragile, coordination function (keeping the business running).
 *
 * PERSPECTIVAL GAP:
 * The Maintenance Engineer (powerless, trapped) perceives a Snare. Their daily work involves navigating a brittle, undocumented system where any change can cause cascading failures, extracting their time and cognitive energy for zero innovative gain. The Legacy System Owner (institutional, mobile) perceives a Rope. For them, the system is a stable, revenue-generating asset. The maintenance costs are just operational expenses, and the risk of a rewrite is far greater than the risk of continued maintenance.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.84) risks a misclassification as a pure Snare. This is resolved by the Tangled Rope classification, which correctly identifies the dual nature of the constraint: it provides an essential coordination function (Rope) that is inextricably tangled with a severe extractive burden on a specific group (Snare). The system avoids mandatrophy by acknowledging the coordination value instead of dismissing the entire constraint as parasitic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_complexity_debt,
    'At what point does the cost of maintenance exceed the system utility (Mountain of Insolvency vs. Snare of Rent-Seeking)?',
    'Tracking the ratio of bug-fix hours to feature-development hours over multiple fiscal cycles.',
    'If utility < cost: Mountain of Ruin. If utility > cost: Snare of Rent-Seeking.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(complexity_debt, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the gradual accumulation of complexity debt.
% Initially, the system was a low-extraction scaffold, but over time,
% layered fixes increased both extraction and performative justification.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(complexity_debt_tr_t0, complexity_debt, theater_ratio, 0, 0.10).
narrative_ontology:measurement(complexity_debt_tr_t5, complexity_debt, theater_ratio, 5, 0.30).
narrative_ontology:measurement(complexity_debt_tr_t10, complexity_debt, theater_ratio, 10, 0.45).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(complexity_debt_ex_t0, complexity_debt, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(complexity_debt_ex_t5, complexity_debt, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(complexity_debt_ex_t10, complexity_debt, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The legacy system acts as the core infrastructure for the organization.
narrative_ontology:coordination_type(complexity_debt, global_infrastructure).

% Complexity debt in a core system structurally inhibits the ability to
% innovate, coupling it to another constraint.
narrative_ontology:affects_constraint(complexity_debt, innovation_pipeline_stagnation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */