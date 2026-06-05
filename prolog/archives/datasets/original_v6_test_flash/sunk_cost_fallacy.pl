% ============================================================================
% CONSTRAINT STORY: sunk_cost_fallacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sunk_cost_fallacy, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sunk_cost_fallacy
 *   human_readable: The Sunk Cost Fallacy
 *   domain: economic/social/cognitive
 *
 * SUMMARY:
 *   The sunk cost fallacy describes the tendency to continue investing in a
 *   losing endeavor due to past investments. This cognitive bias affects
 *   decisions across various domains, from personal relationships to
 *   large-scale projects. It is a complex constraint exhibiting
 *   characteristics of both coordination and extraction. While the initial
 *   investment may have been rational, the continuation often becomes
 *   irrational due to the fallacy.
 *
 * KEY AGENTS:
 *   - Rational Decision Makers: Primary victim (powerless/trapped) - Affected by suboptimal decisions resulting from sunk cost considerations.
 *   - Project Stakeholders: Secondary victim (moderate/constrained) - Impacted by the project's poor performance and resource allocation.
 *   - Vendors of Escalating Projects: Primary beneficiary (institutional/arbitrage) - Benefit from continued funding and involvement.
 *   - Status Quo Maintainers: Secondary beneficiary (powerful/constrained) - benefit from maintaining familiar systems even if inefficient.
 *   - The Analytical Observer: Observers of the phenomenon (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sunk_cost_fallacy, 0.55).
domain_priors:suppression_score(sunk_cost_fallacy, 0.65).
domain_priors:theater_ratio(sunk_cost_fallacy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sunk_cost_fallacy, extractiveness, 0.55).
narrative_ontology:constraint_metric(sunk_cost_fallacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sunk_cost_fallacy, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sunk_cost_fallacy, tangled_rope).
narrative_ontology:human_readable(sunk_cost_fallacy, "The Sunk Cost Fallacy").
narrative_ontology:topic_domain(sunk_cost_fallacy, "economic/social/cognitive").

domain_priors:requires_active_enforcement(sunk_cost_fallacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sunk_cost_fallacy, vendors_of_escalating_projects).
narrative_ontology:constraint_beneficiary(sunk_cost_fallacy, status_quo_maintainers).
narrative_ontology:constraint_victim(sunk_cost_fallacy, rational_decision_makers).
narrative_ontology:constraint_victim(sunk_cost_fallacy, project_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Individual (powerless/trapped). An individual who has heavily invested time, effort, or money into a project may feel trapped, unable to abandon it even when it's clearly failing. The sunk cost fallacy acts as a snare, preventing rational decision-making.
constraint_indexing:constraint_classification(sunk_cost_fallacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Organization (moderate/constrained). An organization that has invested heavily in a particular process or technology may find it difficult to switch to a more efficient alternative, even when the benefits of doing so are clear. They are constrained by the prior investment but also derive some (diminishing) benefit from continuing the familiar process. They experience the sunk cost fallacy as a tangled rope.
constraint_indexing:constraint_classification(sunk_cost_fallacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 3: Vendors of escalating projects (institutional/arbitrage). These vendors benefit from the sunk cost fallacy. As projects escalate, they continue to receive funding and contracts, regardless of the project's success. They see the continued investment as a rope, ensuring their continued involvement and profit.
constraint_indexing:constraint_classification(sunk_cost_fallacy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: Status quo maintainers (powerful/constrained). Institutions and individuals who benefit from the existing system may resist change, even when it's clear that the system is not working effectively. They are constrained by the existing system, but are powerful enough to resist change due to the inertia created by prior investments. This manifests as a Piton, where the original function is degraded, but the structure remains.
constraint_indexing:constraint_classification(sunk_cost_fallacy, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: Analytical observer (analytical/analytical). The sunk cost fallacy is a cognitive bias that affects decision-making across various domains. From an analytical perspective, it represents a tangled rope because it involves both a psychological commitment (an investment) and a detrimental effect (suboptimal decisions).
constraint_indexing:constraint_classification(sunk_cost_fallacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sunk_cost_fallacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sunk_cost_fallacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sunk_cost_fallacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sunk_cost_fallacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sunk_cost_fallacy, TR),
    TR >= 0.70.

:- end_tests(sunk_cost_fallacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The sunk cost fallacy leads to the inefficient allocation of resources. It prevents rational decision-making by making it difficult to cut losses. The extraction is experienced as wasted resources, effort, and opportunity costs. Suppression (0.65): Moderate-High. The suppression arises from the psychological difficulty of admitting a past mistake and the social pressure to continue endeavors already begun. Theater Ratio (0.30): Low. While performative aspects exist (e.g., saving face), the primary driver is the cognitive bias rather than pure theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives highlight the different experiences of the sunk cost fallacy. Individuals trapped in failing projects experience it as a snare, while vendors benefit from continued investment as a rope. Organizations are constrained by previous investments and experience it as a tangled rope. The analytical observer identifies it as a broad cognitive bias. The status quo maintainers see a piton, a degraded system that they continue to support.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vendors and status quo maintainers) have low 'd' value due to deriving benefit, while victims (rational decision makers and project stakeholders) have high 'd' value, reflecting the costs they bear. The specific d values and subsequent classifications are calculated by the engine based on the declared power levels and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The sunk cost fallacy can be mislabeled as efficient long-term investment if the cognitive bias is not recognized. The framework clarifies that this bias is present if decisions are made based on past costs rather than future benefits. The perspectives highlight the differential experiences of those affected, resolving the mandatrophy by providing a multi-faceted view of the phenomenon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_detection_effectiveness,
    'How effective are interventions designed to mitigate the sunk cost fallacy?',
    'Empirical studies measuring the impact of debiasing techniques on decision-making.',
    'Determines whether the ''snare'' effect can be significantly reduced, potentially shifting classifications toward ''tangled_rope'' or even ''rope'' if effective interventions are readily available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_detection_effectiveness, empirical, 'Effectiveness of interventions to mitigate the sunk cost fallacy.').

omega_variable(
    irreversible_investment_threshold,
    'What level of investment makes abandoning a project psychologically prohibitive?',
    'Behavioral experiments varying the scale and type of prior investment.',
    'Defines the boundary between rational persistence and irrational escalation, influencing the perceived extractiveness and the resulting classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversible_investment_threshold, empirical, 'Level of investment making abandoning a project psychologically prohibitive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sunk_cost_fallacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sunk_tr_t0, sunk_cost_fallacy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sunk_tr_t5, sunk_cost_fallacy, theater_ratio, 5, 0.2).
narrative_ontology:measurement(sunk_tr_t10, sunk_cost_fallacy, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(sunk_be_t0, sunk_cost_fallacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sunk_be_t5, sunk_cost_fallacy, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sunk_be_t10, sunk_cost_fallacy, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sunk_cost_fallacy, resource_allocation).
narrative_ontology:affects_constraint(sunk_cost_fallacy, cognitive_biases).
narrative_ontology:affects_constraint(sunk_cost_fallacy, escalation_of_commitment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
