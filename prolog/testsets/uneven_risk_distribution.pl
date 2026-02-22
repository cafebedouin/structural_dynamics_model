% ============================================================================
% CONSTRAINT STORY: uneven_risk_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uneven_risk_distribution, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uneven_risk_distribution
 *   human_readable: Uneven Distribution of Modifiable Health Risk Factors
 *   domain: public_health/epidemiology
 *
 * SUMMARY:
 *   This constraint represents the observed, persistent statistical fact that
 *   primary modifiable health risk factors (e.g., smoking, HPV, H. pylori,
 *   alcohol consumption) are not uniformly distributed across human
 *   populations. They cluster predictably by geography, gender, and
 *   socioeconomic status. This is a foundational, non-extractive reality that
 *   public health policy must contend with, analogous to a feature of the
 *   physical landscape.
 *
 * KEY AGENTS:
 *   - Epidemiologists/Researchers (analytical): Observers who measure and model this distribution.
 *   - Public Health Institutions (institutional): Actors who must design policies around this unchangeable statistical landscape.
 *   - Individuals in high-risk groups (powerless): Subjects whose life outcomes are shaped by their position within this statistical reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uneven_risk_distribution, 0.05).
domain_priors:suppression_score(uneven_risk_distribution, 0.02).
domain_priors:theater_ratio(uneven_risk_distribution, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uneven_risk_distribution, extractiveness, 0.05).
narrative_ontology:constraint_metric(uneven_risk_distribution, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(uneven_risk_distribution, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(uneven_risk_distribution, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(uneven_risk_distribution, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uneven_risk_distribution, mountain).
narrative_ontology:human_readable(uneven_risk_distribution, "Uneven Distribution of Modifiable Health Risk Factors").
narrative_ontology:topic_domain(uneven_risk_distribution, "public_health/epidemiology").

domain_priors:emerges_naturally(uneven_risk_distribution).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% This perspective sees the statistical reality as a fundamental feature of population health dynamics, observable across cultures and time.
constraint_indexing:constraint_classification(uneven_risk_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% For policymakers, this is a baseline reality that all interventions must account for. It is a fixed feature of the landscape they operate in.
constraint_indexing:constraint_classification(uneven_risk_distribution, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an individual's perspective, their baseline risk profile based on geography and demography is an unchangeable fact of their birth and environment.
constraint_indexing:constraint_classification(uneven_risk_distribution, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uneven_risk_distribution_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(uneven_risk_distribution, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uneven_risk_distribution, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(uneven_risk_distribution, ExtMetricName, E),
    domain_priors:suppression_score(uneven_risk_distribution, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(uneven_risk_distribution),
    narrative_ontology:constraint_metric(uneven_risk_distribution, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(uneven_risk_distribution, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(uneven_risk_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it is an emergent, observable pattern of human populations, not a designed system. Base extractiveness (ε=0.05) and suppression (0.02) are near zero, as the pattern itself does not extract value or coerce behavior. The high accessibility_collapse (0.95) reflects that diverse, independent studies consistently converge on the same conclusion about risk clustering. Resistance (0.05) is low because the epidemiological data is overwhelming and not seriously contested.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap. All rational actors, regardless of their power or position, must acknowledge this statistical reality. It is a Mountain for the analyst who studies it, the policymaker who plans around it, and the individual who lives within it. The *implications* of the constraint differ dramatically for each agent, but the classification of the constraint itself is invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint has no direct beneficiaries or victims. The constraint is the map of risk, not the risk itself. The 'victims' are those who suffer from the risk factors (e.g., lung cancer from smoking), but the constraint is merely the observation that these factors are unevenly distributed. No agent directly benefits from the *pattern* of distribution in a way that would imply a coordination or extraction function.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Mountain is crucial to prevent misattributing a baseline statistical reality to a specific, malicious policy (which would be a Snare). It forces analysts to distinguish between the landscape of risk (Mountain) and the specific policies or economic systems that may exacerbate or mitigate the harms within that landscape. This prevents the category error of blaming the map for the territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uneven_risk_distribution, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(uneven_risk_distribution, healthcare_resource_allocation).
narrative_ontology:affects_constraint(uneven_risk_distribution, public_health_messaging_effectiveness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
