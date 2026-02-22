% ============================================================================
% CONSTRAINT STORY: individual_vs_systemic_causation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_vs_systemic_causation, []).

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
 *   constraint_id: individual_vs_systemic_causation
 *   human_readable: Individual vs. Systemic Causation in Public Health
 *   domain: public_health/policy
 *
 * SUMMARY:
 *   This constraint describes the framing of public health risks as
 *   'lifestyle choices' versus industrially-driven, systemic, or
 *   environmental exposures. This narrative shifts the locus of
 *   responsibility, and therefore cost, from producers of risk (e.g., fossil
 *   fuel, ultra-processed food, tobacco industries) to the individuals
 *   exposed to those risks. The primary observable is the allocation of
 *   public health funding and messaging, which overwhelmingly favors
 *   individual education campaigns over systemic regulation.
 *
 * KEY AGENTS:
 *   - Industries Promoting Risk Factors: Primary beneficiary (institutional/arbitrage) - Avoids regulation, liability, and costs of externalities.
 *   - Individuals Blamed for Systemic Exposures: Primary victim (powerless/trapped) - Bears direct health costs, lost productivity, and social stigma for conditions largely outside their control.
 *   - Public Health Agencies: Enforcer/Victim (institutional/constrained) - Tasked with improving health outcomes but politically and financially constrained from addressing powerful systemic drivers.
 *   - Epidemiologists and Researchers: Analytical Observer (analytical/analytical) - Documents the statistical evidence for systemic causation, often in conflict with the dominant public narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_vs_systemic_causation, 0.68).
domain_priors:suppression_score(individual_vs_systemic_causation, 0.75).
domain_priors:theater_ratio(individual_vs_systemic_causation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_vs_systemic_causation, extractiveness, 0.68).
narrative_ontology:constraint_metric(individual_vs_systemic_causation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(individual_vs_systemic_causation, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_vs_systemic_causation, tangled_rope).
narrative_ontology:human_readable(individual_vs_systemic_causation, "Individual vs. Systemic Causation in Public Health").
narrative_ontology:topic_domain(individual_vs_systemic_causation, "public_health/policy").

domain_priors:requires_active_enforcement(individual_vs_systemic_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_vs_systemic_causation, industries_promoting_risk_factors).
narrative_ontology:constraint_victim(individual_vs_systemic_causation, individuals_blamed_for_systemic_exposures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an individual with limited resources and exposed to systemic risks, the narrative of 'personal responsibility' is a trap that assigns blame while ignoring the constraints on their choices.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% For industries whose products create health externalities, framing risk as individual choice is a highly effective coordination strategy to maintain market access and prevent costly regulation.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Public health institutions are caught between the genuine goal of improving health (coordination) and the political/economic reality of industry influence that shifts costs onto the public (extraction).
constraint_indexing:constraint_classification(individual_vs_systemic_causation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes a dual function: a minor coordination role in promoting individual health, and a major extractive role in deflecting regulatory responsibility from systemic sources of harm.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_vs_systemic_causation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_vs_systemic_causation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_vs_systemic_causation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_vs_systemic_causation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(individual_vs_systemic_causation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.68) represents the immense health and financial costs externalized onto individuals and public healthcare systems, which would otherwise be borne by industries as a cost of doing business. The high suppression (0.75) reflects the effectiveness of corporate lobbying, public relations campaigns, and funding of research to maintain the 'individual responsibility' narrative and actively suppress or discredit evidence supporting regulatory alternatives.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists. For the risk-producing industry, the narrative is a pure Rope, a coordination mechanism to manage public perception and regulatory threats. For the individual living in a food desert or polluted area, it is a Snare, trapping them in a cycle of exposure and blame. For the constrained public health agency and the analytical observer, it is a Tangled Rope, acknowledging the (minor) legitimate coordination function of health education while recognizing the overwhelmingly extractive effect of deflecting corporate responsibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the clear structural relationship. 'Industries_promoting_risk_factors' are declared beneficiaries; their business models profit directly from the externalization of costs this constraint enables. 'Individuals_blamed_for_systemic_exposures' are the victims who bear these costs. This maps to a low directionality (d) for beneficiaries, yielding a low or negative effective extraction (χ), and a high d for victims, yielding a high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical example of mandatrophy. A legitimate, socially-desirable coordination goal ('encouraging healthy habits') serves as a theatrical pretext for a large-scale extractive process ('protecting industrial polluters and food manufacturers from regulation'). The Tangled Rope classification is crucial for resolving this, as it correctly identifies the dual nature of the constraint, preventing a misclassification as a pure Snare (which would ignore the coordination pretext) or a Rope (which would ignore the vast, asymmetric extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_ambiguity,
    'To what extent are chronic disease outcomes attributable to individual agency versus unavoidable systemic exposures?',
    'Large-scale longitudinal studies that control for environmental/economic factors and quantify the causal impact of industrial products (e.g., ultra-processed foods, pollutants).',
    'If individual agency is dominant (<20% systemic), the constraint is closer to a Rope. If systemic factors are dominant (>80%), it's a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_ambiguity, empirical, 'The irreducible ambiguity in attributing chronic disease causation between individual choice and systemic factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_vs_systemic_causation, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ivsc_tr_t0, individual_vs_systemic_causation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ivsc_tr_t37, individual_vs_systemic_causation, theater_ratio, 37, 0.3).
narrative_ontology:measurement(ivsc_tr_t74, individual_vs_systemic_causation, theater_ratio, 74, 0.4).

% Extraction over time
narrative_ontology:measurement(ivsc_be_t0, individual_vs_systemic_causation, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ivsc_be_t37, individual_vs_systemic_causation, base_extractiveness, 37, 0.55).
narrative_ontology:measurement(ivsc_be_t74, individual_vs_systemic_causation, base_extractiveness, 74, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_vs_systemic_causation, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a downstream consequence of the 'uneven_risk_distribution' mountain, which establishes that risks are not borne equally. This constraint describes the specific social mechanism used to justify and maintain that unequal distribution in the domain of public health.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
