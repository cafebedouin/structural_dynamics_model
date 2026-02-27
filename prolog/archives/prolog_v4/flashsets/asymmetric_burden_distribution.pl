% ============================================================================
% CONSTRAINT STORY: asymmetric_burden_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_burden_distribution, []).

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
 *   constraint_id: asymmetric_burden_distribution
 *   human_readable: The Externalization Lever
 *   domain: economic/social/logistical
 *
 * SUMMARY:
 *   This scenario describes the Externalization Lever, where systemic
 *   benefits concentrate for some, while operational burdens shift onto
 *   others. Examples include global shipping benefiting corporations, while
 *   coastal communities suffer pollution; digital convenience for urban
 *   residents, burdening logistics workers and e-waste disposal sites.
 *
 * KEY AGENTS:
 *   - Central Beneficiaries: Institutional actors who gain disproportionately from the system, enjoying the benefits while externalizing costs.
 *   - Peripheral Population: Powerless group that bears the brunt of the operational burdens, lacking adequate access to the benefits.
 *   - Analytical Observer: Assesses the overall system-wide extraction and coordination.
 *   - Regulatory Bodies: (moderate/constrained) Attempt to modulate the extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_burden_distribution, 0.6).
domain_priors:suppression_score(asymmetric_burden_distribution, 0.4).
domain_priors:theater_ratio(asymmetric_burden_distribution, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_burden_distribution, extractiveness, 0.6).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_burden_distribution, tangled_rope).
narrative_ontology:human_readable(asymmetric_burden_distribution, "The Externalization Lever").
narrative_ontology:topic_domain(asymmetric_burden_distribution, "economic/social/logistical").

domain_priors:requires_active_enforcement(asymmetric_burden_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_burden_distribution, central_beneficiaries).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, peripheral_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Peripheral populations often bear the brunt of the operational burdens without enjoying proportional benefits, and often lack the power or resources to relocate or influence policy.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Central beneficiaries, such as corporations or affluent urban residents, gain disproportionately from the system while externalizing costs onto others.  They can easily 'arbitrage' their exposure to negative effects.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees the intertwined coordination and extraction: the system provides real benefits, but at an unevenly distributed cost. The long-term consequences of this distribution create systemic risk.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_burden_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_burden_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asymmetric_burden_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Reflects the significant transfer of negative externalities onto peripheral populations. Suppression (0.4): Represents the barriers that prevent the peripheral population from effectively challenging or escaping the burden distribution. Theater Ratio (0.3): The performative aspect of addressing the externalities is moderate: there may be some attempts to mitigate, but rarely address the core asymmetric.
 *
 * PERSPECTIVAL GAP:
 *   The central beneficiaries see the system as a coordination mechanism (Rope), while the peripheral population experiences pure extraction (Snare). The analytical perspective reveals the hybrid nature of the system (Tangled Rope) where coordination exists, but is coupled to asymmetric burden distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position: central beneficiaries gain from the arrangement (low d) and peripheral populations bear the costs (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The key to resolving the mandatrophy here is to recognize that both coordination and extraction are simultaneously occurring. It prevents misclassifying the coordination as pure extraction, as the benefits ARE real for central agents. Conversely, classifying as pure coordination misses the extraction being imposed. The Tangled Rope classification captures this hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    burden_measurement_accuracy,
    'How accurately can the true burdens (health impacts, environmental degradation, social costs) be measured and attributed to the source?',
    'Improved scientific monitoring, comprehensive cost-benefit analysis including externalities, longitudinal studies tracking health and social outcomes.',
    'If burdens are underestimated: The system appears more beneficial than it is, perpetuating the asymmetric distribution. If burdens are overestimated: Legitimate benefits may be curtailed due to perceived high cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_measurement_accuracy, empirical, 'Accuracy of burden measurement and attribution.').

omega_variable(
    compensatory_mechanism_effectiveness,
    'How effectively do compensatory mechanisms (taxes, regulations, infrastructure improvements) mitigate the externalized burdens?',
    'Comparative analysis of policies in different regions, econometric studies assessing the impact of regulations on pollution and health outcomes, surveys assessing public perception of fairness.',
    'If compensation is ineffective: The asymmetric distribution persists, potentially leading to social unrest and system failure. If compensation is effective: The system becomes more sustainable and equitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensatory_mechanism_effectiveness, empirical, 'Effectiveness of compensatory mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_burden_distribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asym_tr_t0, asymmetric_burden_distribution, theater_ratio, 0, 0.1).
narrative_ontology:measurement(asym_tr_t5, asymmetric_burden_distribution, theater_ratio, 5, 0.2).
narrative_ontology:measurement(asym_tr_t10, asymmetric_burden_distribution, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(asym_be_t0, asymmetric_burden_distribution, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(asym_be_t5, asymmetric_burden_distribution, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(asym_be_t10, asymmetric_burden_distribution, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_burden_distribution, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
