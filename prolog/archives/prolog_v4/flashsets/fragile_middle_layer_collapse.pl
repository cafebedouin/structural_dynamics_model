% ============================================================================
% CONSTRAINT STORY: fragile_middle_layer_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fragile_middle_layer_collapse, []).

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
 *   constraint_id: fragile_middle_layer_collapse
 *   human_readable: The Intermediary Decay
 *   domain: economic/technological/logistical
 *
 * SUMMARY:
 *   The intermediary decay describes a scenario where the middle layer of a
 *   system is hollowed out by automation and direct-to-end-user scaling. This
 *   can lead to increased efficiency and lower prices for end-users, but also
 *   results in job losses for intermediaries and a potential degradation of
 *   service quality.
 *
 * KEY AGENTS:
 *   - Platform Owners: Primary beneficiary (institutional/arbitrage) - benefits from increased efficiency and scalability.
 *   - Middle Layer Intermediaries: Primary victim (powerless/trapped) - suffers job losses and declining demand for services.
 *   - End Users: Secondary actor (moderate/mobile) - benefits from lower prices but may experience reduced service quality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fragile_middle_layer_collapse, 0.65).
domain_priors:suppression_score(fragile_middle_layer_collapse, 0.7).
domain_priors:theater_ratio(fragile_middle_layer_collapse, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, extractiveness, 0.65).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fragile_middle_layer_collapse, tangled_rope).
narrative_ontology:human_readable(fragile_middle_layer_collapse, "The Intermediary Decay").
narrative_ontology:topic_domain(fragile_middle_layer_collapse, "economic/technological/logistical").

domain_priors:requires_active_enforcement(fragile_middle_layer_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fragile_middle_layer_collapse, platform_owners).
narrative_ontology:constraint_beneficiary(fragile_middle_layer_collapse, end_users).
narrative_ontology:constraint_victim(fragile_middle_layer_collapse, middle_layer_intermediaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the displaced intermediary (e.g., regional distributor). Limited exit options due to specialized skills and local market knowledge becoming obsolete. Trapped by lack of alternatives and declining demand for their services.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Perspective of the platform owner. Benefits from increased efficiency, reduced costs, and greater scalability. Sees the process as pure coordination, connecting producers directly with consumers.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of the end-user. Benefits from lower prices and greater access, but also experiences reduced service quality and loss of personalized support. Mobile: end users can choose between platform and traditional alternatives, even if that access is limited.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Analytical observer perspective. Recognizes both the coordination benefits (increased efficiency, lower prices) and the extraction costs (loss of jobs, reduced service quality, concentration of power). Sees the overall process as a tangled rope.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fragile_middle_layer_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fragile_middle_layer_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fragile_middle_layer_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The platform extracts significant value from the displaced intermediaries. Suppression (0.70): High. The automation and direct-to-end-user model significantly reduces the alternatives available to the intermediaries. Theater ratio (0.30): Low. There is relatively little performative activity as the shift is driven by genuine technological and economic forces.
 *
 * PERSPECTIVAL GAP:
 *   The platform owner sees the process as pure coordination, connecting producers directly with consumers. The displaced intermediary sees it as a snare, with limited exit options and declining demand for their services. The end-user experiences a mixed outcome, with lower prices but potentially reduced service quality.
 *
 * DIRECTIONALITY LOGIC:
 *   The platform owners are the primary beneficiaries and experience low extraction, the end-users have increased optionality and have a mobile exit making them a moderate agent. The intermediaries bear the brunt of the negative impact and are classified as victims with trapped exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    re_skilling_effectiveness,
    'How effective are re-skilling programs for displaced intermediaries in transitioning to new roles within the evolving economy?',
    'Longitudinal tracking of employment outcomes for re-skilling program participants compared to non-participants.',
    'High effectiveness: mitigates the negative impacts of intermediary decay. Low effectiveness: exacerbates inequality and social unrest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(re_skilling_effectiveness, empirical, 'Effectiveness of re-skilling programs.').

omega_variable(
    service_quality_degradation,
    'To what extent does the elimination of intermediaries lead to a degradation of service quality and personalized support for end-users?',
    'Surveys and user feedback analysis comparing service experiences with and without intermediaries.',
    'Significant degradation: undermines user satisfaction and adoption. Minimal degradation: strengthens the case for direct-to-end-user models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_quality_degradation, empirical, 'Impact of disintermediation on service quality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fragile_middle_layer_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frag_tr_t0, fragile_middle_layer_collapse, theater_ratio, 0, 0.1).
narrative_ontology:measurement(frag_tr_t5, fragile_middle_layer_collapse, theater_ratio, 5, 0.2).
narrative_ontology:measurement(frag_tr_t10, fragile_middle_layer_collapse, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(frag_be_t0, fragile_middle_layer_collapse, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(frag_be_t5, fragile_middle_layer_collapse, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(frag_be_t10, fragile_middle_layer_collapse, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fragile_middle_layer_collapse, resource_allocation).
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, job_polarization).
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, platform_monopoly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
