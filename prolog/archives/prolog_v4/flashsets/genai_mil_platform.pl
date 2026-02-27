% ============================================================================
% CONSTRAINT STORY: genai_mil_platform
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genai_mil_platform, []).

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
 *   constraint_id: genai_mil_platform
 *   human_readable: Mandatory Use of the GenAI.mil Platform for Defense Intelligence Analysis
 *   domain: technological
 *
 * SUMMARY:
 *   The Department of Defense (DoD) mandate for intelligence analysts to use
 *   the GenAI.mil platform presents a complex constraint with both potential
 *   benefits and significant risks. The centralized platform aims to
 *   standardize intelligence analysis, enhance efficiency, and provide a
 *   single source of truth. However, it also raises concerns about limiting
 *   analytical diversity, suppressing alternative methods, and potentially
 *   introducing biases into intelligence assessments.
 *
 * KEY AGENTS:
 *   - Intelligence Analysts: Primary target (powerless/trapped) – forced to use the platform and constrained in their analytical methods.
 *   - GenAI.mil Platform Contractor: Primary beneficiary (institutional/arbitrage) – benefits from a guaranteed revenue stream.
 *   - DoD Leadership: Beneficiary (institutional/arbitrage) – benefits from centralized access and control over intelligence products.
 *   - Independent Verification: Victims (moderate/constrained) – hindered by the limited access to diverse viewpoints.
 *   - Alternative Analysis Methods: Victims (moderate/constrained) – are undermined and potentially phased out
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genai_mil_platform, 0.6).
domain_priors:suppression_score(genai_mil_platform, 0.7).
domain_priors:theater_ratio(genai_mil_platform, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genai_mil_platform, extractiveness, 0.6).
narrative_ontology:constraint_metric(genai_mil_platform, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(genai_mil_platform, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genai_mil_platform, tangled_rope).
narrative_ontology:human_readable(genai_mil_platform, "Mandatory Use of the GenAI.mil Platform for Defense Intelligence Analysis").
narrative_ontology:topic_domain(genai_mil_platform, "technological").

domain_priors:requires_active_enforcement(genai_mil_platform).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genai_mil_platform, genai_mil_platform_contractor).
narrative_ontology:constraint_beneficiary(genai_mil_platform, dod_leadership).
narrative_ontology:constraint_victim(genai_mil_platform, intelligence_analysts).
narrative_ontology:constraint_victim(genai_mil_platform, independent_verification).
narrative_ontology:constraint_victim(genai_mil_platform, alternative_analysis_methods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Intelligence analysts are forced to use the GenAI.mil platform, limiting their ability to leverage alternative tools and methods. They are trapped within the system due to the mandate.
constraint_indexing:constraint_classification(genai_mil_platform, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Verification teams are constrained by the mandated use of GenAI.mil, which limits their access to diverse analytical viewpoints and potentially hinders their ability to conduct truly independent assessments. They benefit from some automation features but are ultimately dependent on a single platform.
constraint_indexing:constraint_classification(genai_mil_platform, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The contractor benefits from a guaranteed revenue stream due to the mandatory use of their platform. This is seen as a coordination mechanism from their perspective, providing a stable environment for platform development and maintenance.
constraint_indexing:constraint_classification(genai_mil_platform, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% DoD leadership benefits from the centralized platform, as it provides a single point of access for intelligence reports and analysis, increasing visibility and control. This serves as a coordination mechanism to standardize intelligence products.
constraint_indexing:constraint_classification(genai_mil_platform, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer recognizes the dual nature of the mandate: it offers coordination benefits through standardization and centralized access, but it also introduces extractive risks by limiting analytical diversity and potentially biasing intelligence assessments.
constraint_indexing:constraint_classification(genai_mil_platform, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genai_mil_platform_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genai_mil_platform, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genai_mil_platform, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genai_mil_platform, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genai_mil_platform_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) reflects the degree to which the mandate restricts the analytical freedom of intelligence analysts. The suppression (0.70) indicates the extent to which alternative methods are suppressed or discouraged. The theater ratio (0.40) is relatively low, suggesting that the platform's functionality is currently prioritized over performative aspects, though this may change over time.
 *
 * PERSPECTIVAL GAP:
 *   Intelligence analysts see the mandate as a snare, limiting their ability to leverage diverse tools and methods. The platform contractor and DoD leadership view it as a rope, facilitating coordination and standardization. Independent verification teams experience it as a tangled rope, providing some benefits while also imposing constraints. The analytical observer recognizes the mixed nature of the mandate, seeing both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are based on the structural relationship of each agent to the constraint. Intelligence analysts, as the primary targets, have a high directionality value due to their limited exit options. The platform contractor and DoD leadership, as beneficiaries, have a low directionality value due to their ability to arbitrage and benefit from the mandate. Independent verification is given a directionality between to the above, as they are constrained by the mandate, but not nearly to the extent the intelligence analysts are.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_bias_source,
    'What sources of data bias are present in the training datasets and algorithms used by the GenAI.mil platform?',
    'Regular audits of the training datasets and model outputs to identify and mitigate biases.',
    'Reduced analyst trust, skewed risk assessment, and increased probability of erroneous decisions',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_bias_source, empirical, 'Potential sources of data bias are present.').

omega_variable(
    cognitive_tunneling_effect,
    'Does the mandated use of GenAI.mil lead to cognitive tunneling or confirmation bias among intelligence analysts?',
    'Experimental studies measuring analysts'' openness to alternative hypotheses after using GenAI.mil-generated reports.',
    'Reduces analytical diversity and lowers trust in the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_tunneling_effect, empirical, 'Evaluate whether it leads to tunnel vision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genai_mil_platform, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gena_tr_t0, genai_mil_platform, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gena_tr_t5, genai_mil_platform, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gena_tr_t10, genai_mil_platform, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(gena_be_t0, genai_mil_platform, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gena_be_t5, genai_mil_platform, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(gena_be_t10, genai_mil_platform, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genai_mil_platform, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
