% ============================================================================
% CONSTRAINT STORY: viral_emergence_covid19_exemplar
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_viral_emergence_covid19_exemplar, []).

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
 *   constraint_id: viral_emergence_covid19_exemplar
 *   human_readable: Societal Response to SARS-CoV-2 Emergence
 *   domain: social/political/health
 *
 * SUMMARY:
 *   The COVID-19 pandemic and the societal response to it represent a
 *   complex, evolving constraint with significant implications for public
 *   health, civil liberties, and economic stability. The response involved a
 *   mixture of coordination (e.g., vaccine development, public health
 *   messaging) and extraction (e.g., lockdowns, travel restrictions,
 *   mandates). Different actors experienced the constraint in vastly
 *   different ways, leading to polarized opinions and debates about the
 *   appropriateness and effectiveness of various measures.
 *
 * KEY AGENTS:
 *   - The Isolated Individual: Primary victim (powerless/trapped) - bore the brunt of lockdowns, mandates, and economic disruption.
 *   - Small Business Owner: Secondary victim (moderate/constrained) - faced economic hardship and uncertainty due to restrictions.
 *   - Incumbent Political Regimes: Primary beneficiary (institutional/arbitrage) - consolidated power through emergency measures.
 *   - Pharmaceutical Companies: Secondary beneficiary (powerful/mobile) - profited from vaccine development and distribution.
 *   - The WHO: Constrained Institution (institutional/constrained)
 *   - The Analytical Observer: Global perspective (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viral_emergence_covid19_exemplar, 0.65).
domain_priors:suppression_score(viral_emergence_covid19_exemplar, 0.75).
domain_priors:theater_ratio(viral_emergence_covid19_exemplar, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, extractiveness, 0.65).
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viral_emergence_covid19_exemplar, tangled_rope).
narrative_ontology:human_readable(viral_emergence_covid19_exemplar, "Societal Response to SARS-CoV-2 Emergence").
narrative_ontology:topic_domain(viral_emergence_covid19_exemplar, "social/political/health").

domain_priors:requires_active_enforcement(viral_emergence_covid19_exemplar).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, incumbent_political_regimes).
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, pharmaceutical_companies).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, civil_liberties).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, small_businesses).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, global_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual trapped by lockdowns, mandates, and travel restrictions; bears the brunt of economic and social costs with limited exit options.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Subject to restrictions and economic uncertainty, but with some capacity to adapt or seek government support.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Benefits from increased executive power and popular support in the initial phase, using the crisis to consolidate control.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Benefits from accelerated regulatory approvals, government contracts, and increased demand for vaccines and therapeutics, with some public scrutiny and reputational risk.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Initially seen as a coordinator, but perceived as degraded as the pandemic progressed. Lacks enforcement and is constrained by member states. Retains performative function (issuing guidance), but effectiveness is diminished.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Sees the complex interplay of coordination and extraction, observing the long-term societal and political consequences of the pandemic response.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viral_emergence_covid19_exemplar_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(viral_emergence_covid19_exemplar, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(viral_emergence_covid19_exemplar, TR),
    TR >= 0.70.

:- end_tests(viral_emergence_covid19_exemplar_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Significant economic and social costs imposed on individuals and businesses. Suppression (0.75): High. Government mandates, travel restrictions, and censorship suppressed alternative viewpoints. Theater ratio (0.75): High. While some measures were scientifically justified, others were performative and lacked clear evidence of effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   The isolated individual experiences the response as a snare. Small businesses experience it as a tangled rope. Incumbent regimes see it as a rope. Pharmaceutical companies benefit through access to arbitrage. The analytical observer sees the complex dynamic as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's power, exit options, and relationship to extraction. Trapped individuals experience high extraction; powerful institutions benefit; analytical observers see the full complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing the legitimate yet conflicting perspectives of different actors. The social response was not simply 'good' or 'bad,' but a complex interplay of coordination and extraction with varying impacts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_assessment_accuracy,
    'How accurately was the initial severity of SARS-CoV-2 assessed, and what impact did this have on subsequent policy decisions?',
    'Retrospective analysis of epidemiological data, seroprevalence studies, and modeling scenarios.',
    'Overestimation led to excessive restrictions and economic damage; underestimation led to delayed action and higher mortality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_assessment_accuracy, empirical, 'Accuracy of initial severity assessment of SARS-CoV-2.').

omega_variable(
    counterfactual_intervention_impact,
    'What would have been the impact of alternative policy interventions, such as focused protection strategies or less stringent lockdowns?',
    'Comparative modeling studies, historical analysis of different regional responses, and randomized controlled trials (where feasible).',
    'Determines the efficiency and effectiveness of implemented policies relative to potential alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_intervention_impact, empirical, 'Counterfactual impact of alternative policy interventions.').

omega_variable(
    long_term_social_consequences,
    'What are the long-term social and psychological consequences of the pandemic and related policies, including mental health impacts, social polarization, and erosion of trust in institutions?',
    'Longitudinal cohort studies, surveys, and qualitative research on social attitudes and behaviors.',
    'Reveals the hidden costs of the pandemic response and informs strategies for mitigating negative social impacts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_social_consequences, empirical, 'Long-term social and psychological consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viral_emergence_covid19_exemplar, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vira_tr_t0, viral_emergence_covid19_exemplar, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vira_tr_t12, viral_emergence_covid19_exemplar, theater_ratio, 12, 0.75).
narrative_ontology:measurement(vira_tr_t24, viral_emergence_covid19_exemplar, theater_ratio, 24, 0.8).

% Extraction over time
narrative_ontology:measurement(vira_be_t0, viral_emergence_covid19_exemplar, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vira_be_t12, viral_emergence_covid19_exemplar, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(vira_be_t24, viral_emergence_covid19_exemplar, base_extractiveness, 24, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viral_emergence_covid19_exemplar, resource_allocation).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, trust_in_institutions).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, economic_inequality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
