% ============================================================================
% CONSTRAINT STORY: humanities_phd_funding_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humanities_phd_funding_model, []).

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
 *   constraint_id: humanities_phd_funding_model
 *   human_readable: The 'Fully Funded' Humanities PhD Model in the US
 *   domain: economic/social
 *
 * SUMMARY:
 *   The 'fully funded' humanities PhD model in the US relies on a system
 *   where universities provide tuition waivers and stipends to graduate
 *   students in exchange for their labor as teachers and researchers.
 *   However, this system often results in the exploitation of graduate
 *   students, who work long hours for little pay and face a highly
 *   competitive job market. The model also relies on a growing pool of
 *   adjunct faculty who receive even less compensation. The system benefits
 *   universities and tenured faculty who obtain cheap labor and enhance their
 *   research productivity.
 *
 * KEY AGENTS:
 *   - PhD Students: Primary target (powerless/trapped) - Exploited labor force with limited exit options.
 *   - Universities: Primary beneficiary (institutional/arbitrage) - Obtains cheap labor for teaching and research.
 *   - Tenured Faculty: Secondary beneficiary (powerful/constrained) - Benefits from cheap labor and increased research productivity.
 *   - Adjunct Faculty: Secondary target (powerless/constrained) - Underpaid and overworked instructors with limited job security.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humanities_phd_funding_model, 0.6).
domain_priors:suppression_score(humanities_phd_funding_model, 0.7).
domain_priors:theater_ratio(humanities_phd_funding_model, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humanities_phd_funding_model, extractiveness, 0.6).
narrative_ontology:constraint_metric(humanities_phd_funding_model, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(humanities_phd_funding_model, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humanities_phd_funding_model, tangled_rope).
narrative_ontology:human_readable(humanities_phd_funding_model, "The 'Fully Funded' Humanities PhD Model in the US").
narrative_ontology:topic_domain(humanities_phd_funding_model, "economic/social").

domain_priors:requires_active_enforcement(humanities_phd_funding_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humanities_phd_funding_model, universities).
narrative_ontology:constraint_beneficiary(humanities_phd_funding_model, tenured_faculty).
narrative_ontology:constraint_victim(humanities_phd_funding_model, phd_students).
narrative_ontology:constraint_victim(humanities_phd_funding_model, adjunct_faculty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The PhD student is trapped in a system where they are expected to work long hours for little pay, with limited exit options due to the sunk cost fallacy. They are vulnerable to exploitation by advisors and departments.
constraint_indexing:constraint_classification(humanities_phd_funding_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Universities benefit from the PhD system by obtaining cheap labor for teaching and research. They are able to arbitrage the system by paying students less than market value for their labor. The reputation of the university can be enhanced by the publications and teaching performed by graduate students.
constraint_indexing:constraint_classification(humanities_phd_funding_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The system provides some coordination (training scholars) but is highly extractive to students and leads to overproduction of PhDs relative to available academic jobs. Alternate models for training future educators and researchers do exist; the current funding regime actively suppresses the spread of alternatives.
constraint_indexing:constraint_classification(humanities_phd_funding_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Tenured faculty benefit from cheap labor by graduate students and have a constrained exit option due to their investment in the current system. They also face increased pressure to publish which makes them reliant on grad student research.
constraint_indexing:constraint_classification(humanities_phd_funding_model, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humanities_phd_funding_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(humanities_phd_funding_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(humanities_phd_funding_model, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(humanities_phd_funding_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humanities_phd_funding_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the system relies on cheap labor and suppresses alternative career paths. Suppression is also high due to the competitive job market and the pressure to conform to academic norms. Theater ratio reflects that much of the 'training' and 'mentorship' are performative, designed to legitimize exploitation.
 *
 * PERSPECTIVAL GAP:
 *   The PhD student sees a snare because they are trapped in a system with limited exit options and high levels of exploitation. The university sees a rope because they are able to obtain cheap labor and enhance their reputation. The analytical observer sees a tangled rope because the system provides some coordination (training scholars) but is highly extractive and suppresses alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   PhD students are victims because they bear the costs of the system (low pay, long hours, limited job prospects). Universities are beneficiaries because they obtain cheap labor. Tenured faculty are both beneficiaries and victims, as they benefit from cheap labor but also face pressure to maintain the system. Adjunct faculty are victims as they are largely more exploited than graduate students.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    job_market_viability,
    'What is the ratio of PhD graduates to available academic jobs?',
    'Tracking job postings and PhD graduation rates over time.',
    'High ratio indicates a snare for students; low ratio suggests a more sustainable system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(job_market_viability, empirical, 'Ratio of PhD graduates to academic jobs.').

omega_variable(
    alt_ac_success,
    'What is the success rate of PhD graduates in alternative academic careers (alt-ac)?',
    'Surveying PhD graduates and tracking their career trajectories.',
    'High success rate suggests a safety net; low rate indicates higher vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alt_ac_success, empirical, 'Success rate of PhDs in alt-ac careers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humanities_phd_funding_model, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humanities_phd_funding_model, theater_ratio, 0, 0.3).
narrative_ontology:measurement(huma_tr_t10, humanities_phd_funding_model, theater_ratio, 10, 0.4).
narrative_ontology:measurement(huma_tr_t20, humanities_phd_funding_model, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humanities_phd_funding_model, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(huma_be_t10, humanities_phd_funding_model, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(huma_be_t20, humanities_phd_funding_model, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humanities_phd_funding_model, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
