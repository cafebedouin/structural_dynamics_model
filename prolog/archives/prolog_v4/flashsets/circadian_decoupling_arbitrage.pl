% ============================================================================
% CONSTRAINT STORY: circadian_decoupling_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_circadian_decoupling_arbitrage, []).

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
 *   constraint_id: circadian_decoupling_arbitrage
 *   human_readable: The Circadian Decoupling Arbitrage
 *   domain: bio_industrial
 *
 * SUMMARY:
 *   This constraint focuses on the bypass of natural circadian rhythms in
 *   industrial settings to maximize productivity and profit. It illustrates
 *   the tension between corporate interests and public health, where the
 *   decoupling of work schedules from the biological clock can lead to
 *   exploitation and negative health outcomes for workers, while benefiting
 *   corporations and their executive management.
 *
 * KEY AGENTS:
 *   - Corporations: Primary beneficiary (institutional/arbitrage) — benefits from increased productivity and operational hours.
 *   - Executive Management: Primary beneficiary (institutional/arbitrage) - benefits from increased profitability and company growth.
 *   - Shift Workers: Primary victim (powerless/trapped) — trapped in schedules that disrupt their circadian rhythms, leading to health issues.
 *   - Public Health: Secondary victim (analytical/analytical) — suffers from the increased rates of chronic diseases and mental health issues associated with circadian decoupling.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(circadian_decoupling_arbitrage, 0.6).
domain_priors:suppression_score(circadian_decoupling_arbitrage, 0.7).
domain_priors:theater_ratio(circadian_decoupling_arbitrage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, extractiveness, 0.6).
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(circadian_decoupling_arbitrage, tangled_rope).
narrative_ontology:human_readable(circadian_decoupling_arbitrage, "The Circadian Decoupling Arbitrage").
narrative_ontology:topic_domain(circadian_decoupling_arbitrage, "bio_industrial").

domain_priors:requires_active_enforcement(circadian_decoupling_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(circadian_decoupling_arbitrage, corporations).
narrative_ontology:constraint_beneficiary(circadian_decoupling_arbitrage, executive_management).
narrative_ontology:constraint_victim(circadian_decoupling_arbitrage, shift_workers).
narrative_ontology:constraint_victim(circadian_decoupling_arbitrage, public_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Shift workers often have limited exit options and are trapped in schedules that disrupt their circadian rhythms, leading to health problems and reduced quality of life. They experience high extraction with little benefit.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Middle managers are constrained by corporate demands but have some mobility. They benefit from corporate success driven by circadian decoupling, but also bear the costs of managing a fatigued and less productive workforce.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Corporations benefit from increased productivity and extended operational hours achieved through circadian decoupling. They can arbitrage labor markets and regulatory environments to maximize profits.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a public health perspective, circadian decoupling poses a significant challenge, leading to increased rates of chronic diseases, mental health issues, and accidents. The long-term societal costs outweigh the short-term economic gains.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(circadian_decoupling_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(circadian_decoupling_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(circadian_decoupling_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because shift workers are often trapped and suffer tangible health consequences. Suppression is high due to limited worker bargaining power and regulatory loopholes. The theater ratio is relatively low since direct efforts are usually enacted to increase productivity and extend operational hours.
 *
 * PERSPECTIVAL GAP:
 *   Corporations perceive circadian decoupling as a necessary strategy for remaining competitive and profitable in a globalized market. However, shift workers experience it as a significant disruption to their health and well-being, with limited control over their schedules. The analytical perspective frames it as a public health issue with long-term societal costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporations and their executive management benefit from increased productivity and profit margins (low d). Shift workers bear the direct health costs of disrupted circadian rhythms (high d). Public health suffers from the societal impact (high d). The directionality reflects this asymmetric distribution of benefits and costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_resistance_threshold,
    'What level of individual resistance (e.g., unionization, legal action) is required to mitigate the extractive effects of circadian decoupling?',
    'Analysis of labor market dynamics, legal frameworks, and health outcomes in regions with varying levels of worker protections.',
    'Determines the feasibility of collective action to counter the corporation’s power, shifting the perspective from snare to potentially tangled rope for the workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_resistance_threshold, empirical, 'Resistance threshold to counter circadian decoupling').

omega_variable(
    corporate_social_responsibility_effectiveness,
    'Can corporate social responsibility initiatives effectively mitigate the negative impacts of circadian decoupling on worker health and well-being?',
    'Comparative analysis of companies with and without robust CSR programs, assessing health outcomes, worker satisfaction, and productivity levels.',
    'Determines if the rope can shift from being purely beneficial to corporations toward a scaffold that improves worker health and ultimately becomes a sustainable rope for all.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corporate_social_responsibility_effectiveness, empirical, 'Effectiveness of CSR in mitigating circadian decoupling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(circadian_decoupling_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(circ_tr_t0, circadian_decoupling_arbitrage, theater_ratio, 0, 0.2).
narrative_ontology:measurement(circ_tr_t5, circadian_decoupling_arbitrage, theater_ratio, 5, 0.25).
narrative_ontology:measurement(circ_tr_t10, circadian_decoupling_arbitrage, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(circ_be_t0, circadian_decoupling_arbitrage, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(circ_be_t5, circadian_decoupling_arbitrage, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(circ_be_t10, circadian_decoupling_arbitrage, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(circadian_decoupling_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(circadian_decoupling_arbitrage, sleep_deprivation_effects).
narrative_ontology:affects_constraint(circadian_decoupling_arbitrage, economic_productivity_metrics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
