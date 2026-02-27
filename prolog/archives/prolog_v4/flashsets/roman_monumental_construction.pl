% ============================================================================
% CONSTRAINT STORY: roman_monumental_construction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_monumental_construction, []).

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
 *   constraint_id: roman_monumental_construction
 *   human_readable: The Roman State's Monopoly on Opus Caementicium Construction
 *   domain: socio_technological
 *
 * SUMMARY:
 *   This constraint describes the socio-technical system surrounding Roman
 *   concrete (opus caementicium) for monumental architecture. The Roman state
 *   maintained a near-monopoly on the production and use of this material for
 *   large-scale building projects, enabling the construction of impressive
 *   structures but also suppressing private enterprise and extracting
 *   resources from subject provinces. Roman concrete allowed for
 *   unprecedented scale and speed of construction. However, this power came
 *   at a cost: the state exercised tight control over its production and
 *   deployment, channeling resources and labor towards projects that served
 *   its political and military ambitions.
 *
 * KEY AGENTS:
 *   - Roman State: Primary beneficiary (institutional/constrained) — benefits from the construction of monuments that project power and legitimize rule.
 *   - Roman Elites: Secondary beneficiary (powerful/constrained) — benefit through state allocation of labor, but are constrained by access to Roman State.
 *   - Private Builders: Primary victim (moderate/trapped) — face barriers to entry into monumental construction and are excluded from lucrative contracts.
 *   - Subject Provinces: Primary victim (powerless/trapped) — bear the cost of resource extraction and forced labor.
 *   - Analytical Observer: Assesses long term effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_monumental_construction, 0.7).
domain_priors:suppression_score(roman_monumental_construction, 0.8).
domain_priors:theater_ratio(roman_monumental_construction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_monumental_construction, extractiveness, 0.7).
narrative_ontology:constraint_metric(roman_monumental_construction, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(roman_monumental_construction, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_monumental_construction, snare).
narrative_ontology:human_readable(roman_monumental_construction, "The Roman State's Monopoly on Opus Caementicium Construction").
narrative_ontology:topic_domain(roman_monumental_construction, "socio_technological").

domain_priors:requires_active_enforcement(roman_monumental_construction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_monumental_construction, roman_state).
narrative_ontology:constraint_beneficiary(roman_monumental_construction, roman_elites).
narrative_ontology:constraint_victim(roman_monumental_construction, private_builders).
narrative_ontology:constraint_victim(roman_monumental_construction, subject_provinces).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Subject provinces were heavily extracted from, with resources and labor directed towards state projects. No exit option as they were under Roman control.
constraint_indexing:constraint_classification(roman_monumental_construction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% As the empire declined, the quality of concrete construction decreased and the state's ability to maintain the infrastructure declined. What was once a source of power, became a constraint, with the structures acting as a reminder of past glory.
constraint_indexing:constraint_classification(roman_monumental_construction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Roman elites benefited from access to state-sponsored construction projects but were also constrained by the state's control over the technology. Constrained as they could not build without state authorization. Tangled rope due to the benefit from the extraction happening in other regions.
constraint_indexing:constraint_classification(roman_monumental_construction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Private builders were largely excluded from monumental construction, limiting their opportunities for profit and innovation. Constrained as they could not compete with state sponsored works.
constraint_indexing:constraint_classification(roman_monumental_construction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% An analytical observer sees the mixed nature of this constraint: while it enabled the construction of impressive monuments, it also suppressed alternative building technologies and extracted resources from subject populations.
constraint_indexing:constraint_classification(roman_monumental_construction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_monumental_construction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_monumental_construction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_monumental_construction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_monumental_construction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_monumental_construction, TR),
    TR >= 0.70.

:- end_tests(roman_monumental_construction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70): High. The state extracted resources and labor from conquered territories to fuel its construction projects. Suppression (0.80): High. Private builders were effectively excluded from monumental construction, and alternative building technologies were not pursued. Theater ratio (0.30): Low. The system was primarily functional, with relatively little emphasis on performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions within the socio-technical system. The Roman state sees the system as a way to project power and legitimize rule, while private builders and subject provinces experience it as a form of extraction and suppression. The Roman elite are caught between the institutional power of the Roman State and their own ambitions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries such as the Roman State experience the constraint as coordination, while victims such as subject provinces experience it as extraction. The directionality values reflect these opposing perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a snare because, despite the impressive construction achievements, it suppressed alternative building technologies, extracted resources from conquered territories, and concentrated power in the hands of the state. The monumental construction isn't seen as a helpful tool, but rather as an oppressive regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_limits,
    'To what degree was the knowledge of opus caementicium construction genuinely limited to the Roman state and its authorized contractors?',
    'Archaeological analysis of construction techniques in different regions; historical records of technological diffusion.',
    'If knowledge was tightly controlled: extraction is higher. If knowledge diffused more freely: constraint is weaker (closer to rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_limits, empirical, 'How much knowledge was needed for successful construction').

omega_variable(
    alternative_construction_viability,
    'How viable were alternative construction technologies in the absence of Roman concrete?',
    'Comparative analysis of building costs and durability; studies of alternative materials and techniques.',
    'If alternatives were viable: suppression is lower. If alternatives were limited: suppression is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_construction_viability, empirical, 'Alternative Construction Viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_monumental_construction, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roma_tr_t0, roman_monumental_construction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(roma_tr_t150, roman_monumental_construction, theater_ratio, 150, 0.3).
narrative_ontology:measurement(roma_tr_t300, roman_monumental_construction, theater_ratio, 300, 0.45).

% Extraction over time
narrative_ontology:measurement(roma_be_t0, roman_monumental_construction, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(roma_be_t150, roman_monumental_construction, base_extractiveness, 150, 0.7).
narrative_ontology:measurement(roma_be_t300, roman_monumental_construction, base_extractiveness, 300, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_monumental_construction, resource_allocation).
narrative_ontology:affects_constraint(roman_monumental_construction, roman_military_expansion).
narrative_ontology:affects_constraint(roman_monumental_construction, roman_economic_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
