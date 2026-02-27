% ============================================================================
% CONSTRAINT STORY: harry_potter_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harry_potter_liberalism, []).

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
 *   constraint_id: harry_potter_liberalism
 *   human_readable: The Potterverse Liberalism Constraint
 *   domain: socio_political
 *
 * SUMMARY:
 *   This constraint represents the "Mirror of Erised" effect where 1990s
 *   liberal virtues (tolerance, non-violence, institutional trust), embodied
 *   by the Harry Potter narrative, act as a coordination mechanism for one
 *   generation but are perceived as an extractive or atrophied trap by a
 *   subsequent generation facing different material realities. The
 *   Potterverse liberalism, initially intended as a force for good, risks
 *   becoming a means of suppressing dissent and maintaining existing power
 *   structures. The varying perspectives on the system depend on their
 *   relative position within the dominant structure.
 *
 * KEY AGENTS:
 *   - Established Institutions: Primary beneficiary (institutional/arbitrage) – Maintain power and influence
 *   - Marginalized Groups: Primary victim (powerless/trapped) – Trapped in a system that promises equality but fails to deliver it
 *   - Radical Reformers: Secondary victim (moderate/constrained) – Constrained by the system's unwillingness to address systemic issues
 *   - Status Quo Advocates: Degraded/Beneficiary (powerful/constrained) - Institutions who once benefited are now stuck within the framework.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harry_potter_liberalism, 0.55).
domain_priors:suppression_score(harry_potter_liberalism, 0.65).
domain_priors:theater_ratio(harry_potter_liberalism, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harry_potter_liberalism, extractiveness, 0.55).
narrative_ontology:constraint_metric(harry_potter_liberalism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(harry_potter_liberalism, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harry_potter_liberalism, tangled_rope).
narrative_ontology:human_readable(harry_potter_liberalism, "The Potterverse Liberalism Constraint").
narrative_ontology:topic_domain(harry_potter_liberalism, "socio_political").

domain_priors:requires_active_enforcement(harry_potter_liberalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harry_potter_liberalism, established_institutions).
narrative_ontology:constraint_beneficiary(harry_potter_liberalism, status_quo_advocates).
narrative_ontology:constraint_victim(harry_potter_liberalism, marginalized_groups).
narrative_ontology:constraint_victim(harry_potter_liberalism, radical_reformers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For marginalized groups, the Potterverse liberalism becomes a snare, trapping them in a system that promises equality but fails to deliver it. They are powerless and see no exit from the system that perpetuates their marginalization.
constraint_indexing:constraint_classification(harry_potter_liberalism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Radical reformers see the Potterverse liberalism as a tangled rope. They recognize the coordination benefits but are constrained by the system's unwillingness to fundamentally address systemic issues. They benefit to some extent but also experience significant extraction.
constraint_indexing:constraint_classification(harry_potter_liberalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Established institutions benefit from the Potterverse liberalism, seeing it as a rope that maintains order and stability. They are able to arbitrage the system to maintain their power and influence.
constraint_indexing:constraint_classification(harry_potter_liberalism, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Status quo advocates experience the Potterverse liberalism as a piton. The original coordination benefits of the system have largely atrophied, but the system persists due to institutional inertia and performative adherence to liberal values. They are constrained, but the system no longer provides them with significant benefits.
constraint_indexing:constraint_classification(harry_potter_liberalism, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees the Potterverse liberalism as a tangled rope, recognizing both the coordination benefits and the asymmetric extraction that the system creates. They analyze the system's long-term consequences and its impact on different groups.
constraint_indexing:constraint_classification(harry_potter_liberalism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harry_potter_liberalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harry_potter_liberalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harry_potter_liberalism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harry_potter_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(harry_potter_liberalism, TR),
    TR >= 0.70.

:- end_tests(harry_potter_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The system extracts resources and agency from marginalized groups and radical reformers, channeling them towards maintaining the status quo. Suppression (0.65): High. The system suppresses dissent and alternative viewpoints, promoting a narrow range of acceptable opinions and behaviors. Theater ratio (0.60): Moderate. There is a degree of performative adherence to liberal values, but the system also has some functional impact on social justice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differential impact of the Potterverse liberalism on different groups. Established institutions benefit from the system's stability and order, while marginalized groups and radical reformers experience its limitations and constraints. The analytical observer attempts to bridge this gap by analyzing the system's long-term consequences and its impact on various stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Established institutions benefit from the framework, and the marginalized are harmed.
 *
 * MANDATROPHY ANALYSIS:
 *   The question of whether this is a useful coordinating mechanism or merely a trap is at the crux of how this should be viewed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systemic_inequality_vs_individual_agency,
    'Does the Potterverse liberalism primarily address systemic inequality, or does it focus on individual agency and tolerance?',
    'Historical analysis of policies enacted under the influence of Potterverse liberalism; assessment of their impact on inequality and individual freedom',
    'If systemic inequality is the primary focus: the system is more likely to be a rope or scaffold. If individual agency is the primary focus: the system is more likely to be a snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_inequality_vs_individual_agency, empirical, 'The relative emphasis on systemic inequality vs. individual agency.').

omega_variable(
    adaptability_vs_rigidity,
    'Is the Potterverse liberalism able to adapt to changing social and political conditions, or is it becoming increasingly rigid and resistant to change?',
    'Analysis of the system''s response to new challenges and emerging social movements; assessment of its ability to incorporate new ideas and perspectives',
    'If adaptable: the system is more likely to be a rope or scaffold. If rigid: the system is more likely to be a snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptability_vs_rigidity, empirical, 'The system''s adaptability to changing conditions.').

omega_variable(
    performative_vs_functional,
    'To what extent is the Potterverse liberalism performative, with little functional impact on social justice?',
    'Detailed study of how often the language of liberalism is used in politics without meaningful structural change',
    'A high degree of performativity would indicate that the system is in a state of decay, or is more akin to a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_vs_functional, empirical, 'The performative vs functional nature of the system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harry_potter_liberalism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harr_tr_t0, harry_potter_liberalism, theater_ratio, 0, 0.3).
narrative_ontology:measurement(harr_tr_t10, harry_potter_liberalism, theater_ratio, 10, 0.45).
narrative_ontology:measurement(harr_tr_t20, harry_potter_liberalism, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(harr_be_t0, harry_potter_liberalism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(harr_be_t10, harry_potter_liberalism, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(harr_be_t20, harry_potter_liberalism, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harry_potter_liberalism, enforcement_mechanism).
narrative_ontology:affects_constraint(harry_potter_liberalism, neoliberal_policy).
narrative_ontology:affects_constraint(harry_potter_liberalism, institutional_conservatism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
