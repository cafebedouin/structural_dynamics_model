% ============================================================================
% CONSTRAINT STORY: demographic_inertia_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_inertia_trap, []).

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
 *   constraint_id: demographic_inertia_trap
 *   human_readable: The Generational Wealth Siphon
 *   domain: social/economic
 *
 * SUMMARY:
 *   The Generational Wealth Siphon describes a scenario where a large, aging
 *   demographic exerts political control to enforce economic transfers
 *   (pensions, healthcare, zoning) from a shrinking youth minority. This
 *   creates a structural disadvantage for younger generations, limiting their
 *   economic opportunities and stifling innovation. The constraint reflects
 *   the power dynamics between generations and the challenges of balancing
 *   competing needs in an aging society.
 *
 * KEY AGENTS:
 *   - Younger Generations: Primary target (powerless/trapped) - Bear the brunt of wealth extraction and have limited exit options.
 *   - Older Generations: Primary beneficiary (institutional/constrained) - Benefit from the system but are also dependent on it.
 *   - Incumbent Politicians: Secondary beneficiary (powerful/constrained) - Maintain power by appealing to the dominant demographic.
 *   - Future Innovation: Victim (moderate/mobile) - Suffers from wealth extraction, which reduces investment opportunities. Some innovation may be offshored
 *   - Analytical Observer: Sees full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_inertia_trap, 0.65).
domain_priors:suppression_score(demographic_inertia_trap, 0.75).
domain_priors:theater_ratio(demographic_inertia_trap, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_inertia_trap, extractiveness, 0.65).
narrative_ontology:constraint_metric(demographic_inertia_trap, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(demographic_inertia_trap, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_inertia_trap, snare).
narrative_ontology:human_readable(demographic_inertia_trap, "The Generational Wealth Siphon").
narrative_ontology:topic_domain(demographic_inertia_trap, "social/economic").

domain_priors:requires_active_enforcement(demographic_inertia_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demographic_inertia_trap, older_generations).
narrative_ontology:constraint_beneficiary(demographic_inertia_trap, incumbent_politicians).
narrative_ontology:constraint_victim(demographic_inertia_trap, younger_generations).
narrative_ontology:constraint_victim(demographic_inertia_trap, future_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Younger generations are trapped in a system that extracts wealth to support older generations, with limited exit options due to legal and social structures.
constraint_indexing:constraint_classification(demographic_inertia_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Older generations benefit from the system through pensions, healthcare, and other social programs, but are also constrained by the system's dependency on a shrinking youth base.
constraint_indexing:constraint_classification(demographic_inertia_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Politicians benefit from the system by maintaining power and appealing to the dominant demographic, but are constrained by the need to balance the demands of different generations.
constraint_indexing:constraint_classification(demographic_inertia_trap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Innovation suffers from wealth extraction due to lower investment. Society continues the system due to institutional inertia, resulting in a degraded but stable process. Some innovation may shift to other countries
constraint_indexing:constraint_classification(demographic_inertia_trap, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% An analytical observer sees the system as a tangled rope, with both coordination (social security) and extraction (wealth transfer) elements, enforced by demographic inertia.
constraint_indexing:constraint_classification(demographic_inertia_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_inertia_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(demographic_inertia_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_inertia_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(demographic_inertia_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(demographic_inertia_trap, TR),
    TR >= 0.70.

:- end_tests(demographic_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. A significant portion of wealth is extracted from younger generations to support older generations. Suppression (0.75): High. Younger generations have limited exit options and are suppressed by the political power of older generations. Theater ratio (0.30): Low. The system has real consequences and is not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   Younger generations experience the system as a snare, with limited exit options and significant wealth extraction. Older generations benefit from the system, but are also constrained by its dependency on a shrinking youth base. Incumbent politicians benefit from the system by maintaining power, but are constrained by the need to balance competing demands. The analytical observer sees the system as a tangled rope, with both coordination and extraction elements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the flow of wealth and political power. Older generations and incumbent politicians benefit from the system, while younger generations bear the costs. The analytical observer sees both coordination (social security) and extraction elements. Exit options constrain this analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between the coordination function of social security and the wealth transfer from younger to older generations. The system has a legitimate coordination function (providing retirement income), but also extracts wealth from younger generations to support older generations, resulting in a snare classification for the powerless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_transition_rate,
    'How quickly will demographic trends shift, and what impact will this have on the system''s stability?',
    'Demographic projections and analysis of birth rates, mortality rates, and migration patterns.',
    'Faster demographic shifts could exacerbate the system''s imbalances and lead to social and economic instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_transition_rate, empirical, 'The rate of demographic transition and its impact on system stability.').

omega_variable(
    political_responsiveness,
    'To what extent are political systems responsive to the needs and concerns of younger generations?',
    'Analysis of political representation, policy outcomes, and public opinion data.',
    'Greater political responsiveness could lead to reforms that mitigate the system''s extractive effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_responsiveness, empirical, 'The responsiveness of political systems to younger generations.').

omega_variable(
    technological_productivity_growth,
    'Will productivity gains offset the wealth extraction, enabling the economy to support both older and younger generations?',
    'Economic modeling and analysis of technological trends and their impact on productivity.',
    'Higher productivity growth could alleviate the burden on younger generations, while slower growth could exacerbate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_productivity_growth, empirical, 'The impact of technological productivity growth on the economy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_inertia_trap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demo_tr_t0, demographic_inertia_trap, theater_ratio, 0, 0.2).
narrative_ontology:measurement(demo_tr_t10, demographic_inertia_trap, theater_ratio, 10, 0.25).
narrative_ontology:measurement(demo_tr_t20, demographic_inertia_trap, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(demo_be_t0, demographic_inertia_trap, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(demo_be_t10, demographic_inertia_trap, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(demo_be_t20, demographic_inertia_trap, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_inertia_trap, resource_allocation).
narrative_ontology:affects_constraint(demographic_inertia_trap, social_security_funding_crisis).
narrative_ontology:affects_constraint(demographic_inertia_trap, youth_economic_mobility_barriers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
