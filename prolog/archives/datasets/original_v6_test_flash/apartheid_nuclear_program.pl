% ============================================================================
% CONSTRAINT STORY: apartheid_nuclear_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_apartheid_nuclear_program, []).

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
 *   constraint_id: apartheid_nuclear_program
 *   human_readable: Apartheid South Africa's Clandestine Nuclear Program
 *   domain: political/military/technological
 *
 * SUMMARY:
 *   Apartheid South Africa's clandestine nuclear program was a strategic
 *   initiative by the white minority regime to ensure its survival through
 *   nuclear deterrence. The program extracted resources from the general
 *   population, suppressed dissent, and posed a threat to neighboring states
 *   and the international community.
 *
 * KEY AGENTS:
 *   - Apartheid Regime: Primary beneficiary (institutional/constrained) - Sought to ensure regime survival
 *   - Black South Africans: Primary victim (powerless/trapped) - Suffered disproportionately from resource diversion
 *   - Neighboring States: Secondary victim (moderate/constrained) - Faced increased security risks
 *   - White Minority: Powerful group (powerful/constrained) - benefitted from deterrent but risked retaliation
 *   - International Community: Institutional group (institutional/constrained)
 *   - Analytical Observer: Views program from ethical and security perspectives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(apartheid_nuclear_program, 0.75).
domain_priors:suppression_score(apartheid_nuclear_program, 0.8).
domain_priors:theater_ratio(apartheid_nuclear_program, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(apartheid_nuclear_program, extractiveness, 0.75).
narrative_ontology:constraint_metric(apartheid_nuclear_program, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(apartheid_nuclear_program, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(apartheid_nuclear_program, snare).
narrative_ontology:human_readable(apartheid_nuclear_program, "Apartheid South Africa's Clandestine Nuclear Program").
narrative_ontology:topic_domain(apartheid_nuclear_program, "political/military/technological").

domain_priors:requires_active_enforcement(apartheid_nuclear_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(apartheid_nuclear_program, apartheid_regime).
narrative_ontology:constraint_beneficiary(apartheid_nuclear_program, white_minority).
narrative_ontology:constraint_victim(apartheid_nuclear_program, black_south_africans).
narrative_ontology:constraint_victim(apartheid_nuclear_program, neighboring_states).
narrative_ontology:constraint_victim(apartheid_nuclear_program, international_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Black South Africans were trapped under the apartheid regime and disproportionately affected by the program's resource diversion and risk of nuclear conflict.
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Neighboring states faced increased security risks due to the potential for nuclear conflict or proliferation, but had limited ability to influence South Africa's policies.
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The apartheid regime benefited from the program as a deterrent but was constrained by international pressure and the risk of retaliation.
constraint_indexing:constraint_classification(apartheid_nuclear_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The international community initially pressured South Africa to dismantle the program. Now there is a degraded memory of the program. South Africa is held up as a model of nuclear disarmament.
constraint_indexing:constraint_classification(apartheid_nuclear_program, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The white minority benefitted from the program as a deterrent but was constrained by international pressure and the risk of retaliation.
constraint_indexing:constraint_classification(apartheid_nuclear_program, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical Perspective: Views the program as a flawed attempt to secure regime survival, with significant ethical and strategic risks. Recognizes the program's role in regional instability and potential for escalation.
constraint_indexing:constraint_classification(apartheid_nuclear_program, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(apartheid_nuclear_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(apartheid_nuclear_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(apartheid_nuclear_program, TR),
    TR >= 0.70.

:- end_tests(apartheid_nuclear_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high due to the significant resources diverted from social programs to fund the clandestine nuclear program. Suppression is high due to the regime's control over information and its suppression of dissent. The program aimed to enforce racial segregation and prevent internal unrest or external intervention.
 *
 * PERSPECTIVAL GAP:
 *   Black South Africans and neighboring states would view the program as a pure snare, while the apartheid regime may see it as a necessary tangled rope for survival. The analytical observer recognizes the ethical and strategic risks and the limited actual strategic benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   The apartheid regime and white minority benefited, even though constrained by international pressure. Black South Africans and neighboring states were victims. The d value is derived from the relationship of the regime extracting from the population.
 *
 * MANDATROPHY ANALYSIS:
 *   The program is classified as a snare because it primarily extracted resources and suppressed alternatives to maintain the regime's power. There was minimal benefit to the victims, and the threat of nuclear retaliation was a significant risk. Even from the regime's perspective, the benefits were limited and the program's legitimacy was questionable. The mandatrophy is resolved because the program lacked genuine coordination or mutual benefit and served primarily to consolidate power through coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    program_effectiveness,
    'To what extent did the program genuinely deter external threats or internal unrest?',
    'Historical analysis of threat perceptions and policy decisions.',
    'If effective, the program may be seen as a constrained rope. If ineffective, it remains a pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_effectiveness, empirical, 'Evaluating the actual deterrent effect of the nuclear program.').

omega_variable(
    ethical_justification,
    'Can any ethical justification be found for a nuclear weapons program developed by an apartheid regime?',
    'Moral and political philosophy analysis.',
    'Affects long-term views of the program''s legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_justification, conceptual, 'Ethical and moral implications of the program given the nature of the regime.').

omega_variable(
    long_term_impact,
    'What is the long-term impact on nuclear proliferation norms?',
    'Analysis of proliferation trends.',
    'Affects assessment of overall damage to international security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact, empirical, 'Long-term consequences of program on global nuclear norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(apartheid_nuclear_program, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(apar_tr_t0, apartheid_nuclear_program, theater_ratio, 0, 0.2).
narrative_ontology:measurement(apar_tr_t5, apartheid_nuclear_program, theater_ratio, 5, 0.3).
narrative_ontology:measurement(apar_tr_t10, apartheid_nuclear_program, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(apar_be_t0, apartheid_nuclear_program, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(apar_be_t5, apartheid_nuclear_program, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(apar_be_t10, apartheid_nuclear_program, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(apartheid_nuclear_program, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
