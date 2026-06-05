% ============================================================================
% CONSTRAINT STORY: jp_nativist_politics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jp_nativist_politics, []).

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
 *   constraint_id: jp_nativist_politics
 *   human_readable: Nativist Online Rhetoric as a Pathway to Political Power in Japan
 *   domain: political
 *
 * SUMMARY:
 *   This constraint examines the rise of nativist online rhetoric as a tool
 *   for gaining political power in Japan, exemplified by figures like Mio
 *   Sugita. This strategy involves using inflammatory and anti-minority
 *   rhetoric to build a political base and influence mainstream discourse.
 *   The constraint highlights the trade-offs between political expediency and
 *   the potential for social division and discrimination.
 *
 * KEY AGENTS:
 *   - Nativist Politicians: Primary beneficiaries (institutional/arbitrage)
 *   - Online Nativist Movements: Organized actors (organized/mobile)
 *   - Minority Groups: Primary victims (powerless/trapped)
 *   - Liberal Opposition: Moderate actors (moderate/constrained)
 *   - Traditional Media Outlets: Institutional actors (institutional/constrained)
 *   - Analytical Observer: Civilizational view (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jp_nativist_politics, 0.65).
domain_priors:suppression_score(jp_nativist_politics, 0.7).
domain_priors:theater_ratio(jp_nativist_politics, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jp_nativist_politics, extractiveness, 0.65).
narrative_ontology:constraint_metric(jp_nativist_politics, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jp_nativist_politics, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jp_nativist_politics, tangled_rope).
narrative_ontology:human_readable(jp_nativist_politics, "Nativist Online Rhetoric as a Pathway to Political Power in Japan").
narrative_ontology:topic_domain(jp_nativist_politics, "political").

domain_priors:requires_active_enforcement(jp_nativist_politics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jp_nativist_politics, nativist_politicians).
narrative_ontology:constraint_beneficiary(jp_nativist_politics, online_nativist_movements).
narrative_ontology:constraint_victim(jp_nativist_politics, minority_groups).
narrative_ontology:constraint_victim(jp_nativist_politics, liberal_opposition).
narrative_ontology:constraint_victim(jp_nativist_politics, public_discourse_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of minority groups targeted by nativist rhetoric. They are trapped in a situation where they are constantly attacked and face discrimination, with limited ability to escape the negative consequences.
constraint_indexing:constraint_classification(jp_nativist_politics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of liberal opposition parties and movements. They are constrained by the need to counter nativist rhetoric while also appealing to a broad electorate. They experience both the costs of the rhetoric and the benefits of increased political engagement, but have limited ability to fully exit the situation.
constraint_indexing:constraint_classification(jp_nativist_politics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of nativist politicians who benefit from using inflammatory rhetoric to gain political power and influence. They can arbitrage the situation by exploiting existing social tensions and resentments to build a political base.
constraint_indexing:constraint_classification(jp_nativist_politics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of organized nativist movements online. They benefit from increased visibility and recruitment through the spread of nativist rhetoric, but also face the risk of being deplatformed or facing legal challenges. They have some mobility in terms of switching platforms or tactics, but are still constrained by the overall political environment.
constraint_indexing:constraint_classification(jp_nativist_politics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective of traditional media outlets. They are constrained by the need to report on nativist rhetoric while also avoiding amplifying it or being accused of bias. They may have once been key agenda-setters, but now find themselves increasingly degraded due to the proliferation of alternative media spaces.
constraint_indexing:constraint_classification(jp_nativist_politics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees the situation as a tangled rope. While it offers nativist politicians a path to power (a coordination function), it simultaneously extracts from minority groups, liberal opposition, and the quality of public discourse. The spread of misinformation and hate speech suppresses constructive dialogue.
constraint_indexing:constraint_classification(jp_nativist_politics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jp_nativist_politics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jp_nativist_politics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jp_nativist_politics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jp_nativist_politics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jp_nativist_politics, TR),
    TR >= 0.70.

:- end_tests(jp_nativist_politics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The nativist rhetoric extracts from minority groups by creating a hostile environment and promoting discrimination. It also extracts from the quality of public discourse by spreading misinformation and hate speech. Suppression (0.70): High. The spread of nativist rhetoric suppresses alternative viewpoints and makes it difficult for minority groups and their allies to express their opinions freely. Theater ratio (0.30): Low. While there is some performative aspect to the rhetoric, it is primarily used to achieve concrete political goals, such as mobilizing voters and influencing policy debates.
 *
 * PERSPECTIVAL GAP:
 *   The nativist politicians see the rhetoric as a rope because it helps them achieve their political goals. The online nativist movements see it as a tangled rope because they benefit from increased visibility and recruitment, but also face the risk of being deplatformed. Minority groups see it as a snare because they are trapped in a situation where they are constantly attacked and face discrimination. The liberal opposition sees it as a tangled rope because they are constrained by the need to counter the rhetoric while also appealing to a broad electorate. Traditional media outlets see it as a piton because they are struggling to maintain their relevance in the face of the rise of online nativist movements. The analytical observer sees it as a tangled rope because it has both a coordination function (helping nativist politicians gain power) and an extraction function (harming minority groups and public discourse).
 *
 * DIRECTIONALITY LOGIC:
 *   Nativist politicians benefit directly by gaining votes and political influence. Online movements grow and find recruits, but also risk deplatforming. Minority groups are directly harmed through increased discrimination and hate speech. The liberal opposition is harmed by the rhetoric, but can also benefit from increased engagement. Traditional media find their authority degraded, but still must report. The analytical observer attempts to balance these effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all these perspectives are valid, depending on the position of the observer. It's not a pure snare, as it enables some level of coordination for nativist politicians. It's not a pure rope as it extracts heavily from vulnerable populations and suppresses open dialogue. Thus, the tangled rope classification balances these competing forces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mainstream_acceptance_threshold,
    'At what point does nativist rhetoric become so normalized that it loses its shock value and effectiveness?',
    'Longitudinal analysis of public opinion polls and media coverage to track the acceptance of nativist rhetoric over time.',
    'If the threshold is low, nativist politicians may need to constantly escalate their rhetoric to maintain attention. If the threshold is high, they may be able to maintain power with a relatively stable level of inflammatory language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mainstream_acceptance_threshold, empirical, 'Threshold of mainstream acceptance of nativist rhetoric.').

omega_variable(
    counter_speech_effectiveness,
    'How effective are counter-speech strategies in combating the spread of nativist rhetoric and reducing its impact on targeted groups?',
    'Controlled experiments and real-world case studies to compare the impact of different counter-speech strategies on public opinion and behavior.',
    'If counter-speech is effective, it could help to reduce the negative consequences of nativist rhetoric. If it is ineffective, it could amplify the rhetoric or backfire by alienating potential allies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_effectiveness, empirical, 'Effectiveness of counter-speech strategies.').

omega_variable(
    digital_platform_responsibility,
    'To what extent should digital platforms be held responsible for the spread of nativist rhetoric on their platforms?',
    'Legal and ethical debates about the responsibilities of digital platforms, as well as analysis of the impact of different platform policies on the spread of nativist rhetoric.',
    'If platforms are held responsible, they may be more likely to take action to remove or limit the spread of nativist rhetoric. If they are not held responsible, they may continue to allow it to spread unchecked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_platform_responsibility, conceptual, 'Responsibility of digital platforms for nativist rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jp_nativist_politics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jp_n_tr_t0, jp_nativist_politics, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jp_n_tr_t5, jp_nativist_politics, theater_ratio, 5, 0.25).
narrative_ontology:measurement(jp_n_tr_t10, jp_nativist_politics, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(jp_n_be_t0, jp_nativist_politics, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jp_n_be_t5, jp_nativist_politics, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(jp_n_be_t10, jp_nativist_politics, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jp_nativist_politics, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
