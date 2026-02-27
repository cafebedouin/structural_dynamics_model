% ============================================================================
% CONSTRAINT STORY: agency_atrophy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agency_atrophy, []).

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
 *   constraint_id: agency_atrophy
 *   human_readable: The Outsourced Cognition Trap
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   The 'Outsourced Cognition Trap' describes a scenario where reliance on
 *   automated decision-support systems (AI assistants, predictive navigation,
 *   automated scheduling) becomes so pervasive that individuals cease to
 *   exercise the underlying cognitive skills. While these technologies
 *   initially appear as a 'Rope,' enhancing efficiency and convenience, their
 *   long-term effect can be a 'Tangled Rope,' where individuals become
 *   increasingly dependent and their independent cognitive abilities atrophy.
 *   This dynamic can lead to a 'Snare' from the perspective of general
 *   cognitive skills, with a decline in cognitive independence at a
 *   population level.
 *
 * KEY AGENTS:
 *   - Technology Providers: Beneficiaries (institutional/arbitrage) - Gain from widespread adoption of technology.
 *   - General Cognitive Skills: Primary Victim (powerless/trapped) - Loss of cognitive ability due to dependence.
 *   - Independent Decision-Making: Secondary Victim (moderate/constrained) - Individual autonomy eroded by algorithms.
 *   - Cognitive Training Programs: Scaffold (organized/mobile) - Attempt to mitigate cognitive atrophy with training.
 *   - Traditional Education Systems: Piton (institutional/constrained) - Slowly adapts to the new reality of technology dependence.
 *   - Analytical Observer: Civilizational view (analytical/analytical) - Recognizes trade-off between cognitive load and cognitive independence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agency_atrophy, 0.55).
domain_priors:suppression_score(agency_atrophy, 0.7).
domain_priors:theater_ratio(agency_atrophy, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agency_atrophy, extractiveness, 0.55).
narrative_ontology:constraint_metric(agency_atrophy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(agency_atrophy, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agency_atrophy, tangled_rope).
narrative_ontology:human_readable(agency_atrophy, "The Outsourced Cognition Trap").
narrative_ontology:topic_domain(agency_atrophy, "technological/cognitive").

domain_priors:requires_active_enforcement(agency_atrophy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agency_atrophy, technology_providers).
narrative_ontology:constraint_beneficiary(agency_atrophy, early_adopters).
narrative_ontology:constraint_victim(agency_atrophy, general_cognitive_skills).
narrative_ontology:constraint_victim(agency_atrophy, independent_decision_making).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL COGNITIVE SKILLS (SNARE) - Representing the collective loss of cognitive ability across a population increasingly reliant on automated systems. The abstract 'skill commons' is powerless and trapped, bearing the full cost of cognitive atrophy with no exit option.
constraint_indexing:constraint_classification(agency_atrophy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT DECISION-MAKING (TANGLED ROPE) - Reflects the individual's experience of having their decision-making autonomy eroded by constant algorithmic nudges. While benefiting from the convenience of automation, the individual is simultaneously constrained by the system's influence, leading to a mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(agency_atrophy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY PROVIDERS (ROPE) - Represents the perspective of companies providing the automated decision support. They benefit from the widespread adoption of their technologies and experience it as a coordination mechanism that enhances market reach and revenue.
constraint_indexing:constraint_classification(agency_atrophy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COGNITIVE TRAINING PROGRAMS (SCAFFOLD) - Reflects a scenario where individuals actively engage in cognitive training to mitigate the effects of cognitive outsourcing. This represents a temporary support structure with a sunset clause: as individuals regain cognitive independence, the need for such programs diminishes. Active enforcement is required in the form of motivating individuals to participate.
constraint_indexing:constraint_classification(agency_atrophy, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: TRADITIONAL EDUCATION SYSTEMS (PITON) - Represents the perspective of legacy educational institutions that may be slow to adapt to the changing cognitive landscape. Their traditional methods become performative, failing to adequately address the atrophy of general cognitive skills due to technology dependence. They are constrained by established curricula and resistant to change, resulting in a degraded function and a high theater ratio.
constraint_indexing:constraint_classification(agency_atrophy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) - Viewed through the lens of Cognitive Load Theory, the phenomenon reflects a fundamental trade-off between reducing immediate cognitive load through outsourcing and the long-term consequences of skill atrophy. This perspective suggests a natural limit to cognitive outsourcing and that constant offloading may lead to reduced cognitive capabilities. The system naturally gravitates to less effort.
constraint_indexing:constraint_classification(agency_atrophy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agency_atrophy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agency_atrophy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agency_atrophy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agency_atrophy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agency_atrophy, TR),
    TR >= 0.70.

:- end_tests(agency_atrophy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The constraint actively extracts cognitive skills from individuals as they offload tasks to automated systems. While there is initial benefit (convenience, reduced cognitive load), the long-term effect leads to a substantial loss of cognitive capacity. Suppression (0.70): High. The dominance and convenience of automated systems suppress the need for individuals to develop and exercise their own cognitive abilities. The systems are so ingrained that alternatives are difficult to adopt. Theater ratio (0.20): Low. There is relatively little performative activity associated with the constraint. The primary effect is a direct reduction in cognitive effort and skill development.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different experiences of the agents. Technology providers see a rope, enhancing coordination and efficiency. Cognitive skills, viewed collectively, experience the situation as a snare where skills are extracted. Individuals experience a tangled rope, benefiting from convenience while also losing autonomy. Cognitive Training Programs sees the temporary solution as a scaffold. Education sees a slowly evolving requirement for reform, resulting in the piton classification. The analytical observer sees the cognitive tradeoffs as a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Technology Providers) gain market share and revenue, experiencing the constraint as a coordination mechanism (Rope). Victims (General Cognitive Skills, Independent Decision-Making) bear the cost of cognitive atrophy and reduced autonomy, experiencing it as extraction (Snare, Tangled Rope). Cognitive Training Programs experience a Scaffold providing limited, temporary support. Traditional Educational Systems have a delayed and distorted view of the overall dynamic, classifying it as a Piton. An Analytical Observer, studying the system using Cognitive Load Theory, perceives the natural tension.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by understanding the indexical nature of the classifications. The same situation can simultaneously be beneficial for some agents and detrimental for others. It is not a matter of which type is 'correct' but understanding that each type represents a distinct perspective on the same constraint. The perspective of General Cognitive Skills clarifies that, viewed at the abstract level, a crucial resource is gradually being depleted from our cognitive system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_transferability,
    'To what extent are specific cognitive skills atrophied by automation transferable to other domains?',
    'Empirical studies measuring cognitive performance in different tasks after periods of automation-induced skill decline.',
    'If transferability is low: Atrophy is domain-specific, less generalized. If transferability is high: Atrophy has broader consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_transferability, empirical, 'How much skill decline in automated tasks affects other areas.').

omega_variable(
    reversibility_of_atrophy,
    'How easily can atrophied cognitive skills be regained after periods of automation dependence?',
    'Longitudinal studies tracking cognitive skill recovery after deliberate reduction in automation use.',
    'If easily reversible: Atrophy is a temporary inconvenience. If difficult to reverse: Atrophy represents a permanent loss of capability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_atrophy, empirical, 'How much training is needed to return to prior skills.').

omega_variable(
    cognitive_diversity_value,
    'How much is diversity of cognitive skill valued in society?',
    'Socio-economic and cultural analysis of the value and rewards associated with different types of cognitive skills.',
    'If highly valued: More incentive to maintain skills. If not valued: Less incentive to resist skill atrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_diversity_value, preference, 'The societal value of cognitive capabilities that are lost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agency_atrophy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agen_tr_t0, agency_atrophy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(agen_tr_t5, agency_atrophy, theater_ratio, 5, 0.15).
narrative_ontology:measurement(agen_tr_t10, agency_atrophy, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(agen_be_t0, agency_atrophy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(agen_be_t5, agency_atrophy, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(agen_be_t10, agency_atrophy, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agency_atrophy, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
