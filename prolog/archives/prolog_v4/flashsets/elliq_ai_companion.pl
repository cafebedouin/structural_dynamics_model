% ============================================================================
% CONSTRAINT STORY: elliq_ai_companion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elliq_ai_companion, []).

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
 *   constraint_id: elliq_ai_companion
 *   human_readable: State-Funded AI Companionship for Seniors
 *   domain: technological / social_policy
 *
 * SUMMARY:
 *   A state-sponsored program deploys the ElliQ AI robot into the homes of
 *   socially isolated seniors to combat loneliness. While the program aims to
 *   improve the well-being of seniors, it also creates new dependencies and
 *   alters existing social structures. The AI companion provides some relief
 *   from loneliness, it can also lead to decreased desire for human
 *   interaction. This program benefits the AI robot manufacturer, while
 *   potentially harming senior autonomy and displacing human caregivers.
 *
 * KEY AGENTS:
 *   - Socially Isolated Seniors: Primary target (powerless/trapped) - reliant on the state for companionship
 *   - AI Robot Manufacturer: Primary beneficiary (institutional/arbitrage) - gains revenue from government contracts
 *   - Human Caregivers: Affected party (moderate/constrained) - potential job displacement
 *   - State Government: Implementing body (institutional/constrained) - balancing benefits and costs
 *   - Senior Advocacy Groups: Organized agent (organized/mobile) - advocate for senior welfare
 *   - Analytical Observer: Civilizational perspective (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elliq_ai_companion, 0.55).
domain_priors:suppression_score(elliq_ai_companion, 0.4).
domain_priors:theater_ratio(elliq_ai_companion, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elliq_ai_companion, extractiveness, 0.55).
narrative_ontology:constraint_metric(elliq_ai_companion, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(elliq_ai_companion, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elliq_ai_companion, tangled_rope).
narrative_ontology:human_readable(elliq_ai_companion, "State-Funded AI Companionship for Seniors").
narrative_ontology:topic_domain(elliq_ai_companion, "technological / social_policy").

domain_priors:requires_active_enforcement(elliq_ai_companion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elliq_ai_companion, socially_isolated_seniors).
narrative_ontology:constraint_beneficiary(elliq_ai_companion, ai_robot_manufacturer).
narrative_ontology:constraint_victim(elliq_ai_companion, senior_autonomy).
narrative_ontology:constraint_victim(elliq_ai_companion, human_caregivers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the socially isolated senior. While the AI companion provides some relief from loneliness, it can also lead to dependency and a decreased desire for human interaction. Trapped due to lack of alternative social connections and reliance on state support.
constraint_indexing:constraint_classification(elliq_ai_companion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of the AI robot manufacturer. Benefits from government contracts and increased market share. Experiences the program as a coordination mechanism that promotes their product. Can arbitrage by shifting focus to other markets if the state program is terminated.
constraint_indexing:constraint_classification(elliq_ai_companion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of human caregivers. The AI companion can reduce their workload but also potentially devalue their role and lead to job displacement. Constrained because they may have limited alternative employment options in elderly care. Extraction is the potential displacement; coordination is the reduced workload.
constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of senior advocacy groups. They see benefits in reducing loneliness and improving well-being, but they recognize potential downsides: the loss of autonomy, data privacy concerns, and the potential for over-reliance on technology. Relatively mobile by advocating for improvements or alternative solutions.
constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective of the state government. Benefits from improved senior well-being and reduced healthcare costs, but bears the costs of program implementation, maintenance, and potential ethical concerns. Constrained by budget limitations and political pressures.
constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observer perspective. The program attempts to address a genuine need but creates new forms of dependency and potentially displaces human connection.  The long-term social and psychological impacts are uncertain.
constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elliq_ai_companion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elliq_ai_companion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elliq_ai_companion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elliq_ai_companion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(elliq_ai_companion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55. The program extracts autonomy and agency from the seniors who rely on it, and it potentially extracts employment opportunities from human caregivers. The state requires active enforcement to maintain the program and ensure that seniors are using it appropriately. Suppression: 0.40. Seniors may have limited alternatives for social interaction, and caregivers may have limited alternative job options. The state actively enforces the program and may discourage seniors from seeking alternative companionship if the program is seen as a solution. Theater Ratio: 0.30. The program may have a performative aspect, with the state showcasing its commitment to senior care, but the AI companions do provide genuine companionship and assistance.
 *
 * PERSPECTIVAL GAP:
 *   The socially isolated senior views the program as a snare, offering limited exit options and extracting autonomy. The AI robot manufacturer sees it as a rope, facilitating their business goals. Human caregivers experience it as a tangled rope, offering some benefits (reduced workload) but also posing a threat (job displacement). Advocacy groups and state actors also perceive it as tangled, recognizing both positive and negative aspects. The analytical observer highlights the inherent tensions and uncertainties of relying on AI for companionship.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the agent's structural position in relation to the program. The AI robot manufacturer, as the primary beneficiary, experiences low extraction. The seniors, as the targets of the program, experience high extraction. Human caregivers and advocacy groups experience mixed extraction and coordination, depending on the perspective. The state government faces the challenge of balancing benefits and costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The program is classified as a Tangled Rope because it embodies elements of both coordination and extraction. It coordinates care for the elderly but may also extract personal data and reduce incentives for human contact. It addresses a pressing social need but poses ethical questions. The differing perspectives reveal the tensions inherent in this type of program.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_dependency_threshold,
    'What level of AI interaction constitutes unhealthy dependency, and how can it be measured?',
    'Longitudinal studies on senior well-being, social interaction, and cognitive function with varying levels of AI companionship.',
    'If dependency threshold is low, the program may be harmful. If dependency threshold is high, the program may be beneficial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_dependency_threshold, empirical, 'Threshold for AI dependency').

omega_variable(
    human_caregiver_displacement,
    'To what extent will AI companions displace human caregivers, and what are the economic and social consequences?',
    'Economic modeling and labor market analysis examining the impact of AI companions on the elderly care workforce.',
    'Significant displacement could lead to job losses and decreased quality of care for seniors with complex needs. Minimal displacement indicates a positive complementary role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_caregiver_displacement, empirical, 'Degree of human caregiver displacement').

omega_variable(
    data_privacy_risk,
    'What are the data privacy risks associated with AI companions, and how can they be mitigated?',
    'Penetration testing and vulnerability assessments of AI companion systems, coupled with strong data privacy regulations and enforcement.',
    'High risk could lead to data breaches and exploitation of vulnerable seniors. Low risk suggests the program is ethical and responsible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_privacy_risk, empirical, 'Level of data privacy risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elliq_ai_companion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elli_tr_t0, elliq_ai_companion, theater_ratio, 0, 0.1).
narrative_ontology:measurement(elli_tr_t5, elliq_ai_companion, theater_ratio, 5, 0.2).
narrative_ontology:measurement(elli_tr_t10, elliq_ai_companion, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(elli_be_t0, elliq_ai_companion, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(elli_be_t5, elliq_ai_companion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(elli_be_t10, elliq_ai_companion, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elliq_ai_companion, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
