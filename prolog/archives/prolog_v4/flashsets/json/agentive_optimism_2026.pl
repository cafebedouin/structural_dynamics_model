% ============================================================================
% CONSTRAINT STORY: agentive_optimism_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agentive_optimism_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: agentive_optimism_2026
 *   human_readable: The Agentive Optimism Gap
 *   domain: political/social
 *
 * SUMMARY:
 *   A fundamental divide has emerged between the 'weird' policy-making class,
 *   defined by a rare sense of personal agency and optimism, and a segment of
 *   the public defined by 'overpowering pessimism'. This creates a structural
 *   problem where the needs and concerns of the pessimists are not adequately
 *   addressed, leading to further disempowerment and social division.
 *
 * KEY AGENTS:
 *   - Policy-Making Elite: Primary beneficiary (institutional/arbitrage) - Benefits from maintaining the status quo and their position of influence.
 *   - Overpowering Pessimists: Primary victim (powerless/trapped) - Feels disenfranchised and unable to change their circumstances.
 *   - Analytical Observer: Analytical observer (analytical/analytical) - Analyzes the gap and its implications for society.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agentive_optimism_2026, 0.55).
domain_priors:suppression_score(agentive_optimism_2026, 0.7).
domain_priors:theater_ratio(agentive_optimism_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agentive_optimism_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(agentive_optimism_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(agentive_optimism_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agentive_optimism_2026, tangled_rope).
narrative_ontology:human_readable(agentive_optimism_2026, "The Agentive Optimism Gap").
narrative_ontology:topic_domain(agentive_optimism_2026, "political/social").

domain_priors:requires_active_enforcement(agentive_optimism_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agentive_optimism_2026, policy_making_elite).
narrative_ontology:constraint_victim(agentive_optimism_2026, overpowering_pessimists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The 'Overpowering Pessimist' - Feels trapped by circumstances, with no agency to change their situation. Sees the gap as a barrier to progress and suffers from policies that do not address their needs.
constraint_indexing:constraint_classification(agentive_optimism_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: The Policy-Making Elite - Benefits from the gap by maintaining control and influence. Their optimism, while potentially genuine, is reinforced by their ability to navigate and manipulate the system to their advantage.
constraint_indexing:constraint_classification(agentive_optimism_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 3: The Analytical Observer - Sees the gap as a complex interplay of social, economic, and political factors. Recognizes the coordination benefits for the elite but also the significant extraction from those who feel disempowered.
constraint_indexing:constraint_classification(agentive_optimism_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agentive_optimism_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agentive_optimism_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agentive_optimism_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agentive_optimism_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(agentive_optimism_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The policy-making elite benefits from the gap by maintaining their position of power. The pessimism of a segment of the public, whether justified or not, is extracted from because it justifies current power dynamic. Suppression (0.70): High. The feeling of 'overpowering pessimism' suppresses attempts to organize or resist policies, leading to limited political action from the oppressed. Theater ratio (0.30): Low. The policy makers feel that this a true, fundamental gap in agency and optimism and believe that their policies help more than they hurt.
 *
 * PERSPECTIVAL GAP:
 *   The policy-making elite views the 'agentive optimism gap' as a coordination mechanism. They see their policies as beneficial and necessary for societal progress, even if some segments of the public are not on board. The overpoweringly pessimistic view the gap as an extraction mechanism. They see the policies as benefiting the elite while not addressing their problems. The analytical observer sees both sides, understanding that the gap is a complex issue involving both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agent's perceived power and ability to exit the situation. The elite has power and can arbitrage the situation to their advantage. The powerless are trapped and feel the full force of the extraction. The analyst has no structural relation to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by classifying a social phenomenon with drastically different perspectives. The elite view the current structure as a coordinating influence, whereas the underprivileged view the structure as a snare. The analytical perspective confirms these viewpoints are both valid, from different levels of power, time, exit, and scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_definition,
    'What constitutes genuine agency versus a perception of agency influenced by privilege and access?',
    'Qualitative research, surveys, and analysis of social mobility data to understand the lived experiences of different groups.',
    'If agency is largely a function of privilege, then the gap is more of an extractive snare. If agency is more equally distributed, the gap may represent a failure of communication or policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_definition, conceptual, 'The structural ambiguity over whether agency is real or a perception').

omega_variable(
    policy_effectiveness,
    'To what extent do policies address the underlying causes of pessimism and disempowerment?',
    'Longitudinal studies tracking the impact of specific policies on the well-being and sense of agency of different groups.',
    'If policies are effective, the gap may narrow over time. If policies are ineffective or exacerbate existing inequalities, the gap may widen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_effectiveness, empirical, 'How the efficacy of policy affects the size of the gap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agentive_optimism_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agen_tr_t0, agentive_optimism_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(agen_tr_t5, agentive_optimism_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(agen_tr_t10, agentive_optimism_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(agen_be_t0, agentive_optimism_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(agen_be_t5, agentive_optimism_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(agen_be_t10, agentive_optimism_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
