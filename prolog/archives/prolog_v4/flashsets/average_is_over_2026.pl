% ============================================================================
% CONSTRAINT STORY: average_is_over_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_average_is_over_2026, []).

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
 *   constraint_id: average_is_over_2026
 *   human_readable: The AI-Talent Barbell Economy
 *   domain: economic/technological
 *
 * SUMMARY:
 *   As of 2026, the economy increasingly rewards a combination of natural
 *   talent and "internal pressure" to work with intelligent machines. This
 *   creates a barbell-shaped distribution of economic opportunities, where a
 *   small group of highly skilled individuals and AI tool owners thrive,
 *   while average skilled workers face increasing competition and
 *   displacement. The active enforcement comes from the market dynamics
 *   favoring those who can best utilize AI.
 *
 * KEY AGENTS:
 *   - ai_tool_owners: Primary beneficiary (institutional/arbitrage) - benefit from increased productivity
 *   - top_tier_talent: Secondary beneficiary (powerful/mobile) - benefit from increased productivity and compensation
 *   - average_skilled_workers: Primary victim (powerless/trapped) - face displacement
 *   - workers_without_ai_access: Secondary victim (moderate/constrained) - face reduced earning potential
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(average_is_over_2026, 0.6).
domain_priors:suppression_score(average_is_over_2026, 0.5).
domain_priors:theater_ratio(average_is_over_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(average_is_over_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(average_is_over_2026, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(average_is_over_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(average_is_over_2026, tangled_rope).
narrative_ontology:human_readable(average_is_over_2026, "The AI-Talent Barbell Economy").
narrative_ontology:topic_domain(average_is_over_2026, "economic/technological").

domain_priors:requires_active_enforcement(average_is_over_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(average_is_over_2026, ai_tool_owners).
narrative_ontology:constraint_beneficiary(average_is_over_2026, top_tier_talent).
narrative_ontology:constraint_victim(average_is_over_2026, average_skilled_workers).
narrative_ontology:constraint_victim(average_is_over_2026, workers_without_ai_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The average skilled worker, without access to AI tools or specialized training, is increasingly trapped in low-value tasks and facing displacement. They have limited exit options and bear a disproportionate cost.
constraint_indexing:constraint_classification(average_is_over_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Workers who lack sufficient AI access or the skills to effectively utilize these technologies find themselves in a constrained situation. They can participate in the AI-augmented economy, but with significantly diminished returns.
constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Those who own and control AI tools, as well as top-tier talent who can leverage these tools most effectively, experience the AI-talent barbell economy as a rope. They benefit from increased productivity and earning potential.
constraint_indexing:constraint_classification(average_is_over_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a long-term analytical perspective, the AI-talent barbell economy represents a complex interplay of coordination and extraction. AI tools can enhance productivity and create new economic opportunities, but they also exacerbate existing inequalities.
constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(average_is_over_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(average_is_over_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(average_is_over_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(average_is_over_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(average_is_over_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.60 - reflects the increasing disparity between the top and bottom of the economic ladder. Suppression: 0.50 - reflects the limited access to AI tools and training for average workers, hindering their ability to compete. Theater ratio: 0.30 - indicates that while there is some performative activity around AI adoption, there is also a significant real impact on productivity and economic outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The average skilled worker views the AI-talent barbell economy as a snare, due to the lack of opportunity to use AI and the potential job displacement. The AI tool owners view the situation as a coordination rope, due to the increase in productivity.
 *
 * DIRECTIONALITY LOGIC:
 *   AI tool owners and top tier talent benefit greatly, with little cost. Average and unskilled workers bear a larger cost, and do not receive the same benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling a coordination mechanism as pure extraction. While AI can be extractive to the powerless, that extraction is part of a larger more complex phenomenon
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_upgrading_effectiveness,
    'How effective are current skill-upgrading programs in enabling average workers to utilize AI tools effectively?',
    'Empirical studies on the impact of skill-upgrading programs on worker productivity and earnings.',
    'If effective, the barbell economy may become less pronounced. If ineffective, inequality may widen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_upgrading_effectiveness, empirical, 'Effectiveness of skill-upgrading programs.').

omega_variable(
    ai_tool_accessibility,
    'Will access to advanced AI tools remain concentrated in the hands of a few or become more widely distributed?',
    'Analysis of market trends in AI tool pricing and availability, as well as policy interventions to promote access.',
    'Wider distribution could mitigate the negative impacts on average workers. Concentrated access could exacerbate inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_tool_accessibility, empirical, 'Accessibility of advanced AI tools.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(average_is_over_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aver_tr_t0, average_is_over_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(aver_tr_t5, average_is_over_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(aver_tr_t10, average_is_over_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(aver_be_t0, average_is_over_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(aver_be_t5, average_is_over_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(aver_be_t10, average_is_over_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(average_is_over_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
