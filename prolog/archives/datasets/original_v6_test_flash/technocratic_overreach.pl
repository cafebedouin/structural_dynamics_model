% ============================================================================
% CONSTRAINT STORY: technocratic_overreach
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technocratic_overreach, []).

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
 *   constraint_id: technocratic_overreach
 *   human_readable: Technocratic Overreach
 *   domain: political/technological
 *
 * SUMMARY:
 *   The rule of the expert is a scenario where administrative or technical
 *   experts impose restrictive constraints under the guise of 'optimization'
 *   or 'safety,' effectively removing the subject's ability to participate in
 *   decision-making. This can lead to a situation where the experts'
 *   preferences are prioritized over the needs and desires of the general
 *   public.
 *
 * KEY AGENTS:
 *   - General Public: Primary target (powerless/trapped) - bears the cost of restricted choices and lack of participation.
 *   - Technical Elite: Primary beneficiary (institutional/arbitrage) - benefits from increased power and influence.
 *   - Governing Body: Secondary beneficiary (institutional/constrained) - benefits from expertise in making decisions, but constrained by expert's recommendations.
 *   - Impacted Community: Targeted Subgroup (powerless/trapped) - A subgroup of the general public, particularly affected by the experts' decisions.
 *   - Advocacy Groups: Organized Actor (organized/mobile) - Attempts to represent impacted community, but constrained by unequal access to power/influence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technocratic_overreach, 0.6).
domain_priors:suppression_score(technocratic_overreach, 0.7).
domain_priors:theater_ratio(technocratic_overreach, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technocratic_overreach, extractiveness, 0.6).
narrative_ontology:constraint_metric(technocratic_overreach, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(technocratic_overreach, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technocratic_overreach, snare).
narrative_ontology:human_readable(technocratic_overreach, "Technocratic Overreach").
narrative_ontology:topic_domain(technocratic_overreach, "political/technological").

domain_priors:requires_active_enforcement(technocratic_overreach).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technocratic_overreach, technical_elite).
narrative_ontology:constraint_beneficiary(technocratic_overreach, governing_body).
narrative_ontology:constraint_victim(technocratic_overreach, general_public).
narrative_ontology:constraint_victim(technocratic_overreach, impacted_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual citizen experiences the rule of the expert as a snare. They are often trapped, with limited ability to exit the system or challenge the expert's decisions. The expert knowledge is used to suppress dissenting opinions.
constraint_indexing:constraint_classification(technocratic_overreach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The governing body (e.g., elected officials) may experience this as a tangled rope. While they benefit from the expertise in making policy decisions, they are also constrained by the experts' recommendations and the potential for public backlash if they deviate.
constraint_indexing:constraint_classification(technocratic_overreach, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The technical experts experience this as a rope. They benefit from increased power and influence, and face less scrutiny on their decisions. They might have arbitrage options by moving to different positions with similar power.
constraint_indexing:constraint_classification(technocratic_overreach, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Advocacy groups may be constrained, but not trapped. They benefit from the focus the issue brings to their cause, and are able to mobilize people for political action, but are forced to dedicate time, effort and other resources to the issue.
constraint_indexing:constraint_classification(technocratic_overreach, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From a long-term, global perspective, this can be seen as a tangled rope, where expertise is useful but may lead to unintended consequences and erosion of democratic processes.
constraint_indexing:constraint_classification(technocratic_overreach, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technocratic_overreach_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technocratic_overreach, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_overreach, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technocratic_overreach, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technocratic_overreach_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The experts extract decision-making power from the general public, leading to a situation where their preferences are prioritized. Suppression (0.7): The experts suppress dissenting opinions by framing them as 'unscientific' or 'irrational.' Theater ratio (0.3): The experts may engage in performative actions to create a sense of legitimacy, but the actual impact of their decisions is often minimal.
 *
 * PERSPECTIVAL GAP:
 *   The general public experiences this as a snare, as they are trapped and their choices are restricted. The experts, on the other hand, experience this as a rope, as they benefit from increased power and influence. The governing body experiences this as a tangled rope, as they are both constrained and benefited by the experts' recommendations.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the structural position of each agent. The general public is powerless and trapped, so their directionality is high. The experts are institutional and have arbitrage options, so their directionality is low. The governing body is institutional but constrained, so their directionality is somewhere in the middle.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by considering the intent and impact of the experts' decisions. If the experts are genuinely trying to optimize for the benefit of the general public, then the situation may be a legitimate coordination problem. However, if the experts are simply trying to increase their own power and influence, then it is a pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_expertise,
    'Is the expertise truly relevant to the decision being made, or is it being used as a smokescreen?',
    'Independent review of the expert''s qualifications and the evidence they are presenting.',
    'If the expertise is relevant, the situation may be a legitimate coordination problem. If not, it is a pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_expertise, empirical, 'Relevance of expertise').

omega_variable(
    transparency_of_process,
    'Is the decision-making process transparent and open to public input?',
    'Review of public records and interviews with stakeholders.',
    'If the process is transparent, there is a greater chance of accountability and less potential for overreach. If not, it is more likely a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_of_process, empirical, 'Transparency of decision-making').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technocratic_overreach, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technocratic_overreach, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tech_tr_t5, technocratic_overreach, theater_ratio, 5, 0.2).
narrative_ontology:measurement(tech_tr_t10, technocratic_overreach, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technocratic_overreach, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tech_be_t5, technocratic_overreach, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(tech_be_t10, technocratic_overreach, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technocratic_overreach, enforcement_mechanism).
narrative_ontology:affects_constraint(technocratic_overreach, regulatory_capture).
narrative_ontology:affects_constraint(technocratic_overreach, information_asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
