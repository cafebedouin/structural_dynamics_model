% ============================================================================
% CONSTRAINT STORY: eu_unanimity_rule_foreign_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_unanimity_rule_foreign_policy, []).

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
 *   constraint_id: eu_unanimity_rule_foreign_policy
 *   human_readable: EU Unanimity Requirement for Foreign Policy and Financial Decisions
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The EU's requirement for unanimous agreement among all member states on
 *   key foreign policy and financial matters creates a complex constraint.
 *   While it protects national interests and prevents overreach, it also
 *   allows individual states to block collective action, potentially
 *   undermining the EU's effectiveness and credibility on the global stage.
 *   This rule benefits states able to extract concessions in exchange for
 *   their consent and hinders the EU's ability to act decisively. The theater
 *   ratio is high due to the performative nature of EU foreign policy
 *   discussions, where the need for unanimity often leads to watered-down or
 *   symbolic actions.
 *
 * KEY AGENTS:
 *   - Member States Blocking Consensus: Beneficiaries (institutional/arbitrage)
 *   - EU Collective Action Capacity: Victim (powerless/trapped)
 *   - Vulnerable Member States: Victim (powerless/trapped)
 *   - External Actors Opposed to EU Policy: Beneficiary (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_unanimity_rule_foreign_policy, 0.6).
domain_priors:suppression_score(eu_unanimity_rule_foreign_policy, 0.7).
domain_priors:theater_ratio(eu_unanimity_rule_foreign_policy, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, extractiveness, 0.6).
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_unanimity_rule_foreign_policy, tangled_rope).
narrative_ontology:human_readable(eu_unanimity_rule_foreign_policy, "EU Unanimity Requirement for Foreign Policy and Financial Decisions").
narrative_ontology:topic_domain(eu_unanimity_rule_foreign_policy, "geopolitical").

domain_priors:requires_active_enforcement(eu_unanimity_rule_foreign_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, member_states_blocking_consensus).
narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, external_actors_opposed_eu_policy).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, eu_collective_action_capacity).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, vulnerable_member_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Vulnerable member states are trapped by the system. They are highly susceptible to coercion or inducement to support policies against their interests, as they lack the power to effect change alone.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% Member states that block consensus can benefit from the rule, achieving specific national objectives or extracting concessions. They have arbitrage options by threatening to veto, thereby increasing their leverage.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% The analytical observer sees the rule as a tangled rope because it serves some coordination functions but also enables extraction. It hampers the EU's ability to act effectively on the world stage (extraction) but also ensures that member states' core interests are protected (coordination).
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The EU High Representative responsible for foreign policy sees the unanimity rule as a piton because it constrains their ability to act decisively and reduces the EU's credibility. The role is constrained because they can't effectively shape policy without consensus, and the suppression level remains high because the rule is institutionally entrenched. The high theater ratio reflects the performative nature of the role given the constraints.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_unanimity_rule_foreign_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_unanimity_rule_foreign_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_unanimity_rule_foreign_policy, TR),
    TR >= 0.70.

:- end_tests(eu_unanimity_rule_foreign_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Individual member states can extract significant concessions by threatening to veto policies. Suppression (0.70): High. The unanimity rule severely restricts the EU's ability to act without the consent of all members. Theater Ratio (0.75): High. The EU's foreign policy efforts can appear performative due to this underlying structural constraint. The need for consensus often leads to symbolic actions or watered-down policies that lack real impact.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ depending on the actor's power and exit options. Vulnerable member states are trapped and see the rule as a snare, while states with the power to block consensus see it as a rope, allowing them to extract benefits. The analytical observer recognizes the dual nature of the rule, viewing it as a tangled rope that both coordinates and extracts. The EU High Representative experiences the constraint as a piton, as their role is undermined by the need for unanimity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural positions. Beneficiary status is associated with lower d values, indicating less extraction, while victim status is associated with higher d values, indicating more extraction. The ability to exit the situation also influences d; greater exit options reduce extraction experienced.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled rope because it is a coordination mechanism with a significant extraction component. A snare classification would not capture the coordination benefit the rule is intended to provide, and a rope classification would ignore the opportunities for extraction. The high extractiveness and suppression, combined with the coordination function, make tangled rope the most accurate classification. The piton perspective of the EU High Representative highlights the degraded functionality of the EU's foreign policy apparatus due to the unanimity rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_sovereignty_vs_eu_unity,
    'What is the acceptable balance between respecting national sovereignty and achieving EU unity in foreign policy?',
    'Analysis of historical cases, polling data on public opinion, and comparative institutional design.',
    'Determines whether the unanimity rule is seen as a necessary safeguard or an impediment to effective action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_sovereignty_vs_eu_unity, preference, 'The tradeoff between national sovereignty and EU unity.').

omega_variable(
    external_actor_influence,
    'To what extent do external actors exploit the unanimity rule to undermine EU foreign policy?',
    'Intelligence analysis, diplomatic disclosures, and study of lobbying efforts.',
    'If significant, strengthens the argument for reform of the rule. If minor, weakens it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_actor_influence, empirical, 'The level of influence from external actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_unanimity_rule_foreign_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_u_tr_t0, eu_unanimity_rule_foreign_policy, theater_ratio, 0, 0.6).
narrative_ontology:measurement(eu_u_tr_t5, eu_unanimity_rule_foreign_policy, theater_ratio, 5, 0.7).
narrative_ontology:measurement(eu_u_tr_t10, eu_unanimity_rule_foreign_policy, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(eu_u_be_t0, eu_unanimity_rule_foreign_policy, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(eu_u_be_t5, eu_unanimity_rule_foreign_policy, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(eu_u_be_t10, eu_unanimity_rule_foreign_policy, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_unanimity_rule_foreign_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, eu_fiscal_policy_coordination).
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, nato_burden_sharing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
