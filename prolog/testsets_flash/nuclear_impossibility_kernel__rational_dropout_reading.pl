% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Impossibility: Rational Dropout Reading
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint describes the 'rational dropout' reading of the nuclear
 *   impossibility kernel: nuclear weapons have created a situation where,
 *   while military victory might be technically achievable, the costs
 *   associated with such a victory (mutual annihilation) are so
 *   astronomically high that no rational actor would ever pursue it. The
 *   option of nuclear war remains structurally possible but is effectively
 *   'dropped out' of rational consideration. This is a Mountain of rational
 *   choice, not physical impossibility, as the physical means for war still
 *   exist.
 *
 * KEY AGENTS:
 *   - nuclear_powers: Primary target (institutional/constrained) — constrained by the rational calculus of mutual destruction
 *   - non_nuclear_states: Beneficiary (organized/mobile) — benefit from the stability imposed by nuclear deterrence
 *   - strategic_theorists: Observer (analytical/analytical) — analyze and debate the implications of nuclear deterrence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.05).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.95).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Impossibility: Rational Dropout Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, '47fcddd7-b13c-4bfa-a369-0a73b9bbae10').
narrative_ontology:cs_kernel_codification('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', implicit).
narrative_ontology:cs_authority_grounding('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', diffuse_epistemic).
narrative_ontology:cs_reading_relation('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', foundational, rational_actors_avoid_self_destruction).
narrative_ontology:cs_axiom_status(rational_actors_avoid_self_destruction, holdable).
narrative_ontology:cs_axiom_grounding('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', rational_actors_avoid_self_destruction, deontological).
narrative_ontology:cs_axiom('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', foundational, costs_exceed_benefits_in_nuclear_war).
narrative_ontology:cs_axiom_status(costs_exceed_benefits_in_nuclear_war, holdable).
narrative_ontology:cs_axiom_grounding('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', costs_exceed_benefits_in_nuclear_war, empirically_contingent).
narrative_ontology:cs_reference_frame('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', post_hiroshima_rational_calculus).
narrative_ontology:cs_drift_state('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('47fcddd7-b13c-4bfa-a369-0a73b9bbae10', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, global_stability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, mutually_assured_destruction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States possessing nuclear weapons. They bear the immense costs of maintaining arsenals and are perpetually constrained by the threat of mutual destruction, yet also benefit from the deterrence it provides against conventional attack.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers, payer,
    institutional, generational, constrained, global).

% States without nuclear weapons. They benefit from the global stability and reduced risk of large-scale conventional war that nuclear deterrence provides, without the direct costs or existential risks of possessing such weapons.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, beneficiary,
    organized, generational, mobile, global).

% Academics and policy analysts who study nuclear deterrence, arms control, and international security. They interpret the implications of nuclear weapons for state behavior and global order.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_theorists, observer,
    analytical, civilizational, analytical, universal).

% Organizations and individuals who promote peace, disarmament, and international cooperation. They benefit from the de facto prevention of major power war, even if they abhor the means by which it is achieved.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, global_stability_advocates, beneficiary,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, diffuse).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding among nuclear-armed states that the costs of nuclear war outweigh any conceivable benefits, thereby coordinating their behavior away from direct military confrontation.
% TRANSFER_FUNCTION: Transfers the 'cost' of potential victory from a tangible military objective to an existential threat, effectively making the pursuit of such victory irrational. It also transfers the burden of maintaining global peace from conventional military superiority to nuclear deterrence.
% ABSENT_VOICES: Any actor who believes in the possibility of a 'winnable' nuclear war or who would benefit from a breakdown of the current nuclear order is effectively silenced by the overwhelming rational calculus. These voices are marginalized in mainstream strategic discourse.
% DISAPPEARANCE_RATIONALE: If the rational dropout constraint vanished (i.e., if nuclear war became rationally conceivable and 'winnable'), the world order would fundamentally rearrange. States would re-evaluate their security postures, arms races would intensify, and the risk of large-scale conflict would dramatically increase.
% FOUNDING_PROBLEM: The problem of preventing catastrophic global war between major powers, given the destructive potential of nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, as attested by ongoing strategic dialogues, arms control efforts, and the continued existence of nuclear arsenals. Independent security analysts and international organizations consistently corroborate the existential threat posed by nuclear weapons, confirming the continued relevance of the constraint's founding problem.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the constraint primarily prevents action rather than extracting resources. Suppression is very high (0.95) because the threat of mutual destruction is an overwhelming deterrent, effectively suppressing any rational impulse towards nuclear conflict. Theater ratio is low (0.1) as the constraint's effect is real and not performative, though some 'theater' exists in maintaining credible deterrence postures. Accessibility collapse is high (0.9) as the rational path to victory has collapsed. Resistance is low (0.05) because the constraint is widely accepted as an unavoidable reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers, the constraint is an existential burden that forces a constant state of readiness and the maintenance of costly arsenals, yet it also guarantees their survival. From non-nuclear states, it is a source of global stability, albeit one built on a terrifying premise. Strategic theorists view it as a fundamental alteration of international relations.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are the primary targets (d=1.0) as they bear the direct costs and risks of maintaining nuclear arsenals and are directly constrained by the threat of mutual destruction. Non-nuclear states are beneficiaries (d=0.0) as they benefit from the stability and reduced likelihood of large-scale conventional war that nuclear deterrence provides, without bearing the direct costs or risks. Strategic theorists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing large-scale war) remains acutely live. There is no evidence of mandatrophy; the 'rational dropout' is a persistent feature of the nuclear age. The classification as a Mountain reflects its fundamental, unchangeable nature within the framework of rational action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine rational dropout, or a structural impossibility, or a credibility paradox?',
    'Empirical observation of state behavior under extreme duress, analysis of strategic doctrine evolution, and philosophical debate on the nature of ''rationality'' in existential threats.',
    'If a structural impossibility (structural_contraction_reading), the constraint is a pure Mountain. If a credibility paradox (credibility_paradox_reading), the constraint is a Tangled Rope where the ''deterrence'' function is cover for an unstable, extractive arms race. This reading (rational_dropout_reading) positions it as a Mountain of rational choice, where the option remains but is never chosen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''nuclear_impossibility_kernel'', specifically the ''rational_dropout_reading''. Sibling readings include ''structural_contraction_reading'' and ''credibility_paradox_reading''.').

omega_variable(
    natural_law_vs_constructed_rationality,
    'Is the ''rational dropout'' a natural law of strategic interaction, or a constructed norm of rationality that could be overridden?',
    'Analysis of historical and counterfactual scenarios where actors might deviate from ''rational'' nuclear non-use, and examination of cultural/ideological influences on strategic decision-making.',
    'If purely constructed, the constraint''s ''mountain'' status is contingent on the persistence of the rationality norm, making it a potential False Summit Mountain. If truly a natural law, its persistence is independent of human choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rationality, conceptual, 'Ambiguity between a natural law of rational choice and a constructed norm of strategic rationality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nucl_tr_t10, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(nucl_tr_t20, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(nucl_tr_t30, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(nucl_be_t10, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(nucl_be_t20, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(nucl_be_t30, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(nucl_su_t10, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 10, 0.95).
narrative_ontology:measurement(nucl_su_t20, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 20, 0.95).
narrative_ontology:measurement(nucl_su_t30, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 30, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, non_proliferation_treaty).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, arms_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nuclear_impossibility_kernel'. Each reading offers a distinct structural interpretation of how nuclear weapons constrain state behavior, with different implications for classification and policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
