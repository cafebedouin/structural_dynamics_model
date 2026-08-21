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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Impossibility Kernel: Rational Dropout Reading
 *   domain: strategic_studies/international_relations/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This constraint describes the 'rational dropout' reading of the nuclear
 *   impossibility kernel: nuclear weapons have made the costs of large-scale
 *   war between major powers so high that such a conflict is no longer a
 *   rational choice, even if technically possible. This reading emphasizes
 *   the role of rational actors in recognizing and responding to the changed
 *   cost-benefit calculus. It is distinct from readings that posit physical
 *   impossibility or inherent incredibility of threats.
 *
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
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Impossibility Kernel: Rational Dropout Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations/nuclear_deterrence_theory").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'a6b13852-e23a-4977-a145-b1f4012dec01').
narrative_ontology:cs_kernel_codification('a6b13852-e23a-4977-a145-b1f4012dec01', implicit).
narrative_ontology:cs_authority_grounding('a6b13852-e23a-4977-a145-b1f4012dec01', self_enforcing).
narrative_ontology:cs_reading_relation('a6b13852-e23a-4977-a145-b1f4012dec01', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6b13852-e23a-4977-a145-b1f4012dec01', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('a6b13852-e23a-4977-a145-b1f4012dec01', foundational, nuclear_war_costs_exceed_benefits).
narrative_ontology:cs_axiom_status(nuclear_war_costs_exceed_benefits, holdable).
narrative_ontology:cs_axiom_grounding('a6b13852-e23a-4977-a145-b1f4012dec01', nuclear_war_costs_exceed_benefits, empirically_contingent).
narrative_ontology:cs_axiom('a6b13852-e23a-4977-a145-b1f4012dec01', foundational, rational_actors_avoid_unprofitable_war).
narrative_ontology:cs_axiom_status(rational_actors_avoid_unprofitable_war, holdable).
narrative_ontology:cs_axiom_grounding('a6b13852-e23a-4977-a145-b1f4012dec01', rational_actors_avoid_unprofitable_war, empirically_contingent).
narrative_ontology:cs_reference_frame('a6b13852-e23a-4977-a145-b1f4012dec01', post_hiroshima_rational_deterrence).
narrative_ontology:cs_drift_state('a6b13852-e23a-4977-a145-b1f4012dec01', contemporary_strategic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a6b13852-e23a-4977-a145-b1f4012dec01', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, global_stability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the stability provided by nuclear deterrence, which prevents large-scale conventional wars between major powers. They are 'locked in' by the perceived necessity of maintaining a deterrent capability, even if they would prefer a world without nuclear weapons.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers, beneficiary,
    institutional, generational, identity_locked, global).

% Bear the diffuse costs of living under the nuclear shadow, including the risk of accidental war and the diversion of resources to non-proliferation efforts. Their options are limited to diplomatic pressure or seeking security guarantees.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Study the implications of nuclear weapons for international relations, modeling scenarios and assessing the stability of deterrence. They are not directly subject to the constraint but analyze its structural effects.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_analysts, observer,
    analytical, civilizational, analytical, global).

% Advocate for arms control and non-proliferation, seeing the rational dropout as a fragile but real constraint that prevents catastrophic conflict. They benefit from the absence of major power war, but are constrained by the persistence of nuclear arsenals.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, global_stability_advocates, beneficiary,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding among nuclear-armed states that the costs of nuclear war outweigh any conceivable benefits, thereby coordinating their behavior away from direct military confrontation.
% TRANSFER_FUNCTION: Transfers the option of large-scale conventional war between major powers from the 'reachable' set of rational choices to the 'unthinkable' set, effectively transferring resources and attention away from such conflicts.
% ABSENT_VOICES: Historical military strategists who believed in the possibility of 'winning' a nuclear war, or those who advocate for pre-emptive strikes, are largely absent from contemporary mainstream strategic discourse, their views marginalized by the perceived futility of nuclear conflict.
% DISAPPEARANCE_RATIONALE: If the rational dropout constraint vanished overnight (e.g., through a technological breakthrough that made nuclear war survivable and winnable), the strategic landscape would fundamentally shift. Major powers would re-evaluate military options, potentially leading to a new arms race and increased risk of direct conflict.
% FOUNDING_PROBLEM: The problem of preventing catastrophic, civilization-ending war between great powers in an era of unprecedented destructive capability.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested as live by international relations scholars, policymakers, and non-proliferation experts across various institutions and national contexts, outside of the immediate nuclear powers themselves. Ongoing arms control treaties and diplomatic efforts further corroborate its persistence.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The extractiveness is low (0.05) because the constraint primarily removes an option rather than actively extracting resources, and its 'cost' is the foregone possibility of large-scale war. Suppression is very high (0.95) because the constraint is maintained by the existential threat of nuclear retaliation, which effectively suppresses any rational consideration of major power conflict. Theater ratio is low (0.1) as the constraint's effect is largely structural and not performative. Accessibility collapse is high (0.9) because the rational path to victory has largely collapsed. Resistance is low (0.05) as the constraint is widely accepted as a necessary evil for global stability.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers might perceive this as a 'mountain' of strategic reality, an unchangeable fact of international relations. Non-nuclear states, while benefiting from the stability, might view it as a 'snare' that limits their agency and exposes them to existential risk without their consent. Strategic analysts, from an 'analytical' seat, would see the structural constraint and its implications.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are beneficiaries (d near 0.0) as they gain security and stability from the constraint, even if they bear the cost of maintaining arsenals. Non-nuclear states are diffuse payers (d near 0.5) as they live under the nuclear shadow without direct control. Global stability advocates are beneficiaries (d near 0.0) as their primary goal is served by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a 'mountain' because the underlying physical reality of nuclear weapons' destructive power creates an irreducible limit on rational action. It prevents mislabeling as a 'snare' by acknowledging the genuine, albeit terrifying, coordination function of preventing global war. The constraint's mandate (preventing catastrophic war) remains live, and its persistence is due to the enduring nature of nuclear destructive capability, not institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_assumption_robustness,
    'How robust is the assumption of rational actors in a crisis, and could non-rational factors (miscalculation, escalation, psychological stress) override the ''rational dropout''?',
    'Historical analysis of near-misses (e.g., Cuban Missile Crisis), psychological studies of decision-making under extreme stress, and theoretical modeling of escalation dynamics.',
    'If rationality is frequently overridden, the constraint''s effective suppression might be lower than measured, and its classification could shift towards a ''tangled_rope'' or ''snare'' if the ''dropout'' is less reliable than assumed, leading to unintended extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_assumption_robustness, empirical, 'Uncertainty about the reliability of rational decision-making in nuclear crises.').

omega_variable(
    nuclear_impossibility_kernel_reading_distinction,
    'Is this constraint a genuine ''rational dropout'' (costs exceed benefits) or a ''structural contraction'' (physical impossibility of victory) or a ''credibility paradox'' (threats are incredible)?',
    'Analysis of strategic doctrine and historical decision-making: does the discourse emphasize cost-benefit analysis, physical limits, or the inherent incredibility of threats? This is a conceptual distinction between the kernel''s readings.',
    'If the ''structural contraction'' reading is more accurate, the constraint is a more absolute ''mountain'' with even lower extractiveness. If the ''credibility paradox'' reading is more accurate, the constraint might be a ''tangled_rope'' or ''snare'' due to the inherent instability of an incredible threat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nuclear_impossibility_kernel_reading_distinction, conceptual, 'Distinguishing between the different readings of the nuclear impossibility kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.05).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1980, 0.04).
narrative_ontology:measurement(nucl_be_t2000, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2000, 0.03).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.95).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1980, 0.9).
narrative_ontology:measurement(nucl_su_t2000, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nuclear_impossibility_kernel', focusing on the rational-choice aspect. Sibling readings include 'structural_contraction_reading' (physical impossibility) and 'credibility_paradox_reading' (inherent incredibility of threats).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
