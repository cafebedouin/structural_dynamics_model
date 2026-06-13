% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: UNSC Article 27 Veto Power (Sovereignty Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint models the P5 veto power in the UN Security Council as an
 *   instantiation of the Westphalian sovereignty principle, specifically
 *   applied to great powers with global-reach enforcement capacity. From this
 *   'sovereignty reading,' the veto is not a choice but a structural
 *   inevitability: no state, particularly one with nuclear weapons and the
 *   capacity to project power globally, can be bound by international law
 *   without its consent. Any attempt to create a global institution that
 *   could compel such a state would simply fail, as the state would disregard
 *   or dismantle it. Thus, the veto is a reflection of the physical reality
 *   of power distribution, making it a Mountain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.02).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "UNSC Article 27 Veto Power (Sovereignty Reading)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '0a319a7e-7e14-49e9-9b68-6e31f47576e2').
narrative_ontology:cs_kernel_codification('0a319a7e-7e14-49e9-9b68-6e31f47576e2', formalized).
narrative_ontology:cs_authority_grounding('0a319a7e-7e14-49e9-9b68-6e31f47576e2', self_enforcing).
narrative_ontology:cs_reading_relation('0a319a7e-7e14-49e9-9b68-6e31f47576e2', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a319a7e-7e14-49e9-9b68-6e31f47576e2', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('0a319a7e-7e14-49e9-9b68-6e31f47576e2', foundational, great_power_cannot_be_compelled).
narrative_ontology:cs_axiom_status(great_power_cannot_be_compelled, holdable).
narrative_ontology:cs_axiom_grounding('0a319a7e-7e14-49e9-9b68-6e31f47576e2', great_power_cannot_be_compelled, empirically_contingent).
narrative_ontology:cs_axiom('0a319a7e-7e14-49e9-9b68-6e31f47576e2', foundational, westphalian_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(westphalian_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('0a319a7e-7e14-49e9-9b68-6e31f47576e2', westphalian_sovereignty_is_absolute, deontological).
narrative_ontology:cs_reference_frame('0a319a7e-7e14-49e9-9b68-6e31f47576e2', anarchic_international_system).
narrative_ontology:cs_drift_state('0a319a7e-7e14-49e9-9b68-6e31f47576e2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0a319a7e-7e14-49e9-9b68-6e31f47576e2', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_27_veto_power__sovereignty_reading, non_p5_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the veto power, which is seen as an inherent attribute of their sovereign power and global enforcement capacity. They are not 'benefiting' from it in a constructed sense, but rather operating within its bounds as a given structural reality.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, p5_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate within an international system where great powers cannot be compelled without their consent. They experience the veto as a limit on collective action, but from this reading, it is a reflection of an unchangeable reality, not an imposed cost.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_p5_states, payer,
    moderate, biographical, constrained, global).

% Analyze the structural implications of the veto power, debating whether it is a necessary evil, a design flaw, or a reflection of fundamental geopolitical realities. This reading aligns with a realist perspective on international relations.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, the veto does not 'coordinate' in the sense of solving a collective action problem among states. Instead, it reflects the pre-existing coordination failure inherent in a system of sovereign great powers that cannot be compelled.
% TRANSFER_FUNCTION: No direct transfer of resources or benefits is identified from this reading. The veto is seen as preventing certain actions, rather than facilitating a transfer.
% ABSENT_VOICES: From this perspective, there are no 'absent voices' whose presence would fundamentally alter the constraint, as the veto reflects an underlying reality of power. Any voice advocating for compelling great powers would be seen as ignoring this reality.
% DISAPPEARANCE_RATIONALE: If the veto 'disappeared' overnight, it would simply mean that great powers would revert to unilateral action or form new alliances outside the UN framework, as the underlying reality of their sovereignty and enforcement capacity would remain. The world would not 'rearrange' around a new, more powerful international body, but rather revert to a less institutionalized form of great-power politics.
% FOUNDING_PROBLEM: The veto was built to acknowledge the reality that great powers would not submit to an international organization that could compel them into action against their vital interests, especially after two world wars.
% FOUNDING_PROBLEM_CORROBORATION: The P5 states themselves consistently attest that the founding problem (the need to prevent great-power war by ensuring consent) is still live. Realist international relations scholars, from outside the benefiting parties, corroborate this view, arguing that the veto is a necessary evil reflecting the anarchic nature of the international system.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_unchanged).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and suppression (0.02) reflect the view that the veto is not 'extracting' from anyone, nor is it 'suppressing' alternatives in a coercive sense. Instead, it merely reflects an irreducible limit on what international institutions can achieve given the distribution of power. The high accessibility collapse (0.9) indicates that alternatives to this power distribution (e.g., a global government that could compel great powers) are seen as structurally impossible. Resistance is low (0.05) because, from this perspective, resistance to the veto is resistance to the underlying reality of state power, which is futile.
 *
 * PERSPECTIVAL GAP:
 *   This reading posits the veto as a fundamental, unchangeable aspect of international relations, akin to a natural law. Other readings (coordination, oligopoly) would see it as a constructed mechanism with identifiable beneficiaries and victims, leading to different classifications. The divergence is precisely what the kernel analysis is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   From the sovereignty reading, there are no 'beneficiaries' or 'victims' in the conventional sense. The P5 states are not 'benefiting' from the veto; they are simply exercising an inherent attribute of their sovereign power. Other states are not 'victims' of the veto; they are simply operating within the constraints of a world where great powers cannot be compelled. The constraint derives from the physical reality of power distribution, not from a human-designed system with a beneficiary structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_coordination_ambiguity,
    'Is the P5 veto a structural inevitability reflecting the distribution of global power (sovereignty reading), or a coordination mechanism to prevent great-power conflict (coordination reading)?',
    'Counterfactual analysis: if a global institution could compel great powers without their consent, would it be stable? If not, the sovereignty reading is strengthened.',
    'If the sovereignty reading is correct, the veto is a Mountain; if the coordination reading is primary, it''s a Rope or Tangled Rope, with different implications for reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_coordination_ambiguity, conceptual, 'Ambiguity between the veto as a reflection of power distribution and as a coordination mechanism.').

omega_variable(
    sovereignty_vs_oligopoly_ambiguity,
    'Is the P5 veto a necessary expression of great-power sovereignty, or a mechanism for entrenching a geopolitical oligopoly and extracting authority rents?',
    'Analysis of institutional evolution: if the veto consistently blocks reforms that would redistribute power without clear security justifications, the oligopoly reading is strengthened.',
    'If the sovereignty reading is correct, the veto is a Mountain; if the oligopoly reading is primary, it''s a Snare, with implications for legitimacy and reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_oligopoly_ambiguity, conceptual, 'Ambiguity between the veto as sovereignty and as oligopolistic rent extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t15, article_27_veto_power__sovereignty_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__sovereignty_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(arti_be_t15, article_27_veto_power__sovereignty_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__sovereignty_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__sovereignty_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(arti_su_t15, article_27_veto_power__sovereignty_reading, suppression_requirement, 15, 0.02).
narrative_ontology:measurement(arti_su_t30, article_27_veto_power__sovereignty_reading, suppression_requirement, 30, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the P5 veto power. This 'sovereignty reading' views the veto as a structural inevitability, while the 'coordination reading' sees it as a conflict-prevention mechanism, and the 'oligopoly reading' as a tool for rent extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
