% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Total War Winnability: Structural Contraction Reading
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'structural contraction' reading of the
 *   post-1945 international system, where the advent of nuclear weapons
 *   fundamentally altered the nature of warfare, rendering total war
 *   physically unwinnable and thus impossible. It is not a social convention
 *   or a normative choice, but a physical reality imposed by the destructive
 *   power of nuclear arsenals. This reading posits that the physical facts of
 *   nuclear weapons are the primary constraint, making other explanations
 *   (normative, cultural) secondary or derivative. The constraint is claimed
 *   as a Mountain due to its physical, irreducible nature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.01).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.01).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Total War Winnability: Structural Contraction Reading").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, '6adad124-2dd3-4fff-b314-dbebf54a5884').
narrative_ontology:cs_kernel_codification('6adad124-2dd3-4fff-b314-dbebf54a5884', implicit).
narrative_ontology:cs_authority_grounding('6adad124-2dd3-4fff-b314-dbebf54a5884', self_enforcing).
narrative_ontology:cs_reading_relation('6adad124-2dd3-4fff-b314-dbebf54a5884', total_war_winnability_post1945__normative_reading_drop, forecloses).
narrative_ontology:cs_reading_relation('6adad124-2dd3-4fff-b314-dbebf54a5884', total_war_winnability_post1945__strategic_culture_drift, forecloses).
narrative_ontology:cs_axiom('6adad124-2dd3-4fff-b314-dbebf54a5884', foundational, total_war_is_self_annihilating).
narrative_ontology:cs_axiom_status(total_war_is_self_annihilating, holdable).
narrative_ontology:cs_axiom_grounding('6adad124-2dd3-4fff-b314-dbebf54a5884', total_war_is_self_annihilating, empirically_contingent).
narrative_ontology:cs_reference_frame('6adad124-2dd3-4fff-b314-dbebf54a5884', pre_nuclear_total_war_paradigm).
narrative_ontology:cs_drift_state('6adad124-2dd3-4fff-b314-dbebf54a5884', post_nuclear_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6adad124-2dd3-4fff-b314-dbebf54a5884', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the physical impossibility of total war, as it prevents global annihilation. However, it remains trapped by the underlying nuclear reality and the existential risk of accidental or limited nuclear exchange.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Administer and maintain nuclear arsenals, which are the physical basis of this constraint. They are constrained by the 'second-strike capability' and 'mutually assured destruction' (MAD) logic, which makes total war unwinnable. They do not extract from the constraint, but manage its existence.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Bear the diffuse costs and risks associated with living under the nuclear umbrella, including the threat of proliferation, regional instability, and the potential for accidental nuclear exchange. They have limited agency to alter the fundamental constraint.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Analyze the implications of nuclear weapons for international relations, developing theories like deterrence and arms control. They observe the constraint's operation and its effects on state behavior.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_theorists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint implicitly coordinates state behavior by making total war an irrational and physically impossible option, thereby forcing states to seek alternative means of conflict resolution or competition.
% TRANSFER_FUNCTION: It transfers the existential risk of global annihilation from the realm of human choice to the realm of physical reality, imposing a 'cost' of living under constant nuclear threat, but preventing the ultimate cost of total war.
% ABSENT_VOICES: Historical proponents of total war as a viable strategic option are absent, as their premise has been physically foreclosed. Future generations, if they could speak, would likely object to the persistent existential risk, even if total war is impossible.
% DISAPPEARANCE_RATIONALE: If the physical impossibility of total war vanished (e.g., through a technological breakthrough rendering nuclear weapons obsolete and harmless, or a global disarmament that genuinely removed the threat), the fundamental calculus of international relations would rearrange. States might once again consider large-scale, decisive conflicts, and the global security architecture would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of preventing catastrophic, civilization-ending conflict between great powers, which became acutely apparent with the advent of nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: The problem of preventing total war remains live, as evidenced by ongoing nuclear deterrence strategies, arms control efforts, and the continued existence of nuclear arsenals. Strategic studies scholars and international relations practitioners universally corroborate this, independent of the nuclear powers' self-interest.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.01, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is negligible (0.01) because the constraint does not actively extract resources or value from any party; it simply defines the boundaries of what is physically possible. Suppression is very high (0.95) because total war, as a viable strategic option, is almost entirely suppressed by the threat of mutual annihilation. Theater ratio is very low (0.05) as the constraint's operation is based on physical reality, not performance. Accessibility collapse is near total (0.98) as the alternative (winnable total war) is physically foreclosed. Resistance is minimal (0.02) because one cannot 'resist' a physical impossibility.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a fixed, objective reality. Other readings (normative, cultural) might perceive it as a choice or a social construct, leading to significant perspectival divergence on the constraint's fundamental nature and persistence mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanity is a beneficiary of the constraint's existence, as it prevents global catastrophe, but this is not an extractive benefit. Nuclear powers are agenda-setters in that they manage the nuclear reality, but they are also constrained by it. Non-nuclear states are payers of diffuse risk. No party actively extracts from the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_social_impossibility,
    'Is total war truly physically impossible, or is its absence primarily maintained by strong normative and cultural constraints that could, in principle, be overcome?',
    'Analysis of counterfactual scenarios where normative/cultural constraints are weakened or removed, to determine if the physical reality of nuclear weapons alone would still prevent total war. This involves examining the ''rationality'' of MAD under extreme stress.',
    'If primarily normative/cultural, the constraint would be reclassified from Mountain to a form of Rope or Tangled Rope, with identifiable beneficiaries of the normative order and potential for extraction through its maintenance. If purely physical, the Mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_social_impossibility, conceptual, 'Ambiguity between physical impossibility and social/normative suppression of total war.').

omega_variable(
    beneficiary_extraction_ambiguity,
    'While humanity benefits from the absence of total war, is there any subtle, indirect extraction by nuclear powers from the maintenance of this ''physical'' constraint (e.g., through geopolitical leverage or resource allocation for nuclear programs)?',
    'Detailed economic and political analysis of resource flows and power dynamics associated with nuclear deterrence, specifically looking for rents derived from the ''nuclear peace'' beyond security provision.',
    'If significant indirect extraction is found, the constraint''s extractiveness would be re-evaluated upward, potentially shifting it from Mountain to a False Summit Mountain or even a Tangled Rope, as the ''natural'' aspect would be compromised by active rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_ambiguity, empirical, 'Whether the ''benefit'' to humanity is truly non-extractive, or if nuclear powers derive subtle rents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(tota_tr_t1980, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(tota_tr_t2000, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1960, 0.01).
narrative_ontology:measurement(tota_be_t1980, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1980, 0.01).
narrative_ontology:measurement(tota_be_t2000, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2000, 0.01).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2024, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1960, 0.95).
narrative_ontology:measurement(tota_su_t1980, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1980, 0.95).
narrative_ontology:measurement(tota_su_t2000, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This 'structural contraction' reading posits physical impossibility as the primary mechanism, influencing but foreclosing the normative and strategic-cultural explanations as primary causes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
