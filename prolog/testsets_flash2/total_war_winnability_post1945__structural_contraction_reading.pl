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
 *   human_readable: Total War Winnability Post-1945 (Structural Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the structural impossibility of winning a total
 *   war between major powers after the advent of nuclear weapons. It is a
 *   'structural contraction' reading of the 'total_war_winnability_post1945'
 *   kernel, arguing that nuclear weapons fundamentally altered the physical
 *   possibility space, not merely social norms or strategic culture. The
 *   constraint is Mountain-class because it is an irreducible
 *   physical/logical limit: attempting total war with nuclear weapons
 *   guarantees mutual destruction, making 'victory' meaningless. There are no
 *   direct beneficiaries in the sense of rent-collection, but all states are
 *   'beneficiaries' of the absence of total war. The victims are the
 *   populations in any counterfactual nuclear exchange.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.01).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.99).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.01).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.99).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Total War Winnability Post-1945 (Structural Contraction Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, '3cb9ae0e-ed68-438e-9712-e0ddc319539e').
narrative_ontology:cs_kernel_codification('3cb9ae0e-ed68-438e-9712-e0ddc319539e', implicit).
narrative_ontology:cs_authority_grounding('3cb9ae0e-ed68-438e-9712-e0ddc319539e', self_enforcing).
narrative_ontology:cs_reading_relation('3cb9ae0e-ed68-438e-9712-e0ddc319539e', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('3cb9ae0e-ed68-438e-9712-e0ddc319539e', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('3cb9ae0e-ed68-438e-9712-e0ddc319539e', foundational, mutual_assured_destruction_is_physical_reality).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_physical_reality, holdable).
narrative_ontology:cs_axiom_grounding('3cb9ae0e-ed68-438e-9712-e0ddc319539e', mutual_assured_destruction_is_physical_reality, empirically_contingent).
narrative_ontology:cs_reference_frame('3cb9ae0e-ed68-438e-9712-e0ddc319539e', pre_nuclear_strategic_paradigm).
narrative_ontology:cs_drift_state('3cb9ae0e-ed68-438e-9712-e0ddc319539e', post_nuclear_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3cb9ae0e-ed68-438e-9712-e0ddc319539e', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_winnability_post1945__structural_contraction_reading, populations_in_counterfactual_nuclear_exchange).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the means to initiate a nuclear exchange, but are also constrained by the physical impossibility of winning a total war. Their strategic choices are shaped by this structural limit.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_armed_states, agenda_setter,
    institutional, generational, constrained, global).

% Bear the existential risk of nuclear war without possessing the means to deter it directly. Their security is fundamentally altered by the structural contraction of the total war option for major powers.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states, payer,
    moderate, generational, trapped, global).

% Are the ultimate victims of the structural impossibility of winning a total war, as any attempt to wage such a war would result in their annihilation. Their 'cost' is hypothetical but absolute.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, populations_in_counterfactual_nuclear_exchange, payer,
    powerless, immediate, trapped, universal).

% Analyze the implications of nuclear weapons for international relations, debating whether the constraint is physical, normative, or cultural. This reading posits a physical, structural limit.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint itself does not coordinate; it is a physical limit that forces states to coordinate by making total war unwinnable. It implicitly coordinates by removing a destructive option.
% TRANSFER_FUNCTION: It transfers the concept of 'winnable total war' from the realm of possibility to impossibility, effectively transferring the existential risk of such a war from a policy choice to a structural given. The cost is borne by all populations, hypothetically.
% ABSENT_VOICES: Historical military strategists who planned for total victory in conventional terms would find their foundational assumptions foreclosed by this structural reality. Their voices are absent because their strategic paradigm is no longer viable.
% DISAPPEARANCE_RATIONALE: If nuclear weapons somehow ceased to exist or their destructive power was negated, the concept of total war would immediately re-enter the realm of strategic possibility, fundamentally altering international relations, military planning, and the balance of power. The world would rearrange around the renewed possibility of conventional total war.
% FOUNDING_PROBLEM: The problem of preventing catastrophic, civilization-ending conflict among great powers.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, attested by ongoing nuclear deterrence theory, arms control efforts, and the continued existence of nuclear arsenals. The structural constraint is the 'solution' that prevents the problem from escalating to its ultimate conclusion, corroborated by the absence of total war between nuclear powers since 1945.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is near zero because no party collects rents from this constraint; it is a physical limit. Suppression is near 1.0 because the physical reality of nuclear weapons 'suppresses' the option of total war almost completely. Theater ratio is zero because there is no performative aspect to physical impossibility. Accessibility collapse is high (0.98) as the option of winning a total war has collapsed. Resistance is low (0.01) because one cannot 'resist' a physical impossibility, only misinterpret it. The metrics are stable over time, reflecting the unchanging physical reality.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is experienced as an existential threat by non-nuclear states, a strategic dilemma by nuclear states, and a physical constant by this analytical reading. The engine's per-seat classification will reflect these different relationships to the same structural limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states are agenda-setters in the sense that they possess the weapons, but they are also constrained by the structural limit. Non-nuclear states are payers in that they bear the existential risk without direct control. Populations are the ultimate, hypothetical victims. The constraint subsidizes all by removing the total war option, but it extracts from any actor attempting to pursue it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_normative_causation,
    'Is the absence of total war between major powers primarily due to the physical impossibility of winning (structural contraction), or due to the development of strong international norms against it (normative drop)?',
    'Counterfactual analysis: if nuclear weapons were ''uninvented'' but norms remained, would total war re-emerge? If norms collapsed but nuclear weapons remained, would total war re-emerge? The reading with the stronger causal link is preferred.',
    'If normative causation is primary, this constraint would be reclassified from Mountain to Rope or Tangled Rope, reflecting a human-constructed and enforced constraint rather than a physical limit. If structural causation is primary, the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_normative_causation, conceptual, 'Distinguishing physical impossibility from normative prohibition as the primary cause of total war''s absence.').

omega_variable(
    structural_vs_ideational_causation,
    'Is the absence of total war between major powers primarily due to the physical impossibility of winning (structural contraction), or due to an ideational shift in strategic culture (strategic culture drift)?',
    'Historical analysis of strategic planning documents and military doctrines: if the shift in planning predates or is independent of the physical reality, ideational causation is stronger. If planning consistently reflects the physical limits, structural causation is stronger.',
    'If ideational causation is primary, this constraint would be reclassified from Mountain to a more socially constructed type, reflecting a constraint that could shift with changes in elite thought. If structural causation is primary, the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_ideational_causation, empirical, 'Distinguishing physical impossibility from ideational shifts in strategic culture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(tota_tr_t1980, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(tota_tr_t2000, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1960, 0.01).
narrative_ontology:measurement(tota_be_t1980, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1980, 0.01).
narrative_ontology:measurement(tota_be_t2000, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2000, 0.01).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2024, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1945, 0.99).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1960, 0.99).
narrative_ontology:measurement(tota_su_t1980, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1980, 0.99).
narrative_ontology:measurement(tota_su_t2000, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2000, 0.99).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2024, 0.99).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, nuclear_proliferation_treaty).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, arms_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_winnability_post1945' kernel. This 'structural contraction' reading posits a physical impossibility, distinct from normative or ideational explanations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
