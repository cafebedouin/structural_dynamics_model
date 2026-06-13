% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear War Unwinnability (Deterrence Unthinkable Reading)
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence unthinkable' reading of
 *   nuclear war winnability post-1945, where great-power total war is
 *   considered categorically unwinnable due to the destructive power of
 *   nuclear weapons. This reading asserts that planning for victory in such a
 *   conflict is inherently incoherent, shifting strategic focus entirely to
 *   war prevention. It is a Mountain because its persistence is a structural
 *   feature of reality, not dependent on enforcement, and it extracts
 *   minimally, primarily from traditional military establishments whose
 *   mission is fundamentally altered.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.15).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.05).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.15).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear War Unwinnability (Deterrence Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '76f61fe6-3dfe-4840-88f9-08fb62bf800d').
narrative_ontology:cs_kernel_codification('76f61fe6-3dfe-4840-88f9-08fb62bf800d', implicit).
narrative_ontology:cs_authority_grounding('76f61fe6-3dfe-4840-88f9-08fb62bf800d', diffuse_epistemic).
narrative_ontology:cs_reading_relation('76f61fe6-3dfe-4840-88f9-08fb62bf800d', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('76f61fe6-3dfe-4840-88f9-08fb62bf800d', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('76f61fe6-3dfe-4840-88f9-08fb62bf800d', foundational, total_war_is_existential_risk).
narrative_ontology:cs_axiom_status(total_war_is_existential_risk, holdable).
narrative_ontology:cs_axiom_grounding('76f61fe6-3dfe-4840-88f9-08fb62bf800d', total_war_is_existential_risk, empirically_contingent).
narrative_ontology:cs_axiom('76f61fe6-3dfe-4840-88f9-08fb62bf800d', foundational, victory_in_nuclear_exchange_is_impossible).
narrative_ontology:cs_axiom_status(victory_in_nuclear_exchange_is_impossible, holdable).
narrative_ontology:cs_axiom_grounding('76f61fe6-3dfe-4840-88f9-08fb62bf800d', victory_in_nuclear_exchange_is_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('76f61fe6-3dfe-4840-88f9-08fb62bf800d', post_hiroshima_existential_threat).
narrative_ontology:cs_drift_state('76f61fe6-3dfe-4840-88f9-08fb62bf800d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('76f61fe6-3dfe-4840-88f9-08fb62bf800d', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, traditional_military_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, defense_contractors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the absence of great-power total war, which is rendered impossible by nuclear weapons. They are the ultimate beneficiaries of deterrence, as their survival is directly tied to its success.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Experience mission incoherence and a fundamental challenge to their traditional role of planning for and achieving victory in great-power conflicts. Their identity is tied to a concept of war that no longer exists.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, traditional_military_establishments, payer,
    institutional, generational, identity_locked, global).

% Are tasked with developing doctrines and policies for nuclear deterrence, which means planning for war prevention rather than warfighting. They shape the discourse around nuclear strategy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nuclear_strategists, agenda_setter,
    organized, biographical, constrained, global).

% Bear the ultimate responsibility for nuclear decision-making, operating under the constraint that total war is unwinnable. Their primary objective becomes avoiding escalation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, political_leaders, agenda_setter,
    institutional, immediate, constrained, national).

% Benefit from the continued investment in nuclear arsenals and related technologies, even if their primary purpose shifts from warfighting to deterrence. They adapt their offerings to the new strategic reality.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, defense_contractors, beneficiary,
    powerful, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding among great powers that total war is no longer a viable instrument of policy, thereby coordinating their strategic behavior towards mutual restraint and crisis management.
% TRANSFER_FUNCTION: Transfers the concept of 'victory' in great-power total war from the realm of achievable military objectives to an incoherent, unreachable state. It transfers strategic focus from warfighting to war prevention.
% ABSENT_VOICES: Historical military strategists who operated before the nuclear age, whose theories of decisive victory are rendered obsolete. Their voices are absent from contemporary strategic planning, which must contend with nuclear realities.
% DISAPPEARANCE_RATIONALE: If the unwinnability of nuclear war vanished overnight, the entire international security architecture would collapse. Great powers would immediately re-evaluate their military doctrines, potentially leading to a return to conventional total war planning and a massive increase in global instability.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons to human civilization, rendering traditional concepts of great-power war obsolete and dangerous.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on the destructive power of nuclear weapons, the historical record of near-misses during the Cold War, and ongoing analyses by independent strategic think tanks and international organizations (e.g., UN, IAEA) corroborate the live status of this problem. This is attested by sources outside the direct beneficiaries (e.g., civilian populations, defense contractors).
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, ExtMetricName, E),
    domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the constraint primarily redefines the possibility space of conflict, rather than actively extracting resources. Suppression is minimal (0.05) as the constraint is a physical reality, not a human-enforced rule. Theater ratio is 0.0, as there is no performative maintenance; the unwinnability is a stark reality. Accessibility collapse is high (0.95) because the alternative (winnable total war) is physically foreclosed. Resistance is low (0.1) because while some military thinkers may resist the implications, the fundamental reality is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civilian populations, this constraint is a Mountain, a natural law ensuring their survival. From the perspective of traditional military establishments, it is a Snare, trapping them in a mission that is fundamentally undermined. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are beneficiaries (d=0.0) as they are spared from total war. Traditional military establishments are victims (d=1.0) as their core mission of achieving victory in great-power conflicts becomes incoherent. Nuclear strategists and political leaders are agenda-setters (d=0.5) who must navigate this new reality. Defense contractors are beneficiaries (d=0.0) as they continue to profit from the maintenance of nuclear arsenals for deterrence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_rhetorical_contraction,
    'Is the unwinnability of nuclear war an operational reality (as this reading claims), or primarily a rhetorical contraction where planning for victory persists covertly?',
    'Analysis of classified strategic planning documents and military exercises: if these consistently show no viable path to victory in total war, it supports operational contraction. If they show continued planning for limited victory, it supports rhetorical contraction.',
    'If primarily rhetorical, the constraint''s effective suppression on military establishments is higher, as they are forced to maintain a public facade of unwinnability while privately planning for it. This would shift the classification towards a Tangled Rope or Snare for military actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_rhetorical_contraction, empirical, 'Distinguishing between actual operational unwinnability and a rhetorical taboo.').

omega_variable(
    natural_law_vs_human_choice,
    'Is the unwinnability of nuclear war a genuine natural law (a Mountain), or a human-constructed constraint that benefits identifiable agents (a False Summit Mountain)?',
    'Examination of the physical and logical limits of nuclear exchange: if the outcomes are truly catastrophic and uncontrollable, it supports natural law. If alternative, less destructive scenarios are plausible, it suggests a constructed constraint.',
    'If it''s a constructed constraint, the ''emerges_naturally'' flag would be false, and the constraint would be reclassified as a Tangled Rope, reflecting the active maintenance of a beneficial narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_human_choice, conceptual, 'Ambiguity between a physical limit and a policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nuclear_proliferation_treaty).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, arms_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'war_winnability_post_1945' kernel. It focuses on the categorical unwinnability of great-power total war, contrasting with readings that emphasize limited victory or rhetorical contraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
