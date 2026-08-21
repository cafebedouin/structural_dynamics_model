% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Possibility Space Contraction (Space Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'space contraction' reading of the impact
 *   of nuclear weapons on international relations: that total war,
 *   particularly between great powers, has been removed from the realm of
 *   strategic possibility, not merely made too costly. This reading posits a
 *   fundamental shift in the strategic landscape, leading to the atrophy of
 *   total-war planning apparatuses and a reorientation of strategic thought
 *   towards sub-nuclear conflict. It is presented as a Mountain due to its
 *   perceived categorical and unchangeable nature within this specific
 *   reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.05).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Possibility Space Contraction (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '1cc099cb-b2eb-4d60-8b15-45da54caac7b').
narrative_ontology:cs_kernel_codification('1cc099cb-b2eb-4d60-8b15-45da54caac7b', implicit).
narrative_ontology:cs_authority_grounding('1cc099cb-b2eb-4d60-8b15-45da54caac7b', self_enforcing).
narrative_ontology:cs_reading_relation('1cc099cb-b2eb-4d60-8b15-45da54caac7b', total_war_possibility_space__deterrence_equilibrium_reading, influences).
narrative_ontology:cs_reading_relation('1cc099cb-b2eb-4d60-8b15-45da54caac7b', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('1cc099cb-b2eb-4d60-8b15-45da54caac7b', foundational, total_war_is_strategically_impossible).
narrative_ontology:cs_axiom_status(total_war_is_strategically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('1cc099cb-b2eb-4d60-8b15-45da54caac7b', total_war_is_strategically_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('1cc099cb-b2eb-4d60-8b15-45da54caac7b', post_hiroshima_strategic_reality).
narrative_ontology:cs_drift_state('1cc099cb-b2eb-4d60-8b15-45da54caac7b', contemporary_strategic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1cc099cb-b2eb-4d60-8b15-45da54caac7b', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, strategic_theorists).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, global_population).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, great_power_militaries).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_revolution_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, long_peace_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically tasked with planning and executing total war, these institutions find their core mission fundamentally altered or rendered obsolete by the impossibility of such conflict. They bear the cost of strategic irrelevance or forced adaptation to sub-nuclear domains, but are identity-locked to the concept of military power.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_militaries, payer,
    institutional, generational, identity_locked, global).

% Benefit from a clearer, albeit more constrained, field of study. Their work shifts from grand strategy for total war to limited war, deterrence, and arms control. They gain intellectual clarity by removing an unthinkably destructive option from the strategic calculus.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_theorists, beneficiary,
    analytical, biographical, analytical, global).

% Benefits from the removal of existential threat, though this benefit is passive and unchosen. They are trapped in the new possibility space, unable to opt out of the consequences of nuclear weapons, but spared the direct experience of total war.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, global_population, beneficiary,
    powerless, generational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the strategic behavior of great powers by fundamentally altering the perceived possibility space of conflict, implicitly guiding all actors away from total war by making it strategically unthinkable.
% TRANSFER_FUNCTION: Transfers the concept of total war from the realm of strategic possibility to the realm of historical artifact, effectively 'costing' great power militaries their traditional grand strategic role and 'benefiting' the global population with reduced existential risk.
% ABSENT_VOICES: Historical military strategists and political leaders who operated under the assumption of total war as a viable, if costly, option. Their strategic frameworks and planning doctrines are now rendered irrelevant, but they are absent from the contemporary discourse that has moved beyond their assumptions.
% DISAPPEARANCE_RATIONALE: If the impossibility of total war vanished overnight, strategic planning, military doctrines, and international relations would fundamentally rearrange. Great powers would immediately re-evaluate their force structures and alliances, potentially leading to a rapid re-escalation of conventional arms races and a return to pre-nuclear strategic thinking.
% FOUNDING_PROBLEM: The problem of how to prevent the recurrence of devastating global conflicts, particularly after the two World Wars, and how to manage the destructive potential of nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: Strategic studies scholars and international relations theorists widely corroborate that preventing total war remains a live problem, even if its strategic possibility has contracted. The continued existence of nuclear arsenals and the ongoing need for deterrence theory attest to this, even from outside the direct beneficiaries of the 'unthinkable' status.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because this reading views the constraint as a natural consequence of nuclear physics, not a human-imposed burden. Suppression is very high (0.95) because the 'unthinkability' of total war is a pervasive and deeply ingrained feature of the strategic environment, effectively suppressing any serious consideration of it. Theater ratio is zero (0.0) as there is no performative aspect to a fundamental shift in possibility space. Accessibility collapse is near total (0.98) as alternatives to avoiding total war are seen as non-existent. Resistance is negligible (0.02) because the constraint is accepted as a fundamental truth by most strategic actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a great power military, the constraint represents a profound challenge to institutional identity and purpose, forcing a re-evaluation of doctrine and training. From the perspective of a strategic theorist, it's a clarifying, if sobering, intellectual framework. The global population experiences it as a background condition of peace, largely unexamined.
 *
 * DIRECTIONALITY LOGIC:
 *   Great power militaries are 'payers' in the sense that their traditional mission is curtailed, but they are identity-locked to the concept of military power, making exit from this new strategic reality impossible. Strategic theorists are 'beneficiaries' as their field gains clarity. The global population is a passive 'beneficiary' of reduced existential risk. No active enforcement is required because the constraint is seen as a self-evident truth of the nuclear age.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_status_of_unthinkability,
    'Is total war truly ''unthinkable'' in strategic planning, or merely ''unpreferable'' and highly deterred?',
    'Analysis of classified military planning documents, war-gaming exercises, and strategic doctrine from great powers. If evidence of active total war planning (even if theoretical) is found, the ''unthinkable'' claim is weakened.',
    'If total war is found to be merely deterred, the constraint shifts from a Mountain (categorical impossibility) to a Tangled Rope (deterrence as coordination with high costs/risks) or Snare (if deterrence is seen as coercive extraction of sovereignty).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_status_of_unthinkability, empirical, 'Distinguishing between strategic impossibility and high-cost deterrence.').

omega_variable(
    institutional_atrophy_vs_adaptation,
    'To what extent has the institutional apparatus for total war planning truly atrophied, versus merely adapted to new forms of deterrence or sub-nuclear conflict?',
    'Longitudinal studies of military budget allocations, personnel training, and strategic command structures. Evidence of continued investment in capabilities relevant only to total war would challenge the atrophy claim.',
    'If institutions are found to have adapted rather than atrophied, the ''space contraction'' reading''s claim of categorical shift is weakened, potentially reclassifying the constraint as a Piton (vestigial planning) or Tangled Rope (active deterrence maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_atrophy_vs_adaptation, empirical, 'Assessing the actual institutional impact of the ''unthinkability'' of total war.').

omega_variable(
    causal_mechanism_ambiguity,
    'Is the absence of total war primarily due to the material reality of nuclear weapons (space contraction), a constructed normative taboo, or a rational deterrence equilibrium?',
    'Comparative historical analysis of periods of nuclear proliferation and de-escalation, and examination of non-nuclear great power conflicts. If normative shifts or rational calculations better explain outcomes, the ''space contraction'' mechanism is less central.',
    'If deterrence or taboo are found to be the primary mechanisms, the constraint''s classification would shift to reflect those dynamics (e.g., Tangled Rope for deterrence, Rope for taboo), rather than a Mountain of strategic impossibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_mechanism_ambiguity, conceptual, 'Ambiguity in the primary causal mechanism preventing total war.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__space_contraction_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__space_contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__space_contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__space_contraction_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1960, 0.07).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2000, 0.04).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1980, 0.95).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2000, 0.96).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. This 'space_contraction_reading' posits total war as strategically unthinkable, influencing (but not foreclosing) the deterrence and taboo readings by setting a background condition of extreme risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
