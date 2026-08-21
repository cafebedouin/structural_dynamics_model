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
 *   of nuclear weapons on total war. It posits that nuclear weapons
 *   fundamentally altered the strategic possibility space, rendering total
 *   war between great powers not merely undesirable or costly, but
 *   categorically unthinkable and impossible. This reading emphasizes the
 *   institutional atrophy of total-war planning apparatuses and a fundamental
 *   shift in strategic thought away from such scenarios. The constraint is
 *   claimed as a Mountain due to its perceived unchangeable nature, but
 *   beneficiaries are declared to trigger FSM evaluation, acknowledging the
 *   contestability of its 'naturalness'.
 *
 * KEY AGENTS:
 *   - great_powers: Primary beneficiary (institutional/identity_locked) — avoids existential destruction
 *   - global_population: Primary beneficiary (powerless/trapped) — avoids species extinction
 *   - strategic_planners: Payer (organized/constrained) — professional obsolescence in total war planning
 *   - military_industrial_complex: Payer (institutional/constrained) — re-allocation of resources away from total war production
 *   - international_relations_theorists: Observer (analytical/analytical) — analyzes the fundamental shift in strategic possibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.15).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Possibility Space Contraction (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '307dd48e-a701-4977-a604-62eb170f8d6f').
narrative_ontology:cs_kernel_codification('307dd48e-a701-4977-a604-62eb170f8d6f', implicit).
narrative_ontology:cs_authority_grounding('307dd48e-a701-4977-a604-62eb170f8d6f', self_enforcing).
narrative_ontology:cs_reading_relation('307dd48e-a701-4977-a604-62eb170f8d6f', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('307dd48e-a701-4977-a604-62eb170f8d6f', total_war_possibility_space__nuclear_taboo_reading, forecloses).
narrative_ontology:cs_axiom('307dd48e-a701-4977-a604-62eb170f8d6f', foundational, total_war_is_strategically_impossible).
narrative_ontology:cs_axiom_status(total_war_is_strategically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('307dd48e-a701-4977-a604-62eb170f8d6f', total_war_is_strategically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('307dd48e-a701-4977-a604-62eb170f8d6f', foundational, nuclear_weapons_fundamentally_alter_strategic_calculus).
narrative_ontology:cs_axiom_status(nuclear_weapons_fundamentally_alter_strategic_calculus, holdable).
narrative_ontology:cs_axiom_grounding('307dd48e-a701-4977-a604-62eb170f8d6f', nuclear_weapons_fundamentally_alter_strategic_calculus, empirically_contingent).
narrative_ontology:cs_reference_frame('307dd48e-a701-4977-a604-62eb170f8d6f', post_nuclear_strategic_reality).
narrative_ontology:cs_drift_state('307dd48e-a701-4977-a604-62eb170f8d6f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('307dd48e-a701-4977-a604-62eb170f8d6f', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, great_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, global_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, strategic_planners).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, military_industrial_complex).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the removal of total war as a viable strategic option, avoiding existential destruction. Their strategic planning apparatus atrophies in this domain, shifting focus to sub-nuclear conflicts. They are identity-locked into this reality by the consequences of any alternative.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_powers, beneficiary,
    institutional, generational, identity_locked, global).

% Benefits from the categorical impossibility of total war, ensuring species survival. They are trapped by the physical reality of nuclear weapons, but also protected by it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, global_population, beneficiary,
    powerless, civilizational, trapped, global).

% Their traditional role of planning for great-power total war becomes obsolete. They must reorient their careers and expertise towards limited warfare or other domains, experiencing a loss of professional identity and relevance in the total war context.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_planners, payer,
    organized, biographical, constrained, national).

% The infrastructure for large-scale conventional mobilization and total war production atrophies, shifting investment to precision, limited-conflict, and nuclear deterrence capabilities. This represents a re-allocation of resources rather than a complete loss, but the total war component is diminished.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, military_industrial_complex, payer,
    institutional, biographical, constrained, national).

% Analyze the implications of nuclear weapons for the possibility of total war. This reading posits a fundamental shift in the strategic landscape, influencing their theoretical frameworks and research agendas.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, international_relations_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, implicit understanding among great powers that total war is no longer a strategically viable option, thus coordinating their strategic planning away from such scenarios.
% TRANSFER_FUNCTION: Transfers the concept of total war from the realm of 'thinkable strategy' to 'categorical impossibility', effectively re-allocating intellectual and material resources away from its planning and preparation.
% ABSENT_VOICES: Historical military strategists and theorists who operated in a pre-nuclear world, for whom total war was a thinkable, albeit costly, option. Their strategic frameworks are now fundamentally incompatible with the nuclear reality.
% DISAPPEARANCE_RATIONALE: If this constraint (the categorical impossibility of total war) vanished, it would imply a fundamental change in the nature of nuclear weapons or their strategic context, which would rearrange the world. However, the constraint itself is a description of a fundamental reality, not an active human construct that could simply disappear. If the *perception* of this impossibility vanished, the world would rearrange into a state of extreme peril.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons, which made traditional great-power total war an unacceptable path to conflict resolution.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals and the absence of great-power total war since 1945 corroborate the problem's live status. Strategic studies literature and historical analysis from various academic and policy institutions outside the direct beneficiaries attest to this shift.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the constraint primarily removes an option rather than imposing direct costs, though it does impose opportunity costs on those whose careers or industries were built around total war. Suppression is very high (0.95) because the physical reality of nuclear weapons fundamentally suppresses the option of total war. Accessibility collapse is near total (0.98) as total war is removed from the realm of viable alternatives. Resistance is negligible (0.02) because the constraint is a fundamental reality, not a policy choice to be resisted. Theater ratio is low (0.05) as there is little performative maintenance; the constraint operates through its inherent physical reality.
 *
 * PERSPECTIVAL GAP:
 *   While the global population and great powers are beneficiaries, strategic planners and the military-industrial complex experience a 'cost' in terms of obsolescence and re-allocation. The constraint is a 'mountain' of strategic reality for all, but its implications are not uniformly beneficial across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Great powers and the global population are full beneficiaries, as the constraint removes an existential threat. Strategic planners and the military-industrial complex are payers, as their traditional functions related to total war become obsolete or are re-directed. International relations theorists are observers, analyzing the structural shift.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests that the 'mandate' for total war planning has atrophied because the 'function' (achieving strategic objectives through total war) has become impossible. The constraint prevents mislabeling this as mere deterrence (where the option still exists but is too costly) or a taboo (where it's normatively prohibited but still physically possible). The atrophy is structural, not merely a policy choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_impossibility,
    'Is the impossibility of total war a ''natural law'' consequence of nuclear physics, or a ''constructed impossibility'' maintained by shared beliefs and institutional practices?',
    'Analysis of historical strategic doctrines and military planning documents: if planning for total war ceased due to perceived physical impossibility rather than cost-benefit analysis or normative prohibition, it supports the ''natural law'' reading. If it persists in any form, it challenges this reading.',
    'If a genuine natural law, the constraint is a true Mountain. If constructed, it might be a Snare (if maintained by extraction) or a Tangled Rope (if it has coordination function but also extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_impossibility, conceptual, 'Ambiguity between physical impossibility and socially constructed impossibility.').

omega_variable(
    space_contraction_vs_deterrence_equilibrium,
    'Does total war truly exit the ''thinkable'' possibility space, or does it remain thinkable but deterred by the unacceptable costs (as per the deterrence equilibrium reading)?',
    'Empirical study of strategic planning documents, military exercises, and public statements from great powers: absence of any serious planning for total war supports space contraction; continued, albeit suppressed, planning supports deterrence equilibrium.',
    'If total war remains thinkable, this constraint''s extractiveness would be higher (as it''s actively suppressed rather than inherently impossible), and its classification would shift towards a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(space_contraction_vs_deterrence_equilibrium, empirical, 'Distinguishing categorical impossibility from high-cost deterrence.').

omega_variable(
    space_contraction_vs_nuclear_taboo,
    'Is the absence of total war due to a fundamental shift in strategic possibility, or a normatively constructed ''taboo'' against nuclear use (as per the nuclear taboo reading)?',
    'Historical analysis of the evolution of nuclear doctrine and international norms: if the shift in strategic thought preceded or was independent of explicit normative prohibitions, it supports space contraction. If normative prohibitions were the primary driver, it supports the taboo reading.',
    'If primarily a taboo, the constraint would be more ''constructed'' and less ''natural'', potentially shifting its classification away from a Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(space_contraction_vs_nuclear_taboo, conceptual, 'Distinguishing structural impossibility from normative prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__space_contraction_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__space_contraction_reading, theater_ratio, 1980, 0.06).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__space_contraction_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__space_contraction_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1960, 0.92).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1980, 0.94).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. This 'space_contraction_reading' posits total war as categorically unthinkable, influencing (and being influenced by) the 'deterrence_equilibrium_reading' (total war is deterred) and the 'nuclear_taboo_reading' (total war is normatively prohibited).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
