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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Possibility Space Contraction (Nuclear Weapons)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'space contraction' reading of the total
 *   war possibility space, arguing that nuclear weapons fundamentally removed
 *   total war from the realm of strategic thought, making it a categorical
 *   impossibility rather than merely a highly undesirable option. This
 *   reading posits an institutional atrophy of total-war planning apparatuses
 *   and a reorientation of strategic studies towards sub-nuclear domains. It
 *   is claimed as a Mountain due to its perceived unchangeable nature in this
 *   reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.05).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Possibility Space Contraction (Nuclear Weapons)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '05f0ed89-12dc-45fd-bd63-0a1c492a318d').
narrative_ontology:cs_kernel_codification('05f0ed89-12dc-45fd-bd63-0a1c492a318d', implicit).
narrative_ontology:cs_authority_grounding('05f0ed89-12dc-45fd-bd63-0a1c492a318d', diffuse_epistemic).
narrative_ontology:cs_reading_relation('05f0ed89-12dc-45fd-bd63-0a1c492a318d', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('05f0ed89-12dc-45fd-bd63-0a1c492a318d', total_war_possibility_space__nuclear_taboo_reading, forecloses).
narrative_ontology:cs_axiom('05f0ed89-12dc-45fd-bd63-0a1c492a318d', foundational, total_war_is_strategically_impossible).
narrative_ontology:cs_axiom_status(total_war_is_strategically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('05f0ed89-12dc-45fd-bd63-0a1c492a318d', total_war_is_strategically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('05f0ed89-12dc-45fd-bd63-0a1c492a318d', foundational, nuclear_weapons_fundamentally_altered_strategic_reality).
narrative_ontology:cs_axiom_status(nuclear_weapons_fundamentally_altered_strategic_reality, holdable).
narrative_ontology:cs_axiom_grounding('05f0ed89-12dc-45fd-bd63-0a1c492a318d', nuclear_weapons_fundamentally_altered_strategic_reality, empirically_contingent).
narrative_ontology:cs_reference_frame('05f0ed89-12dc-45fd-bd63-0a1c492a318d', pre_nuclear_strategic_thought).
narrative_ontology:cs_drift_state('05f0ed89-12dc-45fd-bd63-0a1c492a318d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('05f0ed89-12dc-45fd-bd63-0a1c492a318d', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, global_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, great_power_militaries).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, defense_industries).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_revolution_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, long_peace_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of total war, which would entail existential risk. Has no agency in the constraint's operation but is the ultimate recipient of its 'benefit'.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, global_population, beneficiary,
    powerless, generational, trapped, global).

% Forced to abandon traditional total war planning and doctrine, leading to atrophy of conventional mobilization capabilities and a shift in strategic focus. Their institutional identity as preparers for all-out conflict is fundamentally altered.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_militaries, payer,
    institutional, generational, identity_locked, global).

% Shifted their intellectual agenda away from total war scenarios, focusing instead on limited conflicts, deterrence, and sub-nuclear strategies. They are the intellectual architects of the new 'thinkable' space.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_theorists, agenda_setter,
    organized, biographical, constrained, global).

% Must adapt their production and R&D to a world without total war, focusing on conventional, limited-conflict, and nuclear deterrence systems rather than mass mobilization for large-scale conventional conflict.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, defense_industries, payer,
    institutional, biographical, constrained, global).

% Represent the historical mindset where total war was a viable, if catastrophic, strategic option. Their perspective is now considered obsolete and strategically irrelevant by the space contraction reading.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, historical_total_war_planners, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the strategic planning and resource allocation of great powers by removing the possibility of total war, thereby channeling competition into sub-nuclear domains and preventing catastrophic escalation.
% TRANSFER_FUNCTION: Transfers the strategic option of total war from the realm of 'thinkable' to 'unthinkable', effectively transferring resources and intellectual effort away from its planning and preparation.
% ABSENT_VOICES: Historical total war planners and those who believe total war remains a latent possibility would object, arguing that the 'unthinkable' is merely 'undesirable' and could re-emerge. Their voices are excluded by the prevailing strategic consensus that total war is no longer a viable option.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, total war would re-enter the strategic possibility space, leading to a radical reorganization of military doctrines, defense spending, and international relations. The world would fundamentally re-orient around the renewed threat.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons, which made any great power conflict potentially escalatory to global annihilation.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live as long as nuclear weapons exist and the potential for their use, even if unthinkable for total war, remains. Strategic analysts and international relations scholars outside of military establishments corroborate this, emphasizing the ongoing need for nuclear non-proliferation and arms control.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

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
 *   Extractiveness is very low (0.05) because the constraint primarily removes an option rather than imposing costs, and the 'benefit' (avoidance of total war) is diffuse. Suppression is very high (0.95) because the strategic reality of nuclear weapons is seen as an overwhelming, unchallengeable force that 'suppresses' the very idea of total war. Theater ratio is low (0.05) as there's little performative maintenance; the constraint is seen as a fundamental shift in reality. Accessibility collapse is near total (0.98) as total war is deemed strategically inaccessible. Resistance is minimal (0.02) because the strategic community largely accepts this fundamental shift.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the global population, this is a pure benefit. From the perspective of military institutions, it's a fundamental redefinition of their purpose, a 'cost' in terms of lost strategic options and institutional identity. The constraint is experienced as a Mountain by all, but with different implications for their roles and futures.
 *
 * DIRECTIONALITY LOGIC:
 *   The global population is a diffuse beneficiary (d=0.0) as they are spared total war. Great power militaries and defense industries are payers (d=1.0) as they bear the cost of abandoning traditional doctrines and reorienting their entire institutional purpose. Strategic theorists are agenda-setters (d=0.5) as they define the new strategic landscape. Historical total war planners are excluded (d=1.0) as their perspective is rendered obsolete.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_deterrence,
    'Is total war truly ''unthinkable'' (space contraction) or merely ''undesirable'' and ''deterred'' (deterrence equilibrium)?',
    'Analysis of declassified military planning documents and strategic exercises: if total war scenarios are entirely absent from serious planning, it supports space contraction; if they persist as low-probability, high-cost options, it supports deterrence equilibrium.',
    'If total war is merely deterred, the constraint is a Tangled Rope (deterrence requires active maintenance and imposes costs); if unthinkable, it is a Mountain (a fundamental shift in strategic reality).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_vs_deterrence, empirical, 'Distinguishing between a categorical impossibility and a highly costly, deterred option.').

omega_variable(
    material_vs_normative_causation,
    'Did nuclear weapons directly cause the contraction of the possibility space (material causation), or did they enable the construction of a normative taboo against total war (normative causation)?',
    'Historical analysis of diplomatic discourse and international law development: if the shift in strategic thought preceded or was independent of formal normative prohibitions, it supports material causation; if the taboo was actively constructed and enforced, it supports normative causation.',
    'If material causation, the constraint is a Mountain (a physical reality); if normative causation, it is a Rope or Tangled Rope (a constructed social constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_vs_normative_causation, conceptual, 'Distinguishing between a material constraint and a normatively constructed one.').


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
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1960, 0.03).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1980, 0.04).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1960, 0.95).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1980, 0.96).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. This 'space contraction' reading posits a fundamental, near-Mountain-like shift in strategic reality, distinct from deterrence or normative taboo.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
