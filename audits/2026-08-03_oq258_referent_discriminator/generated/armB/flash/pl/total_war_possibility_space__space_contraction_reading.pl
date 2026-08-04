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
 *   human_readable: Total War Exits Strategic Possibility Space (Space Contraction Reading)
 *   domain: international_relations_theory/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint represents the 'space contraction' reading of the impact
 *   of nuclear weapons on international relations: that they fundamentally
 *   altered the strategic possibility space, making total war between great
 *   powers literally unthinkable, not merely too costly or normatively
 *   forbidden. This reading posits a structural, almost 'natural law' shift
 *   in strategic reality, leading to the atrophy of military doctrines and
 *   planning for such conflicts. It is one reading of the
 *   'total_war_possibility_space' kernel.
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
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Exits Strategic Possibility Space (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations_theory/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, 'fbf7aa47-9b70-44ca-9466-072c51b34b66').
narrative_ontology:cs_kernel_codification('fbf7aa47-9b70-44ca-9466-072c51b34b66', implicit).
narrative_ontology:cs_authority_grounding('fbf7aa47-9b70-44ca-9466-072c51b34b66', diffuse_epistemic).
narrative_ontology:cs_reading_relation('fbf7aa47-9b70-44ca-9466-072c51b34b66', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('fbf7aa47-9b70-44ca-9466-072c51b34b66', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('fbf7aa47-9b70-44ca-9466-072c51b34b66', foundational, total_war_is_strategically_impossible).
narrative_ontology:cs_axiom_status(total_war_is_strategically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('fbf7aa47-9b70-44ca-9466-072c51b34b66', total_war_is_strategically_impossible, deontological).
narrative_ontology:cs_axiom('fbf7aa47-9b70-44ca-9466-072c51b34b66', foundational, nuclear_weapons_fundamentally_altered_strategic_reality).
narrative_ontology:cs_axiom_status(nuclear_weapons_fundamentally_altered_strategic_reality, holdable).
narrative_ontology:cs_axiom_grounding('fbf7aa47-9b70-44ca-9466-072c51b34b66', nuclear_weapons_fundamentally_altered_strategic_reality, empirically_contingent).
narrative_ontology:cs_reference_frame('fbf7aa47-9b70-44ca-9466-072c51b34b66', pre_nuclear_strategic_paradigm).
narrative_ontology:cs_drift_state('fbf7aa47-9b70-44ca-9466-072c51b34b66', contemporary_strategic_thought, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('fbf7aa47-9b70-44ca-9466-072c51b34b66', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, global_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, military_planners).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, strategic_theorists).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_revolution_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, long_peace_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of total war, which would entail existential risk. Has no direct agency over the constraint but is the ultimate recipient of its 'benefit'.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, global_population, beneficiary,
    powerless, generational, trapped, global).

% Their professional identity and institutional mandate are shaped by the impossibility of planning for total war. They bear the 'cost' of having a core function (great-power war planning) rendered obsolete, leading to atrophy of relevant doctrines and capabilities.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, military_planners, payer,
    institutional, biographical, identity_locked, national).

% Their field of study shifts away from total war scenarios, focusing instead on limited conflicts, deterrence, and sub-nuclear domains. They must adapt their research agendas and theoretical frameworks, effectively 'paying' with the obsolescence of prior intellectual capital.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_theorists, payer,
    organized, biographical, constrained, global).

% Administer military and strategic policy, implicitly or explicitly acknowledging the constraint. They redirect resources away from total war preparations, reflecting the perceived impossibility. Their 'agenda' is set by the new strategic reality.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global strategic environment by removing the possibility of total war, thereby implicitly coordinating national defense postures and resource allocation away from such scenarios.
% TRANSFER_FUNCTION: Transfers the existential risk of total war from the global population to a state of strategic impossibility, effectively 'transferring' the burden of planning for such a conflict from military institutions to non-existence.
% ABSENT_VOICES: Historical military strategists who operated before the nuclear age, or hypothetical future strategists who might conceive of total war again, are absent. They would argue for the enduring possibility of such conflict, regardless of nuclear weapons, based on historical patterns of human behavior.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, total war would re-enter the realm of strategic possibility. Military doctrines would rapidly re-orient, defense budgets would surge for conventional forces, and the global security architecture would fundamentally shift to account for this renewed threat. The 'long peace' would be immediately jeopardized.
% FOUNDING_PROBLEM: The problem of preventing existential catastrophe from great-power conflict, which became acute with the advent of nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, as the potential for nuclear conflict still exists, even if total war is unthinkable. Strategic analysts and international relations scholars (outside of direct military planning) corroborate that the problem of managing nuclear risk is ongoing, even if the nature of the 'solution' (total war's impossibility) is debated.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.05) because this reading posits a fundamental shift in reality, not an imposed cost. Suppression is very high (0.95) because the 'unthinkability' of total war is a powerful, almost absolute, barrier to its consideration. Theater ratio is low (0.05) as there is little performative maintenance; the constraint operates as a deep structural reality. Accessibility collapse is near total (0.98) as alternatives (planning for total war) are simply removed from the strategic menu. Resistance is negligible (0.02) because the constraint is perceived as an objective reality, not a policy choice to be resisted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the global population, this is a pure mountain, an unchangeable reality that prevents catastrophe. From the perspective of military planners, it's a structural limit that redefines their entire profession, effectively 'extracting' the possibility of their traditional highest-stakes work. The engine's per-seat classification should reflect this divergence, even within a mountain claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The global population is a diffuse beneficiary, avoiding existential risk. Military planners and strategic theorists are 'payers' in the sense that their professional domains are fundamentally reshaped and parts of their expertise rendered obsolete by this new strategic reality. National governments act as agenda-setters, adapting policy to this perceived impossibility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the ''unthinkability'' of total war a genuine structural feature of the nuclear age (a natural law), or a deeply ingrained social/cognitive construct that could, in principle, be unlearned or overcome?',
    'Analysis of historical shifts in strategic thought, or the emergence of new doctrines that explicitly re-introduce total war as a planning option, would challenge the ''natural law'' framing.',
    'If it''s a construct, the constraint''s ''emerges_naturally'' property would be false, and its classification would shift towards a highly effective, deeply internalized Snare or Tangled Rope, maintained by cognitive and institutional inertia rather than inherent impossibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between a natural strategic limit and a powerful social construct.').

omega_variable(
    institutional_atrophy_reversibility,
    'To what extent has the institutional atrophy of total-war planning apparatuses been irreversible, and could these capabilities be rapidly reconstituted if the constraint weakened?',
    'Historical case studies of rapid military adaptation to new threats, or detailed analysis of current ''deep state'' contingency planning for extreme scenarios, could provide evidence.',
    'If atrophy is easily reversible, the ''space contraction'' is less absolute, and the constraint might be closer to a Rope or Tangled Rope, where the ''unthinkability'' is a strong preference rather than an impossibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_atrophy_reversibility, empirical, 'Reversibility of institutional atrophy related to total war planning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__space_contraction_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__space_contraction_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__space_contraction_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__space_contraction_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1960, 0.03).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1960, 0.95).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1980, 0.98).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2000, 0.98).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
