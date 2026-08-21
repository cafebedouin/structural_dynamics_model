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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   human_readable: Total War as Strategically Unthinkable (Space Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'space contraction' reading of the
 *   'total_war_possibility_space' kernel. From this perspective, nuclear
 *   weapons fundamentally altered the strategic landscape, rendering total
 *   war between great powers not merely costly or undesirable, but
 *   strategically unthinkable and impossible. This reading posits a
 *   categorical shift in the nature of warfare, leading to the atrophy of
 *   total-war planning apparatuses and a reorientation of strategic studies
 *   towards sub-nuclear conflict. The constraint is claimed as a Mountain,
 *   reflecting its perceived status as an irreducible structural feature of
 *   the post-nuclear world.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.1).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.8).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War as Strategically Unthinkable (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '4ef5fc37-32b5-41e9-af1e-7574d706b618').
narrative_ontology:cs_kernel_codification('4ef5fc37-32b5-41e9-af1e-7574d706b618', implicit).
narrative_ontology:cs_authority_grounding('4ef5fc37-32b5-41e9-af1e-7574d706b618', expertise).
narrative_ontology:cs_interpretation_layer_present('4ef5fc37-32b5-41e9-af1e-7574d706b618').
narrative_ontology:cs_reading_relation('4ef5fc37-32b5-41e9-af1e-7574d706b618', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('4ef5fc37-32b5-41e9-af1e-7574d706b618', total_war_possibility_space__nuclear_taboo_reading, forecloses).
narrative_ontology:cs_axiom('4ef5fc37-32b5-41e9-af1e-7574d706b618', foundational, total_war_is_strategically_impossible).
narrative_ontology:cs_axiom_status(total_war_is_strategically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('4ef5fc37-32b5-41e9-af1e-7574d706b618', total_war_is_strategically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('4ef5fc37-32b5-41e9-af1e-7574d706b618', secondary, great_power_mobilization_doctrine_is_obsolete).
narrative_ontology:cs_axiom_status(great_power_mobilization_doctrine_is_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('4ef5fc37-32b5-41e9-af1e-7574d706b618', great_power_mobilization_doctrine_is_obsolete, empirically_contingent).
narrative_ontology:cs_reference_frame('4ef5fc37-32b5-41e9-af1e-7574d706b618', pre_nuclear_strategic_calculus).
narrative_ontology:cs_drift_state('4ef5fc37-32b5-41e9-af1e-7574d706b618', post_cold_war_era, gap(stable, severe, true)).
narrative_ontology:cs_created_at('4ef5fc37-32b5-41e9-af1e-7574d706b618', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, global_stability_advocates).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, strategic_planners).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_revolution_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the structural impossibility of total war, which underpins their security doctrines and allows for strategic competition below the nuclear threshold. They are the primary actors whose strategic calculus is shaped by this constraint.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_powers, beneficiary,
    institutional, civilizational, analytical, global).

% Must adapt their planning and doctrine to exclude total war scenarios, shifting focus to limited conflicts, deterrence, and non-proliferation. The 'cost' is the atrophy of traditional total-war planning capabilities and the intellectual effort of re-framing strategy.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_planners, payer,
    institutional, biographical, constrained, national).

% Analyze and articulate the implications of nuclear weapons for the possibility of total war, contributing to the intellectual framework that defines what is strategically 'thinkable'. They are not directly subject to the constraint but interpret its effects.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, international_relations_theorists, observer,
    analytical, generational, analytical, global).

% Benefit from the perceived removal of total war from the strategic landscape, which allows for efforts towards arms control, non-proliferation, and conflict resolution without the constant specter of existential conflict. They champion this outcome as a positive structural shift.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, global_stability_advocates, beneficiary,
    organized, generational, mobile, global).

% Their doctrines and theories, which assumed total war as a viable strategic option, are rendered obsolete by this constraint. They are excluded from contemporary strategic discourse that accepts the unthinkability of total war, often finding their identity tied to a superseded strategic paradigm.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, pre_nuclear_strategists, excluded,
    powerful, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Implicitly coordinates global strategic thought by defining the outer bounds of acceptable conflict, thereby channeling strategic competition into sub-nuclear domains and preventing the planning of suicidal total wars.
% TRANSFER_FUNCTION: Transfers strategic attention, resources, and intellectual effort away from total-war preparations and towards limited conflict scenarios, deterrence maintenance, and non-proliferation efforts.
% ABSENT_VOICES: Pre-nuclear strategists and any hypothetical advocates for total war are structurally excluded from contemporary strategic discourse. Their frameworks are considered obsolete or irrational in the nuclear age.
% DISAPPEARANCE_RATIONALE: If total war became strategically thinkable again, the entire post-WWII global security architecture would collapse. Nuclear deterrence would be re-evaluated, arms races would intensify, and the risk of existential conflict would return to the forefront of international relations, fundamentally reorganizing state behavior and military planning.
% FOUNDING_PROBLEM: The existential threat of nuclear annihilation, which rendered traditional great-power total war a mutually suicidal endeavor, necessitating a fundamental re-evaluation of strategic possibility.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals, the absence of great-power total war since 1945, and the consensus among most strategic studies scholars (outside of specific revisionist schools) corroborate that the problem of nuclear war remains live and shapes strategic thought.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.1) reflects that if total war is truly unthinkable, there's no active 'cost' to avoid it; it's simply not an option. High suppression (0.8) indicates that the very concept of total war has been suppressed from legitimate strategic thought. Low theater ratio (0.1) suggests minimal performative maintenance, as the constraint is seen as a fundamental reality. High accessibility collapse (0.9) means alternatives (i.e., planning for total war) have genuinely vanished. Very low resistance (0.05) indicates little opposition to the idea that total war is unthinkable. The temporal measurements show a stable, low extractiveness and theater, with high suppression, reflecting the enduring nature of this 'unthinkability' since the dawn of the nuclear age.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, there is little perspectival gap regarding the unthinkability of total war; it is a shared structural reality. However, other readings of the same kernel (e.g., deterrence equilibrium, nuclear taboo) offer alternative explanations for the absence of total war, which would lead to different classifications and metric profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers and global stability advocates are beneficiaries, as the constraint underpins a more stable (albeit still competitive) international system. Strategic planners are 'payers' in the sense that they must adapt their entire professional framework to this new reality. International relations theorists act as observers, analyzing and articulating this structural shift. Pre-nuclear strategists are excluded, their ideas rendered obsolete.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unthinkable_vs_deterred,
    'Is total war truly strategically unthinkable (space contraction), or merely deterred by the unacceptable costs of mutual vulnerability (deterrence equilibrium)?',
    'Analysis of declassified strategic planning documents and military exercises: if total war scenarios are entirely absent from serious planning, it supports unthinkability; if they are present but consistently rejected due to cost, it supports deterrence.',
    'If merely deterred, the constraint''s extractiveness would be higher (representing the cost of maintaining deterrence), and its claimed type might shift to a Tangled Rope or Snare, as deterrence requires active enforcement and imposes costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unthinkable_vs_deterred, empirical, 'Distinguishing between strategic impossibility and high-cost deterrence.').

omega_variable(
    strategic_vs_normative_causation,
    'Is the absence of total war primarily due to strategic impossibility (space contraction) or a constructed normative prohibition (nuclear taboo)?',
    'Historical analysis of decision-making during crises: if leaders consistently frame total war as an impossible option, it supports space contraction; if they frame it as a morally unacceptable but technically feasible option, it supports the taboo.',
    'If primarily normative, the constraint''s claimed type might shift to a Rope or Tangled Rope, reflecting a socially constructed and enforced norm rather than a natural law of strategy. This would also imply different beneficiaries and enforcement mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_normative_causation, conceptual, 'Distinguishing between structural strategic change and normative prohibition.').

omega_variable(
    atrophy_genuineness,
    'Is the atrophy of total-war planning apparatuses and mobilization doctrines genuine, or is it merely theatrical, with latent capabilities and plans maintained in reserve?',
    'Detailed institutional ethnography of military general staffs and strategic think tanks, examining resource allocation, training curricula, and internal planning documents over decades.',
    'If the atrophy is theatrical, the constraint''s theater_ratio would be significantly higher, and its suppression might be lower (as the ''unthinkability'' is less deeply ingrained). This could suggest a Piton-like quality, where the performance of unthinkability masks a persistent, albeit dormant, capability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(atrophy_genuineness, empirical, 'Assessing the true extent of total-war planning atrophy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(tota_tr_t1965, total_war_possibility_space__space_contraction_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(tota_tr_t1985, total_war_possibility_space__space_contraction_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1965, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1965, 0.1).
narrative_ontology:measurement(tota_be_t1985, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1985, 0.1).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.1).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(tota_su_t1965, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(tota_su_t1985, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(tota_su_t2005, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_possibility_space' kernel, focusing on the strategic contraction of the possibility space. It is linked to sibling readings that offer alternative explanations for the absence of total war.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
