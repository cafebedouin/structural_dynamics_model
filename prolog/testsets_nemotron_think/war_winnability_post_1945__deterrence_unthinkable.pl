% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Unwinnability of Great-Power Total War
 *   domain: strategic/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'deterrence_unthinkable' reading
 *   of the contested kernel 'war_winnability_post_1945'. The reading asserts
 *   that nuclear weapons created a structural mountain: great-power total war
 *   became categorically unwinnable by physics and logic, not by policy
 *   choice. Planning for victory is incoherent because any nuclear exchange
 *   destroys the political objects war seeks to control. The constraint's
 *   beneficiaries are civilian populations (who avoid great-power war) and
 *   non-nuclear states (who gain existential security from the taboo). The
 *   victims are military establishments whose warfighting mission becomes
 *   incoherent — they must plan for wars they cannot fight and cannot win.
 *   The claim/metric gap is deliberate: the reading CLAIMS mountain (natural
 *   law) while the metrics show low but non-zero extractiveness (0.22) and
 *   rising theater (0.15), reflecting the engine's detection of institutional
 *   adaptation at the margins.
 *
 * KEY AGENTS:
 *   - civilian_populations: Primary beneficiary (powerless/trapped) — avoids nuclear war but bears existential risk
 *   - military_establishments: Primary victim (institutional/identity_locked) — mission incoherence, budgetary displacement
 *   - nuclear_armed_states: Agenda setter (institutional/constrained) — maintains deterrence posture, controls escalation
 *   - deterrence_theorists: Observer (analytical/analytical) — elaborates and legitimizes the unwinnability logic
 *   - disarmament_advocates: Excluded (organized/constrained) — argues for abolition, not managed deterrence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.22).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.88).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.22).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Unwinnability of Great-Power Total War").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic/nuclear_deterrence/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '061856ca-b11b-438c-a5d9-23d4ad9d0f8e').
narrative_ontology:cs_kernel_codification('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', formalized).
narrative_ontology:cs_authority_grounding('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', practice).
narrative_ontology:cs_interpretation_layer_present('061856ca-b11b-438c-a5d9-23d4ad9d0f8e').
narrative_ontology:cs_reading_relation('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', foundational, nuclear_war_categorically_unwinnable).
narrative_ontology:cs_axiom_status(nuclear_war_categorically_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', nuclear_war_categorically_unwinnable, empirically_contingent).
narrative_ontology:cs_axiom('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', secondary, war_prevention_only_legitimate_strategy).
narrative_ontology:cs_axiom_status(war_prevention_only_legitimate_strategy, holdable).
narrative_ontology:cs_axiom_grounding('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', war_prevention_only_legitimate_strategy, conventional).
narrative_ontology:cs_reference_frame('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', post_1945_nuclear_order).
narrative_ontology:cs_drift_state('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', contemporary_multipolar_nuclear_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('061856ca-b11b-438c-a5d9-23d4ad9d0f8e', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, non_nuclear_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, warfighting_doctrine_bureaucracies).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, mutual_assured_destruction_logic).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, crisis_stability_imperative).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, nuclear_taboo_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Avoid great-power total war through the unwinnability constraint. Bear existential risk of deterrence failure without consent or exit option. Cannot opt out of nuclear threat; no migration or political action reliably removes them from targeting. Benefit is survival; cost is involuntary risk-bearing.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, civilizational, trapped, global).

% Gain existential security from the nuclear taboo and extended deterrence guarantees without bearing posture costs. Constrained exit: cannot independently verify or enforce the unwinnability claim; depend on nuclear-armed states' restraint. Some pursue hedging (latent capability) as partial exit.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, non_nuclear_states, beneficiary,
    moderate, generational, constrained, global).

% Must maintain nuclear postures, targeting plans, and escalation ladders for wars they cannot fight and cannot win. Institutional identity is fused to warfighting mission; admitting unwinnability hollows out professional self-concept. Budgetary resources diverted to nuclear missions that have no operational use. Exit requires abandoning institutional identity — effectively impossible from inside.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, identity_locked, global).

% Produce and maintain warfighting doctrine (counterforce, limited options, escalation control) that the mountain claim renders incoherent. Career advancement depends on elaborating plans for unwinnable wars. Professional identity requires belief in operational relevance. Exit means leaving the profession or transferring to conventional roles — identity lock prevents both.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, warfighting_doctrine_bureaucracies, payer,
    organized, biographical, identity_locked, global).

% Set and enforce deterrence posture; control escalation thresholds; decide force structure and declaratory policy. Benefit: no great-power war since 1945. Pay: massive posture costs, political risk of accidents, constraint on conventional freedom of action. Exit constrained by mutual vulnerability — unilateral disarmament invites coercion.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nuclear_armed_states, agenda_setter,
    institutional, generational, constrained, global).

% Elaborate the logical/mathematical structure of mutual assured destruction, crisis stability, and escalation dynamics. Provide intellectual legitimization for the unwinnability claim. Neither collect nor pay; their role is to make the mountain legible. Exit is analytical — they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, deterrence_theorists, observer,
    analytical, generational, analytical, global).

% Argue that the unwinnability constraint proves abolition is necessary, not that deterrence is sufficient. Structurally excluded from nuclear decision-making; their voice would expand the constraint from 'war prevention' to 'weapon elimination'. Exit constrained by nuclear-armed states' monopoly on legitimate violence and discourse.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, disarmament_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power total war by making it physically and logically unwinnable; coordinates mutual restraint without central enforcement — the physics of nuclear exchange enforces itself.
% TRANSFER_FUNCTION: Transfers strategic initiative from offensive warfighting to deterrence posture; moves resources from victory-seeking capabilities to survivable second-strike forces; transfers existential risk from civilians (who would die in war) to military establishments (who bear posture costs and mission incoherence).
% ABSENT_VOICES: Disarmament advocates who argue the unwinnability constraint proves abolition is necessary, not that managed deterrence is sufficient. Future generations who bear the accumulated risk of deterrence failure without having consented to the system. Their absence is structural — nuclear-armed states monopolize the discourse and decision-making.
% DISAPPEARANCE_RATIONALE: If the unwinnability constraint vanished overnight, great-power war planning would resume immediately. Nuclear-armed states would develop and exercise victory-oriented doctrines (counterforce, damage limitation, escalation dominance). Arms races would accelerate from posture maintenance to warfighting capability. Crisis stability would degrade as first-strike incentives returned. The 77-year great-power peace would face its most severe test.
% FOUNDING_PROBLEM: How to prevent great-power total war in the nuclear age, given that any such war would destroy the political objects it seeks to control and likely civilization itself.
% FOUNDING_PROBLEM_CORROBORATION: Attested by deterrence theorists (Schelling, Jervis, Powell), arms control practitioners (Nunn, Perry, Shultz, Kissinger), and the historical record of zero great-power wars since 1945 — not only by military establishments who benefit from nuclear budgets. The founding problem remains live per all non-beneficiary sources.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is low (0.22) because the constraint's primary operation is war prevention, not resource transfer. The extraction that exists flows from military establishments (mission incoherence, opportunity cost of nuclear posture) to civilian populations (survival). Suppression is very high (0.88) because alternatives (victory planning) are structurally closed by physics — not by enforcement. Theater ratio rises slowly (0.02→0.15) as nuclear establishments elaborate limited-use doctrines (counterforce, escalation ladders) that performatively maintain warfighting relevance while the mountain claim holds. Accessibility collapse is near-total (0.92) — no credible great-power victory plan exists. Resistance is low (0.18) because the constraint is accepted as structural reality by all major powers; resistance appears only in doctrinal adaptation at the margins.
 *
 * PERSPECTIVAL GAP:
 *   From the civilian/analytical seat, this is a genuine mountain — physics made total war unwinnable, and the coordination function (mutual restraint) is real. From the military establishment seat, the constraint extracts through mission incoherence: they must maintain expensive postures for wars they cannot fight, and their institutional identity (warfighting) is hollowed out. The engine computes this divergence from the structural data (power/institutional vs powerless/trapped; identity_locked vs trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are structural beneficiaries (d→0): the constraint subsidizes their survival. Military establishments are structural targets (d→1): they bear the cost of maintaining postures for unwinnable wars. Nuclear-armed states sit near symmetric (d~0.5): they both benefit (no great-power war) and pay (posture costs). Deterrence theorists are analytical (d=0.5 by definition). Disarmament advocates are excluded — their exit is constrained by nuclear-armed states' monopoly on the discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power total war) remains LIVE — nuclear weapons still exist, deterrence still operates. The constraint has not atrophied; if anything, its relevance has grown with proliferation. The mountain claim prevents mislabeling coordination as extraction: the unwinnability is not a story told to extract from militaries; it is a structural fact that militaries must adapt to. The extraction (0.22) is the cost of adaptation, not the purpose of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the categorical unwinnability of nuclear war a genuine mountain of physics/politics, or a constructed deterrence doctrine that benefits identifiable agents (civilian populations, non-nuclear states) while extracting from military establishments?',
    'Historical counterfactual analysis: if nuclear weapons had never been used after 1945, would the unwinnability claim still hold as structural necessity? Compare with countervailing_thinkable reading''s empirical claims about limited nuclear options.',
    'If mountain: the constraint is physics/logic, not policy; no party collects rents. If false summit (tangled_rope): civilian populations and non-nuclear states benefit from the taboo while military establishments pay through mission incoherence and budgetary displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether deterrence_unthinkable is a natural law or a constructed constraint with beneficiaries').

omega_variable(
    military_mission_adaptation,
    'Have military establishments genuinely suffered ''mission incoherence'' (victim extraction), or have they successfully adapted through counterforce targeting, limited nuclear options, and conventional-nuclear integration to restore winnability claims?',
    'Doctrinal analysis of current nuclear postures (US, Russia, China, others): do operational plans treat total war as unwinnable while preserving limited victory pathways? Budgetary analysis: has nuclear mission funding grown or shrunk relative to conventional?',
    'If adaptation succeeded, the victim claim weakens and the constraint may be piton (degraded mountain maintained theatrically). If mission incoherence persists, the extraction from military establishments is real and the false summit signature strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_mission_adaptation, empirical, 'Whether military establishments are genuine victims or have adapted the constraint').

omega_variable(
    civilian_beneficiary_or_hostage,
    'Are civilian populations genuine beneficiaries of the unwinnability constraint (no war = benefit), or are they hostages to a deterrence system that could fail catastrophically?',
    'Risk analysis: expected value of deterrence failure (nuclear war) vs. expected value of great-power conventional war in counterfactual non-nuclear world. Survey data on civilian threat perception.',
    'If hostages: the beneficiary declaration is false; the constraint extracts risk from civilians without consent. If genuine beneficiaries: the mountain claim''s coordination function (war prevention) is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_beneficiary_or_hostage, preference, 'Whether civilian populations benefit or bear involuntary risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.02).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_tr_t1962, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1962, 0.05).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_tr_t1972, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1972, 0.08).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_tr_t1983, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1983, 0.1).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_tr_t1991, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1991, 0.12).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_tr_t2001, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2001, 0.13).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_tr_t2010, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_tr_t2022, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2022, 0.15).

% Extraction over time
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_be_t1962, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1962, 0.08).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_be_t1972, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1972, 0.12).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_be_t1983, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1983, 0.15).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_be_t1991, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1991, 0.18).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_be_t2001, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2001, 0.2).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_be_t2010, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2010, 0.21).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_be_t2022, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2022, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_su_t1962, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1962, 0.85).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_su_t1972, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1972, 0.88).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_su_t1983, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1983, 0.9).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_su_t1991, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1991, 0.88).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_su_t2001, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2001, 0.87).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_su_t2010, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(war_winnability_post_1945__deterrence_unthinkable_su_t2022, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2022, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, global_infrastructure).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__deterrence_unthinkable, 0.18).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nuclear_deterrence_posture_maintenance).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, arms_control_verification_regime).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, crisis_stability_mechanisms).

% DUAL FORMULATION NOTE:
% This reading (deterrence_unthinkable) and countervailing_thinkable are mutually foreclosing within a single strategic framework: you cannot hold that total war is categorically unwinnable AND that limited victory is achievable. This reading and rhetorical_contraction coexist — one describes the structural reality, the other the discursive/operational gap. Both influence nuclear_deterrence_posture_maintenance (downstream constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, institutional, 0.45).
constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, powerless, 0.05).
constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
