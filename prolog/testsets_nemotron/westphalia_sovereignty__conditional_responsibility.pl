% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Sovereignty as Conditional Responsibility to Protect
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The conditional_responsibility reading of Westphalian sovereignty holds
 *   that states forfeit territorial inviolability when they fail to protect
 *   their populations from mass atrocities. This reading emerged from the
 *   post-Cold War humanitarian intervention debates (Somalia, Rwanda, Bosnia,
 *   Kosovo) and was formalized in the 2001 ICISS report and 2005 UN World
 *   Summit 'Responsibility to Protect' (R2P) endorsement. It lowers the
 *   intervention threshold from categorical prohibition to atrocity-triggered
 *   permission, grants the international community adjudicative authority
 *   over sovereignty's scope, and makes populations under atrocity regimes
 *   the primary victims of the prior absolute sovereignty regime. The
 *   constraint is claimed as Tangled Rope: it solves a genuine coordination
 *   problem (mobilizing collective action against atrocities) but
 *   simultaneously extracts from targeted states (surrender of territorial
 *   control) and non-interventionist states (compelled participation in
 *   enforcement), with active enforcement required (UNSC mandates, regional
 *   authorizations, coalition operations). The three readings of the
 *   westphalia_sovereignty kernel are structurally distinct constraints with
 *   different ε, stakeholder sets, and extraction patterns — linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - humanitarian_intervention_coalitions: Primary beneficiaries (institutional/arbitrage) — gain mandate, resources, and strategic positioning from intervention authority
 *   - global_governance_institutions: Primary beneficiaries (institutional/generational) — UN, regional organizations gain expanded mandate and operational relevance
 *   - populations_under_atrocity_regimes: Primary beneficiaries (powerless/biographical) — the intended protection target, but often bear intervention's collateral costs
 *   - targeted_sovereign_states: Primary victims (powerful/identity_locked) — lose territorial inviolability when deemed failing R2P; regime identity fused with absolute sovereignty claim
 *   - non_interventionist_states: Secondary victims (organized/constrained) — compelled to participate in or legitimize interventions that violate their sovereignty doctrine; diplomatic exit constrained by great power politics
 *   - great_power_patrons: Agenda setters (institutional/arbitrage) — veto-wielding UNSC members who authorize or block interventions based on strategic calculus
 *   - analytical_observers: Observers (analytical/analytical) — international lawyers, IR scholars, NGOs documenting the constraint's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Sovereignty as Conditional Responsibility to Protect").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, 'f2546db6-ecff-4ac4-b900-bb3a73a626ed').
narrative_ontology:cs_kernel_codification('f2546db6-ecff-4ac4-b900-bb3a73a626ed', formalized).
narrative_ontology:cs_authority_grounding('f2546db6-ecff-4ac4-b900-bb3a73a626ed', lineage).
narrative_ontology:cs_interpretation_layer_present('f2546db6-ecff-4ac4-b900-bb3a73a626ed').
narrative_ontology:cs_reading_relation('f2546db6-ecff-4ac4-b900-bb3a73a626ed', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('f2546db6-ecff-4ac4-b900-bb3a73a626ed', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('f2546db6-ecff-4ac4-b900-bb3a73a626ed', foundational, state_sovereignty_forfeited_by_atrocity_failure).
narrative_ontology:cs_axiom_status(state_sovereignty_forfeited_by_atrocity_failure, holdable).
narrative_ontology:cs_axiom_grounding('f2546db6-ecff-4ac4-b900-bb3a73a626ed', state_sovereignty_forfeited_by_atrocity_failure, conventional).
narrative_ontology:cs_axiom('f2546db6-ecff-4ac4-b900-bb3a73a626ed', foundational, international_community_adjudicates_sovereignty_scope).
narrative_ontology:cs_axiom_status(international_community_adjudicates_sovereignty_scope, holdable).
narrative_ontology:cs_axiom_grounding('f2546db6-ecff-4ac4-b900-bb3a73a626ed', international_community_adjudicates_sovereignty_scope, conventional).
narrative_ontology:cs_reference_frame('f2546db6-ecff-4ac4-b900-bb3a73a626ed', westphalian_absolute_sovereignty_1945).
narrative_ontology:cs_drift_state('f2546db6-ecff-4ac4-b900-bb3a73a626ed', post_r2p_world_summit_2005, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2546db6-ecff-4ac4-b900-bb3a73a626ed', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, targeted_sovereign_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, non_interventionist_states).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_legitimacy).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, mass_atrocity_as_threshold_trigger).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Form ad hoc or standing coalitions (NATO, ECOWAS, AU, 'coalitions of the willing') authorized to intervene militarily in atrocity situations. Gain UNSC mandates, institutional legitimacy, operational experience, resource flows, and strategic positioning in target regions. Can select which atrocities to respond to based on strategic interest — arbitrage-grade exit from specific interventions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    institutional, biographical, arbitrage, global).

% UN Security Council, General Assembly, Human Rights Council, ICC, regional organizations (AU, EU, OAS). Gain expanded mandate, operational budgets, staffing, and normative authority from R2P framework. Adjudicate intervention thresholds through UNSC resolutions and fact-finding missions. Can prioritize cases and shape doctrine — arbitrage-grade exit from specific enforcement actions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, agenda_setter).

% Civilian populations targeted by mass killing, ethnic cleansing, or crimes against humanity. Are the nominal protection beneficiaries of the constraint. In practice, often bear collateral harm from intervention (bombing, displacement, post-intervention instability) and have no exit from the atrocity situation itself — trapped by the regime targeting them and the intervention responding to it.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, beneficiary,
    powerless, biographical, trapped, local).

% States deemed to have failed their responsibility to protect (e.g., Yugoslavia 1999, Libya 2011, Syria 2011-). Lose territorial inviolability, face military intervention, sanctions, ICC referral, and regime change. Regime identity is fused with absolute sovereignty claim — conceding R2P legitimacy threatens regime survival. No exit preserves both regime and territory; identity_locked by the sovereignty-at-atrocity-threshold fusion.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, targeted_sovereign_states, payer,
    powerful, biographical, identity_locked, national).

% States (e.g., China, Russia, India, Brazil, NAM members) that reject conditional sovereignty and uphold absolute non-intervention. Compelled to participate in or legitimize UNSC mandates they oppose, face diplomatic pressure to contribute to coalitions, and see their sovereignty doctrine eroded by precedent. Can sometimes abstain or veto but face great power costs — constrained exit within the UN system.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, non_interventionist_states, payer,
    organized, biographical, constrained, national).

% P5 UNSC members (US, China, Russia, UK, France) who authorize or block interventions via veto. Use R2P selectively: intervene where strategic interests align (Libya 2011), block where they don't (Syria 2011-). Control the adjudicative machinery and extract strategic value from both intervention and non-intervention. Arbitrage-grade exit — they choose which cases activate the constraint.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, great_power_patrons, agenda_setter,
    institutional, generational, arbitrage, global).

% International lawyers, IR scholars, human rights NGOs, UN special rapporteurs who document, critique, and theorize the constraint's operation. Neither collect nor pay; provide the epistemic infrastructure that legitimizes or contests the threshold. Analytical exit — they observe from outside the enforcement chain.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes collective military and diplomatic action to halt or prevent mass atrocities when the territorial state is the perpetrator or unable to protect — solves the collective action problem of 'who acts when the state fails?' by vesting authority in the international community.
% TRANSFER_FUNCTION: Transfers territorial authority, decision-making control, and physical security from the targeted sovereign state to intervention coalitions and global governance institutions; transfers protection (nominal) to populations under atrocity regimes; transfers diplomatic and material costs to non-interventionist states compelled to participate.
% ABSENT_VOICES: Future generations who inherit the precedent of conditional sovereignty; populations in non-intervened atrocity cases (selective application); states that would intervene but lack great power patronage; the UN Charter's Article 2(4) prohibitions as originally understood — excluded from the room where the R2P doctrine was negotiated and codified.
% DISAPPEARANCE_RATIONALE: If the conditional_responsibility constraint vanished overnight, the international community would lose its legal-moral basis for humanitarian intervention. Atrocity regimes would regain categorical territorial inviolability. Intervention coalitions would lose mandates and face prosecution for past interventions. The UNSC would revert to Chapter VII-only authorization (threats to international peace). The world would rearrange toward the absolute_non_intervention reading — but the graded_sovereignty reading would compete for the adjudicative vacuum.
% FOUNDING_PROBLEM: Mass atrocities (Rwanda 1994, Srebrenica 1995, Kosovo 1999) escaped effective international response because the absolute sovereignty norm (Article 2(4) UN Charter) categorically prohibited intervention in domestic affairs — even when the state was the perpetrator. The international community had no recognized authority to act.
% FOUNDING_PROBLEM_CORROBORATION: The ICISS commission (2001), UN Secretary-General's High-level Panel (2004), and 2005 World Summit Outcome Document attest the founding problem. However, China, Russia, NAM, and many Global South states corroborate that the problem persists but argue the R2P solution has become a cover for regime change — they attest the founding problem is live but the solution has mutated into extraction. No single corroboration exists outside the beneficiary set that endorses the current constraint as the solution.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the significant transfer of territorial authority from targeted states to intervention coalitions and global institutions, calibrated to atrocity severity but applied selectively. Suppression (0.72) is high because the constraint's persistence depends on actively overriding the prior absolute sovereignty norm — UNSC mandates, NATO operations, and regional authorizations are enforcement machinery. Theater ratio (0.41) is moderate: the atrocity prevention function is real (coordination), but a growing share of intervention activity serves intervener strategic interests and institutional self-expansion. The measurement series shows steady extraction accumulation from 1990 (emergent norm) to 2024 (institutionalized but contested practice), with theater rising as the coordination function atrophies relative to extraction. All metrics share the same 7-point time grid aligned to the 1990-2024 interval.
 *
 * PERSPECTIVAL GAP:
 *   From the humanitarian intervention coalition seat, the constraint appears as genuine coordination (Rope-like): it solves the collective action problem of mobilizing against atrocities. From the targeted sovereign state seat, it appears as enforced extraction (Snare-like): territorial integrity is forfeited on criteria they reject, adjudicated by bodies they don't control. From the non-interventionist state seat, it appears as compelled participation (Tangled Rope): they are drafted into enforcement of a norm they oppose. The engine computes this per-seat divergence from the structural data — the claimed_type (Tangled Rope) is the author's structural assessment, not a seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanitarian intervention coalitions and global governance institutions are structural beneficiaries (d near 0.0) — they collect mandate, resources, and authority from the constraint. Populations under atrocity regimes are intended beneficiaries but often experience net harm from intervention's collateral effects (d ~0.4-0.5). Targeted sovereign states are full targets (d near 1.0) — identity_locked because regime survival is fused with absolute sovereignty claims; they have no exit that preserves both regime and territory. Non-interventionist states are constrained payers (d ~0.7) — they can sometimes abstain diplomatically but face great power pressure to legitimize interventions. Great power patrons are agenda setters with arbitrage exit (d ~0.15) — they control the authorization machinery and can opt out of specific interventions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass atrocities escaping international response due to absolute sovereignty) remains live — atrocities persist and the international response remains inconsistent. However, the constraint's mandate has expanded beyond its founding function: intervener coalitions and global institutions now derive structural benefits (mandate, resources, strategic positioning) that exceed the coordination function. The theater ratio rise (0.15→0.41) tracks this mandatrophy — the performance of atrocity prevention increasingly covers extraction. The constraint is not resolved mandatrophy (mandatrophy_resolved: false) because the extraction layer is now structural, not incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the conditional_responsibility reading a distinct constraint from absolute_non_intervention and graded_sovereignty, or merely a different observable of the same ''sovereignty'' claim?',
    'Test ε-invariance: if measuring intervention threshold, adjudicative authority, and victim set yields structurally different ε and stakeholder configurations, the readings are distinct constraints. The three declared readings instantiate different extraction patterns and should be separate stories linked by network.affects_constraints.',
    'If ε-invariant across readings, the kernel is a single constraint with observational variance. If ε-variant, the three readings are a constraint family requiring decomposition — which this story assumes and the engine will verify via the network links.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel''s three readings decompose into three ε-invariant constraints or collapse into one').

omega_variable(
    intervention_threshold_calibration,
    'What level of mass atrocity triggers the forfeiture of territorial inviolability — and who adjudicates?',
    'Analyze UNSC practice, regional organization authorizations, and unilateral intervention cases since 1990 to map the actual trigger threshold and adjudicative authority distribution.',
    'A clear, consensual threshold with centralized adjudication (UNSC-only) reduces extraction by limiting arbitrary intervention. A contested, multi-actor adjudication space increases extraction by enabling selective application. This directly affects the constraint''s effective extraction for targeted_sovereign_states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_threshold_calibration, empirical, 'Precision and authority of the atrocity threshold that triggers sovereignty forfeiture').

omega_variable(
    humanitarian_motive_purity,
    'Do intervention coalitions and global governance institutions benefit from the conditional_responsibility reading in ways that exceed the coordination function (preventing atrocities)?',
    'Track post-intervention resource flows, institutional mandate expansion, and strategic positioning gains for interveners vs. atrocity prevention outcomes. Compare cases with and without strategic interests at stake.',
    'If intervener benefits systematically exceed atrocity reduction, the constraint operates as Tangled Rope with asymmetric extraction. If benefits track prevention outcomes, it operates closer to Rope. This determines whether the coordination story is cover or core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_motive_purity, empirical, 'Whether the beneficiaries'' gains are proportional to the coordination function or constitute extractive surplus').

omega_variable(
    targeted_state_exit_options,
    'What exit options do targeted sovereign states actually have when faced with intervention threats under this reading?',
    'Analyze compliance pathways: internal reform, diplomatic off-ramps, regional mediation, great power patronage. Assess whether exit is structurally trapped, identity_locked (regime survival fused with sovereignty claim), constrained, or mobile.',
    'If targeted states are identity_locked (regime identity fused with absolute sovereignty claim), effective extraction is amplified. If they have constrained but real diplomatic exits, extraction is moderated. This drives the directionality derivation for the victim seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeted_state_exit_options, empirical, 'Structural exit options for states facing conditional sovereignty forfeiture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ws_cond_resp_tr_t1990, westphalia_sovereignty__conditional_responsibility, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(ws_cond_resp_tr_t1995, westphalia_sovereignty__conditional_responsibility, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(ws_cond_resp_tr_t1999, westphalia_sovereignty__conditional_responsibility, theater_ratio, 1999, 0.28).
narrative_ontology:measurement(ws_cond_resp_tr_t2005, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(ws_cond_resp_tr_t2011, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2011, 0.39).
narrative_ontology:measurement(ws_cond_resp_tr_t2015, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(ws_cond_resp_tr_t2024, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(ws_cond_resp_be_t1990, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(ws_cond_resp_be_t1995, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(ws_cond_resp_be_t1999, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 1999, 0.51).
narrative_ontology:measurement(ws_cond_resp_be_t2005, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(ws_cond_resp_be_t2011, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2011, 0.65).
narrative_ontology:measurement(ws_cond_resp_be_t2015, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(ws_cond_resp_be_t2024, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ws_cond_resp_su_t1990, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(ws_cond_resp_su_t1995, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(ws_cond_resp_su_t1999, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 1999, 0.61).
narrative_ontology:measurement(ws_cond_resp_su_t2005, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(ws_cond_resp_su_t2011, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2011, 0.71).
narrative_ontology:measurement(ws_cond_resp_su_t2015, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(ws_cond_resp_su_t2024, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__conditional_responsibility, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three in the westphalia_sovereignty kernel family. absolute_non_intervention (ε≈0.15, Mountain) is the upstream Mountain — the historical baseline of territorial inviolability. conditional_responsibility (ε≈0.68, Tangled Rope) is the downstream contested extraction layer that overrides the Mountain's immunity for atrocity cases. graded_sovereignty (ε≈0.45, Scaffold) is a parallel transitional reading that calibrates intervention to capacity deficits rather than atrocity triggers. The ε values differ by wide margins (0.15 vs 0.45 vs 0.68) because they instantiate different constraints: different victim sets, different adjudicative authorities, different enforcement machinery. They are linked as a constraint family because the upstream Mountain's immunity is what the downstream readings must breach, and the graded reading competes with conditional_responsibility for the adjudicative authority space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, powerful, 0.95).
constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, organized, 0.65).
constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, institutional, 0.1).
constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, powerless, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
