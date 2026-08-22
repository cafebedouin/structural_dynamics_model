% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Sovereignty Doctrine (Non-Interference Shield)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolute-sovereignty reading of the
 *   Westphalian kernel: the claim that sovereignty grants states
 *   unconditional authority over domestic affairs and that any external
 *   interference is categorically illegitimate, regardless of internal
 *   conduct. The reading provides a real coordination function (a stable
 *   baseline against cross-border conquest and coerced regime change) but, as
 *   practiced by incumbent and especially authoritarian regimes and enforced
 *   selectively through the UN Security Council veto structure, also
 *   functions as an extraction shield: domestic populations under repression
 *   bear costs the doctrine categorically forecloses external remedy for.
 *   This is a distinct constraint from the conditional-sovereignty reading
 *   (which ties legitimacy to a responsibility-to-protect threshold) and the
 *   graduated-sovereignty reading (which scales sovereign authority to
 *   governance capacity) — those are separate stories with separate ε values,
 *   linked here via network.affects_constraints, not alternative measurements
 *   of this one.
 *
 * KEY AGENTS:
 *   - authoritarian_governments: agenda_setter/beneficiary (institutional/arbitrage) — invoke and administer the doctrine domestically
 *   - un_security_council_permanent_members: beneficiary/agenda_setter (institutional/arbitrage) — control selective global enforcement
 *   - domestic_populations_under_repression: payer (powerless/trapped) — bear the extraction with no exit
 *   - international_legal_scholars: observer (analytical/analytical) — document the gap between doctrine and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.68).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Sovereignty Doctrine (Non-Interference Shield)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '7d00c1a3-198c-4eef-bf3e-a255b5c93bd4').
narrative_ontology:cs_kernel_codification('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', distributed).
narrative_ontology:cs_authority_grounding('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', distributed).
narrative_ontology:cs_reading_relation('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', foundational, sovereignty_is_categorical_and_unconditional).
narrative_ontology:cs_axiom_status(sovereignty_is_categorical_and_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', sovereignty_is_categorical_and_unconditional, conventional).
narrative_ontology:cs_axiom('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', foundational, external_interference_illegitimate_regardless_of_internal_conduct).
narrative_ontology:cs_axiom_status(external_interference_illegitimate_regardless_of_internal_conduct, holdable).
narrative_ontology:cs_axiom_grounding('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', external_interference_illegitimate_regardless_of_internal_conduct, deontological).
narrative_ontology:cs_reference_frame('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', peace_of_westphalia_non_interference_baseline).
narrative_ontology:cs_drift_state('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', post_cold_war_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7d00c1a3-198c-4eef-bf3e-a255b5c93bd4', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, incumbent_state_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_governments).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, un_security_council_permanent_members).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, ethnic_and_religious_minorities).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, dissidents_and_political_prisoners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, weaker_states_facing_selective_intervention).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, territorial_integrity_norm).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, non_aggression_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke sovereignty to bar external scrutiny of internal governance, deploy the doctrine at the UN and in bilateral diplomacy to block sanctions, investigations, or intervention regardless of internal conduct. They administer domestic enforcement and control the border across which the sovereignty claim operates.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, authoritarian_governments, beneficiary).

% Any government, repressive or not, gains a categorical shield against external interference in domestic affairs — a general insurance policy against foreign pressure that many governments value even when not currently repressive.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, incumbent_state_regimes, beneficiary,
    institutional, generational, arbitrage, national).

% Wield veto power to enforce the non-interference norm selectively — invoking it to shield allies and client states from intervention while suspending it against adversaries. Their institutional position lets them both benefit from the doctrine and control its application.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, un_security_council_permanent_members, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, un_security_council_permanent_members, agenda_setter).

% Live under regimes that cite sovereignty to bar humanitarian monitors, foreign journalists, and international legal accountability. Have no domestic recourse (courts, elections, or press are controlled by the same regime) and no external recourse (the sovereignty norm forecloses intervention). Exit is bordered by the same state that extracts from them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, biographical, trapped, local).

% Subject to state violence or systematic discrimination framed by the regime as an internal matter; international bodies citing the sovereignty norm decline to classify the situation as warranting action until atrocity thresholds are reached, if ever.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, ethnic_and_religious_minorities, payer,
    powerless, biographical, trapped, local).

% Detained, tortured, or disappeared for opposing the regime; foreign governments and international bodies that might otherwise pressure for release cite non-interference as the reason they will not act, or act only through weak diplomatic channels with no enforcement teeth.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, dissidents_and_political_prisoners, payer,
    powerless, immediate, trapped, local).

% Document abuses and seek entry or standing to investigate, but are routinely denied visas, expelled, or have their findings dismissed as illegitimate interference. Their evidentiary work has no formal channel into the sovereignty-gated decision process.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_monitors_and_ngos, excluded,
    organized, generational, constrained, global).

% Nominally hold the same sovereign shield as powerful states, but lack the diplomatic and military weight to make the norm stick when a great power decides intervention serves its interest — the doctrine protects strong states reliably and weak ones selectively.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, weaker_states_facing_selective_intervention, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, weaker_states_facing_selective_intervention, excluded).

% Study the doctrine's application record, documenting the gap between its universal claim and its selective enforcement, without power to change how states or the Security Council apply it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable baseline rule against cross-border aggression and forced regime change, letting states plan and interact without constant threat of external conquest or coerced restructuring under the pretext of internal reform.
% TRANSFER_FUNCTION: Moves accountability away from the international system and onto domestic institutions exclusively, regardless of whether those institutions are captured by the same actors accused of abuse — effectively transferring protection from vulnerable populations to incumbent power-holders.
% ABSENT_VOICES: Domestic populations under repression, minorities, and dissidents have no seat in the interstate system that adjudicates sovereignty claims; their objections are filtered entirely through the state accused of harming them, which has every incentive to suppress or deny the claim.
% DISAPPEARANCE_RATIONALE: If absolute non-interference vanished overnight, the entire architecture of UN Charter Article 2(4)/2(7) diplomacy, non-intervention treaties, and great-power justifications for restraint (or selective intervention) would require reconstruction; states would need a new baseline principle to structure interstate relations and could no longer categorically bar humanitarian or accountability mechanisms.
% FOUNDING_PROBLEM: Post-Thirty Years' War Europe needed to stop trans-national religious and dynastic wars fought under the banner of correcting other rulers' internal religious or political arrangements — sovereignty as non-interference was built to end centuries of externally justified invasion.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Peace of Westphalia and international relations theorists corroborate the founding anti-war-of-religion function as historically real but note it has been substantially repurposed; UN human rights mechanisms, war crimes tribunals, and NGOs external to any single incumbent regime attest that the doctrine's current chief practical effect is shielding internal repression, not preventing interstate war (which is now governed more by separate non-aggression and collective security norms).
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.55, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55, within the expected 0.45-0.60 band: high enough to reflect that the doctrine's categorical form (not merely its occasional abuse) structurally forecloses remedy for internal victims, but not maximal because the coordination function (preventing wars of external regime-correction) is genuinely operative and non-trivial. Suppression (0.68) is higher than extractiveness because the doctrine's persistence depends on active diplomatic and institutional enforcement — Security Council vetoes, denial of humanitarian access, expulsion of monitors — not on voluntary participant preference. Theater ratio (0.4) reflects that a meaningful share of invocations are genuine sovereignty defenses against real interference risk, but a rising share (0.2 to 0.4 over the interval) is performative justification for blocking accountability specifically. Accessibility collapse (0.5) is moderate: alternative doctrines (conditional/graduated sovereignty, R2P) exist and are actively argued in international forums, so alternatives have not collapsed, but the doctrine's veto-backed enforcement makes them practically unreachable in specific cases.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seat (authoritarian governments, P5 members), the doctrine reads as legitimate coordination — the bedrock of a stable interstate order that any government would want protected, including their own if roles were reversed. From the payer seat (domestic populations under repression), the identical rule reads as a total foreclosure of remedy: the same categorical language that protects a state from invasion also protects it from accountability for internal atrocity. The engine should compute these as structurally different seat classifications from the same base data, not reconcile them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (authoritarian governments, incumbent regimes generally, P5 members) sit near the full-beneficiary end of directionality: the doctrine subsidizes their freedom of action and is arbitrage-mobile for them (they can invoke or waive it strategically for allies vs. adversaries). Victims (domestic populations, minorities, dissidents) sit at the full-target end: trapped exit options, powerless power atom, and the doctrine's operation is specifically what removes their only potential lever (external pressure). Weaker states occupy an intermediate position — nominal beneficiaries of the norm's universal claim, but its selective enforcement by the P5 means they experience it more as payers when a great power decides intervention serves its interest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending externally justified wars of religious/dynastic correction) is genuinely dead in the narrow historical sense — no major power today invades to correct another's religious establishment — but the doctrine's mandate has been redirected wholesale toward shielding domestic human-rights conduct from any external mechanism, a function it was never built for. This is exactly the mismatch the R5 corroboration surfaces: founding_problem_status is contested because incumbent regimes assert the problem (external aggression) is still live and justifies the shield, while independent observers (tribunals, NGOs, historians) attest the doctrine now chiefly does something else — protect repression, not prevent invasion. Classifying this as tangled_rope rather than snare preserves the genuine coordination residue (anti-conquest norm) that would be lost by naive extraction-only framing, while the requires_active_enforcement flag and named victims prevent this residue from laundering the extractive current use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_ratio_stability,
    'Does the genuine anti-conquest coordination function of absolute sovereignty remain load-bearing in the current interstate system, or has it become vestigial cover for the extraction function (shielding internal repression)?',
    'Comparative analysis of interstate invocation patterns: count invocations of the non-interference norm that actually deterred cross-border military aggression versus invocations that blocked human-rights monitoring, sanctions, or accountability mechanisms, over the measurement interval.',
    'If the coordination invocations have become rare relative to the extraction invocations, the constraint''s tangled_rope classification is closer to a snare with residual coordination language; if coordination invocations remain frequent and load-bearing, tangled_rope is the accurate steady-state classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_stability, empirical, 'Whether the anti-war coordination function of absolute sovereignty is still active or has become a legitimating veneer.').

omega_variable(
    selective_enforcement_versus_universal_norm,
    'Is absolute sovereignty a genuinely universal norm applied unevenly due to power asymmetry, or is the universalist framing itself a fiction that only ever described great-power protection?',
    'Historical audit of Security Council intervention/non-intervention decisions cross-referenced with the relative power of the target state and its alignment with P5 interests.',
    'If the norm was never genuinely universal in practice, the coordination story for weaker states is substantially weaker than authored here, pushing the classification toward snare for that subgroup even while remaining tangled_rope in aggregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_versus_universal_norm, conceptual, 'Whether apparent selective enforcement reflects an implementation gap or the doctrine''s actual design.').

omega_variable(
    kernel_framing_choice_disagreement_location,
    'Where exactly does the absolute reading''s authority-grounding claim diverge from the conditional and graduated readings — is it a difference about facts (what triggers legitimate concern) or a difference about the deontological status of the sovereign boundary itself?',
    'Trace specific historical disputes (e.g., Rwanda 1994, Kosovo 1999, Libya 2011, Xinjiang) where advocates of each reading gave different accounts of the same facts, isolating whether the divergence was empirical (what happened) or normative (whether it mattered enough to override sovereignty).',
    'If the divergence is chiefly normative/deontological, the readings genuinely coexist as competing value commitments (as authored in reading_relations); if it is chiefly empirical (disagreement about facts on the ground), the readings may be more reconcilable than the coexists_with relation suggests, which would push toward reclassifying the kernel relationship as influences rather than parallel coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_disagreement_location, conceptual, 'Whether the kernel''s readings diverge on facts or on foundational values, which determines whether the coexists_with framing is durable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(west_tr_t1960, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(west_tr_t1975, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(west_tr_t2015, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(west_be_t1960, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(west_be_t1975, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(west_be_t2015, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(west_su_t1960, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(west_su_t1975, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(west_su_t2015, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__absolute_sovereignty, 0.1).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the westphalian_sovereignty kernel. absolute_sovereignty (this story) authors ε=0.55 with a tangled_rope classification favoring incumbent and authoritarian regimes. conditional_sovereignty and graduated_sovereignty are separate stories with their own beneficiary/victim structures and ε values reflecting their different normative commitments about when sovereignty yields to external accountability. All three should be read as siblings, not as measurements of one underlying constraint at different observables — per the ε-invariance principle, differing beneficiary/victim structures and differing extraction profiles across the readings mean these are three distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
