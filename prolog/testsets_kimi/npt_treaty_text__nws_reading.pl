% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Treaty Text â NWS Reading: Binding Non-Proliferation, Aspirational Disarmament
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint story instantiates the NWS reading of the NPT treaty text
 *   kernel: non-proliferation is a binding obligation on NNWS backed by
 *   active IAEA safeguards and sanctions, while Article VI disarmament is an
 *   aspirational long-term goal without enforcement. The NWS reading benefits
 *   nuclear weapon states by preserving interpretive control over 'at an
 *   early date' and concentrating verification resources on horizontal
 *   proliferation. It is authored as a tangled_rope â the arrangement
 *   solves a genuine coordination problem (preventing cascade proliferation)
 *   while asymmetrically extracting compliance costs and sovereignty from
 *   NNWS. The metrics and claim are independent: the high extraction and
 *   active enforcement scores describe the asymmetry, while the tangled_rope
 *   claim reflects the hybrid coordination-extraction structure.
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (NWS): Primary beneficiary and interpretive authority (institutional/arbitrage) â control treaty meaning and collect strategic latitude.
 *   - Non-Nuclear Weapon States (NNWS): Primary target and payer (organized/constrained) â bear safeguards burdens and relinquished weapons option.
 *   - IAEA Secretariat: Enforcement and administration (institutional/constrained) â concentrates verification on NNWS under the NWS-favored reading.
 *   - Abolition Advocacy Networks: Structurally excluded voice (organized/constrained) â demand Article VI enforcement without leverage to obtain it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.76).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.68).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Treaty Text â NWS Reading: Binding Non-Proliferation, Aspirational Disarmament").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '8de6edaf-de81-47f7-973e-9e57faa79674').
narrative_ontology:cs_kernel_codification('8de6edaf-de81-47f7-973e-9e57faa79674', fixed_text).
narrative_ontology:cs_authority_grounding('8de6edaf-de81-47f7-973e-9e57faa79674', extraction).
narrative_ontology:cs_interpretation_layer_present('8de6edaf-de81-47f7-973e-9e57faa79674').
narrative_ontology:cs_reading_relation('8de6edaf-de81-47f7-973e-9e57faa79674', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('8de6edaf-de81-47f7-973e-9e57faa79674', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('8de6edaf-de81-47f7-973e-9e57faa79674', foundational, article_vi_non_binding_aspiration).
narrative_ontology:cs_axiom_status(article_vi_non_binding_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('8de6edaf-de81-47f7-973e-9e57faa79674', article_vi_non_binding_aspiration, conventional).
narrative_ontology:cs_axiom('8de6edaf-de81-47f7-973e-9e57faa79674', foundational, nws_interpretive_primacy_on_disarmament).
narrative_ontology:cs_axiom_status(nws_interpretive_primacy_on_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('8de6edaf-de81-47f7-973e-9e57faa79674', nws_interpretive_primacy_on_disarmament, conventional).
narrative_ontology:cs_reference_frame('8de6edaf-de81-47f7-973e-9e57faa79674', nws_led_nonproliferation_order).
narrative_ontology:cs_drift_state('8de6edaf-de81-47f7-973e-9e57faa79674', nws_modernization_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8de6edaf-de81-47f7-973e-9e57faa79674', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea_secretariat).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, extended_deterrence_legitimacy).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, horizontal_nonproliferation_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise interpretive control over NPT Articles VI and II, ensuring that 'at an early date' is treated as aspirational while non-proliferation obligations on NNWS are binding and enforced. They retain nuclear arsenals, modernize warheads, and extend deterrence guarantees, collecting strategic latitude and regime legitimacy.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter).

% Relinquish the nuclear weapons option and accept comprehensive IAEA safeguards on civilian nuclear programs. They bear compliance costs, technology denial, and sovereignty intrusions while receiving only rhetorical disarmament commitments from NWS. Withdrawal under Article X is legally possible but incurs severe diplomatic and economic penalties.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% Administers the safeguards system that enforces NNWS non-proliferation obligations under the NPT. Its verification mandate, budget, and institutional focus are concentrated on horizontal proliferation risks in NNWS, with limited access to monitor NWS disarmament. Its authority and funding derive from the NWS-favored reading that prioritizes non-proliferation over disarmament verification.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea_secretariat, beneficiary).

% Campaign for binding nuclear disarmament timelines and full implementation of Article VI. They participate in NPT review conferences and civil society forums but lack institutional leverage to alter interpretive outcomes controlled by NWS; their demands are acknowledged rhetorically and systematically deferred.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, abolition_advocacy_networks, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the international community to prevent the horizontal spread of nuclear weapons to additional states, reducing cascade proliferation risks and stabilizing a multi-state security environment through a common safeguards and verification framework.
% TRANSFER_FUNCTION: Moves compliance burdens, sovereignty costs, and forgone military nuclear options from NNWS to the NWS security umbrella and IAEA verification architecture, while concentrating interpretive authority over disarmament timelines in NWS diplomatic dominance.
% ABSENT_VOICES: NNWS disarmament advocates within the non-aligned movement and civil society abolition campaigns; they are present at review conferences but structurally excluded from interpretive control. The International Court of Justice's 1996 advisory opinion on Article VI is likewise marginalized in NWS security policy.
% DISAPPEARANCE_RATIONALE: Without this reading, NNWS would likely demand symmetrical binding disarmament schedules or reconsider safeguards compliance; the IAEA would lose its central non-proliferation mandate; and the global nuclear order would fragment into ad hoc bilateral or regional security arrangements.
% FOUNDING_PROBLEM: The rapid horizontal proliferation of nuclear weapons in the 1960s threatened to destabilize the international system by multiplying nuclear-armed states beyond the existing five.
% FOUNDING_PROBLEM_CORROBORATION: NWS and the IAEA attest horizontal proliferation remains a live threat, citing cases like Iran and North Korea. Independent nuclear security scholars and NNWS coalitions attest the founding crisis has shifted to vertical modernization and doctrinal expansion by NWS; academic security studies and the Bulletin of the Atomic Scientists corroborate the functional drift away from the original proliferation emergency.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.76) is high because the constraint binds NNWS to intrusive verification and technology denial while NWS disarmament remains unenforced and rhetorical. Suppression (0.68) reflects active enforcement via IAEA safeguards, export control regimes, and sanctions for NNWS non-compliance; NWS vertical proliferation is unsuppressed. Theater ratio (0.45) captures the growing gap between disarmament rhetoric and actual NWS modernization. Accessibility collapse (0.72) is high because no viable alternative treaty framework exists for NNWS seeking security without the NPT's asymmetric structure. Resistance (0.55) reflects persistent NNWS and non-aligned demands at review conferences, but these have not altered the interpretive status quo.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the constraint is necessary global public goods management: preventing nuclear anarchy. From the NNWS seat, it is institutionalized discrimination that locks in technological subordination and security dependence. The IAEA seat experiences the constraint as a legitimate technical mission with underfunded disarmament dimensions. The engine computes this divergence from the same structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS have near-zero directionality as beneficiaries (dâ0): the constraint subsidizes their strategic position by externalizing non-proliferation enforcement costs and deferring disarmament indefinitely. NNWS have high directionality as trapped payers (dâ1): they incur sovereignty costs, technology restrictions, and compliance burdens while the benefit flow (security assurances) is non-binding and conditional. IAEA sits near symmetric (d~0.5) because it is both mission-fulfilling and institutionally dependent on the NWS reading. Abolition advocates are excluded and thus outside the primary directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â horizontal proliferation â remains live enough that the coordination function is genuine, preventing classification as a pure snare. However, the disarmament component has atrophied: 'at an early date' has been reinterpreted from an urgent commitment to an indefinite aspiration. This mandatrophy is unresolved because the atrophied limb persists and continues to shape expectations. The reading prevents mislabeling the constraint as a rope by documenting the asymmetric enforcement, and prevents mislabeling it as a snare by preserving the genuine non-proliferation coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disarmament_binding_vs_aspirational,
    'Does Article VI of the NPT create a binding obligation of result for disarmament, or merely an unenforceable obligation of conduct?',
    'Adjudication by the International Court of Justice with compulsory jurisdiction, or unanimous adoption of a binding treaty amendment clarifying Article VI.',
    'If Article VI is binding result, the NWS reading collapses toward the NNWS reading, effective extraction drops sharply, and the constraint reclassifies toward rope. If it remains aspirational, the asymmetry persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_binding_vs_aspirational, conceptual, 'The legal status of Article VI as binding or aspirational').

omega_variable(
    iaea_capture_by_nws_interests,
    'Has the IAEA safeguards regime been structurally captured by the NWS reading, or does it neutrally administer the treaty text?',
    'Comparative budget analysis of IAEA spending on NNWS safeguards versus NWS disarmament verification, plus assessment of Director General appointment politics.',
    'If captured, the coordination function is subordinate to beneficiary interests, reinforcing tangled_rope classification. If neutral, the extraction is lower and the classification edges toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_capture_by_nws_interests, empirical, 'Whether IAEA institutional focus reflects NWS interpretive preferences').

omega_variable(
    withdrawal_threshold_regime_stability,
    'Does the NWS reading''s emphasis on regime stability structurally pressure a high threshold for Article X withdrawal, thereby trapping NNWS in the constraint?',
    'Comparative case study of North Korea''s 2003 withdrawal and subsequent sanctions to establish whether exit is punished as defection or accepted as sovereign right.',
    'If exit is systematically punished, NNWS exit options are more constrained than they appear, amplifying effective extraction. If exit is genuinely available, the constraint is less extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(withdrawal_threshold_regime_stability, empirical, 'Interaction between NWS reading and Article X withdrawal coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nws_tr_t0, npt_treaty_text__nws_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(npt_nws_tr_t11, npt_treaty_text__nws_reading, theater_ratio, 11, 0.25).
narrative_ontology:measurement(npt_nws_tr_t22, npt_treaty_text__nws_reading, theater_ratio, 22, 0.32).
narrative_ontology:measurement(npt_nws_tr_t33, npt_treaty_text__nws_reading, theater_ratio, 33, 0.38).
narrative_ontology:measurement(npt_nws_tr_t44, npt_treaty_text__nws_reading, theater_ratio, 44, 0.42).
narrative_ontology:measurement(npt_nws_tr_t55, npt_treaty_text__nws_reading, theater_ratio, 55, 0.45).

% Extraction over time
narrative_ontology:measurement(npt_nws_be_t0, npt_treaty_text__nws_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt_nws_be_t11, npt_treaty_text__nws_reading, base_extractiveness, 11, 0.52).
narrative_ontology:measurement(npt_nws_be_t22, npt_treaty_text__nws_reading, base_extractiveness, 22, 0.6).
narrative_ontology:measurement(npt_nws_be_t33, npt_treaty_text__nws_reading, base_extractiveness, 33, 0.66).
narrative_ontology:measurement(npt_nws_be_t44, npt_treaty_text__nws_reading, base_extractiveness, 44, 0.72).
narrative_ontology:measurement(npt_nws_be_t55, npt_treaty_text__nws_reading, base_extractiveness, 55, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(npt_nws_su_t0, npt_treaty_text__nws_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(npt_nws_su_t11, npt_treaty_text__nws_reading, suppression_requirement, 11, 0.48).
narrative_ontology:measurement(npt_nws_su_t22, npt_treaty_text__nws_reading, suppression_requirement, 22, 0.56).
narrative_ontology:measurement(npt_nws_su_t33, npt_treaty_text__nws_reading, suppression_requirement, 33, 0.62).
narrative_ontology:measurement(npt_nws_su_t44, npt_treaty_text__nws_reading, suppression_requirement, 44, 0.66).
narrative_ontology:measurement(npt_nws_su_t55, npt_treaty_text__nws_reading, suppression_requirement, 55, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the npt_treaty_text kernel. It is structurally paired with nnws_reading (disarmament binding) and withdrawal_threshold_reading (regime stability vs. sovereignty). The NWS reading's high extraction derives from interpretive control over disarmament ambiguity, while the NNWS reading would redistribute extraction toward NWS disarmament compliance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
