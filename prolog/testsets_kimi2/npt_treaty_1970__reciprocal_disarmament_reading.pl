% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI Reciprocal Disarmament Obligation
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This constraint story instantiates the reciprocal disarmament reading of
 *   the 1968 Nuclear Non-Proliferation Treaty (entered into force 1970).
 *   Under this reading, Article VI constitutes a binding legal obligation
 *   with temporal urgency, and the treaty's core structure is a reciprocal
 *   bargain: NNWS forgo nuclear weapons in exchange for verified NWS
 *   disarmament. The reading treats NWS strategic modernization as a victim
 *   position â the constraint extracts from NWS by legally obligating
 *   disarmament even while enforcement gaps prevent actual compliance. NNWS
 *   gain normative leverage through the Review Conference process, though
 *   they also bear the costs of permanent technological forbearance and
 *   intrusive safeguards. The enforcement gap â IAEA verification of NNWS
 *   but no Article VI verification of NWS â is treated as structural
 *   injustice rather than implementation detail. This reading competes with
 *   the oligopoly enforcement reading (Articles I-II as primary, Article VI
 *   as aspirational) and the withdrawal sovereignty reading (Article X as
 *   legitimate escape).
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (NWS): Primary payer/target â institutional power, constrained exit, bear the Article VI obligation while resisting its operationalization.
 *   - Non-Nuclear Weapon States (NNWS): Primary beneficiary â organized power, constrained exit, derive normative leverage and nonproliferation security.
 *   - IAEA Safeguards System: Agenda-setter/administrator â institutional power, analytical exit, enforces NNWS obligations but lacks Article VI mandate.
 *   - Non-Aligned Movement disarmament advocates: Secondary beneficiary â organized power, coordinate NNWS pressure.
 *   - International Court of Justice: Analytical observer â institutional power, assesses legal obligation without enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.72).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI Reciprocal Disarmament Obligation").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '1dd18710-a0d5-42e1-bc8f-e9aa9168e496').
narrative_ontology:cs_kernel_codification('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', formalized).
narrative_ontology:cs_authority_grounding('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', lineage).
narrative_ontology:cs_interpretation_layer_present('1dd18710-a0d5-42e1-bc8f-e9aa9168e496').
narrative_ontology:cs_reading_relation('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', npt_treaty_1970__oligopoly_enforcement_reading, forecloses).
narrative_ontology:cs_reading_relation('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', foundational, article_vi_binding_temporal_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_temporal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', article_vi_binding_temporal_obligation, conventional).
narrative_ontology:cs_axiom('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', foundational, horizontal_vertical_nonproliferation_reciprocity).
narrative_ontology:cs_axiom_status(horizontal_vertical_nonproliferation_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', horizontal_vertical_nonproliferation_reciprocity, conventional).
narrative_ontology:cs_reference_frame('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', reciprocal_nonproliferation_bargain).
narrative_ontology:cs_drift_state('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', contemporary_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1dd18710-a0d5-42e1-bc8f-e9aa9168e496', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_aligned_movement_disarmament_advocates).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, nonproliferation_norm).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, article_vi_legal_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain nuclear arsenals and modernize delivery systems while formally acknowledging Article VI obligations. Experience the constraint as a legal and political limitation on strategic autonomy and modernization programs that is not matched by verification or enforcement mechanisms targeting their arsenals. Their unilateral exit from the treaty would collapse the nonproliferation regime, so they remain within it while resisting disarmament timetables and incremental reduction benchmarks.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% Forgoed nuclear weapons programs in exchange for the promise of general and complete disarmament under Article VI. Derive normative leverage from the NWS disarmament obligation at Review Conferences and in UN forums. Experience genuine coordination benefit from reduced proliferation risk among neighbors, but also bear the cost of permanent technological exclusion from the nuclear field and acceptance of intrusive IAEA safeguards.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states, beneficiary,
    organized, generational, constrained, global).

% Administers comprehensive safeguards agreements verifying NNWS compliance with nonproliferation obligations. Has no mandate to verify NWS disarmament under Article VI, creating the enforcement gap that defines the constraint's asymmetry. Its authority derives from the treaty but its operational scope is structurally truncated at the NWS threshold.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_system, agenda_setter,
    institutional, generational, analytical, global).

% Coordinate NNWS diplomatic pressure to hold NWS accountable to Article VI commitments at NPT Review Conferences and in the UN General Assembly. Collect normative leverage and institutional voice through the review process and disarmament resolutions. Their advocacy reinforces the reciprocal framing against NWS resistance but does not itself enforce compliance.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_aligned_movement_disarmament_advocates, beneficiary,
    organized, generational, constrained, global).

% Issued the 1996 Advisory Opinion affirming the legal obligation to pursue nuclear disarmament in good faith. Sits outside the treaty power structure but provides jurisprudential support for the reciprocal reading. Lacks enforcement capacity and relies on state consent for jurisdiction over contentious cases.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents widespread horizontal nuclear proliferation by creating a legally binding framework in which NNWS forgo nuclear weapons in exchange for NWS commitment to general and complete disarmament, solving the collective-action problem of unconstrained nuclear arms racing among sovereign states.
% TRANSFER_FUNCTION: Transfers nuclear restraint obligations asymmetrically: NNWS surrender the sovereign option to acquire nuclear weapons and accept intrusive verification, while NWS incur the obligation to eliminate their arsenals over time. Normative and diplomatic leverage flows from NNWS to NWS through review conference politics and international legal pressure.
% ABSENT_VOICES: States that developed nuclear weapons outside the treaty architecture (India, Pakistan, Israel) and non-state actors seeking nuclear capability are structurally excluded from the bargain. Their exclusion reveals the constraint's dependence on sovereign consent rather than universal coverage, and their absence from the regime means the reciprocal frame is not tested against their behavior.
% DISAPPEARANCE_RATIONALE: If the reciprocal disarmament obligation vanished overnight, the bargain holding the nonproliferation regime together would dissolve. NNWS would lose the legal foundation for their restraint and the normative leverage they wield at Review Conferences; the regime would likely fragment as threshold states reconsider their nuclear options, and the IAEA safeguards system would lose its central legal mandate.
% FOUNDING_PROBLEM: The rapid horizontal proliferation of nuclear weapons in the 1960s threatened to distribute nuclear capability to dozens of states, creating unacceptable escalation risks, command-and-control instability, and regional arms racing.
% FOUNDING_PROBLEM_CORROBORATION: Independent security studies scholars, the IAEA Director General's annual reports on proliferation risks, and successive UN Secretaries-General attest that horizontal proliferation remains a live threat. However, the reciprocal character of the solution â tying NNWS abstinence to NWS disarmament â is contested by NWS parties who assert the problem is solved by horizontal prevention alone; no neutral party outside the NNWS coalition corroborates that the reciprocal linkage is the operative mechanism rather than a diplomatic framing.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the treaty's operational reality extracts permanent technological abstinence from NNWS while returning only nominal legal obligations from NWS; the asymmetry accumulates as NWS modernize. Suppression (0.72) is higher still because the regime's persistence depends on actively excluding nuclear alternatives for NNWS (safeguards, sanctions for violators) and diplomatically suppressing Article VI enforcement demands. Theater_ratio (0.65) reflects the rise of performative disarmament diplomacy (review conferences, working papers, incremental confidence-building) that substitutes for actual arsenals elimination. Resistance (0.45) is moderate: NWS actively resist disarmament timetables, while some NNWS resist the unequal bargain through diplomatic obstruction. Accessibility_collapse (0.70) is high because the NPT has become the nearly exclusive legitimate pathway for nuclear status; withdrawal (North Korea) or outsider acquisition (India, Pakistan) carries catastrophic diplomatic cost, collapsing viable alternatives for most states.
 *
 * PERSPECTIVAL GAP:
 *   The NWS seat experiences the constraint as a legitimate coordination regime they sustain and administer, with Article VI as a flexible political commitment. The NNWS seat experiences the same text as a broken contractual promise in which their permanent forbearance is traded for NWS bad faith. The IAEA seat experiences a truncated mandate that verifies the weak party but not the strong party. These divergences are structurally derived from the same treaty text and do not resolve into a single type from any one seat.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are declared victims because Article VI structurally constrains their strategic autonomy and modernization options under international law, even if enforcement is weak; their directionality sits near the target end (d elevated by victim status and constrained exit). NNWS are declared beneficiaries because they gain normative leverage and nonproliferation security; their directionality sits near the beneficiary end (d lowered by beneficiary status, though constrained exit prevents full mobility). The IAEA is agenda_setter with analytical exit â its directionality is structurally ambiguous and computed by the engine from its enforcement role without capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the genuine coordination function (preventing horizontal proliferation, which serves global security) from the asymmetric extraction pattern (NNRS permanent forbearance unmatched by NWS disarmament). A pure rope reading would ignore the Article VI enforcement gap and the NWS modernization trajectory; a pure snare reading would ignore the real collective-action problem solved by horizontal nonproliferation. The tangled_rope classification captures that both are present and coupled: the coordination story is not merely cover, but the extraction is structurally embedded in the same arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_enforcement_gap_ambiguity,
    'Does the absence of a verification mechanism for Article VI disarmament render the obligation legally unenforceable, or does it constitute a deliberate structural asymmetry preserving NWS privilege?',
    'Comparative analysis of disarmament verification architectures proposed but rejected at Review Conferences; examination of NWS nuclear modernization budgets and force-posture statements.',
    'If the gap is deliberate asymmetry, the constraint is more extractive toward NWS than operational records suggest (they are free-riding on NNWS restraint). If merely unenforceable, the extraction is lower but the coordination is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_enforcement_gap_ambiguity, conceptual, 'Whether the Article VI enforcement gap is designed asymmetry or implementation failure.').

omega_variable(
    npt_kernel_reading_structural_distinctness,
    'Does the reciprocal disarmament reading represent a structurally distinct constraint from the oligopoly enforcement reading, or do they converge on the same operational pattern of NWS privilege?',
    'Comparative seat-classification across the full constraint family; if all seats compute identical types despite divergent claims, the readings are not structurally distinct.',
    'If readings converge operationally, the kernel is a pure narrative contest with no structural variance; if they diverge, the decomposition is validated and the epsilon-invariance principle is satisfied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_kernel_reading_structural_distinctness, conceptual, 'Structural distinctness of reciprocal reading against oligopoly reading.').

omega_variable(
    nnws_restraint_exit_trajectory,
    'Would NNWS actually proliferate if the reciprocal bargain collapsed, or is their restraint now self-enforcing through regional stability preferences?',
    'Regional security dilemma modeling and historical cases of threshold states (Japan, South Korea, Brazil, South Africa).',
    'If restraint is self-enforcing, the coordination function was always stronger than the bargain; if restraint depends on the bargain, the constraint is a genuine rope for NNWS despite NWS non-compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nnws_restraint_exit_trajectory, empirical, 'Whether NNWS restraint is endogenous or treaty-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_reciprocal_tr_t0, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt_reciprocal_tr_t10, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(npt_reciprocal_tr_t20, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(npt_reciprocal_tr_t30, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(npt_reciprocal_tr_t40, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement(npt_reciprocal_tr_t50, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(npt_reciprocal_be_t0, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(npt_reciprocal_be_t10, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(npt_reciprocal_be_t20, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(npt_reciprocal_be_t30, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(npt_reciprocal_be_t40, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(npt_reciprocal_be_t50, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt_reciprocal_su_t0, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(npt_reciprocal_su_t10, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(npt_reciprocal_su_t20, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(npt_reciprocal_su_t30, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(npt_reciprocal_su_t40, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(npt_reciprocal_su_t50, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested NPT treaty kernel. It decomposes the treaty into structurally distinct claims: the reciprocal disarmament reading (Article VI binding), the oligopoly enforcement reading (Articles I-II primary), and the withdrawal sovereignty reading (Article X as safety valve). Each reading emits a different constraint with different beneficiary/victim structures and different empirical anchors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
