% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad Faith Determination
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action) is a 2015 multilateral
 *   nuclear agreement between Iran and the P5+1. This constraint story models
 *   ONE READING of the treaty's bindingness: the transactional provisional
 *   reading, which treats the deal as a reversible exchange voidable upon any
 *   party's unilateral determination of bad faith. Under this reading, the
 *   withdrawing state (US, 2018) legitimately reimposed sanctions based on
 *   its own assessment of Iranian non-nuclear malign behavior and sunset
 *   clause inadequacy — not based on IAEA-verified nuclear noncompliance. The
 *   constraint's extraction operates asymmetrically: Iran performed
 *   irreversible compliance (centrifuge removal, concrete pouring, stockpile
 *   reduction) in exchange for reversible sanctions relief that a single
 *   party could revoke. The coordination function (verified nonproliferation)
 *   is genuine but the extraction function (unilateral exit optionality for
 *   the powerful party) is structural. This reading coexists with and
 *   pressures two sibling readings but forecloses the binding multilateral
 *   reading within any single legal framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad Faith Determination").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '512f843d-1c16-4205-9820-830e102ea9fc').
narrative_ontology:cs_kernel_codification('512f843d-1c16-4205-9820-830e102ea9fc', formalized).
narrative_ontology:cs_authority_grounding('512f843d-1c16-4205-9820-830e102ea9fc', lineage).
narrative_ontology:cs_interpretation_layer_present('512f843d-1c16-4205-9820-830e102ea9fc').
narrative_ontology:cs_reading_relation('512f843d-1c16-4205-9820-830e102ea9fc', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('512f843d-1c16-4205-9820-830e102ea9fc', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('512f843d-1c16-4205-9820-830e102ea9fc', foundational, unilateral_withdrawal_legitimate_upon_national_bad_faith_determination).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_legitimate_upon_national_bad_faith_determination, holdable).
narrative_ontology:cs_axiom_grounding('512f843d-1c16-4205-9820-830e102ea9fc', unilateral_withdrawal_legitimate_upon_national_bad_faith_determination, conventional).
narrative_ontology:cs_axiom('512f843d-1c16-4205-9820-830e102ea9fc', foundational, sanctions_reimposition_requires_no_multilateral_consensus).
narrative_ontology:cs_axiom_status(sanctions_reimposition_requires_no_multilateral_consensus, holdable).
narrative_ontology:cs_axiom_grounding('512f843d-1c16-4205-9820-830e102ea9fc', sanctions_reimposition_requires_no_multilateral_consensus, conventional).
narrative_ontology:cs_axiom('512f843d-1c16-4205-9820-830e102ea9fc', secondary, verified_nuclear_compliance_insufficient_for_durable_relief).
narrative_ontology:cs_axiom_status(verified_nuclear_compliance_insufficient_for_durable_relief, holdable).
narrative_ontology:cs_axiom_grounding('512f843d-1c16-4205-9820-830e102ea9fc', verified_nuclear_compliance_insufficient_for_durable_relief, instrumental).
narrative_ontology:cs_reference_frame('512f843d-1c16-4205-9820-830e102ea9fc', jcpoa_as_executed_agreement_2015).
narrative_ontology:cs_drift_state('512f843d-1c16-4205-9820-830e102ea9fc', post_2018_unilateral_withdrawal, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('512f843d-1c16-4205-9820-830e102ea9fc', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_sovereignty).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_opposition_coalitions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iran_nuclear_program).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, non_withdrawing_parties).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, nonproliferation_regime_credibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, non_withdrawing_parties).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, sovereign_right_to_unilateral_treaty_exit).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, bad_faith_as_ground_for_termination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts the right to unilaterally determine Iranian bad faith and reimpose sanctions without multilateral consensus. Collects domestic political benefits from opposition coalitions and preserves sovereign freedom of action. The constraint's enforcement machinery (sanctions apparatus) is operated by this actor.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state, beneficiary).

% Bears the full cost of sanctions reimposition upon unilateral withdrawal determination. Complied with verified restrictions for years in exchange for sanctions relief that proves reversible at another party's sole judgment. Exit from the constraint means accepting economic strangulation or nuclear breakout — both imposed by the constraint's operation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iran_nuclear_program, payer,
    organized, generational, constrained, global).

% UK, France, Germany, Russia, China — invested diplomatic capital and economic relations in the deal's survival. Forced to choose between secondary sanctions compliance (abandoning legitimate commerce) or defending the deal against their ally's withdrawal. Benefit from nonproliferation coordination but pay the cost of the constraint's instability.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, non_withdrawing_parties, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, non_withdrawing_parties, beneficiary).

% Political factions, interest groups, and allied foreign lobbies that opposed the JCPOA from inception. Gain policy vindication and domestic political capital when the withdrawing state exercises its unilateral exit right. Their influence shapes the 'bad faith' determination threshold.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_opposition_coalitions, beneficiary,
    organized, biographical, mobile, national).

% The NPT architecture and IAEA verification system's authority erodes when a verified compliance agreement is treated as provisionally voidable. Future nonproliferation bargains lose credibility because compliance no longer reliably purchases durable sanctions relief. This is a non-agent abstract good that bears diffuse structural costs.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, nonproliferation_regime_credibility, payer,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__transactional_provisional_reading, nonproliferation_regime_credibility).

% Analyze whether the transactional reading is compatible with Vienna Convention treaty law (pacta sunt servanda, Article 60 material breach). Their interpretations influence legitimacy perceptions but they neither collect nor pay the constraint's extraction.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a verified nonproliferation monitoring framework (IAEA access, centrifuge limits, uranium stockpile caps) that all parties accepted as preferable to uncontrolled escalation or military strikes.
% TRANSFER_FUNCTION: Moves the burden of irreversible compliance onto Iran (dismantling infrastructure, accepting intrusive verification) while moving the benefit of reversible sanctions relief and sovereign exit optionality onto the withdrawing state and its domestic opponents.
% ABSENT_VOICES: The Iranian population (distinct from the nuclear program) bears sanctions' humanitarian impact but has no voice in the 'bad faith' determination. Future nonproliferation negotiators (states considering similar bargains) watch the precedent but cannot participate in the current constraint's operation.
% DISAPPEARANCE_RATIONALE: If the provisional reading vanished, the binding multilateral or graduated compliance readings would govern — Iran's compliance would purchase durable relief, withdrawal would require consensus or material breach findings, and the nonproliferation regime's credibility would recover. The mobile phone market analogy: rival payment networks enter, prices shift, the operator loses its exclusivity rent.
% FOUNDING_PROBLEM: The 2015 JCPOA was built to solve the immediate crisis: Iran's advancing nuclear breakout capacity versus the West's unwillingness to accept a nuclear-armed Iran or launch preventive war. The founding problem was a binary crisis aversion, not a permanent security architecture.
% FOUNDING_PROBLEM_CORROBORATION: The Obama administration attested the problem was live and the deal solved it. The Trump administration attested the problem was misdiagnosed — the deal empowered Iranian regional malignity, making the founding problem 'dead' in the sense of superseded by a broader threat assessment. IAEA Director General reports (2016-2018) corroborated verified compliance; subsequent US intelligence assessments corroborated the deal's nonproliferation value but noted its sunset clauses. No single outside authority corroborates either status definitively.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the asymmetry: Iran's compliance costs were sunk and irreversible; the withdrawing state's sanctions relief was reversible at will. Suppression (0.55) is moderate — the constraint suppresses the binding multilateral reading's legal force but does not eliminate it; other parties continue operating under it. Theater ratio (0.42) rose sharply after 2018: the withdrawing state performs 'maximum pressure' diplomacy while the non-withdrawing parties perform 'preserving the deal' diplomacy — both increasingly ritualized. Accessibility collapse (0.48) is moderate: the binding and graduated readings remain live alternatives. Resistance (0.72) is high: Iran, EU, Russia, China, and international legal opinion actively contest the provisional reading's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the withdrawing state's seat, the constraint is a rope (genuine coordination with exit optionality preserving sovereignty). From Iran's seat, it is a snare (compliance extracted, relief revoked, no recourse). From non-withdrawing parties' seats, it is a tangled rope (coordination function degraded by extraction they cannot control). The engine computes this divergence from the structural data — the claimed_type 'tangled_rope' reflects the analyst's structural judgment that both functions coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   The withdrawing state sits at d ≈ 0.15 (beneficiary end): it collects sovereign optionality and domestic political capital, controls the 'bad faith' determination, and operates the sanctions enforcement machinery. Iran sits at d ≈ 0.9 (target end): it bears sunk compliance costs, faces reimposed sanctions on another's judgment, and has no exit that avoids extraction. Non-withdrawing parties sit at d ≈ 0.55 (moderate target): they lose economic relations and diplomatic credibility but retain some coordination benefit. Domestic opposition coalitions sit at d ≈ 0.1 (strong beneficiary): they gain policy vindication without bearing international costs. The nonproliferation regime (non-agent) sits at d ≈ 0.8: it bears structural credibility erosion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (binary crisis aversion) is contested as live/dead. If dead, the arrangement persists as mandate without function — a mandatrophy candidate. But the nonproliferation coordination function remains live (Iran's breakout time still matters), so the constraint is not purely atrophied. The provisional reading's extraction (unilateral exit) is not inertial — it is actively maintained by domestic coalitions. This is not piton; it is active contested extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the transactional provisional reading a distinct constraint from the binding multilateral and graduated compliance readings, or merely a rhetorical posture toward the same constraint?',
    'Test ε-invariance: if measuring ''JCPOA bindingness'' under the provisional reading yields ε ≈ 0.68 (asymmetric extraction) while the binding reading yields ε ≈ 0.15 (negligible extraction), they are distinct constraints per DP-001. The different beneficiary/victim structures confirm distinctness.',
    'If distinct, each reading gets its own constraint story with its own classification. If same constraint, the ε variance would indicate measurement error or observable-dependence — violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate structurally distinct constraints with invariant ε values.').

omega_variable(
    bad_faith_determination_mechanism,
    'What constitutes ''bad faith'' under this reading, and who authoritatively determines it?',
    'Analyze the withdrawing state''s 2018 justification: cited Iranian ballistic missile program, regional proxy activities, and sunset clauses — none of which were JCPOA violations per IAEA. If ''bad faith'' is defined extra-textually by the withdrawing state alone, the constraint''s extraction is unbounded.',
    'If bad faith determination is purely unilateral and extra-textual, the constraint is a snare (pure extraction under coordination cover). If bounded by treaty text or multilateral assessment, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bad_faith_determination_mechanism, conceptual, 'Whether the ''bad faith'' trigger is structurally bounded or an open-ended extraction license.').

omega_variable(
    committer_structure_uncertainty,
    'Does the kernel structure (three readings of one treaty) represent a genuine commitment-system dynamic, or a post-hoc analytical decomposition?',
    'Trace whether the three readings map to distinct institutional positions with authoritative interpreters (US State Department legal adviser vs. EU High Representative vs. IAEA Director General) or to academic taxonomies.',
    'If genuine CS dynamic, cs_structure fields (authority_grounding, reading_relations, axioms) are structurally warranted. If analytical only, the CS block over-claims institutional reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_uncertainty, empirical, 'Whether the kernel/reading decomposition reflects actual institutional commitment structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(jcpo_tr_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2019, 0.5).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement(jcpo_tr_t2025, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2015, 0.25).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2017, 0.3).
narrative_ontology:measurement(jcpo_be_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2019, 0.72).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2023, 0.7).
narrative_ontology:measurement(jcpo_be_t2025, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2017, 0.35).
narrative_ontology:measurement(jcpo_su_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2021, 0.52).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement(jcpo_su_t2025, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, npt_credibility).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_authority).

% DUAL FORMULATION NOTE:
% This story is one of three in the jcpoa_treaty_bindingness constraint family. The transactional provisional reading (this story) has ε ≈ 0.68 and claimed_type tangled_rope. The binding multilateral reading has ε ≈ 0.15 and claimed_type mountain/rope. The graduated compliance reading has ε ≈ 0.35 and claimed_type rope/tangled_rope. The ε spread confirms distinct constraints per the ε-invariance principle. This reading forecloses the binding reading and influences the graduated reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
