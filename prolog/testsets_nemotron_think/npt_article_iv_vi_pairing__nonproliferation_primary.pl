% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing — Nonproliferation-Primary Reading
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint story captures the 'nonproliferation-primary' reading of
 *   the NPT Article IV/VI pairing — the interpretation that has operationally
 *   governed the regime since 1970. Under this reading, Article IV's
 *   'inalienable right' to peaceful nuclear energy is conditional on Article
 *   III safeguards compliance; Article VI's disarmament obligation is
 *   aspirational, non-justiciable, and lacks an enforcement mechanism; the
 *   treaty's authority derives from the five weapon states' shared security
 *   interest in preventing horizontal proliferation to additional states. The
 *   result is a permanently stabilized two-tier order: weapon states retain
 *   arsenals indefinitely while non-weapon states bear permanent verification
 *   costs and technology restrictions. The constraint has genuine
 *   coordination value (horizontal non-proliferation has largely held) fused
 *   with asymmetric extraction (the grand bargain's reciprocal disarmament
 *   has not). Active enforcement through IAEA safeguards, NSG export
 *   controls, and UNSC resolutions maintains the structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.75).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.7).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing — Nonproliferation-Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '7ed6b964-299f-4487-bbd8-02ad57afde38').
narrative_ontology:cs_kernel_codification('7ed6b964-299f-4487-bbd8-02ad57afde38', formalized).
narrative_ontology:cs_authority_grounding('7ed6b964-299f-4487-bbd8-02ad57afde38', extraction).
narrative_ontology:cs_interpretation_layer_present('7ed6b964-299f-4487-bbd8-02ad57afde38').
narrative_ontology:cs_reading_relation('7ed6b964-299f-4487-bbd8-02ad57afde38', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('7ed6b964-299f-4487-bbd8-02ad57afde38', npt_article_iv_vi_pairing__abolitionist, forecloses).
narrative_ontology:cs_axiom('7ed6b964-299f-4487-bbd8-02ad57afde38', foundational, horizontal_proliferation_prevention_primary).
narrative_ontology:cs_axiom_status(horizontal_proliferation_prevention_primary, holdable).
narrative_ontology:cs_axiom_grounding('7ed6b964-299f-4487-bbd8-02ad57afde38', horizontal_proliferation_prevention_primary, empirically_contingent).
narrative_ontology:cs_axiom('7ed6b964-299f-4487-bbd8-02ad57afde38', foundational, article_vi_aspirational_non_justiciable).
narrative_ontology:cs_axiom_status(article_vi_aspirational_non_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('7ed6b964-299f-4487-bbd8-02ad57afde38', article_vi_aspirational_non_justiciable, conventional).
narrative_ontology:cs_axiom('7ed6b964-299f-4487-bbd8-02ad57afde38', secondary, weapon_state_arsenals_excluded_from_enforcement).
narrative_ontology:cs_axiom_status(weapon_state_arsenals_excluded_from_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('7ed6b964-299f-4487-bbd8-02ad57afde38', weapon_state_arsenals_excluded_from_enforcement, conventional).
narrative_ontology:cs_reference_frame('7ed6b964-299f-4487-bbd8-02ad57afde38', nonproliferation_primary_order).
narrative_ontology:cs_drift_state('7ed6b964-299f-4487-bbd8-02ad57afde38', contemporary_review_conference_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7ed6b964-299f-4487-bbd8-02ad57afde38', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_bureaucracy).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_aligned_movement_states).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, nonproliferation_as_primary_obligation).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, article_vi_aspirational_non_justiciable).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, horizontal_proliferation_prevention_as_security_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized nuclear-weapon states (US, Russia, UK, France, China) set the interpretation of the treaty through their Security Council veto power and control of the nuclear fuel cycle. They retain their arsenals indefinitely under this reading, face no enforceable disarmament timeline, and benefit from the non-proliferation regime constraining competitors. Their exit option is trivial — they are the enforcement authority.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states, agenda_setter,
    powerful, generational, arbitrage, global).

% 185+ non-nuclear-weapon states parties that have permanently foregone the nuclear weapons option in exchange for Article IV technology access conditional on Article III safeguards. They bear the costs of intrusive verification, export control restrictions, and the opportunity cost of nuclear latency. Exit (withdrawal under Article X) invites severe political and economic sanctions, making exit structurally constrained rather than free.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states, payer,
    moderate, biographical, constrained, global).

% 48 nuclear supplier states that coordinate export controls through NSG guidelines, capturing the commercial benefits of nuclear technology transfer while gatekeeping access based on safeguards compliance. They benefit from the regime's restriction of sensitive technology to compliant states, which reduces proliferation risk to their own security and preserves market position.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group, beneficiary,
    organized, generational, mobile, global).

% The IAEA Secretariat and safeguards inspectorate that administers the verification regime. They derive institutional mission, budget, and authority from the ongoing verification demand. Their interests align with maintaining and expanding the safeguards system regardless of Article VI progress, creating an institutional beneficiary of the nonproliferation-primary interpretation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_bureaucracy, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_bureaucracy, beneficiary).

% The core NAM cohort (Indonesia, Egypt, South Africa, Brazil, Mexico, etc.) that negotiated the NPT expecting reciprocal disarmament. They experience the constraint as extraction — bearing permanent verification costs while weapon states avoid Article VI implementation. Their collective voice is structurally excluded from treaty amendment (requires majority including all NWS) and review conference consensus blocks.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_aligned_movement_states, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, non_aligned_movement_states, excluded).

% Civil society networks (ICAN, IPPNW, Article 36) and TPNW supporter states that argue Article VI creates a legal obligation to disarm. They are excluded from NPT decision-making forums and their legal interpretation is treated as non-justiciable under this reading. Their exit option is building an alternative treaty (TPNW) — mobile but institutionally separate.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, humanitarian_law_advocates, excluded,
    organized, biographical, mobile, global).

% The structural observer seat that sees the full two-tier architecture: a genuine non-proliferation coordination function that prevents horizontal spread, fused with an asymmetric extraction where non-weapon states pay permanent verification costs while weapon states retain arsenals without enforceable disarmament.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal nuclear proliferation through verified non-acquisition commitments from 185+ states, conditional technology transfer for peaceful uses, and a collective security framework that reduces the incentive for individual nuclear hedging.
% TRANSFER_FUNCTION: Moves the sovereign option to acquire nuclear weapons from non-weapon states to the collective security of weapon states; moves verified peaceful nuclear technology access to compliant non-weapon states; moves the costs of intrusive verification, export control compliance, and nuclear latency opportunity cost onto non-weapon states; moves the political benefit of non-proliferation leadership to weapon states and NSG members without requiring reciprocal disarmament.
% ABSENT_VOICES: Nuclear-armed states outside the NPT (India, Pakistan, Israel, North Korea) whose arsenals the regime does not constrain; future generations who inherit the two-tier order and its proliferation risks; populations affected by nuclear testing and uranium mining whose consent was never sought; the TPNW state parties whose treaty declares the NPT's disarmament obligation unfulfilled.
% DISAPPEARANCE_RATIONALE: If the NPT vanished overnight, the legal barrier to horizontal proliferation would collapse. Dozens of latency-capable states (Japan, South Korea, Germany, Brazil, Turkey, Saudi Arabia, UAE, etc.) would face acute pressure to hedge or acquire weapons. The IAEA safeguards system would lose its treaty mandate. The NSG export control regime would lose its legal foundation. Weapon states would lose the primary legal instrument constraining competitors. The global nuclear order would reorganize around uncontrolled proliferation or new coercive arrangements.
% FOUNDING_PROBLEM: Preventing horizontal nuclear proliferation after the 1964 Chinese test demonstrated that proliferation was no longer limited to superpowers; managing US-Soviet rivalry by freezing the number of nuclear-armed states; enabling peaceful nuclear energy development without enabling weapons programs.
% FOUNDING_PROBLEM_CORROBORATION: The historical record corroborates non-proliferation as the primary driver: the 1963 Gilpatric Committee report warned of 10-15 nuclear states by 1970s; the Eighteen Nations Disarmament Committee negotiations centered on non-acquisition; US and Soviet security interests aligned on preventing German, Japanese, and Chinese nuclear weapons. Weapon states' security interest in horizontal prevention is documented in negotiating history. Non-weapon states' dissent is recorded in NAM statements at every Review Conference since 1975 — the 'grand bargain' was never universally accepted as reciprocal, and the TPNW's adoption (2017) by 122 states confirms the contested status.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because non-weapon states permanently forego the weapons option and accept intrusive verification while weapon states face no enforceable disarmament timeline — the cost asymmetry is structural and permanent. Suppression (0.7) is high because the regime actively prevents exit (Article X withdrawal triggers sanctions), enforces compliance through coercive measures (Iran, Iraq, Libya, North Korea cases), and blocks amendment (requires NWS consent). Theater ratio (0.45) is moderate and rising: the non-proliferation coordination function is real (horizontal spread contained to 4 non-NPT states), but the disarmament obligation has become increasingly performative — Review Conferences produce 'action plans' that are not implemented, and the 2010 Action Plan's disarmament benchmarks were abandoned. Accessibility collapse (0.65) reflects that non-weapon states cannot practically exit the regime without severe consequences, and the two-tier structure is treated as immutable. Resistance (0.5) is moderate: NAM statements, TPNW adoption, and occasional Review Conference walkouts show dissent, but no coalition has forced structural change.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon state seat, the constraint is a successful Rope: it coordinates non-proliferation with minimal cost to them. From the non-weapon state seat, it is a Snare: permanent extraction with no exit. From the NAM seat, it is a broken Scaffold: the transition to disarmament never came. From the IAEA seat, it is a Rope that must be maintained and expanded. The engine computes these per-seat classifications from the structural data — this reading's claimed_type (tangled_rope) captures the coordination-extraction hybrid at the system level, while the seat divergence reveals the two-tier reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states (powerful, arbitrage exit) are structural beneficiaries: they collect the security benefit of constrained competitors while paying no disarmament cost — directionality d near 0.0 (beneficiary end). Non-weapon states (moderate, constrained exit) are structural targets: they pay verification costs, accept technology restrictions, and forego the weapons option permanently — directionality d near 1.0 (target end). NSG states (organized, mobile exit) are beneficiaries: they capture commercial rents from gated technology transfer. IAEA bureaucracy (institutional, analytical exit) is a dual-role agenda-setter/beneficiary: it administers the regime and derives mission/budget from its expansion. NAM states (organized, constrained exit) are payers who are also excluded from decision-making. Humanitarian advocates (organized, mobile exit) are excluded voices building an alternative framework (TPNW). The analytical observer sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing horizontal proliferation) remains live — proliferation risk persists. However, the reciprocal element (Article VI disarmament) has atrophied: the 'grand bargain' exchange has become one-sided. The nonproliferation-primary reading resolves the mandatrophy by declaring Article VI aspirational, converting a potentially extinct scaffold (reciprocal bargain) into a permanent tangled_rope where coordination persists without its original justification. This prevents mislabeling the regime as pure coordination (rope) or pure extraction (snare) — it is both, fused. The theater ratio rise tracks the disarmament obligation's conversion from live commitment to performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability,
    'Is Article VI truly non-justiciable, or do ICJ advisory opinions (1996 Legality of Nuclear Weapons, 2010 Kosovo) and customary law create enforceable disarmament obligations despite NPT text?',
    'Future ICJ contentious case or UNGA request for advisory opinion on Article VI implementation; state practice regarding NPT Article VI as customary law; TPNW''s legal effect on non-parties.',
    'If Article VI becomes justiciable, the constraint shifts from tangled_rope toward scaffold (with enforceable sunset) or snare (if weapon states refuse compliance). The extraction asymmetry would become legally contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Whether the disarmament obligation is legally enforceable or purely aspirational.').

omega_variable(
    two_tier_necessity,
    'Is the two-tier order (weapon states keep arsenals, non-weapon states permanently restrained) structurally necessary for non-proliferation, or is it a contingent political settlement that could be replaced by a universal prohibition regime?',
    'Counterfactual analysis: would a universal prohibition (TPNW model) achieve better non-proliferation outcomes? Historical analysis of whether NPT''s two-tier structure enabled or prevented proliferation in threshold states.',
    'If contingent, the extraction is not coordination-necessary but power-constructed — supporting snare classification. If necessary, the extraction is the price of coordination — supporting tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(two_tier_necessity, conceptual, 'Whether the two-tier structure is a necessary condition for non-proliferation coordination.').

omega_variable(
    permanent_restraint_vs_conditional,
    'Do non-weapon states'' Article II commitments constitute permanent renunciation, or are they implicitly conditional on Article VI progress (as the grand_bargain reading holds)?',
    'Vienna Convention on the Law of Treaties Article 60 (material breach) analysis; state practice regarding withdrawal justifications; NAM collective statements on Article VI as conditionality.',
    'If conditional, the constraint''s extraction is unstable — non-weapon states have a legal exit pathway. If permanent, the extraction is locked in — supporting tangled_rope/snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanent_restraint_vs_conditional, conceptual, 'Whether non-weapon state restraint is legally permanent or conditionally reciprocal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_iv_vi_nonprolif_tr_t0, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(npt_iv_vi_nonprolif_tr_t9, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 9, 0.3).
narrative_ontology:measurement(npt_iv_vi_nonprolif_tr_t18, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 18, 0.35).
narrative_ontology:measurement(npt_iv_vi_nonprolif_tr_t27, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 27, 0.38).
narrative_ontology:measurement(npt_iv_vi_nonprolif_tr_t36, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 36, 0.41).
narrative_ontology:measurement(npt_iv_vi_nonprolif_tr_t45, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 45, 0.43).
narrative_ontology:measurement(npt_iv_vi_nonprolif_tr_t54, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 54, 0.45).

% Extraction over time
narrative_ontology:measurement(npt_iv_vi_nonprolif_be_t0, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(npt_iv_vi_nonprolif_be_t9, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(npt_iv_vi_nonprolif_be_t18, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(npt_iv_vi_nonprolif_be_t27, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 27, 0.67).
narrative_ontology:measurement(npt_iv_vi_nonprolif_be_t36, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 36, 0.71).
narrative_ontology:measurement(npt_iv_vi_nonprolif_be_t45, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 45, 0.73).
narrative_ontology:measurement(npt_iv_vi_nonprolif_be_t54, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 54, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(npt_iv_vi_nonprolif_su_t0, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(npt_iv_vi_nonprolif_su_t9, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(npt_iv_vi_nonprolif_su_t18, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(npt_iv_vi_nonprolif_su_t27, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 27, 0.65).
narrative_ontology:measurement(npt_iv_vi_nonprolif_su_t36, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 36, 0.67).
narrative_ontology:measurement(npt_iv_vi_nonprolif_su_t45, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 45, 0.69).
narrative_ontology:measurement(npt_iv_vi_nonprolif_su_t54, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 54, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__nonproliferation_primary, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_vi_disarmament_obligation).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_system).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group_guidelines).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, tpnw_prohibition).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_x_withdrawal).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, unsc_resolution_1540).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_review_conference_process).

% DUAL FORMULATION NOTE:
% Part of NPT Article IV/VI kernel family (npt_article_iv_vi_pairing). This reading (nonproliferation_primary) treats Article IV as conditional on Article III and Article VI as aspirational/non-justiciable. The grand_bargain reading treats them as reciprocal obligations (non-weapon restraint conditional on weapon state disarmament). The abolitionist reading treats Article VI as mandatory disarmament and Article IV as illegitimate dual-use enabler. Each reading has distinct ε, beneficiary/victim structure, and claimed_type. This reading's ε=0.75 reflects high extraction on non-weapon states; grand_bargain reading would author lower ε (reciprocity reduces extraction); abolitionist reading would author ε for the standing arrangement as seen from humanitarian law (highest extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, powerful, 0.05).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, moderate, 0.85).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, organized, 0.75).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
