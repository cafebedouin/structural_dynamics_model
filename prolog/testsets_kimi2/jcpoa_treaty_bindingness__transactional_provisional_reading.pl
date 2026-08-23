% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Determination of Bad Faith
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This constraint story models the Joint Comprehensive Plan of Action
 *   (JCPOA) under the transactional provisional reading: the agreement is
 *   treated not as a binding multilateral treaty but as a reversible
 *   transaction in which one party's compliance obligations are fixed while
 *   the other's commitments remain contingent on a unilateral domestic
 *   determination of bad faith. The kernel is the legal and political status
 *   of the JCPOA itself; this reading instantiates the low-bindingness,
 *   high-voidability interpretation against sibling readings that treat the
 *   text as multilaterally binding or as a graduated compliance scale.
 *
 * KEY AGENTS:
 *   - sovereignty_asserting_state (institutional/arbitrage): Primary beneficiary â holds unilateral void option and captures strategic flexibility.
 *   - domestic_deal_opposition (organized/mobile): Secondary beneficiary â extracts domestic political capital from provisional framing.
 *   - iranian_state (institutional/trapped): Primary target â bears sunk compliance costs with no reciprocal bindingness.
 *   - european_compliance_parties (institutional/constrained): Secondary target â trade normalization investments are hostage to unilateral extraterritorial enforcement.
 *   - iaea_verification_body (institutional/constrained): Analytical observer â technical findings overridden by political determinations.
 *   - multilateral_security_framework (institutional/analytical): Excluded seat â multilateral dispute architecture bypassed by national determination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.8).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.86).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Determination of Bad Faith").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '8bcb99f3-5687-4f1e-861a-1156ebd14f9d').
narrative_ontology:cs_kernel_codification('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', formalized).
narrative_ontology:cs_authority_grounding('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', extraction).
narrative_ontology:cs_reading_relation('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', foundational, unilateral_voidability_as_sovereign_right).
narrative_ontology:cs_axiom_status(unilateral_voidability_as_sovereign_right, holdable).
narrative_ontology:cs_axiom_grounding('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', unilateral_voidability_as_sovereign_right, conventional).
narrative_ontology:cs_axiom('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', foundational, compliance_reciprocity_conditional).
narrative_ontology:cs_axiom_status(compliance_reciprocity_conditional, holdable).
narrative_ontology:cs_axiom_grounding('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', compliance_reciprocity_conditional, instrumental).
narrative_ontology:cs_reference_frame('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', provisional_sovereignty_framework).
narrative_ontology:cs_drift_state('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', post_unilateral_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8bcb99f3-5687-4f1e-861a-1156ebd14f9d', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, sovereignty_asserting_state).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_deal_opposition).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, european_compliance_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims unilateral authority to determine Iranian bad faith and to reimpose sanctions without multilateral consent, preserving maximum strategic flexibility and domestic political optionality while Iran remains bound by dismantlement and inspection commitments.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, sovereignty_asserting_state, beneficiary,
    institutional, biographical, arbitrage, global).

% Gains political capital from the deal's provisional framing, using threat of withdrawal to demand tougher terms while avoiding the domestic ratification costs and political lock-in of a binding treaty.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_deal_opposition, beneficiary,
    organized, biographical, mobile, national).

% Must dismantle centrifuges, accept intrusive inspections, and forego nuclear enrichment; compliance investments are sunk and slowly reversible, while counterparty commitments remain retractable upon a unilateral political determination made abroad.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state, payer,
    institutional, generational, trapped, national).

% Invested in trade normalization and structured their sanctions frameworks around the JCPOA; suffer secondary sanctions and strategic exclusion when the unilateral state voids the arrangement, with limited ability to shield their firms or override the extraterritorial enforcement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, european_compliance_parties, payer,
    institutional, biographical, constrained, continental).

% Provides technical verification of Iranian compliance, but its findings are subordinated to unilateral political determinations of bad faith that can override technical assessments and trigger sanctions snapback.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_body, observer,
    institutional, biographical, constrained, global).

% The UN Security Council endorsement in UNSCR 2231 is treated as non-binding political guidance under this reading, excluding the Council's multilateral dispute architecture from the sanctions-snapback mechanism in favor of national determination.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_security_framework, excluded,
    institutional, generational, analytical, global).

narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents Iranian nuclear weaponization through verified limits on enrichment, centrifuge numbers, and stockpiles in exchange for sanctions relief, creating a transactional quid pro quo between the P5+1 and Iran.
% TRANSFER_FUNCTION: Moves Iranian nuclear capacity offline and transfers sanctions relief (and its unilateral revocation) between the international community and Iran; specifically, transfers the costs of irreversible compliance investments from Iran to the reversible political commitments of the counterparty.
% ABSENT_VOICES: Multilateral dispute resolution bodies and the IAEA's technical assessment are sidelined when unilateral political determinations override their findings; Iranian domestic constituencies benefiting from economic opening are structurally excluded from the withdrawing state's domestic political process that triggers voidability.
% DISAPPEARANCE_RATIONALE: Iranian nuclear infrastructure expansion would resume without the inspection and limitation framework; European and Asian trade with Iran would reorganize without the threat of secondary sanctions tied to unilateral determinations; the regional non-proliferation order would shift to bilateral deterrence or military contingency planning.
% FOUNDING_PROBLEM: Iran's advancing uranium enrichment program threatened to reach weapons-grade capability, triggering potential military strikes by Israel or the United States and a regional nuclear arms race.
% FOUNDING_PROBLEM_CORROBORATION: Non-proliferation experts, pre-2015 IAEA reports, and regional intelligence services outside the direct beneficiary set of US domestic opposition attested the urgency of Iran's breakout timeline. Iranian officials acknowledged the sanctions pressure motivating negotiation. The 'provisional transactional' framing itself is corroborated primarily by the withdrawing state's legal arguments and domestic political platforms, while multilateral parties (EU, Russia, China) dispute that framing.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.8, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.80) is high because Iran's dismantlement and inspection acceptance are effectively irreversible on a generational horizon, while sanctions relief and trade access are reversible upon unilateral political will. Suppression (0.86) is higher still because the constraint's persistence after unilateral withdrawal depends on active secondary-sanctions enforcement against foreign firms, not on Iranian consent. Theater ratio (0.52) reflects that the original quid-pro-quo coordination was genuine but increasingly performative after 2018 as the 'transactional' framing revealed itself to be a unilateral leverage mechanism. Accessibility collapse (0.85) captures how Iran's alternative paths to nuclear capability and economic normalization collapsed as it accepted the framework; resistance (0.72) reflects Iranian progressive non-compliance post-2018 and European legal resistance to extraterritorial sanctions.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty-asserting state and its domestic opposition experience this constraint as a flexible instrument of statecraft that preserves option value. The Iranian state experiences it as a binding ceiling on sovereign nuclear activity with no corresponding binding floor on economic relief. The European parties experience it as a reputational and commercial trap. These divergences are structural, not perspectival illusions: the same text produces opposite directionalities because exit options are asymmetrically distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (sovereignty_asserting_state, domestic_deal_opposition) have low directionality because the constraint subsidizes their strategic and political flexibility; they can exit without cost while Iran cannot easily reverse compliance. Victims (iranian_state, european_compliance_parties) have high directionality because they bear the sunk costs and secondary sanctions respectively, with trapped or constrained exit options. The IAEA sits near symmetric but is politically subordinate; the multilateral framework is analytically excluded.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids snare classification because the coordination function is real and historically active: Iranian enrichment was verifiably capped and IAEA inspections functioned. It avoids rope classification because the coordination is not symmetric; one party can unilaterally void the transaction while the other has already delivered irreversible performance. The tangled_rope classification captures the hybrid: genuine non-proliferation coordination layered with asymmetric extraction through unilateral voidability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_determination_legitimacy,
    'Is unilateral determination of bad faith a legally valid interpretation of the JCPOA text and UNSCR 2231, or a political construction post-dating the agreement?',
    'Textual legal analysis of the JCPOA''s silence on unilateral exit mechanisms and the non-binding character of UNSCR 2231 endorsement, compared to Vienna Convention on the Law of Treaties requirements for treaty withdrawal.',
    'If the unilateral reading is textually illegitimate, the constraint is an extraction layer retroactively imposed on a multilateral commitment; if legitimate, the provisional framing was built into the architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_determination_legitimacy, conceptual, 'Legitimacy of unilateral voidability under international law').

omega_variable(
    compliance_asymmetry_irreversibility,
    'To what extent are Iran''s compliance steps (dismantling centrifuges, exporting enriched uranium, accepting the Additional Protocol) reversible, and does this irreversibility constitute structural extraction?',
    'Technical assessment of nuclear restart timelines and facility reconstruction costs, combined with economic analysis of foregone sanctions-relief opportunity costs.',
    'High irreversibility on a generational horizon confirms the asymmetric extraction; rapid reversibility would undermine the victim narrative and reduce effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_asymmetry_irreversibility, empirical, 'Reversibility of Iranian compliance investments').

omega_variable(
    multilateral_vs_unilateral_frame,
    'Does the provisional transactional reading serve the specific domestic political interests of the withdrawing state''s opposition coalitions, or is it an inherent feature of great-power non-proliferation diplomacy?',
    'Comparative analysis of other non-proliferation agreements (NPT, Agreed Framework) to determine whether unilateral voidability without multilateral review is standard or anomalous.',
    'If unique to domestic politics, extraction is higher and the coordination story is more heavily cover; if standard great-power flexibility, the constraint sits closer to conventional enforcement asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_vs_unilateral_frame, conceptual, 'Whether unilateral voidability is structurally anomalous').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(jcpo_tr_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 7, 0.6).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement(jcpo_be_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 7, 0.78).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 10, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(jcpo_su_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 7, 0.83).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 10, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jcpoa_treaty_bindingness kernel, which decomposes the colloquial label 'JCPOA' into three structurally distinct commitments. The transactional provisional reading has high extractiveness and unilateral voidability; the binding multilateral reading has low extractiveness and consensus requirements; the graduated compliance reading has proportional enforcement and medium extractiveness. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
