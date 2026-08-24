% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty Requiring Consensus-Based Modification
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA (2015) is a multilateral nuclear agreement between Iran and
 *   P5+1 (US, UK, France, Germany, Russia, China, EU) endorsed by UNSC
 *   Resolution 2231. This reading treats it as a binding treaty: modification
 *   or dissolution requires consensus; sanctions reimposition ('snapback')
 *   requires UNSC action; Iranian violations trigger the Joint Commission
 *   dispute resolution before any party can claim material breach. The US
 *   withdrawal (2018) and subsequent Iranian breaches (2019-) test whether
 *   the constraint holds without universal participation. Beneficiaries are
 *   the multilateral institutions (IAEA, UNSC) and the non-proliferation
 *   regime; victims are the parties bearing asymmetric compliance costs
 *   (Iran's nuclear concessions, E3's sanctions relief without full US
 *   cooperation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.45).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty Requiring Consensus-Based Modification").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '6b793dcd-9a7a-4590-a31f-d83c849e485b').
narrative_ontology:cs_kernel_codification('6b793dcd-9a7a-4590-a31f-d83c849e485b', formalized).
narrative_ontology:cs_authority_grounding('6b793dcd-9a7a-4590-a31f-d83c849e485b', lineage).
narrative_ontology:cs_interpretation_layer_present('6b793dcd-9a7a-4590-a31f-d83c849e485b').
narrative_ontology:cs_reading_relation('6b793dcd-9a7a-4590-a31f-d83c849e485b', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('6b793dcd-9a7a-4590-a31f-d83c849e485b', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('6b793dcd-9a7a-4590-a31f-d83c849e485b', foundational, treaty_binding_consensus_required).
narrative_ontology:cs_axiom_status(treaty_binding_consensus_required, holdable).
narrative_ontology:cs_axiom_grounding('6b793dcd-9a7a-4590-a31f-d83c849e485b', treaty_binding_consensus_required, conventional).
narrative_ontology:cs_axiom('6b793dcd-9a7a-4590-a31f-d83c849e485b', foundational, sanctions_snapback_requires_unsc_consensus).
narrative_ontology:cs_axiom_status(sanctions_snapback_requires_unsc_consensus, holdable).
narrative_ontology:cs_axiom_grounding('6b793dcd-9a7a-4590-a31f-d83c849e485b', sanctions_snapback_requires_unsc_consensus, conventional).
narrative_ontology:cs_reference_frame('6b793dcd-9a7a-4590-a31f-d83c849e485b', unsc_resolution_2231_framework).
narrative_ontology:cs_drift_state('6b793dcd-9a7a-4590-a31f-d83c849e485b', post_us_withdrawal_2018, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b793dcd-9a7a-4590-a31f-d83c849e485b', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_plus_2).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_plus_2).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, russia).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, china).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, russia).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, china).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_treaty_law).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_resolution_2231_authority).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, nuclear_nonproliferation_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accepts comprehensive nuclear limitations and intrusive IAEA verification in exchange for sanctions relief. Bears high compliance costs (centrifuge limits, monitoring) but gains economic relief. Exit is constrained by desire for sanctions relief and international legitimacy; unilateral withdrawal triggers snapback.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, beneficiary).

% European parties (France, Germany, UK, EU) provide sanctions relief and economic cooperation. Benefit from non-proliferation assurance and regional stability. Constrained by US secondary sanctions and domestic politics; cannot fully deliver promised benefits without US cooperation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_plus_2, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_plus_2, beneficiary).

% Gains from non-proliferation regime stability and sanctioned trade channels (e.g., Fordow conversion). Provides technical cooperation and diplomatic cover. Mobile exit: can leverage veto power in UNSC and bilateral ties with Iran.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, russia, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, russia, payer).

% Benefits from Middle East stability and energy security; maintains oil trade with Iran. Provides economic lifeline and diplomatic support. Mobile exit: deep strategic partnership with Iran and UNSC veto.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, china, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, china, payer).

% Primary sanctions authority; obliged to waive nuclear-related sanctions under treaty. Withdrew in 2018 and reimposed sanctions, violating consensus requirement. Arbitrage exit: can unilaterally exit due to global financial dominance, but faces allied opposition and credibility costs.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states, payer,
    institutional, biographical, arbitrage, global).

% Gains unprecedented verification authority (Additional Protocol, continuous monitoring). Institutional mandate and budget reinforced. Analytical exit: cannot exit its statutory role; legitimacy tied to impartial verification.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea, beneficiary,
    institutional, generational, analytical, global).

% Resolution 2231 enshrines JCPOA; snapback mechanism gives UNSC central enforcement role. Authority strengthened if consensus holds; eroded if unilateral actions bypass it. Analytical exit: structural role cannot be exited.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council, beneficiary,
    institutional, generational, analytical, global).

% Israel, Saudi Arabia, UAE — oppose JCPOA as insufficiently restrictive. Excluded from negotiations; would demand tighter limits and regional missile constraints. Trapped: cannot join treaty, must lobby external powers or act unilaterally (e.g., sabotage, diplomatic pressure).
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_adversaries, excluded,
    powerful, biographical, trapped, regional).

% Analyze treaty interpretation, state responsibility, and snapback legality. Provide epistemic foundation for binding vs. provisional readings. No material stake; exit is analytical disengagement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates multilateral non-proliferation assurance and sanctions relief through a single verified agreement, replacing ad hoc pressure with a structured reciprocal commitment monitored by IAEA and guaranteed by UNSC Resolution 2231.
% TRANSFER_FUNCTION: Moves nuclear concessions (enrichment limits, centrifuge reductions, monitoring access) from Iran to the international community; moves sanctions relief (oil, banking, trade) from P5+1 to Iran. Transfer is phased and conditional on verified compliance.
% ABSENT_VOICES: Regional adversaries (Israel, Gulf states) were excluded from negotiations and dispute resolution. Their security concerns (missiles, regional proxies, sunset clauses) are not addressed in the treaty structure. Iranian civil society and domestic political opponents of the deal were not represented.
% DISAPPEARANCE_RATIONALE: If the binding multilateral treaty vanished overnight, Iran would immediately expand enrichment; US and EU sanctions would snap back or be reimposed unilaterally; IAEA would lose enhanced access; UNSC Resolution 2231 would become inoperative; regional tension would escalate toward military confrontation. The entire non-proliferation architecture for Iran would reorganize around coercion rather than verified agreement.
% FOUNDING_PROBLEM: The Iranian nuclear program had expanded to near-weapons capability (20% enrichment, thousands of centrifuges) without adequate international verification. The founding problem was preventing Iranian nuclear breakout while avoiding military conflict, through a verifiable diplomatic agreement that all P5+1 could endorse.
% FOUNDING_PROBLEM_CORROBORATION: IAEA Director General reports (2015-2018) corroborate that Iran met nuclear commitments. US intelligence community assessments (2018, 2023) corroborate Iran was not pursuing a weapon at signing. However, Israeli intelligence (2018 archive seizure) and US withdrawal rationale claim the program retained weaponization knowledge. The founding problem (breakout prevention) is live per IAEA/E3; dead per US withdrawal argument; contested per Iranian advances post-2018.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects mutual but asymmetric concessions: Iran gives up irreversible nuclear infrastructure; others give reversible sanctions relief. Suppression (0.55) is moderate: the treaty constrains unilateral action but provides exit via dispute resolution; US withdrawal shows suppression is not absolute. Theater ratio (0.22) is low during implementation (2016-2018) but rises as compliance becomes performative (Iran's incremental breaches, E3's INSTEX ineffectiveness). Accessibility collapse (0.62) is high: once the treaty exists, alternatives (military strike, maximum pressure) are politically costly. Resistance (0.68) is high: domestic opponents in US, Iran, and regional states actively undermine the treaty.
 *
 * PERSPECTIVAL GAP:
 *   From Iran's seat, the treaty is a snare if sanctions relief is not delivered (extraction without benefit). From E3 seat, it is a tangled rope (coordination + asymmetric cost). From IAEA/UNSC seat, it is a rope (pure coordination). From US seat (post-withdrawal), it is a snare they escaped. The engine computes per-seat types from these structural differences.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran is primary payer (nuclear concessions) but also beneficiary (sanctions relief) — dual role. E3/EU are payers (sanctions relief) and beneficiaries (non-proliferation). Russia/China are net beneficiaries (trade, regime stability) with low costs. US is payer (sanctions waiver) but exited, gaining arbitrage exit. IAEA/UNSC are institutional beneficiaries (authority, mandate). Regional adversaries are excluded payers (bear risk without voice). Directionality derived from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The treaty's mandate (prevent breakout) remains live per IAEA verification, but the arrangement persists without full US participation. The mandate has not atrophied — the non-proliferation problem persists — but the consensus enforcement mechanism has degraded. This is not mandatrophy (function persists) but a coordination mechanism operating with a missing party. The binding reading insists the treaty remains legally valid; the provisional reading argues US withdrawal voided it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the JCPOA text and UNSC Resolution 2231 legally require consensus for modification, or does it permit unilateral withdrawal upon material breach determination?',
    'Authoritative interpretation by ICJ or UNSC consensus; state practice of remaining parties; Vienna Convention on the Law of Treaties analysis.',
    'If consensus required, US withdrawal is a violation and snapback is illegal; if unilateral withdrawal permitted, the treaty is provisional and Iran''s countermeasures are lawful. Changes classification from tangled_rope (binding) to snare (provisional extraction) or rope (graduated).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Core legal ambiguity: binding treaty vs. provisional political commitment.').

omega_variable(
    snapback_legitimacy,
    'Can a non-participating state (US post-2018) trigger UNSC snapback under Resolution 2231?',
    'UNSC procedural vote; legal opinions from UN Office of Legal Affairs; state practice of snapback invocation attempts (2020).',
    'If snapback requires participating state status, US invocation is void and the constraint''s enforcement mechanism is degraded. If any UNSC member can invoke, the constraint''s suppression mechanism remains active but asymmetrically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snapback_legitimacy, empirical, 'Whether the snapback mechanism survives US withdrawal.').

omega_variable(
    iran_compliance_trajectory,
    'Are Iranian nuclear advancements post-2018 (60% enrichment, metal production) reversible technical steps or irreversible breakout preparation?',
    'IAEA verification reports; technical assessment of breakout time; Iranian statements on reversibility.',
    'If reversible, the treaty''s coordination function survives; if irreversible, the treaty''s foundational problem (breakout prevention) has failed, shifting classification toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iran_compliance_trajectory, empirical, 'Reversibility of Iranian nuclear advancements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_bmr_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jcpoa_bmr_tr_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement(jcpoa_bmr_tr_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(jcpoa_bmr_tr_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(jcpoa_bmr_tr_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(jcpoa_bmr_tr_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 10, 0.22).

% Extraction over time
narrative_ontology:measurement(jcpoa_bmr_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jcpoa_bmr_be_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(jcpoa_bmr_be_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(jcpoa_bmr_be_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(jcpoa_bmr_be_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(jcpoa_bmr_be_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 10, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jcpoa_treaty_bindingness__binding_multilateral_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_dispute_resolution).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_resolution_2231_snapback).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_nuclear_program).

% DUAL FORMULATION NOTE:
% Part of JCPOA treaty bindingness kernel family: binding_multilateral_reading (this), transactional_provisional_reading, graduated_compliance_reading. This reading emphasizes consensus modification and UNSC snapback; transactional emphasizes unilateral voidability; graduated emphasizes proportional reciprocity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__binding_multilateral_reading, institutional, 0.35).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__binding_multilateral_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
