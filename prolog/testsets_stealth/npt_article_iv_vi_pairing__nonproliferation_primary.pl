% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing — Nonproliferation-Primary Reading (Verification-Gated Sharing over a Stabilized Two-Tier Order)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   Under the nonproliferation-primary reading, the treaty operates as a
 *   verification-gated technology-sharing arrangement layered over a
 *   permanent two-tier security order. Article IV's promise of peaceful
 *   nuclear cooperation is honored only through Article III's full-scope
 *   safeguards; Article VI's disarmament language is treated as hortatory —
 *   an obligation of conduct without justiciable content, timetable, or
 *   remedy; and the regime's operative authority flows from the weapon
 *   states' security interest in capping the number of new entrants while
 *   their own arsenals stand outside the enforcement machinery. The
 *   arrangement solves a real collective problem (a proliferation cascade
 *   among dozens of latent-capable states) and simultaneously locks in a
 *   one-way burden: non-weapon states restrain themselves perpetually and
 *   submit to inspection, weapon states keep their arsenals and administer
 *   the rules. This file is one member of a three-story constraint family
 *   decomposing the colloquial label 'the NPT bargain': sibling files
 *   instantiate the grand_bargain reading (reciprocal enforceable
 *   obligations) and the abolitionist reading (binding disarmament mandate
 *   grounded in humanitarian law), each with its own epsilon over the same
 *   referent; the family is linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states_p5: agenda-setter and principal beneficiary (institutional/arbitrage) — arsenals exempt from enforcement, permanent Security Council vetoes, control interpretation and pacing
 *   - non_weapon_state_parties: primary target and payer (organized/constrained) — perpetual restraint-bearers with inspection obligations and no enforcement lever on reciprocity
 *   - iaea_verification_system: administering enforcer (institutional/constrained) — runs the inspection regime, funded and steered by the states it polices indirectly
 *   - threshold_armed_states_outside_treaty: excluded outsiders (powerful/arbitrage) — India, Pakistan, Israel; shape the regime's perimeter without bearing its obligations
 *   - sanctioned_withdrawal_states: punished exit-seekers (moderate/trapped) — the DPRK precedent prices exit for everyone else
 *   - peaceful_nuclear_commerce_sector: secondary beneficiary (organized/mobile) — reactor vendors and fuel-cycle firms trading under Article IV protection
 *   - tpnw_humanitarian_coalition: excluded challenger bloc (organized/mobile) — builds its remedy outside the review-conference room
 *   - arms_control_jurists: analytical observer (analytical/analytical) — debate the text's legal content with no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.62).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing — Nonproliferation-Primary Reading (Verification-Gated Sharing over a Stabilized Two-Tier Order)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '9ccfc73b-f8de-40d2-aa43-50cc6236f0b1').
narrative_ontology:cs_kernel_codification('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', fixed_text).
narrative_ontology:cs_authority_grounding('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', extraction).
narrative_ontology:cs_interpretation_layer_present('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1').
narrative_ontology:cs_reading_relation('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_reading_relation('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', npt_article_iv_vi_pairing__abolitionist, forecloses).
narrative_ontology:cs_axiom('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', foundational, horizontal_proliferation_paramount_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_paramount_threat, holdable).
narrative_ontology:cs_axiom_grounding('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', horizontal_proliferation_paramount_threat, empirically_contingent).
narrative_ontology:cs_axiom('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', foundational, article_vi_aspirational_not_justiciable).
narrative_ontology:cs_axiom_status(article_vi_aspirational_not_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', article_vi_aspirational_not_justiciable, conventional).
narrative_ontology:cs_axiom('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', secondary, safeguards_precondition_for_peaceful_use).
narrative_ontology:cs_axiom_status(safeguards_precondition_for_peaceful_use, holdable).
narrative_ontology:cs_axiom_grounding('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', safeguards_precondition_for_peaceful_use, conventional).
narrative_ontology:cs_reference_frame('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', p5_monopoly_nonproliferation_order).
narrative_ontology:cs_drift_state('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', contemporary_tpnw_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9ccfc73b-f8de-40d2-aa43-50cc6236f0b1', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states_p5).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, peaceful_nuclear_commerce_sector).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_system).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, sanctioned_withdrawal_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states that tested weapons before 1967 and were grandfathered into the treaty's definition of a weapon state. They retain and modernize their arsenals outside the treaty's verification machinery, sit as permanent veto members of the Security Council that polices everyone else's programs, fund and steer the inspection agency's priorities, and control the pacing of disarmament diplomacy at five-year review conferences. Leaving the arrangement would cost them the legitimacy shield it provides for their own arsenals, so they stay and manage it instead.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% Roughly 185 states that joined without weapons. They accept inspections of all their nuclear facilities, forgo building arsenals indefinitely, and absorb the industrial costs of hosting inspectors and segregating civilian from military fuel cycles. In return they receive access to peaceful nuclear technology, conditional on continuous verified compliance, plus non-binding security assurances. They press disarmament language at every review cycle with no procedural lever to compel it; formal withdrawal remains open, but the North Korean precedent shows that exit brings sanctions and isolation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties, beneficiary).

% The Vienna-based agency that runs the inspection regime: materials accounting, facility design review, and complementary access under the Additional Protocol. Its findings feed the Board of Governors and the Security Council, where enforcement decisions are made by others. Its budget, inspector access, and political protection depend on the major powers; its non-compliance reports trigger sanction debates it does not control.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_system, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_system, beneficiary).

% India, Pakistan, and Israel — states that acquired weapons without joining and were never subjected to the treaty's machinery. They argue the accord freezes an unfair hierarchy and decline accession on those terms. Their capabilities are accommodated selectively: India negotiated a civil-nuclear waiver in 2008 despite testing after 1970, while supplier states deny equivalent technology to treaty members suspected of intent. They shape the regime's perimeter from outside without bearing its obligations.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, threshold_armed_states_outside_treaty, excluded,
    powerful, generational, arbitrage, regional).

% States that left or sought to leave, principally North Korea, which announced withdrawal in 2003 and tested thereafter. They bear comprehensive sanctions, supply cutoffs, and interdiction efforts. Re-entry on acceptable terms has proven elusive; the episode stands as the regime's standing demonstration of what exit costs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, sanctioned_withdrawal_states, payer,
    moderate, biographical, trapped, regional).

% Reactor vendors, fuel-cycle firms, isotope producers, and research institutions conducting the trade the peaceful-use article protects. They profit from guaranteed markets among compliant states and lose contracts when supplier states tighten conditions; their commercial planning tracks the inspection agency's country assessments.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, peaceful_nuclear_commerce_sector, beneficiary,
    organized, biographical, mobile, global).

% A bloc of mostly small and middle-power states plus humanitarian organizations that negotiated a parallel prohibition treaty banning nuclear weapons outright, in force since 2021. The weapon states and their allies boycott it. The coalition's remedy lives outside the review-conference room, where its interventions are recorded but carry no vote.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, tpnw_humanitarian_coalition, excluded,
    organized, generational, mobile, global).

% International lawyers and treaty-interpretation scholars who debate what Article VI's good-faith language legally requires, whether customary law has moved past the 1968 text, and how the International Court's 1996 advisory opinion should be read. They hold no votes; their analyses circulate through delegations and courts.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, arms_control_jurists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states_p5).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal proliferation: centralizes verification of declared programs, raises the cost of covert weapons efforts, caps the number of new nuclear entrants, and provides a predictable framework for peaceful nuclear trade among compliant states.
% TRANSFER_FUNCTION: Moves restraint obligations, inspection access, and permanently foregone military options from non-weapon states toward the collective security of the armed few; moves peaceful-use technology and partial security assurances back toward compliant states; leaves weapon-state arsenals untouched by either flow.
% ABSENT_VOICES: The armed states outside the treaty (India, Pakistan, Israel) whose capabilities the regime's categories pretend away; the populations of weapon states, who never consented to indefinite arsenal retention and have no seat in review conferences; the TPNW coalition, whose objections are recorded but carry no vote; and future generations, who inherit the two-tier order without representation anywhere in the machinery.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, supplier states would improvise ad hoc export controls, armed states would hedge against uncertainty, several threshold-capable states would likely move toward weapons within years, and peaceful nuclear commerce would fragment into distrustful bilateral deals — the entire architecture of inspection, conditionality, and sanctioned exit would have to be rebuilt from scratch.
% FOUNDING_PROBLEM: In the early 1960s, informed forecasts projected fifteen to twenty-five nuclear states within two decades; the United States and Soviet Union shared an interest in freezing the club at five, West German and Japanese latent capabilities alarmed both blocs, and the post-Cuba-crisis environment made capping entrants a superpower priority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: non-weapon-state security establishments treat the cascade danger as real (their restraint purchases their own safety too), inspection-agency reporting documented clandestine programs in Iraq, Libya, and Iran, and the observed behavior of states that pursued weapons when constraints loosened confirms the underlying dynamic. What those same non-weapon-state parties dispute is not the problem's liveness but the two-tier remedy — corroboration of the founding problem is broad, corroboration of this reading's solution is confined to the weapon states and their allies.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.62 by this reading's own lights over the standing two-tier arrangement: non-weapon states bear permanent restraint, intrusive verification, and technology-denial spillover, partially offset by genuine cascade-security goods and conditional peaceful-use access that this reading treats as legitimate consideration rather than extraction. Suppression is 0.68 as a raw structural property (unscaled by power or scope): the regime's persistence depends on a mature coercive architecture — strengthened safeguards after the 1991 Iraq discovery, the Additional Protocol, harmonized export controls, and sanctions visited on the one state that exited. Theater ratio is 0.40: the verification function is real skilled work, but review-cycle governance has become increasingly ritual — consensus documents restating unmet promises, two consecutive conferences ending without outcomes — so a growing share of activity performs commitment rather than producing it. Accessibility collapse is 0.55: for insiders the weapons alternative is largely closed, but formal withdrawal survives and permanent-outsider status remains a demonstrated path. Resistance is 0.60: organized bloc resistance, the TPNW schism, and repeated conference breakdowns are real and continuing. The claimed type (tangled_rope) and these metric values were authored independently: the claim states what I believe is structurally true (genuine coordination function plus asymmetric extraction under active enforcement), the metrics state what I believe is descriptively true of operation. All three tracked series run on one shared nine-point grid (1968-2025) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the weapon-state seat the arrangement is protective coordination it built, funds, and administers — the verification burden falls on others, the arsenals are grandfathered, and Article VI's softness is prudence, not bad faith. From the non-weapon-state seat the same structure is enforced restraint with unenforceable reciprocity: obligations are justiciable in one direction only. From the inspection agency's seat it is a professional verification regime whose distributional questions belong to someone else. From the jurists' seat it is an indeterminate text whose vagueness does the political work. The engine derives these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the P5 sit nearest the beneficiary pole (full capture of the arrangement's security rents, arbitrage-grade exit since they write and reinterpret the rules); the commerce sector is a mobile beneficiary trading under the regime's protection; the inspection agency derives a modest beneficiary position (budget, mandate, and centrality grow with the regime) tempered by the enforcement labor it absorbs. Victim declarations drive high directionality: non-weapon-state parties sit near the target pole, amplified by constrained exit (withdrawal is nominally open but the DPRK precedent prices it punitively); sanctioned withdrawal states sit nearest the full-target end. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options produces the correct relationships for every seat, and no agent's derived position is distorted by capture or indirect effects that an override would correct.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is resolved here: the founding problem (preventing a proliferation cascade among latent-capable states) remains live, and corroboration comes from outside the beneficiary set — non-weapon-state security establishments, inspection-agency findings of clandestine programs, and the observed behavior of states that raced for weapons when constraints loosened. The classification discipline matters in both directions for this constraint. Labeling it pure extraction would erase the genuine cascade-prevention good that every seat, including the restrained ones, consumes; labeling it pure coordination would erase the enforced, one-directional burden structure that the same machinery maintains and that this reading deliberately stabilizes. The tangled form keeps both facts visible simultaneously. What this reading adds over the interval is the progressive hardening of the asymmetry — extractiveness rising from 0.42 to 0.62 as disarmament reciprocity receded from operative politics — which the temporal series records and which distinguishes a maintained hybrid from a static one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading of the npt_article_iv_vi_pairing kernel; which reading governs the regime''s actual operation, and what would the sibling readings change structurally?',
    'Track state practice over time: review-conference voting patterns, TPNW accession counts, judicial and arbitral treatment of Article VI, and whether any enforcement body ever conditions Article IV benefits on Article VI performance.',
    'If the grand_bargain reading prevails, Article IV legitimacy becomes conditional on disarmament progress and the burden structure rebalances toward weapon states; if the abolitionist reading prevails, the two-tier referent itself dissolves and this constraint''s epsilon referent disappears.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the Article IV/VI pairing governs the regime.').

omega_variable(
    constructed_vs_irreducible_asymmetry,
    'Is the two-tier asymmetry a constructed arrangement serving identifiable agents, or an irreducible feature of nuclear technology once weapon knowledge exists (verification reaches declared programs, so the armed few will always stand partly outside)?',
    'Counterfactual technical analysis: assess whether a symmetric universal verification regime is achievable (IPNDV-style disarmament verification research), and compare with the structural symmetry of the chemical and biological weapons conventions.',
    'If constructed, the arrangement behaves as a defended artifact and extraction estimates rise; if irreducible, a floor of asymmetry persists under any reading and part of the measured extraction is misattributed to the arrangement rather than to physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_irreducible_asymmetry, conceptual, 'Whether the asymmetry is chosen or forced.').

omega_variable(
    vi_justiciability_deliberateness,
    'Is Article VI genuinely indeterminate in content (hence non-justiciable), or was that indeterminacy manufactured during the 1968 negotiations to protect the drafters'' arsenals?',
    'Negotiating-history scholarship (the Acheson-Evatt-Gromyko exchanges and allied assurances), drafting records, and comparison with contemporaneously drafted justiciable treaty obligations.',
    'If manufactured, the non-justiciability is itself an enforcement artifact; the extraction component rises sharply and the computed type drifts toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vi_justiciability_deliberateness, empirical, 'Whether Article VI''s vagueness is textual fate or drafting choice.').

omega_variable(
    security_interest_vs_club_rent,
    'How much of weapon-state enforcement intensity tracks genuine cascade fear versus preservation of exclusive deterrence status?',
    'Compare enforcement responses to proliferation by aligned outsiders (Israeli and Indian programs tolerated or accommodated) versus adversarial programs (Iraq, Libya, DPRK, Iran met with zero tolerance); the differential response isolates the rent component.',
    'A large rent share raises effective extraction attributable to the weapon-state seat and supports reading the regime''s authority as extraction-grounded rather than security-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_interest_vs_club_rent, empirical, 'Security motive versus monopoly-rent motive in enforcement.').

omega_variable(
    withdrawal_exit_reality,
    'Does Article X withdrawal function as a real exit option for dissatisfied non-weapon states, or does the punishment precedent convert it into a trap?',
    'DPRK outcome tracking plus counterfactual pricing: quantify what a compliant state would forfeit in trade, finance, and security assurances by withdrawing versus remaining.',
    'If exit is effectively trapped, non-weapon-state directionality sits nearer the full-target end and effective extraction rises; if exit is real, the arrangement retains a competitive discipline that tempers extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(withdrawal_exit_reality, empirical, 'Whether treaty exit is available or punitive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1968, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_np_primary_tr_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1968, 0.1).
narrative_ontology:measurement(npt_np_primary_tr_t1975, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(npt_np_primary_tr_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(npt_np_primary_tr_t1991, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(npt_np_primary_tr_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(npt_np_primary_tr_t2003, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(npt_np_primary_tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(npt_np_primary_tr_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2015, 0.34).
narrative_ontology:measurement(npt_np_primary_tr_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(npt_np_primary_be_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(npt_np_primary_be_t1975, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1975, 0.46).
narrative_ontology:measurement(npt_np_primary_be_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(npt_np_primary_be_t1991, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1991, 0.53).
narrative_ontology:measurement(npt_np_primary_be_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1995, 0.56).
narrative_ontology:measurement(npt_np_primary_be_t2003, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2003, 0.59).
narrative_ontology:measurement(npt_np_primary_be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement(npt_np_primary_be_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(npt_np_primary_be_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(npt_np_primary_su_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1968, 0.35).
narrative_ontology:measurement(npt_np_primary_su_t1975, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(npt_np_primary_su_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(npt_np_primary_su_t1991, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1991, 0.52).
narrative_ontology:measurement(npt_np_primary_su_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(npt_np_primary_su_t2003, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2003, 0.62).
narrative_ontology:measurement(npt_np_primary_su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(npt_np_primary_su_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(npt_np_primary_su_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NPT bargain' conflates three structurally distinct readings of the Article IV/VI pairing, decomposed per the epsilon-invariance principle into a three-story constraint family. This file instantiates nonproliferation_primary (epsilon 0.62 by its own lights: verification-gated sharing over a stabilized two-tier order, weapon-state arsenals outside enforcement). The sibling file npt_article_iv_vi_pairing__grand_bargain instantiates the reciprocal-obligations reading (higher epsilon attributed to weapon-state non-performance, Article IV legitimacy made conditional on Article VI progress). The sibling file npt_article_iv_vi_pairing__abolitionist instantiates the disarmament-mandate reading (highest epsilon, authority external to the treaty text via humanitarian law and the TPNW). Each story carries its own beneficiaries, victims, claimed type, and epsilon; the edges here express family membership and mutual structural influence, not endorsement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
