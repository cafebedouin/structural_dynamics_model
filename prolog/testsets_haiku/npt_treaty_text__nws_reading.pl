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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: NPT Treaty Text: NWS Reading (Non-proliferation Binding, Disarmament Aspirational)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint is ONE READING of the Non-Proliferation Treaty (NPT),
 *   specifically the reading endorsed by nuclear weapon states and dominant
 *   in official treaty interpretation. The kernel is the NPT text itself,
 *   particularly Article VI's commitment to pursue 'negotiations in good
 *   faith on effective measures relating to cessation of the nuclear arms
 *   race at an early date and to nuclear disarmament.' The NWS reading
 *   interprets 'at an early date' and 'good faith' as commitments to dialogue
 *   and effort without binding timelines, allowing NWS to maintain arsenals
 *   indefinitely while NNWS face perpetual non-proliferation obligation. The
 *   sibling reading (NNWS reading) interprets the same text as binding both
 *   parties equally: NWS to disarm, NNWS to refrain. This constraint story
 *   models ONLY the NWS reading as an ε-invariant structure; the sibling
 *   readings are separate constraints in the same family, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (agenda-setters, beneficiaries): US, UK, France, Russia, China; set the interpretive frame for Article VI's disarmament language and defer indefinitely.
 *   - Non-Nuclear Weapon States (payers, trapped): ~190 NNWS signatories; accept binding non-proliferation while disarmament recedes as aspirational.
 *   - Disarmament Advocates (excluded payers): Civil society, NAM states, humanitarian coalitions; argue for binding disarmament timelines but lack formal treaty interpretation power.
 *   - IAEA Safeguards System (beneficiary): Institutional machinery concentrated on horizontal proliferation verification, not NWS arsenal monitoring.
 *   - Treaty Text Supremacy Doctrine (vindicated proposition): Legal-positivist reading of treaties as binding only to their written terms, supporting indefinite deferral of disarmament 'at an early date'.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.71).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.68).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Treaty Text: NWS Reading (Non-proliferation Binding, Disarmament Aspirational)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '7cae7e94-b4d4-4a92-9c43-e5d12eaa089f').
narrative_ontology:cs_kernel_codification('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', fixed_text).
narrative_ontology:cs_authority_grounding('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', extraction).
narrative_ontology:cs_interpretation_layer_present('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f').
narrative_ontology:cs_reading_relation('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', foundational, disarmament_commitment_procedural_not_substantive).
narrative_ontology:cs_axiom_status(disarmament_commitment_procedural_not_substantive, holdable).
narrative_ontology:cs_axiom_grounding('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', disarmament_commitment_procedural_not_substantive, deontological).
narrative_ontology:cs_axiom('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', foundational, nws_arsenal_legitimacy_stable_under_treaty).
narrative_ontology:cs_axiom_status(nws_arsenal_legitimacy_stable_under_treaty, holdable).
narrative_ontology:cs_axiom_grounding('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', nws_arsenal_legitimacy_stable_under_treaty, conventional).
narrative_ontology:cs_reference_frame('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', nws_good_faith_indefinite_disarmament).
narrative_ontology:cs_drift_state('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', contemporary_post_2020, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cae7e94-b4d4-4a92-9c43-e5d12eaa089f', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, npt_security_architecture).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, disarmament_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea_safeguards_system).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, proliferation_risk_countries).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, treaty_text_supremacy_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, legal_positivist_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent UNSC members (US, UK, France, Russia, China) set the interpretive frame for Article VI disarmament language through their presence and veto power in NPT Review Conferences and bilateral Strategic Arms Control Talks. They defend 'at an early date' as a commitment to negotiate in good faith without a binding timeline or endpoint. They collect the security benefit of a non-proliferation regime that prevents other states from achieving their arsenal status, while they retain indefinite arsenal rights. Their exit options are available but costly: they could withdraw from the NPT, but that would isolate them diplomatically and undermine the regime that protects them from cascading proliferation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nuclear_weapon_states, beneficiary).

% The ~190 NNWS signatories are bound by binding non-proliferation commitments: IAEA inspections, comprehensive safeguards agreements, technology export controls, and political pressure against any indigenous enrichment or reprocessing capability. They accepted the bargain that NWS would move toward disarmament 'at an early date' in exchange for perpetual non-proliferation restraint. They bear the cost of asymmetric oversight, capability denial, and international isolation if they pursue indigenous programs. Exit (NPT withdrawal) is available but geopolitically catastrophic — it triggers international pressure, sanctions, and isolation from trade and security partnerships.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    powerless, generational, trapped, global).

% Civil society organizations, many non-aligned governments, and humanitarian coalitions argue that Article VI's disarmament language binds NWS to negotiate specific timelines and endpoints for arsenal elimination. They are excluded from formal treaty interpretation processes (NPT Review Conferences are dominated by state governments; NGO testimony is advisory only) and from the institutions that define what 'early date' and 'good faith' mean (UNSC, bilateral strategic talks, defense department reviews). Their only leverage is public pressure, civil disobedience, and non-compliance rhetoric that attempts to undermine the regime's legitimacy.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, disarmament_advocates, excluded).

% The International Atomic Energy Agency operates the nuclear safeguards system that verifies NNWS non-proliferation compliance under NPT agreements. The NWS reading of Article VI (disarmament as aspirational) results in an IAEA budget and mandate concentrated on horizontal proliferation detection rather than vertical disarmament monitoring. The IAEA's institutional survival depends on demonstrating effective non-proliferation verification on the NNWS side; it has minimal resources or political mandate to monitor NWS arsenals. It benefits from the regime's legitimacy but is constrained by consensus-rule politics in its Board of Governors.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_safeguards_system, beneficiary,
    institutional, generational, constrained, global).

% The NPT Review Conference every five years is the formal mechanism for treaty interpretation and amendment. In practice, consensus rules mean Review Conferences either produce vague consensus language that accommodates both readings ('reaffirms commitment to disarmament' without defining timelines), or fail to produce a final document. This structural feature prevents the interpretive contest from producing a legal verdict and maintains both readings in live institutional tension, while the NWS reading dominates actual practice.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, treaty_review_process, agenda_setter,
    institutional, generational, analytical, global).

% States with technical capacity to develop nuclear weapons (Iran, Japan, South Korea, Iraq pre-2003, others) face intense IAEA oversight, technology export restrictions, and political pressure under the NWS reading. They pay through capability denial (banned from certain fuel cycles), international isolation, and reduced strategic autonomy. Their exit options are constrained: leaving the NPT triggers immediate sanctions and isolation; staying means accepting technology denial and sovereignty restrictions.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, proliferation_risk_countries, payer,
    moderate, biographical, constrained, regional).

% A legal doctrine that interprets treaties according to their written text and ordinary meaning, resisting teleological (purpose-driven) or evolutionary interpretation. Under legal positivism, 'at an early date' is a binding commitment to pursue disarmament, but without specifying timelines or endpoints — the NWS reading. This doctrine is vindicated by the constraint's institutional dominance; it shapes how international lawyers teach treaty interpretation and how state parties conduct negotiations.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, legal_positivist_interpretation, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__nws_reading, legal_positivist_interpretation).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified global non-proliferation verification system (IAEA under NPT safeguards) that prevents horizontal proliferation by directing intensive inspection on non-nuclear states, avoiding a fragmented world of bilateral intelligence assessments and unilateral security responses. The coordination problem solved: how to maintain confidence that no new states will develop nuclear weapons without each state conducting autonomous, costly intelligence operations.
% TRANSFER_FUNCTION: Moves from NNWS to NWS: (1) consent to intrusive IAEA inspections on civilian nuclear facilities; (2) restrictions on uranium enrichment and plutonium reprocessing technology; (3) restrictions on peaceful nuclear technology transfer; (4) political legitimacy for NWS arsenals (the treaty recognizes their permanent status as NWS). Moves from NWS to global order: legitimacy of existing arsenals through treaty-enshrined status, and a commitment to disarmament negotiations that (under the NWS reading) can be fulfilled indefinitely through procedural compliance without substantive arsenal reduction.
% ABSENT_VOICES: Proliferation-risk countries (Iran, Japan, South Korea, etc.) are signatories but not agenda-setters in treaty interpretation. Disarmament advocates and humanitarian coalitions have no voting rights in Review Conferences; their testimony is received but not binding. Subnational actors (uranium-mining communities, target publics of disarmament messaging, indigenous peoples in nuclear weapon state territories) have no institutional voice.
% DISAPPEARANCE_RATIONALE: If the NPT and its enforcement machinery disappeared overnight: (1) IAEA safeguards inspections would cease; (2) technology export controls on enrichment and reprocessing would collapse; (3) multiple states would pursue indigenous nuclear programs within 5–10 years; (4) NWS arsenals would remain in place but lose treaty-derived legitimacy; (5) the security architecture that depends on NNWS non-proliferation compliance would fracture into a multipolar nuclear world. The constraint's disappearance would trigger cascading proliferation and systemic instability.
% FOUNDING_PROBLEM: After World War II, the Soviet Union developed nuclear weapons (1949), and multiple nations pursued their own programs — China (1964), India (1974 covert test), Pakistan (1998 overt test), others. The founding problem was: how to prevent unlimited horizontal proliferation (every state acquiring nuclear weapons) while accommodating superpower nuclear deterrence?
% FOUNDING_PROBLEM_CORROBORATION: NWS governments attest the founding problem of horizontal proliferation remains live and the NPT is the solution preventing a world of 20+ NWS. Non-aligned governments, disarmament advocates, and proliferation scholars attest the founding problem was substantially contained by the 1970s–1980s (the number of NWS stabilized at 7–9) and the regime now persists not to prevent proliferation risk but to freeze great-power nuclear dominance. IAEA data shows the number of NWS has remained stable (9 confirmed) since ~1998; no new state has succeeded in a test since 1998 (though several programs advanced). This supports the contested verdict: the regime may have solved the coordination problem of managing global confidence and international pressure against proliferation, but the founding problem of proliferation risk appears substantially contained, suggesting the constraint's asymmetry may be serving interests other than the founding problem.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness reaches 0.71 by 2024 because the constraint systematically transfers non-proliferation compliance (inspection, technology denial, political isolation) from NNWS to NWS, while the reciprocal disarmament commitment (Article VI) is reframed as aspirational and indefinitely deferred. The NWS reading is the structural mechanism of this extraction: it privileges legal-positivist interpretation of 'good faith' as procedural rather than substantive, allowing NWS to satisfy the letter of Article VI through occasional disarmament talks while maintaining arsenal stability. Suppression increases from 0.48 to 0.68 because IAEA oversight intensifies (post-Cold War, post-Iran programs) and political pressure on proliferation-risk states hardens, while the asymmetry between binding non-proliferation and aspirational disarmament is never explicitly contested in treaty bodies (consensus-rule Review Conferences prevent a final legal verdict). Theater ratio rises from 0.15 to 0.42, reflecting the increasing share of NPT Review Conference rhetoric devoted to disarmament language that remains unimplemented, creating a performative character while real IAEA verification work concentrates on NNWS horizontal proliferation. The measurement series tracks the constraint's drift toward higher extraction and increased theater as the founding problem (preventing horizontal proliferation) appears contained, yet the constraint's asymmetry persists and deepens.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the constraint is protective (prevents dangerous proliferation, maintains strategic stability, preserves deterrence legitimacy). From the NNWS seat, especially those with technical capacity for indigenous programs, the constraint is extractive: it locks them into perpetual non-proliferation while NWS arsenals grow or stabilize indefinitely. The IAEA sits in a dependent position — funded by both NWS and NNWS, its institutional survival depends on demonstrating effective non-proliferation on the NNWS side while having no mandate to verify NWS disarmament. The engine's per-seat classification should compute this divergence: NWS as lower-extraction (they benefit, have exit options via arbitrage or treaty withdrawal with negotiated transition), NNWS as higher-extraction (they are trapped in non-proliferation by geopolitical and technical asymmetry), and the disarmament advocates as seeing pure extraction (a binding obligation on NNWS with an unenforceable counterpart on NWS).
 *
 * DIRECTIONALITY LOGIC:
 *   NWS directionality is low (near beneficiary end, ~0.2): they set the rules, collect security benefits, have exit options (withdrawal entails costs but is available; they can also unilaterally reinterpret Article VI). NNWS directionality is high (near target end, ~0.8): they bear binding non-proliferation costs with no reciprocal obligation that binds NWS to action; their exit (withdrawal from NPT) is available but geopolitically catastrophic. Disarmament advocates have high directionality (~0.85) because they are excluded from institutional power and the constraint operates against their preferred outcomes (binding NWS disarmament). The IAEA sits near symmetric on institutional grounds (it benefits from treaty authority but is constrained by consensus rules and NWS veto), though its funding structure creates implicit dependence on NWS goodwill.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing unlimited horizontal proliferation after WWII — appears substantially contained by the 1970s (India's 1974 test was treated as anomaly; subsequent proliferation has been slow and limited). Yet the constraint persists and intensifies in its extraction function (IAEA budget increases, inspection frequency increases, technology export controls tighten) without corresponding change to Article VI interpretation. A mandatrophy signal is present: the constraint's original coordination function (prevent proliferation) may have been achieved, but the asymmetric extraction (binding NNWS, aspirational NNWS disarmament) persists due to institutional inertia and NWS institutional power. The theater ratio rising from 0.15 to 0.42 is the key metric: Review Conferences generate increasingly elaborate disarmament language and commitments while arsenals remain stable, suggesting performative maintenance. The constraint has drifted from coordination (all parties benefited from a non-proliferation architecture) toward tangled-rope, then toward extraction asymmetry sustained by consensus rules that prevent either explicit renegotiation or formal split.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_interpretation,
    'Does Article VI ''at an early date'' constitute a binding disarmament timeline (NNWS reading) or a commitment to pursue good-faith negotiations indefinitely without a deadline (NWS reading)?',
    'International Court of Justice advisory opinion on Article VI interpretation; consensus at a future NPT Review Conference; or emergence of a new international legal authority (e.g., treaty amendment process) that produces an explicit verdict.',
    'If binding timeline wins: the constraint flips to a more symmetric coordination (tangled-rope with equal reciprocal obligations, or rope if disarmament credibly commits). If indefinite-deferral reading prevails: extraction asymmetry is legally formalized and the constraint consolidates as snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_binding_interpretation, conceptual, 'The core interpretive disagreement over Article VI: binding timeline vs. aspirational indefinite commitment.').

omega_variable(
    founding_problem_persistence,
    'Has the founding problem (preventing horizontal proliferation) been substantially solved by the constraint, or does it remain live?',
    'Empirical assessment: track the number of NWS, rate of new state nuclear programs, and IAEA safeguards efficacy. If the number of NWS stabilizes at 9–10 and no new programs succeed, the constraint''s founding function is achieved. If new programs accelerate or threshold states proliferate, the founding problem persists.',
    'If founding problem is solved and constraint persists: mandatrophy signal strengthens, suggesting the constraint has drifted from coordination toward extraction maintenance. If founding problem persists: the constraint''s extraction asymmetry may be justified as the price of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the NPT''s non-proliferation objective has been achieved or remains contested.').

omega_variable(
    asymmetric_verification_necessity,
    'Is intensive IAEA verification of NNWS non-proliferation structurally necessary to prevent proliferation, or is the asymmetry serving NWS interests in arsenal legitimacy without verification need justification?',
    'Controlled experiment unavailable, but proxy comparison: examine whether IAEA''s shift to increased inspections of NNWS correlates with increased proliferation risk (suggesting verification is necessary) or with political pressure from NWS (suggesting asymmetry serves interests).',
    'If verification asymmetry is necessary: it is coordination cost, not extraction. If it serves NWS interests in unopposed arsenals: it is pure extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_verification_necessity, empirical, 'Whether the asymmetric IAEA verification burden reflects necessary coordination or NWS benefit.').

omega_variable(
    consensus_rule_effect_on_interpretation,
    'Does the NPT Review Conference consensus rule (requiring all states to agree on final documents) preserve legitimate regime stability, or does it give NWS veto power that protects their reading indefinitely?',
    'Institutional reform: move to majority-vote or supermajority decision rules in Review Conferences; observe whether the NWS reading loses institutional protection and the disarmament interpretation shifts.',
    'If consensus rule is necessary: it is coordination infrastructure. If it primarily serves NWS veto: it is extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_rule_effect_on_interpretation, conceptual, 'Whether the NPT''s consensus rule is coordination mechanism or NWS power protection.').

omega_variable(
    knowledge_of_kernel_contest,
    'To what degree does the general NNWS population, policymakers outside the treaty system, and civil society understand that the NPT text is contested and that the NWS reading is one of multiple possible interpretations?',
    'Epistemic access asymmetry: NWS officials and legal scholars understand the interpretive contest; NNWS governments may suppress this knowledge domestically to maintain compliance. Public education or leak of treaty negotiation records could shift the epistemic frame.',
    'If the reading contest remains hidden: NWS reading appears as law-given text, not as chosen interpretation; extraction is normalized. If the contest becomes visible: NNWS populations may withdraw support for compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_of_kernel_contest, conceptual, 'Epistemic management of the treaty-interpretation contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nws_tr_t1968, npt_treaty_text__nws_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement_basis(npt_nws_tr_t1968, observed).
narrative_ontology:measurement(npt_nws_tr_t1980, npt_treaty_text__nws_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement_basis(npt_nws_tr_t1980, observed).
narrative_ontology:measurement(npt_nws_tr_t1995, npt_treaty_text__nws_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(npt_nws_tr_t1995, observed).
narrative_ontology:measurement(npt_nws_tr_t2005, npt_treaty_text__nws_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement_basis(npt_nws_tr_t2005, observed).
narrative_ontology:measurement(npt_nws_tr_t2015, npt_treaty_text__nws_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(npt_nws_tr_t2015, observed).
narrative_ontology:measurement(npt_nws_tr_t2024, npt_treaty_text__nws_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(npt_nws_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(npt_nws_be_t1968, npt_treaty_text__nws_reading, base_extractiveness, 1968, 0.44).
narrative_ontology:measurement_basis(npt_nws_be_t1968, observed).
narrative_ontology:measurement(npt_nws_be_t1980, npt_treaty_text__nws_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement_basis(npt_nws_be_t1980, observed).
narrative_ontology:measurement(npt_nws_be_t1995, npt_treaty_text__nws_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement_basis(npt_nws_be_t1995, observed).
narrative_ontology:measurement(npt_nws_be_t2005, npt_treaty_text__nws_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement_basis(npt_nws_be_t2005, observed).
narrative_ontology:measurement(npt_nws_be_t2015, npt_treaty_text__nws_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement_basis(npt_nws_be_t2015, observed).
narrative_ontology:measurement(npt_nws_be_t2024, npt_treaty_text__nws_reading, base_extractiveness, 2024, 0.71).
narrative_ontology:measurement_basis(npt_nws_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_nws_su_t1968, npt_treaty_text__nws_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement_basis(npt_nws_su_t1968, observed).
narrative_ontology:measurement(npt_nws_su_t1980, npt_treaty_text__nws_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(npt_nws_su_t1980, observed).
narrative_ontology:measurement(npt_nws_su_t1995, npt_treaty_text__nws_reading, suppression_requirement, 1995, 0.61).
narrative_ontology:measurement_basis(npt_nws_su_t1995, observed).
narrative_ontology:measurement(npt_nws_su_t2005, npt_treaty_text__nws_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(npt_nws_su_t2005, observed).
narrative_ontology:measurement(npt_nws_su_t2015, npt_treaty_text__nws_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement_basis(npt_nws_su_t2015, observed).
narrative_ontology:measurement(npt_nws_su_t2024, npt_treaty_text__nws_reading, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement_basis(npt_nws_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nws_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, export_control_regimes).

% DUAL FORMULATION NOTE:
% The NPT treaty text constitutes a kernel instantiated by multiple readings. This constraint (nws_reading) models the NWS interpretation: non-proliferation as binding on NNWS, disarmament as aspirational. The sibling reading (nnws_reading) models the NNWS/disarmament-advocate interpretation: both non-proliferation and disarmament as binding reciprocal obligations. These are not the same constraint viewed from two angles — they have different beneficiaries, different ε values, and different terminal states. The ε-invariance principle (DP-001) requires separate constraint stories per reading. Both readings are live in international discourse; neither is foreclosed by the text or by state practice. The network link indicates that understanding one reading requires understanding the other as the structural alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nws_reading, powerless, 0.82).
constraint_indexing:directionality_override(npt_treaty_text__nws_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
