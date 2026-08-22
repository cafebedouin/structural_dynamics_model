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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI Reciprocal Disarmament Obligation (Binding Reading)
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   The NPT of 1968 (entered into force 1970) creates a two-tier system:
 *   non-nuclear weapons states renounce weapons development and submit to
 *   IAEA inspection; nuclear weapons states commit to disarmament 'in good
 *   faith' under Article VI. This constraint story instantiates the
 *   reciprocal_disarmament_reading, which interprets Article VI as a binding
 *   legal obligation with temporal urgency, making horizontal and vertical
 *   nonproliferation a reciprocal bargain: NNWS restraint is conditional on
 *   NWS disarmament progress. This reading competes with the
 *   oligopoly_enforcement_reading (horizontal nonproliferation as primary,
 *   Article VI as contingent aspirational commitment) and the
 *   withdrawal_sovereignty_reading (Article X withdrawal right as paramount,
 *   treaty obligations contingent on security environment). The reciprocal
 *   reading frames NWS modernization programs and indefinite nuclear
 *   deterrence as breach of Article VI, constraining NWS strategic autonomy
 *   and elevating NNWS coalition leverage. The constraint is a tangled_rope
 *   from the reciprocal perspective: genuine coordination function (mutual
 *   security through nonproliferation restraint) layered with asymmetric
 *   extraction (NNWS submit to intrusive verification; NWS retain strategic
 *   autonomy despite binding disarmament obligation). The claim/metric gap is
 *   deliberate and structural: extractiveness rises from 0.35 (1970, when the
 *   bargain was newly struck) to 0.68 (2025, after 55 years of NWS
 *   disarmament non-compliance), and theater_ratio rises steeply (0.15 to
 *   0.58) as Review Conferences generate rhetoric about Article VI commitment
 *   while NWS continue modernization programs—the performative gap widens
 *   over time.
 *
 * KEY AGENTS:
 *   - non_nuclear_weapons_states_coalition: primary beneficiary of reciprocal bargain, constrained by horizontal nonproliferation verification, leverages NNWS voting bloc at Review Conferences to press for NWS Article VI enforcement
 *   - nuclear_weapons_states_modernization_programs: primary payer under this reading, identity-locked to nuclear deterrence doctrine, constrained by Article VI obligation but unverified, continue modernization while claiming disarmament commitment
 *   - npt_review_conference_institution: agenda-setter for treaty interpretation, generates consensus documents that interpret Article VI obligations and set enforcement benchmarks, carries political but not enforcement authority
 *   - iaea_safeguards_inspectorate: enforcer of horizontal (NNWS) nonproliferation, structurally asymmetrical—no verification authority for NWS Article VI compliance, evidence of extraction mechanism
 *   - non_aligned_movement_voting_bloc: organized NNWS actor with voting leverage, uses NAM coalition to push for binding disarmament benchmarks and verification at Review Conferences
 *   - security_council_nuclear_powers: P5 members hold veto on enforcement, structurally unable to enforce Article VI against themselves, enforce against NNWS via Chapter VII authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.42).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.31).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI Reciprocal Disarmament Obligation (Binding Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, 'fe78317d-094a-4c4b-b150-38b2d45be0c1').
narrative_ontology:cs_kernel_codification('fe78317d-094a-4c4b-b150-38b2d45be0c1', formalized).
narrative_ontology:cs_authority_grounding('fe78317d-094a-4c4b-b150-38b2d45be0c1', lineage).
narrative_ontology:cs_interpretation_layer_present('fe78317d-094a-4c4b-b150-38b2d45be0c1').
narrative_ontology:cs_reading_relation('fe78317d-094a-4c4b-b150-38b2d45be0c1', npt_treaty_1970__oligopoly_enforcement_reading, forecloses).
narrative_ontology:cs_reading_relation('fe78317d-094a-4c4b-b150-38b2d45be0c1', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('fe78317d-094a-4c4b-b150-38b2d45be0c1', foundational, article_vi_binding_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('fe78317d-094a-4c4b-b150-38b2d45be0c1', article_vi_binding_obligation, deontological).
narrative_ontology:cs_axiom('fe78317d-094a-4c4b-b150-38b2d45be0c1', foundational, reciprocity_principle_npt_contract).
narrative_ontology:cs_axiom_status(reciprocity_principle_npt_contract, holdable).
narrative_ontology:cs_axiom_grounding('fe78317d-094a-4c4b-b150-38b2d45be0c1', reciprocity_principle_npt_contract, conventional).
narrative_ontology:cs_reference_frame('fe78317d-094a-4c4b-b150-38b2d45be0c1', mutual_disarmament_commitment_1970).
narrative_ontology:cs_drift_state('fe78317d-094a-4c4b-b150-38b2d45be0c1', contemporary_post_2020_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fe78317d-094a-4c4b-b150-38b2d45be0c1', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapons_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapons_states_modernization_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_aligned_movement_voting_bloc).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, alliance_partners_extended_deterrence).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, disarmament_advocacy_movements).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapons_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, weapons_states_technical_communities).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, alliance_partners_extended_deterrence).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, treaty_reciprocity_principle).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, temporal_urgency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NNWS parties to the NPT obtain the bargain of reciprocal disarmament commitment from NWS: in exchange for renouncing nuclear weapons development, NNWS receive verifiable binding commitment to nuclear disarmament timelines and to cessation of NWS arms races. They bear the constraint of horizontal nonproliferation verification and inspection. Under this reading, NNWS hold NWS to Article VI as the quid pro quo for indefinite restraint. Coalition leverage is normative and institutional (bloc voting, treaty review conferences, referral authority). Exit is constrained: withdrawal triggers security isolation and regional instability.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapons_states_coalition, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapons_states_coalition, payer).

% NWS strategic doctrine is built on nuclear deterrence modernization and force modernization as security guarantees. This reading treats such modernization as breach of Article VI obligation to pursue 'cessation of the nuclear arms race' and 'nuclear disarmament.' NWS strategic autonomy is constrained by reading Article VI as binding and temporally urgent. Exit options are identity-locked: NWS identity as 'responsible nuclear power' is fused with nonproliferation compliance narrative; withdrawal or explicit repudiation of Article VI would dissolve the legitimacy frame that justifies their nuclear arsenals within the international legal order.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapons_states_modernization_programs, payer,
    powerful, civilizational, identity_locked, global).

% The Review Conference process (mandated every five years) is the formal venue for treaty interpretation, consensus-building, and enforcement proposals. Under the reciprocal disarmament reading, Review Conferences are the primary mechanism for holding NWS to Article VI: they produce final documents that interpret obligations, set verification benchmarks, and establish the consensus reading of what 'good faith' disarmament effort looks like. The institution carries no enforcement power but can condition agreement on specific benchmarks, withhold consensus legitimacy from NWS behavior, or trigger withdrawal/renegotiation votes.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, npt_review_conference_institution, agenda_setter,
    institutional, generational, analytical, global).

% The International Atomic Energy Agency operates the inspection and verification regime for horizontal (NNWS) nonproliferation under NPT Article III. Under this reading, IAEA enforcement of horizontal obligations is structurally asymmetrical: no comparable verification regime exists for NWS compliance with Article VI vertical disarmament. IAEA reports treaty violations by NNWS; no inspection authority exists to verify NWS disarmament progress. The asymmetry is the reading's claim: it establishes the structure of extraction — NNWS submit to intrusive, mandatory, continuous inspection; NWS submit only to voluntary, declarative, self-reported disarmament efforts.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_inspectorate, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_inspectorate, observer).

% Nuclear weapons design, production, and modernization communities within NWS (national laboratories, defense ministries, scientific academies) bear the constraint of Article VI as a binding obligation. Career paths and institutional missions are structured around weapons development and modernization. Under the reciprocal disarmament reading, these technical communities are constrained from pursuing full-spectrum modernization (warhead design, delivery systems, command-and-control) without claiming they serve disarmament verification or stockpile stewardship rather than enhancement. Exit is constrained by institutional identity: a weapons scientist's professional identity and career are fused with the weapons program.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, weapons_states_technical_communities, payer,
    powerful, biographical, constrained, global).

% The Non-Aligned Movement (NAM) countries, numerically dominant in the NNWS coalition, use treaty review processes to push for binding Article VI benchmarks and verification mechanisms. They frame horizontal nonproliferation as conditional on NWS disarmament progress: the reciprocal bargain is the basis for their continued NPT membership. They benefit from the reading's reframing of Article VI as binding because it elevates their leverage within the institution. Exit is mobile in principle (could leave NPT) but costlier than for powerful states (regional nuclear risks, sanctions, loss of IAEA peaceful-use assistance).
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_aligned_movement_voting_bloc, beneficiary,
    organized, generational, mobile, global).

% Non-nuclear alliance partners of NWS (Japan, South Korea, NATO members without nuclear weapons) depend on extended nuclear deterrence guarantees for security. Under the reciprocal disarmament reading, they are constrainedly dual-positioned: they benefit from the NWS commitment to nuclear disarmament as a long-term goal (reduced global risks), but they pay the constraint of that commitment via pressure for NWS restraint on modernization that might weaken extended deterrence credibility. Exit is identity-locked: their security identity is fused with the alliance relationship and deterrence commitment.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, alliance_partners_extended_deterrence, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, alliance_partners_extended_deterrence, beneficiary).

% International disarmament NGOs, civil society, and humanitarian coalitions (ICAN, IPPNW, etc.) benefit from the reciprocal disarmament reading by gaining a binding legal framework that can be invoked to pressure NWS on Article VI compliance. The reading legitimates their advocacy by elevating Article VI from aspirational to binding obligation. They organize outside the formal treaty institution but leverage the reading through track-two diplomacy, public pressure campaigns, and legal argumentation at review conferences. Exit is mobile: these movements can shift focus or strategy.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, disarmament_advocacy_movements, beneficiary,
    moderate, generational, mobile, global).

% The five Permanent Members of the UN Security Council (P5) are also the recognized NWS under the NPT. The Security Council is the formal institution with authority to refer nonproliferation violations to enforcement mechanisms (Chapter VII actions). Under this reading, the Security Council carries authority to interpret Article VI obligations and to enforce compliance, but the P5 veto ensures enforcement is used only against non-P5 NWS (if any emerged) or against NNWS, never against P5 itself. This structural asymmetry is the reading's evidence of extraction: enforcement authority exists but is structurally disabled for the primary binding obligation (Article VI) for the states most able to violate it (NWS).
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, security_council_nuclear_powers, observer,
    institutional, civilizational, analytical, global).

% States suspected or confirmed to be pursuing covert nuclear weapons programs (Iran under JCPOA, DPRK post-withdrawal) are excluded from the NPT reciprocity bargain by definition: they are not parties in good standing. Under the reciprocal disarmament reading, their behavior is cited as evidence of either NNWS security concern (driving proliferation) or failed NWS disarmament commitment (making NNWS skeptical that restraint brings security). Their voice — that they pursue weapons because NWS refuse disarmament — is excluded from the formal treaty negotiation. Exit is trapped: they cannot rejoin the NPT while openly pursuing weapons; pursuing weapons while in the NPT triggers expulsion.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iran_dprk_suspected_proliferators, excluded,
    powerful, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapons_states_modernization_programs).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT solves the international collective-action problem of preventing nuclear proliferation through a two-tier bargain: horizontal (NNWS restrain from weapons development, submit to intrusive verification) and vertical (NWS pursue disarmament in good faith, subject to binding Article VI obligation). The reciprocal disarmament reading frames this coordination as genuinely reciprocal: NNWS restraint is conditional on verified NWS disarmament progress, not on unilateral NWS choice to retain arsenals indefinitely. The coordination function is mutual security assurance via symmetrical restraint commitments and verification.
% TRANSFER_FUNCTION: Moves restraint, verification burden, and security assurance from NNWS to NWS. Horizontally: NNWS transfer their weapons development options to the international community (via IAEA safeguards and inspection rights). Vertically: NWS transfer (under this reading) their strategic modernization autonomy, warhead design freedom, and arms-race autonomy to a binding disarmament obligation with temporal benchmarks. The constraint operates by extracting verification burden from NNWS (intrusive inspection, economic costs of peaceful-only nuclear development) while extracting strategic autonomy constraints from NWS (modernization limits, disarmament timetables, transparency requirements). The asymmetry: NNWS extraction is institutionalized via IAEA; NWS extraction relies on treaty text interpretation and political pressure (no verification authority for Article VI).
% ABSENT_VOICES: States that have withdrawn from or are excluded from the NPT (Iran, DPRK, Pakistan, India, Israel—the latter three never signed). Their voice — that they pursue or retain weapons because NWS refuse disarmament, or because security concerns outweigh treaty restraint — is structurally absent from the formal Review Conference negotiation. Their position would argue that the reciprocal disarmament reading is a dead letter without enforcement mechanisms and NWS have systematically evaded Article VI obligation, invalidating the reciprocity bargain. Also absent: technical communities in NWS arguing that disarmament verification is impossible without unacceptable transparency costs and that strategic modernization is necessary for deterrence stability.
% DISAPPEARANCE_RATIONALE: If this reading (Article VI as binding, reciprocal bargain) were accepted and enforced—or if it disappeared and was replaced by the oligopoly_enforcement_reading (horizontal nonproliferation as primary, Article VI as contingent)—the international security landscape would rearrange substantially. Acceptance of this reading would activate NWS disarmament obligations, likely triggering either genuine disarmament negotiations (if NWS comply), treaty breakdowns (if NWS refuse and withdraw), or Security Council enforcement attempts (if the reading becomes consensus). Disappearance of this reading (replacement by oligopoly enforcement frame) would consolidate NWS de facto arms-race autonomy, would likely accelerate NNWS withdrawal and regional proliferation (if the reciprocity bargain is seen as dead), and would weaken the IAEA verification regime's legitimacy (horizontal-only enforcement becomes explicit, not implicit). The world does not remain unchanged under either shift.
% FOUNDING_PROBLEM: The founding problem of the NPT (1968, entry into force 1970) was the Cold War risk of uncontrolled nuclear proliferation: a world in which every technologically capable state could develop nuclear weapons would increase the risk of nuclear use through accident, miscalculation, or regional conflict. The horizontal nonproliferation solution was to bar non-weapon states from acquiring weapons. The reciprocal disarmament reading interprets the founding problem as requiring BOTH horizontal AND vertical restraint: horizontal nonproliferation is unstable without NWS disarmament, because NNWS have no security incentive to restrain if armed NWS remain armed indefinitely. The founding problem thus demands reciprocal commitment as the stabilizing mechanism.
% FOUNDING_PROBLEM_CORROBORATION: NWS and the oligopoly_enforcement reading attest that the founding problem (preventing NNWS proliferation) is live and successfully managed via horizontal nonproliferation, with Article VI as an aspirational long-term goal. NNWS coalition, disarmament advocates, and the reciprocal_disarmament_reading attest that the founding problem requires reciprocal disarmament as binding, and that NWS failure to pursue Article VI violates the treaty's core bargain—making the founding problem CONDITIONALLY live (NNWS restraint is conditional on NWS reciprocal effort). The International Court of Justice (ICJ) in the Legality of the Threat or Use of Nuclear Weapons advisory opinion (1996) affirmed that Article VI imposes a binding obligation to pursue 'in good faith' negotiations for disarmament, supporting the reciprocal reading's claim that the founding problem is live—but the opinion stopped short of mandating enforcement mechanisms or specific timetables. Corroboration from outside NWS beneficiary interests: the ICJ, the NNWS-majority UN General Assembly (which annually reaffirms calls for disarmament), and humanitarian organizations attest the reciprocal reading's interpretation of the founding problem as binding legal obligation.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.68 at interval end because the constraint operates as asymmetric restraint: NNWS are barred from weapons development and submit to intrusive, continuous IAEA inspection; NWS are obligated (under this reading) to disarm but face no comparable verification regime, no binding timetables, and no enforcement penalty for non-compliance. The asymmetry persists because NWS control the verification authority (IAEA Board of Governors includes NWS veto), the Security Council (P5 veto precludes enforcement against NWS), and the Review Conference (NWS can block consensus). Suppression is measured at 0.42, moderate rather than high, because the constraint relies on political pressure (NNWS bloc voting, civil society advocacy, ICJ opinions) rather than coercive enforcement—NWS cannot be forced to disarm, but they face normative pressure and the threat of NNWS treaty withdrawal. Theater_ratio rises steeply (0.15 to 0.58) because NWS rhetoric about Article VI commitment coexists with accelerating modernization programs: the gap between stated obligation and actual behavior grows over the interval, indicating that performative disarmament commitment replaces functional disarmament effort. Accessibility_collapse is low (0.31) because NNWS alternatives remain available: withdrawal from the NPT is legally possible (Article X), though costlier than compliance. Resistance is high (0.72) because NNWS coalition and disarmament movements mount substantial normative and political resistance through Review Conferences, UN resolutions, and advocacy—the reciprocal_disarmament reading itself is a form of resistance, reframing the treaty to elevate NNWS leverage.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (NWS modernization programs) and the beneficiary seat (NNWS coalition) should compute different types from this structural data. From the NWS position, Article VI is interpreted as aspirational (oligopoly_enforcement_reading), the constraint is a rope (genuine coordination of horizontal nonproliferation with optional disarmament), and NWS strategic autonomy is uncontested—d near beneficiary end (NWS benefits from indefinite deterrence under the cover of a coordination regime). From the NNWS position under the reciprocal_disarmament_reading, Article VI is binding, the constraint is a tangled_rope (coordination + extraction), and NWS strategic autonomy is a victim of the constraint—d near target end (NNWS are targets because their restraint is hostage to NWS non-compliance). The engine computes this divergence from the structural data: beneficiary/victim declarations (NNWS as beneficiary, NWS programs as victim), exit options (NWS identity-locked to deterrence; NNWS constrained but mobile), power levels (both powerful but in different ways: NWS have enforcement veto, NNWS have voting leverage), and spatial_scope (global). The review conference institution and IAEA sit at agenda-setter positions with analytical exit, computing a middling type (rope or tangled_rope depending on whether the reciprocal reading is accepted as operative). The Security Council sits at observer position with analytical exit, seeing the whole structure but unable to enforce it.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS modernization programs are the primary target: they are identity-locked to nuclear deterrence doctrine, making exit structurally difficult (repudiation of deterrence requires existential identity reconstruction); they face binding Article VI obligation under this reading, which constrains modernization options; they have powerful institutional positions (P5 veto, control of verification) but those powers are constrained by political pressure from NNWS coalition and civil society. Directionality d for NWS is high (~0.75) because the constraint extracts strategic autonomy and constrains modernization, and the target is trapped or identity-locked rather than mobile. NNWS coalition is the primary beneficiary: they obtain commitment to mutual nonproliferation, gain reciprocal disarmament obligation from NWS, leverage their voting bloc for enforcement mechanisms. But NNWS also bear costs: intrusive verification, restraint on nuclear development, dependence on NWS disarmament (which doesn't materialize). Directionality d for NNWS is near-symmetric (~0.50) because they are dual-positioned: beneficiary of the coordination function (mutual security), but payer of the extraction asymmetry (verification burden + NWS non-compliance). Alliance partners (Japan, South Korea, NATO NNWS) are near-symmetric (~0.48) because they benefit from NWS extended deterrence but pay constraints on NWS modernization that underpin that deterrence. No directionality override is needed: the derivation from beneficiary/victim + exit options produces accurate directional values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids false classification as pure rope by explicitly naming both coordination function (mutual nonproliferation security) and extraction asymmetry (horizontal verification + vertical non-verification, NNWS restraint + NWS modernization freedom). The tangled_rope claim captures the structural reality: genuine coordination function exists (the NPT does prevent proliferation and reduce security risks relative to an uncontrolled proliferation world), but it is layered with asymmetric extraction (NNWS carry verification burden while NWS retain strategic autonomy). The mandatrophy question—has the founding problem (preventing uncontrolled proliferation) been solved such that the constraint should sunset?—is contested by the readings. The oligopoly_enforcement reading would argue that mandatrophy is resolved: the founding problem (NNWS proliferation) is successfully managed via horizontal nonproliferation, making Article VI sunset-ready. The reciprocal_disarmament reading argues mandatrophy is NOT resolved: the founding problem requires reciprocal disarmament as the condition for NNWS restraint, and NWS failure to disarm means the founding problem is still live (NNWS are only restrained conditionally, and that condition is not met). The constraint does not carry an explicit sunset clause, which is evidence of extraction: a genuine rope would have a natural termination point (when everyone has disarmed), but the constraint has persisted for 55 years without termination, suggesting the beneficiaries (NWS under oligopoly reading, or whoever benefits from the asymmetry) have no incentive to end it. Theater_ratio is high (0.58) because NWS have incentive to perform disarmament commitment (maintain the coordination frame that legitimates their arsenals) while not delivering actual disarmament, exactly the pattern of a constraint with low functional value and high performative value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Is Article VI legally binding with temporal urgency, or is it an aspirational commitment contingent on evolving security conditions?',
    'International Court of Justice ruling on a state petition for interpretation of Article VI obligations; binding arbitration by the treaty review conference; Security Council resolution interpreting Article VI in a specific enforcement case.',
    'If binding and temporally urgent: NWS disarmament obligations activate enforcement pressure, NNWS gain leverage to condition treaty compliance on NWS progress, the reading becomes consensus treaty interpretation. If aspirational: NWS strategic autonomy is preserved, NNWS face pressure to accept indefinite treaty restraint without reciprocal NWS obligation, the oligopoly_enforcement_reading becomes the de facto operative reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'The core interpretive dispute: does Article VI impose binding disarmament obligation or aspirational commitment?').

omega_variable(
    verification_asymmetry_as_extraction,
    'Is the absence of NWS Article VI verification mechanisms (compared to IAEA inspection of NNWS) evidence of structural extraction, or a reasonable accommodation of NWS security interests?',
    'Establishment of an international disarmament verification authority with NWS inspection access at least as intrusive as IAEA inspection of NNWS; comparative transparency metrics for warhead inventories, modernization budgets, and disarmament progress.',
    'If the asymmetry is acknowledged as extractive: NWS are under structural obligation to accept verification on par with NNWS, the reading''s claim of tangled_rope (coordination + asymmetric extraction) is validated, enforcement pressure on NWS rises. If the asymmetry is reasonable: NWS retain strategic autonomy over verification, the reading''s extraction claim is weakened, the oligopoly_enforcement_reading gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_as_extraction, empirical, 'Whether the lack of NWS verification for Article VI is structural extraction or reasonable security accommodation.').

omega_variable(
    reciprocity_bargain_empirical_status,
    'Has the NWS engagement in disarmament negotiations and stockpile reductions met the empirical bar of good-faith Article VI compliance under the reciprocal_disarmament_reading?',
    'Quantitative assessment of NWS disarmament progress (warhead reductions, treaty verification, weapons-program transparency) against plausible benchmarks for ''good faith'' effort; comparison of NWS disarmament spending vs. modernization spending; evaluation of NNWS assessment of NWS compliance via treaty review conference final documents.',
    'If NWS have substantially complied: the reading''s extraction claim is weakened, the coordination function appears to operate as intended, NNWS restraint appears justified by reciprocal NWS effort. If NWS have substantially non-complied: the reading''s extraction claim is validated, the reciprocity bargain is seen as dead, NNWS are relieved of restraint obligations by NWS breach, proliferation pressure rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_bargain_empirical_status, empirical, 'Empirical status of NWS Article VI compliance: have they pursued disarmament in good faith?').

omega_variable(
    kernel_reading_contest_structural_closure,
    'Can the three readings of the NPT kernel (reciprocal_disarmament, oligopoly_enforcement, withdrawal_sovereignty) coexist within a single treaty framework, or does one reading logically foreclose the others?',
    'Treaty interpretation authority (International Court of Justice, Security Council, or treaty review conference consensus) resolves the contest by selecting one reading as the authoritative operative reading; alternatively, treaty amendment that explicitly specifies which reading governs.',
    'If readings coexist: the NPT operates under ambiguity, each party can assert its preferred reading, enforcement is contested, the constraint becomes effectively a platform for competing claims rather than a stable arrangement. If one reading forecloses others: that reading becomes treaty law, competing readings are invalid, the constraint stabilizes into one clear type (tangled_rope under reciprocal reading; snare under oligopoly reading; scaffold under withdrawal reading). If the three readings explicitly coexist (treaty amendment): the NPT bifurcates into separate regimes for horizontal (oligopoly enforcement) and vertical (reciprocal disarmament + withdrawal sovereignty).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_structural_closure, conceptual, 'Whether the kernel''s readings structurally foreclose each other or coexist under ambiguity.').

omega_variable(
    identity_locked_nws_disarmament_barrier,
    'Is NWS strategic identity (as nuclear powers, as P5 permanent members, as security guarantors) so fused with nuclear deterrence that Article VI compliance is effectively impossible without existential identity reconstruction?',
    'Post-disarmament scenario analysis by strategic communities; interview/testimony from NWS defense ministers, scientists, and strategic doctrine bodies about the relationship between nuclear weapons and state identity; observation of whether any NWS initiates genuine disarmament-preparation steps (warhead design documentation, dismantling infrastructure, transparency mechanisms) vs. rhetorical commitment only.',
    'If identity-locked: NWS exit from Article VI is identity-locked (cannot exit without repudiating core security identity), the constraint''s extraction becomes structural and permanent, NNWS should expect indefinite vertical nonproliferation without reciprocal disarmament, the reciprocal_disarmament reading becomes a permanent grievance mechanism rather than a description of operative obligation. If not identity-locked: NWS strategic identity can accommodate disarmament, the barrier is political will rather than structural, the reading''s claim that extraction is identity-locked rather than coercive is falsified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_nws_disarmament_barrier, conceptual, 'Whether NWS strategic identity prevents Article VI compliance or merely makes it politically costly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement_basis(npt__tr_t1970, observed).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(npt__tr_t1985, observed).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(npt__tr_t2000, observed).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.54).
narrative_ontology:measurement_basis(npt__tr_t2010, observed).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2020, 0.58).
narrative_ontology:measurement_basis(npt__tr_t2020, observed).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2025, 0.58).
narrative_ontology:measurement_basis(npt__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement_basis(npt__be_t1970, observed).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement_basis(npt__be_t1985, observed).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(npt__be_t2000, observed).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement_basis(npt__be_t2010, observed).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(npt__be_t2020, observed).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(npt__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement_basis(npt__su_t1970, observed).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1985, 0.32).
narrative_ontology:measurement_basis(npt__su_t1985, observed).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement_basis(npt__su_t2000, observed).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement_basis(npt__su_t2010, observed).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement_basis(npt__su_t2020, observed).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(npt__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__reciprocal_disarmament_reading, 0.18).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_authority_asymmetry).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_deterrence_modernization_doctrine).

% DUAL FORMULATION NOTE:
% The npt_treaty_1970 kernel has three structurally distinct readings, each instantiating a different constraint with different beneficiary/victim sets, different ε values, and different classifications. This story (reciprocal_disarmament_reading) claims Article VI is binding and frames the constraint as tangled_rope with extraction asymmetry (horizontal verification + vertical non-verification). The oligopoly_enforcement_reading frames Article VI as contingent and the constraint as rope (genuine coordination). The withdrawal_sovereignty_reading frames the constraint as scaffold with a contingent sunset (treaty obligation conditional on security environment). All three readings share the same kernel text but read it differently. They are linked via network.affects_constraints to enable the corpus to model the contest over treaty interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
