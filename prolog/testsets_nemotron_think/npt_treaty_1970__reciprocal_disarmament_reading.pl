% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI Reciprocal Disarmament Obligation
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (1970) establishes a grand bargain:
 *   non-nuclear weapon states (NNWS) forgo nuclear weapons (Articles I-II),
 *   nuclear weapon states (NWS) pursue nuclear disarmament in good faith
 *   (Article VI). The reciprocal_disarmament_reading treats Article VI as a
 *   binding legal obligation with temporal urgency — not aspirational, not
 *   contingent. Horizontal and vertical nonproliferation are reciprocal: the
 *   NNWS restraint is consideration for NWS disarmament. The enforcement gap
 *   — IAEA verifies horizontal compliance exhaustively, but no verification
 *   exists for vertical disarmament — is structural injustice, not
 *   implementation detail. NWS strategic autonomy (modernization, retention)
 *   enters the victim set as constrained by an obligation they treat as
 *   aspirational; NNWS coalition gains normative leverage through TPNW and
 *   Review Conference blocking. The constraint is a tangled rope: genuine
 *   coordination (nonproliferation regime has prevented cascades) fused with
 *   asymmetric extraction (disarmament bargain unfulfilled for 50+ years).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.45).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI Reciprocal Disarmament Obligation").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '3214d377-0715-46f9-9311-8871108335ee').
narrative_ontology:cs_kernel_codification('3214d377-0715-46f9-9311-8871108335ee', formalized).
narrative_ontology:cs_authority_grounding('3214d377-0715-46f9-9311-8871108335ee', lineage).
narrative_ontology:cs_interpretation_layer_present('3214d377-0715-46f9-9311-8871108335ee').
narrative_ontology:cs_reading_relation('3214d377-0715-46f9-9311-8871108335ee', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('3214d377-0715-46f9-9311-8871108335ee', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('3214d377-0715-46f9-9311-8871108335ee', foundational, reciprocal_disarmament_obligation_binding).
narrative_ontology:cs_axiom_status(reciprocal_disarmament_obligation_binding, holdable).
narrative_ontology:cs_axiom_grounding('3214d377-0715-46f9-9311-8871108335ee', reciprocal_disarmament_obligation_binding, deontological).
narrative_ontology:cs_axiom('3214d377-0715-46f9-9311-8871108335ee', foundational, horizontal_vertical_nonproliferation_inseparable).
narrative_ontology:cs_axiom_status(horizontal_vertical_nonproliferation_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('3214d377-0715-46f9-9311-8871108335ee', horizontal_vertical_nonproliferation_inseparable, deontological).
narrative_ontology:cs_reference_frame('3214d377-0715-46f9-9311-8871108335ee', grand_bargain_1968).
narrative_ontology:cs_drift_state('3214d377-0715-46f9-9311-8871108335ee', contemporary_tpnw_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3214d377-0715-46f9-9311-8871108335ee', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nws_alliance_structures).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_modernization_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nnam_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nnam_coalition).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_nonproliferation_norm).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, disarmament_as_legal_obligation).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, reciprocal_bargain_horizontal_vertical).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized NWS (US, Russia, UK, France, China) set the treaty agenda through Review Conferences and P5 process. They benefit from the horizontal nonproliferation regime that protects their nuclear monopoly and alliance structures. They also bear the Article VI disarmament obligation — but treat it as aspirational, continuing modernization programs (life extension, new warheads, hypersonic delivery). Their exit option is arbitrage: they can reinterpret Article VI, block verification proposals, and leverage alliance commitments without leaving the treaty.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, beneficiary).

% NATO nuclear sharing arrangements and US extended deterrence commitments to allies (Japan, South Korea, Australia) depend on the NPT's legitimacy. They benefit from horizontal nonproliferation preventing more nuclear-armed adversaries. They are not directly constrained by Article VI but their security architecture assumes NWS retention. Their exit is arbitrage: they can pressure NWS on disarmament rhetoric while relying on the deterrent.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_alliance_structures, beneficiary,
    institutional, generational, arbitrage, global).

% 180+ NNWS parties forgo the nuclear option under Articles I-II. They receive security assurances (positive/negative) and access to peaceful nuclear technology (Article IV). The reciprocal benefit — NWS disarmament — has not materialized. Their exit is constrained: withdrawal under Article X triggers supreme national interest clause and severe political/isolation costs (North Korea precedent); breakout risks preventive attack and sanctions. They pay the non-acquisition cost continuously; the disarmament benefit is deferred indefinitely.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states, beneficiary).

% The strategic autonomy of NWS to determine their nuclear posture, modernization pace, and arsenal size is constrained by Article VI's legal obligation (under this reading). This is a non-agent entity representing the structural position: NWS must justify modernization against a binding disarmament obligation. The constraint extracts compliance costs (diplomatic, legal, political) from NWS strategic autonomy. Exit is constrained — they cannot disavow Article VI without collapsing the regime that protects their monopoly.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy).

% Concrete modernization programs (US W76-1/W88 Alt 370, B61-12, LRSO, GBSD; Russian Sarmat, Avangard, Poseidon; UK Warhead Replacement; French ASMPA-R; Chinese silo expansion) are structurally constrained by Article VI under this reading. Each program must be justified as consistent with 'cessation of the arms race' and 'nuclear disarmament.' The constraint extracts political capital and legal risk from these programs. Exit is constrained — programs proceed but under growing normative and legal pressure.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_modernization_programs, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__reciprocal_disarmament_reading, nws_modernization_programs).

% The Non-Aligned Movement (120 states) operates as the organized NNWS coalition. They coordinate positions at Review Conferences, block consensus on documents that weaken Article VI, and drove the TPNW process. They pay the coordination cost of maintaining coalition unity across diverse security environments. They benefit normatively from the TPNW's stigmatization of nuclear weapons and the normative leverage it creates. Exit is constrained — leaving the coalition isolates them diplomatically.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nnam_coalition, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nnam_coalition, beneficiary).

% ICAN, IPPNW, and other civil society actors are structurally excluded from NPT decision-making (state-party only). They provide the normative engine for TPNW and supply evidence for Article VI violations. They would object to the enforcement gap and modernization-as-compliance framing. Their exit is mobile — they can shift focus to TPNW, domestic politics, or public pressure campaigns.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, civil_society_disarmament_advocates, excluded,
    moderate, biographical, mobile, global).

% IAEA safeguards verify horizontal compliance (Articles I-II) with increasing intrusiveness (Additional Protocol). No mandate exists for vertical verification (Article VI). They observe the structural asymmetry: a massive verification apparatus for NNWS obligations, zero verification for NWS disarmament. Their analytical exit means they report the gap but cannot mandate its closure.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iaea_verification_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear weapons proliferation to additional states (horizontal nonproliferation) through a verified, legally binding regime with security assurances and peaceful use access — solving the collective action problem of nuclear cascade.
% TRANSFER_FUNCTION: Moves the option to acquire nuclear weapons from NNWS to NWS (horizontal), and moves the obligation to disarm from NWS to the international community (vertical). In practice, the horizontal transfer is enforced; the vertical transfer is not. The extraction flows from NNWS (foregone deterrent) to NWS (retained monopoly without reciprocal disarmament).
% ABSENT_VOICES: Populations in NNWS who bear the security cost of non-acquisition without the promised disarmament benefit; future generations who inherit the disarmament debt; states that withdrew or never joined (India, Pakistan, Israel, North Korea) whose security calculations are shaped by the regime's asymmetry but who have no voice in its governance.
% DISAPPEARANCE_RATIONALE: If the NPT and Article VI vanished overnight: horizontal proliferation constraints would collapse — multiple states (Japan, South Korea, Saudi Arabia, Turkey, others) would likely pursue nuclear weapons within years; NWS would lose the legal basis for their monopoly and face uncontrolled proliferation; the IAEA verification regime would lose its treaty mandate; the TPNW would lose its reference framework. The world nuclear order would fundamentally reorganize.
% FOUNDING_PROBLEM: The 1960s fear of uncontrolled nuclear proliferation: 20-30 nuclear weapon states predicted by 1970s, creating unstable multipolar deterrence and high probability of nuclear use. The NPT was built to cap the number of nuclear-armed states at five while committing those five to eliminate their arsenals.
% FOUNDING_PROBLEM_CORROBORATION: NWS attest the proliferation problem remains live (Iran, North Korea, potential cascades) — the horizontal pillar's success proves the problem persists. NNWS coalition (NAM, TPNW states) and independent commissions (WMD Commission 2006, Evans-Kawaguchi Commission 2009) attest the disarmament pillar's failure means the founding bargain is broken — the problem has shifted from 'too many nuclear states' to 'nuclear-armed states not disarming.' The 1996 ICJ advisory opinion (unanimous on Article VI obligation) corroborates the binding character from outside the beneficiary set.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the accumulated gap between Article VI's promise and NWS practice: arsenals reduced from Cold War peaks but modernization continues, new warhead types developed, disarmament deadlines missed. Suppression (0.45) is structural — the treaty's legitimacy depends on the reciprocal bargain holding; when NNWS perceive bad faith, the regime's coercive power against horizontal proliferation weakens (Iran, North Korea cases). Theater ratio (0.38) captures Review Conference rituals: consensus documents reaffirming Article VI while NWS modernization programs advance. Accessibility collapse (0.48) — alternatives exist (withdrawal under Article X, TPNW, breakout) but carry extreme security and political costs. Resistance (0.55) — NNWS coalition (NAM, TPNW states) actively contests the extraction through procedural obstruction and parallel treaty.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS agenda-setter seat (oligopoly_enforcement_reading), the constraint is a rope: horizontal coordination works, vertical is aspirational. From the NNWS payer seat (reciprocal_disarmament_reading), the same structure is a snare: they paid (non-acquisition) and received nothing (disarmament). The engine computes this divergence from the declared beneficiary/victim structure — NWS appear in both arrays, NNWS appear as victims. The claimed_type (tangled_rope) reflects the authoring seat's judgment that both coordination and extraction are real and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are primary beneficiaries of horizontal nonproliferation (their monopoly protected) but become payers/victims under Article VI's disarmament obligation — their strategic autonomy is constrained by a legal obligation they treat as non-binding. This dual position is the structural signature of the reciprocal reading. NNWS are payers on the non-acquisition side (foregone deterrent option) and intended beneficiaries on the disarmament side — but the benefit is unrealized, making them net payers. The NNWS coalition (organized, generational horizon, constrained exit) gains normative leverage as the enforcement gap becomes visible. NWS alliance structures (NATO nuclear sharing) are secondary beneficiaries — extended deterrence relies on the regime's legitimacy. IAEA and verification bodies are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear war through controlled nonproliferation + disarmament) is contested: NWS say the problem is live (proliferation risks persist), NNWS say the problem has shifted (disarmament debt now the primary risk). The mandate has atrophied on the vertical pillar — the reciprocal bargain's consideration has failed. This is not a piton (no theatrical maintenance of a dead function); the horizontal function remains live and enforced. The mandatrophy is asymmetric: one pillar atrophied, the other hardened. The classification (tangled_rope) captures this precisely — it is not a degraded rope (piton) because the coordination function is actively maintained and valued by all parties; it is not a snare because the coordination is genuine, not cover. The extraction is the gap between the two pillars.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status,
    'Is Article VI a legally binding obligation of result (disarmament) or an obligation of conduct (good faith negotiation)?',
    'ICJ advisory opinion (1996) parsing; subsequent state practice and NPT Review Conference consensus language; treaty interpretation under VCLT Articles 31-32.',
    'If obligation of result, NWS modernization and retention constitute ongoing violation; if obligation of conduct, the enforcement gap is procedural not substantive. Changes extraction classification from structural injustice to implementation deficit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_status, conceptual, 'Legal character of Article VI — binding result vs. binding process').

omega_variable(
    enforcement_gap_nature,
    'Is the absence of Article VI verification machinery an implementation oversight or a structural feature that benefits NWS?',
    'Compare IAEA verification architecture for Articles I-II (horizontal) vs. Article VI (vertical): asymmetry in resources, mandate, and institutional design. Historical record of NWS opposition to vertical verification proposals.',
    'If structural feature, the constraint is a snare for NNWS — coordination cover for extraction. If oversight, it is a tangled_rope with a remediable gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_gap_nature, empirical, 'Whether the vertical verification vacuum is designed or accidental').

omega_variable(
    nws_modernization_as_violation,
    'Do NWS nuclear modernization programs (life extension, new warheads, delivery systems) constitute material breach of Article VI?',
    'Legal analysis of ''cessation of the nuclear arms race'' and ''nuclear disarmament'' against modernization facts; NWS declarations at review conferences; NNWS coalition statements.',
    'If modernization = violation, extractiveness rises and NWS move from beneficiary to payer/victim in the structural accounting. If modernization = permitted, the bargain holds but extraction accumulates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_modernization_as_violation, conceptual, 'Whether qualitative arms racing violates the reciprocal bargain').

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested NPT kernel, and what would sibling readings change structurally?',
    'Compare the three declared readings on beneficiary/victim sets, claimed_type, and extraction referent. The oligopoly_enforcement_reading centers horizontal nonproliferation as primary; the withdrawal_sovereignty_reading centers Article X as escape valve.',
    'Each reading instantiates a different constraint with different ε, different stakeholders, different classification. The kernel_id npt_treaty_1970 admits multiple ε-invariant constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this reading of npt_treaty_1970 kernel; siblings are oligopoly_enforcement_reading and withdrawal_sovereignty_reading').

omega_variable(
    nnws_coalition_leverage,
    'Does the NNWS coalition (NAM, TPNW proponents) possess structural leverage to enforce reciprocity, or is its leverage purely normative?',
    'Track NPT Review Conference outcomes, TPNW ratification trajectory, NWS security assurance commitments, and whether NNWS withdrawal threats (e.g., Iran, North Korea precedent) create material pressure.',
    'If leverage is only normative, extraction persists without correction mechanism — piton drift. If structural (withdrawal credibility, TPNW stigmatization), the constraint may shift toward scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_coalition_leverage, empirical, 'Whether NNWS normative leverage translates to structural correction capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t0, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t10, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t20, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t30, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t40, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t50, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t0, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t10, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t20, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t30, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t40, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t50, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t0, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t10, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t20, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t30, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t40, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t50, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__reciprocal_disarmament_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, ianea_verification_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, tpnw_2017).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, nws_modernization_programs).

% DUAL FORMULATION NOTE:
% NPT kernel family: three readings of one treaty. ehrenfest_barrier equivalent is the physical reality of nuclear weapons (mountain). bgs_spectral_universality equivalent is the horizontal nonproliferation regime (rope/tangled_rope with low extraction). bgs_eigenvector_thermalization equivalent is the vertical disarmament obligation (tangled_rope/snare with high extraction). This reading centers the vertical pillar as binding; the oligopoly reading centers the horizontal pillar; the withdrawal reading centers the exit right.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__reciprocal_disarmament_reading, institutional, 0.35).
constraint_indexing:directionality_override(npt_treaty_1970__reciprocal_disarmament_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
