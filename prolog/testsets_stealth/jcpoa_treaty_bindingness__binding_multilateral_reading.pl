% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty (Consensus-Gated Modification and Dissolution)
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   A multilateral accord freezes a contested nuclear program under intrusive
 *   international verification in exchange for sequenced sanctions relief,
 *   with every term change, dispute escalation, and termination routed
 *   through a consensus gate that no single participant can open alone. The
 *   arrangement is presented by its architects as the definitive form of the
 *   bargain: binding on all parties equally, modifiable only collectively,
 *   with reimposition of prior sanctions lawful only through the agreed
 *   multilateral ladder. KEY AGENTS (by structural relationship): -
 *   joint_commission_signatories: Agenda-setter (institutional/constrained) —
 *   administers the arrangement and is self-bound by its own consensus rule -
 *   iranian_state: Primary payer with compensating receipts
 *   (powerful/constrained) — concedes capacity and access, receives relief -
 *   un_security_council: Enforcement apex and authority collector
 *   (institutional/arbitrage) — every escalation terminates at its gate -
 *   iaea_verification_body: Institutional beneficiary (institutional/mobile)
 *   — collects mandate, budget, and precedent - excluded_regional_powers:
 *   Payer without consent (powerful/trapped) — bears security externalities
 *   with no seat - us_federal_legislature: Payer (institutional/constrained)
 *   — legislative prerogative converted into a treaty-faithfulness question -
 *   russia_china_bloc: Beneficiary (powerful/mobile) — gains trade and a
 *   check on unilateral financial power - nonproliferation_policy_community:
 *   Analytical observer (analytical/analytical) — tracks verification and
 *   enforcement performance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.55).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.62).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty (Consensus-Gated Modification and Dissolution)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '7cc683fc-44f1-4500-a216-a233c1f6ffe1').
narrative_ontology:cs_kernel_codification('7cc683fc-44f1-4500-a216-a233c1f6ffe1', formalized).
narrative_ontology:cs_authority_grounding('7cc683fc-44f1-4500-a216-a233c1f6ffe1', lineage).
narrative_ontology:cs_interpretation_layer_present('7cc683fc-44f1-4500-a216-a233c1f6ffe1').
narrative_ontology:cs_reading_relation('7cc683fc-44f1-4500-a216-a233c1f6ffe1', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('7cc683fc-44f1-4500-a216-a233c1f6ffe1', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('7cc683fc-44f1-4500-a216-a233c1f6ffe1', foundational, consensus_gated_modification_and_dissolution).
narrative_ontology:cs_axiom_status(consensus_gated_modification_and_dissolution, holdable).
narrative_ontology:cs_axiom_grounding('7cc683fc-44f1-4500-a216-a233c1f6ffe1', consensus_gated_modification_and_dissolution, conventional).
narrative_ontology:cs_axiom('7cc683fc-44f1-4500-a216-a233c1f6ffe1', secondary, unilateral_reimposition_constitutes_breach).
narrative_ontology:cs_axiom_status(unilateral_reimposition_constitutes_breach, holdable).
narrative_ontology:cs_axiom_grounding('7cc683fc-44f1-4500-a216-a233c1f6ffe1', unilateral_reimposition_constitutes_breach, conventional).
narrative_ontology:cs_reference_frame('7cc683fc-44f1-4500-a216-a233c1f6ffe1', consensus_bound_treaty_order).
narrative_ontology:cs_drift_state('7cc683fc-44f1-4500-a216-a233c1f6ffe1', post_us_withdrawal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7cc683fc-44f1-4500-a216-a233c1f6ffe1', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, joint_commission_signatories).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_body).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_china_bloc).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_state).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, excluded_regional_powers).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, us_federal_legislature).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the arrangement through the Joint Commission: adjudicate compliance disputes, sequence sanctions relief against verified limits, and gate any modification or termination behind the consent of all participants. Having written the consensus rule, they are bound by their own procedure — no participant, including the largest, can alter terms alone. Exit means open breach, forfeiture of the verification regime, and bearing responsibility for the arrangement's collapse.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, joint_commission_signatories, agenda_setter,
    institutional, generational, constrained, global).

% Caps enrichment capacity, accepts intrusive continuous inspection, and ships out fissile material; in exchange receives staged sanctions relief, restored trade channels, and international legitimation. Walking away forfeits the relief, exposes the economy to coordinated reimposition through the council gate, and isolates the state — so the realistic option set is compliance, negotiated complaint through the dispute-resolution ladder, or calibrated escalation short of exit.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_state, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_state, beneficiary).

% Holds the apex enforcement gate: the arrangement's dispute-resolution ladder terminates here, and reimposition of prior council resolutions runs through its procedures. Every modification, dispute escalation, and snapback decision passes through this body, which collects procedural authority from being the mandatory final station. Its permanent members are insulated from the arrangement's costs by veto power they retain regardless of compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council, beneficiary).

% Receives the most intrusive verification mandate ever granted for a non-weapons program: continuous monitoring, environmental sampling, centrifuge surveillance, and managed access to declared and undeclared sites. The mandate expands its budget, staffing, technical precedent, and standing as the indispensable verifier. It exists independently of this arrangement and applies the accumulated precedent across its other country files.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_body, beneficiary,
    institutional, generational, mobile, global).

% Neighboring states that regard the arrangement's counterparty as an existential threat were given no seat at the negotiation and no consent right over its terms. They bear the security externalities — a legitimated, sanctions-relieved regional rival with a sunset-limited enrichment freeze — while being unable to leave the region or opt out of the consequences. Their recourse is lobbying the signatories and preparing unilateral contingencies outside the framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, excluded_regional_powers, excluded,
    powerful, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, excluded_regional_powers, payer).

% Largely sidelined by the executive-agreement form, then further constrained by the claim that the arrangement binds as a matter of multilateral obligation: statutory attempts to reimpose or renegotiate sanctions unilaterally are reframed as international breach rather than ordinary legislation. Each electoral cycle returns a chamber whose majority may oppose the terms, yet the binding frame converts that opposition into a question of treaty faithfulness.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, us_federal_legislature, payer,
    institutional, biographical, constrained, national).

% Signatories whose economies are comparatively hardened against Western secondary sanctions. They gain expanded trade with the relieved counterparty, a constraint on unilateral American financial power they have long sought, and a working demonstration that multilateral process can check great-power discretion. Their alternatives are rich — independent trade relationships, parallel financial rails — so their continued participation is choice, not captivity.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_china_bloc, beneficiary,
    powerful, generational, mobile, global).

% Technical analysts, former inspectors, and arms-control scholars who track verification data, breakout timelines, and enforcement precedents. They publish assessments of whether the inspection regime is functioning, whether dispute-resolution steps are resolving or ritualizing, and what the arrangement's fate teaches about the durability of consensus-bound instruments. They hold no vote and bear no compliance burden.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_policy_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a mutual-distrust collective-action problem: verifiable capping of a nuclear program in exchange for sequenced sanctions relief, replacing a spiraling cycle of covert enrichment, sanctions escalation, and strike threats with a single inspected framework all parties can observe. The consensus gate prevents any single party from converting the framework into a unilateral instrument.
% TRANSFER_FUNCTION: Moves enrichment capacity, fissile material, and inspection access from the sanctioned state to the multilateral verifier; moves sanctions relief, frozen assets, and trade access from the sanctioning coalition to the sanctioned state; and moves interpretive and enforcement authority upward into the Joint Commission and the council gate.
% ABSENT_VOICES: The neighboring states most exposed to the arrangement's security externalities had no seat at the table and no consent right; within signatory states, legislative bodies and publics opposed to the terms were presented with a completed instrument rather than a negotiating voice. Both would object that they are bound by, or pay for, terms they never accepted.
% DISAPPEARANCE_RATIONALE: If the arrangement and its binding frame vanished overnight, the inspection regime would lose its mandate within weeks, the sanctioning coalition's relief package would unravel, the capped program would resume unconstrained trajectory toward weapons capability, and regional powers would move from contingency planning to preemption debates — the entire sanctions-inspection-diplomacy architecture around the file would reorganize around military and unilateral instruments.
% FOUNDING_PROBLEM: A covert, expanding enrichment program discovered in the early 2000s, converging on weapons capability, with the available responses appearing to be ineffective sanctions or a preventive war — the problem was how to verifiably freeze the program short of conflict while giving the sanctioned state a face-saving path back into economic integration.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem is attested from outside the benefiting parties: the excluded neighboring states themselves — who reject the remedy — affirm the proliferation threat is real, and the verifier's safeguard reports document the program's pre-arrangement expansion independently of any signatory's diplomacy. Whether the founding problem remains LIVE in its original form is disputed between the remaining signatories, who cite post-defection program growth as proof of continuing need, and the excluded regional powers, who hold the original bargain obsolete.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and rising (0.42 to 0.55 across the interval): at adoption the bargain's reciprocity is visible and fresh, but as one major participant defects and others must honor terms the defector punishes, the residual binding frame extracts progressively more from those who stay inside — remaining signatories extend relief while their firms face secondary-sanctions exposure, and the capped state maintains limits while its relief erodes. Suppression (0.62) reflects the arrangement's designed foreclosure of alternatives: unilateral exit is reframed as breach, unilateral sanctions reimposition as illegitimate, and alternative diplomatic or military tracks as regime-breaking. Theater rises from 0.14 to 0.35 as the Joint Commission's sessions shift from operative dispute resolution toward ritual reaffirmation after the enforcement apex fractures — meetings continue, communiques issue, but a growing share of activity ratifies positions rather than resolving disputes. The suppression series is authored deliberately: this story traces enforcement-capacity change (snapback threats activated, dispute-resolution ladders escalated, secondary-sanctions pressure applied to hold the remaining coalition together), so a rising suppression trajectory is the honest picture rather than a static scalar. All three series run on one shared six-point grid; every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute fundamentally different constraints from identical text. From the council gate and the verifier's position, the arrangement is regime-building: authority, mandate, and precedent flow inward, and the consensus rule is the crown of procedural legitimacy. From the capped state's position and the excluded neighbors' position, the same instrument is a cage — one built by others, governing assets (enrichment capacity, regional security posture) they did not agree to surrender on others' terms. The legislature seat experiences a third structure: a domestic constitutional question recoded as an international-obligation question. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: the council gate collects procedural supremacy (every dispute terminates there), the verifier collects mandate and resources, the eastern bloc collects trade and a constraint on rival financial power, and the signatory collective collects the public good of a frozen program. Victim declarations map to real burden-bearers: the capped state surrenders capacity and access, the excluded neighbors absorb security externalities they never consented to, and the legislature surrenders statutory discretion to the binding frame. The capped state's dual position (payer with compensating receipts) is carried by its secondary beneficiary role — its directionality sits well short of full target because the relief side of the exchange is real while it holds. The excluded neighbors sit nearest the full-target end despite their power, because their exit options are geographic rather than political: trapped seats amplify effective extraction regardless of strength.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure coordination (rope) would erase the extraction that is structurally undeniable: non-consenting regional powers pay, a legislature's prerogative is absorbed, and the institutional layer concentrates authority in its own gate. Reading it as pure extraction (snare) would erase the genuine coordination core: the verification regime solves a real mutual-distrust problem that no bilateral instrument solved, and participants are net beneficiaries on the material ledger. The mandatrophy risk runs forward, not backward: the founding problem is contested, theater is climbing, and if the Joint Commission completes its drift into ritual reaffirmation while the enforcement apex stays fractured, the arrangement slides toward piton — maintained ceremonially by administrators who could change it but for whom the cost of fixing exceeds what they individually bear. The rising theater series is the early-warning trace of exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the jcpoa_treaty_bindingness kernel — what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three reading-stories: the transactional_provisional_reading relocates the dissolution right from the consensus gate to any party''s own bad-faith determination (collapsing the council gate''s authority and raising extraction on the capped state); the graduated_compliance_reading replaces fixed consensus-gated terms with proportionality-indexed scaling (softening the binding frame, lowering suppression, and shifting the victim set toward whichever party under-performs at each assessment).',
    'If the transactional reading were adopted, this constraint''s consensus gate dissolves as a structural element and the arrangement recomputes as a revocable exchange; if the graduated reading were adopted, the fixed-obligation structure yields to indexed adjustment and the enforcement trigger moves from collective finding to proportional assessment. The disagreement is located in the exit/dissolution mechanism and the enforcement trigger — not in the verification function, which all three readings share.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement sits.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression holding the remaining signatories in place structural (financial chokepoints, snapback threat, secondary-sanctions exposure) or internalized (multilateralist institutional identity that makes exit unthinkable)?',
    'Post-defection behavior of the remaining signatories: if exit costs were purely structural, the cheapest rational path after the largest participant defected would be coordinated renegotiation or matched exit; the observed decade of costly fidelity suggests identity fusion — the European coordinating establishments have fused their institutional self-concept with the arrangement''s survival as proof that effective multilateralism works.',
    'If suppression is substantially internalized, the constraint''s effective suppression exceeds the structural measure — the remaining parties carry the binding frame with them into successor negotiations, and breaking the identity frame (a publicly acknowledged failure of the multilateralist project) would release exits that structural analysis alone predicts should already have been taken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for the fidelity-bearing signatory seats.').

omega_variable(
    consensus_gate_defection_durability,
    'Does the consensus gate survive breach by a great-power participant, or does the binding frame hold only while all major parties consent to be held?',
    'Observe whether the remaining signatories sustain full-term performance, whether the council gate retains practical authority over reimposition, and whether third-party states continue treating the instrument as binding law rather than politics-by-other-means, across a full decade after the defection.',
    'If the gate collapses on first major breach, the constraint''s binding character was conditional on universal consent and its effective extraction profile shifts sharply toward the defector''s preferred structure; if the gate holds, the binding frame is demonstrated to bind even non-consenting defectors, validating the high-constraint authorship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_gate_defection_durability, empirical, 'Whether consensus-gated bindingness is robust to great-power defection or merely fair-weather.').

omega_variable(
    excluded_party_cost_attribution,
    'Do the security costs borne by the excluded regional powers count as extraction BY the arrangement, or as ordinary geopolitical friction that any settlement of this file would impose on someone?',
    'Counterfactual comparison across settlement structures: if every feasible instrument (military action, tighter sanctions, no deal) imposes comparable or greater costs on the same parties, the costs are friction inherent to the situation; if specifically the consensus-bound relief-for-limits structure generates costs those parties would not bear under alternatives they prefer, the costs are attributable to this arrangement''s particular shape.',
    'If attributed to the arrangement, the excluded seats raise measured extraction and strengthen the tangled-rope asymmetry; if attributed to the situation, extraction concentrates almost entirely in the consent-bearing seats and the constraint moves toward the rope end of the spectrum.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_party_cost_attribution, conceptual, 'Whether non-consenting third parties'' costs are extraction by the constraint or background friction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2017, 0.16).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2019, 0.23).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2021, 0.29).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2023, 0.32).
narrative_ontology:measurement(jcpo_tr_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2017, 0.45).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2019, 0.51).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2021, 0.53).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2023, 0.54).
narrative_ontology:measurement(jcpo_be_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2017, 0.47).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2023, 0.61).
narrative_ontology:measurement(jcpo_su_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, resource_allocation).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'JCPOA bindingness' decomposes into three structurally distinct constraint stories under the jcpoa_treaty_bindingness kernel, one per reading. This story (binding_multilateral_reading) authors epsilon for the standing consensus-bound arrangement as this reading assesses it; the sibling stories author epsilon for the same referent under their own lights, producing different victim sets, enforcement triggers, and classifications. The upstream/downstream structure runs through the shared verification function, which all three readings preserve; the readings diverge on the dissolution mechanism and the enforcement trigger. All family members are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
