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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Determination
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA is contested on its bindingness. This constraint story
 *   instantiates the transactional_provisional_reading: the agreement is
 *   framed as a revocable executive arrangement, not a binding multilateral
 *   treaty. Under this reading, any signatory can unilaterally determine Iran
 *   has committed bad faith and withdraw, reimposing sanctions without
 *   requiring consensus or dispute resolution. This reading privileges
 *   individual state sovereignty and domestic political coalitions that
 *   opposed the deal over the multilateral constraint imposed by the
 *   agreement. The constraint is CLAIMED as a snare because the transactional
 *   reading grants beneficiary states (especially the US) unilateral veto
 *   power while imposing asymmetric restraint on Iran, and suppression is
 *   high because the framework's bindingness depends on active enforcement by
 *   signatories to maintain the sanctions regime in the event of unilateral
 *   withdrawal.
 *
 * KEY AGENTS:
 *   - United States Executive — sets the exit condition and interprets bad faith unilaterally
 *   - Iran — bears nuclear program restraint and is subject to sanctions based on subjective good-faith determination
 *   - European Signatories — financially bound by US sanctions even if they assess Iran compliant
 *   - Domestic Hawkish Coalitions — benefit from reduction in constraint on executive action against Iran
 *   - IAEA — provides technical reporting but has no authority over the binding question
 *   - UN Security Council — structurally excluded from the framework authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.71).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, snare).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Determination").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '97b1debb-2987-4609-adab-4feedb481179').
narrative_ontology:cs_kernel_codification('97b1debb-2987-4609-adab-4feedb481179', fixed_text).
narrative_ontology:cs_authority_grounding('97b1debb-2987-4609-adab-4feedb481179', extraction).
narrative_ontology:cs_interpretation_layer_present('97b1debb-2987-4609-adab-4feedb481179').
narrative_ontology:cs_reading_relation('97b1debb-2987-4609-adab-4feedb481179', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('97b1debb-2987-4609-adab-4feedb481179', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('97b1debb-2987-4609-adab-4feedb481179', foundational, state_unilateral_exit_authority_supreme).
narrative_ontology:cs_axiom_status(state_unilateral_exit_authority_supreme, holdable).
narrative_ontology:cs_axiom_grounding('97b1debb-2987-4609-adab-4feedb481179', state_unilateral_exit_authority_supreme, deontological).
narrative_ontology:cs_axiom('97b1debb-2987-4609-adab-4feedb481179', foundational, bad_faith_subjective_determination_sufficient).
narrative_ontology:cs_axiom_status(bad_faith_subjective_determination_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('97b1debb-2987-4609-adab-4feedb481179', bad_faith_subjective_determination_sufficient, instrumental).
narrative_ontology:cs_reference_frame('97b1debb-2987-4609-adab-4feedb481179', transactional_state_sovereignty).
narrative_ontology:cs_drift_state('97b1debb-2987-4609-adab-4feedb481179', post_us_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97b1debb-2987-4609-adab-4feedb481179', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, individual_state_sovereignty).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_hawkish_coalitions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multinational_signatories).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, global_non_proliferation_regime).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, european_signatories).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, european_signatories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the JCPOA as a sitting president; retains the executive power to declare Iran in material breach and reimpose sanctions unilaterally, overriding multilateral consensus. The transactional reading grants the executive a veto on continuation regardless of other signatories' assessment of Iranian compliance. A successor administration can reframe the agreement's binding force and exit without collective approval.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive, beneficiary).

% Bears the substantive restraint: nuclear program limitations, inspections regime, centrifuge restrictions. Under this reading, compliance is conditional—Iran must not only meet technical terms but must satisfy subjective assessments of 'good faith' by individual signatories. A unilateral determination of bad faith (real or claimed) voids the agreement and triggers automatic sanctions reimposition, regardless of Iran's actual technical compliance or multilateral dispute resolution.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iran, payer,
    powerful, generational, identity_locked, global).

% Signed the JCPOA as co-architects; under the transactional reading, their signature does not bind the framework against unilateral US exit. They bear the economic cost of sanctions alignment or resistance (reputational, compliance, trade friction). Their ability to keep the agreement alive independently is constrained—if the US unilaterally exits, the lifting of secondary sanctions collapses even if Europe maintains JCPOA compliance, because US financial and trade dominance can isolate European signatories from global markets.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, european_signatories, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, european_signatories, beneficiary).

% Opposed the JCPOA as a constraint on executive action against Iran. The transactional reading vindicates their position: the agreement is not binding law but a revocable executive arrangement. A unilateral exit (or threat of it) strengthens their negotiating position domestically and their ability to influence foreign policy toward Iran confrontation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_hawkish_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Operates the technical inspection regime and reports on Iranian compliance. Under this reading, the IAEA's factual reporting (whether Iran is technically compliant) is decoupled from the binding force of the agreement—a signatories' unilateral determination of bad faith can override the IAEA's own compliance assessment, making the inspectorate's judgments advisory rather than determinative.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea, observer,
    institutional, generational, analytical, global).

% Is bypassed by the transactional reading: the framework explicitly removed the JCPOA from UN Security Council authority (UNSC Res. 2231 merely endorses it; the agreement itself is not a Security Council decision). A unilateral exit need not be contested or approved at the Security Council; the framework grants individual signatories unilateral authority to declare bad faith and reimpose sanctions, which the UNSC cannot veto.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, un_security_council, excluded,
    institutional, generational, trapped, global).

% A doctrine, not an actor: the norm that major powers will honor multilateral arms-control commitments. The transactional reading treats bilateral good-faith assessment as superior to multilateral binding force, which erodes the norm's authority. Future agreements cannot claim binding force if signatories retain unilateral exit authority.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_non_proliferation_norms, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__transactional_provisional_reading, international_non_proliferation_norms).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents Iranian nuclear weapons development and provides multilateral monitoring and sanctions relief in exchange for verifiable constraints on the nuclear program. Solves a collective action problem: major powers and Iran coordinate on a single inspections regime and sanctions-relief schedule rather than unilateral enforcement.
% TRANSFER_FUNCTION: Iran transfers nuclear program constraints and inspections access; in return, signatories transfer sanctions relief and economic normalization. The transactional reading reframes this as a revocable transaction: if any signatory determines Iran has committed bad faith (a subjective standard), the signatories can unilaterally cancel the sanctions-relief side while demanding Iran continue the constraints, or else reimpose sanctions for non-compliance with a retroactively voided agreement.
% ABSENT_VOICES: The UN Security Council is structurally excluded from the binding authority structure—JCPOA is deliberately a side agreement outside UNSC authority, so the Council cannot formally defend the agreement or mediate breach. Non-state actors (NGOs, disarmament advocates, Iranian civil society) are excluded from the framework's decision structure; they cannot contest unilateral determinations of bad faith or propose dispute resolution. The IAEA's technical judgment is excluded from the binding force question: the organization can report compliance, but signatories retain unilateral authority to declare bad faith regardless of the IAEA's assessment.
% DISAPPEARANCE_RATIONALE: If the JCPOA (including this transactional reading of its bindingness) vanished, Iran would have no constraints on nuclear development and could pursue weapons capabilities openly; signatories would face the choice to enforce constraints unilaterally through military means or accept Iranian weapons development. The agreement's disappearance forces the return to the pre-2015 bargaining problem it was designed to solve: individual enforcement, proxy conflict, and the risk of nuclear escalation.
% FOUNDING_PROBLEM: After Iran's 2002 disclosure of uranium enrichment facilities, international negotiations toward Iranian nuclear restraint repeatedly failed. The founding problem was how to credibly constrain Iran's nuclear program when no single power could enforce constraints unilaterally without triggering conflict escalation, yet Iran did not accept permanent, irreversible constraints. The JCPOA was constructed to solve this: verify Iran's compliance through independent inspection, offer graduated sanctions relief as Iran meets milestones, and make the agreement reciprocal so all parties bore equivalent restraint and could exit if the other side violated.
% FOUNDING_PROBLEM_CORROBORATION: The Obama administration (the JCPOA's architects) attests the founding problem was a stalemate in nuclear negotiations and ongoing Iranian weapons development; they claim the agreement solved the problem by creating enforceable constraints. The Trump administration and allied hawkish coalition attest the founding problem persists: Iran's alleged 'fundamental hostility' to the West makes any negotiated restraint temporary and unreliable, so the agreement was doomed and unilateral exit authority was always necessary. The IAEA's technical reporting shows Iran met initial milestones; Iranian officials attest the US violated the agreement by reimposing sanctions and that the founding problem of Western coercion persists. Independent analysts and European signatories dispute whether Iran has violated key provisions or merely engaged in marginal non-compliance as retaliation for US sanctions.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness score of 0.68 reflects the asymmetric power to exit: the US executive can unilaterally declare bad faith and reimpose sanctions (extraction) while Iran must satisfy subjective compliance standards it does not control (suppression). The suppression score of 0.71 accounts for the fact that the framework's persistence depends on active enforcement—signatories must maintain secondary sanctions and coordinate on exclusion of Iran from global financial systems. Theater ratio of 0.42 indicates moderate performative activity: the JCPOA's stated coordination function (prevent weapons development through multilateral verification) is real, but a growing share of the framework's enforcement activity serves to vindicate unilateral exit authority and domestic coalition opposition rather than genuine multilateral constraint. The measurement series shows extractiveness and suppression rising over the interval (from 0.52 to 0.68 and 0.58 to 0.71 respectively), tracking the increasing frequency of unilateral breach accusations, secondary sanctions enforcement, and the erosion of the agreement's stabilizing function as individual signatories prioritize exit options over reciprocal constraint.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Iran, European signatories) and the agenda-setter seat (US executive) should compute different types. From Iran's perspective—identity-locked to its nuclear program and vulnerable to sanctions—the arrangement is a snare: the constraint is asymmetric, the exit determination is unilateral, and compliance is judged by standards Iran does not control. From the US executive's perspective—mobile, setting the rules, controlling exit—the arrangement is a rope (coordination) or even a scaffold (transitional, subject to renegotiation). The engine should compute this divergence from the structural data; the authored claim does not reconcile the perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The US executive occupies a low-directionality (beneficiary) seat under this reading: it sets the bad-faith standard, controls exit timing, and faces minimal cost for unilateral withdrawal. Iran occupies a high-directionality (target) seat: it bears the substantive nuclear restraint, submits to inspections, and loses sanctions relief if any signatory declares bad faith. European signatories occupy a moderate-to-high seat: they nominally benefit from the agreement (no Iranian weapons development) but are constrained by US financial dominance; if the US exits, they cannot sustain the agreement unilaterally. The domestic hawkish coalition has a low-directionality seat (beneficiary): they opposed the constraint and benefit from restoration of unilateral executive action. The directionality derivation chain runs from the base structural relationship (who sets rules, who bears restraint) through the exit options (US mobile, Iran identity-locked to regional position, Europe constrained by financial integration) to the effective extraction calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint presents a clear mandatrophy signal: the founding problem (how to credibly constrain Iranian weapons development without unilateral enforcement) is contested and potentially dead, while the arrangement persists through active enforcement of exit authority rather than reciprocal restraint. If Iran is compliant (per IAEA reporting) but the US nonetheless exits and reimpose sanctions, the founding coordination function has atrophied and the constraint's persistence depends on suppression of Iran and of signatories who wish to maintain the agreement. This is a candidate piton or snare: the question is whether the constraint persists because it solves the founding problem or because it vindicates the beneficiary coalition's preference for unilateral action. The transactional reading inverts the mandatrophy test: mandatrophy is resolved by the explicit ascendance of individual sovereignty over multilateral bindingness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bad_faith_standard_ambiguity,
    'What constitutes ''bad faith'' that triggers unilateral withdrawal under the transactional reading? Is it technical non-compliance, intent to develop weapons, violation of the spirit of the agreement, or the unilateral determination of a signatory?',
    'Explicit declaration by a signatory of what factual or behavioral criteria constitute bad faith; comparison of stated criteria to actual enforcement decisions (do the decisions track the stated standard or override it on political grounds).',
    'If bad faith is defined as objective technical non-compliance, the reading retains some binding force tied to verifiable facts. If bad faith is defined as a unilateral determination by any signatory, the reading grants maximum exit discretion and the constraint becomes a snare. If the stated standard diverges from enforcement decisions, the constraint operates as theatrical justification for predetermined exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bad_faith_standard_ambiguity, conceptual, 'Whether bad faith is an objective standard or a unilateral determination.').

omega_variable(
    multilateral_vs_unilateral_supremacy,
    'Does the JCPOA''s text and negotiating history establish a binding multilateral commitment, or does the agreement itself reserve individual state sovereignty as superior to collective enforcement?',
    'Textual analysis of the JCPOA preamble, annexes, and dispute resolution mechanisms; comparison to other multilateral treaties (NPT, CWC) to establish whether the JCPOA''s silence on enforcement hierarchy indicates delegation to signatories or implicit assumption of mutual reciprocity.',
    'If the agreement implicitly assumes mutual reciprocity and binding multilateral force, the transactional reading misreads the kernel and the constraint should be reclassified as a rope or tangled_rope from a binding-multilateral seat. If the agreement is deliberately silent on enforcement hierarchy and reserves state sovereignty, the transactional reading is structurally accurate and the constraint is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_vs_unilateral_supremacy, conceptual, 'Whether the JCPOA is implicitly binding multilateral or explicitly transactional.').

omega_variable(
    iran_compliance_assessment_dispute,
    'Has Iran materially violated the JCPOA''s technical provisions, or has Iran engaged in marginal non-compliance as retaliation for the US withdrawal and secondary sanctions reimposition?',
    'Independent technical analysis by the IAEA and third-party experts comparing Iranian actions to the JCPOA text; timeline correlation between US sanctions reimposition (May 2018) and Iranian escalation (uranium enrichment increases post-July 2019) to establish causality.',
    'If Iran''s escalation is response to breach, the transactional reading''s founding problem (how to constrain a hostile state) is self-fulfilling: the reading''s implementation (unilateral exit, sanctions reimposition) triggered the Iranian violation it was designed to prevent. If Iran violated first, the transactional reading''s extraction is justified as enforcement of a real commitment by a bad-faith actor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iran_compliance_assessment_dispute, empirical, 'Whether Iran''s non-compliance is original or retaliatory.').

omega_variable(
    reading_selection_kernel_contest,
    'Why does this constraint story instantiate the transactional_provisional_reading rather than the binding_multilateral_reading or graduated_compliance_reading?',
    'The SCOPE manifest selected this reading as one axis for investigation. The selection reflects a committer question: what is the structural effect of treating treaty bindingness as transactional rather than multilateral? This omega documents the reading as authored, not as a verdict on which reading is correct.',
    'The transactional reading and its structure are analyzed independently of whether they reflect the ''true'' JCPOA intent or the ''better'' legal interpretation. The sibling readings, if authored, will carry their own structural data and omega variables. The corpus compares the three constraints'' metrics and classifications to measure how strongly the reading selection drives the structural outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_kernel_contest, preference, 'The committer frame: this reading is one of three equally valid instantiations of the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(jcpo_tr_t0, observed).
narrative_ontology:measurement(jcpo_tr_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 3, 0.33).
narrative_ontology:measurement_basis(jcpo_tr_t3, observed).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement_basis(jcpo_tr_t6, observed).
narrative_ontology:measurement(jcpo_tr_t12, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(jcpo_tr_t12, observed).
narrative_ontology:measurement(jcpo_tr_t18, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement_basis(jcpo_tr_t18, observed).
narrative_ontology:measurement(jcpo_tr_t24, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(jcpo_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(jcpo_be_t0, observed).
narrative_ontology:measurement(jcpo_be_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(jcpo_be_t3, observed).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(jcpo_be_t6, observed).
narrative_ontology:measurement(jcpo_be_t12, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(jcpo_be_t12, observed).
narrative_ontology:measurement(jcpo_be_t18, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement_basis(jcpo_be_t18, observed).
narrative_ontology:measurement(jcpo_be_t24, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(jcpo_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(jcpo_su_t0, observed).
narrative_ontology:measurement(jcpo_su_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 3, 0.63).
narrative_ontology:measurement_basis(jcpo_su_t3, observed).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(jcpo_su_t6, observed).
narrative_ontology:measurement(jcpo_su_t12, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(jcpo_su_t12, observed).
narrative_ontology:measurement(jcpo_su_t18, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement_basis(jcpo_su_t18, observed).
narrative_ontology:measurement(jcpo_su_t24, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(jcpo_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% The JCPOA_treaty_bindingness kernel decomposes into three structurally distinct constraints, one per reading of what bindingness means. The transactional_provisional_reading instantiates individual state sovereignty and unilateral exit authority as superior to multilateral constraint. The binding_multilateral_reading treats the JCPOA as a binding collective commitment requiring consensus to modify or exit. The graduated_compliance_reading frames the JCPOA as reciprocal and graduated, with enforcement tied to proportional compliance assessment. Each reading authors different ε values, beneficiary/victim structures, and types. The three constraints are linked via network.affects_constraints to enable corpus analysis of how strongly the reading selection drives the structural classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
