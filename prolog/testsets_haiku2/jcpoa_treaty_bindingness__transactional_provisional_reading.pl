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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: JCPOA Treaty Bindingness: Transactional Provisional Reading
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA is a multilateral agreement constraining Iranian nuclear
 *   development through IAEA verification and conditional sanctions relief.
 *   Under the transactional-provisional reading instantiated here, the
 *   agreement is not a binding multilateral treaty requiring consensus-based
 *   modification or dissolution, but rather a provisional transactional
 *   framework that any signatory can exit unilaterally upon a determination
 *   of bad faith. This reading favors states (particularly those opposing the
 *   deal domestically) that retain the authority to withdraw, reimpose
 *   sanctions, and reinterpret compliance without multilateral consent. The
 *   referent is the standing arrangement as the transactional reading
 *   understands it: a temporary framework anchored to individual state
 *   judgment rather than institutional consensus.
 *
 * KEY AGENTS:
 *   - Unilateral withdrawal states: institutional agents that control the determination of bad faith and carry no cost for reassessment; beneficiaries of the reading's low friction for exit.
 *   - Domestic anti-deal coalitions: organized actors who pressure their governments to interpret Iranian actions as bad faith; benefit from the reading's shift from consensus to unilateral determination.
 *   - Iran: powerful agent constrained by JCPOA verification but facing unilateral withdrawal risk; any ambiguous dual-use action can trigger exit without consensus-based protection.
 *   - Signatories dependent on agreement stability (EU, Russia, China): institutional agents paying the cost of unilateral withdrawal through sanctions reimposition and disrupted commercial relationships.
 *   - IAEA: institutional agent providing verification but losing independence when unilateral determinations of bad faith override technical findings.
 *   - Regional non-proliferation framework: analytical seat observing the precedent-weakening effect of unilateral exit authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.72).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, snare).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA Treaty Bindingness: Transactional Provisional Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '2ae16220-e055-431a-b44f-b593950b2f9e').
narrative_ontology:cs_kernel_codification('2ae16220-e055-431a-b44f-b593950b2f9e', fixed_text).
narrative_ontology:cs_authority_grounding('2ae16220-e055-431a-b44f-b593950b2f9e', lineage).
narrative_ontology:cs_interpretation_layer_present('2ae16220-e055-431a-b44f-b593950b2f9e').
narrative_ontology:cs_reading_relation('2ae16220-e055-431a-b44f-b593950b2f9e', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ae16220-e055-431a-b44f-b593950b2f9e', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('2ae16220-e055-431a-b44f-b593950b2f9e', foundational, unilateral_withdrawal_sovereignty_preserved).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_sovereignty_preserved, holdable).
narrative_ontology:cs_axiom_grounding('2ae16220-e055-431a-b44f-b593950b2f9e', unilateral_withdrawal_sovereignty_preserved, deontological).
narrative_ontology:cs_axiom('2ae16220-e055-431a-b44f-b593950b2f9e', foundational, bad_faith_determination_unilateral).
narrative_ontology:cs_axiom_status(bad_faith_determination_unilateral, holdable).
narrative_ontology:cs_axiom_grounding('2ae16220-e055-431a-b44f-b593950b2f9e', bad_faith_determination_unilateral, empirically_contingent).
narrative_ontology:cs_reference_frame('2ae16220-e055-431a-b44f-b593950b2f9e', nation_state_treaty_discretion).
narrative_ontology:cs_drift_state('2ae16220-e055-431a-b44f-b593950b2f9e', post_2018_withdrawal_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2ae16220-e055-431a-b44f-b593950b2f9e', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, unilateral_withdrawal_states).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_anti_deal_coalitions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, signatories_dependent_on_agreement_stability).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_infrastructure).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_non_proliferation_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iran).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Any signatory state can unilaterally determine a violation has occurred, declare bad faith, and withdraw, reimposing sanctions immediately without requiring consensus from other signatories. Controls the threshold for bad faith determination and carries no cost for reassessment. Justifies withdrawal as defending national security interests and responses to Iranian non-compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, unilateral_withdrawal_states, agenda_setter,
    institutional, biographical, arbitrage, global).

% Oppose the agreement as strategically risky or insufficiently punitive. Under the transactional reading, they can pressure their government to interpret ambiguous Iranian actions as bad faith and exit, without the institutional friction of consensus-based processes. The reading shifts power from institutional supermajority requirements to domestic coalitions with exit authority.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_anti_deal_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Bound by JCPOA verification and sanctions-relief conditions, but faces unilateral withdrawal risk. Any ambiguous action (centrifuge testing, ballistic missile development, precision engineering justified as civilian) can be unilaterally reinterpreted as bad faith. Sanctions reimposition follows the interpretation, not a consensus determination. The framework provides no procedural protection against withdrawal triggered by political reassessment in a withdrawing state.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iran, payer,
    powerful, generational, constrained, global).

% European signatories, Russia, and China benefit from the agreement's stability and sanctions-relief provisions as instruments of trade and diplomatic engagement. Unilateral withdrawal by one signatory destabilizes the entire framework; they can object but cannot prevent withdrawal. Incur costs when sanctions reimposition disrupts established commercial relationships and diplomatic infrastructure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, signatories_dependent_on_agreement_stability, payer,
    institutional, generational, constrained, global).

% Operates under JCPOA to provide verification of Iranian compliance. Under the transactional reading, a state's unilateral bad-faith determination can override IAEA technical findings, undermining the verification framework's independence. Faces institutional pressure when determinations of violation precede or overrule technical assessment.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_infrastructure, payer,
    institutional, generational, constrained, global).

% The transactional reading weakens the precedent that multilateral non-proliferation commitments require consensus-based compliance assessment. Each signatory's unilateral exit capacity incentivizes arms-race dynamics in the region; weaker signatories lose the institutional protection the multilateral framework provided.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_non_proliferation_framework, payer,
    analytical, civilizational, trapped, regional).

% Opposed the JCPOA from inception and advocate unilateral withdrawal. They are excluded from direct signatories' decision process but historically have leverage through US Senate and executive authority. The transactional reading aligns with their preferred interpretation: the agreement is a temporary arrangement vulnerable to bad-faith reinterpretation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, us_domestic_skeptics, excluded,
    organized, biographical, mobile, national).

% Assesses the strategic consequences of unilateral withdrawal authority. They observe that the transactional reading amplifies withdrawal incentives relative to multilateral readings where consensus friction delays or prevents exit.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, us_diplomatic_establishment, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, unilateral_withdrawal_states).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for verifying Iranian nuclear compliance through IAEA inspection, conditioning sanctions relief on observed non-proliferation behavior, and enabling multilateral diplomacy without immediate war-risk escalation.
% TRANSFER_FUNCTION: Moves sanctions-relief benefits to Iran conditional on JCPOA compliance; moves withdrawal authority to individual signatories who can unilaterally reimpose sanctions upon bad-faith determination; transfers the power to define bad faith from consensus-based assessment to individual state determination.
% ABSENT_VOICES: Weaker non-aligned states dependent on non-proliferation frameworks are structurally excluded from defining bad faith. Iran's interpretation of its own compliance obligations is not a seat at the table; the framework allows unilateral reinterpretation by external parties. International law scholars and non-proliferation specialists who view transactional readings as destabilizing are not signatories and cannot prevent withdrawal.
% DISAPPEARANCE_RATIONALE: If this reading and its unilateral withdrawal authority disappeared—if the framework instead required consensus-based bad-faith determination—the signatory states could not immediately reimpose sanctions on unilateral interpretation. Iran would face only those consequences the multilateral framework collectively authorized. Regional non-proliferation incentives would shift; weaker states would lose the institutional protection they currently forfeit to unilateral withdrawal powers.
% FOUNDING_PROBLEM: The need to prevent Iranian nuclear weapons development through verified constraints and to create diplomatic off-ramps from escalatory conflict through conditional sanctions relief.
% FOUNDING_PROBLEM_CORROBORATION: Supporters of the transactional reading attest that Iranian non-compliance with ballistic missile development and precision-engineering projects indicates bad faith and justifies withdrawal. Multilateral-reading supporters (European signatories, Russia, China, non-proliferation scholars) attest that ambiguous dual-use activities do not constitute JCPOA violations and that unilateral withdrawal authority destabilizes the framework without requiring consensus-based assessment. The IAEA has not technically certified Iranian violations that would justify exit under unified standards; the reading divergence is not about technical fact but about who interprets and whether bad faith requires multilateral agreement or unilateral determination.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) because the reading concentrates the power to determine bad faith and trigger exit in individual state hands rather than requiring multilateral consensus; Iran and agreement-dependent signatories absorb the cost of this concentrated exit authority. Suppression is also high (0.72) because maintaining the unilateral determination power requires active policing: states must be able to interpret ambiguous Iranian actions as violations without multilateral challenge, and agreement-dependent signatories must suppress their preference for consensus-based procedures. Theater is moderate (0.41) because the framework maintains real verification infrastructure and genuine compliance monitoring, but an increasing share of enforcement activity defends the withdrawal authority itself rather than the original non-proliferation function. Accessibility collapse is lower (0.48) than would appear in a pure snare: alternative interpretations (multilateral readings) remain live among signatories and are actively contested; the transactional reading has not monopolized understanding. Resistance is high (0.71) because multilateral signatories actively resist unilateral withdrawal authority through diplomatic pressure and calls for consensus-based modification. The measurement series show rising extractiveness and suppression requirement as the reading's authority to reinterpret compliance solidifies over time; theater ratio stabilizes as the framework settles into its extractive pattern.
 *
 * PERSPECTIVAL GAP:
 *   From the withdrawal state's perspective, the transactional reading protects national sovereignty and deters Iranian bad faith through credible exit threat. From Iran's perspective, the reading removes procedural protections against reinterpretation and allows political reassessment to override technical verification. From agreement-dependent signatories' perspective, the reading weakens their bargaining power and exposes them to unilateral sanctions reimposition. The engine computes these divergent types from the structural data: the withdrawal state seat will compute as rope or tangled-rope (coordination + flexibility), while the Iran and constraint-dependent seats compute as snare or tangled-rope (extraction + suppression). The authored claim (snare) reflects the structure as the reading itself instantiates it—a framework whose persistence depends on the withdrawal state's unilateral determination authority and whose beneficiaries are concentrated.
 *
 * DIRECTIONALITY LOGIC:
 *   Unilateral withdrawal states are structural beneficiaries (d ≈ 0.15): they collect the power to exit, reinterpret, and reimpose sanctions. Domestic anti-deal coalitions are beneficiaries (d ≈ 0.20): they gain influence over their government's withdrawal decision. Iran is a target (d ≈ 0.85): constrained by verification and vulnerable to unilateral exit; every ambiguous action can be reinterpreted as violation. Signatories dependent on agreement stability are targets (d ≈ 0.80): they absorb the cost when unilateral withdrawal destabilizes the framework. The IAEA is a constrained payer (d ≈ 0.65): its technical independence is subordinated to unilateral bad-faith determination. The regional framework is a diffuse payer (d ≈ 0.75): weakened precedent and lost institutional protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—preventing Iranian nuclear weapons through verified constraints and diplomatic off-ramps—remains structurally live: IAEA inspections continue, Iran is constrained, escalation is averted. Under a binding-multilateral reading, consensus-based modification would preserve this function with lower extraction cost to agreement-dependent signatories. Under the transactional reading, the unilateral withdrawal authority persists even when the founding problem is under control, because the power to exit benefits withdrawal states and their domestic coalitions independently of non-proliferation outcomes. This is a mandatrophy candidate: the arrangement's founding function (verified non-proliferation) is separable from its extractive function (concentrated exit authority for domestic coalition benefit). The reading does not declare mandatrophy resolved; instead, it generates the structural conditions for mandatrophy to emerge if the founding problem stabilizes while extraction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bad_faith_determination_criteria,
    'What constitutes bad faith in the transactional reading? Is it IAEA-technical violation, Iranian intent to develop weapons, ambiguous dual-use activity, or political reassessment by the withdrawing state?',
    'Examine actual bad-faith determinations by withdrawing states: do they cite IAEA findings, technical violations, or policy reversals? Compare determinations across signatories to establish whether a shared standard or unilateral interpretation controls.',
    'If bad-faith determination is anchored to IAEA technical findings, the transactional reading converges toward graduated-compliance logic. If anchored to political reassessment, extractiveness increases and the snare classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bad_faith_determination_criteria, empirical, 'What standard governs bad-faith determination and whether it is technical or political.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the transactional reading''s core premise (unilateral withdrawal authority) logically foreclose the binding-multilateral reading''s core premise (consensus-based modification), or do they coexist as competing interpretations of the same agreement?',
    'Examine whether a single legal framework (e.g., international law precedent, UN Charter, treaty interpretation doctrine) can accommodate both premises simultaneously. If the Vienna Convention on the Law of Treaties assigns modification authority, does it require consensus or allow unilateral withdrawal?',
    'If the readings foreclose each other, one will be eliminated by case law and practice over time; if they coexist, the kernel remains contested indefinitely and different signatories will operate under different readings. This determines whether the constraint is stable or subject to terminal reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Logical relationship between transactional and binding-multilateral readings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.72) structural (agreement-dependent signatories are powerless to prevent withdrawal and face economic costs) or internalized (they accept withdrawal as legitimate because the transactional reading becomes institutionalized)?',
    'Post-withdrawal: if agreement-dependent signatories actively resist (legal action, sanctions escalation, diplomatic boycotts) or if they accept and normalize the withdrawal, the mechanism is revealed. Resistance trajectory shows structural suppression; acceptance shows internalized legitimacy.',
    'If structural, the constraint''s effective suppression exceeds the authored 0.72; if internalized, the power atom for agreement-dependent signatories shifts from constrained to mobile (they have accepted the reading and adapted). Classification may shift from snare toward tangled-rope if internalized suppression is accepted as the cost of the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of agreement-dependent signatories'' preferences is structural or internalized.').

omega_variable(
    kernel_reading_identity_within_committer_frame,
    'Is this constraint one reading of a single kernel, or does the transactional reading''s emphasis on unilateral determination make it a different constraint altogether, one that happens to have the same text as the binding-multilateral reading?',
    'Test whether changing the reading changes the referent for ε. Under the binding-multilateral reading, ε measures the agreement''s extraction from consensus-dependent signatories. Under the transactional reading, ε measures extraction from Iran and agreement-stability signatories by withdrawal states. If changing the reading changes which agent is the target, the ε referent is reading-indexed (different constraints). If ε stays the same regardless of reading, it is reading-free (one constraint, two interpretations).',
    'If reading-indexed (different constraints), the corpus should carry separate files for each reading, linked via network.affects_constraints. If reading-free, this single file captures the constraint; the readings are observer-relative positions on the same structure. The DP-001 ε-invariance principle implies reading-indexed (different readings, different ε values) is the correct decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_within_committer_frame, conceptual, 'Whether this is a reading-indexed constraint (different ε per reading) or a reading-free constraint (single ε, observer-relative interpretations).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(jcpo_tr_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(jcpo_tr_t15, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(jcpo_tr_t20, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jcpo_be_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(jcpo_be_t15, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(jcpo_be_t20, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jcpo_su_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(jcpo_su_t15, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(jcpo_su_t20, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% The JCPOA kernel admits three structurally distinct readings. The transactional_provisional_reading (this story) emphasizes unilateral withdrawal authority and bad-faith determination, yielding high extractiveness from agreement-dependent signatories and Iran. The binding_multilateral_reading emphasizes consensus-based modification and collective enforcement, yielding lower extractiveness from any single seat but higher collective commitment costs. The graduated_compliance_reading emphasizes proportional reciprocal response and compliance assessment tied to escalation level, yielding moderate extractiveness with graduated thresholds. Each reading instantiates a different constraint with different beneficiary/victim structures and different ε values. They are linked because accepting one reading constrains the others: the transactional reading influences the graduated-compliance reading by weakening institutional friction for exit; the binding-multilateral reading forecloses unilateral withdrawal authority. All three stories are required to model the kernel fully.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
