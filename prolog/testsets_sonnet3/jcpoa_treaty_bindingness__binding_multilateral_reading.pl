% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty (Consensus-Modification Reading)
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This story instantiates the binding-multilateral reading of the JCPOA
 *   kernel: the treaty is a legally binding multilateral instrument, embedded
 *   in UNSC Resolution 2231, that can only be lawfully modified or dissolved
 *   through consensus process among the P5+1 and Iran, with disputes routed
 *   through the Joint Commission before any snapback of sanctions. Under this
 *   reading, the 2018 US withdrawal was a violation of binding obligations,
 *   not a lawful unilateral exit, and unilateral secondary-sanctions
 *   reimposition thereafter compounds that violation rather than exercising a
 *   reserved right. The metrics rise sharply around 2018-2019 because the
 *   binding framework's central claim — that consensus process constrains
 *   exit — was tested and, empirically, did not prevent unilateral action;
 *   the reading survives as a legal doctrine even as its practical
 *   suppressive force weakened. This is one of three linked constraints on
 *   the same kernel: the transactional-provisional reading treats US
 *   withdrawal as lawful exercise of a reserved right, and the
 *   graduated-compliance reading treats enforcement as scaled to proportional
 *   compliance rather than binary breach. Each reading carries its own
 *   epsilon and stakeholder structure; they are not merged here.
 *
 * KEY AGENTS:
 *   - unsc_permanent_members: agenda_setter/beneficiary (institutional/arbitrage) — administer Resolution 2231's binding framework
 *   - eu_coordinating_parties: agenda_setter/beneficiary (institutional/constrained) — run Joint Commission dispute process
 *   - iaea_verification_apparatus: beneficiary/agenda_setter (institutional/analytical) — technical predicate for the consensus process
 *   - united_states_unilateral_policy_capacity: payer (institutional/constrained) — treated as in breach for 2018 withdrawal
 *   - iranian_civilian_economy: payer (powerless/trapped) — bears sanctions costs when the consensus process is bypassed
 *   - regional_states_excluded_from_negotiation: excluded (moderate/constrained) — no seat in the dispute-resolution channel
 *   - international_law_scholars: observer (analytical/analytical) — assess the binding-vs-political-commitment question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.38).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty (Consensus-Modification Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, 'a0834f22-6918-44fe-bf66-3c54767807ae').
narrative_ontology:cs_kernel_codification('a0834f22-6918-44fe-bf66-3c54767807ae', fixed_text).
narrative_ontology:cs_authority_grounding('a0834f22-6918-44fe-bf66-3c54767807ae', lineage).
narrative_ontology:cs_interpretation_layer_present('a0834f22-6918-44fe-bf66-3c54767807ae').
narrative_ontology:cs_reading_relation('a0834f22-6918-44fe-bf66-3c54767807ae', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('a0834f22-6918-44fe-bf66-3c54767807ae', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('a0834f22-6918-44fe-bf66-3c54767807ae', foundational, consensus_process_is_binding_precondition).
narrative_ontology:cs_axiom_status(consensus_process_is_binding_precondition, holdable).
narrative_ontology:cs_axiom_grounding('a0834f22-6918-44fe-bf66-3c54767807ae', consensus_process_is_binding_precondition, conventional).
narrative_ontology:cs_axiom('a0834f22-6918-44fe-bf66-3c54767807ae', secondary, unilateral_withdrawal_constitutes_breach).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_constitutes_breach, holdable).
narrative_ontology:cs_axiom_grounding('a0834f22-6918-44fe-bf66-3c54767807ae', unilateral_withdrawal_constitutes_breach, conventional).
narrative_ontology:cs_reference_frame('a0834f22-6918-44fe-bf66-3c54767807ae', resolution_2231_binding_incorporation).
narrative_ontology:cs_drift_state('a0834f22-6918-44fe-bf66-3c54767807ae', post_2018_withdrawal_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a0834f22-6918-44fe-bf66-3c54767807ae', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_coordinating_parties).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime_architecture).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_apparatus).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_unilateral_policy_capacity).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_economy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_states_excluded_from_negotiation).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_consensus_supremacy_doctrine).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_resolution_2231_binding_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Co-drafted UNSC Resolution 2231, which endorsed the JCPOA and embedded a dispute-resolution/snapback mechanism requiring the Joint Commission process to run before sanctions can be reimposed. They administer the consensus machinery and can shape its application through Security Council procedure, giving them structural control over whether the treaty holds or breaks.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members, beneficiary).

% Serve as coordinator of the Joint Commission and administer the dispute resolution steps. Benefit from a stable multilateral framework that preserves their diplomatic centrality and commercial access to Iran, but cannot unilaterally reimpose or lift sanctions without running the consensus process — their leverage depends on the treaty's bindingness being real.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_coordinating_parties, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_coordinating_parties, beneficiary).

% Conducts inspections and certifies compliance, which is the technical predicate the consensus-modification process depends on. Its institutional relevance and budget are sustained by the treaty's continued operation as a verifiable, binding instrument rather than a discretionary arrangement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_apparatus, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_apparatus, agenda_setter).

% The broader NPT-centered nonproliferation order is not itself an actor but is what the binding-treaty reading protects: a precedent that negotiated nuclear constraints survive changes of government and cannot be dissolved by one party's unilateral judgment. Its stability is cited as the reason multilateral consensus process is essential.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime_architecture, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime_architecture).

% Under this reading, US withdrawal in 2018 was a treaty violation rather than a lawful exit, because Resolution 2231 embeds the deal in binding multilateral process. The US bears the cost of having its unilateral executive action treated as internationally unlawful, and re-entry requires negotiating readmission through the same consensus machinery rather than simply re-signing. Domestically, this constrains future administrations' ability to exit by executive fiat alone.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_unilateral_policy_capacity, payer,
    institutional, biographical, constrained, global).

% Ordinary Iranian businesses, banks, and households bear the sanctions consequences whenever the multilateral process stalls or one party defects outside the agreed procedure. Under the binding-multilateral reading, snapback should require the Joint Commission process to run first, but in practice unilateral secondary sanctions were reimposed anyway, leaving this population absorbing costs the treaty's own bindingness was supposed to prevent.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_economy, payer,
    powerless, biographical, trapped, national).

% Israel, Saudi Arabia, and other regional states were not parties to the JCPOA and had no seat in the P5+1 negotiation or the Joint Commission dispute process, yet live with the security consequences of Iran's enrichment trajectory. The binding-multilateral reading treats their objections as outside the treaty's cognizable dispute-resolution channel.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_states_excluded_from_negotiation, excluded,
    moderate, generational, constrained, regional).

% Assess whether Resolution 2231's incorporation of the JCPOA created binding obligations under international law independent of any single signatory's domestic politics, and whether the 2018 US withdrawal and subsequent 2019-2025 Iranian enrichment escalations constitute breaches of a binding instrument or exits from a non-binding political commitment.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves the multilateral verification problem: without a binding consensus-based structure, any single party's change of government or policy preference could unilaterally dissolve years of negotiated nuclear constraints and inspection access, making long-term nonproliferation commitments impossible to sustain across electoral cycles.
% TRANSFER_FUNCTION: Moves sanctions-relief predictability and diplomatic centrality toward the P5+1/EU coordinating parties and the verification apparatus, and moves the cost of process delay and unilateral defection onto the Iranian civilian economy and onto whichever government (US in 2018) is treated as having broken a binding instrument rather than exited a discretionary one.
% ABSENT_VOICES: Regional states with direct security stakes (Israel, Gulf states) were never seated in the P5+1 negotiation or the Joint Commission; they would object that a consensus process controlled entirely by the original signatories cannot adequately weigh their exposure to Iranian enrichment decisions made without their input.
% DISAPPEARANCE_RATIONALE: If the binding-multilateral framing were abandoned entirely, snapback and re-entry would revert to raw bargaining power among individual states, UNSC Resolution 2231 would lose its distinguishing legal force, the Joint Commission's dispute-resolution steps would become optional courtesy rather than procedural predicate, and future nuclear negotiations would have to be renegotiated from scratch rather than treated as durable multilateral law.
% FOUNDING_PROBLEM: In 2015, negotiators sought a mechanism that would survive changes in domestic political leadership on both the US and Iranian sides — a purely bilateral or executive-agreement structure was judged too fragile against the demonstrated pattern of new administrations repudiating predecessors' nuclear diplomacy.
% FOUNDING_PROBLEM_CORROBORATION: UN Secretariat legal officers and European Joint Commission representatives attest the binding character survived the 2018 US withdrawal as a matter of Resolution 2231's continued force; independent international law scholars outside any P5+1 government corroborate that the resolution created obligations distinct from the political commitment, while noting the 2018-2025 record shows the binding framework did not in practice prevent unilateral sanctions reimposition or Iranian enrichment escalation, undercutting the founding problem's claimed solution in operation even where the legal doctrine persists on paper.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the binding-multilateral reading genuinely constrains arbitrary unilateral action for the institutional parties who administer it, while imposing real costs on the Iranian civilian population and on future US administrations bound by a legal characterization they did not choose. Suppression sits mid-range (0.38) — the consensus requirement is a real procedural constraint, not merely rhetorical, but it proved insufficiently coercive to prevent the 2018 breach, which is exactly the empirical test this reading's bindingness claim failed. Theater ratio rose sharply after 2018 (0.35-0.40) as the Joint Commission process continued to be invoked procedurally by EU parties even after its practical capacity to prevent snapback had been demonstrated hollow — a growing gap between the formal dispute-resolution machinery and its actual constraining force.
 *
 * PERSPECTIVAL GAP:
 *   From the EU/UNSC agenda-setter seat, the binding-multilateral reading is straightforward treaty law: Resolution 2231 created obligations, and 2018 was a breach. From the US institutional seat, the same structure appears as an attempt to bind a sovereign's foreign-policy discretion through international legal characterization it never fully accepted as constraining. From the Iranian civilian seat, the abstract legal debate over bindingness is irrelevant to the material fact that sanctions costs landed regardless of which reading a given actor endorsed.
 *
 * DIRECTIONALITY LOGIC:
 *   UNSC permanent members, EU coordinating parties, and the IAEA sit near the beneficiary end: they administer the binding framework and their institutional standing depends on treaties surviving domestic political turnover. The US unilateral policy capacity and Iranian civilian economy sit near the target end: the US bears the cost of being characterized as a treaty-breaker rather than a lawful exiter, and Iranian civilians bear sanctions costs regardless of whether the consensus process was actually followed. Regional excluded states are not directly extracted from financially but are structurally locked out of the dispute channel that would let them contest enrichment decisions affecting their security.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — durable nuclear constraints surviving political turnover — remains partially live (multilateral nonproliferation architecture still values durability) but the specific mechanism (Resolution 2231's snapback-blocking consensus process) has been shown not to function as designed once a permanent member chose to defect. This is not full mandatrophy (the coordination function is not dead) but it is a case where continued invocation of 'binding' status outpaced the mechanism's demonstrated capacity to bind — exactly the kind of claim/metric divergence this reading's rising theater_ratio is meant to surface rather than obscure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resolution_2231_binding_status_ambiguity,
    'Did UNSC Resolution 2231 create genuinely binding international legal obligations independent of the JCPOA''s status as a political commitment, or did it merely endorse a political arrangement that remained terminable at each party''s discretion?',
    'Authoritative international judicial or arbitral determination (e.g., an ICJ advisory opinion or contentious case) on whether Resolution 2231''s operative paragraphs created binding obligations distinct from the JCPOA text itself, and whether the 2018 US withdrawal constituted an internationally wrongful act.',
    'If genuinely binding, this reading''s classification as tangled_rope (real coordination function plus asymmetric cost imposition on the US and Iranian civilians) holds. If merely endorsement of a discretionary arrangement, the constraint collapses toward the transactional_provisional_reading and the coordination claim weakens substantially, pushing toward snare from the US perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resolution_2231_binding_status_ambiguity, conceptual, 'Whether Resolution 2231 created binding law or merely endorsed a discretionary political commitment — the central premise this reading depends on.').

omega_variable(
    consensus_process_actual_constraining_force,
    'Given that the Joint Commission dispute-resolution process did not in practice prevent unilateral US withdrawal (2018) or unilateral secondary sanctions reimposition, does the binding-multilateral reading describe a real structural constraint or a formally-invoked but practically hollow procedure?',
    'Track whether any subsequent dispute (post-2019 Iranian enrichment escalations, European E3 dispute mechanism triggering in 2020) actually altered the substantive outcome versus being invoked and then bypassed by unilateral action from any party.',
    'If the process was consistently bypassed without consequence, the theater_ratio trajectory understates the degree to which this reading''s bindingness claim is aspirational rather than operative, which would push the classification toward piton (a coordination form maintained ceremonially after its constraining function atrophied) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_process_actual_constraining_force, empirical, 'Whether the consensus-modification process has retained real constraining force after repeated unilateral bypasses.').

omega_variable(
    excluded_regional_states_standing,
    'Should regional states with direct security exposure to Iranian enrichment (Israel, Gulf states) have had cognizable standing within the binding-multilateral framework''s dispute-resolution process, and does their exclusion undermine the legitimacy claim of consensus-based bindingness?',
    'Comparative analysis of other multilateral security treaties'' provision (or non-provision) for third-party security-affected states'' standing in dispute mechanisms.',
    'If exclusion is a structural defect rather than a reasonable limitation of party-based treaty law, the beneficiary set (multilateral institutions, nonproliferation architecture) should be read as partly self-serving in defining the boundaries of whose consensus counts — reinforcing the tangled_rope reading''s victim/beneficiary asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_regional_states_standing, preference, 'Whether third-party regional states'' exclusion from the consensus process is a legitimate limitation or a structural extraction from their security interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2023, 0.32).
narrative_ontology:measurement(jcpo_tr_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2017, 0.28).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2023, 0.5).
narrative_ontology:measurement(jcpo_be_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2017, 0.24).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2021, 0.38).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2023, 0.36).
narrative_ontology:measurement(jcpo_su_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_safeguards_verification_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_resolution_2231_snapback_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the jcpoa_treaty_bindingness kernel. binding_multilateral_reading claims moderate tangled_rope extraction with UNSC/EU/IAEA as beneficiaries and the US policy capacity plus Iranian civilians as payers. transactional_provisional_reading (sibling, separate file) claims the withdrawal was a lawful exercise of reserved unilateral judgment and would author a substantially different beneficiary/victim structure and likely lower epsilon from the US perspective. graduated_compliance_reading (sibling, separate file) treats enforcement intensity as scaled to compliance proportionality rather than binary breach, producing a different suppression profile. All three share the same underlying treaty text but instantiate structurally distinct constraints per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
