% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework (Transactional Provisional Reading)
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story captures the transactional provisional reading of
 *   the JCPOA: the agreement is a conditional, time-limited exchange of
 *   sanctions relief for nuclear constraints, voidable at will by any party
 *   that unilaterally determines the other has acted in bad faith. Under this
 *   reading, the JCPOA imposes minimal constraint on sovereign withdrawal —
 *   the US 2018 withdrawal and snapback of sanctions is the paradigmatic
 *   operation of this reading. The beneficiaries are domestic political
 *   coalitions in the US and Israel that opposed the deal, and Iranian
 *   hardliners who gain from the deal's collapse. The victims are the Iranian
 *   civilian economy (sanctions reimposition), European businesses that
 *   invested in reliance on the deal, and the multilateral arms control
 *   architecture (eroded credibility of negotiated non-proliferation
 *   agreements). The scaffold classification reflects the provisional,
 *   transitional intent: the JCPOA was explicitly a 10-15 year
 *   confidence-building arrangement meant to transition to a normal NPT
 *   relationship. The sunset clauses (provisions expiring at years 10, 13,
 *   15) are structural, not rhetorical.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.22).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.35).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, scaffold).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework (Transactional Provisional Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:has_sunset_clause(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '310604fe-c154-4deb-a5ef-fcbfcb0ddc7f').
narrative_ontology:cs_kernel_codification('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', formalized).
narrative_ontology:cs_authority_grounding('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', practice).
narrative_ontology:cs_interpretation_layer_present('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f').
narrative_ontology:cs_reading_relation('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', foundational, sovereign_right_to_judge_compliance_unilaterally).
narrative_ontology:cs_axiom_status(sovereign_right_to_judge_compliance_unilaterally, holdable).
narrative_ontology:cs_axiom_grounding('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', sovereign_right_to_judge_compliance_unilaterally, conventional).
narrative_ontology:cs_axiom('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', foundational, treaties_as_conditional_exchanges_voidable_on_bad_faith).
narrative_ontology:cs_axiom_status(treaties_as_conditional_exchanges_voidable_on_bad_faith, holdable).
narrative_ontology:cs_axiom_grounding('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', treaties_as_conditional_exchanges_voidable_on_bad_faith, instrumental).
narrative_ontology:cs_reference_frame('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', provisional_reciprocal_exchange_2015_2025).
narrative_ontology:cs_drift_state('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', post_us_withdrawal_2018_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('310604fe-c154-4deb-a5ef-fcbfcb0ddc7f', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, us_domestic_skeptics_coalition).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_hardline_faction).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civilian_economy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, european_business_interests).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_arms_control_architecture).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, eu_e3_governments).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_government).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, eu_e3_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the JCPOA, implemented sanctions relief, and unilaterally withdrew in 2018 citing Iranian bad faith (ballistic missiles, regional proxies, sunset provisions). Reimposed comprehensive sanctions including secondary sanctions on third parties. Holds the sovereign right to determine compliance and exit. Bears reputational cost with allies but no material exit barrier.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive, agenda_setter,
    institutional, biographical, arbitrage, global).

% Accepted nuclear constraints (centrifuge limits, enrichment caps, IAEA monitoring) in exchange for sanctions relief. Remained in compliance for one year after US withdrawal (verified by IAEA), then began graduated non-compliance. Cannot exit the NPT without triggering Security Council referral and potential military action. Bears sanctions extraction whether compliant or not.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_government, payer,
    powerful, biographical, constrained, global).

% Strong supporters of the JCPOA as non-proliferation achievement. Invested diplomatic capital and created INSTEX (special purpose vehicle) to maintain trade with Iran. Blocked US secondary sanctions via EU blocking statute but could not compel European firms to use INSTEX. Gains non-proliferation benefit; loses commercial credibility and investment when US snapback destroys the economic exchange.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, eu_e3_governments, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, eu_e3_governments, payer).

% Bears the full force of reimposed sanctions: currency collapse, inflation, medicine shortages, unemployment. No political voice in compliance determinations. No exit — cannot leave Iran, cannot access global financial system. The constraint's extraction lands here regardless of Iranian government compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civilian_economy, payer,
    powerless, immediate, trapped, national).

% Signed contracts and invested in Iran post-JCPOA (Total, Airbus, Siemens, Peugeot, etc.). Forced to exit by US secondary sanctions threat (loss of US market access). INSTEX was too limited (humanitarian only, small volume) to sustain commercial operations. Gains nothing from the constraint; loses sunk investment and market access.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, european_business_interests, payer,
    moderate, biographical, constrained, regional).

% The JCPOA was a landmark of negotiated non-proliferation. Its unilateral voiding by a permanent Security Council member erodes the credibility of all future arms control agreements — why negotiate if compliance doesn't prevent sanctions? The constraint extracts credibility from the architecture to pay for sovereign withdrawal freedom.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_arms_control_architecture, payer,
    institutional, generational, analytical, universal).

% Opposed the JCPOA from negotiation through implementation. Lobbied US for withdrawal. Gains strategic freedom from constraint on military options against Iranian nuclear program. No cost from the constraint — does not participate in the exchange, only benefits from its collapse.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, israeli_security_establishment, beneficiary,
    powerful, biographical, arbitrage, national).

% Opposed the JCPOA domestically (insufficient sovereignty, excessive inspections). Gains political capital from US withdrawal validating their critique. Uses deal collapse to argue for nuclear breakout and resistance economy. No material cost from constraint — only political benefit from its failure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_hardline_faction, beneficiary,
    organized, biographical, mobile, national).

% Verifies Iranian compliance continuously (quarterly reports). Provides the factual baseline that all readings reference but interpret differently. No stake in the exchange; bears no extraction. The constraint's theater_ratio reflects the gap between IAEA-verified compliance and political determinations of bad faith.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_inspectorate, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates reciprocal sanctions relief for verified nuclear constraints, creating a confidence-building period (10-15 years) intended to transition Iran to normal NPT status without breakout capacity.
% TRANSFER_FUNCTION: Moves sanctions relief (oil revenue, financial access, commercial contracts) from P5+1 to Iran, in exchange for nuclear constraints (enrichment limits, centrifuge caps, monitoring) from Iran to P5+1. The transactional reading treats this as a conditional exchange voidable on bad faith determination.
% ABSENT_VOICES: Iranian civil society (reformists, women's movement, labor) who supported the deal for economic opening but had no seat at the table. Global South states who rely on multilateral non-proliferation norms but were not parties. Future generations who inherit a weakened arms control architecture.
% DISAPPEARANCE_RATIONALE: If the transactional provisional framework vanished overnight, Iran would likely accelerate nuclear program (breakout), US/Israel would face military decision, EU would lose last diplomatic channel, NPT review cycles would fracture. The world rearranges because the constraint currently structures the entire Iran nuclear crisis management.
% FOUNDING_PROBLEM: The 2002-2013 Iran nuclear crisis: Iran's undisclosed enrichment program (Natanz, Fordow) created breakout risk; UNSC sanctions failed to stop advancement; military strike was contemplated. JCPOA built to trade time and verification for sanctions relief — a provisional bridge to normal NPT status.
% FOUNDING_PROBLEM_CORROBORATION: IAEA Director General reports (2016-2025) corroborate Iran had no breakout during JCPOA compliance period. US intelligence community (2023) assesses Iran not currently pursuing weaponization. European governments corroborate the non-proliferation benefit was real. US/Israel political leadership asserts the problem persists (sunsets, missiles, proxies). No consensus outside benefiting parties.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.22 at interval end) because the transactional reading extracts little from compliant parties — the exchange is voluntary and reciprocal. However, extractiveness spiked to 0.48 at T=12 (US withdrawal/snapback) when sanctions reimposed extracted heavily from Iranian civilians and European firms. Theater ratio is high (0.45) because the provisional framework's compliance theater (IAEA monitoring, JCPOA Joint Commission meetings) increasingly performed the deal's vitality after US withdrawal while the substantive exchange had collapsed. Suppression is moderate (0.35) — the constraint does not coerce participation (Iran remained in the deal for a year post-US withdrawal) but the snapback mechanism suppresses exit alternatives for third parties. Accessibility collapse (0.4) reflects that alternatives (renegotiation, snapback, military action) remain conceptually available but politically costly. Resistance (0.55) is significant: Iran resisted snapback via graduated non-compliance; EU resisted via INSTEX and blocking statute.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (US under this reading) experiences the constraint as a rope — a voluntary coordination mechanism it can exit at will. The payer (Iran) experiences it as a snare — extraction (sanctions) imposed despite compliance, with exit blocked by NPT consequences. The EU experiences it as a tangled rope — genuine coordination (non-proliferation) coupled with asymmetric extraction (secondary sanctions). The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The US (agenda_setter) sits at d≈0.15 — beneficiary of sovereign withdrawal right, but bears reputational cost. Iran (payer) sits at d≈0.85 — bears sanctions extraction, constrained exit (NPT withdrawal would escalate). EU (beneficiary/payer dual) sits near d≈0.5 — gains non-proliferation benefit, loses commercial investment. Israeli security establishment (beneficiary) at d≈0.1 — gains strategic freedom from constraint. Iranian hardliners (beneficiary) at d≈0.2 — gain domestic political capital from deal collapse. Iranian civilians (victim) at d≈0.9 — bear sanctions with no exit. European businesses (victim) at d≈0.7 — trapped by secondary sanctions. Multilateral architecture (victim) at d≈0.6 — eroded but not destroyed.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's founding problem (confidence-building toward normal NPT status) is contested — the US reading says the problem persists (Iran's regional behavior, sunset provisions); Iran says the problem was solved until US withdrawal. The transition target (normal NPT relationship) was never reached. The constraint persists in degraded form (theater_ratio 0.45) — the JCPOA Joint Commission still meets, IAEA monitoring continues, but the core exchange is suspended. This is mandatrophy: the provisional framework outlived its transition function but was not formally dissolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the transactional provisional reading a structurally distinct constraint from the binding multilateral and graduated compliance readings, or a rhetorical position on the same constraint?',
    'Compare ε values and stakeholder structures across the three readings. If ε differs significantly (>0.15) or beneficiary/victim sets are disjoint, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own constraint story with own classification. If same constraint, the three readings are observer positions on one constraint with one ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether kernel readings instantiate separate constraints or observer perspectives').

omega_variable(
    bad_faith_determination_mechanism,
    'What constitutes the unilateral determination of bad faith that voids the framework — is there a procedural standard, or is it purely political?',
    'Analyze state practice: US 2018 withdrawal used IAEA-verified compliance as bad faith; compare with other treaty withdrawals citing material breach.',
    'If purely political, the constraint is a snare (extraction cover). If procedural, it retains scaffold character with genuine sunset logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bad_faith_determination_mechanism, empirical, 'Nature of the bad faith trigger — procedural vs. political').

omega_variable(
    scaffold_transition_function,
    'What is the provisional framework transitioning TO — a permanent treaty, a new deal, or managed conflict?',
    'Track post-JCPOA diplomatic proposals and Iranian nuclear trajectory. The transition target defines whether the scaffold''s sunset clause is genuine or theatrical.',
    'If no credible transition target exists, the scaffold classification collapses to piton (inertial maintenance) or snare (extraction cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_transition_function, conceptual, 'Whether the provisional framework has a genuine transition target').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jcpo_tr_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(jcpo_tr_t9, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 9, 0.55).
narrative_ontology:measurement(jcpo_tr_t12, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 12, 0.7).
narrative_ontology:measurement(jcpo_tr_t15, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(jcpo_be_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 3, 0.15).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement(jcpo_be_t9, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 9, 0.35).
narrative_ontology:measurement(jcpo_be_t12, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(jcpo_be_t15, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 15, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(jcpo_su_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 3, 0.2).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 6, 0.25).
narrative_ontology:measurement(jcpo_su_t9, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 9, 0.6).
narrative_ontology:measurement(jcpo_su_t12, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(jcpo_su_t15, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 15, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, npt_article_vi_compliance).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, unsc_resolution_2231).

% DUAL FORMULATION NOTE:
% JCPOA treaty bindingness kernel decomposes into three constraint stories with distinct ε values and stakeholder structures. This reading (transactional_provisional) has ε=0.22 (low extractiveness when operating as intended, spiking at withdrawal). Binding multilateral reading would have lower ε (treaty as law). Graduated compliance reading would have moderate ε with reciprocal scaling. All three share the same referent (the JCPOA text and practice) but instantiate different constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, institutional, 0.15).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, powerful, 0.85).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, organized, 0.5).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, moderate, 0.7).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
