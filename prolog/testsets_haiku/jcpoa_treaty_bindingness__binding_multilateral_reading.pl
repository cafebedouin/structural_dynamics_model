% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA Binding Multilateral Treaty Constraint (Binding Reading)
 *   domain: international_law/nuclear_non_proliferation
 *
 * SUMMARY:
 *   The JCPOA constraint exists in the contested space between two radically
 *   different treaty readings: as a BINDING multilateral commitment that
 *   cannot be unilaterally withdrawn or materially modified, or as a
 *   TRANSACTIONAL framework voidable upon unilateral determination of bad
 *   faith. This story instantiates the binding reading—the view that signed
 *   commitments create obligations on all parties irreversible without
 *   consensus, that Iranian enrichment violations trigger multilateral
 *   dispute resolution before unilateral snapback, and that the UNSC veto
 *   power is the mechanism that enforces bindingness. The competing reading
 *   treats the JCPOA as provisional, subject to unilateral exit, and
 *   triggered by perceived Iranian deception. The U.S. 2018 withdrawal is the
 *   literal flashpoint: under the binding reading, a violation; under the
 *   transactional reading, a legitimate remedial exit. The authored metrics
 *   describe high extraction (0.68) and suppression (0.71) because the
 *   binding reading severely constrains unilateral actor choice—no single
 *   party can exit, no single party can trigger snapback without UNSC
 *   consensus. The claim/metric independence rule applies: this reading
 *   claims the constraint is tangled_rope (real coordination benefit plus
 *   asymmetric binding on unilateral actors), while the metrics show
 *   extractive operation; the engine measures that gap.
 *
 * KEY AGENTS:
 *   - UN Security Council P5+1 (Russia, China, U.S., France, U.K., Germany): institutional agenda-setters with power to interpret and enforce binding status; Russia and China hold veto power that benefits them under this reading.
 *   - Iran: powerful nation-state payer constrained by binding enrichment limits and identity-locked to nuclear sovereignty; exit requires renegotiation or consensus dissolution.
 *   - United States: institutional signatory torn between beneficiary role (non-proliferation assurance) and payer role (inability to unilaterally reimpose sanctions or withdraw).
 *   - European signatories: institutional beneficiaries of non-proliferation assurance; constrained from preferred unilateral responses when U.S. withdraws.
 *   - IAEA: organized beneficiary whose verification authority derives from binding treaty status.
 *   - Unilateral-withdrawal advocates: excluded actors whose core claim—that the treaty is provisional—defines the competing reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.71).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA Binding Multilateral Treaty Constraint (Binding Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, 'f5626eb6-53b0-4758-8dd3-467d08e332f0').
narrative_ontology:cs_kernel_codification('f5626eb6-53b0-4758-8dd3-467d08e332f0', fixed_text).
narrative_ontology:cs_authority_grounding('f5626eb6-53b0-4758-8dd3-467d08e332f0', lineage).
narrative_ontology:cs_interpretation_layer_present('f5626eb6-53b0-4758-8dd3-467d08e332f0').
narrative_ontology:cs_reading_relation('f5626eb6-53b0-4758-8dd3-467d08e332f0', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('f5626eb6-53b0-4758-8dd3-467d08e332f0', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('f5626eb6-53b0-4758-8dd3-467d08e332f0', foundational, treaty_text_binding_all_parties_irreversibly).
narrative_ontology:cs_axiom_status(treaty_text_binding_all_parties_irreversibly, holdable).
narrative_ontology:cs_axiom_grounding('f5626eb6-53b0-4758-8dd3-467d08e332f0', treaty_text_binding_all_parties_irreversibly, conventional).
narrative_ontology:cs_axiom('f5626eb6-53b0-4758-8dd3-467d08e332f0', foundational, unilateral_withdrawal_violates_pacta_sunt_servanda).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_violates_pacta_sunt_servanda, holdable).
narrative_ontology:cs_axiom_grounding('f5626eb6-53b0-4758-8dd3-467d08e332f0', unilateral_withdrawal_violates_pacta_sunt_servanda, deontological).
narrative_ontology:cs_reference_frame('f5626eb6-53b0-4758-8dd3-467d08e332f0', multilateral_consensus_irreversibility).
narrative_ontology:cs_drift_state('f5626eb6-53b0-4758-8dd3-467d08e332f0', post_us_withdrawal_2018, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('f5626eb6-53b0-4758-8dd3-467d08e332f0', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime_stability).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_consensus_institutions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateral_state_actors).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_nuclear_program_constraints).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_signatory).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatories_eu).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, russian_federation).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, chinese_prc).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_international_atomic_energy_agency).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_aligned_movement_regional_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_islamic_republic).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_signatory).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, treaty_pacta_sunt_servanda_doctrine).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, collective_security_supremacy).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_binding_commitment_irreversibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and formally adopted the JCPOA framework; holds authority to interpret its terms, authorize dispute resolution, and enforce snapback sanctions. Acts through consensus on major modifications; dissent from any permanent member blocks unilateral withdrawal remedies. The binding reading ascribes to this body the authority to adjudicate whether Iranian breaches are material enough to trigger snapback.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council_p5_plus_one, agenda_setter,
    institutional, generational, analytical, global).

% Bound to the JCPOA's uranium enrichment caps, transparency inspections, and nuclear program constraints. Under this reading, unilateral withdrawal is not available; exit requires either multilateral consensus to dissolve the treaty or material breach by other parties triggering a dispute-resolution process. National sovereignty and nuclear program autonomy are constrained by binding interpretation of the written text.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_islamic_republic, payer,
    powerful, generational, identity_locked, global).

% Gains assurance from multilateral verification and binding Iranian compliance; loses unilateral escalation authority to reimpose sanctions without UNSC consensus under this reading. Internal U.S. debate pivots on whether the binding constraint on American action (inability to unilaterally snapback) is justified by the binding constraint on Iran (inability to unilaterally withdraw). Withdrew unilaterally in 2018, which this reading interprets as violation of binding commitment.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_signatory, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_signatory, payer).

% Benefits from non-proliferation assurance and nuclear risk reduction in the region; constrained by the binding interpretation which prevents their preferred unilateral response (sanctions adjustment without UNSC process) when U.S. withdrawal occurs. Advocates strongly for this reading to preserve the framework; has structured domestic policy around JCPOA permanence.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatories_eu, beneficiary,
    institutional, generational, constrained, global).

% Benefits from the binding reading because it grants Russia veto power over snapback sanctions and unilateral withdrawal remedies through the UNSC consensus requirement. Gains leverage over other signatories who depend on UNSC consensus. Constrained by same binding commitment if Iran breaches and Russia must choose between blocking snapback or accepting Iranian non-compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, russian_federation, beneficiary,
    institutional, generational, constrained, global).

% Holds veto power over snapback; benefits from the binding reading that makes unilateral action impossible and all remedies require consensus. Strategic partner relationship with Iran aligns interests against unilateral snapback; the binding reading protects that alignment.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, chinese_prc, beneficiary,
    institutional, generational, constrained, global).

% Gains verification mandate and access to Iranian nuclear sites; operates as the technical arbiter of compliance under this reading. Authority derives from the JCPOA's binding legal status; if the reading is transactional or provisional instead, IAEA's verification role becomes advisory rather than authoritative.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_international_atomic_energy_agency, beneficiary,
    organized, generational, constrained, global).

% Benefit from regional non-proliferation assurance and predictable Iranian compliance; constrained from pursuing independent nuclear programs if the binding reading holds (signals that nonproliferation regime is binding on all). Their exit would require renegotiating the entire multilateral framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, non_aligned_movement_regional_states, beneficiary,
    moderate, biographical, constrained, regional).

% U.S. administrations and domestic actors advocating unilateral withdrawal without UNSC process are structurally excluded from the decision-making authority under this reading. Their argument—that JCPOA is provisional, defective, subject to unilateral exit—is the core of the competing reading. They are excluded by the binding reading's axioms, not merely outvoted.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateral_withdrawal_advocates, excluded,
    powerful, biographical, constrained, national).

% International legal scholars, UN bodies, and treaty-regime monitoring entities assess whether the binding reading holds and what precedent it sets for treaty bindingness generally. Their analysis does not change the constraint's operation but informs its persistence and future similar treaties.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_treaty_regime_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council_p5_plus_one).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates multilateral verification of Iranian nuclear non-proliferation, creates binding constraints on enrichment and plutonium production, establishes unified snapback-sanctions authority through the UNSC, and substitutes multilateral consensus for unilateral escalation or withdrawal—solving the collective-action problem of monitoring hostile nuclear development.
% TRANSFER_FUNCTION: Transfers Iranian nuclear program autonomy to multilateral verification and constraint; transfers unilateral U.S. (and other signatories') authority to reimpose sanctions to a UNSC consensus requirement; creates binding obligations on all parties that cannot be revoked unilaterally.
% ABSENT_VOICES: Domestic U.S. political actors who view the JCPOA as fundamentally defective (receiving inadequate inspections access, insufficient duration, deficient consequences for minor violations) are formally outside the renegotiation process under this reading. Iranian hardliners who reject any binding constraint on the nuclear program are also excluded—they cannot renegotiate within the binding framework. These excluded voices constitute the core of the competing provisional reading.
% DISAPPEARANCE_RATIONALE: If the JCPOA's binding multilateral status vanished overnight—i.e., if all parties suddenly acted under the transactional reading instead—sanctions regimes would diverge, Iranian enrichment would accelerate toward weapons grade without unified multilateral response, and the non-proliferation regime's deterrent force would fragment across national preferences. Regional nuclear competition would intensify; confidence in multilateral binding commitment would collapse for all future treaties.
% FOUNDING_PROBLEM: Iran's accelerating nuclear weapons development in the 2000s–2010s created existential security risk for the region and global non-proliferation treaty system; unilateral military intervention threatened wider regional conflict; multilateral economic sanctions had reached the limit of coercive leverage without negotiated off-ramp. The JCPOA was built as a binding multilateral commitment to resolve this through verified constraint instead of military confrontation.
% FOUNDING_PROBLEM_CORROBORATION: The P5+1 negotiators and the IAEA attest the founding problem is solved: Iranian enrichment is constrained, inspections verify compliance, and military option is deferred. U.S. withdrawal advocates attest the founding problem persists unresolved: Iran's missile program, regional proxy activity, and post-2015 breakout timeline mean the agreement does not address the core threat. Independent security analysis from SIPRI, CSIS, and non-proliferation scholars is split: roughly half support the binding reading's claim that verified constraint solves the founding problem; roughly half support the competing reading that the problem is incompletely addressed and the binding commitment is therefore unjustified.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) because the binding reading concentrates constraint-modification authority in the multilateral consensus machinery (the UNSC and P5+1), removing it from any single nation. The measurement series shows rise from 0.52 at interval start to 0.68 at end, tracking the accumulation of enforcement mechanisms post-2015 ratification: inspections deepen, dispute-resolution procedures activate, and the cost of unilateral exit becomes manifest (exemplified by the 2018 U.S. withdrawal's international isolation cost). Suppression is high (0.71) because the binding reading requires active exclusion of unilateral remedies: the constraint's persistence depends on maintaining that snapback requires UNSC consensus, that withdrawal requires multilateral agreement, that disputes flow through the Joint Commission before escalation. Theater rises from 0.28 to 0.42, reflecting the growing share of diplomatic activity devoted to performing compliance (declarations of faithfulness to binding status) versus actual verification work. Accessibility collapse is high (0.78) because the binding reading leaves alternatives to multilateral process almost entirely unavailable—unilateral actors are structurally foreclosed; the only exit is renegotiation with all other parties. Resistance is moderate (0.59) because the U.S. withdrawal and persistent Iranian hardliner opposition show real resistance to the binding reading, even though the P5+1 consensus held through the interval.
 *
 * PERSPECTIVAL GAP:
 *   The greatest divergence between seats occurs between the permanent UNSC members (beneficiaries of veto power; the binding reading maximizes their leverage) and Iran/unilateral-exit advocates (victims of constraint irreversibility). From the P5+1 consensus perspective, the constraint is genuine multilateral coordination—binding all parties equally to verified non-proliferation, which is the coordination function. From Iran's perspective, the constraint is asymmetric extraction: Iran's nuclear autonomy is surrendered while the U.S. retains the option of withdrawal (which occurred in 2018), which this reading classifies as violation of bindingness but which the transactional reading treats as legitimate exit. The engine should compute Iran's directionality as near full target (d ≈ 0.85–0.95) because Iran is identity-locked to the nuclear program, has no arbitrage exit, and bears the enrichment constraints; the U.S. and Russia should compute lower (beneficiary-adjacent for Russia due to veto; moderate for the U.S. which faces mixed incentives).
 *
 * DIRECTIONALITY LOGIC:
 *   The binding reading creates asymmetric directionality: Iran is a full target (high d) because its nuclear sovereignty is constrained without exit; unilateral-withdrawal advocates are excluded from authority (not seated), so they experience the constraint as coercive closure of their preferred action. The P5+1 members benefit from the binding reading because it grants them consensus authority and veto power—Russia and China especially, as their veto becomes structurally valuable. The IAEA benefits (low d, near-zero extraction) because its verification mandate flows directly from the binding status. The non-proliferation regime (listed as a beneficiary, non-agent) is vindicated by this reading: the binding interpretation supports the regime's core claim that treaty commitments are irreversible. The directionality for the U.S. is genuinely mixed: it benefits from Iranian constraint (beneficiary), but it is also constrained by inability to unilaterally exit or snapback (payer). This reading should derive d ≈ 0.45–0.55 for the U.S., reflecting the dual position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Iranian nuclear weapons development) is contested: P5+1 negotiators say verified constraint solves it; withdrawal advocates say it persists unresolved. This contest maps to the reading divide: under the binding reading, the founding problem is substantially solved and the constraint persists as the mechanism of that solution. Under the transactional reading, the founding problem is incompletely addressed (Iranian missile development, regional role persist) and the constraint is therefore unjustified because it binds the actor to an insufficient solution. The mandatrophy question is whether the JCPOA persists because its founding function is live and valuable (binding reading = justified persistence) or because the institutional machinery is too entrenched to dissolve despite the founding problem being unresolved (binding reading = potential mandatrophy hidden by enforcement inertia). The measurement series showing theater_ratio rise from 0.28 to 0.42 (40% increase) suggests some drift toward performance: as the binding status becomes contested, more diplomatic energy goes into asserting that the treaty remains binding rather than verifying Iran's compliance. This is diagnostic of mandatrophy risk: if the binding mechanism itself becomes the constraint's primary function (rather than the non-proliferation coordination it was built for), mandatrophy has set in. The engine's mandatrophy detector should flag this story as a candidate for investigation, especially if the transactional reading's metrics show lower theater and higher genuine-function concentration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_fixation,
    'Does the JCPOA text unambiguously support the binding reading, or does its language permit multiple coherent readings?',
    'Formal legal interpretation by the International Court of Justice or a binding arbitral tribunal; systematic linguistic analysis of the treaty''s modification and withdrawal clauses; comparison with Vienna Convention on the Law of Treaties standard doctrine.',
    'If the text unambiguously supports binding, the competing readings are illegitimate and the binding reading is the only defensible constraint. If the text permits multiple readings, this reading and its siblings are all structurally live; classification would depend on which reading is endorsed by the authority structure (the UNSC, the P5+1).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_fixation, conceptual, 'Whether the JCPOA''s bindingness is textually determined or depends on the authority structure''s reading choice.').

omega_variable(
    authority_grounding_ambiguity,
    'What entity''s authority grounds the binding interpretation—the written JCPOA text itself, the Vienna Convention principles, or the P5+1 consensus that negotiated it?',
    'Analysis of which authority the enforcement machinery actually defers to: if UNSC consensus is required for snapback, the P5+1 consensus is the grounding authority, not the text alone. If Iran''s breach automatically triggers snapback, the text is the authority.',
    'If the text alone is the authority, the binding reading is more durable (survives changes in UNSC politics). If the P5+1 consensus is the authority, the reading is vulnerable to defection by a permanent member (as occurred with the U.S. 2018 withdrawal).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, empirical, 'What authority structure grounds the binding reading''s legitimacy.').

omega_variable(
    symmetry_of_binding,
    'Is the binding constraint symmetric—all parties equally bound to identical obligations—or asymmetric, with Iran bearing enrichment constraints while the U.S. bears only political/economic costs?',
    'Comparison of the costs of defection: for Iran, nuclear weapons capability forgone; for the U.S., re-imposition of sanctions costs and strategic alliance damage. If the costs are asymmetric, the reading may be extraction masquerading as coordination.',
    'If symmetric, the binding reading describes genuine coordination. If asymmetric (Iran''s costs >> U.S. costs), the reading is mining structural asymmetry under the cover of multilateral commitment language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_of_binding, empirical, 'Whether the binding constraint''s costs are symmetrically distributed across parties.').

omega_variable(
    transactional_vs_binding_foreclosure,
    'Could a party coherently hold both the binding reading (JCPOA is irreversible) and the transactional reading (JCPOA is voidable on bad-faith finding) simultaneously, or do they logically foreclose each other?',
    'Philosophical analysis of whether a single framework can accommodate both ''binding unless dissolved by consensus'' and ''voidable by unilateral bad-faith determination.'' This is the key to whether the readings coexist or foreclose.',
    'If foreclosing, one reading is false and the other true (high stakes for classification and precedent). If coexisting, both are live positions held by different parties, which means the real constraint is the CONTEST between readings, not either reading alone—a meta-constraint about treaty interpretation authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transactional_vs_binding_foreclosure, conceptual, 'Logical compatibility of binding and transactional readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(jcpo_tr_t0, observed).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(jcpo_tr_t5, observed).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(jcpo_tr_t10, observed).
narrative_ontology:measurement(jcpo_tr_t15, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(jcpo_tr_t15, observed).
narrative_ontology:measurement(jcpo_tr_t20, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(jcpo_tr_t20, observed).
narrative_ontology:measurement(jcpo_tr_t30, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(jcpo_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(jcpo_be_t0, observed).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(jcpo_be_t5, observed).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(jcpo_be_t10, observed).
narrative_ontology:measurement(jcpo_be_t15, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(jcpo_be_t15, observed).
narrative_ontology:measurement(jcpo_be_t20, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(jcpo_be_t20, observed).
narrative_ontology:measurement(jcpo_be_t30, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(jcpo_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(jcpo_su_t0, observed).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(jcpo_su_t5, observed).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(jcpo_su_t10, observed).
narrative_ontology:measurement(jcpo_su_t15, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(jcpo_su_t15, observed).
narrative_ontology:measurement(jcpo_su_t20, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(jcpo_su_t20, observed).
narrative_ontology:measurement(jcpo_su_t30, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(jcpo_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.18).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested JCPOA kernel. The binding reading holds that the JCPOA text creates irreversible multilateral commitments requiring consensus to modify or dissolve. The transactional reading treats the JCPOA as provisional and subject to unilateral exit. The graduated reading acknowledges binding status but proposes scaled enforcement tied to compliance degree. These are not measurements of the same constraint from different perspectives—they are different constraints instantiated by different reading choices of the same kernel text. All three must be authored and linked via network.affects_constraints to capture the constraint family. The binding reading (this story) forecloses the transactional reading's core claim (unilateral voidability) but coexists with the graduated reading (both accept binding status; they diverge on enforcement). Ε-invariance is preserved: each reading has its own ε, its own beneficiary/victim structure, its own type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__binding_multilateral_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
