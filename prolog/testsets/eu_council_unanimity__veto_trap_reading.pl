% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity as Minoritarian Veto-Trap (Extraction Reading)
 *   domain: institutional/international/political
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement for decision-making in most policy
 *   domains creates a structural vulnerability: any single member state can
 *   credibly threaten to block progress on collectively desired policies
 *   unless it receives compensation. This constraint story instantiates the
 *   VETO-TRAP READING of the unanimity kernel — the reading that emphasizes
 *   the extraction mechanism through blocking threats. The founding problem
 *   (preventing majoritarian coercion of small states) is CONTESTED: it
 *   persists as a real concern in high-stakes sovereignty domains, but
 *   empirical analysis of actual veto use shows that blocking is increasingly
 *   deployed for routine policy carve-outs and side payments unrelated to
 *   vital interests. The veto mechanism transforms from a sovereignty
 *   safeguard into a rent-extraction device. The measurement series documents
 *   the rising extractiveness and theater ratio over 25 years, indicating
 *   that veto use has drifted from its sovereignty-protection origins toward
 *   routinized minoritarian extraction. Suppression is moderate because the
 *   coalition majority retains structural power (can move to enhanced
 *   cooperation, amend treaties to require QMV, or impose coalition costs on
 *   blockers) but does not exercise it, suggesting partial internalization of
 *   the veto's legitimacy narrative alongside structural hold-up.
 *
 * KEY AGENTS:
 *   - Blocking state (e.g., Hungary, Poland, Cyprus): leverages unanimity to extract policy carve-outs, budget exemptions, sectoral waivers. Collects the extraction rent directly through concessions offered by the coalition majority to unlock consent.
 *   - Coalition majority (27 minus blocker): wishes to coordinate on harmonized regulation, joint investment, or common foreign policy; pays the blocking state's rent through diluted policy, carve-outs, or side payments to obtain its consent.
 *   - EU supranational institutions (Commission, Parliament, specialized agencies): would implement coordinated EU-wide policy but are blocked or forced to negotiate with veto holders; their institutional capacity and regulatory agendas are systematically held up by the extraction mechanism.
 *   - Academic and regulatory observers: produce empirical institutional analysis documenting the shift from sovereignty protection to routine rent-seeking; corroborate the founding problem's contested status.
 *   - Citizens and constituencies in the coalition majority: would benefit from the coordinated policy or harmonization that the veto delays or fragments; they are excluded from the Council negotiation and pay the extraction cost through foregone coordination gains.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.68).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.52).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity as Minoritarian Veto-Trap (Extraction Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional/international/political").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'c6447eb6-c9ad-44aa-82d7-18ea34052a62').
narrative_ontology:cs_kernel_codification('c6447eb6-c9ad-44aa-82d7-18ea34052a62', formalized).
narrative_ontology:cs_authority_grounding('c6447eb6-c9ad-44aa-82d7-18ea34052a62', extraction).
narrative_ontology:cs_interpretation_layer_present('c6447eb6-c9ad-44aa-82d7-18ea34052a62').
narrative_ontology:cs_reading_relation('c6447eb6-c9ad-44aa-82d7-18ea34052a62', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6447eb6-c9ad-44aa-82d7-18ea34052a62', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('c6447eb6-c9ad-44aa-82d7-18ea34052a62', foundational, veto_power_enables_minoritarian_extraction).
narrative_ontology:cs_axiom_status(veto_power_enables_minoritarian_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c6447eb6-c9ad-44aa-82d7-18ea34052a62', veto_power_enables_minoritarian_extraction, empirically_contingent).
narrative_ontology:cs_axiom('c6447eb6-c9ad-44aa-82d7-18ea34052a62', secondary, consensus_legitimacy_has_decayed_into_rent_collection).
narrative_ontology:cs_axiom_status(consensus_legitimacy_has_decayed_into_rent_collection, holdable).
narrative_ontology:cs_axiom_grounding('c6447eb6-c9ad-44aa-82d7-18ea34052a62', consensus_legitimacy_has_decayed_into_rent_collection, empirically_contingent).
narrative_ontology:cs_reference_frame('c6447eb6-c9ad-44aa-82d7-18ea34052a62', consensus_based_legitimacy_protection).
narrative_ontology:cs_drift_state('c6447eb6-c9ad-44aa-82d7-18ea34052a62', contemporary_rent_seeking_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6447eb6-c9ad-44aa-82d7-18ea34052a62', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_collective_interest).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, structural_veto_power_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A member state that leverages the unanimity requirement to extract concessions from the coalition majority. By credibly threatening to withhold consent on collective action (e.g., EU budget increases, harmonized regulation, common foreign policy), the blocking state secures side payments, budgetary carve-outs, sectoral exemptions, or policy modifications that advantage its position. The blocker's veto power gives it asymmetric bargaining leverage: the coalition majority often faces a worse outcome (no collective action) than the blocker does (status quo), so the blocker can credibly demand compensation to consent. Recent examples: Hungary blocking EU budget and foreign policy statements to extract rule-of-law exemptions or budgetary rebates; Poland blocking judicial harmonization to extract exemptions; Cyprus blocking military cooperation statements to extract recognition or carve-outs. The blocking state collects the extraction rent directly through these concessions.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_state, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, blocking_state, agenda_setter).

% The remaining 26 or 27 EU member states that wish to pursue coordinated action but are blocked until unanimity is achieved. They bear multiple costs: (1) policy delay — legislative cycles are extended; (2) policy dilution — the blocker's exemptions and carve-outs reduce policy effectiveness and EU-wide coherence (e.g., the blocker opts out of judicial reforms, creating two-tier justice standards); (3) side payments and budgetary transfers — the coalition majority offers the blocker additional resources or favorable treatment to unlock consent; (4) agenda capture — the blocker's veto power lets it set the negotiation agenda, forcing the coalition to prioritize the blocker's concerns. The majority's exit options are constrained: enhancing cooperation (bypassing the blocker) requires unanimity for the enhanced-cooperation structure itself, so it can be vetoed; amending treaties to require QMV also requires unanimity; and exiting the EU entirely (as Brexit showed) is extremely costly. The majority thus remains trapped in the unanimity framework and pays the extraction rent repeatedly.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority, payer,
    powerful, generational, constrained, global).

% The European Commission (proposes legislation), European Parliament (co-legislates in most domains), and specialized agencies (implement and regulate) are structurally excluded from the Council's unanimity-based power play. The Commission can propose regulations, but member states veto at the Council stage, blocking the entire regulatory agenda if a single state withholds consent. The Parliament has co-legislative power in many domains but cannot override a Council veto. Specialized agencies (e.g., the EMA — European Medicines Agency, the ECB — European Central Bank) are subordinate to Council decisions and cannot act when the Council is gridlocked by veto. The veto trap thus immobilizes supranational institutions, whose institutional capacity and agendas are hostage to member-state extractive behavior. Reform proposals from the Commission for QMV voting are repeatedly vetoed by the blocking states.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, supranational_institutions, excluded,
    institutional, generational, constrained, global).

% Citizens and interest groups in coalition-majority member states that would benefit from the harmonized policy or coordinated action that the veto delays or fragments. Example: patients in member states without access to medicines approved in other EU countries (the EMA can approve, but implementation is blocked by unanimity requirements for cross-border recognition); workers seeking harmonized labor standards (regulation proposed by Commission, blocked by veto); researchers wanting seamless cross-border funding and collaboration (Horizon program fragmented by carve-outs for blocking states). These constituencies cannot participate in the Council negotiation and cannot veto the blocker's extraction; they pay the cost through foregone coordination gains and fragmented policy. They are neither agents nor stakeholders in the Council's formal structure — they are the diffuse victims of minoritarian extraction.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, prospective_coordination_beneficiaries, excluded,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(eu_council_unanimity__veto_trap_reading, prospective_coordination_beneficiaries).

% Interest groups, political movements, and civil-society organizations that would benefit from specific coordinated policies (environmental regulation, social harmonization, fiscal coordination) observe the veto mechanism fragmenting these objectives. Environmental coalitions see green-economy directives weakened to exempt blocking states; labor organizations see labor standards harmonization blocked; social movements see asylum and migration policy fragmented by opt-outs. These coalitions cannot directly participate in Council voting but must lobby member states and supranational institutions to advance their agendas. The veto mechanism thus gives their preferences — and those of the coalition-majority member states that share them — lower priority than the blocking state's preference for exemption.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, competing_policy_coalitions, observer,
    organized, generational, constrained, global).

% Political economists (Tsebelis, Schneider), institutional analysts, and legal scholars who study EU governance empirically produce corroborating testimony about the veto trap's operation and costs. They document: (1) legislative gridlock datasets showing correlations between veto use and policy delay; (2) impact assessments of fragmented regulation (two-tier standards, competitive disadvantages); (3) comparative institutional analysis showing that qualified-majority-voting bodies (e.g., the European Parliament, the ECB's Governing Council) make faster decisions with less fragmentary outcomes; (4) classification of veto episodes into vital-interest categories vs. extractive-leverage categories, showing the rising fraction of extractive use. Their analyses provide external corroboration (R3 standard) that the founding problem has shifted from vital-interest protection to routine minoritarian rent-seeking.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, academic_and_regulatory_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_state).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The founding coordination function was legitimate: unanimity ensures that no member state is bound by collective decisions it believes fundamentally violate its vital interests or sovereignty. It requires all parties to negotiate until unanimous agreement emerges, theoretically preventing majoritarian coercion of minorities.
% TRANSFER_FUNCTION: In the veto-trap reading, the constraint transfers policy concessions, budgetary exemptions, regulatory carve-outs, and side payments from the coalition majority to the blocking state. The blocker credibly threatens to withhold consent on legislation or collective action that the majority desires; the majority pays by granting the blocker favored treatment, exemptions, or transfers in exchange for consent. The mechanism also transfers the negotiation agenda from parliamentary or supranational processes to bilateral hold-up negotiations between the blocker and the majority.
% ABSENT_VOICES: Supranational institutions (Commission, Parliament, specialized agencies) have no vote in the Council and cannot participate directly in the extraction negotiation — they can only lobby member states. Citizens and constituencies in coalition-majority member states that would benefit from coordinated policy (patients, workers, researchers, environmental advocates) are geographically dispersed, unrepresented in the Council, and cannot participate. Competing policy coalitions (green-economy advocates, labor organizations, social movements) likewise cannot directly influence the Council's veto-based negotiations.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement and its veto enforcement disappeared overnight and the EU shifted to qualified-majority voting, the world would rearrange substantially: (1) blocking states would lose their extraction power and would no longer be able to veto legislation they dislike — policies would be decided by majority vote; (2) the coalition majority would pass coordinated policies that are currently blocked or fragmented (harmonized regulation, joint investment, common foreign policy) without carve-outs for blocking states; (3) supranational institutions (Commission, agencies) would have their regulatory agendas unlocked and could implement decisions faster; (4) prospective beneficiaries of coordination would gain access to harmonized policy outcomes; (5) member states' bargaining positions would restructure — smaller states would lose their veto leverage and would need to rely on coalition-building instead. The incentive structures of all member states would shift from veto-based hold-up to coalition-based cooperation.
% FOUNDING_PROBLEM: The founding problem was: how can small or less-powerful EU member states protect themselves against majoritarian coercion by larger states seeking to impose policies that violate the smaller states' vital interests? The response was unanimity: any state can veto collective action that threatens its sovereignty, forcing all parties to negotiate until unanimous agreement emerges. This was a legitimate protection mechanism designed to ensure that no member state is forced into arrangements it fundamentally rejects.
% FOUNDING_PROBLEM_CORROBORATION: The blocking states (Hungary, Poland, Cyprus, and others) continue to invoke the founding problem as justification for veto use: they claim to be defending vital sovereignty interests against majoritarian coercion by larger states. However, institutional audits and academic analysis provide competing corroboration. Tsebelis, Schneider, and cross-national legislative-gridlock studies document that a growing fraction of veto use (estimated 60%+ in recent decades) is for routine policy carve-outs, sectoral exemptions, and side payments that have no connection to sovereignty protection — the blocker's core interest is merely advancing narrow economic or sectoral preferences. European Commission impact assessments and regulatory analyses corroborate that veto fragmentation produces policy ineffectiveness (two-tier standards, competitive disadvantages) inconsistent with the founding problem's legitimacy rationale. Legal scholars and comparative-institutional researchers corroborate that the mechanism has drifted from protecting vital interests toward routinized rent-seeking. In short: blocking states continue to invoke sovereignty protection, but external analysts document that most veto use serves extractive rent-seeking rather than protecting genuine vital interests.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 because blocking states systematically extract value from the coalition majority through credible veto threats: the threat point (no collective action) is often worse for the coalition than for the blocker, giving the blocker asymmetric bargaining power. The measurement series shows extractiveness rising from 0.38 to 0.68 over 25 years, documenting the drift from sovereignty-safeguard use (lower extractiveness in early EU when the founding problem was more acute) toward routine minoritarian rent-seeking (higher extractiveness in recent decades as blocking becomes normalized and institutionalized). Suppression is moderate (0.52) because the coalition majority technically has exits — enhanced cooperation procedures, treaty amendment to shift to QMV, or in extremis, selective coalitions outside the unanimity rule — but these are costly, require supermajorities themselves, and face blocking resistance. The majority is thus structurally trapped but not completely suppressed; they retain latent power but do not exercise it, suggesting internalized acceptance of the veto's legitimacy narrative. Theater ratio rises from 0.18 to 0.41 (rising throughout), indicating increasing performative justification: blocking states invoke sovereignty and vital interests (the founding-problem framing) to legitimize routine carve-outs and side payments that have nothing to do with genuine sovereignty protection. The 0.41 final ratio suggests that nearly half of veto-related negotiation activity is performative justification rather than substantive negotiation. Resistance is high (0.71) because the coalition majority and supranational institutions actively argue against blocking, propose QMV reforms, and document the costs of veto paralysis. The constraint persists not because resistance is weak but because reform requires unanimity (paradoxically), giving the blocking states a veto over the rule change itself.
 *
 * PERSPECTIVAL GAP:
 *   From the blocking state's seat, the unanimity rule is a legitimate sovereignty protection and a rational negotiation lever — the blocker exercises veto power to defend its interest and secure fair treatment. From the coalition majority's seat, the same rule operates as an extractive hold-up mechanism that delays collective action and extracts side payments for consent that should be freely granted (on the majority's framing). The engine computes this divergence: from the blocker's seat (moderate power, arbitrage exit, benefits), the constraint appears as ROPE (genuine coordination with legitimate safeguarding); from the majority's seat (powerful but constrained exit, pays costs), it appears as SNARE (extraction sustained by coercion and lack of alternatives). The author's claim (SNARE) reflects the majority's seat; the engine will compute the blocker's seat as ROPE or near-rope, depending on whether the blocker's interests genuinely align with the coordination problem or are purely extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking_state is the structural beneficiary (d near 0.0): it extracts concessions, receives carve-outs, and leverages its veto power to shift the negotiation agenda in its favor. Its power is 'powerful' and its exit_options are 'arbitrage' — it can threaten to block or to withdraw concessions, arbitraging between the coalition's preference for progress and its own preference for exemption. The coalition_majority are the structural targets (d near 1.0): they pay the extraction rent (diluted policy, carve-outs, side payments) and bear the cost of delay. Their power is 'powerful' (individually, they are large EU members) but their exit_options are 'constrained' — they cannot exit the unanimity framework without supermajority reform (which requires unanimity, so is blocked by the same veto holders). EU_collective_interest and prospective_joint_action are non-agents listed for narrative completeness; they are the abstract beneficiaries of coordination whose interests are fragmented by the extraction mechanism. The directionality derivation chain produces: blocker (arbitrage exit + beneficiary role → d~0.1); majority (constrained exit + payer role → d~0.85); non-agents (dropped from directionality). No overrides are needed; the structural data derives the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is CONTESTED: small states and some commentators maintain that unanimity protects vital sovereignty interests and should be preserved or enhanced. However, institutional audits of veto episodes show that a growing fraction of veto use (estimated 60%+ in recent decades) is for routine policy carve-outs, sectoral exemptions, or side payments that have no connection to sovereignty protection — the blocking state's core interest is merely advancing its narrow preference or financial interest. The constraint has evolved from its founding justification (preventing majoritarian sovereignty coercion) toward something that violates that justification's own terms: instead of protecting genuine vital interests, it enables minoritarian extraction for non-vital interests. This is mandatrophy — the founding problem's rationale has decayed, but the constraint persists as a vestigial rule. The theater ratio's rise from 0.18 to 0.41 documents the increasing gap between stated justification (sovereignty protection) and actual function (routine extraction). Reform is technically blocked by the same unanimity rule, creating a second-order mandatrophy: the constraint that has become obsolete cannot be reformed because it governs its own amendment. The measurement trajectory and the founding_problem_status='contested' encode this: the constraint's own founding legitimacy is now disputed even among its designed-in protectors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vital_interest_extraction_boundary,
    'Is the measured extractiveness (0.68) a rate of genuine vital-interest veto use (sovereignty protection), or a rate of routine extractive blocking (rent-seeking)?',
    'Institutional audit classifying 15 years of veto episodes by whether the blocking state''s stated justification involves genuine sovereignty/core interest (treaty renegotiation, cross-border security, fundamental identity claim) or routine preference/budget interest (sectoral exemption, trade carve-out, funding claim). Cross-validate against blocking state''s post-concession behavior: does it accept similar policies outside EU contexts, or only when exempted?',
    'If >60% is routine extractive (non-vital), the snare classification is confirmed and institutional reform (QMV shift) is justified. If >60% is vital-interest protection, the sovereignty_guarantor_reading is more accurate and the constraint requires enforcement norms (strict scrutiny of veto claims) rather than structural reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vital_interest_extraction_boundary, empirical, 'Whether extractiveness reflects vital-interest protection or routine minoritarian rent-seeking.').

omega_variable(
    kernel_framing_contest,
    'Is this constraint best characterized as a sovereignty-guarantor mechanism being misused for rent-seeking (the true structure is ROPE, and veto-trap is deviation), or as inherently structuring minoritarian extraction regardless of stated justification (the true structure is SNARE)?',
    'Counterfactual: implement strict-scrutiny norm enforcement for 10 years — only vetos genuinely implicating sovereignty are recognized; extraction-motivated vetos are treated as illegitimate. If extraction pressure disappears and blocking states stop using veto, the sovereignty_guarantor_reading is confirmed. If blocking continues under different justifications or reorganizes into finer-grained hold-up, the veto_trap_reading is confirmed as capturing the true structure.',
    'Confirms or contests the core reading choice: whether the unanimity kernel is legitimately protective (needs norm enforcement) or inherently extractive (needs structural reform via QMV).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Which reading of the unanimity kernel captures the true structural logic.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the coalition majority''s tolerance of veto extraction suppressed through structural hold-up (they cannot exit without supermajority reform, which the blocker can veto), or through internalized legitimacy narrative (they accept the veto''s sovereignty-protection framing even when extraction is evident)?',
    'Post-reform trajectory: if the EU successfully shifted to QMV on extraction-prone domains and the coalition majority immediately leveraged majority power without regret, suppression was structural. If they subsequently expressed misgivings about ''losing influence'' or invoked the veto norm, suppression was partly internalized.',
    'High internalization indicates the veto has become a cultural institution legitimizing extraction; reform alone would not suffice without norm-erosion. High structurality indicates pure hold-up power; structural reform would be sufficient to dislodge it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural dependency or internalized legitimacy.').

omega_variable(
    reform_paradox_sustainability,
    'The unanimity rule governs its own amendment: shifting to QMV requires unanimity, which means blocking states can veto the reform itself. Is this paradox sustainably locking the constraint in place, or can it be bypassed (treaty renegotiation, enhanced cooperation, coalition defection)?',
    'Institutional analysis of reform pathways: which have been attempted, which succeeded/failed, at what cost? Are blocking states willing to veto QMV reforms, or is the threat-of-veto sufficient to deter reform attempts without actual blocking?',
    'If the reform paradox is truly locking the constraint, the second-order mandatrophy is severe: the constraint cannot reform itself even though its founding problem has decayed. If bypasses exist (e.g., enhanced cooperation in non-unanimity domains), the lock is weaker and reform pressure may accumulate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_paradox_sustainability, empirical, 'Whether the unanimity rule''s self-amendment requirement creates a sustainable lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(eu_c_tr_t0, observed).
narrative_ontology:measurement(eu_c_tr_t5, eu_council_unanimity__veto_trap_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(eu_c_tr_t5, observed).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__veto_trap_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(eu_c_tr_t10, observed).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__veto_trap_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(eu_c_tr_t15, observed).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(eu_c_tr_t20, observed).
narrative_ontology:measurement(eu_c_tr_t25, eu_council_unanimity__veto_trap_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(eu_c_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t0, observed).
narrative_ontology:measurement(eu_c_be_t5, eu_council_unanimity__veto_trap_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(eu_c_be_t5, observed).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__veto_trap_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(eu_c_be_t10, observed).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__veto_trap_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(eu_c_be_t15, observed).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(eu_c_be_t20, observed).
narrative_ontology:measurement(eu_c_be_t25, eu_council_unanimity__veto_trap_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(eu_c_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(eu_c_su_t0, observed).
narrative_ontology:measurement(eu_c_su_t5, eu_council_unanimity__veto_trap_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(eu_c_su_t5, observed).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__veto_trap_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(eu_c_su_t10, observed).
narrative_ontology:measurement(eu_c_su_t15, eu_council_unanimity__veto_trap_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(eu_c_su_t15, observed).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__veto_trap_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(eu_c_su_t20, observed).
narrative_ontology:measurement(eu_c_su_t25, eu_council_unanimity__veto_trap_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(eu_c_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__veto_trap_reading, 0.18).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, qualified_majority_voting__alternative_mechanism).

% DUAL FORMULATION NOTE:
% The EU Council unanimity rule is decomposed into three constraint stories, each instantiating a different reading of the same kernel (formalized rule requiring unanimous consent to Council decisions). This story (veto_trap_reading) emphasizes the extraction mechanism and minoritarian leverage; the sovereignty_guarantor_reading emphasizes legitimacy protection; the diplomatic_capital_reading emphasizes consensus-building. Each reading produces a different ε, different type classification, and different beneficiary/victim structure. The three stories are linked via this network field to enable cross-reading comparative analysis. Do NOT collapse these into a single story — the contest between readings is the empirical content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
