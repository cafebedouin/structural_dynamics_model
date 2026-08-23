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
 *   This story instantiates ONE reading of the contested kernel
 *   jcpoa_treaty_bindingness: the binding multilateral reading, under which
 *   the JCPOA is a binding multilateral instrument whose modification or
 *   dissolution requires consensus, unilateral withdrawal is a breach rather
 *   than a release, and sanctions reimposition runs through Security Council
 *   procedure rather than national determination. The referent for epsilon is
 *   the standing arrangement as it actually operated from signature through
 *   the present - signature, Implementation Day, US withdrawal, Iranian
 *   threshold escalation, Vienna-process stagnation - assessed by this
 *   reading's own lights. The sibling readings
 *   (transactional_provisional_reading, graduated_compliance_reading)
 *   instantiate DIFFERENT constraints with different epsilon values,
 *   different victim sets, and likely different classifications; they are
 *   separate files linked through the network, not hedges folded into this
 *   one. KEY AGENTS (by structural relationship): -
 *   united_states_federal_government: primary target-turned-defector
 *   (powerful/mobile) - the party whose unilateral freedom this constraint
 *   exists to remove, and the party that removed itself; -
 *   iran_islamic_republic: dual-positioned core party (moderate/trapped) -
 *   subsidized by relief, burdened by verification; - european_e3_parties:
 *   believing enforcers paying for the belief (institutional/constrained); -
 *   russia_and_china: agenda-setters capturing residual rents
 *   (institutional/constrained); - israeli_security_establishment and
 *   gulf_arab_states: excluded payers bearing uncompensated regional risk
 *   (powerful and organized, both trapped outside the conversation); -
 *   iranian_civilian_population: powerless diffuse payers beneath the entire
 *   structure; - iaea_inspection_regime: administering apparatus; -
 *   arms_control_policy_community: analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.58).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.35).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty (Consensus-Gated Modification and Dissolution)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:has_sunset_clause(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466').
narrative_ontology:cs_kernel_codification('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', fixed_text).
narrative_ontology:cs_authority_grounding('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', lineage).
narrative_ontology:cs_interpretation_layer_present('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466').
narrative_ontology:cs_reading_relation('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', foundational, pacta_sunt_servanda_absent_consensual_release).
narrative_ontology:cs_axiom_status(pacta_sunt_servanda_absent_consensual_release, holdable).
narrative_ontology:cs_axiom_grounding('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', pacta_sunt_servanda_absent_consensual_release, conventional).
narrative_ontology:cs_axiom('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', secondary, collective_determination_prerequisite_for_exit).
narrative_ontology:cs_axiom_status(collective_determination_prerequisite_for_exit, holdable).
narrative_ontology:cs_axiom_grounding('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', collective_determination_prerequisite_for_exit, conventional).
narrative_ontology:cs_reference_frame('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', consensus_bound_multilateral_treaty).
narrative_ontology:cs_drift_state('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', post_us_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e5449150-8e1e-4d0f-a9c5-0ca6a5ac3466', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_islamic_republic).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, european_e3_parties).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_and_china).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_inspection_regime).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_federal_government).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_federal_government).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, israeli_security_establishment).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, gulf_arab_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_population).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, international_business_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_islamic_republic).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, european_e3_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accepted enrichment caps, stockpile limits, and intrusive IAEA access in exchange for coordinated multilateral sanctions relief. After the US withdrawal restored unilateral sanctions, it remains nominally inside the arrangement while incrementally exceeding enrichment thresholds, keeping violation calibrated below the level that would hand its rivals a consensus trigger. Full exit would forfeit remaining relief, invite snapback attempts, and deepen isolation; the regime has invested political identity in both the deal's defense and its defiance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_islamic_republic, beneficiary,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_islamic_republic, payer).

% Champions of the binding reading: they treat the agreement as live law notwithstanding the US defection, built a special-purpose vehicle (INSTEX) to preserve lawful trade, and triggered the dispute-resolution mechanism over Iranian violations. They absorb the cost of maintaining a bargain whose relief side they cannot deliver without US financial clearing, and their firms bear the losses when Washington penalizes engagement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, european_e3_parties, beneficiary,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, european_e3_parties, payer).

% Permanent Security Council members whose concurrence is required for any modification or termination under this reading. They continue trade and military-technical cooperation with Iran, bear almost no compliance cost themselves, and gain strategically from rivals entangled in a constraint they help administer. Their assent is the practical gate on the arrangement's death.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_and_china, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_and_china, beneficiary).

% Provided the sanctions-relief side of the exchange and received verification of Iranian rollback in return. Domestic political reversal produced unilateral withdrawal in 2018 despite the binding reading's prohibition on exactly this act; the government bore reputational cost, lost inspection access, and then enforced a rival maximum-pressure architecture. Its demonstrated mobility is the empirical fact this reading's constraint exists to deny.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_federal_government, payer,
    powerful, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_federal_government, beneficiary).

% Administers the verification machinery: expanded access, continuous monitoring, quarterly reporting to the Board of Governors. Its mandate and budget grew with the deal; its authority depends on Iranian cooperation that has eroded since 2019. It can attest but not compel, and its findings are the raw material every other seat litigates.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_inspection_regime, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_inspection_regime, beneficiary).

% Excluded from the negotiation that set the terms governing its core security concern. Regards retained enrichment capacity and sunset clauses as an unacceptable glide path to a threshold state, campaigned against the arrangement's legitimacy in third capitals, and conducts covert operations against the program. It bears the consequences of an instrument it never agreed to and cannot veto.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, israeli_security_establishment, excluded,
    powerful, generational, trapped, regional).

% Excluded regional powers facing a realignment that strengthens their rival regardless of whether the arrangement holds or collapses. They hedge through independent normalization tracks and proxy competition, absorbing security externalities from both the sanctions-relief window and its aftermath without a seat in the Joint Commission.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, gulf_arab_states, excluded,
    organized, generational, trapped, regional).

% Bore the pre-deal sanctions depression, a brief relief dividend, and renewed economic collapse after the US exit. Compliance politics among the great powers is conducted over their livelihoods without their consent; currency collapse, medicine scarcity, and unemployment arrive regardless of which reading of the kernel prevails. Emigration is the only exit most households possess.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_population, payer,
    powerless, immediate, trapped, national).

% Signed aircraft, energy, shipping, and banking contracts during the relief window on the strength of the arrangement's apparent durability, then abandoned billions in committed investment when secondary sanctions returned. Their exit was real but costly, marking the exact boundary where the constraint's protection ended and jurisdictional arbitrage began.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, international_business_interests, payer,
    organized, immediate, mobile, global).

% Tracks inspection data, models breakout timelines, and authors the competing readings of the kernel from think tanks and academies. Holds no material stake in the arrangement's operation; its products are cited by every other seat in support of opposed conclusions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, arms_control_policy_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_and_china).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the trust-and-verification problem of reversibly constraining a nuclear program: centralized monitoring replaces parallel national intelligence estimates, defined enrichment caps replace open-ended escalation, a Joint Commission dispute channel replaces ad hoc crisis diplomacy, and coordinated sanctions relief replaces fragmented unilateral easing. The multilateral frame converts a clandestine-program crisis into a scheduled, inspectable process.
% TRANSFER_FUNCTION: Moves sanctions relief and trade access from the sanctioning states to Iran; moves verification transparency and enrichment constraints from Iran to the P5+1 and the IAEA; moves agenda control over compliance disputes to the Joint Commission and, ultimately, the Security Council.
% ABSENT_VOICES: The Israeli and Gulf Arab security establishments were excluded from the negotiation and hold no seat in the Joint Commission; Syrian and Yemeni proxy dimensions touched by the regional balance were unrepresented; Iranian civil society, which bears both the sanctions and the compliance burdens, has no voice anywhere in the machinery. Their absence is load-bearing: unanimity among the seated parties partly reflects who was never invited.
% DISAPPEARANCE_RATIONALE: If the binding frame vanished overnight, the E3 would lose the legal instrument anchoring their Iran policy, snapback threats would lose their procedural basis, Iran's enrichment program would lose the negotiated ceiling it currently tests against, and regional powers would reprice deterrence around an unconstrained program. Sanctions architecture, inspection access, and alliance positioning all hang on the arrangement's continued existence, even in its hollowed form.
% FOUNDING_PROBLEM: After the covert Natanz and Fordow facilities were exposed in 2002, the founding problem was preventing Iranian acquisition of nuclear weapons without preventive war: converting a clandestine enrichment effort approaching breakout capability into a verifiably capped, reversible program, in exchange for reintegration into the world economy.
% FOUNDING_PROBLEM_CORROBORATION: IAEA Board of Governors quarterly reports attest enrichment levels and stockpile growth beyond the original ceilings; independent proliferation analyses (e.g., Institute for Science and International Security assessments) and congressional testimony corroborate that the weaponization problem persists in altered, threshold form. No corroboration of the founding problem's resolution comes from the arrangement's own secretariat alone; conversely, the E3 attest the problem remains live, while Tehran attests its program is purely civilian. The status is genuinely disputed across seats.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   The claimed type is tangled_rope because the structure genuinely coordinates AND asymmetrically extracts through the same machinery, and it requires active enforcement to hold. The coordination half is real and was demonstrably productive: from Implementation Day through early 2018, verified enrichment rollback, stockpile reduction, and continuous monitoring solved a trust problem that neither parallel bilateral bargaining nor preventive strikes had solved, at materially lower cost than the alternatives. The extraction half is equally real: excluded regional powers bear security externalities they never consented to; Iranian civilians absorb sanctions whiplash from decisions made over their heads; international business interests absorbed billions in abandoned commitments at the exact moment the constraint's protection stopped; and the US seat bore the constraint's central demand - surrendered unilateral flexibility - until it demonstrated that a sufficiently powerful party could walk. Extraction rises across the interval (0.36 to 0.58) as the bargain's symmetry decays: after 2018 the remaining parties maintain obligations whose counterparty benefits have been unilaterally destroyed, while Russia and China bear almost nothing and gate the exits. Suppression falls (0.55 to 0.35) because the enforcement story is one of capacity decay, not intensification: the snapback threat was credible while the US anchored it, and the defection converted the constraint's coercive backbone into reputational pressure and E3 procedural protest. Theater rises past 0.5 (0.12 to 0.58) as the Joint Commission cycle, dispute-mechanism triggers, and censure resolutions increasingly perform fidelity to a bargain whose material substance has drained - classic Goodhart drift of process substituting for function. Accessibility collapse is moderate (0.42) because alternatives did not close: military action, maximum pressure, and transactional renegotiation remained visible and were partially exercised, which is precisely why this is not a mountain. Resistance is substantial (0.6): US withdrawal, calibrated Iranian threshold violations, regional lobbying, and domestic opposition in multiple capitals. All three tracked series run on one shared six-point grid (2015, 2017, 2019, 2021, 2023, 2025) so no metric row borrows another's endpoint. Coalition note: the powerless seat (iranian_civilian_population) has latent coalition potential - diaspora advocacy networks and periodic domestic protest waves - but repression and economic desperation have kept it from converting into constraint-shaping power within the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical text. From the russian_and_china seat, the arrangement is a low-cost instrument of rival management: binding enough to tax American and European flexibility, unenforced enough to never bind themselves. From the european_e3_parties seat, it is live law whose defense is a matter of systemic credibility - abandon pacta sunt servanda here and every nonproliferation bargain becomes provisional. From the united_states_federal_government seat, the same structure reads as an unconstitutional-style entanglement of executive discretion, and its 2018 exit was not a breach but a correction - which is the transactional sibling reading's home turf. From the excluded regional seats, the entire edifice is a great-power condominium that priced their security at zero. Same power atoms (institutional) sit on opposite sides of the extraction line because their beneficiary/victim declarations and exit options differ, not because their global standing does. The engine computes these divergences from the structural data; this story's claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for iran_islamic_republic (relief recipient, though its secondary payer position keeps it off the pure-beneficiary pole), european_e3_parties (regime-stability and verification gains, offset by real absorbed losses), russia_and_china (strategic gains at negligible cost), iaea_inspection_regime (mandate and budget growth), and the US secondary beneficiary position (verification value). Victim declarations drive high directionality for united_states_federal_government (the constraint's primary demand lands on it, and its mobile exit makes the extracted flexibility legible), israeli_security_establishment and gulf_arab_states (uncompensated, unconsented risk-bearing), iranian_civilian_population (full-cost bearing with zero exit), and international_business_interests (contract losses at the constraint's protection boundary). Trapped exit amplifies effective extraction for Iran and the excluded regional seats; mobile exit lets the US and the business seat realize their exit, which is why the constraint's suppression metric fell rather than rose as they left. No directionality overrides were needed: the beneficiary/victim declarations plus differentiated power atoms and exit options already separate the institutional seats (E3 versus Russia/China versus the US) along the lines the derivation produces.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - verifiable prevention of Iranian weaponization without war - is contested, not dead: the program is capped differently than it was, but threshold status has returned, so the arrangement persists while its original warrant is disputed. The classification discipline prevents two symmetrical errors. Reading the structure as pure extraction (snare) would erase the documented 2016-2018 coordination achievement: verification worked, cascade incentives were suppressed, and the alternative paths were all worse. Reading it as pure coordination (rope) would erase the excluded seats, the whipsawed civilians and contractors, and the rent now accruing to the agenda-setting seat that bears no burden. Tangled rope holds both truths: the same Joint Commission that verifies enrichment also launders the exclusion of the parties who pay for the region's instability. The rising theater ratio marks the drift vector: if the machinery continues performing fidelity while delivering neither relief nor rollback, the terminal attractor is piton - a consensus-gated shell administered by seats with no incentive to bury it and no capacity to revive it, costly to fix (prohibitive, given proliferation-cascade risk) and profitable only to those gating its death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta_transactional,
    'This constraint is one reading of kernel jcpoa_treaty_bindingness (reading: binding_multilateral_reading). What structurally changes if the transactional_provisional_reading governs instead?',
    'Author and compile the sibling story jcpoa_treaty_bindingness__transactional_provisional_reading and compare computed classifications: victim-set inversion (US as wronged party, Iran as bad-faith counterparty), epsilon reassessed over a provisional-voidable referent, enforcement re-read as self-help rather than breach-response.',
    'If the transactional reading governs, this reading''s tangled_rope profile likely shifts toward a snare-or-rope boundary with reversed directionalities: the seat this story codes as defector-payer becomes the aggrieved beneficiary, and the consensus gate becomes the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta_transactional, conceptual, 'Committer-frame omega recording the kernel membership and the structural delta to the transactional sibling reading.').

omega_variable(
    kernel_reading_delta_graduated,
    'This constraint is one reading of kernel jcpoa_treaty_bindingness (reading: binding_multilateral_reading). What structurally changes if the graduated_compliance_reading governs instead?',
    'Author the sibling story jcpoa_treaty_bindingness__graduated_compliance_reading: enforcement becomes proportional-assessment-driven rather than consensus-gated, snapback legitimacy attaches to graded violation indices rather than Security Council procedure, and the E3 dispute-mechanism triggers re-read as calibration acts rather than protest.',
    'If the graduated reading governs, the consensus-gate extraction identified here (agenda-setter veto rents) dissolves into a proportionality mechanism, and the theater trajectory may read as measurement-lag rather than hollowing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta_graduated, conceptual, 'Committer-frame omega recording the structural delta to the graduated-compliance sibling reading.').

omega_variable(
    unratified_agreement_bindingness,
    'Is a non-ratified executive political agreement, endorsed by Security Council Resolution 2231 but never presented as a Article-VII treaty or Senate-ratified instrument, actually binding under international law in the way this reading asserts?',
    'Adjudication: an ICJ advisory opinion, an arbitral proceeding, or authoritative doctrinal settlement on the legal status of UNSC-endorsed political agreements and the estoppel effect of Implementation-Day conduct.',
    'If the instrument is not legally binding, this constraint is a political commitment wearing treaty grammar - the coordination function stands but the breach framing collapses, the US exit stops being a violation, and the classification migrates toward rope or scaffold with the bindingness claim exposed as aspirational cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unratified_agreement_bindingness, empirical, 'Whether the arrangement''s bindingness is settled law or contested characterization - the load-bearing premise of this reading.').

omega_variable(
    snapback_mechanism_post_defection,
    'Does the Resolution 2231 snapback procedure survive the drafter''s own defection - can the E3 unilaterally trigger reinstated UN sanctions against Iranian objections, or did the US exit destroy the mechanism''s operability?',
    'Procedural test: an actual E3 snapback notification and the resulting Security Council and member-state responses, or authoritative legal analysis of whether Res 2231''s reverse-veto design survives a withdrawing participant.',
    'If snapback remains operable, the constraint retains enforcement teeth and the falling suppression series understates residual coercive capacity; if it is dead letter, the constraint''s enforcement is purely reputational and the piton-drift hypothesis strengthens considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snapback_mechanism_post_defection, empirical, 'Operational status of the arrangement''s central enforcement mechanism after the enforcer''s withdrawal.').

omega_variable(
    excluded_seats_extraction_status,
    'Do the security externalities borne by the excluded regional powers constitute extraction through the constraint, or the legitimate incidence of a majority bargain that could not have been concluded with every affected party at the table?',
    'Counterfactual institutional design analysis: whether a negotiating format admitting Israeli and Gulf participation was feasible at the time, and comparative outcomes from inclusive versus exclusive multilateral security bargains.',
    'If the exclusion was a feasible-to-avoid design choice, the excluded seats'' burden counts as structural extraction and raises effective chi on the beneficiary coalition; if inclusion was impossible, their burden is background cost and the extraction reading narrows to the compliance asymmetries alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_seats_extraction_status, preference, 'Whether exclusion of affected regional parties is extraction or unavoidable bargain geometry.').

omega_variable(
    sunset_hollowing_trajectory,
    'As the deal''s original restriction sunsets mature (Day 15 and Day 20-25 provisions) and the theater ratio climbs, is the arrangement transitioning from tangled rope toward piton - a consensus-gated shell maintained performatively by seats unable to bury or revive it?',
    'Continued temporal measurement past 2025: if theater_ratio sustains above 0.5 while base_extractiveness plateaus and no seat moves to terminate or restore, the piton signature is confirmed; a restored US entry or a consensual termination would falsify it.',
    'Confirmation would reclassify the constraint''s forward trajectory as inertial maintenance with diffuse costs and a capturing agenda-setter seat, shifting remediation analysis from renegotiation to managed burial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_hollowing_trajectory, empirical, 'Forward lifecycle question: tangled rope decaying toward piton as sunsets mature and performance substitutes for function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_bind_multi_tr_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(jcpoa_bind_multi_tr_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2017, 0.18).
narrative_ontology:measurement(jcpoa_bind_multi_tr_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2019, 0.34).
narrative_ontology:measurement(jcpoa_bind_multi_tr_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2021, 0.45).
narrative_ontology:measurement(jcpoa_bind_multi_tr_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2023, 0.52).
narrative_ontology:measurement(jcpoa_bind_multi_tr_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(jcpoa_bind_multi_be_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement(jcpoa_bind_multi_be_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2017, 0.4).
narrative_ontology:measurement(jcpoa_bind_multi_be_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2019, 0.46).
narrative_ontology:measurement(jcpoa_bind_multi_be_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2021, 0.51).
narrative_ontology:measurement(jcpoa_bind_multi_be_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2023, 0.55).
narrative_ontology:measurement(jcpoa_bind_multi_be_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_bind_multi_su_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(jcpoa_bind_multi_su_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2017, 0.52).
narrative_ontology:measurement(jcpoa_bind_multi_su_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2019, 0.44).
narrative_ontology:measurement(jcpoa_bind_multi_su_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2021, 0.4).
narrative_ontology:measurement(jcpoa_bind_multi_su_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2023, 0.37).
narrative_ontology:measurement(jcpoa_bind_multi_su_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the JCPOA's bindingness' decomposes into three structurally distinct constraints corresponding to the three readings of the kernel jcpoa_treaty_bindingness. This file is the binding_multilateral_reading (tangled_rope candidate: real coordination, asymmetric extraction, consensus-gated exits). The transactional_provisional_reading inverts the victim set and re-reads enforcement as self-help; the graduated_compliance_reading replaces the consensus gate with proportional assessment. The upstream claim in this family is the bindingness premise itself: each sibling cites or denies it, so degradation of this reading's credibility propagates to both siblings - if the instrument was never binding, the transactional reading's voidability becomes trivially true and the graduated reading's proportionality calculus loses its substrate. Family members link via affects_constraints in both directions of dependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
