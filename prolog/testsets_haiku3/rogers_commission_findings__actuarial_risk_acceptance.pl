% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__actuarial_risk_acceptance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Commission Actuarial Risk Acceptance Framework
 *   domain: organizational/safety/regulatory
 *
 * SUMMARY:
 *   The Rogers Commission investigation of the Challenger disaster produced
 *   technical findings about O-ring vulnerability at low temperature and
 *   management decision-making processes. This constraint story instantiates
 *   ONE reading of the Rogers kernel: the actuarial_risk_acceptance reading.
 *   Under this reading, the Commission's findings establish a framework for
 *   acceptable flight: quantify failure probability, document it, and permit
 *   informed decision-makers to authorize operations under bounded risk. This
 *   reading benefits mission planners and program administrators (who can
 *   schedule launches under documented uncertainty) and extracts compliance
 *   from safety doctrine and field engineers (whose absolute-safety position
 *   is subordinated to actuarial trade-offs). The framework's central
 *   tension: quantifying risk and obtaining authorized acceptance appear
 *   procedurally legitimate but may function as permission to operate
 *   hazardously under bureaucratic cover. The measurement series shows
 *   theater_ratio rising toward 0.5, indicating growing disjunction between
 *   the documented risk-management process and actual engineering
 *   uncertainty—the constraint increasingly operates as procedural theater
 *   maintaining the appearance of risk acceptance rather than robust safety
 *   management.
 *
 * KEY AGENTS:
 *   - Mission planners: institutional agenda-setters who benefit from permission to fly under documented risk
 *   - Program administrators: institutional beneficiaries who defend the framework in regulatory filings
 *   - Field engineers: identity-locked payers who know the hazard but lack institutional authority to halt missions
 *   - Safety doctrine (non-agent): abstract payer subordinated by the framework's reframing of absolutes as preferences
 *   - Informed decision-makers: institutional beneficiaries positioned as the locus of legitimate risk acceptance
 *   - External safety advocates: excluded from decision authority despite technical expertise
 *   - Regulatory authority: observer seat verifying procedural compliance rather than technical safety
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.68).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.72).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk Acceptance Framework").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational/safety/regulatory").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, '2c86ea48-ae9b-4eb0-822a-f347ee832ee9').
narrative_ontology:cs_kernel_codification('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', formalized).
narrative_ontology:cs_authority_grounding('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', extraction).
narrative_ontology:cs_interpretation_layer_present('2c86ea48-ae9b-4eb0-822a-f347ee832ee9').
narrative_ontology:cs_reading_relation('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', foundational, quantified_risk_legitimates_authorization).
narrative_ontology:cs_axiom_status(quantified_risk_legitimates_authorization, holdable).
narrative_ontology:cs_axiom_grounding('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', quantified_risk_legitimates_authorization, instrumental).
narrative_ontology:cs_axiom('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', foundational, informed_decision_maker_supremacy_over_engineer_veto).
narrative_ontology:cs_axiom_status(informed_decision_maker_supremacy_over_engineer_veto, holdable).
narrative_ontology:cs_axiom_grounding('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', informed_decision_maker_supremacy_over_engineer_veto, conventional).
narrative_ontology:cs_reference_frame('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', absolute_safety_doctrine_as_binding_boundary).
narrative_ontology:cs_drift_state('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', post_challenger_institutional_normalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2c86ea48-ae9b-4eb0-822a-f347ee832ee9', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, program_administrators).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_doctrine).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, field_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate under pressure to achieve scheduled launches and mission objectives. The actuarial framework permits flight with documented probability bounds, allowing them to weigh risk explicitly and proceed while the absolute-redesign approach would indefinitely halt operations. They author risk acceptance documents that certify their understanding of failure modes and probabilities. They benefit from the framework's permission to operate under quantified risk rather than wait for absolute technical certainty.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, agenda_setter,
    institutional, biographical, constrained, national).

% Defend the actuarial reading in regulatory filings and internal reviews. The framework permits them to demonstrate procedural compliance (risk documented, decision-makers informed) without requiring technical impossibilities (zero-failure redesign). They collect the political and budgetary benefit of maintained flight schedules and avoided indefinite program suspension.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_administrators, beneficiary,
    institutional, biographical, constrained, national).

% Know the technical vulnerability (O-ring erosion at low temperature) and see it documented in the risk acceptance calculus. They are constrained from halting flight by the institutional hierarchy: mission planners have authority to accept documented risk. Their professional identity (spacecraft engineers responsible for safety) is overridden by the institutional decision framework that permits known hazards to fly. Exit means career termination or institutional isolation; raising concerns escalates but does not block missions already approved under the actuarial framework.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, field_engineers, payer,
    moderate, biographical, identity_locked, national).

% The traditional engineering principle that safety-critical systems must not operate with known failure modes. The actuarial framework treats this as one input to a risk calculus, not a boundary condition. Probability, consequences, and resource constraints enter as trade-off factors. The framework extracts compliance from this doctrine by reframing its absolutes as preferences rather than constraints.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_doctrine, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_doctrine).

% Are positioned as the locus of legitimate risk acceptance. The framework does not constrain their choice: they are authorized to accept any probability if it is documented and they are informed. In practice, institutional hierarchy and schedule pressure shape what 'informed acceptance' means. They bear formal responsibility for the decision but sit sheltered from technical consequences by the probabilistic framing.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers, beneficiary,
    institutional, biographical, constrained, national).

% Include independent safety experts and whistleblowers who would argue for absolute redesign (engineering_absolute_threshold reading) but lack institutional authority within the decision framework. Their expertise is solicited for risk quantification but not for go/no-go authority. The actuarial framework absorbs their technical input into the probability estimate but does not grant them veto power.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, external_safety_advocates, excluded,
    moderate, biographical, constrained, national).

% Reviews the documented risk acceptance to verify process compliance. The actuarial reading frames regulatory oversight as auditing the decision procedure (was risk quantified, were decision-makers informed) rather than evaluating the technical boundary (is the risk actually acceptable). This shifts the regulatory burden from safety certification to procedural verification.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, regulatory_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, program_administrators).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables continued mission planning and institutional operations under documented uncertainty: rather than halt all flights until absolute technical guarantees exist, the framework permits flight when failure probability is quantified, consequences are understood, and authorized decision-makers formally accept the risk. Coordinates the competing institutional demands of safety rigor and mission schedules.
% TRANSFER_FUNCTION: Transfers authority to accept hazards from technical experts and safety doctrine to institutional administrators and mission planners. The transfer moves the locus of legitimate risk-taking from engineers (who must design safely) to decision-makers (who can authorize operation with known hazards). Moves compliance burden from redesign (technical, costly, time-consuming) to documentation (procedural, auditable).
% ABSENT_VOICES: Field engineers who understand the technical mechanism (O-ring erosion) are subordinated by institutional hierarchy to decision-makers with less direct technical knowledge. Independent safety advocates, if present, have no veto authority within the decision framework. The absolute-threshold reading (engineering_absolute_threshold) would demand their voice be the final authority; the actuarial reading absorbs their input into probability estimates but permits override by institutional decision-makers.
% DISAPPEARANCE_RATIONALE: If the actuarial risk acceptance framework were removed, the engineering_absolute_threshold reading would dominate: flights would halt until O-ring redesign was complete. The program would suspend for the duration of redesign and testing. Mission schedules would slip indefinitely. The program's budget and political support would face challenges during suspension. Institutional arrangements would revert to treating safety doctrine as a hard boundary rather than one factor in a risk calculus.
% FOUNDING_PROBLEM: The Rogers Commission was convened to investigate the Challenger disaster and establish legitimate grounds for resuming spaceflight operations. The actuarial reading addresses the problem: how can we fly again when we understand known hazards but cannot eliminate all risk? Answer: document the hazards, quantify their probability, obtain informed authorization from decision-makers, and proceed under acknowledged risk.
% FOUNDING_PROBLEM_CORROBORATION: Program administrators and mission planners attest the founding problem remains live: operational uncertainty requires decision-making frameworks that permit acceptable risk. Engineering safety experts and external advocates attest the founding problem was misdiagnosed: the actual problem was not 'how to make decisions under risk' but 'how to prevent operating with known technical hazards.' The Rogers Commission's own technical analysis documented the O-ring vulnerability; whether quantifying it and obtaining management sign-off answers the safety problem or merely permits hazardous operation under procedural cover is the lived contest between the readings.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the framework permits continued flight despite known hazards by shifting the locus of responsibility from engineers (who must design safely) to decision-makers (who can authorize operation). Suppression is substantial (0.72) because field engineers and external advocates are institutionally silenced: their objections are absorbed into probability estimates but do not block missions already approved through the actuarial framework. Theater ratio climbs toward 0.5 (starting at 0.38, ending at 0.51) because early in the interval the framework appears genuinely deliberative (risk is quantified, decision-makers are informed, oversight seems real), but over time the theater function—procedural performance of safety management—grows relative to actual technical revision. The initial extractiveness gap (early interval shows lower values) reflects the genuinely coordinating function: the framework does solve a real problem (how to operate under uncertainty). The rising trajectory indicates extraction accumulation: as the framework matures, it increasingly functions to permit known hazards under bureaucratic legitimacy rather than to improve technical safety. All measurements on a single shared time grid: every metric is authored at every examined point (0, 3, 6, 12, 18, 25).
 *
 * PERSPECTIVAL GAP:
 *   From the mission planner and administrator seats, the constraint is a legitimate coordination mechanism solving real institutional constraints. From the field engineer and safety advocate seats, it is enforced authorization to operate dangerously. The engine computes this divergence from the stakeholder positions: mission planners sit as agenda-setters with constrained exit (tied to the program), while field engineers sit as identity-locked payers (professional identity as safety-conscious engineers is overridden by institutional hierarchy). The same constraint structure produces cooperation (from planners) and suppressed resistance (from engineers). The framework's legitimacy depends on the actuarial reading holding: if decision-makers are truly informed and risk is genuinely quantified, authorization is procedurally sound. If decision-makers lack technical understanding or risk quantification is theater, the same structure permits hazardous flight under bureaucratic cover—which reading is true is what the temporal measurements track (rising theater_ratio suggests the latter).
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners and program administrators benefit from the framework's permission to operate under documented risk: they can maintain schedules, defend the program politically, and discharge institutional responsibilities. Their directionality (d) is low—they are subsidized by the arrangement. Field engineers pay through suppression: their safety concerns are absorbed into probability estimates but do not halt operations. Their directionality is high (d near target end). The identity-lock (exit_options: identity_locked for field engineers) amplifies the suppression: they cannot exit without abandoning professional identity. Safety doctrine is a non-agent payer (agent: false) because it is an abstract principle, not a real-world actor, but the framework extracts from it by reframing absolutes as one factor in a trade-off. External safety advocates are excluded (not in the decision loop) and would oppose the framework if consulted, but their exclusion itself is part of the constraint structure—institutional hierarchy determines whose voice reaches the decision-maker.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to resume spaceflight given uncertainty) and its status (contested) create a mandatrophy risk. Mission planners and administrators attest the founding problem is live: operational decisions still require frameworks for accepting risk. Engineers and advocates attest the founding problem was misdiagnosed: the real problem was preventing hazardous flight, not authorizing it under bureaucracy. The disappearance verdict (world_rearranges) indicates the constraint's operation is contingent: remove it and the engineering_absolute_threshold reading would dominate (flights halt until redesign). This is a genuine tangled_rope mandatrophy candidate: the coordination function (permits operations under uncertainty) is real, but extraction (subordinates field engineers and safety doctrine to institutional decision-makers) is substantial and requires active suppression (institutional hierarchy, exclusion of external voices, theater of procedural legitimacy). If the founding problem were declared dead (redesign completed, O-ring certified safe), the framework's justification collapses—it would become pure extraction, a snare. The contestation of founding_problem_status means the mandatrophy is unresolved: the program's authority to keep operating under the actuarial reading is challenged by those who read the Rogers findings as demanding absolute technical resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_actuarial_vs_threshold,
    'Is the Rogers Commission''s finding that O-ring failure probability was known before Challenger a finding that quantified risk acceptance is legitimate, or a finding that hazardous flight occurred due to decision-maker failure?',
    'Read the Commission''s explicit statements on what decision-making framework they endorse: do they recommend proceeding under bounded risk, or do they recommend not proceeding until technical redesign? The answer determines which sibling reading the Commission itself endorsed.',
    'If the Commission endorsed engineering_absolute_threshold (redesign first), then instantiating actuarial_risk_acceptance as the constraint is a misreading of the kernel, and this story misidentifies what constraint the Rogers findings actually establish. If the Commission was ambiguous or contradictory, the three readings genuinely coexist as the kernel''s indeterminacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_actuarial_vs_threshold, empirical, 'Whether the Rogers Commission''s own statements endorse the actuarial or threshold reading.').

omega_variable(
    informed_decision_maker_fiction,
    'In institutional practice, who are the ''informed decision-makers'' authorized to accept risk under this framework? Do they actually possess technical understanding of the hazards, or is informed acceptance a procedural fiction?',
    'Examine decision-making records (memos, testimonies, technical briefings) to establish what information decision-makers actually possessed and whether objections from technical experts were presented and overridden, or filtered out before the decision point.',
    'If decision-makers genuinely understood the risks and actively chose to accept them, the constraint is a legitimate tangled_rope (coordination + asymmetric extraction). If they were insulated from contrary technical views or lacked the expertise to evaluate probabilities, the constraint is a snare—apparent legitimacy masking institutional hazard-acceptance for schedule/budget reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_decision_maker_fiction, empirical, 'Whether informed decision-maker authorization is genuine understanding or bureaucratic theater.').

omega_variable(
    suppression_mechanism_hierarchy_vs_internalization,
    'Is field engineer suppression structural (institutional hierarchy forbids them from halting flights) or internalized (they internalize the decision-maker''s authority and accept their role as advisor rather than authority)?',
    'Post-constraint analysis: if engineers who left the program or the institution maintained their safety-first stance, suppression is primarily structural. If they absorbed the organization''s risk-acceptance framing, suppression is internalized and portable—they carry it after exit.',
    'If structural, the constraint''s effective suppression is lower (can be relieved by institutional reform or engineer mobility). If internalized, suppression is higher and more durable (persists in individual engineers even outside the institutional context). Affects classification toward snare (internalized suppression) vs. tangled_rope (structural but enforceable via institutional position).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_hierarchy_vs_internalization, empirical, 'Structural hierarchy vs. internalized engineer suppression.').

omega_variable(
    actuarial_reading_identity_lock_depth,
    'For field engineers, is identity-lock rooted in professional ethics (they identify as safety-conscious engineers and cannot psychologically align with hazardous operations) or career path dependence (they cannot exit without losing employment/credentials)?',
    'Narrative analysis: examine whether engineers who left the program cite principled opposition (professional identity) or practical barriers (career, family, location). If principled opposition dominates, identity-lock is axiological; if practical barriers dominate, it is economic.',
    'Identity-lock as axiological (professional conscience) makes suppression more resilient—engineers will continue advocating for safety even under institutional pressure, making the constraint require higher suppression to maintain. Identity-lock as economic (career path) makes it a capital-mediated exit trap, functionally similar to trapped exit_options, lowering the suppression requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_reading_identity_lock_depth, empirical, 'Depth and type of field engineer identity-lock: axiological vs. economic.').

omega_variable(
    sibling_reading_coexistence_stability,
    'The three Rogers readings (actuarial, threshold, compliance-narrative) coexist across different institutional seats. Is the coexistence stable (each seat can hold its reading indefinitely) or unstable (one reading will eventually dominate as institutional power consolidates)?',
    'Institutional trajectory analysis: track which reading''s framers accumulate power and authority over time. Stable coexistence appears as sustained alternation between readings (periods of threshold-enforcement, periods of actuarial-authorization, periods of compliance-theater). Unstable coexistence appears as one reading''s institutional proponents displacing others'' authority.',
    'If coexistence is stable, the three readings form a perpetual constraint family, and this story correctly models one seat''s actuarial reading alongside sibling stories modeling the others. If one reading eventually dominates, the kernel''s indeterminacy resolves and one constraint story becomes canonical. Affects long-term classification: a perpetually coexisting actuarial reading stays tangled_rope; a dominating reading either consolidates as rope (coordination-stable) or snare (extraction-stable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_stability, conceptual, 'Institutional stability of the three coexisting Rogers readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(roge_tr_t0, observed).
narrative_ontology:measurement(roge_tr_t3, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 3, 0.42).
narrative_ontology:measurement_basis(roge_tr_t3, observed).
narrative_ontology:measurement(roge_tr_t6, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 6, 0.46).
narrative_ontology:measurement_basis(roge_tr_t6, observed).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 12, 0.49).
narrative_ontology:measurement_basis(roge_tr_t12, observed).
narrative_ontology:measurement(roge_tr_t18, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 18, 0.5).
narrative_ontology:measurement_basis(roge_tr_t18, observed).
narrative_ontology:measurement(roge_tr_t25, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(roge_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(roge_be_t0, observed).
narrative_ontology:measurement(roge_be_t3, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 3, 0.51).
narrative_ontology:measurement_basis(roge_be_t3, observed).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(roge_be_t6, observed).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(roge_be_t12, observed).
narrative_ontology:measurement(roge_be_t18, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 18, 0.67).
narrative_ontology:measurement_basis(roge_be_t18, observed).
narrative_ontology:measurement(roge_be_t25, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(roge_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(roge_su_t0, observed).
narrative_ontology:measurement(roge_su_t3, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 3, 0.61).
narrative_ontology:measurement_basis(roge_su_t3, observed).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 6, 0.65).
narrative_ontology:measurement_basis(roge_su_t6, observed).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(roge_su_t12, observed).
narrative_ontology:measurement(roge_su_t18, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 18, 0.71).
narrative_ontology:measurement_basis(roge_su_t18, observed).
narrative_ontology:measurement(roge_su_t25, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(roge_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__actuarial_risk_acceptance, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% The Rogers Commission findings instantiate a contested kernel with three structurally distinct readings. This constraint story models the actuarial_risk_acceptance reading (risk quantification + informed authorization permits continued flight). Sibling stories model engineering_absolute_threshold (technical redesign required before flight) and management_compliance_narrative (procedural compliance sufficient for authorization). The three readings split on what legitimate grounds exist for resuming spaceflight: quantified probability, technical certainty, or documented compliance process. Each reading has its own beneficiary/victim structure, directionality profile, and classification. All three are linked via network.affects_constraints; commentary in each explains why the readings coexist and what would resolve the kernel's indeterminacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__actuarial_risk_acceptance, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
