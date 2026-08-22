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
 *   Following the Challenger disaster, the Rogers Commission issued findings
 *   that reframed acceptable risk in spaceflight. THIS READING instantiates
 *   the actuarial-risk-acceptance interpretation: Rogers findings establish a
 *   requirement that flight operations may continue if failure probability is
 *   documented and formally accepted by informed decision-makers. The
 *   constraint distributes the power to authorize flight from categorical
 *   safety rules to probabilistic acceptance procedures. Mission planners and
 *   schedule authorities benefit operationally; engineers and categorical
 *   safety norms bear the cost of a framework that permits flights they
 *   believe remain dangerous. The claim and metrics are independent: the
 *   constraint is CLAIMED as tangled_rope (coordination of risk-decision
 *   authority plus asymmetric extraction from engineers), while the authored
 *   metrics describe substantial extraction (0.68), high suppression (0.72 —
 *   suppression of engineer veto and categorical refusal), and rising theater
 *   (approaching 0.5 — performative acceptance replacing substantive
 *   redesign). The measurement series show extraction rising through t=15,
 *   then slightly declining at t=20 (likely reflecting O-ring redesigns and
 *   mitigation measures that reduced the perceived risk, changing the
 *   calculus).
 *
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
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk Acceptance Framework").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational/safety/regulatory").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, '6c07da45-163c-4105-aa4d-b47c0bc5cb06').
narrative_ontology:cs_kernel_codification('6c07da45-163c-4105-aa4d-b47c0bc5cb06', fixed_text).
narrative_ontology:cs_authority_grounding('6c07da45-163c-4105-aa4d-b47c0bc5cb06', lineage).
narrative_ontology:cs_interpretation_layer_present('6c07da45-163c-4105-aa4d-b47c0bc5cb06').
narrative_ontology:cs_reading_relation('6c07da45-163c-4105-aa4d-b47c0bc5cb06', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('6c07da45-163c-4105-aa4d-b47c0bc5cb06', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('6c07da45-163c-4105-aa4d-b47c0bc5cb06', foundational, probabilistic_acceptance_sufficient_for_flight).
narrative_ontology:cs_axiom_status(probabilistic_acceptance_sufficient_for_flight, holdable).
narrative_ontology:cs_axiom_grounding('6c07da45-163c-4105-aa4d-b47c0bc5cb06', probabilistic_acceptance_sufficient_for_flight, instrumental).
narrative_ontology:cs_axiom('6c07da45-163c-4105-aa4d-b47c0bc5cb06', foundational, quantified_risk_documentation_enables_informed_authorization).
narrative_ontology:cs_axiom_status(quantified_risk_documentation_enables_informed_authorization, holdable).
narrative_ontology:cs_axiom_grounding('6c07da45-163c-4105-aa4d-b47c0bc5cb06', quantified_risk_documentation_enables_informed_authorization, empirically_contingent).
narrative_ontology:cs_reference_frame('6c07da45-163c-4105-aa4d-b47c0bc5cb06', actuarial_risk_authorization_paradigm).
narrative_ontology:cs_drift_state('6c07da45-163c-4105-aa4d-b47c0bc5cb06', contemporary_post_columbia_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6c07da45-163c-4105-aa4d-b47c0bc5cb06', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, nasa_schedule_authority).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, risk_averse_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authorized by the Rogers framework to proceed with missions once failure probability is quantified and documented, even when that probability remains elevated. Gains operational continuity and schedule preservation under an actuarial accountability structure rather than categorical prohibition. Their exit is constrained by institutional mission mandates and budget cycles tied to launch schedules.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary,
    institutional, biographical, constrained, national).

% Administers the Rogers framework by requiring engineers to quantify risk and certify informed decision-maker acceptance. Controls the certification process and the definition of 'adequately documented' and 'sufficiently informed.' Can adjust the rigor of acceptance criteria while remaining within the actuarial paradigm. Collects the operational benefit of schedule preservation.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, nasa_schedule_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Are required to quantify failure probabilities they believe are unknowable or too high to accept, then watch those quantifications be used to justify flight. Their professional identity and ethical commitments are fused with engineering-for-safety; exit means accepting that calculations replace categorical refusal, or leaving the profession. They bear the reputational cost when quantified-but-likely failures occur.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, risk_averse_engineers, payer,
    moderate, biographical, identity_locked, national).

% The doctrine that unacceptable risks must be eliminated by design, not managed by documentation and acceptance. The Rogers framework replaces this norm with probabilistic acceptance. Listed as victim here because the constraint's operation vindicates a competing norm (probabilistic accountability) at the expense of categorical safety. As a non-agent doctrine, it does not negotiate or resist, but it is what is structurally sacrificed.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).

% Receive information about quantified risks and are authorized to accept them. Their role is formal: they must attest understanding and acceptance, but the framework offers them the same exit that mission planners have — if missions are delayed, so are their budgets and reputations. True power to refuse is constrained by institutional incentives aligned with schedule preservation.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers, beneficiary,
    powerful, biographical, mobile, national).

% Congress, NASA Inspector General, and external review boards that can audit whether the actuarial framework has been applied with appropriate rigor. They observe and can challenge specific certifications or the framework itself, but cannot directly reverse the constraint without legislative or executive action.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, oversight_bodies, observer,
    institutional, generational, analytical, national).

% Bear the ultimate consequence of failure but are excluded from the decision-making process. Their objections are not systematically solicited. They depend on decision-makers to refuse flights they believe unsafe, but the Rogers framework shifts the question to documented probability acceptance rather than engineer/crew veto. Their exclusion is the institutional arrangement, not an incidental gap.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, shuttle_crews, excluded,
    moderate, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, nasa_schedule_authority).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform decision protocol: risks are quantified, documented, and formally accepted by designated authorities rather than left to individual engineer judgment or categorical prohibition. Solves the coordination problem of aligning technical judgment (engineers' risk assessment) with organizational authorization (decision-makers' mandate).
% TRANSFER_FUNCTION: Moves operational authority from categorical safety rules to actuarial acceptance procedures. Transfers risk-decision power from engineers (who can say no) to mission planners and decision-makers (who must say yes once probability is documented). Transfers reputational and epistemic authority from absolute-threshold safety engineering to probabilistic-acceptance management.
% ABSENT_VOICES: Shuttle crews are structurally excluded from acceptance decisions despite bearing the failure consequence. A crew veto right would fundamentally alter the constraint's operation. Contractors and O-ring manufacturers face institutional incentives to participate in the risk-quantification process, not to resist it — their absence from the framework's governance is institutional, not accidental.
% DISAPPEARANCE_RATIONALE: If the actuarial-acceptance framework vanished, engineers could return to categorical refusal authority, schedule pressures would force either design fixes or explicit prohibition, and decision-making would revert to technical veto rather than documented acceptance. The Shuttle program would reorganize around one of the sibling readings (engineering_absolute_threshold) or a hybrid approach.
% FOUNDING_PROBLEM: The Challenger disaster revealed that O-ring failure risk was quantifiable and known to some engineers, but the categorical-threshold approach did not force explicit risk acceptance or create accountability for proceeding despite known risk. The Rogers Commission sought to establish a framework where risk is explicit, measured, and formally accepted by authorized decision-makers—turning tacit toleration into documented choice.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission itself, established as the official investigative body, documented the founding problem in its 1986 report: the Space Shuttle program had evolved with known risks that were not surfaced or formalized in acceptance decisions. NASA Administrator statements and congressional testimony from outside the agency corroborate the framing. However, debates about whether documented acceptance adequately addresses the problem vs. whether categorical design fixes are still preferable remain live among independent engineers and oversight bodies.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at end) because the framework permits flight despite documented risk, enabling schedule preservation that benefits mission planners at the cost of proceeding under conditions engineers classified as unacceptable. Suppression is correspondingly high (0.72) because the constraint's persistence requires suppressing engineer veto authority and categorical refusal norms — the institutional machinery actively prevents engineers from simply stopping the launches. Theater ratio rises to 0.5+ because acceptance documentation becomes increasingly performative: the procedure of 'documenting and accepting' risk substitutes for the action of 'redesigning to eliminate it.' As O-ring mitigation measures mature (t>15), the documented risk decreases, reducing extraction pressure, which explains the slight decline to 0.68 at t=20. Accessibility alternatives (switching to non-commercial spaceflight, pausing the program, redesigning before flying) remain partially collapsed: the institutional mission mandate constrains exit; yet the rising theater ratio suggests that categorical alternatives (simply refusing unsafe flights) are increasingly recognized as theoretically available but practically overridden by schedule authority.
 *
 * PERSPECTIVAL GAP:
 *   From the mission planner seat, the Rogers framework is genuine coordination: it replaces ad hoc risk tolerance with systematic documentation and accountability, making risk explicit and authorized. From the engineer seat, it is enforced extraction: they are required to participate in a process that legitimizes flights they believe should not happen. The NASA schedule authority seat and the risk-averse engineer seat experience this constraint inversely — one collects, the other pays — despite the same institutional framework. The engine's per-seat computation should surface this divergence through directionality: engineers get high d because they are targets; mission planners get low d because they are beneficiaries, even though both occupy the same organizational chart.
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners and NASA schedule authority are structural beneficiaries: they collect operational continuity under a framework that legally permits flight despite known risks (d near 0.0 for them). Risk-averse engineers are structural targets: they are required to quantify risks, participate in acceptance procedures, and then watch those procedures justify flights they believe unwise (d near 1.0 for them — high extraction of their professional judgment, high suppression of their veto right). Categorical safety norms are the non-agent victim: the constraint operates by replacing 'acceptable only when safe by design' with 'acceptable when risk is quantified and accepted,' vindicating a competing norm at its expense. Informed decision-makers sit between: they have formal power to refuse, but their institutional role (supporting the mission) and incentive structure (budget and reputation tied to schedule) constrain their true exit, pushing d toward 0.6–0.7 (high directionality toward target status despite nominal authority). Crews are identity-locked — they must accept the decisions made on their behalf or exit the space program entirely, an identity-dissolving choice for those whose professional identity fuses with spaceflight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Challenger revealed tacit risk tolerance) remains live and the disappearance verdict (world_rearranges) confirms the constraint's necessity. However, the theater_ratio rising to 0.5+ signals mandatrophy drift: acceptance documentation is increasingly performative, not substantive. If the theatrical maintenance (the ritual of risk quantification and acceptance) were to become the primary function while actual risk mitigation stalls, the constraint would degrade toward piton status—persisting by institutional inertia and procedural maintenance rather than genuine coordination. Currently it sits at tangled_rope: real coordination (risk is explicit; decision-makers are accountable) coexists with real extraction (engineers are suppressed; flight proceeds despite their judgment; categorical safety norms are overridden). The boundary between these two readings and management_compliance_narrative is that THIS reading operationalizes permission-to-fly; the compliance reading operationalizes permission-to-document. The distinction is ε-invariant: the actuarial reading extracts from categorical safety; the compliance reading extracts from the appearance of control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantification_epistemic_status,
    'Are failure probabilities for O-ring performance under shuttle launch conditions genuinely quantifiable, or is quantification a procedure that produces a number without reducing actual uncertainty?',
    'Post-hoc analysis of whether quantified probabilities matched observed failure rates across subsequent flights; epistemological assessment of whether the failure mode is regular enough to permit statistical modeling.',
    'If genuinely quantifiable, the actuarial framework is coordination via better information; if not genuinely quantifiable, quantification is the illusion that permits extraction—engineers are required to produce numbers they believe to be artifacts, which are then used to authorize flight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantification_epistemic_status, empirical, 'Whether O-ring failure probability is epistemically quantifiable or procedurally constructed.').

omega_variable(
    informed_decision_maker_autonomy,
    'Do the designated informed decision-makers possess actual authority to refuse flight despite documented acceptance criteria being met, or is their authorization role procedurally constrained by mission and budget pressures?',
    'Historical case review of decisions where documented risks met acceptance criteria; investigation of instances where decision-makers refused despite meeting criteria, or their reasoning where they did not refuse.',
    'True autonomy makes the framework genuinely coordinative (authorization is backed by real power); constrained autonomy reveals extraction (the decision is predetermined by schedule authority, and documentation is the ritual that legitimizes it).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informed_decision_maker_autonomy, empirical, 'Whether decision-maker acceptance is authentic authorization or procedural rubber-stamping.').

omega_variable(
    categorical_safety_reversion,
    'Is the categorical safety norm (redesign to eliminate unacceptable risk rather than documenting and accepting it) structurally available to the space program, or has the actuarial framework become institutionally locked such that reversion is path-dependent and costly?',
    'Institutional history: whether subsequent mission redesigns were pursued to reduce quantified risks, or whether the framework permitted indefinite flight at accepted risk levels; cost analysis of switching back to categorical safety standards.',
    'If reversion is genuinely available, the victim (categorical safety norms) retains an exit and the extraction is contestable; if reversion is locked out, the extraction becomes structural and the constraint approaches snare-level persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_safety_reversion, conceptual, 'Whether actuarial acceptance is reversible or institutionally locked.').

omega_variable(
    engineer_suppression_mechanism,
    'Is the suppression of engineer veto authority structural (legal/institutional prohibition) or internalized (professional norm that they must participate in the process even if they disagree with its conclusions)?',
    'Post-constraint transition: if engineer dissent persists or resurfaces after the actuarial framework is removed or modified, the suppression was partly internalized. If it immediately reappears upon framework change, the suppression was primarily structural.',
    'Structural suppression is lower-cost for the agenda-setter (institutions can be changed); internalized suppression is higher-extraction and more durable (it persists even after the institutional mechanism is gone).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(engineer_suppression_mechanism, empirical, 'Whether engineer veto suppression is structural or internalized.').

omega_variable(
    kernel_reading_coexistence,
    'Can the three Rogers readings (actuarial_risk_acceptance, engineering_absolute_threshold, management_compliance_narrative) coherently coexist in a single decision process, or do they foreclose one another?',
    'Textual analysis of the Rogers Commission report for signals that it endorses one reading over others; institutional history showing which reading was operationalized in post-1986 shuttle management and whether pressure to change was met with reinstatement or reframing.',
    'If coexistent, the readings represent institutional pluralism (different seats implement different readings); if foreclosing, one reading necessarily displaces the others and the choice between them is the real constraint. This determines whether the kernel itself is the salient unit or whether the readings are genuinely independent constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether Rogers readings coexist or foreclose one another in institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 20).

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
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 10, 0.5).
narrative_ontology:measurement_basis(roge_tr_t10, observed).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 15, 0.53).
narrative_ontology:measurement_basis(roge_tr_t15, observed).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(roge_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(roge_be_t0, observed).
narrative_ontology:measurement(roge_be_t3, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 3, 0.59).
narrative_ontology:measurement_basis(roge_be_t3, observed).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 6, 0.63).
narrative_ontology:measurement_basis(roge_be_t6, observed).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(roge_be_t10, observed).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 15, 0.7).
narrative_ontology:measurement_basis(roge_be_t15, observed).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(roge_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(roge_su_t0, observed).
narrative_ontology:measurement(roge_su_t3, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 3, 0.68).
narrative_ontology:measurement_basis(roge_su_t3, observed).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 6, 0.71).
narrative_ontology:measurement_basis(roge_su_t6, observed).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(roge_su_t10, observed).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(roge_su_t15, observed).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(roge_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__actuarial_risk_acceptance, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% The Rogers Commission findings form a constraint family across three structurally distinct readings of the same kernel. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different types. The actuarial_risk_acceptance reading (this file) emphasizes probabilistic authorization and benefits mission planners at the cost of categorical safety norms. The engineering_absolute_threshold reading (sibling) emphasizes design-phase safety fixes and benefits categorical safety. The management_compliance_narrative reading (sibling) emphasizes procedural governance and benefits compliance administrators. All three readings cite the same Rogers Commission authority; the kernel's underdetermination permits all three. They are linked via network.affects_constraints because institutional choices between them propagate to related constraints (launch approval procedures, engineering standards, contractor oversight). Each story carries the others in its cs_structure.reading_relations block.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__actuarial_risk_acceptance, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
