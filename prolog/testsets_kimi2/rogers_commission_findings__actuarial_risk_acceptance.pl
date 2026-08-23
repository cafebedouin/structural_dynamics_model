% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Commission Actuarial Risk Acceptance Reading
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This constraint story instantiates the actuarial-risk-acceptance reading
 *   of the Rogers Commission findings. Under this reading, the Commission
 *   established a procedural rule: spaceflight may proceed if failure
 *   probabilities are quantified, documented, and formally accepted by
 *   informed decision-makers. The reading treats this as a governance
 *   innovation that coordinates continued operations under uncertainty, but
 *   it asymmetrically transfers catastrophic risk to flight crew and public
 *   while benefiting mission planners and program managers who gain mission
 *   continuity. The constraint is actively enforced through pre-flight review
 *   boards, risk-acceptance documentation, and sign-off chains. It is
 *   contested by an engineering-absolute-threshold reading (same kernel) that
 *   holds flight must cease until redesign is certified, and by a
 *   management-compliance-narrative reading that sees the actuarial apparatus
 *   devolved into ritual documentation.
 *
 * KEY AGENTS:
 *   - Mission planners (NASA program management): Primary beneficiary â gain schedule and budget continuity from quantified risk acceptance.
 *   - Program decision authorities (Launch directors, Program Managers): Agenda-setters â enforce the documentation and sign-off procedure.
 *   - Flight crew (Astronauts): Primary payer â bear the physical realization of accepted failure probabilities.
 *   - Engineering safety officers: Payer â their categorical safety norms and redesign recommendations are overridden by actuarial framing.
 *   - Public/taxpayers: Diffuse payer â fund the program and bear catastrophic downside risk without direct voice in launch decisions.
 *   - External oversight commissions: Analytical observers â review procedural compliance but rarely overturn substantive risk acceptance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.68).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.62).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk Acceptance Reading").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, '0c78b3cf-cdb7-4256-9657-efcef6ef9493').
narrative_ontology:cs_kernel_codification('0c78b3cf-cdb7-4256-9657-efcef6ef9493', fixed_text).
narrative_ontology:cs_authority_grounding('0c78b3cf-cdb7-4256-9657-efcef6ef9493', extraction).
narrative_ontology:cs_interpretation_layer_present('0c78b3cf-cdb7-4256-9657-efcef6ef9493').
narrative_ontology:cs_reading_relation('0c78b3cf-cdb7-4256-9657-efcef6ef9493', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('0c78b3cf-cdb7-4256-9657-efcef6ef9493', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('0c78b3cf-cdb7-4256-9657-efcef6ef9493', foundational, quantified_risk_sufficient_for_go).
narrative_ontology:cs_axiom_status(quantified_risk_sufficient_for_go, holdable).
narrative_ontology:cs_axiom_grounding('0c78b3cf-cdb7-4256-9657-efcef6ef9493', quantified_risk_sufficient_for_go, instrumental).
narrative_ontology:cs_axiom('0c78b3cf-cdb7-4256-9657-efcef6ef9493', foundational, managerial_authority_over_engineering_caution).
narrative_ontology:cs_axiom_status(managerial_authority_over_engineering_caution, holdable).
narrative_ontology:cs_axiom_grounding('0c78b3cf-cdb7-4256-9657-efcef6ef9493', managerial_authority_over_engineering_caution, conventional).
narrative_ontology:cs_reference_frame('0c78b3cf-cdb7-4256-9657-efcef6ef9493', actuarial_risk_governance).
narrative_ontology:cs_drift_state('0c78b3cf-cdb7-4256-9657-efcef6ef9493', post_challenger_operational_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c78b3cf-cdb7-4256-9657-efcef6ef9493', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, flight_crew).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, engineering_safety_officers).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, public_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive mission continuity, schedule adherence, and budget protection from the ability to document quantified failure probabilities and obtain formal sign-off rather than halting for absolute redesign. They schedule launches around risk-acceptance board cycles.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary,
    institutional, generational, constrained, national).

% Possess formal authority to accept documented failure probabilities and authorize flight. They define what counts as informed acceptance, convene review boards, enforce documentation requirements, and bear reputational liability if accepted risks materialize.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_decision_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear the physical realization of accepted failure probabilities. They are briefed on risk quantifications but do not set acceptance thresholds. Exit is constrained by career investment, specialized training, and the absence of alternative human spaceflight employers.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, flight_crew, payer,
    moderate, biographical, constrained, national).

% Their categorical safety assessments and redesign recommendations are overridden when managers document and accept probabilistic risk bounds. They bear the institutional cost of watching absolute safety norms displaced by actuarial framing that accepts known component vulnerabilities.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, engineering_safety_officers, payer,
    organized, generational, constrained, national).

% Fund the program through federal appropriations and bear catastrophic downside risk of accepted failure probabilities without individual voice in launch decisions. They cannot opt out of the national spaceflight risk pool.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, public_taxpayers, payer,
    powerless, civilizational, trapped, national).

% Review risk-acceptance documentation for procedural adequacy and conformance to policy. They rarely overturn the substantive probabilistic judgments of program authorities, functioning as analytical auditors rather than operational veto holders.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, external_oversight_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a procedural framework for continuing flight operations under uncertainty by quantifying failure probabilities and delegating acceptance authority to designated decision-makers, avoiding indefinite program paralysis while maintaining a paper trail of accountability.
% TRANSFER_FUNCTION: Transfers catastrophic risk consequences from institutional decision-makers to flight crew and public while delivering mission continuity and budget protection to program management; transfers authority over go/no-go decisions from engineering safety absolutists to managerial risk-acceptors.
% ABSENT_VOICES: Engineering safety absolutists who would halt all flights until categorical redesign is certified; astronauts who might refuse to fly if they held equal authority in risk acceptance; the general public who bear catastrophic risk without individual veto or direct representation in acceptance boards.
% DISAPPEARANCE_RATIONALE: If the actuarial acceptance requirement vanished, program managers would lose the procedural basis to continue flights with known component vulnerabilities. Operations would likely default to the engineering-absolute-threshold reading (halt until redesign) or face ungovernable political gridlock, fundamentally rearranging NASA mission tempo, contractor relationships, and institutional legitimacy.
% FOUNDING_PROBLEM: Post-Challenger paralysis: how to resume flight operations when absolute safety is impossible, redesign is expensive and time-consuming, and indefinite grounding threatens program survival and institutional legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Mission planners and NASA administrators attest the problem is live and the actuarial frame solved it. Engineering safety community and astronaut families attest from outside the beneficiary set that the founding problem was displaced rather than solved, shifting risk onto crew. The Rogers Commission itself provides neutral-ground testimony that procedural reform was necessary, without endorsing managerial empowerment as the sole solution.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is moderately high: the actuarial frame permits continued operations with known vulnerabilities, effectively socializing catastrophic risk onto crew and public while privatizing mission-continuity benefits to program management. Suppression (0.62) reflects the active marginalization of categorical safety engineering voices; accessibility_collapse (0.65) captures the substantial but incomplete delegitimation of 'halt until safe' alternatives. Resistance (0.58) is moderate: safety engineers and astronaut families resist, but the institutional weight of the actuarial frame and post-accident political pressure favor continuity. Theater ratio (0.48) shows substantial performative drift: over the interval, risk documentation has partially ritualized into compliance theater, though genuine quantification persists. Temporal measurements trace rising extraction and theater from 1986 to 2026 as the actuarial apparatus matured and absorbed programmatic pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the mission-planner seat, the constraint is indispensable coordination without which the program collapses under bureaucratic and budgetary paralysis. From the flight-crew and engineering-safety seats, the same structure is an institutionalized risk-shift that substitutes managerial sign-off for genuine safety margins. The engine will compute divergent per-seat types from these structural data: the beneficiary seat may compute toward rope or scaffold, while payer seats compute toward snare or tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners are structural beneficiaries (low d): the constraint subsidizes their organizational goals by providing a procedural path around absolute safety halts. Program decision authorities sit near symmetric (moderate d): they wield power but bear personal and institutional liability if accepted risks materialize. Flight crew, engineering safety officers, and public are structural targets (high d): they bear the realized costs of accepted probabilities without commensurate voice in setting the acceptance threshold. The public is particularly high-d due to powerlessness and trapped exit (taxation without opt-out).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction declarations. A pure coordination reading (rope) would ignore the asymmetric risk transfer to crew and public; a pure extraction reading (snare) would ignore the genuine epistemic and governance function of quantified risk assessment in complex systems. The tangled_rope classification captures that the same procedural mechanism coordinates mission continuity and extracts from safety absolutism. The founding problem (post-accident paralysis) is contested: mission planners claim it is live, while the safety community claims it has been solved by displacement rather than genuine remedy. This tension is structurally required for tangled_rope certification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the Rogers Commission finding establish an actuarial risk-acceptance framework, an engineering absolute threshold, or a management compliance narrative?',
    'Forensic textual analysis of the Commission report chapters on risk assessment, cross-referenced against subsequent NASA Policy Directives and actual launch decision memoranda to determine which reading was operationalized.',
    'If the actuarial reading is correct, the constraint is a tangled rope coordinating mission continuity at the expense of safety absolutism. If the engineering threshold reading is correct, the constraint is a scaffold or mountain of technical necessity. If the compliance narrative reading is correct, the constraint is a snare of theatrical accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the Rogers kernel this constraint instantiates').

omega_variable(
    actuarial_vs_categorical_suppression,
    'Is the marginalization of categorical safety objections under the actuarial frame structural (managerial authority and resource allocation) or internalized (engineers adopting probabilistic self-censorship)?',
    'Post-decision ethnographic study of NASA safety engineering culture: do safety officers continue to file categorical objections that are overruled (structural), or has the profession shifted to framing all concerns in probabilistic terms acceptable to managers (internalized)?',
    'If internalized, effective suppression is higher than the structural measure suggests and the constraint is more deeply embedded than institutional rules indicate; if purely structural, reform through authority restructuring may suffice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_vs_categorical_suppression, empirical, 'Whether suppression of safety absolutism is structural or internalized').

omega_variable(
    risk_quantification_fidelity,
    'Do the documented failure probabilities reflect genuine actuarial rigor, or are they shaped by programmatic pressure to produce acceptable numbers?',
    'Independent quantitative audit of historical NASA risk assessments against post-flight anomaly data and Bayesian recalibration to detect systematic optimism bias.',
    'If probabilities are systematically optimistic, the actuarial frame is extraction masquerading as rationality; if rigorously neutral, the coordination function is genuine and extraction lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_quantification_fidelity, empirical, 'Whether risk quantification is epistemically sound or politically calibrated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_actuarial_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.12).
narrative_ontology:measurement(rogers_actuarial_tr_t10, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 10, 0.24).
narrative_ontology:measurement(rogers_actuarial_tr_t20, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 20, 0.34).
narrative_ontology:measurement(rogers_actuarial_tr_t30, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 30, 0.42).
narrative_ontology:measurement(rogers_actuarial_tr_t40, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(rogers_actuarial_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rogers_actuarial_be_t10, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rogers_actuarial_be_t20, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(rogers_actuarial_be_t30, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(rogers_actuarial_be_t40, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rogers_actuarial_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rogers_actuarial_su_t10, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(rogers_actuarial_su_t20, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(rogers_actuarial_su_t30, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(rogers_actuarial_su_t40, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% The Rogers Commission findings kernel decomposes into three structurally distinct constraints: actuarial_risk_acceptance (this file), engineering_absolute_threshold, and management_compliance_narrative. They share the same source text but instantiate incompatible operational logics and directionality structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
