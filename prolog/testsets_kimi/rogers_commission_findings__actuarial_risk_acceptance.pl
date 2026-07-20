% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Findings â Actuarial Risk Acceptance Reading
 *   domain: organizational safety / technology governance / regulatory compliance
 *
 * SUMMARY:
 *   This constraint story captures the actuarial_risk_acceptance reading of
 *   the Rogers Commission findings: the interpretation that the Commission
 *   established a procedural requirement permitting flight to proceed if
 *   failure probability is formally documented and accepted by informed
 *   decision-makers. This reading treats the Rogers report as legitimizing
 *   continued operations under probabilistic bounds rather than requiring an
 *   absolute engineering threshold or redesign certification before launch.
 *   It is one of three structurally distinct readings of the same kernel,
 *   competing with the engineering_absolute_threshold reading and the
 *   management_compliance_narrative reading. The structural delta
 *   concentrates benefit in mission planners and program management while
 *   imposing costs on categorical safety norms, the engineering safety
 *   organization, and flight crews.
 *
 * KEY AGENTS:
 *   - program_management (agenda_setter/institutional/mobile) â administers the risk acceptance framework and overrides engineering dissent
 *   - mission_planners (beneficiary/powerful/constrained) â gain flight continuity and schedule adherence
 *   - engineering_safety_org (payer/moderate/constrained) â loses absolute-threshold authority and bears institutional subordination
 *   - flight_crews (payer/moderate/identity_locked) â bear the physical risk of accepted failure probabilities
 *   - congressional_oversight (observer/institutional/analytical) â monitors from legislative distance
 *   - dissenting_engineers (excluded/moderate/constrained) â object but are overridden by management acceptance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.64).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.7).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.64).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Findings â Actuarial Risk Acceptance Reading").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational safety / technology governance / regulatory compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, '274b5150-08b8-4fd8-8409-38875da9b501').
narrative_ontology:cs_kernel_codification('274b5150-08b8-4fd8-8409-38875da9b501', formalized).
narrative_ontology:cs_authority_grounding('274b5150-08b8-4fd8-8409-38875da9b501', lineage).
narrative_ontology:cs_interpretation_layer_present('274b5150-08b8-4fd8-8409-38875da9b501').
narrative_ontology:cs_reading_relation('274b5150-08b8-4fd8-8409-38875da9b501', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('274b5150-08b8-4fd8-8409-38875da9b501', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_axiom('274b5150-08b8-4fd8-8409-38875da9b501', foundational, actuarial_sufficiency_for_flight_authority).
narrative_ontology:cs_axiom_status(actuarial_sufficiency_for_flight_authority, holdable).
narrative_ontology:cs_axiom_grounding('274b5150-08b8-4fd8-8409-38875da9b501', actuarial_sufficiency_for_flight_authority, instrumental).
narrative_ontology:cs_axiom('274b5150-08b8-4fd8-8409-38875da9b501', foundational, management_prerogative_to_accept_quantified_risk).
narrative_ontology:cs_axiom_status(management_prerogative_to_accept_quantified_risk, holdable).
narrative_ontology:cs_axiom_grounding('274b5150-08b8-4fd8-8409-38875da9b501', management_prerogative_to_accept_quantified_risk, conventional).
narrative_ontology:cs_reference_frame('274b5150-08b8-4fd8-8409-38875da9b501', actuarial_authority_frame).
narrative_ontology:cs_drift_state('274b5150-08b8-4fd8-8409-38875da9b501', post_columbia_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('274b5150-08b8-4fd8-8409-38875da9b501', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, engineering_safety_org).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, flight_crews).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, probabilistic_risk_assessment).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, management_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Rogers Commission findings as authorizing a risk-quantification requirement for flight readiness. Reviews and signs off on actuarial failure-probability documentation, exercising institutional authority to override categorical engineering no-go recommendations when informed decision criteria are met.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_management, agenda_setter,
    institutional, generational, mobile, national).

% Gains schedule continuity and mission throughput from a decision rule that permits flight operations under documented probability bounds rather than requiring absolute engineering certification or redesign completion before each launch.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary,
    powerful, biographical, constrained, national).

% Bears the erosion of categorical safety thresholds and engineering no-go authority. Their absolute-standard recommendations are subordinated to management actuarial sign-off, and the institutional weight of dissent is reduced to probabilistic inputs.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, engineering_safety_org, payer,
    moderate, biographical, constrained, national).

% Bear the physical consequences of missions where known component failure probabilities have been documented and accepted rather than eliminated. Professional identity and mission commitment bind them to a flight schedule shaped by management risk acceptance.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, flight_crews, payer,
    moderate, immediate, identity_locked, national).

% Monitors NASA safety culture and the implementation of Rogers recommendations from a legislative and budgetary distance. Can impose structural reforms or funding conditions but does not participate in individual flight-risk decisions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, congressional_oversight, observer,
    institutional, generational, analytical, national).

% Would object to launches where actuarial bounds override unresolved engineering uncertainty or incomplete component understanding. Their objections are heard in the safety process but are ultimately subordinate to management risk-acceptance authority.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, dissenting_engineers, excluded,
    moderate, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal decision framework that allows complex, high-risk technological operations to continue under uncertainty by substituting documented probabilistic bounds and informed management sign-off for absolute pre-flight safety guarantees.
% TRANSFER_FUNCTION: Transfers the authority to authorize flight from engineering certification of absolute safety to management acceptance of quantified risk; transfers physical risk exposure from the organizational mission schedule to the flight crews, and transfers the institutional cost of safety override to the engineering safety organization.
% ABSENT_VOICES: Engineers who hold that unknown failure modes or incomplete data should categorically preclude flight regardless of documented probability; crew members who bear the physical risk but are not seated in the risk-acceptance decision chain.
% DISAPPEARANCE_RATIONALE: If the actuarial risk acceptance requirement disappeared overnight, NASA would lose the primary post-Rogers mechanism used to authorize launches in the presence of known component risks. Flight schedules would halt pending absolute engineering certification or redesign, and the institutional balance between program management and engineering safety would invert toward categorical no-go authority.
% FOUNDING_PROBLEM: How to maintain flight operations and program momentum after the Challenger disaster when absolute safety guarantees are impossible, engineering dissent is chronically overridden by schedule pressure, and a procedural mechanism is needed to legitimate continued launch under known risk.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission documented the breakdown of risk communication but did not unambiguously endorse actuarial acceptance as the remedy. Post-Columbia investigation boards contested that the founding problem had been solved, arguing the actuarial frame legitimized continuing drift. External aerospace safety researchers and accident-investigation literatures corroborate the persistence of schedule pressure overriding safety signals.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.64, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.64) is substantial because the constraint systematically transfers safety authority from engineering to management, allowing schedule pressure to operate through a veneer of rational documentation. Suppression (0.70) is high because the constraint's persistence requires actively overriding or marginalizing engineering no-go recommendations that rest on categorical rather than probabilistic reasoning. Theater ratio (0.48) reflects that an increasing share of risk-documentation activity performs legitimacy for management decisions rather than materially altering safety outcomes. Accessibility collapse (0.62) indicates that once the actuarial frame is institutionalized, the alternative of absolute engineering refusal becomes organizationally illegible. Resistance (0.58) reflects persistent but institutionally defeated engineering dissent. The measurement series share a single time grid to prevent misaligned temporal sampling.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (program management) experiences the constraint as a necessary governance tool that prevents operational paralysis under uncertainty. The payer seats (engineering safety org, flight crews) experience it as the institutionalization of schedule-over-safety. The beneficiary seat (mission planners) sees genuine coordination enabling mission continuity. The excluded seat (dissenting engineers) sees suppressed voice. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners are the concentrated structural beneficiary of continued operations (low d). Engineering safety org and flight crews are structural targets (high d) because the constraint's operation explicitly subordinates their safety interests to management risk acceptance. Program management sits near symmetric on the beneficiary side for procedural authority but carries agenda-setting power. Dissenting engineers are structurally excluded, which the engine reads as trapped and target-proximate. Congressional oversight is analytical and outside the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both coordination and extraction markers. It is not a pure rope because identifiable victims (engineering safety org, flight crews) bear asymmetric costs that are not incidental coordination overhead. It is not a pure snare because there is a genuine coordination function: complex space systems cannot operate without some framework for decision-making under uncertainty. The tangled_rope classification captures this hybridity, where the same structure coordinates mission planners while extracting from safety advocates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the Rogers Commission report structurally endorse actuarial risk acceptance as a requirement, or does it criticize the specific failure of risk communication that led to Challenger?',
    'Close textual analysis of the Rogers report recommendations versus its findings, cross-referenced with post-Columbia reinterpretations by the Columbia Accident Investigation Board.',
    'If the report is found to criticize rather than endorse actuarial acceptance, this constraint collapses toward a management_compliance_narrative or engineering_absolute_threshold reading, reclassifying the beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this reading is a faithful or projected interpretation of the Rogers kernel').

omega_variable(
    actuarial_accuracy_in_tail_risk,
    'Do actuarial probability estimates for shuttle components accurately capture tail risks, or do they systematically produce overconfident bounds that legitimate unsafe flight?',
    'Retrospective statistical analysis of predicted versus actual failure modes across the shuttle program; comparison with high-reliability organization safety data from domains with similar redundancy.',
    'If estimates are systematically overconfident, the coordination function is undermined and the constraint shifts toward snare; if accurate, the extraction is bounded by genuine information value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_accuracy_in_tail_risk, empirical, 'Empirical validity of the probabilistic risk estimates that ground flight authority').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of engineering dissent under the actuarial frame structural (hierarchical management authority) or internalized (engineers adopting the probabilistic frame and self-censoring)?',
    'Ethnographic and documentary study of NASA safety organization decision logs; post-exit interviews with engineers who left the program after risk-acceptance overrides.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s effective extractiveness is higher than documented; if purely structural, reform could be achieved by hierarchy revision alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of engineering safety dissent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_actuarial_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.3).
narrative_ontology:measurement(rogers_actuarial_tr_t6, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 6, 0.35).
narrative_ontology:measurement(rogers_actuarial_tr_t12, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 12, 0.4).
narrative_ontology:measurement(rogers_actuarial_tr_t18, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 18, 0.45).
narrative_ontology:measurement(rogers_actuarial_tr_t24, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 24, 0.48).
narrative_ontology:measurement(rogers_actuarial_tr_t30, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(rogers_actuarial_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(rogers_actuarial_be_t6, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(rogers_actuarial_be_t12, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(rogers_actuarial_be_t18, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 18, 0.64).
narrative_ontology:measurement(rogers_actuarial_be_t24, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(rogers_actuarial_be_t30, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rogers_actuarial_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(rogers_actuarial_su_t6, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(rogers_actuarial_su_t12, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(rogers_actuarial_su_t18, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(rogers_actuarial_su_t24, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(rogers_actuarial_su_t30, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Rogers Commission findings kernel. The actuarial_risk_acceptance reading is downstream of the formal report text and competes with the engineering_absolute_threshold and management_compliance_narrative readings for institutional authority. The epsilon values and victim/beneficiary structures differ structurally across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
