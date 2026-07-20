% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__management_compliance_narrative, []).

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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Compliance Process: Documented Risk Awareness Sufficient to Proceed
 *   domain: organizational safety / technology governance / regulatory compliance
 *
 * SUMMARY:
 *   The Rogers Commission findings on the Challenger disaster are interpreted
 *   by NASA management as establishing a compliance process: if risk
 *   awareness is documented and mitigation efforts are recorded as
 *   'sufficient,' launch may proceed. This reading preserves management's
 *   launch authority and program continuity while subordinating engineering
 *   safety objections to a paperwork threshold. The constraint operates as a
 *   procedural filter that transfers safety veto power from technical
 *   officers to programmatic managers.
 *
 * KEY AGENTS:
 *   - - program_management: Agenda-setter and beneficiary (institutional/mobile) â defines 'sufficient' documentation and captures program continuity
 *   - - engineering_safety_officers: Primary target (moderate/constrained) â bear the extraction of their technical veto authority
 *   - - actuarial_risk_analysts: Excluded voice (moderate/constrained) â their quantified-threshold perspective is structurally absent from the compliance narrative
 *   - - congressional_oversight: Analytical observer (institutional/analytical) â monitors implementation without altering daily decision rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.62).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.58).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Compliance Process: Documented Risk Awareness Sufficient to Proceed").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational safety / technology governance / regulatory compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '7faacae9-f613-4134-a08e-5f810a96468a').
narrative_ontology:cs_kernel_codification('7faacae9-f613-4134-a08e-5f810a96468a', fixed_text).
narrative_ontology:cs_authority_grounding('7faacae9-f613-4134-a08e-5f810a96468a', extraction).
narrative_ontology:cs_interpretation_layer_present('7faacae9-f613-4134-a08e-5f810a96468a').
narrative_ontology:cs_reading_relation('7faacae9-f613-4134-a08e-5f810a96468a', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('7faacae9-f613-4134-a08e-5f810a96468a', rogers_commission_findings__actuarial_risk_acceptance, influences).
narrative_ontology:cs_axiom('7faacae9-f613-4134-a08e-5f810a96468a', foundational, documented_rationale_suffices_for_proceed).
narrative_ontology:cs_axiom_status(documented_rationale_suffices_for_proceed, holdable).
narrative_ontology:cs_axiom_grounding('7faacae9-f613-4134-a08e-5f810a96468a', documented_rationale_suffices_for_proceed, conventional).
narrative_ontology:cs_axiom('7faacae9-f613-4134-a08e-5f810a96468a', foundational, management_retains_launch_discretion).
narrative_ontology:cs_axiom_status(management_retains_launch_discretion, holdable).
narrative_ontology:cs_axiom_grounding('7faacae9-f613-4134-a08e-5f810a96468a', management_retains_launch_discretion, conventional).
narrative_ontology:cs_reference_frame('7faacae9-f613-4134-a08e-5f810a96468a', programmatic_continuity_framework).
narrative_ontology:cs_drift_state('7faacae9-f613-4134-a08e-5f810a96468a', post_rogers_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7faacae9-f613-4134-a08e-5f810a96468a', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_safety_officers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the risk documentation standards and determines what constitutes 'sufficient mitigation' to authorize launch. Benefits from continued program milestones and budget continuity. Interprets the Rogers findings as requiring process compliance rather than operational stand-down.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_management, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, program_management, beneficiary).

% Responsible for identifying technical risks and recommending mitigations. Their formal objections can be overridden once management judges the documentation record sufficient. Bear the structural cost of subordinated technical authority.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_safety_officers, payer,
    moderate, biographical, constrained, national).

% Advocate for quantified probability thresholds and formal risk acceptance criteria. Their perspective is structurally excluded from the compliance narrative, which relies on qualitative managerial judgment of 'sufficient' mitigation rather than actuarial rigor.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, actuarial_risk_analysts, excluded,
    moderate, biographical, constrained, national).

% Monitors NASA's implementation of Rogers recommendations through hearings and funding authority. Does not sit in the daily launch decision chain but can impose structural reforms. Currently observes without altering the management compliance framework.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, congressional_oversight, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a standardized, auditable process for documenting and reviewing flight risks before launch decisions, ensuring that risk awareness is systematically recorded and considered by decision-makers.
% TRANSFER_FUNCTION: Transfers final launch authority from technical safety judgment to programmatic management discretion, contingent on producing documentation that management itself deems sufficient.
% ABSENT_VOICES: Engineers advocating for absolute flight suspension pending hardware redesign, and actuarial-risk analysts seeking quantified probability thresholds, are structurally sidelined because the narrative frames their concerns as satisfiable through paperwork rather than operational change.
% DISAPPEARANCE_RATIONALE: If the compliance-process constraint vanished, management would lose the procedural mechanism that legitimizes launch despite unresolved technical objections. Program tempo would slow, and engineering safety officers would regain de facto veto authority over flight readiness.
% FOUNDING_PROBLEM: Post-Challenger, there was no structured, accountable method for management to acknowledge and weigh known engineering risks before launch; decisions were ad hoc and verbally communicated, leaving no audit trail.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission documented the ad-hoc communication failure that preceded the Challenger loss. However, corroboration that the current compliance process still addresses a live problem is absent from outside the beneficiary set; external safety reviewers and subsequent accident investigation boards (e.g., Columbia Accident Investigation Board) have attested that the process became ceremonial and did not prevent later safety erosion.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the process systematically transfers go/no-go authority from engineering judgment to managerial discretion. Suppression (0.58) is moderate because the constraint must actively suppress the alternative engineering-absolute-threshold reading and contain technical dissent. Theater ratio (0.45) is moderate because documentation requirements are real but increasingly performative, serving to justify predetermined launch schedules. Accessibility collapse (0.50) reflects that alternatives like unilateral engineering stand-down become harder to advocate once the compliance process is institutionalized. Resistance (0.55) captures ongoing but structurally contained engineering opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the management seat, the constraint is a necessary governance innovation ensuring informed, accountable risk acceptance. From the engineering safety officer seat, the same structure is a mechanism for overriding technical judgment with administrative sign-offs. The engine computes this divergence from the structural data: identical institutional setting, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Program management is the structural beneficiary (low d): the constraint subsidizes their authority by supplying a procedural legitimacy framework they control. Engineering safety officers are the structural target (high d): the constraint extracts their veto capacity and replaces it with a documentation requirement they do not adjudicate. Congressional oversight sits near neutral (analytical exit). Actuarial analysts are excluded, receiving no directional flow.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure snare would miss the genuine coordination function of documented risk review; classifying it as a pure rope would miss the asymmetric extraction of engineering veto power. Tangled rope captures both: there is a real information-coordination problem (how to ensure risks are seen before launch), but the same structure is weaponized to convert technical objections into administratively manageable paperwork, enabling programmatic continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_legitimacy,
    'Is the management compliance narrative a good-faith operationalization of the Rogers findings, or a strategic misreading that extracts program continuity by suppressing technical safety thresholds?',
    'Archival analysis of NASA internal memos and risk acceptance documents post-1986 to determine whether ''sufficient mitigation'' criteria were calibrated to genuine safety outcomes or to preserve launch schedules.',
    'If a misreading, the coordination function is largely cover and the constraint migrates toward snare; if good-faith, the extraction is the necessary cost of administrative risk governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Ambiguity about whether this reading legitimately derives from the Rogers kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of engineering veto power accomplished through structural process design, internalized professional submission, or both?',
    'Post-exit interviews and whistleblower testimony from engineers who left NASA: if they report continued psychological pressure to accept managerial risk judgments after leaving, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s hold on engineering safety officers is deeper than procedural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of engineering dissent').

omega_variable(
    process_theater_vs_function,
    'Has the compliance process become primarily theater, or does it still produce genuine risk discovery?',
    'Independent safety audits comparing documented mitigation records against actual hardware and process modifications implemented before launch.',
    'If theater dominates, the coordination component has atrophied and the constraint approaches piton or snare; if genuine, it remains a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(process_theater_vs_function, empirical, 'Whether documentation requirements still produce real risk mitigation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.2).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__management_compliance_narrative, theater_ratio, 4, 0.28).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__management_compliance_narrative, theater_ratio, 8, 0.35).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__management_compliance_narrative, theater_ratio, 12, 0.4).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__management_compliance_narrative, theater_ratio, 16, 0.43).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(roge_su_t16, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% The rogers_commission_findings kernel decomposes into three structurally distinct constraints: management_compliance_narrative (process compliance sufficient to proceed), engineering_absolute_threshold (flight must cease until redesign), and actuarial_risk_acceptance (quantified probability with informed acceptance). Each reading has a distinct epsilon, beneficiary/victim structure, and type classification. This reading is the management compliance instantiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
