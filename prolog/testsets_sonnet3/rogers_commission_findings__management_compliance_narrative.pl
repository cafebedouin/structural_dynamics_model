% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Commission Compliance-Narrative Reading: Documented Risk Awareness Sufficient to Proceed
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the contested Rogers
 *   Commission findings kernel: that the findings establish a compliance
 *   PROCESS, under which management may proceed with flight operations upon
 *   demonstrating documented risk awareness and mitigation efforts, rather
 *   than requiring proof of a resolved technical threshold or a quantified,
 *   informed-consent risk acceptance. Under this reading, program management
 *   retains launch authority; the coordination function is real (a workable
 *   decision procedure under irreducible uncertainty) but the process is also
 *   structurally extractive — it converts engineering objections into inputs
 *   to a management-authored narrative rather than binding constraints, and
 *   the documentation record itself becomes evidence of due diligence
 *   regardless of whether the underlying risk was actually mitigated. This
 *   reading is deliberately narrow: it does not describe the
 *   engineering-threshold reading (a hard technical gate) or the actuarial
 *   reading (formal probability quantification and informed acceptance);
 *   those are separate constraints with their own ε values, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - program_management: agenda_setter (institutional/arbitrage) — controls what counts as sufficient documentation
 *   - launch_schedule_stakeholders: beneficiary (institutional/arbitrage) — gains program continuity without exposure
 *   - engineering_veto_authority: payer (organized/constrained) — objections absorbed into paper trail, not binding
 *   - frontline_engineers: payer (powerless/trapped) — raise specific warnings that are logged and overridden
 *   - astronaut_crews: payer (powerless/trapped) — bear the physical consequence with no visibility into the process
 *   - safety_oversight_boards: observer (institutional/analytical) — post-hoc review of whether the standard was met in good faith
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.62).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.58).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Compliance-Narrative Reading: Documented Risk Awareness Sufficient to Proceed").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '24b983b5-471a-4695-a14b-0b494358994a').
narrative_ontology:cs_kernel_codification('24b983b5-471a-4695-a14b-0b494358994a', formalized).
narrative_ontology:cs_authority_grounding('24b983b5-471a-4695-a14b-0b494358994a', extraction).
narrative_ontology:cs_interpretation_layer_present('24b983b5-471a-4695-a14b-0b494358994a').
narrative_ontology:cs_reading_relation('24b983b5-471a-4695-a14b-0b494358994a', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('24b983b5-471a-4695-a14b-0b494358994a', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('24b983b5-471a-4695-a14b-0b494358994a', foundational, documented_mitigation_effort_is_sufficient_condition_to_proceed).
narrative_ontology:cs_axiom_status(documented_mitigation_effort_is_sufficient_condition_to_proceed, holdable).
narrative_ontology:cs_axiom_grounding('24b983b5-471a-4695-a14b-0b494358994a', documented_mitigation_effort_is_sufficient_condition_to_proceed, conventional).
narrative_ontology:cs_axiom('24b983b5-471a-4695-a14b-0b494358994a', foundational, management_retains_final_launch_authority_over_engineering_veto).
narrative_ontology:cs_axiom_status(management_retains_final_launch_authority_over_engineering_veto, holdable).
narrative_ontology:cs_axiom_grounding('24b983b5-471a-4695-a14b-0b494358994a', management_retains_final_launch_authority_over_engineering_veto, conventional).
narrative_ontology:cs_reference_frame('24b983b5-471a-4695-a14b-0b494358994a', post_challenger_flight_readiness_review_baseline).
narrative_ontology:cs_drift_state('24b983b5-471a-4695-a14b-0b494358994a', pre_columbia_operational_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24b983b5-471a-4695-a14b-0b494358994a', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, launch_schedule_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_veto_authority).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, frontline_engineers).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, astronaut_crews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the launch decision process; requires that risk be 'documented and mitigated' rather than 'eliminated' before proceeding. Controls what counts as sufficient documentation, and can waive or reinterpret engineering objections that do not rise to a formally coded no-go criterion. Bears schedule and budget pressure from above and answers for program continuity, not for individual technical judgments.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_management, agenda_setter,
    institutional, biographical, arbitrage, national).

% Political sponsors, contractors, and mission planners whose funding and reputational continuity depend on maintaining launch cadence. They benefit whenever a compliance narrative lets a flight proceed rather than triggering a costly stand-down, and face no personal exposure when the mitigation-sufficiency judgment turns out wrong.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, launch_schedule_stakeholders, beneficiary,
    institutional, generational, arbitrage, national).

% Engineers and engineering management who can raise technical objections but, under this reading, cannot stop a launch once management judges the documented mitigation record adequate. Their concerns are absorbed into a paper trail rather than treated as binding; the constraint converts their authority into an input to management's narrative rather than a threshold management must clear.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_veto_authority, payer,
    organized, immediate, constrained, national).

% Individual engineers (e.g. Thiokol seal engineers) who raise specific technical warnings the night before flight. Their objections are logged, discussed, and then overridden through a management sign-off process; they have no formal channel to force a stand-down and career risk attaches to escalating further.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, frontline_engineers, payer,
    powerless, immediate, trapped, local).

% Bear the physical consequence of the launch decision. They have no visibility into or vote on whether the documented mitigation record was actually sufficient; the compliance process operates entirely upstream of their knowledge and consent.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, astronaut_crews, payer,
    powerless, immediate, trapped, local).

% Post-hoc review bodies (including the Rogers Commission itself and later the CAIB) that examine whether the documented-mitigation standard was met in good faith or used as a rationalization vehicle. They can recommend structural changes to the decision process but do not control any individual launch decision in real time.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, safety_oversight_boards, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable decision procedure for proceeding under irreducible technical uncertainty: rather than requiring proof of zero risk (which no complex system can supply), it requires a documented record showing risks were identified and mitigations considered, allowing the program to continue operating.
% TRANSFER_FUNCTION: Moves de facto launch-stop authority from engineering (which under the sibling absolute-threshold reading would hold a hard veto) to program management, in exchange for a paper record of risk acknowledgment; the cost of an unresolved technical risk shifts from being borne by the schedule (a stand-down) to being borne by whoever is exposed if the risk materializes (crew, and secondarily the engineers whose warnings were overridden).
% ABSENT_VOICES: Frontline engineers who raised specific, dated technical objections (the O-ring erosion history, cold-weather seal behavior) had their concerns entered into the documentation process but had no mechanism to convert 'documented objection' into 'stop.' Astronaut crews are entirely absent from the compliance determination despite bearing its full consequence.
% DISAPPEARANCE_RATIONALE: If this compliance-narrative reading were replaced by a hard engineering threshold, numerous flights that proceeded under 'documented and mitigated' risk records would instead have been grounded pending redesign; program cadence, contractor revenue timing, and the entire launch-decision org chart (who has stop authority) would restructure substantially.
% FOUNDING_PROBLEM: Complex, first-of-kind engineering systems generate continuous streams of technical anomalies that can never be fully resolved before flight; some decision procedure is needed to distinguish anomalies that must ground the program from anomalies that can be flown with, since a zero-tolerance standard would make flight operations impossible.
% FOUNDING_PROBLEM_CORROBORATION: Program management and its sponsors attest the compliance process remains necessary and functioning as designed. The Rogers Commission itself, and later the Columbia Accident Investigation Board, attest from outside the benefiting management chain that the same 'documented and accepted' process was repeatedly used to normalize deviance — treating prior survived anomalies as evidence of acceptable risk rather than as warnings — which is corroboration that the founding problem persists but the compliance-narrative solution to it had substantially failed by the time of both accidents.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that the compliance-narrative reading channels a genuine coordination problem (deciding when to fly under irreducible uncertainty) through a structure that systematically favors program continuity over the engineering objections it formally consults but does not bind. Theater ratio is high and rising (0.30 to 0.61) because as the process matured across shuttle flights, documentation volume grew substantially while the substantive gating power of that documentation over launch decisions did not — precisely the 'normalization of deviance' pattern the Rogers Commission and later CAIB both identified. Suppression (0.58) is moderate: engineering objections are not silenced outright, they are procedurally absorbed and outvoted, which is a softer but real suppression mechanism. Accessibility collapse is moderate (0.50) — engineers could escalate outside the chain in principle, but doing so carried severe career cost, so the alternative was not fully closed but was heavily discouraged.
 *
 * PERSPECTIVAL GAP:
 *   From program management's seat, this looks like reasonable, responsible administration of a coordination problem no absolute standard could solve. From the engineering veto authority's and frontline engineers' seats, the same structure looks like extraction of their technical authority into a documentation exercise that management could route around. The engine should register this seat divergence: management (agenda_setter/beneficiary) computes closer to rope/tangled_rope-as-coordination; the payer seats compute the extraction component.
 *
 * DIRECTIONALITY LOGIC:
 *   Program management and launch-schedule stakeholders sit near the beneficiary end: they retain decision authority and capture program continuity benefits without bearing the downside if a documented-but-unresolved risk materializes. Engineering veto authority, frontline engineers, and astronaut crews sit near the target end: their technical judgment and physical safety are the resource being spent to keep the program moving, and their exit options (constrained to trapped) reflect that escalating beyond documentation carried real professional or, for crews, mortal risk with no formal recourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deciding when a complex system with continuous anomalies is safe enough to fly) remains genuinely live — no engineering system can achieve zero anomalies. But this reading's specific SOLUTION (documented mitigation sufficiency as management's proceed criterion) is what both post-hoc bodies found had drifted from a genuine judgment procedure into a rationalization vehicle: prior survived flights with the same anomaly were repeatedly cited as evidence the anomaly was 'mitigated' rather than as escalating warnings. That is the mandatrophy signature — the mandate (safe decision-making under uncertainty) persists as live, but this particular embodiment of it had, by the accidents, substantially inverted into justifying continuation rather than gating it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'The Rogers Commission findings do not themselves specify which of three structurally distinct decision procedures they mandate: a compliance-narrative process (this reading), an absolute engineering threshold, or a quantified actuarial acceptance standard. Which reading NASA''s actual post-Rogers institutional practice adopted, and whether that adoption was itself a form of regulatory capture by program management, is not settled by the findings'' text alone.',
    'Comparative institutional analysis of NASA''s actual Flight Readiness Review procedures pre- and post-Challenger, cross-referenced against CAIB''s independent findings on Columbia, would show which reading actually governed practice at each point and whether management''s compliance-narrative reading displaced a more binding engineering standard over time.',
    'If institutional practice shows genuine progressive displacement of engineering veto power by management''s documentation standard, this reading''s extraction score should be treated as a lower bound; if practice shows the compliance process operated alongside a real, occasionally-exercised engineering stop authority, the extraction score here may overstate the reading''s actual operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of three structurally distinct readings of the Rogers findings actually governed NASA practice, and when.').

omega_variable(
    documentation_sufficiency_standard_drift,
    'Was the standard for ''sufficient'' documented risk awareness fixed at the time of the Rogers Commission, or did it drift downward across subsequent flights as prior undamaged flights were cited as evidence that lower documentation thresholds were adequate?',
    'Longitudinal comparison of Flight Readiness Review documentation rigor and content across the shuttle program''s operational history, checking whether formal documentation requirements loosened, tightened, or stayed constant while theater_ratio (performative vs. substantive review activity) increased.',
    'If the sufficiency bar itself drifted downward, this constraint should be understood as having undergone Goodhart-style substitution — the proxy (documentation exists) replacing the goal (risk is actually reduced) — which would support classifying later-period operation of this reading as closer to snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_sufficiency_standard_drift, empirical, 'Whether the compliance documentation standard itself degraded over the program''s operational history.').

omega_variable(
    management_good_faith_vs_capture,
    'Is program management''s use of the documented-mitigation standard a good-faith attempt to operationalize an intractable judgment problem, or a captured process that management (as beneficiary of continued launch cadence) shaped to systematically favor proceeding?',
    'Internal communications discovery (as performed by both the Rogers Commission and CAIB) showing whether management actively sought out or suppressed dissenting technical opinion, and whether documentation requirements were applied symmetrically to go/no-go evidence.',
    'Good faith would support treating this as a genuine, if imperfect, coordination mechanism (tangled_rope with real coordination function); capture would support reclassification toward snare, since the coordination story would be functioning primarily as cover for schedule-driven extraction of engineering authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(management_good_faith_vs_capture, conceptual, 'Whether the compliance process was administered in good faith or functioned as a captured extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.3).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__management_compliance_narrative, theater_ratio, 4, 0.38).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__management_compliance_narrative, theater_ratio, 8, 0.45).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__management_compliance_narrative, theater_ratio, 12, 0.51).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__management_compliance_narrative, theater_ratio, 16, 0.57).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(roge_su_t16, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'the Rogers Commission findings.' Each sibling reading has its own ε and classification: management_compliance_narrative (this story, tangled_rope, moderate-high extraction) authors a documentation-sufficiency standard that retains management launch authority; engineering_absolute_threshold authors a hard technical gate with no management discretion (expected lower extraction, closer to rope/mountain-adjacent given its coordination purity); actuarial_risk_acceptance authors a formal probability-quantification-and-informed-acceptance standard (expected intermediate extraction, contingent on who counts as 'informed'). Do not average these into one ε — each is a structurally distinct claim the Rogers findings' natural-language label conflates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
