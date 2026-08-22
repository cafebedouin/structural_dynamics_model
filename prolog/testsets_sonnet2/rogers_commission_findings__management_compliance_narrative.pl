% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Rogers Commission Compliance Narrative: Documented Risk Awareness Sufficient to Proceed
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested Rogers Commission
 *   kernel: the finding that flight readiness requires demonstrated,
 *   documented risk awareness and mitigation sufficient to proceed — not a
 *   resolved technical hazard, and not a quantified accepted failure
 *   probability. Under this reading, the compliance process is a
 *   management-controlled gate: engineering objections are absorbed into a
 *   documented record that satisfies procedural due diligence, while final
 *   launch authority remains with program management. Over repeated launch
 *   cycles this reading describes exactly the normalization-of-deviance
 *   dynamic the Rogers Commission itself identified: the same O-ring erosion
 *   finding was documented and 'accepted' launch after launch without ever
 *   being resolved, each cycle further entrenching the compliance-narrative
 *   as a substitute for engineering veto power rather than a complement to
 *   it. This is a distinct constraint from the sibling readings
 *   (engineering_absolute_threshold, actuarial_risk_acceptance) — it has its
 *   own ε, its own beneficiary/victim structure, and is linked to them only
 *   via network edges, not merged into a single measurement.
 *
 * KEY AGENTS:
 *   - program_management: agenda_setter (institutional/arbitrage) — administers the compliance gate and retains launch authority
 *   - launch_schedule_stakeholders: beneficiary (institutional/arbitrage) — benefits from continuity without bearing technical risk
 *   - contractor_management_layer: beneficiary/agenda_setter (organized/constrained) — overrides engineering under schedule pressure, documents rather than fixes
 *   - engineering_veto_authority: payer (moderate/trapped) — technical objection is procedurally logged but not binding
 *   - shuttle_crews: payer (powerless/trapped) — bears the physical consequence with no voice in the process
 *   - safety_review_engineers: payer/excluded (moderate/constrained) — dissent becomes due-diligence evidence, not a stop-work trigger
 *   - post_accident_investigators: observer (institutional/analytical) — examines the compliance record after failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.68).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.71).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Compliance Narrative: Documented Risk Awareness Sufficient to Proceed").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, 'e542dc1e-0e52-4cab-b7ad-8b426b9cead4').
narrative_ontology:cs_kernel_codification('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', formalized).
narrative_ontology:cs_authority_grounding('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', extraction).
narrative_ontology:cs_interpretation_layer_present('e542dc1e-0e52-4cab-b7ad-8b426b9cead4').
narrative_ontology:cs_reading_relation('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', foundational, documented_awareness_satisfies_diligence).
narrative_ontology:cs_axiom_status(documented_awareness_satisfies_diligence, holdable).
narrative_ontology:cs_axiom_grounding('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', documented_awareness_satisfies_diligence, conventional).
narrative_ontology:cs_axiom('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', foundational, management_retains_final_launch_authority_over_engineering_dissent).
narrative_ontology:cs_axiom_status(management_retains_final_launch_authority_over_engineering_dissent, holdable).
narrative_ontology:cs_axiom_grounding('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', management_retains_final_launch_authority_over_engineering_dissent, conventional).
narrative_ontology:cs_reference_frame('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', post_apollo_engineering_authority_norm).
narrative_ontology:cs_drift_state('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', pre_challenger_launch_cycle, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e542dc1e-0e52-4cab-b7ad-8b426b9cead4', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, launch_schedule_stakeholders).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, contractor_management_layer).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_veto_authority).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, shuttle_crews).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, safety_review_engineers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the go/no-go process and defines what counts as 'sufficient' documented risk awareness. Sets the paperwork threshold that converts an engineering objection into a documented-and-overridden concern rather than a blocking veto. Bears schedule and budget pressure from above and passes launch authority decisions downward as compliance sign-offs.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_management, agenda_setter,
    institutional, biographical, arbitrage, national).

% Political and budgetary actors (agency leadership, congressional appropriators, contractor executives) who benefit from launches proceeding on schedule. They do not personally bear technical risk; the documentation process gives them a defensible record that risk was 'considered' without requiring the underlying hazard to be resolved.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, launch_schedule_stakeholders, beneficiary,
    institutional, biographical, arbitrage, national).

% Contractor-side management (e.g., Thiokol management above the engineering floor) that reverses or overrides engineering recommendations under schedule pressure, then documents the reversal as a risk-acceptance decision. Retains the contract relationship and reputational standing by producing a paper trail rather than a fix.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, contractor_management_layer, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, contractor_management_layer, agenda_setter).

% Engineers who identified the O-ring erosion hazard and argued for a launch hold. Under this reading, their technical objection is procedurally satisfied — and effectively neutralized — once it is logged as a 'considered and mitigated' risk rather than acted upon as a stop-work condition. Their only recourse is escalation through the same management chain that overrode them; there is no independent channel that binds the launch decision to their assessment.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_veto_authority, payer,
    moderate, immediate, trapped, national).

% Bear the physical consequence of any gap between documented risk awareness and actual risk mitigation. Have no visibility into, or authority over, the compliance paperwork that stands between an identified hazard and a launch decision.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, shuttle_crews, payer,
    powerless, immediate, trapped, national).

% Independent safety reviewers whose findings are folded into the compliance record but whose authority to actually halt a launch is subordinate to program management's sign-off. Their technical dissent becomes evidence of due diligence rather than a binding constraint.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, safety_review_engineers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, safety_review_engineers, excluded).

% The Rogers Commission itself and subsequent oversight bodies, who examine the compliance record after failure to determine whether documented risk awareness substituted for actual risk resolution.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, post_accident_investigators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, contractor_management_layer).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured mechanism for program management to weigh competing engineering, schedule, and budget inputs and reach a documented, defensible launch decision under uncertainty, rather than freezing all operations pending zero-risk certification.
% TRANSFER_FUNCTION: Moves authority to accept residual technical risk from the engineers who identified the hazard to the management layer that owns schedule and budget outcomes, in exchange for a paper record of 'risk awareness' that substitutes for hazard resolution.
% ABSENT_VOICES: The engineers whose specific launch-hold recommendation was overruled (e.g., Thiokol engineering staff on the eve of the Challenger launch) are documented as consulted but are not present in the final decision room; astronaut crews have no seat in the compliance process at all despite bearing its entire physical consequence.
% DISAPPEARANCE_RATIONALE: If this compliance-narrative process vanished, program management would lose its documented-rationale shield for proceeding over unresolved engineering objections; either launch decisions would require binding engineering sign-off (collapsing toward the engineering_absolute_threshold reading) or would require explicit quantified risk acceptance by a named accountable decision-maker (collapsing toward the actuarial_risk_acceptance reading). Either shift would materially change who can authorize flight over a known hazard.
% FOUNDING_PROBLEM: Complex systems engineering under schedule and budget pressure generates continuous low-level technical dissent; some mechanism is needed to let programs proceed without every unresolved engineering concern becoming an automatic stop-work order, while still creating a record that concerns were raised and considered.
% FOUNDING_PROBLEM_CORROBORATION: Program management and contractor executives attest the process functioned as intended — documented, considered risk acceptance is a legitimate management prerogative. The Rogers Commission itself, examining the record from outside the benefiting management chain, found that the process had been used to normalize deviance: repeated O-ring erosion was documented as 'acceptable risk' launch after launch without the underlying hazard ever being resolved, corroborating the reading that the compliance narrative substituted for engineering veto power rather than complementing it.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.35 to 0.68 across the measured interval because each successive launch cycle converts an unresolved hazard into a further-normalized 'documented and accepted' precedent — the paperwork accretes while the underlying engineering risk does not close. Theater ratio climbs in parallel (0.30 to 0.58) because an increasing share of the compliance activity is retrospective justification rather than forward-looking mitigation. Suppression rises (0.40 to 0.71) as the compliance narrative becomes institutionally harder to challenge with each cycle it 'worked' (i.e., did not produce a visible failure) — engineering objections become easier to overrule the more precedent exists for overruling them. All three series share one time grid (T=0,2,4,6,8,10).
 *
 * PERSPECTIVAL GAP:
 *   From program management's seat, the compliance process reads as legitimate coordination — a structured way to weigh competing inputs and proceed rather than stall indefinitely. From engineering's seat, the same structure reads as extraction of veto power: their technical judgment is procedurally captured (logged, considered, documented) but strategically decoupled from the launch decision itself. The engine computes these as different seat-classifications from the same structural data; this divergence is the analytical point, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Program management and the launch-schedule/contractor beneficiaries sit near the beneficiary end of directionality: they control what counts as 'sufficient' documentation and bear no direct physical consequence of residual risk, while capturing the benefit of continued program operation. Engineering veto authority and safety review engineers sit near the target end: their technical assessments are absorbed into the record as evidence of process compliance without controlling the outcome — their exit options are trapped/constrained because escalation routes back through the same management hierarchy that overruled them. Shuttle crews are the most extreme target: powerless, trapped, and bearing the full physical cost of any gap between the documented awareness and actual mitigation, with zero structural voice in the compliance process itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding blanket stop-work for every unresolved engineering concern) may have been genuinely live early in the program, but by the measured end of the interval the founding_problem_status is contested: the compliance process no longer discriminates between minor, resolvable concerns and a specific, repeatedly-flagged, unresolved hazard (O-ring erosion in cold weather). Its mandate — 'demonstrate documented awareness' — has drifted from a coordination tool into a mechanism that forecloses the specific veto the founding problem was never meant to neutralize. This is the divergence the classification exists to surface: claimed_type is tangled_rope (a real coordination function plus a real, asymmetric extraction from engineering authority and crew safety), and the metrics substantiate rather than contradict that claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_vs_resolution_substitution,
    'Does ''documented risk awareness and mitigation efforts'' in this reading actually require the mitigation to reduce the hazard, or only require that mitigation efforts be recorded as having been considered?',
    'Trace specific launch decision records (e.g., pre-Challenger teleconference minutes) to determine whether documented ''mitigation'' corresponded to any change in physical risk, or only to a paper record of discussion and management sign-off.',
    'If documentation substituted for resolution, this reading''s compliance process is a coordination shell around pure extraction of engineering veto authority; if documentation typically tracked genuine risk reduction, the coordination function is more substantial and the classification would move toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(documentation_vs_resolution_substitution, empirical, 'Whether documented awareness functioned as a proxy for genuine hazard mitigation.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the Rogers Commission''s own language locate the operative standard — closer to a compliance-process standard (this reading), an absolute engineering threshold, or a quantified actuarial threshold?',
    'Close textual analysis of the Rogers Commission report''s findings and recommendations sections, cross-referenced with NASA''s contemporaneous internal launch-commit-criteria documents, to determine which reading the historical actors themselves understood as binding.',
    'If the historical record supports multiple readings operating simultaneously (as different actors invoked different standards at different points), that corroborates treating these as genuinely distinct, coexisting constraints rather than competing interpretations of one constraint — validating the decomposition into three separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locating where the compliance-narrative reading''s boundary sits relative to its sibling readings within the historical record.').

omega_variable(
    management_capture_vs_genuine_judgment_call,
    'Is program management''s authority to accept documented risk a genuine, defensible organizational judgment function, or is it structurally captured by schedule and budget pressures that make risk-acceptance a foregone conclusion regardless of the documentation''s content?',
    'Compare rate of launch-hold decisions versus launch-proceed decisions across all documented risk-acceptance events in the program''s history; a near-zero hold rate despite recurring documented hazards would indicate capture.',
    'If captured, program_management''s beneficiary/agenda_setter role is better modeled as coupled to launch_schedule_stakeholders'' interests than as an independent risk-weighing function, strengthening the tangled_rope classification toward snare-like asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_capture_vs_genuine_judgment_call, empirical, 'Whether management''s risk-acceptance authority is independently exercised or captured by schedule pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.3).
narrative_ontology:measurement(roge_tr_t2, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2, 0.38).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__management_compliance_narrative, theater_ratio, 4, 0.45).
narrative_ontology:measurement(roge_tr_t6, rogers_commission_findings__management_compliance_narrative, theater_ratio, 6, 0.5).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__management_compliance_narrative, theater_ratio, 8, 0.55).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__management_compliance_narrative, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(roge_be_t2, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(roge_su_t2, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 10, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__management_compliance_narrative, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'Rogers Commission findings' concept, per the ε-invariance principle. engineering_absolute_threshold treats unresolved hazard as an automatic stop-work condition (near-zero tolerance, engineering-controlled veto); actuarial_risk_acceptance treats quantified, accepted failure probability as sufficient (informed-decision-maker-controlled, but requires actual quantification); this reading (management_compliance_narrative) requires only documented awareness and mitigation effort, with management retaining final authority. The three readings have different ε (extraction is highest here, where documentation can substitute for resolution; lowest under the engineering reading, where the hazard must actually close), different beneficiary/victim sets, and different claimed types. They are linked via network edges rather than merged into one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
