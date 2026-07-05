% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Rogers Commission Reading: Documented-Compliance Launch Authority
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the Rogers Commission
 *   findings — the management-compliance-narrative reading, under which the
 *   Commission's post-Challenger recommendations are read as establishing a
 *   documentation and process standard: management may proceed with launch as
 *   long as it can demonstrate documented awareness of risk and a record of
 *   mitigation effort, even where a specific engineering objection has not
 *   been technically resolved. This is distinct from a sibling reading
 *   (engineering_absolute_threshold) that treats Rogers as establishing a
 *   hard technical stop-work boundary, and another sibling
 *   (actuarial_risk_acceptance) that treats it as requiring formal
 *   quantified-probability sign-off. Each reading has a different ε: this one
 *   is a moderate, actively-enforced constraint with a real coordination
 *   function (large programs need a repeatable way to move decisions forward)
 *   paired with asymmetric extraction (engineering veto power is diminished;
 *   program continuity is protected) — a tangled rope, not a mountain or a
 *   clean rope. The other readings are separate constraint files with their
 *   own ε.
 *
 * KEY AGENTS:
 *   - program_management: agenda_setter (institutional/constrained) — administers the compliance process and retains launch authority
 *   - schedule_dependent_contractors: beneficiary (organized/constrained) — protected from launch delay by documentation-sufficiency standard
 *   - engineering_veto_authority: payer (moderate/trapped) — technical objection reduced to a logged input rather than a binding stop
 *   - flight_crews: payer (powerless/trapped) — bear the physical consequence with no visibility or exit
 *   - oversight_bodies: observer (institutional/analytical) — reviews after the fact
 *   - regulatory_body_administering_process: agenda_setter/observer (institutional/analytical) — codifies and audits the documentation standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.62).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.58).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Reading: Documented-Compliance Launch Authority").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '0691de38-bce2-4445-926c-52fe7191b1d8').
narrative_ontology:cs_kernel_codification('0691de38-bce2-4445-926c-52fe7191b1d8', formalized).
narrative_ontology:cs_authority_grounding('0691de38-bce2-4445-926c-52fe7191b1d8', extraction).
narrative_ontology:cs_interpretation_layer_present('0691de38-bce2-4445-926c-52fe7191b1d8').
narrative_ontology:cs_reading_relation('0691de38-bce2-4445-926c-52fe7191b1d8', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('0691de38-bce2-4445-926c-52fe7191b1d8', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('0691de38-bce2-4445-926c-52fe7191b1d8', foundational, documented_awareness_satisfies_due_diligence).
narrative_ontology:cs_axiom_status(documented_awareness_satisfies_due_diligence, holdable).
narrative_ontology:cs_axiom_grounding('0691de38-bce2-4445-926c-52fe7191b1d8', documented_awareness_satisfies_due_diligence, conventional).
narrative_ontology:cs_axiom('0691de38-bce2-4445-926c-52fe7191b1d8', foundational, management_retains_launch_authority_absent_resolved_veto).
narrative_ontology:cs_axiom_status(management_retains_launch_authority_absent_resolved_veto, holdable).
narrative_ontology:cs_axiom_grounding('0691de38-bce2-4445-926c-52fe7191b1d8', management_retains_launch_authority_absent_resolved_veto, instrumental).
narrative_ontology:cs_reference_frame('0691de38-bce2-4445-926c-52fe7191b1d8', pre_rogers_ad_hoc_launch_determination).
narrative_ontology:cs_drift_state('0691de38-bce2-4445-926c-52fe7191b1d8', post_columbia_caib_review, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0691de38-bce2-4445-926c-52fe7191b1d8', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, schedule_dependent_contractors).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_veto_authority).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, flight_crews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the compliance process: requires that risk be documented and mitigation efforts recorded, then makes the launch determination itself. Retains launch authority as long as a paper record of risk awareness exists, regardless of whether the underlying engineering objection was resolved. Answers to schedule pressure from above and reports compliance, not resolution, upward.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_management, agenda_setter,
    institutional, biographical, constrained, national).

% Depend on maintaining the launch cadence for contract renewal and program funding. The documented-compliance process lets launches proceed without requiring engineering sign-off to be resolved in their favor, which protects their revenue stream and reputational standing with the funding agency.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, schedule_dependent_contractors, beneficiary,
    organized, biographical, constrained, national).

% Under this reading, engineering objection is reduced to an input that must be documented and considered, not a binding stop. Engineers who raise the O-ring concern find their objection absorbed into a paper trail rather than acted upon as a hard constraint; their only recourse is to escalate through a chain that itself set the compliance bar, or to refuse to sign off and be overridden anyway.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_veto_authority, payer,
    moderate, immediate, trapped, national).

% Bear the physical consequence of a launch decision made through a documentation standard rather than a resolved technical threshold. They have no visibility into whether 'sufficient mitigation' means the risk was reduced or merely that a memo was filed, and no exit once assigned to a mission.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, flight_crews, payer,
    powerless, immediate, trapped, national).

% Review the compliance record after the fact — commissions, inspectors general, congressional committees. They can see whether the documentation process substituted for genuine risk resolution, but their findings arrive after the launch decision, not before it.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, oversight_bodies, observer,
    institutional, generational, analytical, national).

% Codifies what counts as 'sufficient documented risk awareness and mitigation' after Rogers, and audits programs against that standard going forward. Has an institutional interest in the process being workable and repeatable, which favors documentation-completeness over case-by-case technical resolution.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, regulatory_body_administering_process, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, regulatory_body_administering_process, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives large, multi-contractor programs a repeatable, auditable way to move a launch decision forward when technical risk cannot be reduced to zero: instead of requiring every engineering objection to be resolved before proceeding, the process requires that risk be identified, documented, and weighed by an accountable decision-maker.
% TRANSFER_FUNCTION: Moves decision authority from the engineers who identify and characterize a specific technical risk to the management chain that owns schedule and budget outcomes, in exchange for a documentation record that substitutes for technical resolution.
% ABSENT_VOICES: The engineers whose specific objection (O-ring erosion at low temperature) is absorbed into the documentation record without being resolved have no seat with binding authority in this reading — their technical judgment is treated as an input to be logged, not a veto to be honored. Flight crews, who bear the ultimate consequence, are not present in the decision process at all.
% DISAPPEARANCE_RATIONALE: If this compliance-narrative reading of Rogers disappeared, launch decisions with documented but unresolved technical risk would either default to the engineering-threshold reading (halt until certified) or the actuarial reading (quantify and formally accept probability) — either way, program management would lose the ability to proceed on the basis of a documentation record alone, and schedule-dependent contractors would face materially different approval timelines.
% FOUNDING_PROBLEM: After the Challenger accident, the Rogers Commission found that known engineering concerns about O-ring performance in cold weather were raised but not acted upon, and that the decision to launch proceeded despite documented dissent. The compliance process was built so that some structured record of risk consideration would exist going forward, preventing decisions made with zero documented awareness of known risk.
% FOUNDING_PROBLEM_CORROBORATION: Program management and the administering regulatory body attest the process functions as intended, citing improved documentation rates in post-Rogers audits. Independent voices — the ASAP (Aerospace Safety Advisory Panel) in later NASA-era reports, and academic organizational-safety researchers (notably Vaughan's 'normalization of deviance' analysis) writing from outside the program management chain — attest that the documentation requirement was substantially satisfied on subsequent missions (including the Columbia loss) while the underlying technical risk remained unresolved, indicating the founding problem is only partially addressed by this reading and in some administrations was substituted for rather than solved.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) reflects that program continuity and contractor revenue are protected at the structural expense of a genuine technical veto and of the crews who bear physical risk — this is asymmetric, not merely a coordination cost. Suppression (0.58) is moderate: engineering dissent is not silenced outright but is procedurally absorbed, which is a real form of active suppression even without formal punishment. Theater ratio is high and rising (0.40 to 0.68 over the interval) because 'documented mitigation' becomes progressively easier to satisfy with paperwork than with resolved engineering fixes — this is the Goodhart drift the framework is built to detect, and it tracks directly onto the historical record (the same documentation standard was satisfied again before the Columbia loss). All three series share one time grid.
 *
 * PERSPECTIVAL GAP:
 *   From program management's seat, the compliance process looks like disciplined, responsible risk governance — a genuine improvement over pre-Rogers ad hoc decision-making. From engineering's seat, the same structure looks like a mechanism that lets a documented objection be overridden by administrative fiat. The engine should compute these as different seat-level classifications from the same structural facts; the divergence is exactly what a documentation-based compliance process produces when it separates 'awareness of risk' from 'resolution of risk.'
 *
 * DIRECTIONALITY LOGIC:
 *   Program management sits near the beneficiary end: it sets the compliance bar and retains the authority the process protects. Schedule-dependent contractors are secondary beneficiaries — the process protects their revenue continuity. Engineering veto authority is the structural target: its actual power (a binding stop) is replaced by a logged input, which is a real transfer of decision authority even though no engineer is directly 'paid' anything. Flight crews are the most extreme target: trapped, powerless, and bearing the full consequence with no representation in the process that decides on their behalf.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no documented risk awareness before Challenger) is only partially live: modern programs do produce risk documentation, so a naive reading would call the mandate discharged. But the deeper founding problem — technical objections being overridden by schedule pressure — recurred under this exact compliance structure in the run-up to Columbia, per the CAIB and outside organizational-safety scholarship (Vaughan). This is the classic mandatrophy trap: a process built to fix a substantive failure mode (overridden objections) can satisfy its own procedural criteria (documentation exists) while the substantive failure mode persists. Classifying this as tangled_rope rather than rope prevents mislabeling a persistent extraction of engineering authority as pure, benign coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_rogers,
    'Which of the three readings of the Rogers Commission findings (compliance-narrative, engineering-absolute-threshold, actuarial-risk-acceptance) is the historically correct interpretation of what the Commission actually mandated, versus what institutional practice subsequently adopted?',
    'Textual analysis of the Commission''s formal recommendations versus NASA''s subsequent implementing directives and Flight Readiness Review procedures; comparison against CAIB''s later finding on whether the post-Rogers process was faithfully implemented or drifted toward documentation-sufficiency.',
    'If the engineering-absolute-threshold reading is the textually correct one, this compliance-narrative reading is itself evidence of institutional drift away from the Commission''s actual mandate — which would make the tangled_rope classification here an understatement, since the ''coordination function'' it claims may not have been authorized by Rogers at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_rogers, conceptual, 'Which kernel reading Rogers actually authorized versus which one institutional practice adopted.').

omega_variable(
    documentation_versus_resolution_gap,
    'Is the gap between ''documented risk awareness'' and ''resolved technical risk'' inherent to any large-program compliance process, or specific to how NASA implemented this particular reading?',
    'Comparative study of other high-consequence engineering domains (nuclear regulatory licensing, aviation certification) that use documentation-based sign-off processes, checking whether they preserve a binding engineering veto that this reading lacks.',
    'If comparable domains preserve a binding veto while still using documentation, this reading''s specific weakening of engineering authority is a contingent, correctable design choice rather than an unavoidable feature of compliance processes generally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_versus_resolution_gap, empirical, 'Whether the documentation/resolution gap is inherent to compliance processes or specific to this implementation.').

omega_variable(
    beneficiary_naturalness_of_program_continuity,
    'Is protecting program continuity a legitimate institutional coordination good (keeping a national space program functioning) or is it primarily a mechanism for insulating management and contractors from the cost of unresolved technical risk?',
    'Track whether program-continuity decisions under this reading correlate with subsequent safety incidents versus with genuinely resolved engineering concerns; a pattern of incidents following continuity-protective decisions would support the extraction reading.',
    'If continuity-protection reliably preceded incidents (as in the Columbia case), the ''coordination function'' claimed for this tangled rope is substantially cover for extraction, which would push the classification toward snare on renewed evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalness_of_program_continuity, empirical, 'Whether program continuity is genuine coordination benefit or extraction dressed as coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.4).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__management_compliance_narrative, theater_ratio, 4, 0.48).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__management_compliance_narrative, theater_ratio, 8, 0.55).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__management_compliance_narrative, theater_ratio, 12, 0.6).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__management_compliance_narrative, theater_ratio, 16, 0.65).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 8, 0.5).
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
% This constraint is one of three sibling readings decomposed from the single natural-language label 'Rogers Commission findings.' The engineering_absolute_threshold reading treats the findings as a hard technical stop-work mandate (forecloses this reading's discretion). The actuarial_risk_acceptance reading treats the findings as requiring formal quantified-probability sign-off (coexists with this reading as a distinct evidentiary standard some later programs adopted alongside documentation practice). Each reading carries its own ε and stakeholder structure; they are linked here per the ε-invariance decomposition principle rather than merged into one constraint with a variable observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
