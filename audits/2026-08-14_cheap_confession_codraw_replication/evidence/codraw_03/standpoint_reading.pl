% ============================================================================
% CONSTRAINT STORY: standpoint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_standpoint_reading, []).

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
 *   constraint_id: standpoint_reading
 *   human_readable: Standpoint Reading of Positional Disagreement (Corrective-Weighting Toward the Marginalized Report)
 *   domain: epistemology/institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the standpoint reading of a contested kernel:
 *   when a marginalized party and an institutionally advantaged party give
 *   conflicting accounts of how an arrangement actually operates, is the
 *   disagreement symmetric input to be pooled, or asymmetric testimony where
 *   one position has structural epistemic access the other lacks in
 *   principle? The standpoint reading holds the latter: the marginalized
 *   position (the parent navigating a school or agency, the frontline worker)
 *   sees features of the arrangement's real operation that the manager's
 *   vantage point is structurally built to filter out — not through
 *   individual bias but because the manager's data sources (aggregate
 *   metrics, self-report, formal channels) are positioned differently in the
 *   causal structure. The existing credibility-allocation regime, which
 *   treats manager and evaluator testimony as more reliable by default, is
 *   therefore not a neutral procedure but a tangled rope: it does perform
 *   real coordination (institutions need SOME way to adjudicate disputed
 *   operational claims) while simultaneously extracting credibility and
 *   remedy away from the parties best positioned to detect the arrangement's
 *   actual failures. ε is authored for the standing credibility-allocation
 *   arrangement as this reading sees it operating now — not for the
 *   corrective-weighting alternative the reading endorses.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(standpoint_reading, 0.68).
domain_priors:suppression_score(standpoint_reading, 0.71).
domain_priors:theater_ratio(standpoint_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(standpoint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(standpoint_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(standpoint_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(standpoint_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(standpoint_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(standpoint_reading, tangled_rope).
narrative_ontology:human_readable(standpoint_reading, "Standpoint Reading of Positional Disagreement (Corrective-Weighting Toward the Marginalized Report)").
narrative_ontology:topic_domain(standpoint_reading, "epistemology/institutional analysis").

domain_priors:requires_active_enforcement(standpoint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(standpoint_reading, '8325f925-da43-467b-828b-881165451a5d').
narrative_ontology:cs_kernel_codification('8325f925-da43-467b-828b-881165451a5d', distributed).
narrative_ontology:cs_authority_grounding('8325f925-da43-467b-828b-881165451a5d', distributed).
narrative_ontology:cs_reading_relation('8325f925-da43-467b-828b-881165451a5d', standpoint_reading__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8325f925-da43-467b-828b-881165451a5d', standpoint_reading__proceduralist_reading, influences).
narrative_ontology:cs_reading_relation('8325f925-da43-467b-828b-881165451a5d', standpoint_reading__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('8325f925-da43-467b-828b-881165451a5d', foundational, asymmetric_epistemic_access_by_position).
narrative_ontology:cs_axiom_status(asymmetric_epistemic_access_by_position, holdable).
narrative_ontology:cs_axiom_grounding('8325f925-da43-467b-828b-881165451a5d', asymmetric_epistemic_access_by_position, empirically_contingent).
narrative_ontology:cs_axiom('8325f925-da43-467b-828b-881165451a5d', foundational, credibility_deficit_warrants_corrective_weighting).
narrative_ontology:cs_axiom_status(credibility_deficit_warrants_corrective_weighting, holdable).
narrative_ontology:cs_axiom_grounding('8325f925-da43-467b-828b-881165451a5d', credibility_deficit_warrants_corrective_weighting, deontological).
narrative_ontology:cs_reference_frame('8325f925-da43-467b-828b-881165451a5d', positional_symmetry_default).
narrative_ontology:cs_drift_state('8325f925-da43-467b-828b-881165451a5d', post_fricker_testimonial_injustice_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8325f925-da43-467b-828b-881165451a5d', '').
narrative_ontology:cs_kernel_id(standpoint_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(standpoint_reading, institutional_manager).
narrative_ontology:constraint_beneficiary(standpoint_reading, credentialed_evaluators).
narrative_ontology:constraint_victim(standpoint_reading, structurally_marginalized_parent).
narrative_ontology:constraint_victim(standpoint_reading, frontline_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits inside the arrangement (a school, agency, or care system) in a position that exposes structural features of how the arrangement actually operates — which promises are kept, which rules bend for whom, what the paperwork conceals. Reports of this kind are routinely discounted as anecdotal, emotional, or self-interested by the credentialed staff who evaluate them. The parent cannot exit the arrangement without losing access to the service their child needs, and cannot make their report land without institutional cooperation they do not control.
narrative_ontology:constraint_stakeholder(standpoint_reading, structurally_marginalized_parent, payer,
    powerless, biographical, trapped, local).

% Occupy a position close to where the arrangement's costs land — they see the daily operation the manager's dashboards do not capture. Their testimony corroborates marginalized reports but is itself discounted in performance reviews and policy debates as biased or insufficiently 'objective' relative to management data.
narrative_ontology:constraint_stakeholder(standpoint_reading, frontline_workers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(standpoint_reading, frontline_workers, excluded).

% Sets policy, controls which reports are treated as data and which are treated as complaints, and is structurally positioned to not see certain features of the arrangement's operation — not through malice but because the manager's vantage point (aggregate metrics, formal channels, self-reported compliance) is built to filter out exactly the structural features the marginalized position sees directly. Benefits from the presumption that the manager's report and the parent's report are equally partial, which licenses treating manager assessments as the tiebreaker.
narrative_ontology:constraint_stakeholder(standpoint_reading, institutional_manager, agenda_setter,
    institutional, generational, arbitrage, regional).

% Professional assessors (auditors, licensed reviewers, expert witnesses) whose institutional credibility is treated as a baseline that lay testimony must clear a higher bar to match. Their professional standing is reinforced whenever a credibility contest resolves in favor of formal expertise over positional testimony, regardless of which report was structurally better-informed.
narrative_ontology:constraint_stakeholder(standpoint_reading, credentialed_evaluators, beneficiary,
    organized, generational, mobile, national).

% Analyze the pattern across cases: document that credibility deficit tracks social position rather than epistemic access, and argue for corrective weighting. They do not adjudicate any single case but supply the framework used to argue that the parent's report deserves more weight, not equal weight, in a dispute with the manager.
narrative_ontology:constraint_stakeholder(standpoint_reading, standpoint_theorists, observer,
    analytical, civilizational, analytical, global).

% Advocacy organizations or ombudspersons who could amplify marginalized testimony but are typically not parties to the original credibility contest — they enter only after a dispute has escalated, by which point the manager's framing has often already prevailed procedurally.
narrative_ontology:constraint_stakeholder(standpoint_reading, external_advocates, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(standpoint_reading, diffuse).
narrative_ontology:fixing_cost_class(standpoint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement resolves disputed factual claims about how an institution operates when parties disagree — some mechanism must decide whose report of 'what happened' becomes the operative record for decisions, remedies, or trust.
% TRANSFER_FUNCTION: Moves credibility, and with it decision-making weight and remedy, from the marginalized reporter to the credentialed or institutionally-positioned reporter whenever their accounts conflict — the parent's or worker's account of the arrangement's operation is discounted relative to the manager's or evaluator's, even though the standpoint claim holds the marginalized account to be structurally better-positioned to see the relevant facts.
% ABSENT_VOICES: The structurally marginalized parent and frontline workers are nominally present (they can file complaints, testify) but their testimony is processed through a credibility framework built by and calibrated to the institutional perspective; external advocates who might corroborate them typically arrive after the credibility contest has already been resolved procedurally in the manager's favor.
% DISAPPEARANCE_RATIONALE: If the presumption of positional symmetry (that manager and parent reports deserve equal a priori weight) were replaced overnight by corrective weighting toward the marginalized position, prior credibility findings would need re-examination, compliance and audit processes built on management self-report would lose their default authority, and a substantial number of disputes currently resolved in the institution's favor would be reopened.
% FOUNDING_PROBLEM: Institutions need a way to adjudicate disputed claims about their own operation when the people running them and the people subject to them disagree about what is actually happening.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of testimonial injustice (Fricker) and standpoint epistemologists, plus ombudsperson and inspector-general reports across multiple institutional domains, corroborate from outside the manager/evaluator seats that credibility allocation systematically tracks social position rather than epistemic access — this is not merely the marginalized parties' self-report.
narrative_ontology:disappearance_verdict(standpoint_reading, world_rearranges).
narrative_ontology:founding_problem_status(standpoint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(standpoint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(standpoint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(standpoint_reading, 0.68, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(standpoint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(standpoint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(standpoint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that credibility and remedy are actively moved away from marginalized reporters toward institutional reporters in live disputes, a directional transfer with real material consequences (denied services, dismissed complaints, delayed intervention). Suppression (0.71) is high because the mechanism that keeps this operating is not merely informal — professional credentialing, procedural precedence for management records, and formal evidentiary hierarchies actively suppress the standing of positional testimony. Theater ratio (0.42) captures that a meaningful share of institutional 'listening' processes (complaint boxes, satisfaction surveys, community engagement sessions) function as legitimating performance rather than genuinely reweighting credibility. Accessibility collapse (0.6) is moderate-high: once a credibility contest is procedurally resolved, alternative routes to correction (external appeal, litigation) exist but are costly and rare. Resistance (0.58) is substantial: parents, workers, and advocates do actively contest these credibility determinations, which is precisely why standpoint theory as a body of argument exists.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional manager and credentialed evaluators are beneficiaries: the presumption of positional symmetry (or of default institutional credibility) protects their accounts from being systematically outweighed, and this benefit is a structural feature of occupying the position, not a personal virtue. The marginalized parent and frontline workers are victims: their trapped/constrained exit options compound the credibility deficit, since they cannot simply leave the arrangement to escape a bad ruling on 'what happened,' and cannot easily manufacture the institutional standing that would let their account compete on equal footing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutions need a way to adjudicate disputed operational claims) remains live — this is not a vestigial constraint. But the specific mechanism currently in place (default deference to institutionally-positioned testimony) has drifted from serving that founding problem to serving the interests of those the mechanism happens to credit. Classifying this as tangled_rope rather than snare preserves the genuine coordination function while flagging the asymmetric extraction riding on it — collapsing it to pure snare would deny that SOME credibility-adjudication mechanism is genuinely needed; collapsing it to rope would deny the documented, patterned harm to marginalized reporters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_access_vs_motivated_reasoning,
    'Is the credibility deficit assigned to marginalized reports actually tracking a real asymmetry in epistemic access (they see what the manager cannot), or could some portion of the discounting reflect a legitimate concern about motivated reasoning or incomplete information on the marginalized side?',
    'Case-level triangulation: compare marginalized reports against independently verifiable records (financial audits, external inspections) across many disputes to establish base rates of accuracy for each report type under conflict.',
    'If marginalized reports are independently verified accurate at much higher rates than institutional reports in genuine disputes, the standpoint reading''s asymmetric-weighting claim is strongly supported. If accuracy rates are closer to parity, the case for corrective (as opposed to merely equal) weighting weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_access_vs_motivated_reasoning, empirical, 'Whether documented credibility deficit tracks genuine asymmetric epistemic access or partly reflects legitimate accuracy concerns.').

omega_variable(
    corrective_weighting_calibration,
    'How much corrective weight should the marginalized report receive — full deference, a thumb on the scale, or some formal weighting function — and who decides the calibration?',
    'This is inherently a policy/values question, not resolvable by data alone; different corrective-weighting regimes would produce different classification outcomes for the SAME underlying facts.',
    'A pure deference rule would look more like a rope (correcting a known bias); a rule that simply shifts which credentialed party wins would look more like a snare (extraction relocated, not eliminated). The calibration choice determines whether the standpoint reading''s proposed remedy itself avoids becoming a new asymmetric extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corrective_weighting_calibration, preference, 'The calibration of corrective weighting is a policy choice, not an empirical finding, and different calibrations yield different downstream constraint structures.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''positional disagreement as evidence'' better modeled as a single kernel with four readings (as done here), or does the standpoint reading actually presuppose a different, incompatible ontology of evidence (testimonial justice) that makes it a different kernel entirely rather than a sibling reading?',
    'Compare whether the readings can be stated as answers to the SAME question (''how should conflicting accounts be weighted?'') or whether the standpoint reading redefines the question itself (from ''weighting'' to ''correcting structural injustice in credibility allocation'').',
    'If the standpoint reading redefines the question rather than answering the shared one, it should be split into its own kernel rather than treated as coexisting with pragmatist/proceduralist/instrumentalist readings — this would change which relation type applies to each sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether standpoint theory is a sibling reading of the same kernel or constitutes a distinct kernel with its own ontology of evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(standpoint_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stan_tr_t0, standpoint_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stan_tr_t4, standpoint_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(stan_tr_t8, standpoint_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(stan_tr_t12, standpoint_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(stan_tr_t16, standpoint_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(stan_tr_t20, standpoint_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(stan_tr_t24, standpoint_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(stan_be_t0, standpoint_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stan_be_t4, standpoint_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(stan_be_t8, standpoint_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(stan_be_t12, standpoint_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(stan_be_t16, standpoint_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(stan_be_t20, standpoint_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(stan_be_t24, standpoint_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stan_su_t0, standpoint_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stan_su_t4, standpoint_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(stan_su_t8, standpoint_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(stan_su_t12, standpoint_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(stan_su_t16, standpoint_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(stan_su_t20, standpoint_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stan_su_t24, standpoint_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(standpoint_reading, identity_coordination).
narrative_ontology:affects_constraint(standpoint_reading, pragmatist_reading).
narrative_ontology:affects_constraint(standpoint_reading, proceduralist_reading).
narrative_ontology:affects_constraint(standpoint_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the kernel 'positional_disagreement_as_evidence.' Each reading authors its own ε, beneficiary/victim structure, and classification for the SAME underlying dispute-adjudication arrangement, read through a different normative lens. The standpoint reading is the only one that treats the disagreement as inherently asymmetric rather than as symmetric input (pragmatist), a procedural question (proceduralist), or a means-ends question (instrumentalist). Linked here so contamination/coupling analysis can trace how a shift in one reading's classification pressures the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
