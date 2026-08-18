% ============================================================================
% CONSTRAINT STORY: procedural_fairness_as_severity_laundering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procedural_fairness_as_severity_laundering, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: procedural_fairness_as_severity_laundering
 *   human_readable: Procedural Fairness as Severity Laundering
 *   domain: legal/folk-institutional
 *
 * SUMMARY:
 *   This constraint models a folk-legal ritual system whose adjudicating
 *   office defines 'fairness' exclusively as procedural consistency: the same
 *   charges, tests, and penalties are applied to every petitioner in every
 *   generation, without exception. Because the office treats this internal
 *   consistency as sufficient proof of justice, the actual severity of the
 *   terms — whether the penalty is proportionate to the harm, whether the
 *   underlying conditions that justified the original severity still hold —
 *   is never examined as a separate question. Symmetry of application
 *   substitutes for scrutiny of content, and the substitution has held for
 *   enough generations that the terms have never been reconsidered even once.
 *   This is downstream of verification_prohibition_as_self_defeating_trial (a
 *   sibling snare in which the trial's own rules forbid the kind of evidence
 *   that could exonerate a petitioner): the self-defeating verification
 *   structure supplies the raw material that
 *   procedural-fairness-as-content-laundering then legitimizes by insisting
 *   only that the (impossible) trial be run identically for everyone.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procedural_fairness_as_severity_laundering, 0.71).
domain_priors:suppression_score(procedural_fairness_as_severity_laundering, 0.62).
domain_priors:theater_ratio(procedural_fairness_as_severity_laundering, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procedural_fairness_as_severity_laundering, extractiveness, 0.71).
narrative_ontology:constraint_metric(procedural_fairness_as_severity_laundering, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(procedural_fairness_as_severity_laundering, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(procedural_fairness_as_severity_laundering, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(procedural_fairness_as_severity_laundering, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procedural_fairness_as_severity_laundering, tangled_rope).
narrative_ontology:human_readable(procedural_fairness_as_severity_laundering, "Procedural Fairness as Severity Laundering").
narrative_ontology:topic_domain(procedural_fairness_as_severity_laundering, "legal/folk-institutional").

domain_priors:requires_active_enforcement(procedural_fairness_as_severity_laundering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procedural_fairness_as_severity_laundering, hereditary_adjudicator_office).
narrative_ontology:constraint_victim(procedural_fairness_as_severity_laundering, petitioner_class_across_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(procedural_fairness_as_severity_laundering, current_generation_elders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the ritual trial according to a fixed formula passed down through the office's lineage: same charges, same tests, same penalties applied to every petitioner regardless of era. Defines fairness entirely as 'the same terms for all' and treats that internal consistency as the complete and sufficient proof of justice, so the actual harshness of the terms is never placed on the agenda for review. Collects fees, deference, and standing from administering a process no one can challenge on content, only on whether it was applied correctly this time.
narrative_ontology:constraint_stakeholder(procedural_fairness_as_severity_laundering, hereditary_adjudicator_office, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(procedural_fairness_as_severity_laundering, hereditary_adjudicator_office, beneficiary).

% Each generation of petitioners submits to a penalty structure inherited unchanged from a founding era whose material conditions, threats, and stakes no longer exist. A petitioner may contest whether the procedure was followed correctly, but has no forum in which to contest whether the terms themselves are proportionate, since 'proportionate' has been defined out of the fairness test entirely. Exit means refusing the ritual altogether, which triggers default judgment or social exile.
narrative_ontology:constraint_stakeholder(procedural_fairness_as_severity_laundering, petitioner_class_across_generations, payer,
    powerless, civilizational, trapped, regional).

% Serve as witnesses and co-adjudicators, drawing status and small stipends from officiating the ritual as it has always been performed. Benefit from the predictability and legitimacy the unchanged formula confers on their own authority, and have no incentive to reopen a settled formula that currently favors their standing.
narrative_ontology:constraint_stakeholder(procedural_fairness_as_severity_laundering, current_generation_elders, beneficiary,
    organized, generational, constrained, local).

% Neighboring communities' mediators or reform-minded jurists who would ask whether the penalty schedule is proportionate to modern harms, but are never invited into the proceeding because the office's fairness criterion (consistency of application) forecloses any question they would raise (proportionality of content).
narrative_ontology:constraint_stakeholder(procedural_fairness_as_severity_laundering, outside_arbiters, excluded,
    moderate, biographical, trapped, regional).

% Study the ritual system from outside, comparing its formula against other folk-legal traditions and noting that the term 'fair' has been operationally narrowed to mean 'identically applied' rather than 'proportionate to harm' — a substitution that survives scrutiny only because no internal actor has standing to ask the excluded question.
narrative_ontology:constraint_stakeholder(procedural_fairness_as_severity_laundering, comparative_ethnographers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(procedural_fairness_as_severity_laundering, hereditary_adjudicator_office).
narrative_ontology:fixing_cost_class(procedural_fairness_as_severity_laundering, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, non-arbitrary procedure so that petitioners across generations know in advance exactly what process and penalty structure will be applied to their case, preventing ad hoc or personally biased adjudication at the point of judgment.
% TRANSFER_FUNCTION: Moves compliance, fees, deference, and — where the inherited penalty schedule is disproportionate to contemporary harms — excess punitive burden from each generation of petitioners to the hereditary office and its allied elders, who collect legitimacy and standing from administering a formula immune to content review.
% ABSENT_VOICES: Outside arbiters and reform-minded community members who would ask whether the penalty terms themselves are proportionate are structurally never seated at the proceeding, because the office's own definition of 'fair' (consistency of application) contains no clause under which such a question could even be raised.
% DISAPPEARANCE_RATIONALE: If the ritual system vanished, petitioners would gain access (through whatever forum replaced it) to argue proportionality of penalty as a live question rather than only procedural correctness; the hereditary office would lose its principal source of standing and revenue, and the community would likely renegotiate penalty severity against contemporary norms for the first time in generations.
% FOUNDING_PROBLEM: An early community needed a non-arbitrary, tamper-resistant way to resolve disputes and assign penalties without each judgment being subject to the adjudicator's momentary bias or the parties' relative power — consistency was the solution to a real problem of arbitrary judgment.
% FOUNDING_PROBLEM_CORROBORATION: The hereditary office and allied elders attest the consistency requirement remains necessary to prevent bias. Comparative ethnographers and excluded outside arbiters, working from outside the beneficiary set, attest that the original bias-prevention problem has been solved by the fixed formula but that the formula has since ossified into a shield against proportionality review, a distinct and unaddressed problem the founding design never anticipated.
narrative_ontology:disappearance_verdict(procedural_fairness_as_severity_laundering, world_rearranges).
narrative_ontology:founding_problem_status(procedural_fairness_as_severity_laundering, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(procedural_fairness_as_severity_laundering, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(procedural_fairness_as_severity_laundering, 'none', 1).
narrative_ontology:epsilon_provenance(procedural_fairness_as_severity_laundering, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_fairness_as_severity_laundering_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(procedural_fairness_as_severity_laundering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(procedural_fairness_as_severity_laundering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored rising over the interval (0.38 → 0.71) because the founding-era penalty schedule increasingly diverges from the harms it is nominally calibrated to as social and material conditions change, while the schedule itself never moves — a widening gap between term-severity and term-justification that the office's own fairness criterion structurally cannot detect. Theater ratio rises in parallel (0.22 → 0.58) as an increasing share of the ritual's evident activity is the performance of consistency itself (invoking precedent, restating the unchanged formula) rather than any function connected to actual dispute resolution or harm-proportionate outcomes. Suppression rises more moderately (0.40 → 0.62), reflecting hardening social costs for refusing or contesting the ritual, but this is a raw structural property, not scaled by scope or power in the authoring — the engine applies those scalings separately.
 *
 * PERSPECTIVAL GAP:
 *   From the office's seat, the arrangement is genuine coordination: a non-arbitrary, bias-resistant procedure solving a real historical problem. From the petitioner class's seat, the identical structure is experienced as an inherited severity that can never be examined on its merits, because the only fairness test on offer (consistency) forecloses the proportionality question before it can be asked. The engine should compute these as different seat-level types from the same structural data; the divergence is the phenomenon under study, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary adjudicator office is the clear structural beneficiary: it collects deference, fees, and legitimacy from administering a formula that is immune, by its own definition of fairness, to substantive challenge — d sits near the beneficiary end. The petitioner class across generations bears the transfer with no available forum to contest content, and exit means default judgment or social exile — trapped, d sits near the full-target end. Current-generation elders occupy an intermediate beneficiary position: they gain standing from officiating a settled ritual and have no incentive to reopen it, even though they are not the primary architects of the formula.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing arbitrary, bias-driven judgment — was real and the consistency solution was a genuine coordination achievement in its origin era. But the mandate has not been re-examined since: the arrangement now persists by treating 'we still apply it consistently' as proof that it still solves the problem it was built for, when the problem it was built for (arbitrary bias) has been solved and a new problem (disproportionate, unreviewable severity) has emerged and gone unaddressed. Classifying this as tangled_rope rather than snare or mountain prevents two errors: mislabeling it pure extraction (it does still perform a real anti-bias coordination function) and mislabeling it natural/inevitable (the formula's immutability is an enforced choice, not a structural necessity — outside arbiters could review proportionality if seated, and are excluded specifically to prevent that).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consistency_vs_proportionality_conflation,
    'Is procedural consistency (same terms for everyone) genuinely sufficient for ''fairness,'' or does the office''s definition simply exclude proportionality by construction, laundering severity as a side effect of a definition chosen for other reasons?',
    'Comparative analysis against folk-legal systems that have separated the two questions (consistency review and periodic proportionality review as distinct processes) to see whether proportionality review can be added without destroying the bias-resistance the consistency rule protects.',
    'If proportionality review is separable from consistency without reintroducing arbitrary bias, the current fusion is revealed as a choice that specifically protects the office''s unreviewable authority, strengthening the tangled_rope reading toward snare. If the functions are genuinely inseparable, part of the measured extraction is the unavoidable cost of bias-resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consistency_vs_proportionality_conflation, conceptual, 'Whether fairness-as-consistency structurally requires excluding proportionality review.').

omega_variable(
    generational_reconsideration_zero_frequency,
    'Is the zero-frequency of term reconsideration across generations a deliberate suppression mechanism, or an emergent property of a system with no institutional mechanism for revision at all (absence of a revision clause rather than active blocking of one)?',
    'Historical/ethnographic examination of whether revision was ever proposed and rejected (active suppression) versus never proposed at all (structural absence of a revision forum).',
    'Active rejection of proposed revisions would strengthen the snare/tangled_rope reading and raise measured suppression; a pure absence of a revision mechanism (never proposed, no gatekeeping event) would suggest the constraint is closer to inertial atrophy (piton-adjacent) than active extraction, and the office''s benefit would appear more incidental than deliberate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_reconsideration_zero_frequency, empirical, 'Whether zero reconsideration reflects active suppression or mere absence of a revision mechanism.').

omega_variable(
    downstream_coupling_with_verification_prohibition,
    'How much of this constraint''s measured extraction is inherited from the upstream verification_prohibition_as_self_defeating_trial constraint (the trial''s own rules foreclosing exculpatory evidence) versus generated independently by the fairness-as-consistency definition alone?',
    'Counterfactual analysis: would a version of this ritual system with normal (non-self-defeating) verification rules, but the same fairness-as-consistency definition, still show comparable extraction growth?',
    'If extraction growth persists even without the upstream verification-prohibition feeding it, the severity-laundering mechanism is independently extractive and should be weighted more heavily on its own account rather than treated as a downstream artifact of the sibling snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_coupling_with_verification_prohibition, empirical, 'How much of the measured extraction here is inherited from the upstream verification-prohibition constraint versus independently generated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procedural_fairness_as_severity_laundering, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_tr_t0, procedural_fairness_as_severity_laundering, theater_ratio, 0, 0.22).
narrative_ontology:measurement(proc_tr_t20, procedural_fairness_as_severity_laundering, theater_ratio, 20, 0.31).
narrative_ontology:measurement(proc_tr_t40, procedural_fairness_as_severity_laundering, theater_ratio, 40, 0.39).
narrative_ontology:measurement(proc_tr_t60, procedural_fairness_as_severity_laundering, theater_ratio, 60, 0.46).
narrative_ontology:measurement(proc_tr_t80, procedural_fairness_as_severity_laundering, theater_ratio, 80, 0.52).
narrative_ontology:measurement(proc_tr_t100, procedural_fairness_as_severity_laundering, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(proc_be_t0, procedural_fairness_as_severity_laundering, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(proc_be_t20, procedural_fairness_as_severity_laundering, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(proc_be_t40, procedural_fairness_as_severity_laundering, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(proc_be_t60, procedural_fairness_as_severity_laundering, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(proc_be_t80, procedural_fairness_as_severity_laundering, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(proc_be_t100, procedural_fairness_as_severity_laundering, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(proc_su_t0, procedural_fairness_as_severity_laundering, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(proc_su_t20, procedural_fairness_as_severity_laundering, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(proc_su_t40, procedural_fairness_as_severity_laundering, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(proc_su_t60, procedural_fairness_as_severity_laundering, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(proc_su_t80, procedural_fairness_as_severity_laundering, suppression_requirement, 80, 0.59).
narrative_ontology:measurement(proc_su_t100, procedural_fairness_as_severity_laundering, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procedural_fairness_as_severity_laundering, enforcement_mechanism).
narrative_ontology:affects_constraint(procedural_fairness_as_severity_laundering, verification_prohibition_as_self_defeating_trial).

% DUAL FORMULATION NOTE:
% This constraint is downstream of verification_prohibition_as_self_defeating_trial in the same folk-legal ritual family. The upstream story (snare) captures the trial's own evidentiary rules foreclosing the possibility of exoneration; this story (tangled_rope) captures the separate mechanism by which the office's definition of 'fairness' as pure procedural consistency exempts the severity of outcomes — including outcomes produced by the upstream foreclosure — from ever being questioned as a distinct matter. The upstream snare supplies harsher raw outcomes; this tangled_rope supplies the durable legitimating cover that keeps those outcomes from ever being renegotiated. Decomposed per the ε-invariance principle because 'is the trial fair' and 'is the trial's evidentiary structure self-defeating' are structurally distinct claims with different victim mechanisms and different ε trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
