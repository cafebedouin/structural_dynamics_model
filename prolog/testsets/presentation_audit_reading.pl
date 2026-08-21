% ============================================================================
% CONSTRAINT STORY: presentation_audit_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_presentation_audit_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: presentation_audit_reading
 *   human_readable: Presentation-Label Honesty Check (Evidence/Inference/Judgment Tagging)
 *   domain: epistemology_of_evaluation/ai_agent_architecture/research_methodology
 *
 * SUMMARY:
 *   This story instantiates one reading of the
 *   'blindness_decomposition_kernel' — the family of possible answers to the
 *   question 'what must a reviewer be deprived of, or distinct from, in order
 *   to check a presentation honestly?' This reading answers: nothing in
 *   particular need be withheld from the reviewer (not substrate, not
 *   history); what matters structurally is that the checking seat is NOT the
 *   same seat that assembled the presentation. The function under audit is
 *   narrow and largely orthogonal to the cold-reader framing (which turns on
 *   withheld history/substrate) and to the frame-independence framing (which
 *   turns on the claim surviving reframing) — this reading isolates the
 *   transmission-hygiene failure mode where a claim's epistemic status
 *   (evidence, inference, or judgment) gets silently upgraded or downgraded
 *   as it moves from source to final presentation, and asks only whether a
 *   distinct seat checks the label against the underlying warrant.
 *
 * KEY AGENTS:
 *   - presentation_assembler: sets labels, moderate power, constrained exit — bears no automatic cost for mislabeling absent audit
 *   - presentation_auditor: distinct seat, checks label against actual warrant — the entire function depends on this seat's non-identity with the assembler, not on any deprivation
 *   - decision_makers_downstream: receives the labeled presentation and allocates trust/scrutiny based on the labels
 *   - presentation_assemblers_who_conflate_categories: bear reputational/output cost when caught mislabeling by the distinct auditor seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(presentation_audit_reading, 0.12).
domain_priors:suppression_score(presentation_audit_reading, 0.18).
domain_priors:theater_ratio(presentation_audit_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(presentation_audit_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(presentation_audit_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(presentation_audit_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(presentation_audit_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(presentation_audit_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(presentation_audit_reading, rope).
narrative_ontology:human_readable(presentation_audit_reading, "Presentation-Label Honesty Check (Evidence/Inference/Judgment Tagging)").
narrative_ontology:topic_domain(presentation_audit_reading, "epistemology_of_evaluation/ai_agent_architecture/research_methodology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(presentation_audit_reading, 'af44a13a-0606-40dc-8087-a554f02ba3bf').
narrative_ontology:cs_kernel_codification('af44a13a-0606-40dc-8087-a554f02ba3bf', distributed).
narrative_ontology:cs_authority_grounding('af44a13a-0606-40dc-8087-a554f02ba3bf', practice).
narrative_ontology:cs_interpretation_layer_present('af44a13a-0606-40dc-8087-a554f02ba3bf').
narrative_ontology:cs_reading_relation('af44a13a-0606-40dc-8087-a554f02ba3bf', presentation_audit_reading__cold_reader_reading, coexists_with).
narrative_ontology:cs_reading_relation('af44a13a-0606-40dc-8087-a554f02ba3bf', presentation_audit_reading__frame_independence_reading, coexists_with).
narrative_ontology:cs_axiom('af44a13a-0606-40dc-8087-a554f02ba3bf', foundational, checker_non_identity_is_sufficient).
narrative_ontology:cs_axiom_status(checker_non_identity_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('af44a13a-0606-40dc-8087-a554f02ba3bf', checker_non_identity_is_sufficient, conventional).
narrative_ontology:cs_axiom('af44a13a-0606-40dc-8087-a554f02ba3bf', secondary, substrate_and_framing_knowledge_are_immaterial_to_the_check).
narrative_ontology:cs_axiom_status(substrate_and_framing_knowledge_are_immaterial_to_the_check, holdable).
narrative_ontology:cs_axiom_grounding('af44a13a-0606-40dc-8087-a554f02ba3bf', substrate_and_framing_knowledge_are_immaterial_to_the_check, empirically_contingent).
narrative_ontology:cs_reference_frame('af44a13a-0606-40dc-8087-a554f02ba3bf', self_audit_as_default_practice).
narrative_ontology:cs_drift_state('af44a13a-0606-40dc-8087-a554f02ba3bf', contemporary_multi_agent_evaluation_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af44a13a-0606-40dc-8087-a554f02ba3bf', '').
narrative_ontology:cs_kernel_id(presentation_audit_reading, blindness_decomposition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(presentation_audit_reading, decision_makers_downstream).
narrative_ontology:constraint_beneficiary(presentation_audit_reading, research_integrity_of_the_field).
narrative_ontology:constraint_victim(presentation_audit_reading, presentation_assemblers_who_conflate_categories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the assembled presentation (a report, a briefing, a model output) and must decide what weight to give each claim. Benefits when evidence, inference, and judgment are visibly and honestly tagged, because that lets them allocate scrutiny correctly; is harmed silently when tags are mislabeled, because the mislabeling is invisible from their seat unless they run the audit themselves.
narrative_ontology:constraint_stakeholder(presentation_audit_reading, decision_makers_downstream, beneficiary,
    moderate, immediate, constrained, national).

% Compiles the claims into the final presentation — selects, orders, and labels them. Has full discretion over how a claim is dressed (as observed fact, as derived inference, or as a judgment call) and bears no automatic cost for mislabeling unless an auditor checks. The constraint asks only: is this agent identical to the auditor who will check the labels? If yes, the audit function collapses; if no, it can function.
narrative_ontology:constraint_stakeholder(presentation_audit_reading, presentation_assembler, agenda_setter,
    moderate, immediate, constrained, national).

% A distinct seat (a second reviewer, a separate agent, a different pass of the same system operating under a different role) that checks whether each claim's presented epistemic status (evidence/inference/judgment) matches its actual epistemic status. Its only structural requirement is non-identity with the assembler — it need not be blind to substrate or history, and may share both with the assembler, so long as it is not the same seat performing the same act of labeling.
narrative_ontology:constraint_stakeholder(presentation_audit_reading, presentation_auditor, observer,
    moderate, immediate, analytical, national).

% Assemblers who, whether through haste, motivated reasoning, or genuine confusion, present inference or judgment as if it were direct evidence. When a distinct auditor seat exists, this conflation gets caught and corrected — a cost to the assembler's reputation or output that would not be incurred if the assembler audited its own presentation.
narrative_ontology:constraint_stakeholder(presentation_audit_reading, presentation_assemblers_who_conflate_categories, payer,
    moderate, immediate, constrained, national).

% The abstract collective good of a research or evaluation ecosystem in which claims are traceable to their actual epistemic warrant. Not an actor itself; benefits structurally whenever the audit function operates, without collecting anything directly.
narrative_ontology:constraint_stakeholder(presentation_audit_reading, research_integrity_of_the_field, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(presentation_audit_reading, research_integrity_of_the_field).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(presentation_audit_reading, decision_makers_downstream).
narrative_ontology:fixing_cost_class(presentation_audit_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine and narrow problem: claims drift in epistemic status as they move from raw observation through inference to interpretive judgment on their way into a final presentation, and the assembler who did that drifting is poorly positioned to catch their own relabeling. A second, non-identical seat checking labels against actual warrant catches mislabeling that self-audit structurally cannot.
% TRANSFER_FUNCTION: Moves scrutiny cost from the downstream decider (who would otherwise have to reverse-engineer each claim's true epistemic status unaided) to the auditor seat, at the price of the assembler occasionally being caught and corrected when they mislabel.
% ABSENT_VOICES: The presentation's original data sources and the domain experts whose inferences got incorporated are not present to attest whether the final label (evidence vs. inference vs. judgment) matches what they intended; their voice would clarify true epistemic status but is rarely solicited once the presentation is assembled.
% DISAPPEARANCE_RATIONALE: If the non-identity requirement disappeared and assemblers were permitted to audit their own presentations, mislabeling would go undetected at a higher rate — not because assemblers are dishonest, but because they cannot occupy a genuinely external checking position relative to their own labeling choices. Decision-makers would lose a cheap proxy for calibrating trust and would have to either trust blindly or independently re-derive epistemic status themselves.
% FOUNDING_PROBLEM: Presentations assembled by a single party tend to launder inference and judgment into the rhetorical clothing of evidence, especially under time pressure or incentive to persuade; the founding problem is that self-labeling is not self-correcting.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by methodology critiques in evaluation science and by AI-agent architecture literature on the necessity of separating generation from verification passes (e.g. critique/generator separation patterns); this attestation comes from methodologists and system architects outside the set of assemblers whose work is subject to audit.
narrative_ontology:disappearance_verdict(presentation_audit_reading, world_rearranges).
narrative_ontology:founding_problem_status(presentation_audit_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(presentation_audit_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(presentation_audit_reading, 'none', 1).
narrative_ontology:epsilon_provenance(presentation_audit_reading, 0.12, 'claude-sonnet-5', 'blind_reviewer_jurisdiction_2026_20260820_211650', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(presentation_audit_reading_tests).
:- end_tests(presentation_audit_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because, absent capture, this is close to a pure verification function: it costs a second read but transfers no rents anywhere durable — it just prevents a cheap failure mode (self-labeling drift) from propagating. Suppression is low (0.18) because nothing forces adoption of a distinct-seat audit; presentations without one simply carry more undetected mislabeling risk, which is a quality defect rather than a coercively enforced arrangement. Theater ratio is modest and slowly rising (0.18 to 0.22) reflecting a realistic risk that 'audit' seats can become nominal (the same assembler wearing a different hat, checking a box) without genuine independence — this is the one drift worth tracking temporally, since it is the failure mode by which this reading could slide toward tangled_rope if the auditor seat becomes captured or merely ceremonial.
 *
 * DIRECTIONALITY LOGIC:
 *   Decision-makers downstream and the field's research integrity are beneficiaries with low d — they gain a cheap, reliable signal without bearing the audit's operating cost directly. The assembler occupies an agenda-setting seat with moderate exit; their directionality is near-symmetric because they both benefit from a functioning field (their work is more trusted if audited) and pay a real cost when caught mislabeling. Assemblers who actually conflate categories are the narrow victim group — they bear the correction cost the audit exists to impose, but this is a cost imposed on error, not on a class of people structurally, which keeps the ε low and the classification well short of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (self-labeling drift) remains fully live — there is no point at which claims stop needing to travel from evidence through inference to judgment, so the audit function does not become vestigial the way scaffolds or pitons do. The risk this reading names is not obsolescence but capture-by-collapse: the auditor seat ceasing to be genuinely distinct from the assembler (the theater_ratio drift modeled above). As long as non-identity is maintained, mandatrophy does not apply; if the auditor seat re-merges with the assembler seat in practice while retaining the label 'audit,' this reading would need to be re-scored as a piton (function atrophied, only the label persists).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_identity_sufficiency,
    'Is mere non-identity between assembler and auditor sufficient to catch mislabeling, or does the audit also require some minimal independence of incentive (not just of seat) to be reliable?',
    'Compare mislabeling detection rates across (a) distinct-seat-same-incentive audits (e.g. a colleague on the same project reviewing) versus (b) distinct-seat-distinct-incentive audits (e.g. an external reviewer with no stake in the presentation''s reception).',
    'If incentive independence matters as much as seat non-identity, this reading''s claim that ''nothing in particular need be withheld, just a different seat'' is incomplete — the kernel would need a fourth reading isolating incentive structure specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_identity_sufficiency, empirical, 'Whether seat non-identity alone suffices, or incentive-independence is a hidden second variable.').

omega_variable(
    kernel_reading_partition_completeness,
    'Do the three readings (cold_reader, frame_independence, presentation_audit) jointly exhaust the space of structurally distinct answers to ''what must a checking seat lack or differ in,'' or is there a fourth axis (e.g. temporal distance from the original claim) not captured by any of the three?',
    'Attempt to construct a checking failure mode that is caught by none of the three readings'' mechanisms (non-identity, substrate-blindness, frame-independence) to test for a residual case.',
    'If a residual failure mode exists, the kernel_id blindness_decomposition_kernel needs a fourth sibling reading; if not, this reading can be treated as closing the partition alongside its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_partition_completeness, conceptual, 'Whether the three-reading decomposition of the kernel is exhaustive.').

omega_variable(
    auditor_seat_capture_detection,
    'How would one detect, from outside, whether a nominally distinct auditor seat has quietly re-merged with the assembler seat (same person under a different hat, or a rubber-stamp second pass)?',
    'Track disagreement rate between assembler-proposed labels and auditor-confirmed labels over time; a disagreement rate trending toward zero without a corresponding improvement in assembler accuracy is evidence of capture.',
    'If capture is occurring, the theater_ratio is understated and the constraint is drifting toward piton (function atrophied, audit label retained as performance) rather than remaining a stable low-extraction rope-adjacent check.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(auditor_seat_capture_detection, empirical, 'Detecting quiet collapse of auditor non-identity into de facto self-audit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(presentation_audit_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, presentation_audit_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pres_tr_t2, presentation_audit_reading, theater_ratio, 2, 0.19).
narrative_ontology:measurement(pres_tr_t4, presentation_audit_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(pres_tr_t6, presentation_audit_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement(pres_tr_t8, presentation_audit_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(pres_tr_t10, presentation_audit_reading, theater_ratio, 10, 0.22).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, presentation_audit_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pres_be_t2, presentation_audit_reading, base_extractiveness, 2, 0.1).
narrative_ontology:measurement(pres_be_t4, presentation_audit_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement(pres_be_t6, presentation_audit_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement(pres_be_t8, presentation_audit_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(pres_be_t10, presentation_audit_reading, base_extractiveness, 10, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(presentation_audit_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(presentation_audit_reading, information_standard).
narrative_ontology:boltzmann_floor_override(presentation_audit_reading, 0.03).
narrative_ontology:affects_constraint(presentation_audit_reading, cold_reader_reading).
narrative_ontology:affects_constraint(presentation_audit_reading, frame_independence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the natural-language concept 'blindness as the operative variable for honest evaluation' (kernel_id: blindness_decomposition_kernel). cold_reader_reading holds that the checker's ignorance of substrate/history is the operative variable. frame_independence_reading holds that the claim's survival under reframing (a property of the claim, not the checker) is operative. presentation_audit_reading (this story) holds that neither is operative — only non-identity between assembler and auditor is structurally required, and substrate/framing knowledge may be freely present in the auditor. The three readings have different ε (this one is lowest, functioning near-mountain/rope), different beneficiary/victim structures, and are linked here rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
