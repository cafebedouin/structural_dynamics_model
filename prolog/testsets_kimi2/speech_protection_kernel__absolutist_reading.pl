% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Speech Protection Doctrine
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the absolutist reading of the speech
 *   protection kernel: a constitutional doctrine holding that speech is
 *   protected near-categorically and that listener harm, standing alone, does
 *   not justify state restriction. The doctrine is anchored in a fixed
 *   constitutional text ('Congress shall make no law...') and enforced by a
 *   judiciary that interprets it as creating a default immunity for speakers.
 *   The constraint coordinates against government censorship but
 *   asymmetrically externalizes the costs of harmful speech onto targets who
 *   are denied state remedy. The constraint is one reading of a contested
 *   kernel; sibling readings include harm-threshold, dignity, marketplace,
 *   and democratic-participation framings.
 *
 * KEY AGENTS:
 *   - judiciary: agenda_setter (institutional/analytical) â interprets and enforces the absolutist doctrine via judicial review
 *   - dissident_speakers: beneficiary (powerless/constrained) â rely on broad immunity to criticize power
 *   - established_press: beneficiary (powerful/mobile) â publishes without liability for listener harm
 *   - targets_of_harmful_speech: payer (powerless/trapped) â bear harms the state is barred from redressing
 *   - state_legislators: payer (institutional/constrained) â barred from restricting speech outside narrow exclusions
 *   - free_speech_jurists: observer (analytical/analytical) â elaborate and defend the absolutist framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.65).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Speech Protection Doctrine").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'f9fce130-c366-40be-b784-a12c7db9b23f').
narrative_ontology:cs_kernel_codification('f9fce130-c366-40be-b784-a12c7db9b23f', fixed_text).
narrative_ontology:cs_authority_grounding('f9fce130-c366-40be-b784-a12c7db9b23f', lineage).
narrative_ontology:cs_interpretation_layer_present('f9fce130-c366-40be-b784-a12c7db9b23f').
narrative_ontology:cs_reading_relation('f9fce130-c366-40be-b784-a12c7db9b23f', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('f9fce130-c366-40be-b784-a12c7db9b23f', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9fce130-c366-40be-b784-a12c7db9b23f', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('f9fce130-c366-40be-b784-a12c7db9b23f', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('f9fce130-c366-40be-b784-a12c7db9b23f', foundational, listener_harm_irrelevant_to_restriction).
narrative_ontology:cs_axiom_status(listener_harm_irrelevant_to_restriction, holdable).
narrative_ontology:cs_axiom_grounding('f9fce130-c366-40be-b784-a12c7db9b23f', listener_harm_irrelevant_to_restriction, deontological).
narrative_ontology:cs_axiom('f9fce130-c366-40be-b784-a12c7db9b23f', foundational, constitutional_text_absolutism).
narrative_ontology:cs_axiom_status(constitutional_text_absolutism, holdable).
narrative_ontology:cs_axiom_grounding('f9fce130-c366-40be-b784-a12c7db9b23f', constitutional_text_absolutism, conventional).
narrative_ontology:cs_reference_frame('f9fce130-c366-40be-b784-a12c7db9b23f', speech_immunity_default).
narrative_ontology:cs_drift_state('f9fce130-c366-40be-b784-a12c7db9b23f', contemporary_harm_awareness_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f9fce130-c366-40be-b784-a12c7db9b23f', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, dissident_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, established_press).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_harmful_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, state_legislators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the absolutist speech doctrine through constitutional judicial review, striking down federal and state laws that restrict speech based on content or perceived harm. Maintains the categorical framework through precedent and originalist interpretation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Rely on broad speech immunity to criticize government, power, and majority norms without fear of state censorship or civil liability for listener harm.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, dissident_speakers, beneficiary,
    powerless, biographical, constrained, national).

% Uses the doctrine's breadth to publish controversial and investigative material without being held liable for emotional or reputational harm to subjects, even when publication causes foreseeable injury.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, established_press, beneficiary,
    powerful, generational, mobile, global).

% Bear psychological, reputational, and sometimes physical harms from speech that the absolutist doctrine prevents the state from restricting, leaving them without public-law remedy and often without practical private-law alternatives.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, biographical, trapped, local).

% Are constitutionally barred from enacting content-based speech restrictions or harm-responsive remedies that their constituents demand, even where the speech causes documented injury to vulnerable groups.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, state_legislators, payer,
    institutional, biographical, constrained, national).

% Analytical seat that elaborates, teaches, and litigates within the absolutist framework, producing the doctrinal arguments that sustain the constraint's categorical character.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, free_speech_jurists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of who may speak and what the state may suppress by removing government discretion over speech content, preventing tyrannical or majoritarian censorship.
% TRANSFER_FUNCTION: Transfers immunity from legal consequence and state redress from targets of harmful speech to speakers, externalizing the cost of expressive injury onto those who cannot obtain restriction or remedy.
% ABSENT_VOICES: Targets of systemic harassment and structural subordination who lack litigation resources are present in theory but absent in effective voice; foreign speakers and non-citizens are structurally excluded from the doctrine's protections and its interpretive conversation.
% DISAPPEARANCE_RATIONALE: If the absolutist protection vanished overnight, legislatures would enact content-based and harm-responsive restrictions, speakers would face liability for previously protected expression, courts would revert to balancing tests, and the legal-political order would reorganize around dignity and harm considerations rather than categorical immunity.
% FOUNDING_PROBLEM: Government suppression of dissenting political and religious speech by colonial and early republic authorities.
% FOUNDING_PROBLEM_CORROBORATION: Historians attest to early speech suppression. However, the absolutist reading's claim that this problem remains live and justifies near-categorical immunity is primarily asserted by judiciary and free speech jurists who benefit from the doctrine's breadth. Critical legal scholars and harmed communities contest the continued salience of the founding problem, arguing the absolutist solution now generates greater harms than the original problem. Corroboration from outside the beneficiary set is weak.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the doctrine systematically transfers the costs of harmful expression from speakers to targets by denying the latter state redress. Suppression (0.55) is moderate; the constraint requires active judicial enforcement to strike down democratically enacted laws, but enforcement is juridical rather than physical. Theater_ratio (0.35) is moderate; judicial opinions increasingly invoke originalist and formalist rhetoric to maintain a doctrine whose practical function is contested. Accessibility_collapse (0.80) is high; once the absolutist framework is accepted, alternative regulatory frameworks (harm-balancing, dignity-based) collapse as constitutionally prohibited. Resistance (0.60) is substantial; targets of harmful speech, civil rights movements, and some state actors actively contest the doctrine's breadth.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and free speech jurists experience the constraint as a necessary coordination mechanism against tyranny; from their seat, the doctrine is low-extraction liberty protection. Targets of harmful speech experience the same structure as an externalization of harm costs; from their seat, the constraint extracts by denying remedy. The engine computes this divergence from the power/exit asymmetry: beneficiaries have mobile or organized exit, while targets are trapped in the harm with no regulatory exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (dissident_speakers, established_press) have low directionality â the constraint subsidizes their expressive activity. Targets (targets_of_harmful_speech) have high directionality â the constraint extracts by withholding state protection. State legislators sit at moderate-high directionality because their policy discretion is captured by the doctrine, though they retain constitutional amendment exit at prohibitive cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading resists mandatrophy mislabeling because its coordination function (preventing government censorship) is genuine and historically grounded. However, the doctrine's persistence is not purely coordination: it actively suppresses alternative regulatory frameworks and externalizes costs. The classification as tangled_rope captures that the same structure that coordinates liberty also extracts from vulnerable targets. It is not a snare because the coordination story is not mere cover; it is not a rope because the asymmetric cost-bearing is structural, not incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_kernel_reading_status,
    'This constraint is the absolutist reading of the speech_protection_kernel; does its classification change if framed as a deontological natural right versus a positive constitutional convention?',
    'Comparative analysis across jurisdictions with and without textual absolutism to see if the same structural extraction persists.',
    'If the extraction is text-dependent, the constraint is a conventional CS structure; if it persists without the text, it may reflect a deeper coordination type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_kernel_reading_status, conceptual, 'Kernel reading framing ambiguity for absolutist speech doctrine').

omega_variable(
    harm_cost_extraction_nature,
    'Is the denial of state remedy to targets of harmful speech an extractive transfer to speakers, or a non-extractive boundary of state competence?',
    'Cross-reading comparison with harm_threshold_reading: measure whether the same speech acts are restricted in harm-threshold jurisdictions and whether targets there experience lower externalized costs.',
    'If targets in harm-threshold jurisdictions bear lower costs, the absolutist reading extracts by denying remedy; if costs are identical, the constraint is non-extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_cost_extraction_nature, empirical, 'Whether denied remedy constitutes extraction').

omega_variable(
    dignity_reading_foreclosure,
    'Does the absolutist reading''s core axiom logically foreclose the dignity reading, or can both operate within a single legal framework through doctrinal partitioning?',
    'Jurisprudential analysis of whether any court has successfully applied both premises simultaneously without contradiction.',
    'If foreclosure is real, the engine should register these readings as mutually exclusive; if partitioning is possible, they are competing elaborations of a single kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_reading_foreclosure, conceptual, 'Foreclosure status between absolutist and dignity readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_abs_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spk_abs_tr_t20, speech_protection_kernel__absolutist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(spk_abs_tr_t40, speech_protection_kernel__absolutist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(spk_abs_tr_t60, speech_protection_kernel__absolutist_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(spk_abs_tr_t80, speech_protection_kernel__absolutist_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement(spk_abs_tr_t100, speech_protection_kernel__absolutist_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(spk_abs_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spk_abs_be_t20, speech_protection_kernel__absolutist_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(spk_abs_be_t40, speech_protection_kernel__absolutist_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(spk_abs_be_t60, speech_protection_kernel__absolutist_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(spk_abs_be_t80, speech_protection_kernel__absolutist_reading, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(spk_abs_be_t100, speech_protection_kernel__absolutist_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spk_abs_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spk_abs_su_t20, speech_protection_kernel__absolutist_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(spk_abs_su_t40, speech_protection_kernel__absolutist_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(spk_abs_su_t60, speech_protection_kernel__absolutist_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(spk_abs_su_t80, speech_protection_kernel__absolutist_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(spk_abs_su_t100, speech_protection_kernel__absolutist_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the speech_protection_kernel family. The absolutist reading and its siblings are structurally distinct constraints derived from the same constitutional text but with different epsilon profiles, beneficiary/victim structures, and axiom sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
