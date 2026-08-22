% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace of Ideas Reading of Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the marketplace reading of the First
 *   Amendment speech-protection kernel: the doctrinal commitment that speech
 *   is protected because unrestricted discourse advances truth, and that the
 *   proper remedy for false or harmful speech is more speech rather than
 *   content-based suppression. The reading treats protection as justified by
 *   collective epistemic benefit rather than individual speaker autonomy. It
 *   is one of five contested readings of the same constitutional kernel,
 *   distinguished by its instrumental grounding and its rejection of
 *   content-based restrictions as distorting the truth-discovery process.
 *
 * KEY AGENTS:
 *   - Constitutional courts (agenda_setter): Institutional authority interpreting the First Amendment under the marketplace framework.
 *   - Commercial publishers (beneficiary): Powerful actors relying on broad speech immunity.
 *   - Digital platforms (beneficiary): Global hosts using the doctrine to resist takedown demands.
 *   - Harassment targets (payer): Powerless actors denied content-based recourse.
 *   - Disinformation targets (payer): Moderate-power actors blocked from preventive state intervention.
 *   - Civil liberties observers (observer): Analytical monitors of doctrinal scope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.48).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace of Ideas Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '7a76ee00-a2d8-4e8b-be68-8c9fd381691b').
narrative_ontology:cs_kernel_codification('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', fixed_text).
narrative_ontology:cs_authority_grounding('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', lineage).
narrative_ontology:cs_interpretation_layer_present('7a76ee00-a2d8-4e8b-be68-8c9fd381691b').
narrative_ontology:cs_reading_relation('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_axiom('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', foundational, unrestricted_discourse_advances_truth).
narrative_ontology:cs_axiom_status(unrestricted_discourse_advances_truth, holdable).
narrative_ontology:cs_axiom_grounding('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', unrestricted_discourse_advances_truth, instrumental).
narrative_ontology:cs_axiom('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', foundational, government_arbiter_untrustworthy).
narrative_ontology:cs_axiom_status(government_arbiter_untrustworthy, holdable).
narrative_ontology:cs_axiom_grounding('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', government_arbiter_untrustworthy, empirically_contingent).
narrative_ontology:cs_reference_frame('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', open_discourse_regime).
narrative_ontology:cs_drift_state('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', digital_disinformation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7a76ee00-a2d8-4e8b-be68-8c9fd381691b', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, commercial_publishers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, digital_platforms).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, harassment_targets).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, disinformation_targets).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, content_neutrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the First Amendment under the marketplace-of-ideas framework, striking down content-based restrictions and elevating counterspeech over censorship as the preferred remedy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Publish and distribute content with broad immunity from government censorship; rely on the doctrine to resist content-based liability and regulation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, commercial_publishers, beneficiary,
    powerful, biographical, constrained, national).

% Host user-generated content under legal frameworks that treat them as forums rather than publishers; use the doctrine to deflect government takedown demands.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, digital_platforms, beneficiary,
    powerful, biographical, constrained, global).

% Experience sustained abuse that the doctrinal framework treats as protected speech or legitimate counterspeech; lack state recourse because content-based restrictions are foreclosed.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, harassment_targets, payer,
    powerless, immediate, trapped, local).

% Suffer concrete harms from false speech in areas like health or electoral integrity; the marketplace reading blocks preventive or corrective state intervention, leaving private counterspeech as the only remedy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, disinformation_targets, payer,
    moderate, immediate, constrained, national).

% Monitor judicial doctrine and legislative encroachments across jurisdictions; litigate and report on the scope of speech protections without being direct beneficiaries or targets of the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, civil_liberties_observers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables collective truth-discovery by preventing any single actor, especially the state, from monopolizing or suppressing ideas, creating a decentralized epistemic marketplace where truth is expected to emerge from conflict rather than curation.
% TRANSFER_FUNCTION: Transfers communicative immunity from the state to private speakers and platforms, while transferring the costs of harmful speech to targets who are denied content-based legal recourse.
% ABSENT_VOICES: Targets of harassment and disinformation are structurally excluded from the doctrinal conversation, which frames their injuries as acceptable costs of an open marketplace; European dignity-based and harm-threshold perspectives are marginalized in US jurisprudence.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, legislatures would gain broad content-based regulatory authority, platforms would face publisher liability, and the information environment would reorganize around permissible speech categories rather than open exchange.
% FOUNDING_PROBLEM: How to discover political and social truth without empowering the state to act as censor and arbiter of acceptable belief.
% FOUNDING_PROBLEM_CORROBORATION: Free speech scholars and civil liberties historians attest to the state-censorship problem from outside the commercial media seats; critical legal theorists and communication empiricists contest that the marketplace reading solves the problem, corroborating the contested status from outside the judiciary.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the doctrine genuinely coordinates epistemic discovery but structurally allocates communicative power toward well-resourced speakers and away from vulnerable targets who cannot effectively deploy counterspeech. Suppression (0.55) reflects the constraint's active suppression of alternative regulatory regimes, such as European-style dignity balancing or harm-threshold content laws. Theater ratio (0.25) captures the ritualistic citation of Holmesian metaphors that increasingly outrun the empirical reality of the digital speech environment. Accessibility collapse (0.72) is high because once the marketplace framework is accepted within US constitutional culture, alternatives collapse rapidly; resistance (0.45) reflects sustained scholarly and social movement critique. The metrics are authored independently of the claimed type (rope), which reflects the reading's self-understanding as coordination.
 *
 * PERSPECTIVAL GAP:
 *   The constitutional courts and civil liberties observers compute the constraint as a genuine coordination mechanism protecting democratic discourse against state overreach. The harassment targets and disinformation targets compute it as a structure that privatizes harm and denies them state protection. The engine derives this divergence from the same structural data: low directionality for beneficiaries with constrained exit, high directionality for powerless payers with trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial publishers and digital platforms are structural beneficiaries with constrained but favorable exit: they operate within the doctrine and capture communicative immunity (low d). Harassment targets and disinformation targets are structural payers with limited exit options; the doctrine blocks the state remedy they would otherwise access, amplifying effective extraction toward the high-d end. Constitutional courts sit near symmetric but agenda-setting, with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The marketplace reading avoids snare classification because its coordination function is genuine and primary: it solves a real collective-action problem in truth-discovery by preventing state monopoly over information. It avoids piton classification because it retains substantial functional operation and is actively maintained by a beneficiary coalition (publishers, platforms) and an agenda-setter (courts) who both benefit from its persistence. The moderate theater ratio (0.25) indicates some ritualization but not dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_marketplace_validity,
    'Does unrestricted discourse actually advance truth in contemporary digital media environments characterized by algorithmic amplification and disinformation campaigns?',
    'Systematic empirical assessment of information-ecosystem outcomes in jurisdictions with varying speech-regulation intensity, comparing epistemic quality metrics across regimes.',
    'If the empirical premise is falsified, the instrumental justification for the marketplace reading collapses, potentially shifting the constraint toward a snare or piton classification as performative maintenance outruns function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_marketplace_validity, empirical, 'Whether the truth-discovery empirical premise holds in modern media environments.').

omega_variable(
    asymmetric_counter_speech,
    'Does the counterspeech remedy assume a symmetry of voice and resources that does not exist structurally, rendering the remedy illusory for powerless payers?',
    'Comparative resource-mapping of counterspeech capacity across power levels, measuring whether targets can effectively respond to organized harassment or platform-scale disinformation.',
    'If asymmetry is severe, the coordination function is partiallyillusory and the constraint functions more extractively than its theory admits, supporting a tangled_rope reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_counter_speech, conceptual, 'Whether counterspeech remedy assumes false symmetry of voice.').

omega_variable(
    coordination_extraction_boundary,
    'Is the suppression of content-based regulation a necessary cost of epistemic coordination, or is it extractive overhead that benefits institutional speakers at the expense of speech-harm victims?',
    'Comparative institutional analysis of jurisdictions that maintain truth-discovery coordination while permitting narrow harm-based content restrictions.',
    'If harm-based restrictions are compatible with truth-discovery coordination, the marketplace reading''s blanket rejection of content-based regulation is extractive overhead; if incompatible, the suppression is a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether content-neutrality suppression is coordination cost or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marketplace_reading_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(marketplace_reading_tr_t25, speech_protection_kernel__marketplace_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(marketplace_reading_tr_t50, speech_protection_kernel__marketplace_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement(marketplace_reading_tr_t75, speech_protection_kernel__marketplace_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement(marketplace_reading_tr_t100, speech_protection_kernel__marketplace_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(marketplace_reading_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(marketplace_reading_be_t25, speech_protection_kernel__marketplace_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(marketplace_reading_be_t50, speech_protection_kernel__marketplace_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(marketplace_reading_be_t75, speech_protection_kernel__marketplace_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement(marketplace_reading_be_t100, speech_protection_kernel__marketplace_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(marketplace_reading_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(marketplace_reading_su_t25, speech_protection_kernel__marketplace_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement(marketplace_reading_su_t50, speech_protection_kernel__marketplace_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(marketplace_reading_su_t75, speech_protection_kernel__marketplace_reading, suppression_requirement, 75, 0.52).
narrative_ontology:measurement(marketplace_reading_su_t100, speech_protection_kernel__marketplace_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the speech_protection_kernel constraint family. The kernel (First Amendment speech protection) decomposes into multiple structurally distinct readings because each reading instantiates a different constraint with different beneficiary/victim structures, epsilon values, and justificatory frameworks. The marketplace reading is linked to its siblings via shared institutional domain and mutual exclusivity pressures, though only the forecloses relation (engine-computed) would indicate logical impossibility of coexistence within a single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
