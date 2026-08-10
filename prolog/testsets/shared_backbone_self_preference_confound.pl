% ============================================================================
% CONSTRAINT STORY: shared_backbone_self_preference_confound
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shared_backbone_self_preference_confound, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: shared_backbone_self_preference_confound
 *   human_readable: Uncrossed Persona-Model/System-Model Design in Simulated-User Evaluation
 *   domain: ai_evaluation_methodology
 *
 * SUMMARY:
 *   The MatrAIx-style simulated-user evaluation infrastructure pairs persona
 *   records with LLMs to produce scalable stand-ins for human evaluation
 *   subjects. The methodology never crosses the persona-agent model against
 *   the system-under-test model on the same task, so when a same-family
 *   pairing produces favorable outcomes, the result is structurally ambiguous
 *   between the system genuinely satisfying a simulated user and the
 *   persona-agent model simply recognizing and preferring output from its own
 *   model family. The paper's own future-work section acknowledges this
 *   ablation is missing rather than closing it. This is authored as a piton:
 *   the confound extracts diffusely from anyone who trusts same-family
 *   comparative results, no single agenda-setter profits enough from the
 *   omission to be called a captured beneficiary in the concentrated sense,
 *   and the gap persists through inertia (scoping choices, deferred
 *   ablations, validation theater around adherence rates) rather than through
 *   active enforcement or coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shared_backbone_self_preference_confound, 0.58).
domain_priors:suppression_score(shared_backbone_self_preference_confound, 0.42).
domain_priors:theater_ratio(shared_backbone_self_preference_confound, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shared_backbone_self_preference_confound, extractiveness, 0.58).
narrative_ontology:constraint_metric(shared_backbone_self_preference_confound, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shared_backbone_self_preference_confound, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shared_backbone_self_preference_confound, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shared_backbone_self_preference_confound, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shared_backbone_self_preference_confound, piton).
narrative_ontology:human_readable(shared_backbone_self_preference_confound, "Uncrossed Persona-Model/System-Model Design in Simulated-User Evaluation").
narrative_ontology:topic_domain(shared_backbone_self_preference_confound, "ai_evaluation_methodology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shared_backbone_self_preference_confound, model_providers_whose_agents_score_well_under_same_family_evaluation).
narrative_ontology:constraint_victim(shared_backbone_self_preference_confound, product_teams_who_might_deploy_matched_backbone_evaluations_unknowingly).
narrative_ontology:constraint_vindicates(shared_backbone_self_preference_confound, persona_conditioning_is_measurable_and_reproducible).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide the backbone models used both as persona agents and as systems under test in benchmarking studies conducted by partners and downstream users. Favorable results attributable to shared-backbone self-recognition accrue to their models' apparent performance without anyone having to argue for it; they neither designed the confound nor need to defend it, because the paper's methodology section never forces the crossed comparison that would surface it. They administer nothing here and pay nothing — the ambiguity simply resolves in their favor by default.
narrative_ontology:constraint_stakeholder(shared_backbone_self_preference_confound, model_providers_whose_agents_score_well_under_same_family_evaluation, beneficiary,
    institutional, generational, arbitrage, global).

% Adopt the persona-agent evaluation infrastructure to screen product decisions cheaply before human studies. If they happen to run persona agents on the same backbone family as the system under test (a natural default, since many teams standardize on one vendor's models for both roles), any favorable outcome is uninterpretable — they cannot tell whether the product performed well or the model simply preferred itself. They have no way to detect this from the paper's reported results because the crossed design was never run; discovering the confound requires re-implementing the ablation the authors declined to do.
narrative_ontology:constraint_stakeholder(shared_backbone_self_preference_confound, product_teams_who_might_deploy_matched_backbone_evaluations_unknowingly, payer,
    moderate, biographical, constrained, national).

% Designed and validated the MatrAIx persona infrastructure, chose which ablations to run, and explicitly deferred the persona-model × system-model crossing to future work. They control the validation apparatus (adherence rates, judge-agreement figures, Cramer's V checks) that certifies the tool's adequacy, and they are the ones who could add the crossed design at comparatively low engineering cost — the omission is a scoping choice, not a technical barrier.
narrative_ontology:constraint_stakeholder(shared_backbone_self_preference_confound, infrastructure_authors_and_maintainers, agenda_setter,
    institutional, generational, mobile, global).

% Real users whose costly, slower human-panel studies are the thing persona-agent evaluation is meant to substitute for or de-prioritize. They have no seat in the methodology debate and no way to object that a same-family evaluation might be measuring model self-preference rather than anything resembling their own preferences, because the studies that would have surfaced this run on synthetic personas instead of them.
narrative_ontology:constraint_stakeholder(shared_backbone_self_preference_confound, human_evaluation_subjects_displaced_by_persona_substitution, excluded,
    powerless, immediate, trapped, national).

% Read the published methodology, notice the absence of a crossed persona-model/system-model design, and can in principle re-run the missing ablation. They bear no direct cost from the confound but are positioned to name it, which is how the ambiguity could eventually be resolved into a measured self-preference bias rather than remaining an open question the paper declines to close.
narrative_ontology:constraint_stakeholder(shared_backbone_self_preference_confound, downstream_researchers_and_auditors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shared_backbone_self_preference_confound, model_providers_whose_agents_score_well_under_same_family_evaluation).
narrative_ontology:fixing_cost_class(shared_backbone_self_preference_confound, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The infrastructure genuinely solves a real coordination problem: pre-deployment screening of product and interface decisions is expensive and slow with human panels, and a validated persona-agent layer lets many trials run in parallel at low marginal cost, surfacing corner cases before committing to costly human studies.
% TRANSFER_FUNCTION: Interpretive confidence moves from anyone who would need to distrust same-family evaluation results toward model providers whose backbones happen to be used on both sides of an uncrossed comparison — a transfer of unearned credibility, not money or labor, from downstream evaluators' epistemic position to upstream model reputation.
% ABSENT_VOICES: Product teams who will actually deploy matched-backbone evaluations in practice were not consulted on whether the crossed ablation should be a precondition for trusting the results; the paper's own future-work section acknowledges the gap exists but treats it as deferred rather than as a live methodological requirement before the tool is used for comparative claims.
% DISAPPEARANCE_RATIONALE: If the confound were closed tomorrow (i.e., the crossed design were run and self-preference bias measured and corrected for), the infrastructure's core coordination function — cheap parallel screening — would be unchanged. What would rearrange is any comparative claim about which system performed better under same-family evaluation; those claims would either be validated, discounted by a measured bias term, or retracted. Model providers who benefit from the current ambiguity would contest that anything needs to change; auditors would say the world already should have rearranged upon publication.
% FOUNDING_PROBLEM: Human-subject evaluation of AI product and interface decisions is slow, expensive, and hard to parallelize across many attribute-conditioned scenarios; the infrastructure was built to generate scalable behavioral trials that approximate or substitute for that costly process.
% FOUNDING_PROBLEM_CORROBORATION: The infrastructure authors themselves attest the screening problem remains live (it motivates the entire paper). No source outside the authors and their funding partners has independently corroborated that the specific crossed-design omission is an acceptable resolution of the self-preference question rather than an unaddressed validity gap; downstream auditors and product teams who might detect the confound have not been asked and have not weighed in in the published record.
narrative_ontology:disappearance_verdict(shared_backbone_self_preference_confound, contested).
narrative_ontology:founding_problem_status(shared_backbone_self_preference_confound, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shared_backbone_self_preference_confound, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-10',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(shared_backbone_self_preference_confound, 'none', 1).
narrative_ontology:epsilon_provenance(shared_backbone_self_preference_confound, 0.58, 'claude-sonnet-5', 'matraix_persona_simulation_2026_20260810_114056', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shared_backbone_self_preference_confound_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shared_backbone_self_preference_confound, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shared_backbone_self_preference_confound_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that misinterpreted same-family results can materially mislead product decisions, but the extraction is diffuse and unintentional rather than a concentrated rent. Suppression (0.42) is moderate — nothing actively prevents someone from running the crossed ablation, but the absence of the design in the flagship methodology normalizes not running it, which functions as a soft barrier. Theater ratio (0.71) is high and rising across the measured interval: adherence rates, judge-agreement percentages, and Cramer's V checks are real validation work, but an increasing share of that validation activity substitutes for the one ablation that would actually test the self-preference question, rather than addressing it directly. Accessibility collapse (0.35) is low-moderate — the alternative (running the crossed design) remains straightforward and undertaken by anyone motivated to do it; this is not a constraint that has foreclosed its alternatives, which is part of why piton (inertial, not enforced) fits better than snare.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure authors' seat, the missing ablation is a scoping decision, one item on a long future-work list, and no different in kind from any other unrun experiment. From a product team's seat that has unknowingly run a matched-backbone evaluation, the same omission is the entire difference between a trustworthy screening signal and a self-congratulating model artifact. The engine should compute these as structurally different experiences of the same missing design choice, not as disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Model providers whose backbones populate both the persona-agent role and the system-under-test role in common deployment patterns are the structural beneficiaries: they collect reputational credit from ambiguous results without any need to defend the ambiguity, and their exit options are effectively arbitrage-grade — they are not implicated by name in any single study. Product teams who deploy matched-backbone evaluations are the targets: they bear the interpretive cost of an ambiguity they likely cannot detect from published results alone, and their exit is constrained by the fact that standardizing on one vendor's models across roles is often the path of least resistance, not a mistake they'd recognize as risky.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cheap, parallelizable pre-deployment screening) remains live — this prevents the constraint from being mislabeled as pure extraction with no coordination function at all. But the specific omission (declining to isolate self-preference) has no sunset, no active defender, and no concentrated profiteer large enough to justify calling it a snare; it persists because closing it costs someone effort and no single actor is hurt enough, yet, to force the fix. That asymmetry — administrable at low cost, but not costly enough to any one actor to compel action — is the piton signature: diffuse, inertial, performatively validated around the edges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_preference_magnitude_unmeasured,
    'What is the actual direction and magnitude of self-preference bias when persona-agent and system-under-test share a backbone, once measured directly?',
    'Run the crossed design: hold the task fixed and vary persona-agent model and system-under-test model independently (same-family vs. cross-family pairings), then compare outcome distributions.',
    'If the measured bias is small, the current uncrossed design is a defensible simplification and the piton reading weakens toward a rope with an unaddressed edge case. If the bias is large, undisclosed same-family evaluations become materially misleading and the constraint moves toward a tangled_rope or snare reading for teams who rely on them without knowing the risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_preference_magnitude_unmeasured, empirical, 'Whether same-family self-preference bias is large enough to invalidate comparative claims.').

omega_variable(
    omission_intentionality_ambiguity,
    'Is the uncrossed design a genuine oversight/resource-scoping decision, or a structurally convenient omission that avoids surfacing a result unfavorable to the infrastructure''s own credibility and to its model-provider funding partners?',
    'Compare the cost of running the crossed ablation against the infrastructure''s other reported ablations; examine whether funding or partnership relationships with model providers correlate with which ablations were prioritized versus deferred.',
    'If genuinely a resourcing oversight, this remains a piton (inertial, no concentrated beneficiary orchestrating the gap). If the omission tracks funding-partner interests, the classification should move toward tangled_rope or snare, since a beneficiary would then be actively (if quietly) served by the gap persisting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omission_intentionality_ambiguity, conceptual, 'Whether the missing ablation is inertial neglect or a structurally convenient avoidance.').

omega_variable(
    kernel_reading_correspondence_vs_mechanism,
    'Does the legitimacy of the persona-as-proxy infrastructure depend on population-level correspondence (which the self-preference confound would undermine directly, since it contaminates the very outcomes claimed to reflect user response) or on narrower behavioral-mechanism claims (steerability, traceable conditioning) that the confound leaves largely untouched?',
    'Determine which reading the paper''s actual comparative claims rely on: if claims are framed as ''users preferred X,'' correspondence framing applies and the confound is damaging; if claims are framed as ''the model was steered toward X,'' mechanism framing applies and the confound is a narrower internal-validity question.',
    'Under the representational_correspondence_reading, the shared-backbone confound compounds an already-low-fidelity validation picture, pushing the constraint toward snare for downstream trust. Under the behavioral_mechanism_reading, the confound matters only for cross-model comparative claims, not for single-model conditioning claims, leaving more of the infrastructure''s core contribution intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_correspondence_vs_mechanism, conceptual, 'Which kernel reading of persona-as-proxy governs how damaging the self-preference confound is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shared_backbone_self_preference_confound, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shar_tr_t0, shared_backbone_self_preference_confound, theater_ratio, 0, 0.55).
narrative_ontology:measurement(shar_tr_t4, shared_backbone_self_preference_confound, theater_ratio, 4, 0.6).
narrative_ontology:measurement(shar_tr_t8, shared_backbone_self_preference_confound, theater_ratio, 8, 0.63).
narrative_ontology:measurement(shar_tr_t12, shared_backbone_self_preference_confound, theater_ratio, 12, 0.66).
narrative_ontology:measurement(shar_tr_t16, shared_backbone_self_preference_confound, theater_ratio, 16, 0.68).
narrative_ontology:measurement(shar_tr_t20, shared_backbone_self_preference_confound, theater_ratio, 20, 0.7).
narrative_ontology:measurement(shar_tr_t24, shared_backbone_self_preference_confound, theater_ratio, 24, 0.71).

% Extraction over time
narrative_ontology:measurement(shar_be_t0, shared_backbone_self_preference_confound, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(shar_be_t4, shared_backbone_self_preference_confound, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(shar_be_t8, shared_backbone_self_preference_confound, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(shar_be_t12, shared_backbone_self_preference_confound, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(shar_be_t16, shared_backbone_self_preference_confound, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(shar_be_t20, shared_backbone_self_preference_confound, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(shar_be_t24, shared_backbone_self_preference_confound, base_extractiveness, 24, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shared_backbone_self_preference_confound, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shared_backbone_self_preference_confound, resource_allocation).
narrative_ontology:affects_constraint(shared_backbone_self_preference_confound, persona_fidelity_correspondence_gap).
narrative_ontology:affects_constraint(shared_backbone_self_preference_confound, responsible_use_disclosure_scaffolding).

% DUAL FORMULATION NOTE:
% This story addresses one specific structural delta within the broader MatrAIx commitment-system contest: the presence/absence of a persona-model x system-model crossed design and its consequences for interpreting favorable outcomes. It is downstream of the representational_correspondence_reading and behavioral_mechanism_reading disagreement documented in the kernel context (persona_as_valid_proxy) — under the correspondence reading this confound is a compounding failure; under the mechanism reading it is a narrower comparative-claims caveat. It is deliberately NOT the same constraint as the general persona-fidelity correspondence gap (median kappa ~0.000 across joinable fields), which has its own ε and its own story, linked here rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
