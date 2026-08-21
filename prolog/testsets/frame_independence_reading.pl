% ============================================================================
% CONSTRAINT STORY: frame_independence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_frame_independence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: frame_independence_reading
 *   human_readable: Frame-Independence Reading of Evaluator Blindness (Rules-and-Categories Deprivation)
 *   domain: epistemology_of_evaluation/ai_agent_architecture/research_methodology
 *
 * SUMMARY:
 *   A project seeking a genuinely frame-independent evaluation withholds its
 *   rule files, category taxonomies, and finding-definitions from the
 *   evaluating instance on the theory that any evaluator who has already been
 *   told what counts as a finding cannot discover errors baked into that very
 *   definition. The protocol is designed to solve a real problem —
 *   self-referential blindness — but the deprivation cannot be verified from
 *   outside: task phrasing, training priors, and residual categorical habits
 *   can silently reintroduce the frame, and no party inside the protocol
 *   (including the evaluator itself) can detect this reintroduction, because
 *   detecting it requires exactly the frame that was supposedly removed. The
 *   result looks like a rigorous audit while functioning, in the worst case,
 *   as a credentialing device that certifies category errors as clean
 *   findings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(frame_independence_reading, 0.81).
domain_priors:suppression_score(frame_independence_reading, 0.72).
domain_priors:theater_ratio(frame_independence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(frame_independence_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(frame_independence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(frame_independence_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(frame_independence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(frame_independence_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(frame_independence_reading, tangled_rope).
narrative_ontology:human_readable(frame_independence_reading, "Frame-Independence Reading of Evaluator Blindness (Rules-and-Categories Deprivation)").
narrative_ontology:topic_domain(frame_independence_reading, "epistemology_of_evaluation/ai_agent_architecture/research_methodology").

domain_priors:requires_active_enforcement(frame_independence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(frame_independence_reading, 'c3e696b0-06c4-4b3f-828c-56bdd1f12259').
narrative_ontology:cs_kernel_codification('c3e696b0-06c4-4b3f-828c-56bdd1f12259', distributed).
narrative_ontology:cs_authority_grounding('c3e696b0-06c4-4b3f-828c-56bdd1f12259', practice).
narrative_ontology:cs_interpretation_layer_present('c3e696b0-06c4-4b3f-828c-56bdd1f12259').
narrative_ontology:cs_reading_relation('c3e696b0-06c4-4b3f-828c-56bdd1f12259', frame_independence_reading__cold_reader_reading, influences).
narrative_ontology:cs_reading_relation('c3e696b0-06c4-4b3f-828c-56bdd1f12259', frame_independence_reading__presentation_audit_reading, influences).
narrative_ontology:cs_axiom('c3e696b0-06c4-4b3f-828c-56bdd1f12259', foundational, rule_and_category_withholding_is_necessary_for_frame_critique).
narrative_ontology:cs_axiom_status(rule_and_category_withholding_is_necessary_for_frame_critique, holdable).
narrative_ontology:cs_axiom_grounding('c3e696b0-06c4-4b3f-828c-56bdd1f12259', rule_and_category_withholding_is_necessary_for_frame_critique, conventional).
narrative_ontology:cs_axiom('c3e696b0-06c4-4b3f-828c-56bdd1f12259', foundational, achieved_deprivation_is_unverifiable_from_outside_the_protocol).
narrative_ontology:cs_axiom_status(achieved_deprivation_is_unverifiable_from_outside_the_protocol, holdable).
narrative_ontology:cs_axiom_grounding('c3e696b0-06c4-4b3f-828c-56bdd1f12259', achieved_deprivation_is_unverifiable_from_outside_the_protocol, empirically_contingent).
narrative_ontology:cs_reference_frame('c3e696b0-06c4-4b3f-828c-56bdd1f12259', self_auditing_frame_impossibility).
narrative_ontology:cs_drift_state('c3e696b0-06c4-4b3f-828c-56bdd1f12259', credentialing_era_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3e696b0-06c4-4b3f-828c-56bdd1f12259', '').
narrative_ontology:cs_kernel_id(frame_independence_reading, blindness_decomposition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(frame_independence_reading, project_designers).
narrative_ontology:constraint_beneficiary(frame_independence_reading, instrument_credentialing_bodies).
narrative_ontology:constraint_victim(frame_independence_reading, downstream_report_consumers).
narrative_ontology:constraint_victim(frame_independence_reading, unstated_selection_rule_subjects).
narrative_ontology:constraint_victim(frame_independence_reading, blind_evaluator_instances).
narrative_ontology:constraint_vindicates(frame_independence_reading, genuine_frame_independence_is_achievable_by_rule_withholding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors the rule files, category taxonomies, and finding-definitions that constitute the project's frame, then decides how much of that frame to withhold from an evaluator claimed to be 'blind.' Collects credibility for the audit by pointing to the deprivation protocol, while retaining full authorship of what the frame ever was and what counts as having successfully withheld it. Faces no real exit cost if the deprivation is incomplete, because incompleteness is invisible from outside.
narrative_ontology:constraint_stakeholder(frame_independence_reading, project_designers, agenda_setter,
    institutional, generational, arbitrage, national).

% Certifies or endorses the blind-evaluation methodology as rigorous without independently verifying that the rules and categories were genuinely absent from the evaluator's context. Benefits from being able to point to a 'frame-independent' audit as a credibility marker without bearing the cost of proving frame-independence was achieved.
narrative_ontology:constraint_stakeholder(frame_independence_reading, instrument_credentialing_bodies, beneficiary,
    organized, generational, mobile, national).

% The instance instructed to evaluate 'without the project's framing' — it has no way to verify from inside its own context whether the rule files, categories, or finding-definitions were actually excluded, partially excluded, or subtly reintroduced through phrasing of the task itself. If the deprivation is fake, the evaluator produces a false-confidence finding and has no mechanism to detect its own contamination; it cannot exit the frame it cannot see.
narrative_ontology:constraint_stakeholder(frame_independence_reading, blind_evaluator_instances, payer,
    moderate, immediate, trapped, local).

% Reads the resulting 'blind audit' report and treats its independence claim as load-bearing evidence — e.g., 'an inventory assembled under an unstated selection rule reads as complete.' Has no access to the rule files that were or weren't withheld, and no practical way to audit the audit. Pays in the form of decisions made on the strength of a completeness claim that may be a category error invisible from inside the frame that produced it.
narrative_ontology:constraint_stakeholder(frame_independence_reading, downstream_report_consumers, payer,
    powerless, biographical, constrained, national).

% The entities or cases that were selected into or out of the inventory/evaluation by a rule that was never stated because the frame that encoded it was withheld along with everything else. They cannot object because their exclusion is not visible even in principle from inside the deprivation protocol — the only vantage from which their absence would register is exactly the frame that was removed.
narrative_ontology:constraint_stakeholder(frame_independence_reading, unstated_selection_rule_subjects, excluded,
    powerless, biographical, trapped, national).

% Studies whether claimed rule-file/category deprivation actually achieves frame-independence, or whether the frame reasserts itself through task phrasing, training priors, or residual category habits the evaluator cannot introspect on. Can in principle detect the gap between claimed and achieved deprivation but has no standing enforcement role over the project.
narrative_ontology:constraint_stakeholder(frame_independence_reading, methodology_auditors, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(frame_independence_reading, project_designers).
narrative_ontology:fixing_cost_class(frame_independence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real problem: an evaluator who already knows the rule files and categories cannot discover category errors baked into those categories themselves, because it has already been told what to look for. Withholding the frame is the only way to test whether the frame itself — not just compliance with it — is sound.
% TRANSFER_FUNCTION: Moves epistemic authority from the withheld frame to whatever the blind evaluator produces, and moves the cost of any residual, undetectable frame-contamination onto downstream consumers of the resulting report and onto anyone whose case was shaped by an unstated selection rule the evaluator could never surface.
% ABSENT_VOICES: The unstated-selection-rule subjects have no seat at all — by construction, no one inside or outside the deprivation protocol can name them, because naming them requires the very frame that was removed. Blind evaluator instances are present but structurally unable to object to their own contamination.
% DISAPPEARANCE_RATIONALE: If the frame-independence reading disappeared — i.e., if evaluators were always handed the rule files and categories up front — every downstream claim of 'independent audit finding' would have to be re-labeled as a compliance check rather than a frame critique, and any category errors currently invisible because they are baked into the withheld categories would become permanently undetectable by this method rather than merely difficult to detect.
% FOUNDING_PROBLEM: Evaluators handed the standing rules cannot see errors in the rules themselves, because the rules define what counts as a finding — the frame cannot audit itself from inside.
% FOUNDING_PROBLEM_CORROBORATION: Methodology auditors (an analytical seat outside the credentialing and design apparatus) attest that genuine rule/category withholding is difficult to verify from outside and that task phrasing routinely reintroduces categorical priors even when literal rule files are absent — this is corroboration from outside the benefiting parties that the founding problem remains only partially solved by the current protocol, not that it is solved.
narrative_ontology:disappearance_verdict(frame_independence_reading, world_rearranges).
narrative_ontology:founding_problem_status(frame_independence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(frame_independence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(frame_independence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(frame_independence_reading, 0.81, 'claude-sonnet-5', 'blind_reviewer_jurisdiction_2026_20260820_211650', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frame_independence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(frame_independence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(frame_independence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) because the arrangement's persistence depends on a claim of achieved frame-independence that cannot be checked by anyone inside the standard evaluation loop — the gap between claimed and achieved deprivation is precisely where the extraction hides. Suppression (0.72) reflects that the unstated-selection-rule subjects have no possible avenue of objection by construction, not merely by neglect; this is closer to snare-adjacent suppression than ordinary institutional friction. Theater ratio rises across the interval (0.30 to 0.58) because as the credentialing use of 'blind audit' language spreads, an increasing share of the deprivation protocol's apparent rigor is performative signaling of independence rather than verified independence. Accessibility collapse (0.62) is moderate-high: once a reader accepts that a completed inventory or finding set is 'the blind result,' the alternative reading (that an unstated selection rule shaped it) becomes very hard to recover, but not fully impossible, because methodology auditors retain an external analytical vantage. Resistance is comparatively low (0.44) precisely because the mechanism that would generate resistance — visibility of the excluded frame — is what has been removed.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this reads as a genuine methodological safeguard — the only way to test whether the frame itself, not just compliance with it, is sound. From the payer seats (blind evaluator, downstream consumers, selection-rule subjects), the same structure operates as an unverifiable independence claim whose failure mode is invisible by design. The engine should compute a sharp seat divergence here: the agenda-setter seat likely reads closer to rope/coordination, while the payer and excluded seats read closer to snare, because the suppression they experience is total precisely where the coordination story is strongest.
 *
 * DIRECTIONALITY LOGIC:
 *   Project designers sit at the beneficiary end: they author the frame, decide what is withheld, and collect the credibility of an 'independent' finding without bearing verification cost. Instrument credentialing bodies benefit similarly at one remove, endorsing the methodology as a marker of rigor. Blind evaluator instances and downstream report consumers are targets: the evaluator cannot detect its own contamination and consumers treat a completeness or independence claim as load-bearing without access to the withheld rule files. Unstated-selection-rule subjects are the deepest target class — trapped not by weak power alone but by a structural invisibility that makes even naming them dependent on the frame that excluded them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — frames cannot audit themselves from inside — remains genuinely live; this prevents dismissing the whole arrangement as pure extraction. But because achieved deprivation is unverifiable, the arrangement risks outliving its actual function: once credentialing bodies treat 'blind audit' as a rigor marker regardless of whether deprivation was real, the mandate persists on the strength of its own label rather than continued demonstration that the deprivation is genuine. Classifying this as tangled_rope rather than snare or mountain preserves the reading that a real coordination function coexists with unverifiable, asymmetric extraction — which is exactly the ambiguity this reading is built to surface, not resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_performed_deprivation,
    'Can rule-file and category deprivation ever be verified as genuinely achieved from outside the evaluating instance, or is ''frame independence'' permanently a claim rather than a checkable state?',
    'Controlled comparison: run matched evaluations with verified full deprivation versus covert partial reintroduction of categories through task phrasing, and check whether downstream findings differ measurably. If they do not differ, deprivation claims are currently unverifiable in practice.',
    'If unverifiable, the arrangement''s coordination claim is structurally unfalsifiable, which pushes the classification toward snare; if a verification method exists and is adopted, the tangled_rope reading becomes more stable as genuine coordination with bounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_performed_deprivation, empirical, 'Whether frame-independence is a checkable achievement or an unfalsifiable claim.').

omega_variable(
    unstated_selection_rule_detectability,
    'Is it even in principle possible to detect an unstated selection rule from inside a deprivation protocol that has removed the very frame that would name the rule?',
    'Formal analysis of whether any evaluator lacking access to a selection criterion can, by construction, ever surface evidence of that criterion''s existence purely from the shape of the resulting set (e.g., statistical anomaly detection on inventory completeness without category access).',
    'If detection is possible in principle via structural anomalies, the victim class (unstated_selection_rule_subjects) has a potential remedy path and the extraction is bounded; if detection is impossible in principle, this reading''s classification toward the snare end of tangled_rope is strengthened, and the excluded-voice problem becomes permanent rather than contingent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unstated_selection_rule_detectability, conceptual, 'Whether the excluded selection-rule subjects have any possible path to visibility.').

omega_variable(
    sibling_reading_severity_ordering,
    'Is it actually true that this reading (rule/category deprivation) is the most severe of the three blindness readings, or does the ordering depend on assumptions about how much category priors versus surface output versus presentation artifacts drive an evaluator''s judgment?',
    'Comparative ablation across the three deprivation levels (cold_reader_reading, presentation_audit_reading, frame_independence_reading) measuring which ablation produces the largest change in evaluator output, holding the underlying material constant.',
    'If category/rule priors turn out to have less influence than assumed, this reading''s claimed epsilon-maximality within the kernel family would be overstated relative to the sibling readings, though this would not change this story''s own epsilon, only its relative ranking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_severity_ordering, conceptual, 'Whether this reading''s assumed severity-maximality within the kernel family is itself an empirical claim requiring evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(frame_independence_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fram_tr_t0, frame_independence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(fram_tr_t0, observed).
narrative_ontology:measurement(fram_tr_t4, frame_independence_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement_basis(fram_tr_t4, observed).
narrative_ontology:measurement(fram_tr_t8, frame_independence_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement_basis(fram_tr_t8, observed).
narrative_ontology:measurement(fram_tr_t12, frame_independence_reading, theater_ratio, 12, 0.47).
narrative_ontology:measurement_basis(fram_tr_t12, observed).
narrative_ontology:measurement(fram_tr_t16, frame_independence_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement_basis(fram_tr_t16, observed).
narrative_ontology:measurement(fram_tr_t20, frame_independence_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement_basis(fram_tr_t20, projected).
narrative_ontology:measurement(fram_tr_t24, frame_independence_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement_basis(fram_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(fram_be_t0, frame_independence_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(fram_be_t0, observed).
narrative_ontology:measurement(fram_be_t4, frame_independence_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement_basis(fram_be_t4, observed).
narrative_ontology:measurement(fram_be_t8, frame_independence_reading, base_extractiveness, 8, 0.67).
narrative_ontology:measurement_basis(fram_be_t8, observed).
narrative_ontology:measurement(fram_be_t12, frame_independence_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement_basis(fram_be_t12, observed).
narrative_ontology:measurement(fram_be_t16, frame_independence_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement_basis(fram_be_t16, observed).
narrative_ontology:measurement(fram_be_t20, frame_independence_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(fram_be_t20, projected).
narrative_ontology:measurement(fram_be_t24, frame_independence_reading, base_extractiveness, 24, 0.81).
narrative_ontology:measurement_basis(fram_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(fram_su_t0, frame_independence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(fram_su_t0, observed).
narrative_ontology:measurement(fram_su_t4, frame_independence_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement_basis(fram_su_t4, observed).
narrative_ontology:measurement(fram_su_t8, frame_independence_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(fram_su_t8, observed).
narrative_ontology:measurement(fram_su_t12, frame_independence_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(fram_su_t12, observed).
narrative_ontology:measurement(fram_su_t16, frame_independence_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(fram_su_t16, observed).
narrative_ontology:measurement(fram_su_t20, frame_independence_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(fram_su_t20, projected).
narrative_ontology:measurement(fram_su_t24, frame_independence_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(fram_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(frame_independence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(frame_independence_reading, 0.12).
narrative_ontology:affects_constraint(frame_independence_reading, cold_reader_reading).
narrative_ontology:affects_constraint(frame_independence_reading, presentation_audit_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings decomposing the natural-language concept 'blindness' in evaluator/audit design: cold_reader_reading (absence of prior project output), presentation_audit_reading (absence of presentation/formatting artifacts), and this frame_independence_reading (absence of the governing rule files and categories themselves). Each reading has a distinct epsilon and victim structure per the epsilon-invariance principle; this reading carries the highest epsilon of the three because rule/category deprivation is both hardest to verify and closest to foreclosing detection of the errors it exists to find. All three stories link to each other via affects_constraints rather than being merged into one story with a deprivation-depth parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
