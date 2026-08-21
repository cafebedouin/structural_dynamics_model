% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deletionist_reading, []).

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
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: Wikipedia Notability Guidelines (Deletionist Reading)
 *   domain: digital_commons/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint represents the 'deletionist' reading of Wikipedia's
 *   Notability Guidelines (WP:N), which views them as an essential epistemic
 *   quality filter. From this perspective, WP:N prevents the degradation of
 *   the digital commons by excluding content that lacks verifiable
 *   significance, thereby preserving the encyclopedia's reliability and
 *   utility for its readership. The constraint is framed as a coordination
 *   mechanism (Rope) that justly excludes non-notable content, with
 *   beneficiaries being the readership and editors, and no identifiable
 *   'victims' as exclusion is seen as legitimate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.15).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.25).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'a9f1dd86-9a0c-4fbe-927c-f9ced0813eab').
narrative_ontology:cs_kernel_codification('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', formalized).
narrative_ontology:cs_authority_grounding('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', practice).
narrative_ontology:cs_interpretation_layer_present('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab').
narrative_ontology:cs_reading_relation('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', foundational, epistemic_quality_preservation_is_paramount).
narrative_ontology:cs_axiom_status(epistemic_quality_preservation_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', epistemic_quality_preservation_is_paramount, deontological).
narrative_ontology:cs_axiom('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', foundational, verifiability_and_significance_are_objective_criteria).
narrative_ontology:cs_axiom_status(verifiability_and_significance_are_objective_criteria, holdable).
narrative_ontology:cs_axiom_grounding('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', verifiability_and_significance_are_objective_criteria, empirically_contingent).
narrative_ontology:cs_reference_frame('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', wikipedia_as_reliable_encyclopedia).
narrative_ontology:cs_drift_state('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a9f1dd86-9a0c-4fbe-927c-f9ced0813eab', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_editors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, content_creators_non_notable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a high-quality, reliable encyclopedia free from spam, vanity, and trivial content. Relies on the notability guidelines to ensure information presented is verifiable and significant.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_readership, beneficiary,
    organized, generational, mobile, global).

% Actively enforce notability guidelines through article creation, editing, and deletion processes (Articles for Deletion - AfD). They see themselves as stewards of the encyclopedia's quality and integrity, preventing degradation of the commons.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_editors, agenda_setter,
    organized, biographical, constrained, global).

% Their submitted articles or contributions are deleted or rejected if they do not meet the notability criteria. They bear the cost of effort expended on content deemed unsuitable, but are not considered 'victims' in this reading as their content is seen as justly excluded.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, content_creators_non_notable, payer,
    powerless, immediate, constrained, global).

% Attempt to use Wikipedia for self-promotion or commercial gain. They are systematically excluded by the notability guidelines and deletion processes, which this reading views as a necessary defense against abuse.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, spam_vanity_promoters, excluded,
    powerless, immediate, trapped, global).

% Study the mechanisms by which Wikipedia maintains its quality and authority, including the role of notability guidelines in shaping the knowledge commons. They analyze the effectiveness and potential biases of these rules.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, knowledge_infrastructure_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective effort of editors to maintain a high-quality, verifiable, and encyclopedic knowledge base by providing clear criteria for inclusion and exclusion, preventing the commons from being overwhelmed by irrelevant or unreliable information.
% TRANSFER_FUNCTION: Transfers editorial effort and attention away from non-notable or low-quality content towards maintaining and improving notable content, ensuring the collective resource is used efficiently for its stated purpose.
% ABSENT_VOICES: Individuals and groups whose content is deemed non-notable by the guidelines, particularly those promoting niche topics or personal projects, are effectively excluded from contributing to the main encyclopedia. They would argue for broader inclusion criteria.
% DISAPPEARANCE_RATIONALE: If notability guidelines vanished, Wikipedia would rapidly degrade into a chaotic repository of unverified, trivial, or promotional content. The quality and trustworthiness that attract readership and editors would erode, leading to a collapse of its epistemic authority and a fundamental reorganization of its function.
% FOUNDING_PROBLEM: Wikipedia faced the challenge of maintaining quality and preventing degradation of its open editing model from spam, vandalism, and content lacking encyclopedic merit.
% FOUNDING_PROBLEM_CORROBORATION: The Wikipedia Foundation, academic studies on information quality, and the majority of active editors corroborate that the problem of maintaining quality in an open platform remains live and requires active measures like notability guidelines. Independent analyses of other open platforms without such filters show rapid degradation.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deletionist_reading_tests).
:- end_tests(notability_guidelines__deletionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary function is quality control, not rent-seeking; any 'cost' to content creators is seen as a necessary consequence of maintaining quality. Suppression is moderate (0.25) as active enforcement (AfD, content review) is required to uphold the standards against a constant influx of non-notable submissions. Theater ratio is low (0.1) because the guidelines are genuinely applied to maintain quality, with minimal performative enforcement. Accessibility collapse is high (0.7) because once content is understood to be non-notable, its path to inclusion is largely closed. Resistance is moderate (0.3) from those whose content is excluded, but this is viewed as resistance to legitimate quality control.
 *
 * PERSPECTIVAL GAP:
 *   While this reading frames exclusion as a necessary function, other readings (inclusionist, deliberative) would highlight the costs borne by excluded content creators as a form of extraction or gatekeeping. The engine's per-seat classification would reflect this divergence, with the deletionist reading emphasizing the collective benefit over individual exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   The Wikipedia readership and editors are the primary beneficiaries, gaining a high-quality knowledge resource and a manageable editorial environment, respectively. Content creators whose submissions are rejected bear the 'cost' of exclusion, but this is framed as a necessary function of the system, not extraction. Spam/vanity promoters are excluded, their attempts to degrade the commons suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_objectivity_ambiguity,
    'Are the notability guidelines an objective measure of epistemic quality, or do they reflect implicit biases of the editing community?',
    'Empirical studies analyzing the demographic representation of notable topics and contributors, and the success rates of AfD nominations for content related to marginalized communities.',
    'If biases are significant, the ''just exclusion'' framing of this reading would be challenged, potentially reclassifying the constraint as more extractive for certain groups, or as a Tangled Rope due to asymmetric benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_objectivity_ambiguity, empirical, 'Assesses the objectivity vs. bias in notability criteria application.').

omega_variable(
    exclusion_as_extraction_ambiguity,
    'Is the exclusion of non-notable content a legitimate quality control function (as per this reading), or does it constitute a form of extraction from content creators whose labor is rejected?',
    'Conceptual analysis of ''extraction'' in digital commons, focusing on the value of rejected labor and the opportunity costs for creators. This would involve a re-evaluation of the normative boundary between ''just exclusion'' and ''unjust extraction''.',
    'If re-framed as extraction, the base_extractiveness metric would increase, potentially shifting the classification from Rope to Tangled Rope or Snare, depending on the severity and asymmetry of the ''cost''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusion_as_extraction_ambiguity, conceptual, 'Re-evaluates the normative status of content exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__deletionist_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__deletionist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__deletionist_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__deletionist_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__deletionist_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__deletionist_reading, base_extractiveness, 15, 0.14).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__deletionist_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__deletionist_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__deletionist_reading, suppression_requirement, 15, 0.24).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deletionist_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'notability_guidelines' kernel. Each reading represents a distinct structural claim about the guidelines' function and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
