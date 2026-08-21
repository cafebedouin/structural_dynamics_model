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
 *   This constraint story instantiates the 'deletionist reading' of
 *   Wikipedia's Notability Guidelines (WP:N). From this perspective, WP:N
 *   functions as a necessary epistemic quality filter, preventing the
 *   degradation of the digital commons by ensuring content is verifiable,
 *   relevant, and encyclopedic. It is viewed as a coordination mechanism
 *   (Rope) that benefits the readership and the project's long-term integrity
 *   by excluding non-notable content, which is not seen as extraction from
 *   legitimate contributors but as a just exclusion of spam/vanity.
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
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'd03ffdab-e50e-4754-92fc-f00f16430627').
narrative_ontology:cs_kernel_codification('d03ffdab-e50e-4754-92fc-f00f16430627', formalized).
narrative_ontology:cs_authority_grounding('d03ffdab-e50e-4754-92fc-f00f16430627', practice).
narrative_ontology:cs_interpretation_layer_present('d03ffdab-e50e-4754-92fc-f00f16430627').
narrative_ontology:cs_reading_relation('d03ffdab-e50e-4754-92fc-f00f16430627', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d03ffdab-e50e-4754-92fc-f00f16430627', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('d03ffdab-e50e-4754-92fc-f00f16430627', foundational, quality_preservation_is_paramount).
narrative_ontology:cs_axiom_status(quality_preservation_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('d03ffdab-e50e-4754-92fc-f00f16430627', quality_preservation_is_paramount, deontological).
narrative_ontology:cs_axiom('d03ffdab-e50e-4754-92fc-f00f16430627', foundational, notability_is_objective_filter).
narrative_ontology:cs_axiom_status(notability_is_objective_filter, holdable).
narrative_ontology:cs_axiom_grounding('d03ffdab-e50e-4754-92fc-f00f16430627', notability_is_objective_filter, empirically_contingent).
narrative_ontology:cs_reference_frame('d03ffdab-e50e-4754-92fc-f00f16430627', encyclopedic_quality_standard).
narrative_ontology:cs_drift_state('d03ffdab-e50e-4754-92fc-f00f16430627', contemporary_digital_commons_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d03ffdab-e50e-4754-92fc-f00f16430627', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_editors_deletionist).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, new_content_creators).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, wikipedia_editors_inclusionist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a high-quality, verifiable encyclopedia free from spam, vanity pages, and trivial content. Relies on the notability guidelines to ensure content relevance and reliability.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_readership, beneficiary,
    moderate, generational, mobile, global).

% Actively enforce WP:N, initiating deletion discussions (AfD) for articles deemed not notable. They see themselves as guardians of Wikipedia's quality and integrity, preventing degradation of the commons. Their efforts maintain the constraint.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_editors_deletionist, agenda_setter,
    organized, biographical, constrained, global).

% Experience their contributions (new articles, significant expansions) being challenged or deleted if they do not meet the strict notability criteria. They bear the cost of effort expended on non-notable content.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, new_content_creators, payer,
    powerless, immediate, constrained, global).

% Are systematically prevented from publishing self-promotional or trivial content. Their attempts are quickly identified and removed, making the platform an unsuitable venue for their goals.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, spam_and_vanity_publishers, excluded,
    powerless, immediate, trapped, global).

% Often find themselves in opposition to deletionist editors, arguing for broader interpretations of notability or for the value of content that might not meet strict criteria. They bear the social cost of frequent disagreements and the effort of defending articles.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_editors_inclusionist, payer,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective effort of editors to maintain a consistent standard of encyclopedic quality and relevance, preventing the degradation of the digital commons by irrelevant or unverifiable content.
% TRANSFER_FUNCTION: Transfers editorial effort from defending and maintaining low-quality or non-notable content towards improving and expanding high-quality, notable content. It also transfers trust and authority to the deletionist editors who enforce the guidelines.
% ABSENT_VOICES: Individuals or groups whose knowledge or subjects are deemed 'not notable' by the current guidelines are effectively silenced or marginalized. They would argue for a more inclusive definition of notability that reflects diverse forms of knowledge and cultural significance.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, Wikipedia would rapidly fill with self-promotional, trivial, and unverifiable content, leading to a severe degradation of its quality and reputation. The platform's utility as a reliable knowledge source would collapse, and its readership would disperse.
% FOUNDING_PROBLEM: The early internet's open platforms struggled with content quality, spam, and the challenge of maintaining a coherent knowledge base without clear editorial standards.
% FOUNDING_PROBLEM_CORROBORATION: The problem of maintaining content quality on an open platform remains live, as attested by ongoing challenges with misinformation and spam across the internet. Independent academic studies on digital commons governance and content moderation corroborate the necessity of such filters, even if their specific application is debated.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.15) because the guidelines are seen as a legitimate cost of maintaining quality, not as rent-seeking. Suppression (0.25) is moderate, reflecting the active enforcement required to filter out non-notable content, but it's considered a necessary function. Theater ratio is low (0.1) as the primary activity is genuinely about content quality, not performative. Accessibility collapse is high (0.7) because alternatives for publishing non-notable content on Wikipedia are effectively closed off. Resistance (0.3) is moderate, primarily from new content creators and inclusionist editors who contest specific applications, but not the principle itself from this reading's perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the deletionist perspective, the constraint is a Rope, a beneficial coordination mechanism. However, from the perspective of new content creators or inclusionist editors, the same guidelines might feel more extractive or suppressive, potentially computing as a Tangled Rope or even a Snare due to perceived gatekeeping. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Wikipedia readership and deletionist editors are primary beneficiaries (d near 0.0) as they gain from content quality and the efficient functioning of the encyclopedia. New content creators and inclusionist editors are payers (d near 1.0) as they bear the costs of navigating or contesting the guidelines. Spam and vanity publishers are excluded, experiencing full suppression (d=1.0) as the constraint is designed to target them.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling essential quality control as extraction. By framing WP:N as a necessary filter, it highlights the ongoing problem of commons degradation that the guidelines address, suggesting the mandate is still live. The low theater ratio indicates that the constraint's function has not atrophied into mere performance; it actively serves its stated purpose of quality preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_definition_ambiguity,
    'Is the definition of ''notability'' sufficiently objective and universally applicable, or does it inherently reflect biases that systematically exclude certain forms of knowledge or cultural contexts?',
    'Empirical analysis of deletion patterns across diverse cultural and subject domains, coupled with qualitative studies of editor biases in AfD discussions. A finding of systematic, unacknowledged bias would support the ''inclusionist reading''.',
    'If notability is found to be systematically biased, the constraint''s effective extractiveness and suppression would be higher for marginalized knowledge, potentially reclassifying it as a Tangled Rope or Snare from those perspectives. This would challenge the ''just exclusion'' premise of the deletionist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_definition_ambiguity, empirical, 'Ambiguity in the objectivity and universality of ''notability'' criteria.').

omega_variable(
    deletionist_vs_inclusionist_framing,
    'Is the primary function of WP:N to preserve quality (deletionist reading) or to act as a gatekeeping mechanism (inclusionist reading)?',
    'Analysis of editor motivations and community discourse over time, particularly in contentious AfD discussions. If the stated quality rationale consistently serves to justify the exclusion of legitimate, albeit non-mainstream, content, the inclusionist framing gains strength.',
    'If the inclusionist framing is validated, the constraint''s ''beneficiary'' status for the readership might be re-evaluated, and the ''victims'' set would expand to include legitimate contributors of marginalized knowledge, shifting the classification towards a more extractive type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deletionist_vs_inclusionist_framing, conceptual, 'Contest over the fundamental purpose of notability guidelines.').


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
% This constraint is one reading of the 'notability_guidelines' kernel. Its sibling readings are 'inclusionist_reading' and 'deliberative_reading', which offer alternative interpretations of the same underlying policy framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
