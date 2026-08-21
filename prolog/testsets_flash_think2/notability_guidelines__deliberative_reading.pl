% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deliberative_reading, []).

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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: Wikipedia Notability Guidelines (Deliberative Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint represents the 'deliberative reading' of Wikipedia's
 *   'notability_guidelines' kernel. It frames notability as an emergent
 *   outcome of a perpetual negotiation process, primarily through Articles
 *   for Deletion (AfD) discussions. The constraint itself is the structured
 *   process of community deliberation, which evolves the boundaries of what
 *   constitutes 'notable' encyclopedic content. This reading emphasizes the
 *   dynamic, consensus-driven nature of knowledge governance in a digital
 *   commons, viewing the process as a Scaffold supporting the growth and
 *   quality of the encyclopedia.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.4).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.3).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "Wikipedia Notability Guidelines (Deliberative Reading)").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, 'ea4735eb-a654-4d60-a638-0ab11ba34525').
narrative_ontology:cs_kernel_codification('ea4735eb-a654-4d60-a638-0ab11ba34525', formalized).
narrative_ontology:cs_authority_grounding('ea4735eb-a654-4d60-a638-0ab11ba34525', practice).
narrative_ontology:cs_interpretation_layer_present('ea4735eb-a654-4d60-a638-0ab11ba34525').
narrative_ontology:cs_reading_relation('ea4735eb-a654-4d60-a638-0ab11ba34525', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea4735eb-a654-4d60-a638-0ab11ba34525', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('ea4735eb-a654-4d60-a638-0ab11ba34525', foundational, notability_is_emergent_property).
narrative_ontology:cs_axiom_status(notability_is_emergent_property, holdable).
narrative_ontology:cs_axiom_grounding('ea4735eb-a654-4d60-a638-0ab11ba34525', notability_is_emergent_property, conventional).
narrative_ontology:cs_axiom('ea4735eb-a654-4d60-a638-0ab11ba34525', foundational, deliberation_as_legitimacy_source).
narrative_ontology:cs_axiom_status(deliberation_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('ea4735eb-a654-4d60-a638-0ab11ba34525', deliberation_as_legitimacy_source, conventional).
narrative_ontology:cs_reference_frame('ea4735eb-a654-4d60-a638-0ab11ba34525', evolving_consensus_governance).
narrative_ontology:cs_drift_state('ea4735eb-a654-4d60-a638-0ab11ba34525', contemporary_wikipedia_governance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ea4735eb-a654-4d60-a638-0ab11ba34525', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_editors).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_readers).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, proponents_of_marginal_topics).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, editors_whose_work_is_deleted).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, deletionist_editors).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, inclusionist_editors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively participate in and moderate Articles for Deletion (AfD) discussions, shaping the notability criteria through consensus. They invest significant time and effort in this process, benefiting from a high-quality, well-defined encyclopedia.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_editors, agenda_setter,
    organized, biographical, constrained, global).

% Benefit from a curated, reliable, and encyclopedic body of knowledge. They are largely unaware of the underlying deliberative processes but rely on their outcomes for content quality.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_readers, beneficiary,
    moderate, immediate, mobile, global).

% Bear the cost of having their proposed topics or contributions subjected to rigorous debate and potential deletion if they do not meet the evolving notability consensus. Their efforts to include content may be rejected, leading to frustration and disengagement.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, proponents_of_marginal_topics, payer,
    powerless, biographical, constrained, global).

% Advocate for stricter notability standards and the deletion of content deemed non-notable. They invest considerable effort in AfD debates to maintain quality and scope, seeing themselves as guardians of the encyclopedia's integrity.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, deletionist_editors, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, deletionist_editors, payer).

% Advocate for broader notability standards and the retention of content, often investing effort to defend articles in AfD. They bear the cost of defending content against deletionist arguments and the emotional labor of contentious debates.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, inclusionist_editors, payer,
    organized, biographical, constrained, global).

% Provides the platform and high-level policy framework for Wikipedia, but largely defers to the community's self-governance processes like AfD for content decisions. They observe the process and intervene only in extreme cases or for platform-wide policy changes.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_foundation, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deliberative_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deliberative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective effort of a global volunteer community to define and maintain the scope, quality, and encyclopedic nature of content, preventing both indiscriminate inclusion and arbitrary deletion through a structured deliberative process.
% TRANSFER_FUNCTION: Transfers editorial labor, attention, and emotional investment towards consensus-building and content refinement, and away from individual arbitrary decisions. It also transfers the burden of demonstrating notability onto those proposing content for inclusion.
% ABSENT_VOICES: New or less experienced editors who are intimidated by the complexity and intensity of AfD debates; communities whose knowledge or perspectives are systematically undervalued by the prevailing notability criteria, leading to their exclusion from the conversation.
% DISAPPEARANCE_RATIONALE: If the deliberative process for notability vanished overnight, Wikipedia would rapidly descend into chaos, either becoming an uncurated repository of all information (losing its encyclopedic character) or ossifying under rigid, unchallengeable rules. The dynamic balance between inclusion and quality, essential for its growth and legitimacy, would be lost, leading to a significant reorganization of content, community engagement, and potentially the platform's very purpose.
% FOUNDING_PROBLEM: How to scale a volunteer-driven, open-access encyclopedia while maintaining quality, preventing spam, vandalism, and content lacking encyclopedic merit, without resorting to centralized editorial control or rigid, top-down rules.
% FOUNDING_PROBLEM_CORROBORATION: The Wikipedia Foundation's ongoing policy discussions, academic studies of online communities and knowledge governance, and the continuous, active operation of the Articles for Deletion process itself corroborate the persistent challenge of balancing growth, quality, and community self-governance in a dynamic digital commons. Independent researchers and former editors often highlight the necessity of such processes for Wikipedia's long-term viability.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deliberative_reading_tests).
:- end_tests(notability_guidelines__deliberative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Scaffold because its primary justification is to provide temporary, evolving support for the growth and quality of Wikipedia's knowledge base, rather than establishing a fixed, steady state. Extraction (0.4) is moderate, reflecting the significant time, effort, and emotional labor invested by editors in the deliberative process. Suppression (0.3) is low, as the process is open to all editors, though it does suppress arbitrary inclusion/deletion. Theater ratio (0.1) is low, indicating the process is largely functional and not performative. The 'has_sunset_clause' is interpreted as the process being transitional until a stable, high-quality encyclopedia is achieved, or a new governance model emerges, even if that 'sunset' is distant. The measurement series show a slight increase in extractiveness and suppression over time, reflecting the growing complexity of content governance as Wikipedia matures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this deliberative reading, the process is a necessary and legitimate coordination mechanism. However, from the 'deletionist_reading' perspective, it might be seen as too lenient or prone to 'scope creep,' while from the 'inclusionist_reading' perspective, it might be seen as an overly burdensome or exclusionary gatekeeping mechanism. The engine's per-seat classification will highlight these divergences based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Wikipedia editors (both general and factional) are agenda-setters and beneficiaries, as they actively shape the process and benefit from a high-quality encyclopedia, but also bear costs in terms of effort. Wikipedia readers are beneficiaries of the quality content. Proponents of marginal topics and editors whose work is deleted are victims, as their contributions may be rejected or removed. The Wikipedia Foundation acts as an observer, providing the platform but largely deferring to community governance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_emergent_vs_biased,
    'Is notability truly an emergent property of community deliberation, or is the deliberative process implicitly guided by pre-existing biases or power dynamics within the editor community?',
    'Sociological analysis of AfD outcomes over time, correlating with editor demographics, topic areas, and power structures; content analysis of arguments for implicit bias.',
    'If implicitly biased, the constraint''s effective suppression and extractiveness would be higher for marginalized groups than currently measured, potentially reclassifying it closer to a Tangled Rope or Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_emergent_vs_biased, conceptual, 'Ambiguity regarding the true source of notability criteria: genuine emergence vs. implicit bias.').

omega_variable(
    scaffold_vs_permanent_governance,
    'Is the deliberative notability process genuinely a temporary Scaffold supporting an evolving knowledge base, or has it become a permanent, self-sustaining governance mechanism (more akin to a Rope or even Tangled Rope) that has lost its transitional character?',
    'Longitudinal study of the ''sunset'' conditions: if the process shows no signs of winding down or being replaced by a more stable state after decades, and its justification shifts from ''transition'' to ''steady-state governance,'' reclassify.',
    'If it has become a permanent governance mechanism, its classification would shift from Scaffold to Rope (if coordination remains primary) or Tangled Rope (if extraction becomes significant), indicating a drift in its fundamental purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_vs_permanent_governance, empirical, 'Whether the ''Scaffold'' nature of the deliberative process is genuinely temporary or has become permanent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__deliberative_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__deliberative_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__deliberative_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__deliberative_reading, base_extractiveness, 5, 0.37).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__deliberative_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__deliberative_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__deliberative_reading, suppression_requirement, 5, 0.27).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__deliberative_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__deliberative_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
