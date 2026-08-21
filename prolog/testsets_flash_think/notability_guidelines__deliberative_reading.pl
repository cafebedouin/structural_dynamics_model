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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: Wikipedia Notability Guidelines (Deliberative Reading)
 *   domain: digital_commons_governance
 *
 * SUMMARY:
 *   This constraint models Wikipedia's notability guidelines as a perpetual
 *   negotiation process, where the boundaries of what constitutes 'notable'
 *   content evolve through community deliberation, primarily via the Articles
 *   for Deletion (AfD) process. This reading frames notability as an output
 *   of this process, rather than a fixed input. It is a 'governance scaffold'
 *   supporting the ongoing, dynamic self-regulation of a digital commons. The
 *   claimed type is 'scaffold' as per the prompt's instruction, despite the
 *   'perpetual' nature of the process, which is addressed in an omega
 *   variable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.35).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.4).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "Wikipedia Notability Guidelines (Deliberative Reading)").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, '198f8de1-efb1-468c-b1c5-8287b1683402').
narrative_ontology:cs_kernel_codification('198f8de1-efb1-468c-b1c5-8287b1683402', formalized).
narrative_ontology:cs_authority_grounding('198f8de1-efb1-468c-b1c5-8287b1683402', practice).
narrative_ontology:cs_interpretation_layer_present('198f8de1-efb1-468c-b1c5-8287b1683402').
narrative_ontology:cs_reading_relation('198f8de1-efb1-468c-b1c5-8287b1683402', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('198f8de1-efb1-468c-b1c5-8287b1683402', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('198f8de1-efb1-468c-b1c5-8287b1683402', foundational, notability_is_process_output).
narrative_ontology:cs_axiom_status(notability_is_process_output, holdable).
narrative_ontology:cs_axiom_grounding('198f8de1-efb1-468c-b1c5-8287b1683402', notability_is_process_output, conventional).
narrative_ontology:cs_axiom('198f8de1-efb1-468c-b1c5-8287b1683402', foundational, boundary_negotiation_is_mechanism).
narrative_ontology:cs_axiom_status(boundary_negotiation_is_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('198f8de1-efb1-468c-b1c5-8287b1683402', boundary_negotiation_is_mechanism, conventional).
narrative_ontology:cs_reference_frame('198f8de1-efb1-468c-b1c5-8287b1683402', community_consensus_through_deliberation).
narrative_ontology:cs_drift_state('198f8de1-efb1-468c-b1c5-8287b1683402', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('198f8de1-efb1-468c-b1c5-8287b1683402', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_community).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, knowledge_seekers).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, inclusion_advocates_seeking_broad_coverage).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, content_creators_with_marginal_notability).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, deletion_advocates_seeking_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of editors and contributors who participate in the Arbitration for Deletion (AfD) process, shaping the evolving definition of notability. They benefit from a well-curated encyclopedia but invest significant time and effort in deliberation.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_community, agenda_setter,
    organized, generational, constrained, global).

% Authors of articles whose notability is contested. They bear the cost of defending their content in AfD, potentially seeing their work deleted, or investing time to meet evolving standards. Their exit options are to leave Wikipedia or accept the community's judgment.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, content_creators_with_marginal_notability, payer,
    moderate, biographical, constrained, global).

% Users who rely on Wikipedia for reliable, curated information. They benefit from the notability guidelines ensuring quality and relevance, without directly participating in the governance process. Their cost is the occasional absence of niche content.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, knowledge_seekers, beneficiary,
    powerless, immediate, mobile, global).

% Editors who argue for the deletion of content they deem non-notable, often seeking to maintain strict quality control or reduce cruft. They invest time in AfD discussions, and their 'cost' is the frustration when content they believe should be deleted is kept due to community consensus.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, deletion_advocates_seeking_finality, payer,
    moderate, biographical, constrained, global).

% Editors who argue for the inclusion of content, often emphasizing the value of broad coverage or the potential for future notability. They benefit when their arguments sway consensus, but invest time in the deliberative process.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, inclusion_advocates_seeking_broad_coverage, beneficiary,
    moderate, biographical, constrained, global).

% The core group of administrators and experienced editors who facilitate AfD discussions, ensure adherence to process, and ultimately close discussions based on consensus. They are deeply invested in the integrity of Wikipedia's governance.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_editors_and_moderators, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars who study Wikipedia's governance, community dynamics, and content quality. They analyze the notability guidelines and AfD process as a case study in digital commons management, without direct participation or stake in specific outcomes.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, external_researchers_and_academics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deliberative_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deliberative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Wikipedia community's collective judgment on what constitutes encyclopedic notability, preventing arbitrary content inclusion or deletion and ensuring a consistent quality standard through a deliberative process.
% TRANSFER_FUNCTION: Transfers community time, effort, and social capital into deliberative consensus, resulting in decisions about content inclusion or exclusion. It also transfers authority over notability from individual editors to the collective AfD process.
% ABSENT_VOICES: New or less experienced editors who find the AfD process opaque or intimidating; individuals whose knowledge domains are systematically undervalued by the current community consensus; those who advocate for a purely algorithmic or expert-driven notability determination, who are often marginalized in the community's self-governance.
% DISAPPEARANCE_RATIONALE: If the AfD process and its underlying notability guidelines vanished overnight, Wikipedia's content quality would rapidly degrade. Without a structured mechanism for collective judgment, standards would diverge, leading to either an unmanageable flood of non-notable content or arbitrary deletion by individual editors, fundamentally undermining its reliability and community governance model.
% FOUNDING_PROBLEM: How to maintain a consistent standard of encyclopedic quality and relevance in a collaboratively edited, open-access knowledge base, preventing both spam and arbitrary censorship while fostering community participation.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing volume of AfD discussions, the continuous evolution of notability interpretations, and the persistent debates over content inclusion/exclusion attest to the problem's enduring nature. External studies of Wikipedia's governance and content quality also corroborate the need for such a mechanism to manage a large-scale digital commons.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.35) is moderate, reflecting the significant time and effort extracted from participants in the deliberative process, whether their content is kept or deleted. Suppression (0.40) is also moderate, as the process suppresses arbitrary individual actions (inclusion or deletion) in favor of community consensus and adherence to guidelines. Theater ratio (0.20) is low, indicating that the deliberation is generally genuine, though occasional strategic voting or entrenched positions can introduce performative elements. The measurement series show a slight increase in extractiveness and suppression as the community grows and the process becomes more formalized, but a stable theater ratio, suggesting the core deliberative function remains intact.
 *
 * PERSPECTIVAL GAP:
 *   Different stakeholders experience this constraint differently. For the Wikipedia community and knowledge seekers, it functions as a beneficial coordination mechanism for quality control. For content creators and deletion advocates, it can be a costly and frustrating process where their efforts may be 'extracted' without achieving their desired outcome. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Wikipedia community and its moderators act as agenda-setters and beneficiaries, guiding the process and benefiting from a well-maintained encyclopedia. Knowledge seekers are clear beneficiaries. Content creators and deletion advocates are payers, investing time and effort with uncertain outcomes. Inclusion advocates are beneficiaries when their efforts succeed. The process itself is the mechanism for defining notability, making it a collective endeavor with distributed costs and benefits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_scaffold_sunset_ambiguity,
    'Can a ''governance scaffold'' for a ''perpetual negotiation process'' genuinely carry a sunset clause, or does this imply a different constraint type?',
    'Conceptual analysis of ''scaffold'' definition in the context of ongoing governance. If the ''scaffold'' refers to a transitional phase of governance towards a more automated or stable system, the sunset clause is valid. If it implies the process itself is temporary, it contradicts ''perpetual''.',
    'If the ''perpetual'' aspect dominates and no plausible sunset is identified, the constraint might reclassify as a ''rope'' or ''tangled_rope'' (ongoing coordination/extraction) rather than a ''scaffold'' (temporary support).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_scaffold_sunset_ambiguity, conceptual, 'Ambiguity regarding the ''scaffold'' classification for a perpetual governance process.').

omega_variable(
    deliberation_quality_drift,
    'Is the AfD process genuinely deliberative, or does it frequently devolve into voting blocs, power plays, or ''wikilawyering'' that undermines its stated function?',
    'Empirical analysis of AfD discussions over time, coding for adherence to deliberative norms, prevalence of logical fallacies, and impact of editor seniority/social capital on outcomes.',
    'If deliberation quality is low, the ''theater_ratio'' would be higher, and the ''extractiveness'' from participants (whose efforts are wasted) would increase, potentially shifting the classification towards a ''snare'' or ''piton'' if the coordination function becomes purely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_quality_drift, empirical, 'Assessing the actual quality of deliberation in the AfD process.').

omega_variable(
    notability_boundary_objectivity,
    'To what extent is notability an objective property of a topic, versus a purely social construct of the Wikipedia community?',
    'Philosophical and sociological analysis of knowledge construction in digital commons, comparing Wikipedia''s notability criteria to external academic or journalistic standards.',
    'If notability is purely a social construct, the constraint''s ''emerges_naturally'' would remain false, but the ''suppression'' of external criteria would be higher. If there''s an objective component, the community''s process is a ''discovery'' mechanism, not pure construction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(notability_boundary_objectivity, conceptual, 'The ontological status of ''notability'' as defined by Wikipedia.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''deliberative_reading'' of the ''notability_guidelines'' kernel. What are the structural implications of this specific framing?',
    'Comparison with sibling readings (''deletionist_reading'', ''inclusionist_reading'') to highlight how this reading''s emphasis on process and negotiation shapes its metrics and stakeholder dynamics.',
    'This reading emphasizes coordination and ongoing adaptation. A ''deletionist_reading'' would likely show higher suppression and extractiveness, while an ''inclusionist_reading'' might show lower suppression but higher accessibility collapse if it led to an unmanageable commons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifying the specific kernel reading and its structural implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__deliberative_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__deliberative_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__deliberative_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__deliberative_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__deliberative_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__deliberative_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__deliberative_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__deliberative_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__deliberative_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
