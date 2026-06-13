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
    narrative_ontology:constraint_vindicates/2,
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
 *   This constraint represents the 'deletionist reading' of Wikipedia's
 *   Notability Guidelines (WP:N), which frames them as an essential epistemic
 *   quality filter. From this perspective, WP:N prevents the degradation of
 *   the digital commons by ensuring that only topics with significant
 *   coverage in reliable, independent sources are included. Content that
 *   fails WP:N is seen as 'spam' or 'vanity' that would dilute the
 *   encyclopedia's quality and verifiability. The constraint is claimed as a
 *   Rope because it is understood to coordinate editors towards a shared goal
 *   of quality, with minimal extraction from legitimate contributions.
 *
 * KEY AGENTS:
 *   - wikipedia_readership: Primary beneficiary (quality preservation)
 *   - deletionist_editors: Agenda-setter/beneficiary (enforce quality standards)
 *   - inclusionist_editors: Payer/excluded (bear costs of deletion, advocate for broader inclusion)
 *   - new_content_creators: Payer (face barriers to entry for non-notable topics)
 *   - marginalized_knowledge_advocates: Excluded (their topics often fail notability criteria)
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
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'a5e5c977-2441-4159-b3a4-df3932386bd6').
narrative_ontology:cs_kernel_codification('a5e5c977-2441-4159-b3a4-df3932386bd6', formalized).
narrative_ontology:cs_authority_grounding('a5e5c977-2441-4159-b3a4-df3932386bd6', practice).
narrative_ontology:cs_interpretation_layer_present('a5e5c977-2441-4159-b3a4-df3932386bd6').
narrative_ontology:cs_reading_relation('a5e5c977-2441-4159-b3a4-df3932386bd6', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5e5c977-2441-4159-b3a4-df3932386bd6', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('a5e5c977-2441-4159-b3a4-df3932386bd6', foundational, verifiability_is_paramount).
narrative_ontology:cs_axiom_status(verifiability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('a5e5c977-2441-4159-b3a4-df3932386bd6', verifiability_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('a5e5c977-2441-4159-b3a4-df3932386bd6', foundational, encyclopedic_scope_is_finite).
narrative_ontology:cs_axiom_status(encyclopedic_scope_is_finite, holdable).
narrative_ontology:cs_axiom_grounding('a5e5c977-2441-4159-b3a4-df3932386bd6', encyclopedic_scope_is_finite, conventional).
narrative_ontology:cs_reference_frame('a5e5c977-2441-4159-b3a4-df3932386bd6', wikipedia_as_reliable_encyclopedia).
narrative_ontology:cs_drift_state('a5e5c977-2441-4159-b3a4-df3932386bd6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a5e5c977-2441-4159-b3a4-df3932386bd6', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, deletionist_editors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, inclusionist_editors).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, new_content_creators).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, epistemic_quality_principle).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, information_signal_to_noise_ratio).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a high-quality, verifiable, and relevant encyclopedia. Their continued engagement is predicated on the perceived quality and trustworthiness of the content, which WP:N aims to ensure.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_readership, beneficiary,
    organized, generational, mobile, global).

% Actively enforce WP:N, participating in Articles for Deletion (AfD) discussions and nominating articles for removal. They see themselves as guardians of Wikipedia's quality and scope, ensuring adherence to core policies. Their influence is tied to their activity and persuasive power within the community.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, deletionist_editors, agenda_setter,
    powerful, biographical, constrained, global).

% Often advocate for broader inclusion criteria, arguing that WP:N can exclude legitimate topics, especially those from marginalized communities or non-traditional sources. They bear the cost of having their contributions or favored topics deleted, and the effort of defending them in AfD.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, inclusionist_editors, payer,
    moderate, biographical, constrained, global).

% Individuals attempting to add new articles, particularly on niche or emerging topics. They often struggle to understand and meet WP:N, leading to frustration, deletion of their work, or abandonment of their contributions. Their 'exit' is to stop contributing to Wikipedia.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, new_content_creators, payer,
    powerless, immediate, trapped, global).

% Groups or individuals seeking to document knowledge, histories, or perspectives that may not meet traditional notability criteria (e.g., oral histories, local community figures, non-Western scholarship). They are often excluded from the conversation about notability and find their contributions systematically rejected, leading to a feeling of systemic bias.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, marginalized_knowledge_advocates, excluded,
    powerless, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates editors and content creators towards a shared understanding of what constitutes an encyclopedic topic, ensuring a consistent scope and quality standard for Wikipedia's content.
% TRANSFER_FUNCTION: Transfers editorial effort from defending low-quality or non-notable content to focusing on improving notable articles. It also transfers the burden of proof for notability onto content creators.
% ABSENT_VOICES: Advocates for topics that consistently fail WP:N due to systemic biases in source availability (e.g., indigenous knowledge, local histories without mainstream media coverage). They are often not present in the AfD discussions that shape the interpretation of WP:N, or their arguments are dismissed as not meeting 'reliable source' criteria.
% DISAPPEARANCE_RATIONALE: If WP:N vanished, Wikipedia would rapidly fill with self-promotional, trivial, or unverified content, leading to a severe degradation of its quality and trustworthiness. The signal-to-noise ratio would collapse, and the encyclopedia's utility as a reliable information source would diminish, causing readers and editors to abandon it.
% FOUNDING_PROBLEM: The problem of maintaining encyclopedic quality and scope in an open-editing environment, preventing the inclusion of trivial, unverifiable, or promotional content that would dilute the project's mission.
% FOUNDING_PROBLEM_CORROBORATION: The Wikipedia Foundation and a large segment of the editor community (including deletionist editors) attest that the problem of maintaining quality and preventing spam is still live and ongoing. Independent academic studies on information quality in crowdsourced projects also corroborate the need for such filters, though they may critique the specific implementation or its biases.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).

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
 *   Extractiveness is low (0.15) because the guidelines primarily exclude content lacking verifiable sources, not to extract value from legitimate contributions. Suppression is moderate (0.25) as it requires active enforcement (deletion debates) to maintain standards, but alternatives for truly notable topics are not suppressed. Theater ratio is low (0.1) as the guidelines are genuinely applied to maintain quality, with little performative maintenance. Accessibility collapse is high (0.7) for non-notable topics, as the guidelines effectively close off that avenue for inclusion. Resistance is low (0.1) from the deletionist perspective, as resistance is seen as attempts to insert non-encyclopedic content.
 *
 * PERSPECTIVAL GAP:
 *   Deletionist editors experience this as a Rope, a necessary tool for quality. Inclusionist editors, however, may experience it as a Snare, as it systematically excludes certain types of knowledge or communities, imposing a cost on their contributions. New content creators may find it a significant barrier, feeling their contributions are 'extracted' by the effort required to meet standards or the frustration of deletion.
 *
 * DIRECTIONALITY LOGIC:
 *   The Wikipedia readership benefits from higher quality content (low d). Deletionist editors benefit from maintaining the encyclopedia's integrity (low d). New content creators and inclusionist editors bear the costs of content removal or exclusion (higher d). Marginalized knowledge advocates are structurally excluded, facing high d due to systemic barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   From the deletionist perspective, the mandate of WP:N (quality control) is very much live. The classification as a Rope reflects this. However, alternative readings (inclusionist, deliberative) suggest potential mandatrophy where the guidelines' function has shifted from pure quality control to a mechanism for gatekeeping or power negotiation. The omegas address this divergence, preventing mislabeling by acknowledging the contested nature of the constraint's function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine epistemic quality filter (deletionist reading), or a gatekeeping mechanism (inclusionist reading), or an evolving social process (deliberative reading)?',
    'Analysis of article deletion patterns over time, focusing on whether deleted content consistently lacks verifiable sources or if deletion correlates with topic marginalization. Longitudinal study of AfD outcomes and editor demographics.',
    'If the inclusionist reading is correct, the constraint''s extractiveness and suppression are significantly higher, reclassifying it as a Snare. If the deliberative reading is correct, the constraint is a Tangled Rope, reflecting ongoing negotiation and power dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the ''notability_guidelines'' kernel. This ''deletionist_reading'' emphasizes quality; sibling readings (''inclusionist_reading'', ''deliberative_reading'') emphasize gatekeeping or process.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of verifiable sources for certain topics) or internalized (editors self-censor due to fear of deletion)?',
    'Survey of editors who have had articles deleted or nominated for deletion, combined with content analysis of topics that consistently fail notability criteria. If editors avoid certain topics due to perceived deletion risk even when sources exist, internalized suppression is present.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as it shapes editor behavior beyond explicit rule enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for content creation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__deletionist_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__deletionist_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__deletionist_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__deletionist_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__deletionist_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__deletionist_reading, suppression_requirement, 10, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, wikipedia_verifiability_policy).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, wikipedia_neutral_point_of_view).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'notability_guidelines' kernel, alongside 'inclusionist_reading' and 'deliberative_reading'. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
