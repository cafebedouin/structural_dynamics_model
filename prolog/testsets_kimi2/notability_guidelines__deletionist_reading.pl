% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: WP:N as Necessary Epistemic Quality Filter (Deletionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint story instantiates the deletionist reading of the
 *   Wikipedia Notability Guidelines (WP:N) kernel. Under this reading, WP:N
 *   is a genuine coordination mechanismâa ropeâthat prevents epistemic
 *   commons degradation by filtering non-notable, promotional, and vanity
 *   content. The primary beneficiary is the global readership, which receives
 *   a curated, high-reliability reference work. There is no victim set
 *   because excluded content is defined as spam or vanity that legitimately
 *   fails objective significance thresholds. The constraint is actively
 *   enforced through deletion discussions and new-page patrol, but its
 *   extraction is minimal and justified as the necessary cost of
 *   coordination. Sibling readings (inclusionist, deliberative) treat the
 *   same kernel as extraction or perpetual negotiation, respectively; they
 *   are modeled as separate constraints.
 *
 * KEY AGENTS:
 *   - Global readership (beneficiary/organized/mobile): receives quality-controlled encyclopedic content; low directionality.
 *   - Wikipedia editor community (agenda-setter/organized/mobile): authors and enforces the guideline; moderate-low directionality as administrators.
 *   - Spam/vanity contributors (excluded/powerless/mobile): attempt non-compliant contributions; excluded from the consensus but not structurally trapped.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.2).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.28).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "WP:N as Necessary Epistemic Quality Filter (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '9e682242-bef0-496e-9438-e5cf1ebcf1fc').
narrative_ontology:cs_kernel_codification('9e682242-bef0-496e-9438-e5cf1ebcf1fc', fixed_text).
narrative_ontology:cs_authority_grounding('9e682242-bef0-496e-9438-e5cf1ebcf1fc', practice).
narrative_ontology:cs_interpretation_layer_present('9e682242-bef0-496e-9438-e5cf1ebcf1fc').
narrative_ontology:cs_reading_relation('9e682242-bef0-496e-9438-e5cf1ebcf1fc', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e682242-bef0-496e-9438-e5cf1ebcf1fc', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('9e682242-bef0-496e-9438-e5cf1ebcf1fc', foundational, commons_degradation_without_filter).
narrative_ontology:cs_axiom_status(commons_degradation_without_filter, holdable).
narrative_ontology:cs_axiom_grounding('9e682242-bef0-496e-9438-e5cf1ebcf1fc', commons_degradation_without_filter, empirically_contingent).
narrative_ontology:cs_axiom('9e682242-bef0-496e-9438-e5cf1ebcf1fc', foundational, significance_threshold_as_quality_gate).
narrative_ontology:cs_axiom_status(significance_threshold_as_quality_gate, holdable).
narrative_ontology:cs_axiom_grounding('9e682242-bef0-496e-9438-e5cf1ebcf1fc', significance_threshold_as_quality_gate, conventional).
narrative_ontology:cs_reference_frame('9e682242-bef0-496e-9438-e5cf1ebcf1fc', stable_quality_filter).
narrative_ontology:cs_drift_state('9e682242-bef0-496e-9438-e5cf1ebcf1fc', contemporary_inclusionist_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e682242-bef0-496e-9438-e5cf1ebcf1fc', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, global_readership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses Wikipedia as a reference work and benefits from its curation: spam, vanity, and non-notable topics are filtered out, preserving a high signal-to-noise ratio. They could use other information sources but none replicate the same scale and open-access model.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, global_readership, beneficiary,
    organized, generational, mobile, global).

% Volunteer editors who write, interpret, and enforce the notability guideline. They propose and debate refinements, patrol new pages, and initiate deletion discussions. Their exit looks like leaving the project or founding a fork, but their identity and effort are embedded in the existing encyclopedia.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_editor_community, agenda_setter,
    organized, generational, mobile, global).

% Attempt to add promotional, self-serving, or non-significant content. Their edits are reverted and articles deleted under the notability standard. They are not party to the editorial consensus and are encouraged to use personal blogs or other platforms.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, spam_vanity_contributors, excluded,
    powerless, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents tragedy-of-the-commons degradation in an open collaborative encyclopedia by filtering out promotional, non-verifiable, and trivial content before it accumulates, thereby preserving reader trust and finite volunteer editorial capacity.
% TRANSFER_FUNCTION: Moves editorial attention and epistemic trust away from low-quality or self-interested contributors toward the general readership, by enforcing an objective significance threshold that excludes off-mission content.
% ABSENT_VOICES: Inclusionist advocates and marginalized-knowledge communities argue that notability criteria encode mainstream-media and academic-prestige biases, systematically excluding legitimate topics from the global South, indigenous knowledge, and non-prominent subjects. They are present in policy debates but hold minority influence under the deletionist framing.
% DISAPPEARANCE_RATIONALE: If the notability filter vanished overnight, the encyclopedia would accumulate promotional biographies, product advertisements, and vanity pages at a rate that would overwhelm volunteer patrols, degrade reader trust, and shift the project's character toward a directory or social-media platform.
% FOUNDING_PROBLEM: Open collaborative encyclopedias face a tragedy of the commons in which unlimited self-promotion, spam, and low-stakes trivia overwhelm finite volunteer editorial capacity and erode reader trust in the epistemic reliability of the work.
% FOUNDING_PROBLEM_CORROBORATION: Independent peer-production researchers and digital-commons scholars (e.g., Benjamin Mako Hill, Aaron Shaw) attest the ongoing risk of commons degradation in open collaboration systems; their work originates outside the direct institutional interest of the Wikipedia editor community that administers the guideline.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.2, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is low (0.20 at interval end) because the deletionist reading treats the guideline as solving a genuine coordination problem with minimal overhead. Suppression is low-moderate (0.28) because active enforcement (deletion, patrol) is present but is not coercive extractionâit is the operational cost of the filter. Theater ratio is very low (0.10) because enforcement is functionally oriented toward quality control rather than performative display. Accessibility collapse is moderate (0.60): once the notability standard is understood, alternatives (creating non-notable articles) collapse because they are deleted, but this is a feature of the coordination mechanism, not a trap. Resistance is low (0.15) because the only sustained opposition comes from spammers and marginal vanity editors, not from coordinated legitimate stakeholders.
 *
 * PERSPECTIVAL GAP:
 *   The deletionist seat computes a rope because the structural data show a symmetric coordination benefit to readership and low extraction. The inclusionist seat on the same kernel would compute a snare or tangled_rope: it would declare marginalized knowledge producers as victims, identify asymmetric extraction through prestige-biased source requirements, and show higher suppression of legitimate alternatives. The deliberative seat might compute a rope or scaffold depending on whether it treats AfD as ongoing coordination or a transitional deliberative mechanism. The engine's per-seat classification captures this divergence without requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Global readership is the declared beneficiary (low d, approaching 0.0) because the constraint subsidizes their access to quality information. The editor community, as agenda-setter, sits at low-moderate d (~0.25): they administer the constraint and absorb coordination costs (time spent on deletion discussions) but are not its targets. Spam/vanity contributors are excluded and bear the cost of deletion, but their exit options are mobile (other platforms, personal blogs) and their power is low; structurally they are not locked targets. Because no victims are declared and no identity-locked agents are present, the derivation chain does not produce high effective extraction for any seat.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared because the founding problemâcommons degradation under open contributionâremains live. Spam and promotional pressure have not disappeared; if anything, they have intensified with the growth of the platform. The constraint is not a piton because its primary function is still performed (quality filtering) and its theater ratio is negligible. The agenda-setting editor community continues to benefit from the constraint's operation, distinguishing it from an atrophied structure maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deletionist_inclusionist_framing,
    'Does the notability guideline function as a genuine quality filter preventing commons degradation, or as a structural gatekeeping apparatus that encodes existing epistemic power and excludes marginalized knowledge?',
    'Cross-reading empirical analysis: compare topic-coverage demographics, source-type distributions, and deletion outcomes against independently verified significance metrics; test whether exclusion correlates with topic marginality or merely with promotional intent.',
    'If the gatekeeping framing is borne out, the victim set must expand to include marginalized knowledge producers, the extraction profile rises, and the constraint reclassifies toward tangled_rope or snare under the inclusionist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deletionist_inclusionist_framing, conceptual, 'Contested kernel framing between quality-filter and gatekeeping readings').

omega_variable(
    spam_marginalized_boundary,
    'Does the guideline''s application in practice consistently distinguish spam and vanity from legitimately marginalized but verifiable knowledge, or does it conflate low-coverage topics with low-quality topics?',
    'Quantitative audit of deletion outcomes by subject area, geographic origin of topic, and availability of high-prestige sources; compare with a counterfactual significance metric independent of mainstream media coverage.',
    'If the boundary is systematically blurred, the deletionist reading''s ''no victim'' claim fails and the constraint must declare a victim set, altering directionality derivation and effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spam_marginalized_boundary, empirical, 'Empirical ambiguity between spam exclusion and marginalized-knowledge exclusion').

omega_variable(
    coordination_cost_or_extraction,
    'Is the editorial labor consumed by deletion discussions and notability enforcement a necessary transaction cost of quality coordination, or does it represent extractive overhead that displaces productive content creation?',
    'Measure the ratio of editorial hours spent on deletion enforcement versus article improvement; compare with open-collaboration projects that lack notability filters.',
    'If enforcement overhead dominates content work, the base extractiveness of the deletionist reading is understated and the constraint edges toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_or_extraction, empirical, 'Whether enforcement labor is coordination cost or extractive overhead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__deletionist_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__deletionist_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__deletionist_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__deletionist_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__deletionist_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__deletionist_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__deletionist_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__deletionist_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__deletionist_reading, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deletionist_reading, suppression_requirement, 20, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.05).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the notability_guidelines constraint family. The kernel (WP:N) decomposes into three structurally distinct constraints under different readings: deletionist (rope), inclusionist (snare/tangled_rope), and deliberative (rope/scaffold). Each has its own epsilon, beneficiary/victim structure, and classification. They are linked because they compete to describe the same institutional arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
