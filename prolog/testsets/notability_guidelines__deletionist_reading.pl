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
 *   domain: epistemic/informational
 *
 * SUMMARY:
 *   The Wikipedia Notability Guidelines embody a choice: preserve the
 *   encyclopedic commons from degradation by enforcing verifiable
 *   significance thresholds. Under the deletionist reading, the constraint is
 *   a protective coordination mechanism—a Rope that solves the
 *   collective-action problem of maintaining epistemic quality at scale. Spam
 *   and promotional insertion are genuine threats; notability prevents them
 *   justly by excluding unvetted content. The reading does not deny that
 *   marginalized knowledge sometimes fails to meet thresholds; it frames that
 *   as a cost of preventing commons degradation, not as systematic
 *   gatekeeping. This is one of three structurally distinct readings of the
 *   same kernel (notability_guidelines). The deliberative_reading treats
 *   notability as an evolving negotiation; the inclusionist_reading treats it
 *   as structural gatekeeping. These are NOT different perspectives on one
 *   constraint—they are three separate constraints with three different ε
 *   values, three different beneficiary structures, and three different
 *   structural dynamics. The deletionist reading is the one authored here.
 *
 * KEY AGENTS:
 *   - wikipedia_readership: organizers of demand for quality, beneficiaries of curation
 *   - subject_matter_experts: contributors who depend on quality preservation to justify their effort
 *   - wikipedia_editors: the enforcement layer, maintaining the boundary through AfD and deletion reviews
 *   - marginal_knowledge_creators: excluded from deliberation; would contest the exclusion but lack standing
 *   - spam_and_promotional_actors: structurally outside the scope of legitimate claim—properly excluded
 *   - verifiability_principle: the foundational normative commitment the constraint operationalizes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.18).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.22).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "epistemic/informational").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'b65e1d35-366b-423e-a27f-23349c61da90').
narrative_ontology:cs_kernel_codification('b65e1d35-366b-423e-a27f-23349c61da90', formalized).
narrative_ontology:cs_authority_grounding('b65e1d35-366b-423e-a27f-23349c61da90', expertise).
narrative_ontology:cs_interpretation_layer_present('b65e1d35-366b-423e-a27f-23349c61da90').
narrative_ontology:cs_reading_relation('b65e1d35-366b-423e-a27f-23349c61da90', notability_guidelines__inclusionist_reading, forecloses).
narrative_ontology:cs_reading_relation('b65e1d35-366b-423e-a27f-23349c61da90', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('b65e1d35-366b-423e-a27f-23349c61da90', foundational, verifiable_significance_distinguishes_legitimate_from_promotional).
narrative_ontology:cs_axiom_status(verifiable_significance_distinguishes_legitimate_from_promotional, holdable).
narrative_ontology:cs_axiom_grounding('b65e1d35-366b-423e-a27f-23349c61da90', verifiable_significance_distinguishes_legitimate_from_promotional, empirically_contingent).
narrative_ontology:cs_axiom('b65e1d35-366b-423e-a27f-23349c61da90', foundational, quality_preservation_justifies_exclusive_gatekeeping).
narrative_ontology:cs_axiom_status(quality_preservation_justifies_exclusive_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('b65e1d35-366b-423e-a27f-23349c61da90', quality_preservation_justifies_exclusive_gatekeeping, instrumental).
narrative_ontology:cs_reference_frame('b65e1d35-366b-423e-a27f-23349c61da90', verifiable_encyclopedic_quality).
narrative_ontology:cs_drift_state('b65e1d35-366b-423e-a27f-23349c61da90', contemporary_distributed_knowledge_production, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('b65e1d35-366b-423e-a27f-23349c61da90', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, quality_preservation_function).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
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
 *   Extractiveness is LOW (0.18 at interval end) because readership and experts genuinely benefit from quality preservation, and the constraint enforces a legitimate quality standard without asymmetric gain. Suppression is also LOW (0.22) because the constraint operates through transparent review (AfD) rather than covert exclusion—participants know why deletions occur and can contest them. Theater ratio is minimal (0.08) because the enforcement function (article deletion, vandalism reversal) performs real work; there is little performative activity. Accessibility collapse is HIGH (0.71) because once notability standards are understood, the alternatives (unvetted, promotional, or spam content) collapse completely—the only exit is acceptance of the standard or departure from the commons. Resistance is MODERATE (0.42) because the constraint meets genuine resistance from those excluded, but that resistance comes from outside the core beneficiary set (marginal knowledge creators, niche advocates) and is dwarfed by voluntary editor participation. The measurement series track a slight drift upward in extractiveness and suppression requirement over 24 units (representing roughly 2000–2020 Wikipedia history): as the baseline quality expectation increased and notability thresholds tightened incrementally, the constraint demanded slightly more enforcement effort and began excluding content that previously would have passed.
 *
 * PERSPECTIVAL GAP:
 *   The deletionist reading produces strong seat-level divergence. From the readership and editor perspective, the constraint is unambiguous Rope—coordination with minimal cost. From the marginal knowledge creator perspective (excluded from the analysis), the same constraint is a Snare—systematic gatekeeping that benefits legacy-institution-based knowledge at the expense of grassroots documentation. The engine computes per-seat types from the structural data; the deletionist reading's authored beneficiary set (readership, experts, verifiability principle) implicitly excludes the marginal creator seat, which is why the divergence arises. This is NOT a failure—it is how the kernel contest works: different readings name different beneficiary sets because they disagree about WHO RIGHTLY BENEFITS from notability. The reading_relations and axioms in cs_structure capture this structural disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the deletionist reading, Wikipedia readership is a near-pure beneficiary (d near 0.0): they collect reliability and quality without enforcement burden. Subject-matter experts are also beneficiaries (d~0.1): they contribute verified knowledge and their work is protected from being overwhelmed by spam. Wikipedia editors sit at d~0.3 (symmetric): they volunteer effort to maintain the boundary, but they do so from internalized commitment to the mission, not coercion. Marginal knowledge creators are structurally outside the beneficiary set (d undefined under this reading—they are marked as excluded, which means their position is NOT used in directionality derivation). This reading does not deny they pay a cost; it asserts that under notability standards, they are not a legitimate target of the constraint—they simply lack the documented significance that inclusion requires. Spam and promotional actors are not treated as a seat (they are marked with agent:false as structural outsiders, not rights-bearing participants).
 *
 * MANDATROPHY ANALYSIS:
 *   The deletionist reading does not invoke mandatrophy. The founding problem (spam and promotional degradation) remains live; the constraint continues to address it. The measurement series show slight tightening of suppression requirement and extractiveness over time, consistent with a constraint under incremental enforcement intensification as the baseline quality expectation rose—NOT consistent with atrophied function. The theater ratio remains minimal, so there is no performative drift that would signal theatricality. The constraint is not maintained by theatrical effort; it is maintained by genuine quality-preservation work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_boundary,
    'Where should the verifiable significance threshold lie? At what point does requiring institutional publication footprint exclude legitimate knowledge versus protecting the commons from promotional noise?',
    'Empirical analysis of deletion-review rationale across AfD cases: measure the proportion of deletions justified by spam/promotional harm versus quality degradation versus boundary-tightening for its own sake. Compare knowledge loss (what gets deleted that was genuinely useful) against knowledge protection (what deletion prevented degradation).',
    'If deletion-review analysis shows the threshold is calibrated primarily to prevent spam, the deletionist reading''s framing holds. If analysis shows substantial proportion of deletions exclude niche knowledge that meets verifiability standards, the threshold may have drifted toward exclusionary gatekeeping (supporting the inclusionist reading). This omega is the empirical gate between the two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_boundary, empirical, 'Whether the deletionist reading''s protective intent matches the threshold''s actual enforcement.').

omega_variable(
    coordinated_vs_distributed_quality_maintenance,
    'Is centralized enforcement of notability thresholds necessary for quality maintenance, or could distributed alternative wikis (sister projects, specialized databases) preserve commons quality without deletionist gatekeeping?',
    'Empirical observation from Wikimedia sister projects (Wikivoyage, Wikidata, Commons) and parallel encyclopedic wikis that use different notability models: do they experience degradation that would occur in mainline Wikipedia without notability enforcement? Controlled comparison of quality drift in deletion-light environments.',
    'If sister projects with looser notability thresholds maintain comparable quality or successfully specialize, the coordination function is separable from the deletionist model—the constraint''s exclusivity is less justified. If quality measurably degrades without deletion enforcement, coordination requires centralized gatekeeping (supporting the deletionist reading). This addresses whether the coordination function REQUIRES the specific enforcement mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinated_vs_distributed_quality_maintenance, empirical, 'Whether notability enforcement is structurally necessary for quality coordination or a chosen instrument.').

omega_variable(
    epistemic_bias_in_publication_footprint,
    'Does the requirement for documented publication presence in ''recognized sources'' systematically bias inclusion toward knowledge produced in wealthy-world institutions, academic structures, and legacy media, and against knowledge produced by grassroots, marginalized, or geographically peripheral communities?',
    'Content analysis of deleted articles and their bylines: measure geographic, institutional, and demographic distribution of creators whose work passed notability thresholds versus those whose work was deleted. Compare against the distribution of human knowledge production globally. Qualitative analysis of AfD debates to identify whether ''verifiability'' is applied as a proxy for ''institutional legitimacy''.',
    'If analysis shows statistically significant bias toward wealthy-world institutions and against marginalized knowledge sources, the deletionist reading''s claim to neutral quality preservation is compromised—the constraint encodes structural gatekeeping (supporting the inclusionist reading). If distribution is roughly proportional to global publication distribution, the deletionist reading''s framing holds. This omega addresses a key structural disagreement between the two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_bias_in_publication_footprint, empirical, 'Whether notability thresholds encode epistemic bias toward institutional knowledge.').

omega_variable(
    reading_forecast_disagreement,
    'Under what future conditions would the deletionist reading and the inclusionist reading swap which is empirically dominant? What changes in technology, knowledge production, or Wikipedia governance would make the constraint look more like gatekeeping than coordination?',
    'Forecasting: identify the key parameters (institutional centralization of knowledge, alternative platforms for niche content, Wikipedia''s user base composition, globalization of publication structures, AI-assisted content review). Scenario analysis: if these parameters shift, which reading becomes more empirically adequate? Revisit the reading classifications at 5-year intervals against actual parameter drift.',
    'This omega documents that the deletionist reading is NOT universally true—it is empirically contingent. Changes in epistemic infrastructure could make the constraint structurally more extractive (inclusionist) than protective (deletionist). Forecasting identifies the tipping points and allows prospective falsification of the reading''s adequacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_forecast_disagreement, conceptual, 'The reading''s empirical stability depends on epistemic infrastructure conditions that may change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(nota_tr_t3, notability_guidelines__deletionist_reading, theater_ratio, 3, 0.05).
narrative_ontology:measurement(nota_tr_t6, notability_guidelines__deletionist_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deletionist_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(nota_tr_t18, notability_guidelines__deletionist_reading, theater_ratio, 18, 0.075).
narrative_ontology:measurement(nota_tr_t24, notability_guidelines__deletionist_reading, theater_ratio, 24, 0.08).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nota_be_t3, notability_guidelines__deletionist_reading, base_extractiveness, 3, 0.11).
narrative_ontology:measurement(nota_be_t6, notability_guidelines__deletionist_reading, base_extractiveness, 6, 0.13).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deletionist_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement(nota_be_t18, notability_guidelines__deletionist_reading, base_extractiveness, 18, 0.17).
narrative_ontology:measurement(nota_be_t24, notability_guidelines__deletionist_reading, base_extractiveness, 24, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(nota_su_t3, notability_guidelines__deletionist_reading, suppression_requirement, 3, 0.14).
narrative_ontology:measurement(nota_su_t6, notability_guidelines__deletionist_reading, suppression_requirement, 6, 0.17).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deletionist_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement(nota_su_t18, notability_guidelines__deletionist_reading, suppression_requirement, 18, 0.21).
narrative_ontology:measurement(nota_su_t24, notability_guidelines__deletionist_reading, suppression_requirement, 24, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.04).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% The notability_guidelines kernel decomposes into three structurally distinct constraints: the deletionist_reading (this file, Rope, quality coordination), the inclusionist_reading (Snare, systematic gatekeeping), and the deliberative_reading (Tangled Rope, contested negotiation). Each reading instantiates a different ε-value and beneficiary/victim structure. They are linked because they interpret the same kernel text (Wikipedia:Notability) but they are NOT the same constraint viewed from different angles—they are distinct constraints with distinct metrics. The ε-invariance principle requires separate files. Network edges enable the corpus to track that all three readings compete to explain the same institutional phenomenon (Wikipedia's inclusion/exclusion decisions), and metrics divergence between the readings is the empirical measurement of the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
