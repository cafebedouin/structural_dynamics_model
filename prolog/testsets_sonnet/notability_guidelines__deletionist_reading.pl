% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: WP:N as Epistemic Quality Filter (Deletionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This story instantiates the deletionist reading of the
 *   notability_guidelines kernel: WP:N functions as a coordination mechanism
 *   that preserves the epistemic quality of a commons resource by requiring
 *   independent, verifiable evidence of significance before a topic is
 *   admitted. Under this reading, exclusion of non-notable submissions is not
 *   extraction from a victim class — the excluded material (vanity pages,
 *   promotional entries, indiscriminate trivia) has no legitimate claim the
 *   guideline denies, and every excluded submitter retains full access to
 *   publish elsewhere. The coordination good (a reliably filtered reference
 *   work) accrues to readers, good-faith editors, and downstream reusers who
 *   depend on the corpus's evidentiary floor. Two sibling constraints exist
 *   for the same kernel text: the inclusionist_reading treats the identical
 *   guideline as systematic exclusion of marginalized-knowledge topics
 *   (different beneficiary/victim structure, likely tangled_rope or snare),
 *   and the deliberative_reading treats it as an evolving negotiation process
 *   rather than a fixed filter (different temporal/enforcement structure,
 *   likely rope or scaffold). Each reading is authored as its own file with
 *   its own stable epsilon; this file does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.12).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.28).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "WP:N as Epistemic Quality Filter (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '214dc0b5-f076-4071-b4d3-73590382a7fd').
narrative_ontology:cs_kernel_codification('214dc0b5-f076-4071-b4d3-73590382a7fd', formalized).
narrative_ontology:cs_authority_grounding('214dc0b5-f076-4071-b4d3-73590382a7fd', practice).
narrative_ontology:cs_interpretation_layer_present('214dc0b5-f076-4071-b4d3-73590382a7fd').
narrative_ontology:cs_reading_relation('214dc0b5-f076-4071-b4d3-73590382a7fd', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('214dc0b5-f076-4071-b4d3-73590382a7fd', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('214dc0b5-f076-4071-b4d3-73590382a7fd', foundational, independent_sourcing_is_neutral_quality_proxy).
narrative_ontology:cs_axiom_status(independent_sourcing_is_neutral_quality_proxy, holdable).
narrative_ontology:cs_axiom_grounding('214dc0b5-f076-4071-b4d3-73590382a7fd', independent_sourcing_is_neutral_quality_proxy, empirically_contingent).
narrative_ontology:cs_axiom('214dc0b5-f076-4071-b4d3-73590382a7fd', foundational, exclusion_of_undocumented_topics_is_not_harm_when_alternative_venues_exist).
narrative_ontology:cs_axiom_status(exclusion_of_undocumented_topics_is_not_harm_when_alternative_venues_exist, holdable).
narrative_ontology:cs_axiom_grounding('214dc0b5-f076-4071-b4d3-73590382a7fd', exclusion_of_undocumented_topics_is_not_harm_when_alternative_venues_exist, conventional).
narrative_ontology:cs_reference_frame('214dc0b5-f076-4071-b4d3-73590382a7fd', sourcing_based_quality_floor).
narrative_ontology:cs_drift_state('214dc0b5-f076-4071-b4d3-73590382a7fd', contemporary_afd_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('214dc0b5-f076-4071-b4d3-73590382a7fd', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, good_faith_editors).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, downstream_knowledge_reusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, good_faith_editors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patrol new-page feeds and AfD discussions applying WP:N to nominate articles lacking significant coverage in independent reliable sources. They administer the filter, citing verifiability and encyclopedic scope as the reason non-notable content must be excluded. They can leave editing at any time without personal cost; their stake is reputational and normative, not material.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, deletionist_editors, agenda_setter,
    organized, biographical, mobile, global).

% Consume the encyclopedia expecting articles that meet a baseline of independently verifiable significance rather than promotional or trivial entries. They benefit from a filtered commons where search and citation reliably surface substantive coverage; they never see the deleted material and bear no direct cost from its absence.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_readership, beneficiary,
    moderate, generational, mobile, global).

% Write articles on genuinely notable subjects and benefit from a shared, predictable bar that protects their work from being swamped by low-quality or promotional entries. Some pay a cost in wasted labor when a borderline-notable article they wrote is deleted, but they retain the option to improve sourcing and request undeletion.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, good_faith_editors, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deletionist_reading, good_faith_editors, payer).

% Mirror sites, search engines, language-model training pipelines, and academic citation tools ingest Wikipedia as a bulk knowledge source. They benefit structurally from a corpus where entries have cleared an independent-sourcing bar, reducing the propagation of unverifiable or promotional content into downstream systems.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, downstream_knowledge_reusers, beneficiary,
    institutional, civilizational, analytical, global).

% Attempt to add self-promotional, corporate, or vanity content lacking independent coverage. Under this reading they are not treated as a victim class: they retain every other publishing venue (personal sites, social media, press releases) and are excluded specifically because their content fails the coordination bar the commons exists to enforce, not because of who they are.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, vanity_and_promotional_submitters, excluded,
    powerless, immediate, constrained, local).

% Evaluate AfD discussions for rough consensus against the notability guideline text and close accordingly. They apply rather than author the standard, and can be appealed to deletion review if the guideline was misapplied.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, afd_closing_administrators, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, source-based threshold (significant coverage in independent reliable sources) so that editors, readers, and downstream reusers can rely on a predictable floor of verifiable substance across millions of articles, without requiring any central editorial board to individually adjudicate every topic's importance.
% TRANSFER_FUNCTION: Moves editorial attention and reader trust away from unverifiable or promotional submissions and toward topics with an independent evidentiary record; no material or monetary transfer occurs — the resource being allocated is finite curatorial/verification attention and the finite trust budget of the shared reference work.
% ABSENT_VOICES: Subjects of borderline articles (small nonprofits, local historical figures, self-published creators) who believe their notability claim is real but under-documented in indexed sources are not directly represented in AfD; under this reading their absence reflects a sourcing gap in the world, not a suppressed voice the guideline owes representation to.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, the encyclopedia would rapidly fill with promotional, vanity, and indiscriminate entries at a rate exceeding any manual curation capacity; reader trust in Wikipedia as a filtered reference would erode, and downstream reusers (search engines, citation tools, model-training pipelines) would need to build their own filtering layer to compensate — the commons' core value proposition depends on this filter existing.
% FOUNDING_PROBLEM: Early Wikipedia had no consistent basis for deciding what belonged in an encyclopedia versus a promotional directory or personal webpage aggregator; without a shared threshold, article quality and reader trust degraded as low-substance and self-interested content accumulated faster than editors could review it.
% FOUNDING_PROBLEM_CORROBORATION: Academic studies of Wikipedia content quality and deletion patterns (e.g. research on new-page patrol backlogs and promotional-content influx rates) attest that unfiltered submission volume continues to include spam, autobiography, and undisclosed paid-editing content at meaningful rates, corroborating the founding problem's persistence from outside the deletionist editor community itself.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.12, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.12) because, under this reading, no party is charged a rent through the guideline's operation — attention allocation is the only resource moved, and it moves toward evidentiary substance rather than toward any capturing party. Suppression is moderate (0.28) reflecting the real, non-trivial friction borderline-notable topics face (sourcing thresholds, AfD process costs) without treating that friction as coercive extraction. Theater ratio is low and only mildly rising (0.10 to 0.15) reflecting that AfD activity remains substantively tied to actual sourcing evaluation rather than performative deletion drives, though a slow drift toward procedural ritual (canvassing patterns, boilerplate rationale citation) is visible over the interval. Accessibility collapse is moderate (0.35): alternative venues for excluded content are abundant (personal sites, other wikis, social media), so the collapse is partial, not total. Resistance is moderate (0.40): inclusionist editors and affected subjects actively contest specific AfD outcomes, which is expected and healthy friction in a rope, not evidence of coercive extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Deletionist editors administer the filter but derive no material benefit from its operation — their d sits near symmetric/administrative rather than beneficiary, since they invest unpaid labor to maintain a public good. Readership, good-faith editors, and downstream reusers are the structural beneficiaries: they receive a filtered, trustworthy corpus without bearing the curatorial cost. Vanity/promotional submitters are deliberately NOT coded as victims under this reading — their exclusion is the coordination mechanism functioning as designed, and they retain full exit to alternative publishing venues (constrained but not trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unfiltered promotional/vanity content overwhelming curatorial capacity) remains live per the corroborating research on ongoing spam and undisclosed-paid-editing submission rates — this guards against reading WP:N as a purely inertial or capture-driven constraint. Because the founding problem is corroborated as live by sources outside the editor community itself (academic content-quality research), this reading resists the mandatrophy critique that would apply if the problem had been solved and the guideline persisted only by institutional habit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_kernel_reading_choice,
    'Is WP:N structurally a quality-preserving coordination filter (this reading), a systematic gatekeeping apparatus against marginalized knowledge (inclusionist_reading), or an ongoing deliberative negotiation without a fixed boundary (deliberative_reading)?',
    'Systematic audit of AfD outcomes by topic category, comparing deletion rates and sourcing-availability baselines across marginalized versus dominant-culture topics, cross-referenced against whether guideline text or applied practice has remained stable or has visibly drifted over multi-year windows.',
    'If deletion rates for marginalized-topic articles are disproportionate relative to their true sourcing availability (accounting for systemic bias in what independent sources exist to cite), the inclusionist_reading''s victim-set claim gains support and this reading''s no-victim claim would need revision as a separate structural fact, not a reinterpretation of this file. If AfD outcomes show continuous doctrinal renegotiation rather than fixed-boundary application, the deliberative_reading better captures the operative dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_kernel_reading_choice, conceptual, 'Which of three structurally distinct readings the notability kernel actually instantiates in practice, and whether they are stable across topic domains.').

omega_variable(
    sourcing_availability_confound,
    'Does apparent non-notability under WP:N reflect genuine absence of significant independent coverage, or does it reflect systemic under-documentation of certain topics (non-Western subjects, women''s history, oral-tradition-based knowledge) by the independent reliable sources the guideline requires?',
    'Comparative analysis of source-availability patterns across topic domains, controlling for subject-matter importance by independent expert assessment rather than by existing citation counts (which would be circular).',
    'If under-documentation is systemic rather than random, the deletionist reading''s claim that excluded content ''has no legitimate claim'' becomes contestable — the filter would be laundering an upstream sourcing bias into an apparently neutral notability judgment, which is precisely the inclusionist_reading''s core objection routed through a different mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sourcing_availability_confound, empirical, 'Whether the sourcing requirement encodes upstream bias in what counts as documented, independent of the guideline''s own neutrality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deletionist_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deletionist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deletionist_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deletionist_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deletionist_reading, base_extractiveness, 4, 0.09).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deletionist_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deletionist_reading, base_extractiveness, 12, 0.11).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deletionist_reading, base_extractiveness, 16, 0.115).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(notability_guidelines__deletionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.05).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% This file is one of three constraint stories decomposing the single natural-language label 'WP:N' per the epsilon-invariance principle: the deletionist_reading (this file, rope, epsilon 0.12), the inclusionist_reading (tangled_rope or snare, higher epsilon, victim set = contributors on marginalized topics), and the deliberative_reading (rope or scaffold, epsilon reflecting ongoing process cost rather than settled filtration). All three read the identical guideline text; they diverge on beneficiary/victim structure and on whether the boundary is fixed or perpetually renegotiated. Each carries its own claimed_type and metrics and is linked to the other two via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
