% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__revisable_translation_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV as Improvable Translation — Scholarly Revision Reading
 *   domain: religious/textual_criticism
 *
 * SUMMARY:
 *   This story instantiates the REVISABLE-TRANSLATION reading of the KJV
 *   kernel: the claim that the 1611 text is a valuable but corrigible product
 *   of its manuscript and linguistic moment, properly subject to ongoing
 *   revision as textual criticism advances. This is a distinct constraint
 *   from the exclusive-inspiration reading (which treats the KJV itself as
 *   the inerrant standard against which all revision is corruption) and the
 *   functional-equivalence reading (which treats multiple translations as
 *   serving complementary, non-competing purposes). Under this reading
 *   specifically, the coordination function is genuine (tracking
 *   best-available manuscript evidence) but the extraction has migrated
 *   toward the modern publishing industry, which monetizes the revisability
 *   premise through copyrighted editions, and toward academic institutions
 *   whose disciplinary standing depends on revision remaining a live, ongoing
 *   enterprise rather than a closed question. Suppression is low: no one is
 *   coerced into accepting a particular modern translation, and the KJV
 *   itself remains freely available in the public domain. The primary victims
 *   are not readers denied access to scholarship but communities whose
 *   theological commitment to the KJV as such is recast by this reading as an
 *   error correctable by better information.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.38).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.14).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV as Improvable Translation — Scholarly Revision Reading").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious/textual_criticism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, '9def35bc-a5b3-453b-914f-5dcdd4efb15f').
narrative_ontology:cs_kernel_codification('9def35bc-a5b3-453b-914f-5dcdd4efb15f', fixed_text).
narrative_ontology:cs_authority_grounding('9def35bc-a5b3-453b-914f-5dcdd4efb15f', expertise).
narrative_ontology:cs_interpretation_layer_present('9def35bc-a5b3-453b-914f-5dcdd4efb15f').
narrative_ontology:cs_reading_relation('9def35bc-a5b3-453b-914f-5dcdd4efb15f', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('9def35bc-a5b3-453b-914f-5dcdd4efb15f', kjv_text_1611__functional_equivalence_reading, influences).
narrative_ontology:cs_axiom('9def35bc-a5b3-453b-914f-5dcdd4efb15f', foundational, inspiration_attaches_to_reconstructed_original_not_1611_text).
narrative_ontology:cs_axiom_status(inspiration_attaches_to_reconstructed_original_not_1611_text, holdable).
narrative_ontology:cs_axiom_grounding('9def35bc-a5b3-453b-914f-5dcdd4efb15f', inspiration_attaches_to_reconstructed_original_not_1611_text, empirically_contingent).
narrative_ontology:cs_axiom('9def35bc-a5b3-453b-914f-5dcdd4efb15f', foundational, manuscript_evidence_since_1611_warrants_translation_revision).
narrative_ontology:cs_axiom_status(manuscript_evidence_since_1611_warrants_translation_revision, holdable).
narrative_ontology:cs_axiom_grounding('9def35bc-a5b3-453b-914f-5dcdd4efb15f', manuscript_evidence_since_1611_warrants_translation_revision, empirically_contingent).
narrative_ontology:cs_axiom('9def35bc-a5b3-453b-914f-5dcdd4efb15f', secondary, translation_quality_is_progressively_improvable).
narrative_ontology:cs_axiom_status(translation_quality_is_progressively_improvable, holdable).
narrative_ontology:cs_axiom_grounding('9def35bc-a5b3-453b-914f-5dcdd4efb15f', translation_quality_is_progressively_improvable, instrumental).
narrative_ontology:cs_reference_frame('9def35bc-a5b3-453b-914f-5dcdd4efb15f', textus_receptus_1611_translation_basis).
narrative_ontology:cs_drift_state('9def35bc-a5b3-453b-914f-5dcdd4efb15f', post_critical_text_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9def35bc-a5b3-453b-914f-5dcdd4efb15f', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, modern_translation_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, textual_critics).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, seminary_institutions).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, kjv_only_congregations).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, low_income_readers_facing_translation_paywalls).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, ordinary_lay_readers).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, ordinary_lay_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Evaluate manuscript discoveries (Dead Sea Scrolls, papyri finds since 1611), advances in Hebrew/Greek philology, and textual-critical methodology to argue the KJV's underlying Textus Receptus and translation choices can be improved. They set the terms of what counts as a better manuscript basis or rendering, publish critical editions, and train the next generation of translators. Their authority is professional and rests on demonstrated expertise rather than institutional coercion.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars, agenda_setter,
    institutional, generational, analytical, global).

% Produce and sell copyrighted modern translations (NIV, ESV, NASB, etc.) justified by appeal to improved manuscripts and updated language. They benefit commercially from the revisability premise: every new manuscript find or linguistic argument is an occasion to market a new edition. They compete with each other and with public-domain KJV editions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, modern_translation_publishers, beneficiary,
    organized, biographical, arbitrage, global).

% Train clergy in original-language exegesis and modern critical methods, treating the KJV as one historical witness among several rather than a final text. They benefit from positioning themselves as necessary interpreters of a text requiring ongoing scholarly mediation, which sustains enrollment and institutional relevance.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, seminary_institutions, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, seminary_institutions, agenda_setter).

% Build careers on comparing manuscript families and proposing emendations. Their professional standing depends on the ongoing legitimacy of revision as a live scholarly enterprise; a settled, unrevisable text would eliminate their disciplinary function.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, textual_critics, beneficiary,
    moderate, civilizational, mobile, global).

% Hold that the KJV itself (not merely its underlying manuscripts) carries unique authority and experience the revisable-translation reading as an erosion of their tradition's claim to certainty. They bear a reputational and doctrinal cost: under this reading their position is recast as scholarly naivete rather than theological conviction, and their congregations face internal pressure to adopt modern translations they regard as compromised.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_only_congregations, payer,
    moderate, generational, constrained, national).

% Cannot freely access many modern, copyrighted translations that this reading treats as the appropriate beneficiaries of revision; the public-domain KJV remains free but is framed as inferior, pushing them either toward paying for licensed modern texts or accepting a translation the reading itself calls outdated.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, low_income_readers_facing_translation_paywalls, payer,
    powerless, biographical, constrained, national).

% Gain access to translations in contemporary idiom and benefit from corrected renderings where manuscript evidence has genuinely advanced since 1611. They also bear the cost of navigating a fragmented marketplace of competing translations with no single authoritative reference point, and must trust scholarly consensus they cannot independently verify.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, ordinary_lay_readers, beneficiary,
    powerless, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, ordinary_lay_readers, payer).

% Adjudicate which translations congregations may use in worship, weighing scholarly consensus against congregational tradition. They observe and sometimes mediate disputes between KJV-only communities and revision-accepting scholars without being direct parties to the underlying manuscript debate.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, denominational_hierarchies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates ongoing correction of translation against improving evidence: as manuscripts are discovered and philological understanding advances, the text used by readers can track the best available reconstruction of the original rather than freezing at a 1611 snapshot.
% TRANSFER_FUNCTION: Moves interpretive authority from a single fixed 17th-century text toward the community of academic textual critics and translation committees, and moves reader spending from free public-domain KJV editions toward copyrighted modern translations sold by publishers who cite revisability as their justification.
% ABSENT_VOICES: KJV-only congregations experience this reading as delegitimizing their tradition but are rarely represented within the academic textual-criticism apparatus that adjudicates 'better manuscripts'; their theological objection (that inspiration attaches to the received text itself, not to a reconstructed original) is treated as a category error rather than engaged on its own terms.
% DISAPPEARANCE_RATIONALE: If the revisable-translation premise vanished, academic textual criticism of the Bible would lose its practical rationale for producing new English editions, modern translation publishers would lose their primary marketing justification, and seminaries would need to recast their exegetical training. KJV-only communities would regard this as vindication rather than loss. Scholars and publishers would say a genuine correction mechanism disappeared; KJV-only readers would say nothing of value was lost. The disagreement is the kernel contest itself, not a side effect of this reading.
% FOUNDING_PROBLEM: Early modern translation relied on a limited manuscript base (largely Byzantine text-type via Erasmus's Textus Receptus) and Renaissance-era linguistic scholarship; subsequent discovery of older and more numerous manuscripts (Codex Sinaiticus, Vaticanus, the Dead Sea Scrolls) and advances in comparative Semitic and Koine Greek philology created a documented gap between the KJV's textual basis and the best currently reconstructable original-language text.
% FOUNDING_PROBLEM_CORROBORATION: Textual variants and manuscript discoveries are independently documented in critical apparatuses (Nestle-Aland, UBS Greek New Testament) maintained by international scholarly consortia that include participants with no financial stake in any single modern translation's sales; paleographic dating of manuscripts is corroborated by non-confessional academic institutions and museums holding the physical artifacts.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, contested).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).
:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rose from near-zero at the reading's founding moment (Revised Version, 1881, when appeal to older manuscripts first displaced the Textus Receptus as the primary translation basis) through the mid-20th century proliferation of copyrighted modern translations (RSV 1952, NIV 1978) to the present fragmented and commercialized translation marketplace (2024). The trajectory reflects genuine scholarly progress (manuscript discoveries are real, documented, and non-partisan) running alongside an increasingly commercialized translation industry that uses the revisability premise as a perpetual justification for new copyrighted editions. Theater ratio rose modestly as marketing language ('most accurate,' 'most readable') increasingly substitutes for substantive manuscript arguments in some publisher campaigns, though the underlying scholarly apparatus (critical editions, textual apparatuses) remains substantively functional rather than purely performative. Suppression stays low throughout: this reading does not compel anyone to abandon the KJV, and readers retain the free public-domain option at every point on the grid.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic scholars and textual critics sit near the beneficiary end: their professional authority and disciplinary existence are underwritten by the premise that translation is an ongoing, correctable project. Modern translation publishers are structural beneficiaries in a more direct commercial sense — every manuscript discovery or philological refinement is a occasion for a new marketable edition. KJV-only congregations sit near the target end: this reading does not merely disagree with them, it recasts their central theological claim as a correctable factual error, with real reputational and communal costs. Low-income readers facing paywalls are targets in a narrower sense — the reading's logic implies the 'better' texts are the copyrighted modern ones, which are not universally free, while the free alternative is framed as inferior. Ordinary lay readers are more genuinely mixed: real linguistic clarity benefits flow to them, offset by navigational cost in a crowded translation marketplace with no single authoritative touchstone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (manuscript basis lagging best available evidence) remains genuinely live — new papyri and computational stemmatic methods continue to refine the textual record, so this is not a case of an arrangement persisting past its function. What has drifted is the coupling between the scholarly correction function (still substantively functional) and the commercial translation industry's monetization of that function (which has grown disproportionately). The classification as rope rather than tangled_rope reflects that no active enforcement compels any party into this arrangement — congregations, denominations, and individual readers remain free to use the KJV exclusively if they choose; the costs borne by KJV-only communities are reputational/doctrinal delegitimization within academic and mainstream Protestant discourse, not coercive suppression of their reading practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_basis,
    'Is the choice between the exclusive-inspiration, functional-equivalence, and revisable-translation readings of the KJV kernel itself adjudicable by evidence, or is it a prior commitment (about what kind of thing biblical inspiration is) that determines which evidence counts as relevant?',
    'No empirical resolution mechanism exists: manuscript evidence is common ground across all three readings, but what that evidence IMPLIES depends on a prior theological commitment about whether inspiration attaches to an underlying autograph text (favoring revisability), to the KJV text specifically (favoring exclusive inspiration), or to whatever text a community receives and uses in practice (favoring functional equivalence). This is a conceptual/preference disagreement, not an empirical one.',
    'If the reading-selection question is conceptual rather than empirical, no amount of further manuscript discovery will settle the kernel contest — the three readings will persist as genuinely incommensurable framings held by different communities, and this story''s classification as ''rope'' (low suppression, genuine coordination) holds only within the revisable-translation framework, not as a framework-independent verdict on the kernel as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the choice among sibling kernel readings is empirically or theologically determined.').

omega_variable(
    revision_vs_commercialization_coupling,
    'Is the correlation between genuine manuscript-based revision and commercial translation proliferation causal (publishers manufacture demand for revision to sell new editions) or merely correlated (both track the same underlying scholarly progress independently)?',
    'Compare the rate of substantive manuscript-driven textual changes (documented in critical apparatuses) against the rate of new copyrighted translation releases; a publisher-release rate substantially exceeding the rate of genuine textual-critical advance would support the causal/extractive reading.',
    'If causal, the extractiveness measured here understates the degree to which the revisability premise has been captured by commercial interests rather than reflecting genuine scholarly necessity; if merely correlated, the current extractiveness score (0.38) appropriately reflects a moderate, non-dominant commercial overlay on a substantively real scholarly process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revision_vs_commercialization_coupling, empirical, 'Whether publisher commercialization drives or merely accompanies genuine textual revision.').

omega_variable(
    kjv_only_objection_engagement,
    'Does the revisable-translation reading engage the KJV-only theological objection (that inspiration/providential preservation attaches to the received text tradition, not a reconstructed critical text) on its own terms, or does it dismiss the objection by definitional fiat (treating ''inspiration'' as necessarily referring to an autograph, which is the very point in dispute)?',
    'Examine whether academic textual-critical literature engaging KJV-only arguments (e.g., responses to the Trinitarian Bible Society or similar) addresses the providential-preservation claim as a coherent theological position or treats it as a category confusion not requiring substantive engagement.',
    'If the objection is dismissed by fiat rather than engaged, the excluded-voices problem for kjv_only_congregations is more severe than the absent_voices field suggests, and the reading''s claim to be simply following the evidence understates its own contestable theological premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kjv_only_objection_engagement, conceptual, 'Whether the reading substantively engages or definitionally excludes the sibling exclusive-inspiration position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 1611, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1611, kjv_text_1611__revisable_translation_reading, theater_ratio, 1611, 0.05).
narrative_ontology:measurement_basis(kjv__tr_t1611, observed).
narrative_ontology:measurement(kjv__tr_t1881, kjv_text_1611__revisable_translation_reading, theater_ratio, 1881, 0.08).
narrative_ontology:measurement_basis(kjv__tr_t1881, observed).
narrative_ontology:measurement(kjv__tr_t1952, kjv_text_1611__revisable_translation_reading, theater_ratio, 1952, 0.12).
narrative_ontology:measurement_basis(kjv__tr_t1952, observed).
narrative_ontology:measurement(kjv__tr_t1978, kjv_text_1611__revisable_translation_reading, theater_ratio, 1978, 0.17).
narrative_ontology:measurement_basis(kjv__tr_t1978, observed).
narrative_ontology:measurement(kjv__tr_t2001, kjv_text_1611__revisable_translation_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement_basis(kjv__tr_t2001, observed).
narrative_ontology:measurement(kjv__tr_t2024, kjv_text_1611__revisable_translation_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(kjv__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1611, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1611, 0.08).
narrative_ontology:measurement_basis(kjv__be_t1611, observed).
narrative_ontology:measurement(kjv__be_t1881, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1881, 0.15).
narrative_ontology:measurement_basis(kjv__be_t1881, observed).
narrative_ontology:measurement(kjv__be_t1952, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1952, 0.22).
narrative_ontology:measurement_basis(kjv__be_t1952, observed).
narrative_ontology:measurement(kjv__be_t1978, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement_basis(kjv__be_t1978, observed).
narrative_ontology:measurement(kjv__be_t2001, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2001, 0.34).
narrative_ontology:measurement_basis(kjv__be_t2001, observed).
narrative_ontology:measurement(kjv__be_t2024, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(kjv__be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kjv_text_1611__revisable_translation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__revisable_translation_reading, 0.05).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the kjv_text_1611 kernel, decomposed per the ε-invariance principle: measuring 'the KJV constraint' under an exclusive-inspiration frame, a functional-equivalence frame, and a revisability frame yields three structurally distinct ε values, victim sets, and suppression profiles. exclusive_inspiration_reading carries high suppression (alternative translations treated as illegitimate) and low extractiveness (no commercial machinery, communal enforcement). functional_equivalence_reading carries low suppression and low-to-moderate extractiveness distributed evenly across a translation ecosystem with no single privileged text. revisable_translation_reading (this story) carries low suppression but rising extractiveness concentrated in the academic-publishing nexus. All three are linked bidirectionally; none is authoritative over the others within this framework — the kernel itself remains ambiguous, and each reading is a fully realized constraint in its own right.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
