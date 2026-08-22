% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV as Revisable Translation — Textual-Critical Reading
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   This story instantiates the revisable-translation reading of the KJV
 *   kernel: the claim that the 1611 text is a landmark but time-bound
 *   scholarly achievement, properly superseded as manuscript evidence and
 *   linguistic understanding improve. Under this reading, translation
 *   selection is consumer choice among competing modern editions, academic
 *   textual critics and denominational committees are the legitimate arbiters
 *   of textual questions, and suppression is low because no single
 *   translation is coercively imposed. The extraction that does exist is not
 *   doctrinal control but market capture: a continuous cycle of new editions,
 *   copyrighted translations, and study-Bible product lines sustained by the
 *   premise that revision is always warranted. This is a distinct constraint
 *   from the exclusive_inspiration_reading (which treats any revision premise
 *   as itself illegitimate) and from the functional_equivalence_reading
 *   (which treats the KJV and modern versions as serving different,
 *   non-competing purposes rather than the KJV being supersedable). Each
 *   reading has its own epsilon and its own beneficiary/victim structure;
 *   they are linked, not merged.
 *
 * KEY AGENTS:
 *   - biblical_scholars: analytical/institutional authority behind revision
 *   - modern_translation_publishers: organized beneficiary capturing market value of the revision premise
 *   - kjv_only_congregations_in_transition: identity-locked payer bearing the cost of devaluation
 *   - readers_dependent_on_single_pew_bible: trapped payer with no practical access to the improvement this reading promises
 *   - exclusive_inspiration_adherents: excluded voice whose objection is categorical, not evidentiary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.42).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.18).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV as Revisable Translation — Textual-Critical Reading").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious_studies/textual_criticism/theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, '0c9bf4c9-132a-4545-9786-e30111be54c5').
narrative_ontology:cs_kernel_codification('0c9bf4c9-132a-4545-9786-e30111be54c5', fixed_text).
narrative_ontology:cs_authority_grounding('0c9bf4c9-132a-4545-9786-e30111be54c5', expertise).
narrative_ontology:cs_interpretation_layer_present('0c9bf4c9-132a-4545-9786-e30111be54c5').
narrative_ontology:cs_reading_relation('0c9bf4c9-132a-4545-9786-e30111be54c5', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('0c9bf4c9-132a-4545-9786-e30111be54c5', kjv_text_1611__functional_equivalence_reading, influences).
narrative_ontology:cs_axiom('0c9bf4c9-132a-4545-9786-e30111be54c5', foundational, manuscript_evidence_supersedes_reception_history).
narrative_ontology:cs_axiom_status(manuscript_evidence_supersedes_reception_history, holdable).
narrative_ontology:cs_axiom_grounding('0c9bf4c9-132a-4545-9786-e30111be54c5', manuscript_evidence_supersedes_reception_history, empirically_contingent).
narrative_ontology:cs_axiom('0c9bf4c9-132a-4545-9786-e30111be54c5', secondary, translation_is_an_ongoing_correctable_craft).
narrative_ontology:cs_axiom_status(translation_is_an_ongoing_correctable_craft, holdable).
narrative_ontology:cs_axiom_grounding('0c9bf4c9-132a-4545-9786-e30111be54c5', translation_is_an_ongoing_correctable_craft, instrumental).
narrative_ontology:cs_reference_frame('0c9bf4c9-132a-4545-9786-e30111be54c5', byzantine_textus_receptus_1611_baseline).
narrative_ontology:cs_drift_state('0c9bf4c9-132a-4545-9786-e30111be54c5', post_dead_sea_scrolls_critical_text_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0c9bf4c9-132a-4545-9786-e30111be54c5', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, biblical_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, modern_translation_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, lay_readers_seeking_clarity).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, kjv_only_congregations_in_transition).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, readers_dependent_on_single_pew_bible).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, translation_committees).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, textual_criticism_methodology).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, manuscript_evidence_hierarchy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Evaluate manuscript evidence (Dead Sea Scrolls, Codex Sinaiticus, papyri unavailable in 1611) and Greek/Hebrew linguistic scholarship developed since the KJV translators worked. They produce critical editions and serve as the credentialing authority behind translation committees. Their standing depends on the premise that translation is an improvable craft responsive to evidence, not a closed inspired act.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, biblical_scholars, agenda_setter,
    institutional, civilizational, analytical, global).

% Produce, copyright, and market successive translations (NIV, ESV, NRSV, etc.) built on the premise that revision is legitimate and ongoing. They profit from a continuous replacement cycle — new editions, study Bibles, licensing fees — that depends on the reading's premise that no translation is final.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, modern_translation_publishers, beneficiary,
    organized, generational, arbitrage, global).

% Choose among many available translations for readability and accuracy in contemporary English. Comprehension is improved by revision; they can switch translations freely with no institutional cost, and their choice functions as ordinary consumer selection.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, lay_readers_seeking_clarity, beneficiary,
    moderate, biographical, mobile, national).

% Belong to congregational or family traditions built around KJV exclusivity. Under this reading, their inherited textual commitment is treated as scholarly error to be corrected rather than doctrine to be respected; the shift asks them to abandon an identity-constituting text on the authority of experts they may not recognize as legitimate arbiters of scripture.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_only_congregations_in_transition, payer,
    powerless, biographical, identity_locked, local).

% Low-resource congregations or individuals (older, rural, or economically constrained) whose only accessible Bible is the KJV pew copy their church already owns. The revisability premise implicitly devalues their only available text without providing them the newer editions that would realize the improvement, leaving them holding a text scholars call outdated with no practical path to the alternative.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, readers_dependent_on_single_pew_bible, payer,
    powerless, immediate, trapped, local).

% Denominational and interdenominational bodies that authorize new translations, incorporating scholarly consensus and manuscript findings. They administer the actual revision process and gain institutional authority and publishing revenue from being the recognized body that updates the text.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, translation_committees, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, translation_committees, beneficiary).

% Hold that the KJV itself is providentially preserved and therefore not a candidate for correction by manuscript scholarship. Their premise is treated by this reading as a category error about how textual authority works; they are not represented in the scholarly apparatus this reading defers to and would reject its authority in principle, not merely its conclusions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, exclusive_inspiration_adherents, excluded,
    moderate, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__revisable_translation_reading, modern_translation_publishers).
narrative_ontology:fixing_cost_class(kjv_text_1611__revisable_translation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates biblical scholarship, denominational translation committees, and readers around a shared method: incorporate improved manuscript evidence and linguistic knowledge into successive translations, so the text in readers' hands tracks the best available reconstruction of the source documents.
% TRANSFER_FUNCTION: Moves interpretive authority from the historical translation committee of 1604-1611 to contemporary academic and denominational scholarly bodies; moves purchasing activity and licensing revenue toward modern translation publishers; moves textual-critical legitimacy away from traditions that treat the KJV as closed and complete.
% ABSENT_VOICES: Exclusive-inspiration adherents are structurally outside the scholarly apparatus this reading defers to — their objection is not merely that a particular revision is wrong but that revision as a category is illegitimate, a premise this reading cannot accommodate without ceasing to be itself.
% DISAPPEARANCE_RATIONALE: If the revisability premise vanished overnight, the modern translation industry's legitimacy claim would collapse (no basis for producing successor texts) and academic textual criticism would lose its practical application to English Bible production, but manuscript scholarship itself would continue as a historical discipline; whether this counts as 'world rearranges' or 'world unchanged' depends on whether one weights the publishing/scholarly ecosystem or the underlying academic activity.
% FOUNDING_PROBLEM: The 1611 KJV was produced from a limited manuscript base (chiefly the Byzantine-tradition Textus Receptus) and pre-modern lexicographic knowledge; discoveries since then (older papyri, the Dead Sea Scrolls, comparative Semitic philology) revealed places where the KJV's source text or word choices could be improved, and English itself has drifted enough that some KJV phrasing is now opaque or misleading to ordinary readers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by working textual critics and papyrologists whose manuscript discoveries (independent of any translation-publishing interest) demonstrably predate and are causally prior to the translation committees that later incorporated them; also corroborated by historical linguists documenting semantic drift in early modern English independent of any Bible-publishing stake. The strongest counter-corroboration comes from exclusive-inspiration adherents themselves, who do not dispute the historical facts of manuscript discovery but dispute their theological relevance.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, contested).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.42) is moderate: real scholarly coordination exists (manuscript evidence genuinely accumulated since 1611), but a for-profit publishing cycle rides on the revisability premise, extracting from readers who must repeatedly repurchase updated editions and study materials. Suppression is low (0.18) because this reading imposes no coercive mechanism — no reader is forced off the KJV, and translation choice functions as market selection. Accessibility collapse is low-moderate (0.25): for most readers alternatives are abundant, but for resource-constrained congregations locked into a single pew Bible, the promised improvement is practically inaccessible. Resistance (0.35) comes from KJV-only communities who experience the revisability premise as an attack on inherited textual identity rather than a neutral scholarly finding.
 *
 * PERSPECTIVAL GAP:
 *   From the scholarly/publisher seats, this looks like a genuine Rope: coordinated, low-coercion, textually justified improvement with real beneficiaries and no meaningful victims. From the identity-locked KJV-only congregations, the same structure looks like an imposed devaluation of their inherited text by an authority they never consented to recognize. The engine should register this divergence rather than resolve it — the reading itself only holds from inside its own premise that manuscript evidence trumps received tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Biblical scholars and translation committees sit as agenda-setters with analytical/institutional power and essentially no exit cost — their professional standing is enhanced, not threatened, by ongoing revision. Modern translation publishers are structural beneficiaries with arbitrage-grade exit (they can pivot across translation lines). Lay readers seeking clarity are near-symmetric beneficiaries with full market mobility. KJV-only congregations and single-pew-Bible readers are targets: identity-locked or outright trapped, bearing the cost of a premise that treats their textual commitment as error, with no comparable exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (limited 1611 manuscript base, archaic English) is still live by scholarly consensus — new manuscript evidence continues to surface and linguistic drift continues. This blocks a piton/mandatrophy classification: the coordination function has not gone dead, it has been joined by an extraction layer (publisher revenue capture) riding on the same justified premise. The correct read is a rope with rising extraction over time (0.22 to 0.42 across the interval), not a fully atrophied constraint — the underlying textual-critical work remains functionally necessary even as commercial capture of that work has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scholarly_authority_vs_denominational_legitimacy,
    'Is academic textual-critical consensus a legitimate arbiter of scriptural authority for religious communities that do not recognize secular/academic epistemic authority over doctrinal matters, or does this reading simply assume an authority structure that exclusive-inspiration communities reject at the premise level?',
    'No empirical resolution exists; this is a genealogical/theological dispute about which knowledge-communities get to adjudicate textual questions about scripture. Comparative study of how other textual traditions (e.g., Quranic textual criticism vs. traditional tajwid authority) handle the same structural tension could illuminate but not resolve it.',
    'If academic authority is accepted as legitimate, this reading''s extractiveness figure is close to a fair coordination cost; if rejected, the entire reading is experienced by non-accepting communities as an imposed extraction dressed as neutral scholarship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholarly_authority_vs_denominational_legitimacy, conceptual, 'Whether academic/scholarly authority over scripture is itself a contested premise or a settled fact this reading can assume.').

omega_variable(
    publisher_capture_of_revision_premise,
    'How much of the observed extraction (0.42) is genuine coordination cost of incorporating manuscript evidence, versus rent extracted by publishers using the revisability premise to justify a continuous product replacement cycle unrelated to actual textual improvement?',
    'Compare the rate of substantive manuscript-driven textual changes across successive translation editions against the rate of new commercial editions/study-Bible releases; a high ratio of commercial releases to substantive changes would indicate capture.',
    'A high capture ratio would push this reading''s computed type toward tangled_rope (genuine coordination function co-existing with asymmetric extraction via the publishing industry) rather than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_capture_of_revision_premise, empirical, 'Whether publisher extraction has decoupled from the genuine textual-critical coordination function this reading claims to serve.').

omega_variable(
    framing_kernel_vs_institution,
    'Is the more defensible framing of this constraint the revision practice itself (translation committees revising a text), or the underlying legitimacy claim that manuscript evidence should govern scriptural text at all (a meta-level epistemic commitment)?',
    'Would require tracing whether disputes in practice center on specific manuscript readings (institutional framing) or on the propriety of using manuscript evidence as a criterion at all (legitimacy-claim framing); denominational schism histories would provide evidence.',
    'Under the institutional framing, this reading remains a rope with rising commercial extraction; under the legitimacy-claim framing, it would classify closer to tangled_rope from inception, since the legitimacy claim itself concentrates authority in a scholarly class whose judgments are not contestable by lay adherents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_kernel_vs_institution, conceptual, 'Whether the constraint is best modeled as the translation-committee practice or the deeper epistemic-authority claim underneath it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 1881, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1881, kjv_text_1611__revisable_translation_reading, theater_ratio, 1881, 0.1).
narrative_ontology:measurement(kjv__tr_t1946, kjv_text_1611__revisable_translation_reading, theater_ratio, 1946, 0.13).
narrative_ontology:measurement(kjv__tr_t1971, kjv_text_1611__revisable_translation_reading, theater_ratio, 1971, 0.16).
narrative_ontology:measurement(kjv__tr_t1990, kjv_text_1611__revisable_translation_reading, theater_ratio, 1990, 0.19).
narrative_ontology:measurement(kjv__tr_t2005, kjv_text_1611__revisable_translation_reading, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(kjv__tr_t2025, kjv_text_1611__revisable_translation_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1881, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1881, 0.22).
narrative_ontology:measurement(kjv__be_t1946, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1946, 0.28).
narrative_ontology:measurement(kjv__be_t1971, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1971, 0.33).
narrative_ontology:measurement(kjv__be_t1990, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1990, 0.37).
narrative_ontology:measurement(kjv__be_t2005, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(kjv__be_t2025, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kjv_text_1611__revisable_translation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, information_standard).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the kjv_text_1611 kernel and must not be merged into one story with an averaged epsilon. exclusive_inspiration_reading treats revision as categorically illegitimate (near-zero coordination function under its own premise, high suppression against modern-translation use in its own communities). functional_equivalence_reading treats the KJV and modern translations as non-competing complements (low extraction, low victim set — a rope with almost no tension). revisable_translation_reading (this story) treats the KJV as supersedable and shows moderate rising extraction driven by commercial capture of the revision premise. Each carries its own epsilon, beneficiaries, and victims; the kernel context in each file documents the relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
