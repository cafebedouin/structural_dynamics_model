% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration and Inerrancy Reading
 *   domain: religious/textual/theological
 *
 * SUMMARY:
 *   The KJV Exclusive Inspiration reading asserts that the 1611 King James
 *   Version (and its minor revisions) is the uniquely inspired, inerrant
 *   English Bible — the 'preserved Word of God' for English speakers. All
 *   other English translations (RV, ASV, RSV, NIV, ESV, NASB, CSB, etc.) are
 *   declared corrupted, inferior, or spiritually dangerous because they rely
 *   on 'corrupt' Alexandrian manuscripts (Sinaiticus, Vaticanus) rather than
 *   the 'received' Byzantine/Textus Receptus tradition. This reading
 *   functions as a gate-keeping constraint: it suppresses alternative
 *   translations, delegates textual authority exclusively to KJV-Only
 *   leadership, and extracts loyalty, labor, and revenue from adherents. The
 *   constraint is actively enforced through preaching, curricula, ordination
 *   requirements, and social discipline. The claimed type is snare: the
 *   coordination story (one true Bible for unity) is cover for extraction
 *   (control of textual authority and institutional revenue).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.82).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.85).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, snare).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration and Inerrancy Reading").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/textual/theological").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '62efba8b-7e0a-4063-b519-92fd5c556dd2').
narrative_ontology:cs_kernel_codification('62efba8b-7e0a-4063-b519-92fd5c556dd2', fixed_text).
narrative_ontology:cs_authority_grounding('62efba8b-7e0a-4063-b519-92fd5c556dd2', lineage).
narrative_ontology:cs_interpretation_layer_present('62efba8b-7e0a-4063-b519-92fd5c556dd2').
narrative_ontology:cs_reading_relation('62efba8b-7e0a-4063-b519-92fd5c556dd2', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('62efba8b-7e0a-4063-b519-92fd5c556dd2', kjv_text_1611__revisable_translation_reading, coexists_with).
narrative_ontology:cs_axiom('62efba8b-7e0a-4063-b519-92fd5c556dd2', foundational, kjv_exclusively_inspired).
narrative_ontology:cs_axiom_status(kjv_exclusively_inspired, holdable).
narrative_ontology:cs_axiom_grounding('62efba8b-7e0a-4063-b519-92fd5c556dd2', kjv_exclusively_inspired, theological).
narrative_ontology:cs_axiom('62efba8b-7e0a-4063-b519-92fd5c556dd2', foundational, modern_translations_corrupted).
narrative_ontology:cs_axiom_status(modern_translations_corrupted, holdable).
narrative_ontology:cs_axiom_grounding('62efba8b-7e0a-4063-b519-92fd5c556dd2', modern_translations_corrupted, theological).
narrative_ontology:cs_reference_frame('62efba8b-7e0a-4063-b519-92fd5c556dd2', preserved_textual_perfection).
narrative_ontology:cs_drift_state('62efba8b-7e0a-4063-b519-92fd5c556dd2', contemporary_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('62efba8b-7e0a-4063-b519-92fd5c556dd2', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutions).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, kjv_only_laity).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_users).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, textual_scholars).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, competing_publishers).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, textual_preservation_doctrine).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, providential_preservation_of_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors, denominational officials, and seminar professors who define and enforce the exclusive inspiration doctrine. They control pulpits, curricula, and ordination requirements. They benefit from absolute interpretive authority, institutional loyalty, and revenue from KJV-only publishing ventures. Exit is near-arbitrage: they can move between KJV-Only institutions or launch independent ministries without losing status.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Bible colleges, mission boards, and publishing houses (e.g., Trinitarian Bible Society, local KJV-Only presses) that depend on the doctrine for donor base, enrollment, and product lines. They collect tuition, donations, and publication revenue tied to the exclusive text. Institutional exit is arbitrage-grade: they can rebrand or shift emphasis without losing infrastructure.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutions, beneficiary,
    organized, generational, arbitrage, global).

% Commercial and non-profit publishers producing KJV-only study Bibles, commentaries, and curricula. They benefit from a captive market that treats competing products as spiritually dangerous. Exit is mobile: they can pivot to broader evangelical publishing if the market shifts.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishers, beneficiary,
    organized, biographical, mobile, global).

% Congregants in KJV-Only churches who bear the costs: restricted access to scholarly resources, cognitive dissonance when confronting manuscript evidence, social ostracism for questioning, and financial support for institutions that gatekeep their spiritual assurance. Their identity is fused to the doctrine — questioning the text feels like questioning salvation. Exit means leaving community, family, and epistemic framework.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_laity, payer,
    organized, biographical, identity_locked, national).

% Believers using NIV, ESV, NASB, CSB, etc., who are explicitly taught their Bibles are corrupted, dangerous, or spiritually inferior. They bear reputational costs in KJV-Only spaces and are denied fellowship/leadership. Exit is constrained: they can stay in their own traditions but cannot participate in KJV-Only communities without submitting to the doctrine.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_users, payer,
    organized, biographical, constrained, global).

% Academic textual critics, linguists, and historians (secular and evangelical) whose work on manuscript families, textual variants, and translation theory is categorically dismissed as unbelief or corruption. They are excluded from the epistemic community; their objections are pre-emptively neutralized by the doctrine's hermeneutic of suspicion.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, textual_scholars, excluded,
    analytical, generational, analytical, global).

% Major Bible publishers (Crossway, Zondervan, Holman, Tyndale) whose products are declared illegitimate. They are structurally locked out of the KJV-Only market segment — a significant revenue pool — by the doctrine's anathemas. They cannot access this market without abandoning their own translational philosophy.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, competing_publishers, excluded,
    powerful, biographical, trapped, global).

% Scholars of religion, sociology of knowledge, and theology who analyze the KJV-Only movement as a social phenomenon. They neither collect nor pay; they map the constraint's operation from outside the commitment framework.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, religious_studies_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, certain textual authority for the believing community, eliminating interpretive chaos from multiple translations and anchoring corporate worship, memorization, and doctrinal formulation in one stable English text.
% TRANSFER_FUNCTION: Moves interpretive authority and textual gatekeeping from diverse scholarly traditions and denominational structures to KJV-Only leadership; moves financial support (tithes, tuition, publication purchases) from laity to KJV-Only institutions and publishers; moves epistemic legitimacy from academic textual criticism to a closed doctrinal system.
% ABSENT_VOICES: Textual critics (Metzger, Ehrman, Comfort, Wallace), historical theologians, linguists, and believers who use modern translations are structurally excluded. Their objections are dismissed as rationalism, unbelief, or Satanic corruption. The doctrine's hermeneutic of suspicion treats dissent as evidence of spiritual defect, not evidence to be weighed.
% DISAPPEARANCE_RATIONALE: If the exclusive inspiration constraint vanished overnight, KJV-Only communities would lose their primary boundary marker and authority structure. Congregations would fragment or adopt mainstream translations; institutions would lose enrollment and donor bases; publishers would lose a protected market. The textual authority vacuum would be filled by scholarly consensus translations (ESV, NASB, CSB) within months.
% FOUNDING_PROBLEM: Late 19th-century textual criticism (Westcott-Hort Greek text, 1881 Revised Version) replaced the Textus Receptus with an eclectic critical text based on Alexandrian manuscripts. KJV-Only advocates perceived this as a corruption of God's preserved Word and a threat to scriptural authority, prompting a movement to defend the KJV as the sole inspired English Bible.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream textual scholars (Bruce Metzger, Daniel Wallace, Philip Comfort) and major denominations (SBC, PCA, EPC official statements) attest the textual controversy is substantially resolved: the critical text is methodologically sound, the TR is a late Byzantine form, and modern translations faithfully represent the earliest manuscripts. KJV-Only sources (Riplinger, Waite, Fuller Seminary KJV-Only faculty) self-attest the threat persists; no non-KJV-Only authority corroborates the founding problem's persistence.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers interpretive authority, financial resources, and epistemic autonomy from laity and scholars to a closed leadership class. Suppression is very high (0.85) because persistence depends on actively delegitimizing all alternatives — manuscript evidence, linguistic scholarship, and competing translations are pre-emptively dismissed via a hermeneutic of suspicion. Theater ratio is moderate (0.38): the textual defense apparatus (journals, conferences, 'Bible version' debates) performs scholarly rigor but operates within a closed conclusion. Accessibility collapse is high (0.82): once the doctrine is accepted, alternatives are not merely disfavored but categorized as spiritually fatal. Resistance is moderate (0.48): scholarly resistance exists but is excluded from the community; internal dissent is suppressed by identity-lock.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat, the constraint appears as rope: a genuine coordination solution to textual uncertainty. From the laity seat, it computes as snare: high extraction with identity-locked exit. From the scholar seat, it appears as mountain (false summit): a claim to natural textual fact that is historically constructed. The engine computes these divergences from the structural data; the authored claim (snare) reflects the generating model's assessment of the constraint's actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-Only leadership and institutions are structural beneficiaries (d ~ 0.1): they collect authority, revenue, and loyalty. KJV-Only laity are identity-locked payers (d ~ 0.9): they bear epistemic, social, and financial costs but cannot exit without losing their self-concept. Modern translation users are constrained payers (d ~ 0.7): they bear reputational exclusion but have alternative communities. Textual scholars and competing publishers are excluded (d not computed — they are outside the constraint's direct governance but structurally suppressed by it). The engine will derive directionality from these declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defending the TR against 19th-century critical text) is contested: scholars and mainline denominations hold it is resolved; KJV-Only leadership claims it persists. The constraint persists not because the founding problem is live, but because the leadership's authority and revenue now depend on the doctrine itself — a classic mandatrophy pattern. The constraint has outlived its founding function and survives through active enforcement and identity-lock.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading of kernel kjv_text_1611 (reading_id: exclusive_inspiration_reading). Sibling readings: functional_equivalence_reading, revisable_translation_reading. What structural elements differ across readings?',
    'Compare each reading''s beneficiary/victim sets, extractiveness referents, and authority structures. The kernel_id and reading_id are recorded here to route committer-frame content through the omega infrastructure per Rules 2–5.',
    'Ensures the ε-invariance principle holds: each reading gets its own ε, stakeholders, and classification. The engine computes per-reading types; divergence across readings measures the kernel''s contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing: this constraint is a kernel reading, not a standalone constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) primarily structural (institutional discipline, social ostracism) or internalized (identity-fused belief that questioning equals apostasy)?',
    'Post-exit trajectory study: track former KJV-Only adherents. If suppression feelings persist after leaving the community (fear of other translations, guilt reading modern versions), reclassify as partially internalized. If suppression lifts immediately, it was structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, affecting χ for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for identity-locked laity.').

omega_variable(
    coordination_extraction_boundary,
    'Does the single-text coordination function (unified worship, memorization, doctrinal stability) genuinely require exclusive inspiration, or is the coordination achievable without the extraction (gatekeeping, anathemas)?',
    'Natural experiment: compare KJV-Only communities with confessional communities that use a single translation (e.g., ESV in some Reformed churches) without claiming exclusive inspiration. If coordination holds without extraction, the extraction is separable.',
    'If coordination is separable from exclusive inspiration, the constraint is pure snare. If exclusive inspiration is structurally necessary for the coordination, it is tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    textual_basis_ambiguity,
    'Does the Textus Receptus / Byzantine manuscript tradition actually support the claim of exclusive providential preservation, or is the textual basis itself a constructed preference?',
    'Manuscript collation data: the TR is based on ~6 late Byzantine manuscripts (12th–15th c.); the critical text uses 5,800+ manuscripts including 2nd–4th c. papyri. If the TR''s manuscript basis is demonstrably late and narrow, the preservation claim is historically falsifiable.',
    'If the textual basis is falsified, the constraint''s founding axiom (providential preservation of the TR) is empirically_contingent and overridden — triggering axiom_overriding drift. If the basis holds, the claim remains theological (unfalsifiable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_basis_ambiguity, empirical, 'Whether the textual-critical premise of exclusive inspiration is empirically falsifiable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_excl_inerrancy_tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kjv_excl_inerrancy_tr_t15, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(kjv_excl_inerrancy_tr_t30, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(kjv_excl_inerrancy_tr_t45, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(kjv_excl_inerrancy_tr_t60, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(kjv_excl_inerrancy_tr_t74, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 74, 0.38).

% Extraction over time
narrative_ontology:measurement(kjv_excl_inerrancy_be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(kjv_excl_inerrancy_be_t15, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(kjv_excl_inerrancy_be_t30, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(kjv_excl_inerrancy_be_t45, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 45, 0.78).
narrative_ontology:measurement(kjv_excl_inerrancy_be_t60, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 60, 0.81).
narrative_ontology:measurement(kjv_excl_inerrancy_be_t74, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 74, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(kjv_excl_inerrancy_su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(kjv_excl_inerrancy_su_t15, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(kjv_excl_inerrancy_su_t30, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(kjv_excl_inerrancy_su_t45, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 45, 0.8).
narrative_ontology:measurement(kjv_excl_inerrancy_su_t60, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 60, 0.83).
narrative_ontology:measurement(kjv_excl_inerrancy_su_t74, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 74, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__exclusive_inspiration_reading, 0.08).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% KJV text 1611 kernel family: three readings with divergent ε and stakeholder structures. exclusive_inspiration_reading (this story) has high ε (0.82) and snare classification; functional_equivalence_reading and revisable_translation_reading have lower ε and rope/tangled_rope classifications. The upstream scholarly consensus (critical text methodology) influences downstream readings; this reading resists that influence via hermeneutic of suspicion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, organized, 0.85).
constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, analytical, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
