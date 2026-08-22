% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV-Only Exclusive Inspiration Doctrine
 *   domain: religious/theological/textual_criticism
 *
 * SUMMARY:
 *   This story instantiates the exclusive_inspiration_reading of the KJV
 *   kernel: the claim that the King James Version alone is the inspired,
 *   inerrant English Bible and that all other translations are corrupted or
 *   spiritually inferior. This is a minority but institutionally entrenched
 *   position within a subset of fundamentalist and independent Baptist
 *   Protestantism, distinct from the mainstream evangelical view that treats
 *   the KJV as one historically significant translation among several
 *   defensible ones (the revisable_translation_reading and
 *   functional_equivalence_reading, authored as separate sibling
 *   constraints). Under this reading, the doctrine functions as a
 *   coordination device (a stable textual standard for preaching and
 *   memorization) fused with an extraction mechanism (institutional and
 *   financial capture built on policing translation choice as a marker of
 *   orthodoxy). ε is authored for the exclusive-inspiration arrangement as
 *   this reading's own advocates present and enforce it — not for whatever
 *   alternative textual policy critics would prefer.
 *
 * KEY AGENTS:
 *   - kjv_only_leadership: agenda_setter (institutional/arbitrage) — administers doctrine, collects authority and revenue
 *   - kjv_only_publishing_houses: beneficiary (organized/arbitrage) — sells into captive apologetics market
 *   - kjv_only_bible_colleges: beneficiary (institutional/arbitrage) — trains and credentials propagators
 *   - congregants_under_kjv_only_pastors: payer (powerless/constrained) — bears social and relational cost of compliance
 *   - modern_translation_readers: payer (moderate/mobile) — bears reputational attack, has exit
 *   - biblical_scholars_and_textual_critics: payer/excluded (moderate/mobile) — delegitimized without evidentiary engagement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.71).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.68).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV-Only Exclusive Inspiration Doctrine").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/theological/textual_criticism").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '3d26ece0-6457-454d-967a-3033799f3789').
narrative_ontology:cs_kernel_codification('3d26ece0-6457-454d-967a-3033799f3789', fixed_text).
narrative_ontology:cs_authority_grounding('3d26ece0-6457-454d-967a-3033799f3789', extraction).
narrative_ontology:cs_interpretation_layer_present('3d26ece0-6457-454d-967a-3033799f3789').
narrative_ontology:cs_reading_relation('3d26ece0-6457-454d-967a-3033799f3789', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('3d26ece0-6457-454d-967a-3033799f3789', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('3d26ece0-6457-454d-967a-3033799f3789', foundational, single_inspired_english_text_exists).
narrative_ontology:cs_axiom_status(single_inspired_english_text_exists, holdable).
narrative_ontology:cs_axiom_grounding('3d26ece0-6457-454d-967a-3033799f3789', single_inspired_english_text_exists, theological).
narrative_ontology:cs_axiom('3d26ece0-6457-454d-967a-3033799f3789', foundational, textus_receptus_providentially_preserved_without_error).
narrative_ontology:cs_axiom_status(textus_receptus_providentially_preserved_without_error, holdable).
narrative_ontology:cs_axiom_grounding('3d26ece0-6457-454d-967a-3033799f3789', textus_receptus_providentially_preserved_without_error, theological).
narrative_ontology:cs_axiom('3d26ece0-6457-454d-967a-3033799f3789', secondary, modern_critical_texts_are_corrupted_transmission).
narrative_ontology:cs_axiom_status(modern_critical_texts_are_corrupted_transmission, holdable).
narrative_ontology:cs_axiom_grounding('3d26ece0-6457-454d-967a-3033799f3789', modern_critical_texts_are_corrupted_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('3d26ece0-6457-454d-967a-3033799f3789', verbal_plenary_preservation_1611).
narrative_ontology:cs_drift_state('3d26ece0-6457-454d-967a-3033799f3789', post_critical_text_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d26ece0-6457-454d-967a-3033799f3789', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishing_houses).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_bible_colleges).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_readers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, congregants_under_kjv_only_pastors).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, biblical_scholars_and_textual_critics).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, verbal_plenary_preservation_doctrine).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors, seminary heads, and denominational leaders who teach that the KJV alone is inspired and inerrant, and who administer church discipline, ordination, and membership standards around this claim. They author conference materials, run publishing houses, and control pulpits; they set the doctrine and benefit from the authority and revenue it generates. Their own exit from the claim is costless relative to those under them — they can quietly moderate their public teaching without losing institutional position, while those below them cannot dissent without consequence.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, beneficiary).

% Publish KJV-only study materials, commentaries, and apologetics works defending exclusive inspiration; sell into a captive market created by the doctrine's exclusivity claim. Revenue depends on the doctrine remaining unsettled in believers' minds as a live controversy requiring continual defense.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishing_houses, beneficiary,
    organized, generational, arbitrage, national).

% Train pastors and missionaries under statements of faith requiring adherence to KJV exclusivity; tuition and accreditation within their own network depend on graduates propagating the doctrine to future congregations.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_bible_colleges, beneficiary,
    institutional, generational, arbitrage, national).

% Lay members taught that using an NIV or ESV is a mark of spiritual compromise or worse, apostasy. Reading modern translations, using study tools built on critical texts, or questioning KJV exclusivity risks being labeled unsound, disciplined, or ostracized from their faith community. Exit means leaving the only religious and often social community they have, at real relational and sometimes economic cost.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, congregants_under_kjv_only_pastors, payer,
    powerless, biographical, constrained, local).

% Christians who use NIV, ESV, NASB, or other modern translations, and are told by KJV-only advocates that their Bibles are corrupted, satanically altered, or invalid for salvation-relevant doctrine. They bear reputational attack and exclusion from KJV-only fellowship, though they typically have other church options available.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_readers, payer,
    moderate, biographical, mobile, national).

% Textual critics whose manuscript work (papyri discoveries, critical apparatus, Nestle-Aland editions) is dismissed wholesale as demonic corruption by KJV-only apologetics, regardless of the specific evidence. Their scholarship is delegitimized by definition rather than engaged, and they have no standing within KJV-only institutions to contest the claim on evidentiary grounds.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, biblical_scholars_and_textual_critics, payer,
    moderate, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, biblical_scholars_and_textual_critics, excluded).

% The historical translators and their underlying Greek text base are invoked as the doctrine's authority but are not agents in the present dispute; included for completeness since the doctrine claims their work as uniquely inspired.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_and_translators_1611, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_and_translators_1611).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, memorizable textual standard for preaching, memorization, and doctrinal cross-reference within a tradition, removing the need for individual congregations to adjudicate manuscript disputes themselves.
% TRANSFER_FUNCTION: Moves authority, institutional loyalty, tuition and publishing revenue, and social standing toward KJV-only leadership and their affiliated institutions, and moves reputational and relational costs onto congregants and scholars who use or defend other translations.
% ABSENT_VOICES: Textual critics and mainstream translation committees are never invited to defend their methodology inside KJV-only institutions; their work is characterized rather than examined. Congregants raised in the tradition who privately doubt the doctrine rarely voice this within the community for fear of discipline, so their dissent is structurally invisible to the leadership that sets the doctrine.
% DISAPPEARANCE_RATIONALE: If exclusive-inspiration doctrine vanished overnight, KJV-only churches would need to renegotiate translation policy, some publishing houses and colleges built specifically around defending the doctrine would lose their reason for existing, and congregants currently barred from using modern translations or study tools would regain access to a much larger body of scholarship and Christian community without institutional penalty.
% FOUNDING_PROBLEM: Nineteenth and twentieth century textual criticism (Westcott-Hort, later critical editions) produced Greek New Testament texts differing from the Textus Receptus underlying the KJV, and new English translations proliferated; some conservative Protestants perceived this as doctrinal erosion via manuscript changes and translation committees they did not trust, and sought a fixed, defensible standard.
% FOUNDING_PROBLEM_CORROBORATION: KJV-only leadership attests the problem remains live: that modern textual criticism and translations continue to corrupt scripture. Mainstream evangelical textual scholars, translation committees, and denominational bodies outside the KJV-only movement attest that the underlying manuscript questions have been extensively studied and resolved by consensus scholarly method, and that the KJV-only doctrine as formulated is a minority position not shared by the historic church prior to the 20th century; this outside corroboration is documented in seminary-level textual criticism literature and cross-denominational statements on Bible translation.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.71) reflects that the doctrine's persistence depends on continual apologetics revenue, tuition, and pulpit authority rather than settled textual evidence — it rose over the measured interval as KJV-only institutions professionalized and the doctrine hardened from an informal preference into a tested-and-enforced boundary marker. Suppression (0.68) is real but structurally asymmetric: it operates almost entirely through social, relational, and institutional consequence (church discipline, family and community estrangement, denial of fellowship) rather than through any external legal mechanism. Theater ratio (0.44) is moderate-high because a substantial share of KJV-only apologetics activity — manuscript-history seminars, 'received text' conferences — functions to perform textual scholarship for an internal audience already committed, rather than to genuinely engage the counter-evidence.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the doctrine looks like faithful stewardship of a settled inheritance — coordination around a fixed, trustworthy text. From the payer seats, particularly congregants with constrained exit, the same structure operates as an enforced boundary that converts a translation preference into a test of salvation-adjacent orthodoxy, with real social cost for noncompliance. The engine computes these as different types from the same structural data; the divergence is exactly the phenomenon the tangled_rope classification is built to register.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-only leadership and affiliated institutions sit at the beneficiary end: they administer the doctrine, control its enforcement, and derive revenue, credentialing power, and pulpit authority from it, with arbitrage-grade exit (they can moderate privately without losing standing). Congregants under KJV-only pastors sit nearest the full-target end: powerless, constrained exit (leaving means losing their faith community), and the highest relational stakes. Modern translation readers and outside scholars are targets of the doctrine's rhetoric but retain mobile exit — they can simply attend elsewhere or publish elsewhere — so their effective extraction, while real, is lower than that of trapped congregants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distrust of 19th/20th century critical-text scholarship and new translation committees) is contested rather than dead: within the KJV-only tradition it is treated as fully live, while outside corroborating sources (mainstream textual criticism, denominational bodies) treat the underlying manuscript questions as substantially resolved by scholarly consensus methods unavailable or unknown to the doctrine's originators. The status=contested / verdict=world_rearranges combination flags a live capture-risk profile requiring the mismatch check: institutions built specifically to defend a still-asserted-live problem, evaluated by outside sources as resolved, are a canonical zombie-mandate pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manuscript_evidence_vs_doctrinal_commitment,
    'Is KJV exclusivity a defensible conclusion from textual evidence (Textus Receptus superiority, providential preservation) or a doctrinal commitment that textual arguments are marshaled to defend after the fact?',
    'Independent evaluation of the manuscript evidence by textual critics without prior institutional stake in either KJV-only or critical-text traditions, compared against how KJV-only apologetics engages or dismisses specific counter-evidence (e.g. papyri dating earlier than Byzantine text-type witnesses).',
    'If the doctrine is evidence-led, the natural-law-flavored ''settled text'' framing has more warrant and the tangled_rope classification should weight coordination more heavily; if doctrine-led, the classification should weight extraction and gatekeeping more heavily, closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manuscript_evidence_vs_doctrinal_commitment, empirical, 'Whether exclusivity claim is evidence-derived or doctrine-first with post-hoc evidentiary framing.').

omega_variable(
    kernel_reading_divergence_source,
    'What specific structural element do the three kernel readings (exclusive_inspiration, revisable_translation, functional_equivalence) actually disagree about — the historical translation act, the underlying manuscript tradition, or the doctrine of inspiration itself?',
    'Decompose the disagreement claim-by-claim: readings may agree on 1611 translation history while disagreeing entirely on whether inspiration is a property of manuscripts, translations, or the communicative act; mapping this precisely would clarify whether ''forecloses'' or ''coexists_with'' is the correct relation to each sibling.',
    'If the disagreement is purely about inspiration doctrine (a theological axiom) and not about manuscript facts, the exclusive_inspiration_reading and revisable_translation_reading may be less foreclosing of each other than assumed, since a revisionist could still hold a strong (non-exclusive) inspiration view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_source, conceptual, 'Locating precisely where the three sibling readings diverge structurally.').

omega_variable(
    suppression_internalization_congregants,
    'For congregants raised inside KJV-only communities, is the suppression they experience primarily structural (church discipline, social exclusion enforced from outside) or internalized (having absorbed the belief that using another translation endangers their soul, such that the fear persists even after leaving the community)?',
    'Track post-exit trajectories: interview former KJV-only congregants who have left the tradition about whether anxiety about ''corrupted'' translations persists years after institutional pressure is removed.',
    'If substantially internalized, effective suppression for this population is higher than the structural measure alone suggests, since the constraint travels with the person after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_congregants, empirical, 'Structural versus internalized suppression mechanism for congregants under the doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(kjv__tr_t50, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 50, 0.43).
narrative_ontology:measurement(kjv__tr_t60, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 60, 0.44).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(kjv__be_t50, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(kjv__be_t60, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(kjv__su_t30, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(kjv__su_t50, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(kjv__su_t60, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the kjv_text_1611 kernel. exclusive_inspiration_reading authors high extraction and an active victim set (modern translation users, textual scholars, constrained congregants) because it is the only reading whose core premise requires suppressing rival translations as illegitimate. revisable_translation_reading and functional_equivalence_reading, as siblings, would author near-zero extraction and no comparable victim set, since neither treats translation plurality as heretical. The three share the same historical kernel object (the 1611 KJV and its textual lineage) but diverge sharply on ε because they make structurally incompatible claims about what inspiration attaches to and what follows from that for other translations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
