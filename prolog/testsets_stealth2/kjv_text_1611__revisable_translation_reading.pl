% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Scholarly Revision and Licensed Modern Translation Regime (Revisable-Translation Reading of the KJV)
 *   domain: religious/textual_criticism/publishing
 *
 * SUMMARY:
 *   Under the revisable-translation reading, the operative arrangement is not
 *   the 1611 text itself but the apparatus that has grown around improving
 *   it: critical editions of the Hebrew and Greek, standing translation
 *   committees, and the publishing houses that commission, own, and license
 *   the resulting English Bibles. Manuscript discoveries (Sinaiticus and
 *   Vaticanus in the 19th century, the papyri through the 20th) and
 *   linguistic scholarship drove successive revisions — RV, ASV, RSV, NIV,
 *   ESV, CSB — while the ownership model shifted from public-domain committee
 *   work to corporate copyright. The reading treats the KJV as honored
 *   predecessor, not final authority; the contest this story measures is
 *   whether the improvement apparatus serves faithful access or has layered
 *   publisher rent onto a scholarly commons. Constraint family: this is one
 *   of three readings of the kjv_text_1611 kernel; the epsilon authored here
 *   (0.62, referent = the licensed-revision arrangement as this reading sees
 *   it) differs structurally from the exclusive-inspiration sibling (single
 *   mandated text, high suppression) and the functional-equivalence sibling
 *   (pluralism, minimal extraction). Claim and metrics are authored
 *   independently: the claimed type states the structure this reading
 *   believes true; the metrics state what the arrangement's operation looks
 *   like from the record.
 *
 * KEY AGENTS:
 *   - - bible_publishing_houses: agenda-setting beneficiary (institutional/arbitrage) — owns modern translation copyrights, sets licensing terms, collects the margin
 *   - - academic_textual_critics: beneficiary (organized/mobile) — supply the critical editions and philological authority the arrangement runs on
 *   - - translation_committees: beneficiary with agenda-setting duties (organized/mobile) — produce the texts under publisher commission
 *   - - lay_bible_readers: primary payer (powerless/constrained) — buy copies, bound by quotation limits, public-domain fallback only
 *   - - independent_scripture_ministries: payer (moderate/constrained) — pay licensing fees to distribute contemporary-language scripture
 *   - - open_scripture_translators: excluded voice (moderate/trapped) — free-licensed producers outside the room
 *   - - biblical_studies_reviewers: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.62).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.42).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "Scholarly Revision and Licensed Modern Translation Regime (Revisable-Translation Reading of the KJV)").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious/textual_criticism/publishing").

domain_priors:requires_active_enforcement(kjv_text_1611__revisable_translation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, '311163ed-49ce-4f16-8178-c9bd2924c53b').
narrative_ontology:cs_kernel_codification('311163ed-49ce-4f16-8178-c9bd2924c53b', fixed_text).
narrative_ontology:cs_authority_grounding('311163ed-49ce-4f16-8178-c9bd2924c53b', expertise).
narrative_ontology:cs_interpretation_layer_present('311163ed-49ce-4f16-8178-c9bd2924c53b').
narrative_ontology:cs_reading_relation('311163ed-49ce-4f16-8178-c9bd2924c53b', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('311163ed-49ce-4f16-8178-c9bd2924c53b', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('311163ed-49ce-4f16-8178-c9bd2924c53b', foundational, manuscript_evidence_overrides_tradition).
narrative_ontology:cs_axiom_status(manuscript_evidence_overrides_tradition, holdable).
narrative_ontology:cs_axiom_grounding('311163ed-49ce-4f16-8178-c9bd2924c53b', manuscript_evidence_overrides_tradition, empirically_contingent).
narrative_ontology:cs_axiom('311163ed-49ce-4f16-8178-c9bd2924c53b', foundational, continual_revision_serves_faithful_access).
narrative_ontology:cs_axiom_status(continual_revision_serves_faithful_access, holdable).
narrative_ontology:cs_axiom_grounding('311163ed-49ce-4f16-8178-c9bd2924c53b', continual_revision_serves_faithful_access, instrumental).
narrative_ontology:cs_reference_frame('311163ed-49ce-4f16-8178-c9bd2924c53b', historically_revisable_english_scripture).
narrative_ontology:cs_drift_state('311163ed-49ce-4f16-8178-c9bd2924c53b', contemporary_digital_manuscript_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('311163ed-49ce-4f16-8178-c9bd2924c53b', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, bible_publishing_houses).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_textual_critics).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, translation_committees).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, lay_bible_readers).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, independent_scripture_ministries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, lay_bible_readers).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, eclectic_critical_text_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commission new translations and revisions, hold the copyrights, and license printing, quotation, and digital distribution. Set the terms under which churches and ministries may reproduce the texts, and collect royalties and retail margin. Can redirect capital to whichever version sells, acquire rival imprints, or launch successor editions; leaving scripture publishing altogether is commercially available though reputationally costly.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, bible_publishing_houses, agenda_setter,
    institutional, generational, arbitrage, global).

% Produce and maintain the critical editions of the Hebrew and Greek source texts and publish the philological judgments that translation committees adopt. Employment, journal space, and professional standing flow through the revision enterprise. Mobile between universities, institutes, and committee appointments; the expertise itself travels with them.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_textual_critics, beneficiary,
    organized, generational, mobile, global).

% Convened and funded by publishers and denominations to turn critical editions into English texts; decide wording, register, and study notes. Members are paid and gain professional visibility; the committee's continued existence depends on publisher sponsorship, so its decision-making discretion operates inside commercially set boundaries.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, translation_committees, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, translation_committees, agenda_setter).

% Purchase Bibles and apps and receive measurably clearer, more accurate texts than the archaic standard. Face a crowded shelf of copyrighted options with differing registers and doctrinally angled study notes; may quote within limits but may not copy, adapt, or freely redistribute most modern versions. A public-domain fallback exists but reads as archaic, so practical exit from the licensed market is limited.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, lay_bible_readers, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, lay_bible_readers, beneficiary).

% Distribute scripture at scale through bulk print runs, app embedding, and further-language translation. Contemporary-language texts require negotiated permissions and per-copy or per-print fees; budgets cap how much they may reproduce. Their mission depends on texts they do not own and cannot modify.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, independent_scripture_ministries, payer,
    moderate, biographical, constrained, global).

% Volunteer communities maintaining free-licensed English translations and digital tools. They argue the texts should be reproducible by anyone and demonstrate the work can be done without royalties. They hold no seat in publisher committees and reach readers mainly through downloads rather than bookstore channels.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, open_scripture_translators, excluded,
    moderate, biographical, trapped, global).

% Academic reviewers who assess translations for fidelity, register, and paratext bias; publish comparisons and critiques. Hold no copyright stake and collect no royalties; their assessments feed seminary curricula and press coverage.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, biblical_studies_reviewers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__revisable_translation_reading, bible_publishing_houses).
narrative_ontology:fixing_cost_class(kjv_text_1611__revisable_translation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of rendering fragmentary, variant ancient manuscripts into accurate contemporary English: pools scarce philological expertise, maintains critical editions of the Hebrew and Greek texts, and funds continuous revision through exclusive publication rights.
% TRANSFER_FUNCTION: Moves licensing fees and retail margin from Bible purchasers and distribution ministries to publishing houses; moves scholarly labor from universities and churches into publisher-owned copyrighted texts; moves interpretive authority from clerical tradition to credentialed critical scholarship.
% ABSENT_VOICES: Open-licensed translation communities and plain-text scripture advocates are outside the committee-and-publisher rooms; lay readers hold no seat (their preferences enter only as sales data); KJV-only traditionalists object from outside this reading's frame and are discounted as anti-scholarly within it.
% DISAPPEARANCE_RATIONALE: If the licensed-revision arrangement vanished overnight, modern translations would cease updating under their current owners, backlists would scatter into legal limbo, and production would reorganize around public-domain texts, denominational patronage, and open-licensed projects; congregations would lose the polished contemporary texts they currently buy unless new patrons replaced publisher funding.
% FOUNDING_PROBLEM: Keeping vernacular scripture faithful to the best attainable source texts: first the 16th-17th century problem of a defective standard translation amid newly available Hebrew and Greek scholarship, then the continuing problem of absorbing manuscript discoveries and linguistic change into English texts.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by manuscript-holding institutions (British Library, Bibliothèque nationale, Vatican Library) whose facsimile and digitization programs document continuing textual uncertainty; by university philology departments publishing critical editions without royalty stakes; and by cross-denominational scholarly bodies whose proceedings treat revision as ongoing necessity. No attesting source within the publishing industry is relied upon.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.62 because the arrangement's rents are real but bounded: Bibles remain inexpensive relative to comparable books, generous quotation allowances soften day-to-day friction, yet exclusive rights on texts derived from public-domain sources and church-funded scholarship concentrate durable margin in a few houses. Suppression is authored low (0.42) relative to what a single-mandated-text arrangement would show — translation selection is consumer choice — but nonzero because copyright machinery actively polices reproduction and adaptation. Theater_ratio 0.45 reflects a revision cadence that increasingly serves shelf differentiation alongside manuscript advance. Accessibility_collapse is low (0.22): alternatives abound and none collapse on inspection. Resistance 0.35 captures open-licensing advocacy, traditionalist pushback, and episodic licensing controversies. All three temporal series run on one shared grid (1881, 1920, 1952, 1978, 2001, 2026): the rising base_extractiveness series models rent layering onto a coordination core, the rising suppression_requirement series models licensing enforcement maturing from the public-domain committee era into corporate IP enforcement, and the rising theater series models marketing-driven revision cycles. Suppression is authored as a raw structural property; the engine alone scales extractiveness by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the publishing-house seat the arrangement is earned return on decades of capital risk — a fair bargain it administers. From the textual-scholar seat it is a vocational system that funds precision work no patron otherwise would. From the lay-reader seat it is a crowded marketplace with legal fences around texts many consider communal inheritance; individually powerless, readers coordinate only as purchasers, which disciplines pricing but not licensing terms. From the ministry seat it is a toll gate on mission. The engine computes these divergent per-seat classifications from the declared positions; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishing houses anchor the beneficiary pole: they set terms and collect the margin, with arbitrage-grade exit across imprints and editions. Textual scholars and committees sit nearer the beneficiary side with mobile exits — they collect standing and income but do not own the rents. Lay readers and independent ministries anchor the target pole: they pay through purchase prices and licensing fees, their exits constrained by the archaic public-domain fallback and by mission dependence respectively. Open-scripture translators, excluded from the arrangement, would sit near the target pole if seated. Global scope raises verification difficulty and thus amplifies effective extraction for the paying seats, per the engine's scope handling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping vernacular scripture faithful to the best attainable source texts as manuscripts surface and language moves — is still live, corroborated by continuing manuscript publication and linguistic change, so this is not a mandate outliving its function; the dead-status-times-rearranging-world mismatch flag does not fire. The tangled-rope classification prevents mislabeling in both directions: a pure-coordination reading would erase the concentrated royalty capture that publishing houses demonstrably accrue, while a pure-extraction reading would erase the real funded scholarship and accuracy gains the arrangement delivers. Rising theater_ratio tracks marketing-cycle revision rather than functional decay — the function persists beneath the performance, which is why this is not drifting toward inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'This story instantiates the revisable_translation_reading of the kjv_text_1611 kernel; how would the classification shift if the exclusive_inspiration_reading or functional_equivalence_reading were instantiated instead?',
    'Compare against the sibling stories'' structural declarations; the engine classifies each reading as its own constraint with its own epsilon, beneficiaries, and victims.',
    'Under the exclusive-inspiration sibling the arrangement becomes a single-mandated-text structure with high suppression and identifiable dissenting victims; under the functional-equivalence sibling it relaxes toward low-extraction pluralism. This story''s mixed coordination-plus-capture profile holds only for the revisable reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer structure: which reading of the KJV kernel this constraint is, and what siblings would change.').

omega_variable(
    copyright_funding_necessity,
    'Are exclusive publication rights a necessary funding mechanism for translation scholarship, or replaceable by patronage, institutional funding, and open-licensed models?',
    'Natural experiments from open-licensed translations (free-licensed community projects), crowdfunded revision efforts, and denominationally patronized committees: if textual quality and update cadence hold without exclusive rights, the funding justification weakens.',
    'If replaceable, publisher margins are pure overhead riding on a scholarly commons and effective extraction rises toward the target pole for all paying seats; if necessary, a portion of the measured extraction is coordination cost rather than rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyright_funding_necessity, empirical, 'Whether the licensing regime is load-bearing for the scholarship it funds.').

omega_variable(
    revision_cycle_driver,
    'Does the post-1970 revision cadence track manuscript discovery and philological advance, or market differentiation between competing imprints?',
    'Correlate edition release dates with significant manuscript publications and measure the magnitude of textual change between successive editions versus packaging and paratext change.',
    'If market-driven, the true theater_ratio exceeds the authored 0.45 and the arrangement drifts toward performative maintenance of the revision ideal; if evidence-driven, the theater score stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revision_cycle_driver, empirical, 'Whether revision cycles serve the reading''s stated justification or shelf competition.').

omega_variable(
    lay_reader_net_position,
    'Does translation abundance net-help or net-harm lay readers — accuracy and register gains versus choice paralysis, doctrinally skewed study notes, and loss of a shared common text?',
    'Comprehension and usage studies across translation ecosystems, including comparison with communities standardized on a single text.',
    'If net harm, the lay_bible_readers victim declaration strengthens and their directionality moves toward the full-target end; if net benefit, their position shifts toward symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_reader_net_position, empirical, 'Net welfare of the paying reader seat under fragmentation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 1881, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1881, kjv_text_1611__revisable_translation_reading, theater_ratio, 1881, 0.1).
narrative_ontology:measurement_basis(kjv__tr_t1881, observed).
narrative_ontology:measurement(kjv__tr_t1920, kjv_text_1611__revisable_translation_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement_basis(kjv__tr_t1920, observed).
narrative_ontology:measurement(kjv__tr_t1952, kjv_text_1611__revisable_translation_reading, theater_ratio, 1952, 0.2).
narrative_ontology:measurement_basis(kjv__tr_t1952, observed).
narrative_ontology:measurement(kjv__tr_t1978, kjv_text_1611__revisable_translation_reading, theater_ratio, 1978, 0.3).
narrative_ontology:measurement_basis(kjv__tr_t1978, observed).
narrative_ontology:measurement(kjv__tr_t2001, kjv_text_1611__revisable_translation_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement_basis(kjv__tr_t2001, observed).
narrative_ontology:measurement(kjv__tr_t2026, kjv_text_1611__revisable_translation_reading, theater_ratio, 2026, 0.45).
narrative_ontology:measurement_basis(kjv__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1881, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1881, 0.18).
narrative_ontology:measurement_basis(kjv__be_t1881, observed).
narrative_ontology:measurement(kjv__be_t1920, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1920, 0.22).
narrative_ontology:measurement_basis(kjv__be_t1920, observed).
narrative_ontology:measurement(kjv__be_t1952, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1952, 0.3).
narrative_ontology:measurement_basis(kjv__be_t1952, observed).
narrative_ontology:measurement(kjv__be_t1978, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement_basis(kjv__be_t1978, observed).
narrative_ontology:measurement(kjv__be_t2001, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement_basis(kjv__be_t2001, observed).
narrative_ontology:measurement(kjv__be_t2026, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(kjv__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t1881, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1881, 0.06).
narrative_ontology:measurement_basis(kjv__su_t1881, observed).
narrative_ontology:measurement(kjv__su_t1920, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1920, 0.09).
narrative_ontology:measurement_basis(kjv__su_t1920, observed).
narrative_ontology:measurement(kjv__su_t1952, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1952, 0.14).
narrative_ontology:measurement_basis(kjv__su_t1952, observed).
narrative_ontology:measurement(kjv__su_t1978, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1978, 0.26).
narrative_ontology:measurement_basis(kjv__su_t1978, observed).
narrative_ontology:measurement(kjv__su_t2001, kjv_text_1611__revisable_translation_reading, suppression_requirement, 2001, 0.35).
narrative_ontology:measurement_basis(kjv__su_t2001, observed).
narrative_ontology:measurement(kjv__su_t2026, kjv_text_1611__revisable_translation_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(kjv__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, resource_allocation).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the KJV's authority' decomposes into three structurally distinct claims per the epsilon-invariance principle. This story authors the revisable-translation reading (epsilon 0.62 over the licensed-revision arrangement); the exclusive-inspiration sibling authors a single-mandated-text arrangement with high suppression; the functional-equivalence sibling authors pluralist complementarity with minimal extraction. The upstream claim (the KJV's historical stature) is cited as evidence within all three readings; each sibling story links back here via its own network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
