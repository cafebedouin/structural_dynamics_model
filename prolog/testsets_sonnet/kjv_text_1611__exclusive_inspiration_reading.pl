% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: KJV-Only Exclusive Inspiration Doctrine
 *   domain: religious/theological/textual
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the KJV-1611 kernel: the
 *   exclusive-inspiration reading, which holds that the King James Version is
 *   the only inspired, inerrant English Bible, and that all other English
 *   translations are corrupted, compromised, or spiritually dangerous. This
 *   reading is distinct from — and not a synthesis with — the
 *   revisable-translation reading (KJV as historically important but
 *   improvable) and the functional-equivalence reading (multiple translations
 *   serving complementary roles). Those are separate constraints with their
 *   own ε values, stakeholders, and classifications, linked here only by
 *   network reference. This story's ε is anchored specifically to the
 *   exclusivity claim's gate-keeping function: it measures the cost imposed
 *   on those who read, prefer, or need translations other than the KJV, and
 *   the benefit captured by the institutional network that adjudicates
 *   textual legitimacy.
 *
 * KEY AGENTS:
 *   - kjv_only_pastors_and_publishers: agenda-setting beneficiaries who administer the doctrine and profit from its defense
 *   - dispensationalist_seminaries: institutional beneficiaries whose curricula and accreditation depend on the doctrine
 *   - congregants_under_kjv_only_discipline: powerless payers bearing social and psychological costs
 *   - modern_translation_readers: payers subjected to exclusion for using mainstream translations
 *   - biblical_scholars_of_textual_criticism: excluded experts whose evidence is disqualified a priori
 *   - non_english_speaking_converts: powerless global payers inheriting a linguistic dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.68).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.72).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV-Only Exclusive Inspiration Doctrine").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/theological/textual").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, 'a2973fab-1fa4-42c4-83cc-aae0b920d40f').
narrative_ontology:cs_kernel_codification('a2973fab-1fa4-42c4-83cc-aae0b920d40f', fixed_text).
narrative_ontology:cs_authority_grounding('a2973fab-1fa4-42c4-83cc-aae0b920d40f', extraction).
narrative_ontology:cs_interpretation_layer_present('a2973fab-1fa4-42c4-83cc-aae0b920d40f').
narrative_ontology:cs_reading_relation('a2973fab-1fa4-42c4-83cc-aae0b920d40f', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('a2973fab-1fa4-42c4-83cc-aae0b920d40f', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('a2973fab-1fa4-42c4-83cc-aae0b920d40f', foundational, kjv_uniquely_and_exclusively_inspired).
narrative_ontology:cs_axiom_status(kjv_uniquely_and_exclusively_inspired, holdable).
narrative_ontology:cs_axiom_grounding('a2973fab-1fa4-42c4-83cc-aae0b920d40f', kjv_uniquely_and_exclusively_inspired, theological).
narrative_ontology:cs_axiom('a2973fab-1fa4-42c4-83cc-aae0b920d40f', foundational, textus_receptus_providentially_preserved_superior_to_all_other_manuscript_traditions).
narrative_ontology:cs_axiom_status(textus_receptus_providentially_preserved_superior_to_all_other_manuscript_traditions, holdable).
narrative_ontology:cs_axiom_grounding('a2973fab-1fa4-42c4-83cc-aae0b920d40f', textus_receptus_providentially_preserved_superior_to_all_other_manuscript_traditions, theological).
narrative_ontology:cs_axiom('a2973fab-1fa4-42c4-83cc-aae0b920d40f', secondary, modern_critical_text_scholarship_constitutes_corruption_not_refinement).
narrative_ontology:cs_axiom_status(modern_critical_text_scholarship_constitutes_corruption_not_refinement, holdable).
narrative_ontology:cs_axiom_grounding('a2973fab-1fa4-42c4-83cc-aae0b920d40f', modern_critical_text_scholarship_constitutes_corruption_not_refinement, empirically_contingent).
narrative_ontology:cs_reference_frame('a2973fab-1fa4-42c4-83cc-aae0b920d40f', textus_receptus_providential_preservation).
narrative_ontology:cs_drift_state('a2973fab-1fa4-42c4-83cc-aae0b920d40f', post_critical_text_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a2973fab-1fa4-42c4-83cc-aae0b920d40f', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_pastors_and_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, dispensationalist_seminaries).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_denominational_leadership).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_readers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, congregants_under_kjv_only_discipline).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, biblical_scholars_of_textual_criticism).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, non_english_speaking_converts).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_priority_doctrine).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, providential_preservation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preach and publish the doctrine that the KJV alone is inspired and inerrant, controlling pulpit access, publishing houses, Bible colleges, and conference circuits built on the claim. They set the terms of orthodoxy for their congregations and collect tuition, book sales, and donor support tied to defending the position; they can exit into mainstream evangelicalism if the doctrine becomes a liability, an option unavailable to their followers.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_pastors_and_publishers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, kjv_only_pastors_and_publishers, beneficiary).

% Train pastors exclusively in KJV textual apologetics, building curricula, faculty positions, and accreditation around defending Textus Receptus priority. Institutional survival depends on the doctrine remaining unquestioned among their donor base and alumni network.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, dispensationalist_seminaries, beneficiary,
    institutional, generational, mobile, national).

% Denominational bodies that have made KJV-exclusivity a marker of doctrinal soundness, using it to discipline, defrock, or exclude ministers who use other translations. Their authority as arbiters of scriptural legitimacy is itself the asset the doctrine protects.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_denominational_leadership, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, kjv_only_denominational_leadership, agenda_setter).

% Taught that using an NIV or ESV imperils their souls or reflects spiritual compromise; many cannot parse Jacobean English fluently and privately struggle to understand scripture they are told is the only true Word. Leaving the church community means losing family, social ties, and sometimes employment in tightly bound congregations; leaving the doctrine while staying in the community is treated as rebellion.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, congregants_under_kjv_only_discipline, payer,
    powerless, biographical, constrained, local).

% Christians who read NIV, ESV, NASB, or other modern translations are declared to be reading a corrupted or Satanically-influenced text by KJV-Only advocates, subjected to social exclusion, public denunciation, or exclusion from KJV-Only fellowship and ministry networks despite using translations produced by mainstream textual scholarship.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_readers, payer,
    moderate, biographical, constrained, national).

% Textual critics who work with the full manuscript record (papyri, Alexandrian and Byzantine text families) are dismissed wholesale as tools of a corrupting conspiracy against scripture, their peer-reviewed findings excluded a priori from KJV-Only discourse regardless of evidence, because the doctrine forecloses the legitimacy of their entire discipline in advance.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, biblical_scholars_of_textual_criticism, excluded,
    moderate, generational, mobile, global).

% In mission contexts where KJV-Only doctrine has been exported, converts are sometimes taught that translations into their own languages made from modern critical texts are untrustworthy, or that English literacy in the KJV's archaic register is a precondition for authentic faith, creating a linguistic and doctrinal dependency on missionaries and English-language materials.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, non_english_speaking_converts, payer,
    powerless, biographical, trapped, global).

% The doctrinal proposition that the Greek text underlying the KJV New Testament is itself providentially preserved and superior to earlier manuscript witnesses; it is vindicated by the constraint's operation but collects no rents itself.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_priority_doctrine, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_priority_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, stable, memorized textual reference that many congregations have used for generations, enabling shared liturgy, memorization, cross-generational continuity, and a bulwark against rapid doctrinal drift from proliferating translations of uneven quality.
% TRANSFER_FUNCTION: Moves authority to declare 'true scripture' from the wider community of manuscript evidence and translation scholarship to a specific network of pastors, publishers, and seminaries; moves social and financial capital (tuition, book sales, donations, pulpit loyalty) to that network; moves psychological and social costs (guilt, exclusion, family rupture) onto those who read or prefer other translations.
% ABSENT_VOICES: Textual critics and mainstream biblical scholars are present in the wider discourse but structurally excluded from KJV-Only pulpits and seminaries — their manuscript evidence is disqualified in advance by the doctrine itself, not engaged and rebutted. Non-English-speaking converts in mission fields rarely have any voice in whether English-language exclusivity doctrine is imposed on their vernacular translation efforts.
% DISAPPEARANCE_RATIONALE: If the exclusive-inspiration doctrine vanished overnight, KJV-Only seminaries would lose their distinguishing curriculum and donor base, denominational discipline structures built on translation-purity would dissolve, congregants would freely read modern translations without social penalty, and the publishing/conference economy built around defending Textus Receptus priority would collapse into ordinary textual-criticism discourse.
% FOUNDING_PROBLEM: In the 19th and 20th centuries, new manuscript discoveries (Sinaiticus, Vaticanus) and new critical Greek texts (Westcott-Hort) produced translations that diverged from the KJV, and some conservative Protestants feared this represented liberal theological erosion of scriptural authority, textual corruption, or a slide toward modernist doubt about biblical inerrancy.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream evangelical and conservative textual scholars (including at conservative seminaries not committed to KJV-Only doctrine) broadly agree the manuscript evidence has only strengthened confidence in the transmitted New Testament text since the doctrine's founding controversies, and view the KJV-Only exclusivity claim as a distinct and unsupported theological add-on rather than a live defense against ongoing textual erosion. No corroboration for 'live' status comes from outside the KJV-Only leadership and seminary network itself.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects the doctrine's function as a gatekeeping mechanism: it does not merely commend the KJV, it delegitimizes all alternatives and channels social, financial, and institutional capital toward those who administer the exclusivity claim. Suppression (0.72) is high because the doctrine is maintained through active disciplinary mechanisms — pulpit exclusion, seminary orthodoxy tests, denominational discipline, social shunning — not mere persuasion. Theater ratio (0.40) reflects that a substantial share of KJV-Only apologetics (textual-critical argumentation, historical claims about manuscript transmission) is performative defense of a foregone conclusion rather than genuine engagement with the evidence, though some real coordination function (shared memorization, liturgical continuity) persists underneath. Accessibility collapse is moderate (0.5) — practically, congregants can and do leave KJV-Only churches, and the wider Christian and scholarly world offers ample alternatives, but exit from *that specific community* is costly. Resistance (0.6) reflects the substantial and organized pushback from textual scholars, mainstream evangelical leadership, and former adherents.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of KJV-Only leadership, this is a rope: a genuine, necessary defense of scriptural purity against manuscript corruption and modernist doubt, undertaken to protect the flock. From the seat of a congregant quietly reading an ESV in private and hiding it from their pastor, or a scholar whose peer-reviewed manuscript work is waved away as satanic tampering, the same structure operates as tangled rope shading toward snare: a coordination story (textual fidelity) providing cover for extraction (institutional capture of scriptural authority) enforced through social and psychological coercion. The engine computes both seats from the same structural data; the divergence between them is the measurement, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   kjv_only_pastors_and_publishers, dispensationalist_seminaries, and denominational leadership sit near the full-beneficiary end: they administer the doctrine, collect the institutional rents (tuition, book sales, donor loyalty, pulpit authority), and have exit options into the wider evangelical mainstream should the doctrine become untenable. Congregants, modern-translation readers, and non-English-speaking converts sit near the full-target end: they bear the social, psychological, and sometimes material costs of the exclusivity claim, and their exit options are constrained by family, community, and (for global converts) missionary dependency. Biblical scholars of textual criticism are excluded rather than coordinated — their entire discipline is disqualified by the doctrine's core premise, which is precisely why they appear as excluded rather than payer: they are not paying into the arrangement, they are locked out of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine anxiety in the 19th/20th century about new manuscript discoveries destabilizing confidence in scripture — has been resolved by the broader field of textual criticism, which has if anything strengthened confidence in the reliability of the transmitted New Testament text across multiple manuscript traditions. The exclusivity claim persists not because the founding problem is live, but because an institutional network's authority, revenue, and identity are now built on the claim itself. This is the classic mandatrophy signature: founding_problem_status is dead, but disappearance_verdict is world_rearranges, because a live institutional apparatus (seminaries, publishers, denominational discipline structures) still depends on the claim continuing to be believed. Classifying this as tangled_rope rather than pure snare preserves the genuine (if now largely obsolete) coordination function the KJV originally served — shared memorization, liturgical stability — while still naming the asymmetric extraction and required enforcement that keep the exclusivity claim, specifically, alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    providential_preservation_vs_institutional_capture,
    'Is the doctrine of the KJV''s exclusive inspiration a genuine (if contested) theological claim about providential textual preservation, or is it better modeled as an institutional capture mechanism that uses theological language as cover for consolidating denominational and financial authority?',
    'Compare doctrinal outcomes across KJV-Only institutions with different governance and revenue structures: if the exclusivity claim persists identically regardless of institutional stake (no correlation between financial/authority dependence and doctrinal intensity), the theological-claim reading gains support; if doctrinal intensity tracks institutional revenue and authority dependence, the capture reading gains support.',
    'If genuinely theological and held independent of institutional stake, the constraint sits closer to rope with an unusually high enforcement cost; if institutional capture explains the doctrine''s persistence, tangled_rope or snare classification is strongly supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(providential_preservation_vs_institutional_capture, conceptual, 'Whether the exclusivity claim is theology or institutional capture wearing theology as cover.').

omega_variable(
    sibling_reading_resource_competition,
    'Does the exclusive_inspiration_reading actively suppress resources (seminary funding, publishing contracts, denominational credentialing) that would otherwise flow to institutions holding the revisable_translation_reading or functional_equivalence_reading, or do the readings simply coexist in separate, non-competing institutional ecosystems?',
    'Track denominational splits, seminary faculty departures, and publishing contract shifts triggered specifically by translation-doctrine disputes; measure whether adopting a non-exclusive reading correlates with loss of denominational standing or funding.',
    'If resource competition is substantial, this reading''s influences relation toward the sibling readings should be read as actively constraining their institutional viability, not merely coexisting alongside them; if ecosystems are genuinely separate, coexists_with is the more accurate structural relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_resource_competition, empirical, 'Whether this reading competes for institutional resources with its sibling readings or merely coexists alongside them.').

omega_variable(
    founding_problem_genuine_residue,
    'Even granting that the founding problem (manuscript-driven doubt about scriptural reliability) is largely resolved at the level of mainstream scholarship, does a genuine residual concern persist for lay believers who lack the expertise to evaluate manuscript evidence themselves and rely on trusted authorities to adjudicate translation reliability?',
    'Survey lay congregants across KJV-Only and non-KJV-Only traditions on comprehension of, and trust in, translation methodology; assess whether confusion or anxiety about translation reliability is meaningfully higher among those without exclusivity doctrine, controlling for catechesis quality.',
    'If lay anxiety about translation reliability is a genuine and widespread need unmet by non-exclusivity traditions, part of this reading''s coordination function may be real rather than purely captured, softening (without eliminating) the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_genuine_residue, empirical, 'Whether a genuine unmet lay need for translation-trust authority survives even after the scholarly founding problem is resolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(kjv__tr_t50, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(kjv__tr_t60, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(kjv__be_t50, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(kjv__be_t60, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(kjv__su_t30, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(kjv__su_t50, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(kjv__su_t60, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__exclusive_inspiration_reading, 0.08).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the KJV-Only controversy' into structurally distinct kernel readings, per the ε-invariance principle. exclusive_inspiration_reading (this story) carries the highest ε because it alone requires active suppression of alternative translations and names identifiable victims (readers, scholars, converts excluded or disciplined for non-KJV use). revisable_translation_reading treats the KJV as improvable given better manuscripts, generating a much lower-ε, largely rope-shaped constraint with no comparable victim class. functional_equivalence_reading treats multiple translations as serving complementary purposes and is closer to a genuine rope or even near-mountain (linguistic diversity of translation serving different reading levels and purposes) with negligible extraction. All three share the same underlying kernel — the 1611 KJV text and its historical-textual status — but instantiate different authority structures, different beneficiary/victim sets, and different ε values; they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
