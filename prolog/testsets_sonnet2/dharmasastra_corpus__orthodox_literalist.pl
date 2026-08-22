% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra Varna/Jati Hierarchy — Orthodox Literalist Reading (Eternal Revealed Law)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This story authors the orthodox literalist reading of the Dharmasastra
 *   kernel: the claim that varna/jati hierarchy prescriptions are eternal
 *   (sanatana), divinely revealed (shruti/smriti authority), and require
 *   literal, unmodified observance rather than historical contextualization.
 *   This reading is one of three structurally distinct constraints sharing
 *   the Dharmasastra kernel — the reformist_contextual reading (dharma's
 *   ethical core separable from time-bound caste prescription) and the
 *   abolitionist_rejection reading (no legitimate authority survives; the
 *   framework must be abandoned) are separate stories with their own epsilon
 *   values, beneficiary/victim sets, and classifications. Under this
 *   literalist reading specifically, extraction is high and concentrated:
 *   ritual, educational, and occupational access is withheld from Shudras,
 *   Dalits (and those outside the varna scheme), and women on textual grounds
 *   treated as beyond human negotiation, while ritual and interpretive
 *   authority concentrates in Brahmin and allied upper-caste institutions.
 *   The theater_ratio and suppression figures reflect a constraint that, over
 *   the interval modeled (roughly the codification-through-colonial-era
 *   span), required progressively more active enforcement (legal codification
 *   under colonial administration, social sanction, and violent reprisal for
 *   boundary transgression) as its coordination rationale weakened relative
 *   to its extractive function.
 *
 * KEY AGENTS:
 *   - brahmin_priesthood: primary beneficiary and agenda-setter — controls textual transmission and ritual monopoly
 *   - kshatriya_landholders: secondary beneficiary — political/martial authority validated by the same hierarchy
 *   - orthodox_commentarial_lineages: agenda-setter whose institutional standing depends on the literalist reading's authority
 *   - dalits_excluded_from_ritual, shudras_barred_from_education, women_excluded_from_scriptural_study: primary victims — bear the hierarchy's exclusions on grounds of birth
 *   - reformist_and_abolitionist_readers: excluded voices whose competing readings are treated as heresy or external critique rather than legitimate interpretation
 *   - colonial_and_postcolonial_courts: analytical observers who codified and later contested the arrangement from outside its own interpretive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.81).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.86).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.81).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra Varna/Jati Hierarchy — Orthodox Literalist Reading (Eternal Revealed Law)").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '3151b895-dc89-4c1d-ad53-6e7b93c010b6').
narrative_ontology:cs_kernel_codification('3151b895-dc89-4c1d-ad53-6e7b93c010b6', fixed_text).
narrative_ontology:cs_authority_grounding('3151b895-dc89-4c1d-ad53-6e7b93c010b6', lineage).
narrative_ontology:cs_interpretation_layer_present('3151b895-dc89-4c1d-ad53-6e7b93c010b6').
narrative_ontology:cs_reading_relation('3151b895-dc89-4c1d-ad53-6e7b93c010b6', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_reading_relation('3151b895-dc89-4c1d-ad53-6e7b93c010b6', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('3151b895-dc89-4c1d-ad53-6e7b93c010b6', foundational, varna_prescriptions_are_eternally_binding_revelation).
narrative_ontology:cs_axiom_status(varna_prescriptions_are_eternally_binding_revelation, holdable).
narrative_ontology:cs_axiom_grounding('3151b895-dc89-4c1d-ad53-6e7b93c010b6', varna_prescriptions_are_eternally_binding_revelation, theological).
narrative_ontology:cs_axiom('3151b895-dc89-4c1d-ad53-6e7b93c010b6', foundational, literal_observance_required_irrespective_of_historical_context).
narrative_ontology:cs_axiom_status(literal_observance_required_irrespective_of_historical_context, holdable).
narrative_ontology:cs_axiom_grounding('3151b895-dc89-4c1d-ad53-6e7b93c010b6', literal_observance_required_irrespective_of_historical_context, theological).
narrative_ontology:cs_reference_frame('3151b895-dc89-4c1d-ad53-6e7b93c010b6', classical_smriti_authority).
narrative_ontology:cs_drift_state('3151b895-dc89-4c1d-ad53-6e7b93c010b6', postcolonial_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3151b895-dc89-4c1d-ad53-6e7b93c010b6', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_landholders).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, orthodox_commentarial_lineages).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits_excluded_from_ritual).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras_barred_from_education).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_excluded_from_scriptural_study).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_ashrama_dharma_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, revealed_scriptural_infallibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Sanskrit textual transmission, ritual monopoly, and commentarial authority (the six schools of Mimamsa and Dharmasastra digests). Determines what counts as correctly transmitted text and correct observance; controls temple entry, ritual officiancy fees, and access to scriptural literacy. Faces no exit cost from the arrangement — the priesthood's structural position IS the top of the hierarchy it interprets.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, beneficiary).

% Warrior/ruling-caste landholders whose claim to political and martial authority is validated by the same textual hierarchy that seats Brahmins above them ritually and seats Shudras and Dalits below them in labor and land relations. Benefit from the enforced order without administering its textual transmission themselves.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_landholders, beneficiary,
    powerful, generational, mobile, regional).

% Guru-disciple transmission chains (parampara) and dharmasastra commentators (Manu, Yajnavalkya digests, later nibandhas) who certify which reading of the corpus is authoritative. Their institutional standing depends on the text being read as eternal and literal rather than as historically contingent — a reformist or abolitionist reading would dissolve their interpretive monopoly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, orthodox_commentarial_lineages, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, orthodox_commentarial_lineages, beneficiary).

% Classed outside or beneath the four-varna scheme (avarna/'untouchable' in colonial-era taxonomy), barred from temple entry, well access, Vedic study, and many occupations by the literal reading of pollution and purity rules. Exit requires either conversion out of the tradition entirely or migration to urban/legal contexts where the constraint's enforcement is weaker — both costly, incomplete, and historically met with violent reprisal for perceived transgression.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits_excluded_from_ritual, payer,
    powerless, generational, trapped, national).

% Placed in the fourth varna, assigned to service occupations, and under the literalist reading barred from Vedic study and many ritual roles (the Shambuka narrative in the Ramayana is cited by this reading as precedent for punishing transgression). Bound to the hierarchy by birth; occupational and educational mobility is structurally foreclosed within orthodox observance.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras_barred_from_education, payer,
    powerless, generational, trapped, national).

% Barred from independent Vedic study and upanayana (sacred thread initiation) under the literalist reading of Manusmriti and related texts, defined as perpetually dependent (on father, husband, son). Exit options vary by class position within the hierarchy — an upper-caste woman has more mobility than a Dalit woman, but the textual exclusion from scriptural authority applies across caste lines.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_excluded_from_scriptural_study, payer,
    powerless, generational, constrained, national).

% Reformist commentators (arguing dharma's ethical core is separable from time-bound caste prescription) and abolitionist critics (arguing the framework is irredeemable) are excluded from orthodox commentarial authority — their readings are not treated as legitimate interpretations of the kernel by the institutions this constraint describes, only as external critique or heresy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_and_abolitionist_readers, excluded,
    organized, generational, constrained, national).

% British colonial courts codified selected Dharmasastra readings into 'Hindu law' for administrative purposes, and postcolonial Indian constitutional and legal institutions (Article 17 abolition of untouchability, reservation policy) now adjudicate the same textual claims from outside the tradition's own interpretive authority, producing an external record of the arrangement's operation and contest.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, colonial_and_postcolonial_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transmitted account of social role, ritual purity, and occupational duty (svadharma) that coordinates a large, diverse population's expectations about status, marriage, occupation, and ritual eligibility without case-by-case negotiation — a genuine information/identity coordination function for those inside the hierarchy's upper tiers.
% TRANSFER_FUNCTION: Moves ritual authority, land and occupational access, educational literacy, and social status upward and inward (toward Brahmin and Kshatriya varnas) while assigning labor obligation, ritual exclusion, and social stigma downward and outward (toward Shudras and those outside the varna scheme entirely), on the basis of birth rather than contribution or consent.
% ABSENT_VOICES: The corpus's own textual layers show internal contestation (Buddhist and Jain rejections of varna, bhakti movements' devotional egalitarianism, the historical Shudra and Dalit voices largely absent from Sanskrit textual production itself) — but under the orthodox literalist reading these are treated as external to the tradition's authoritative transmission, not as competing interpretations within it.
% DISAPPEARANCE_RATIONALE: If the orthodox literalist reading's enforcement authority disappeared overnight, ritual officiancy monopolies would lose their scriptural warrant, occupational and educational exclusions tied to birth would lose their textual justification (though social practice and habit would persist independently for a time), and the commentarial lineages whose institutional standing depends on literalist certification would lose their interpretive monopoly to reformist and secular legal authorities.
% FOUNDING_PROBLEM: Ancient and classical-era Brahminical society sought a stable account of social order, ritual purity, and cosmological duty that would organize an agrarian, multi-community society without continuous renegotiation of status and obligation — dharma as cosmic and social order requiring textual codification.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox commentarial lineages and Brahmin priesthood attest the founding problem (social/cosmic order requiring eternal textual warrant) remains live. Outside the beneficiary set: the Indian Constitution's framers (Article 17, Article 15 anti-discrimination provisions), the Dalit-led Ambedkarite movement (Ambedkar himself, a jurist and outside authority, argued the textual order was never a solution to a live problem but the codification of an extraction), and comparative historians of South Asian religion documenting bhakti and Buddhist counter-traditions from within the same historical period attest that the founding problem, if genuine, was answered by an arrangement whose function has long since shifted to rent-preservation for the interpreting castes.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.81) is authored high because the literalist reading assigns ritual, educational, and occupational access on birth rather than any coordination-relevant criterion, and the asymmetry is stark: near-total concentration of interpretive and ritual authority in one varna, near-total exclusion of others from the same goods. Suppression (0.86) exceeds extractiveness because the reading's persistence has historically depended on active enforcement — social sanction, legal codification (including some colonial administrative codification), and documented violent reprisal for transgression (e.g., the Shambuka narrative cited approvingly within the tradition as precedent) — not on participant preference. Accessibility_collapse (0.62) is moderate-high but not mountain-grade: unlike a genuine natural law, alternative readings of the same textual corpus visibly exist and are actively practiced by reform and devotional movements, so alternatives have not fully collapsed even though the literalist reading treats them as illegitimate. Resistance (0.70) is high, reflecting centuries of internal contestation (bhakti movements, Buddhist/Jain rejection, Ambedkarite abolition) that a genuine mountain would not generate. Theater_ratio starts low (0.12) and rises across the interval (0.28) as the coordination rationale (stable social/cosmic order) increasingly gives way to enforcement whose primary observable function is boundary-maintenance for the benefiting castes rather than the order it claims to secure.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priesthood and orthodox commentarial lineages sit at the full-beneficiary end of directionality: they administer the interpretive apparatus and collect status, ritual fee income, and institutional deference from its operation, with essentially arbitrage-grade exit (they can always retreat to a 'more authentic' or 'more literal' reading if challenged). Kshatriya landholders are beneficiaries at one remove — validated by, but not administering, the hierarchy. Dalits, Shudras, and women are structurally trapped or constrained targets: birth-assigned position, generational time horizon, and (for Dalits and Shudras especially) historically near-zero exit without full exit from the social order itself. Women's exit options are marked constrained rather than trapped because mobility varies with the woman's caste position, but the scriptural exclusion from independent study applies regardless of caste.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem answer states the reading's own genealogy honestly (a claimed cosmic/social ordering function) while founding_problem_status is marked contested and founding_problem_corroboration explicitly routes outside the beneficiary set (constitutional framers, Ambedkarite movement, comparative historians) to avoid the arrangement's own account certifying itself. This is the R5 discipline the schema requires: a genealogy corroborated only by Brahmin priesthood and commentarial lineages would be a textbook cover-story generator. The tangled_rope classification (rather than pure snare) is authored because a genuine coordination function is not fabricated here — pre-modern agrarian societies plausibly needed SOME account of role and obligation — but the same structure that would solve that problem is also the mechanism through which asymmetric extraction is enforced on Shudras, Dalits, and women, and active enforcement (requires_active_enforcement: true) is required to sustain it against internal contestation. This prevents both over-simplification (calling it pure snare erases the genuine, if contested, coordination claim the reading itself makes) and under-simplification (calling it rope or mountain would erase the well-documented victim set and enforcement history).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eternal_vs_constructed_kernel,
    'Is the varna/jati hierarchy genuinely eternal/revealed as this reading claims, or is it a time-bound social construction later retroactively textualized as eternal (as the reformist and abolitionist readings both hold, for different reasons)?',
    'Textual-historical scholarship on the dating and layering of Dharmasastra compositions (Manusmriti''s composite, multi-century authorship; comparison with earlier Vedic-era social organization, which was less rigid) versus the tradition''s own claim of unauthored, eternally revealed status for smriti texts.',
    'If constructed and historically layered, the eternal/revealed claim is itself an extraction-supporting device rather than a description of the text''s nature, strengthening the case that this reading''s coordination story is cover for the concentrated beneficiary structure. If the tradition''s revealed-status claim is granted on its own terms, the coordination framing gains more independent weight, though the victim/beneficiary asymmetry remains regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eternal_vs_constructed_kernel, conceptual, 'Whether the eternal/revealed status is a genuine feature of the text or a legitimating construction.').

omega_variable(
    committer_structure_dharmasastra_kernel,
    'This constraint instantiates the orthodox_literalist reading of the shared dharmasastra_corpus kernel. Two sibling readings exist: reformist_contextual (dharma''s ethical core separable from time-bound caste prescription, yielding a much lower extraction profile and a narrower victim set) and abolitionist_rejection (no legitimate authority survives the textual framework at all, yielding zero coordination credit and treating the entire arrangement as pure extraction). Where is the disagreement located structurally?',
    'The disagreement is located at the interpretive-authority layer, not at the descriptive layer: all three readings can agree on what the text historically says about varna/jati; they diverge on whether literal observance is currently obligatory (this reading: yes, eternally), whether the ethical core is separable from the caste prescriptions (reformist: yes), or whether the framework retains any legitimate authority at all (abolitionist: no). Resolution would require either textual-authority adjudication within the tradition (unlikely to be decisive, since the traditions themselves are split) or external constitutional/legal displacement of the tradition''s authority claim (which has already partially occurred via Article 17 and anti-discrimination law).',
    'Adopting the reformist reading instead would sharply reduce authored epsilon (the caste-prescription victim set would be read as historically contingent rather than eternally binding, and the beneficiary concentration would be attributed to historical accident rather than revealed necessity) and would likely reclassify the constraint toward scaffold or rope (a historically bounded coordination device whose caste-specific prescriptions are not meant to be permanent). Adopting the abolitionist reading would eliminate the coordination-function claim entirely and reclassify toward snare (no genuine coordination credit, pure extraction under religious cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_dharmasastra_kernel, conceptual, 'Where the three sibling readings of the dharmasastra_corpus kernel structurally diverge and what adopting each would change.').

omega_variable(
    internalized_vs_structural_suppression,
    'Among Shudra, Dalit, and women''s populations historically subject to this reading''s exclusions, how much of the measured suppression is structural (legal/social barriers, violence, exclusion from institutions) versus internalized (accepted cosmological legitimacy of one''s own subordinate position, taught and reinforced across generations)?',
    'Comparative study of populations that exited the tradition''s authority (conversion to Buddhism, Christianity, or Islam historically; post-independence legal emancipation via reservation policy) and whether internalized status-acceptance persisted after structural barriers were formally removed — the Ambedkarite Buddhist conversion movement is a directly relevant natural experiment.',
    'If suppression is substantially internalized, the effective suppression borne by victim groups is higher than the structural measure alone suggests, since formal legal abolition (Article 17) does not fully resolve lived exclusion — consistent with continued caste-based social exclusion in contemporary India despite constitutional abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized suppression mechanism among populations excluded under the literalist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__orthodox_literalist, theater_ratio, 40, 0.15).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__orthodox_literalist, theater_ratio, 80, 0.19).
narrative_ontology:measurement(dhar_tr_t120, dharmasastra_corpus__orthodox_literalist, theater_ratio, 120, 0.22).
narrative_ontology:measurement(dhar_tr_t160, dharmasastra_corpus__orthodox_literalist, theater_ratio, 160, 0.25).
narrative_ontology:measurement(dhar_tr_t200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 200, 0.28).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 80, 0.79).
narrative_ontology:measurement(dhar_be_t120, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 120, 0.8).
narrative_ontology:measurement(dhar_be_t160, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 160, 0.81).
narrative_ontology:measurement(dhar_be_t200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 200, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 80, 0.79).
narrative_ontology:measurement(dhar_su_t120, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 120, 0.82).
narrative_ontology:measurement(dhar_su_t160, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 160, 0.84).
narrative_ontology:measurement(dhar_su_t200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 200, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__orthodox_literalist, 0.08).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the shared dharmasastra_corpus kernel. orthodox_literalist (this story) authors high extraction (0.81) and an expansive victim set on the claim that varna/jati prescriptions are eternal and require literal observance. reformist_contextual authors substantially lower extraction on the claim that dharma's ethical core is separable from time-bound caste prescriptions. abolitionist_rejection authors extraction near-maximal with zero coordination credit, holding that no legitimate authority survives the framework. Per the ε-invariance principle, these are three separate constraints, not one constraint measured three ways — each carries its own ε, beneficiary/victim structure, and classification, linked here via network edges rather than folded into a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
