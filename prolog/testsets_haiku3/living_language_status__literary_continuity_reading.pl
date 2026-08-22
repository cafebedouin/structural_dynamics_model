% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Living Language Status via Literary Continuity
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   The literary-continuity reading of living-language status defines a
 *   language as alive if it remains a productive medium for new literary and
 *   intellectual work, decoupled from native-speaker demographics. This
 *   reading anchors the vitality of Hebrew in the Haskalah's periodicals,
 *   essays, and new literary works produced by maskilim and secular
 *   intellectuals, regardless of whether those writers were native speakers.
 *   It displaces the liturgical-preservation reading (which grounds vitality
 *   in ritual transmission) and coexists with — but structurally
 *   disadvantages — the native-generation reading (which requires
 *   intergenerational transmission as a mother tongue). The
 *   literary-continuity reading benefits maskilim and diaspora intellectuals
 *   by positioning them as arbiters of linguistic life, while rendering
 *   non-literary native speakers and liturgical authorities invisible or
 *   subordinate. Extraction is moderate (0.38) because the constraint
 *   operates through definitional authority rather than coercive enforcement:
 *   the primary extraction is epistemic — redefinition of what counts as
 *   evidence. Suppression is moderate (0.42) because non-literary speakers
 *   and liturgical authorities are excluded from the frame rather than
 *   actively punished; the constraint's persistence depends on intellectuals'
 *   continued literary production and on diaspora communities' investment in
 *   the intellectual reading. Theater rises modestly over the interval (0.08
 *   to 0.28) as the literary-production claim is increasingly invoked to
 *   defend Hebrew status in contexts where demographic vitality is questioned
 *   — the constraint becomes more performative as it is deployed defensively.
 *
 * KEY AGENTS:
 *   - Maskilim and secular intellectuals: producers of the literary work that vindicates the constraint; beneficiaries of the frame that positions them as authority on linguistic vitality
 *   - Traditional religious authorities: displaced from epistemic authority; identity-locked to the older liturgical-transmission frame
 *   - Non-literary native speakers: excluded from the vitality definition; their daily speech and child-rearing are rendered invisible
 *   - Diaspora communities: beneficiaries through the decoupling of vitality from mass adoption; gain linguistic belonging without demographic return
 *   - Liturgical guardians: excluded from the conversation; their evidence (ritual transmission) is redefined as insufficient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.38).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.42).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Living Language Status via Literary Continuity").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, 'a7406802-5d45-425f-8346-9952e2f25a77').
narrative_ontology:cs_kernel_codification('a7406802-5d45-425f-8346-9952e2f25a77', distributed).
narrative_ontology:cs_authority_grounding('a7406802-5d45-425f-8346-9952e2f25a77', distributed).
narrative_ontology:cs_reading_relation('a7406802-5d45-425f-8346-9952e2f25a77', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7406802-5d45-425f-8346-9952e2f25a77', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('a7406802-5d45-425f-8346-9952e2f25a77', foundational, literary_production_suffices_for_vitality).
narrative_ontology:cs_axiom_status(literary_production_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a7406802-5d45-425f-8346-9952e2f25a77', literary_production_suffices_for_vitality, instrumental).
narrative_ontology:cs_axiom('a7406802-5d45-425f-8346-9952e2f25a77', foundational, vitality_decoupled_from_native_speaker_demographics).
narrative_ontology:cs_axiom_status(vitality_decoupled_from_native_speaker_demographics, holdable).
narrative_ontology:cs_axiom_grounding('a7406802-5d45-425f-8346-9952e2f25a77', vitality_decoupled_from_native_speaker_demographics, instrumental).
narrative_ontology:cs_reference_frame('a7406802-5d45-425f-8346-9952e2f25a77', secular_intellectual_authority_framework).
narrative_ontology:cs_drift_state('a7406802-5d45-425f-8346-9952e2f25a77', contemporary_hebrew_native_revival, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7406802-5d45-425f-8346-9952e2f25a77', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_and_non_literary_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, jewish_diaspora_communities).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, traditional_religious_authorities).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, native_hebrew_speakers_outside_literary_culture).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, cultural_vitality_decoupled_from_mass_adoption).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, literary_production_as_linguistic_continuity_marker).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hebrew maskilim, writers, and Haskalah periodical editors set the frame that linguistic vitality flows from literary production and intellectual work. They benefit from this definition because it positions them as the arbiters of what counts as 'living' — they produce the literary work that vindicates the language's status. They have exit options (can write in other languages, can adopt other frames) but choose to invest in Hebrew literary continuity as a marker of cultural authority and national possibility.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals, beneficiary).

% Rabbinical and liturgical authorities previously held the sole authority over Hebrew vitality claims — the language was alive because it was the vehicle of sacred law and prayer. The literary-continuity reading displaces this authority by redefining vitality in secular, literary terms. Their losses are epistemic: the frame shifts away from liturgical preservation as the primary proof of life. They are identity-locked to the traditional authority structure and cannot simply adopt an alternative frame.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, traditional_religious_authorities, payer,
    powerful, civilizational, identity_locked, regional).

% Monolingual Hebrew speakers without access to literacy or literary culture are excluded from the vitality definition under this reading. Their native speech is rendered invisible or subordinate to literary production as the marker of linguistic life. They bear the cost of a framework that treats their existence as irrelevant to the language's status — their communication, even if rich and productive in daily life, does not count as evidence of vitality. They are trapped in an asymmetric frame: they speak the language natively but are told they do not validate its existence.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_and_non_literary_speakers, payer,
    powerless, biographical, trapped, regional).

% Eastern European and diaspora Jewish communities gain from the literary-continuity frame because it allows Hebrew to be 'alive' even where native speakers are few or non-existent. Diaspora intellectuals and educated elites can participate in Hebrew literary production and intellectual work without requiring their entire community to be native speakers. The constraint opens a pathway to linguistic participation and national belonging that does not require mass linguistic shift.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, jewish_diaspora_communities, beneficiary,
    moderate, generational, constrained, global).

% Native speakers of Hebrew in daily commerce, craft, and domestic life whose speech does not reach the literary periodicals or formal intellectual venues are excluded from the vitality calculus. Their linguistic work — creating new vocabulary through use, maintaining the language through daily transmission to children, adapting it to new circumstances — is rendered invisible or subordinate. They are trapped in a definitional frame that privileges a narrow class of activity (literary production) over the wider reproductive work they perform.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, native_hebrew_speakers_outside_literary_culture, payer,
    powerless, biographical, trapped, local).

% Non-Jewish European scholars and philosophers observe the Haskalah's use of Hebrew as a literary vehicle and provide external corroboration of its vitality claims. They occupy an analytical seat: they can testify to the empirical fact of Hebrew literary production without stakes in the religious or national outcome. Their testimony lends epistemic weight to the literary-continuity reading.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, european_enlightenment_intellectuals, observer,
    institutional, generational, analytical, global).

% Conservative and Orthodox authorities who ground Hebrew vitality in liturgical continuity are structurally excluded from the conversation about literary production as the primary marker. They would argue that ritual recitation, prayer-book usage, and Torah study constitute vitality sufficient on their own. Their exclusion is enforced by the frame itself: their evidence (liturgical transmission) is redefined as 'preservation of a corpse' rather than vitality, rendering their epistemic contribution null.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, hebrew_liturgical_guardians, excluded,
    powerful, civilizational, identity_locked, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals).
narrative_ontology:fixing_cost_class(living_language_status__literary_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for what counts as linguistic vitality in the modern context, enabling intellectuals and displaced diaspora communities to make coherent claims about cultural identity and textual tradition without requiring demographic return to Judea or mass native-speaker transmission. Solves the problem of how to maintain Hebrew as a meaningful cultural and intellectual medium when native speakers are sparse or absent.
% TRANSFER_FUNCTION: Transfers epistemic authority from religious authorities (who ground vitality in liturgical transmission) to secular intellectuals (who ground it in literary production). The transfer moves the power to define what keeps a language alive from clerical gatekeepers to maskilim, writers, and periodical editors. Simultaneously, it transfers the authority to validate speakers: native speakers outside literary culture lose validation; literary producers without native status gain it.
% ABSENT_VOICES: Monolingual and non-literate Hebrew speakers who transmit the language daily through speech and child-rearing, but produce no literary work, are systematically excluded from the conversation. Traditional liturgical authorities who argue for the sufficiency of ritual transmission are also effectively excluded — their frame is treated as obsolete rather than as a live alternative. Speakers of living Hebrew outside the literary-intellectual circuit have no standing in the definition of their own language's vitality.
% DISAPPEARANCE_RATIONALE: If this literary-continuity reading were abandoned, the authority to define Hebrew vitality would revert to liturgical gatekeepers (or shift to demographic criteria like native transmission). The Haskalah's claim to cultural authority and the diaspora's claim to linguistic belonging-without-return would collapse. Hebrew would be re-evaluated as 'dead' or as 'alive only in prayer' — the intellectual class's position in the linguistic economy would downshift dramatically.
% FOUNDING_PROBLEM: In the late 18th and 19th centuries, Hebrew faced a legitimacy crisis: it had few native speakers, yet it carried immense cultural and religious weight. How could Hebrew be claimed as 'living' when most Jews spoke Yiddish, Ladino, or local languages? The literary-continuity reading solved this by decoupling vitality from native-speaker demographics and anchoring it in intellectual productivity — the Haskalah's periodicals and new literary works became the evidence of life.
% FOUNDING_PROBLEM_CORROBORATION: The maskilim themselves and modern Hebrew literary scholars attest that the founding problem was urgent and that the literary-production solution was generative (Haskalah periodicals, Hebrew fiction, poetry, and essays did flourish). Competing readings attest the problem was differently framed: liturgical authorities claim the problem was already solved (Hebrew lived in prayer and study); native-speaker advocates claim the 'solution' was a false reading that postponed a real demographic reckoning. No external, non-invested authority offers neutral testimony — all observers have stakes in which definition prevails.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate because the constraint operates primarily through definitional capture rather than resource extraction or coercion. The maskilim establish that literary production is THE marker of linguistic vitality, but they do not directly take anything from other speakers — the extraction is epistemic. Suppression (0.42) is moderate because non-literary speakers are excluded rather than actively suppressed: the constraint defines them as irrelevant, which is softer than preventing them from speaking. Accessibility collapse (0.45) is low-to-moderate because alternatives persist: liturgical authorities maintain the ritual-transmission reading, and demographic vitality could be asserted by native speakers if they organized. Resistance (0.58) is moderate-to-high because multiple parties (religious authorities, native-speaker advocates, Yiddish speakers) contest this frame — the constraint does not face zero opposition. Theater rises over the interval from 0.08 to 0.28 because the literary claim is increasingly invoked defensively in contexts where Hebrew faces extinction pressures: more effort goes into demonstrating new literary production as the language faces demographic challenges, increasing the performative component.
 *
 * PERSPECTIVAL GAP:
 *   From the maskilim's perspective, the literary-continuity reading is a genuine solution to a real problem: How can Hebrew remain alive and culturally meaningful when native speakers are sparse? From the perspective of non-literary native speakers, the same frame is a delegitimization: their own existence and language use are rendered invisible. From the liturgical authorities' perspective, the reading is a displacement and a loss of epistemic authority. From the diaspora's perspective, it is a pathway to belonging. The engine should compute dramatically different types across these seats: the maskilim and diaspora intellectuals should perceive coordination and benefit; the religious authorities should perceive authority loss; the non-literary speakers should perceive exclusion and invisibility. The constraint is 'alive' only from seats that produce or benefit from literary work.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim and secular intellectuals sit at d ≈ 0.1–0.2 (low directionality, strong beneficiaries): they set the frame, collect the epistemic authority, and have mobile exit (can write in other languages but choose Hebrew). Traditional religious authorities sit at d ≈ 0.7–0.8 (high directionality, targets): they lose authority, their evidence is redefined as insufficient, and they are identity-locked to their role. Non-literary native speakers sit at d ≈ 0.9 (extreme target): they speak the language natively but are told they do not validate its existence; they are trapped and invisible. Diaspora intellectuals sit at d ≈ 0.3–0.4 (moderate beneficiary): they gain linguistic belonging without demographic return, but they depend on the maskilim's continued literary production — they have constrained exit (cannot simply abandon the frame without losing their identity claim). The spread is wide because the constraint asymmetrically benefits one class (literary producers) while rendering others invisible or dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (How can Hebrew be claimed as living when few are native speakers?) may or may not be live. If Hebrew has organically developed a substantial native-speaker base (modern Israel), then the founding problem is dead and the constraint persists as rent-collection by the intellectual class. If Hebrew remains sparse as a native tongue, the founding problem is still live and the constraint remains adaptive. The measured theater ratio rising from 0.08 to 0.28 suggests the constraint is increasingly performing (invoking literary production as evidence of life in contexts where survival is questioned) — a sign of mounting defensive investment. The mandatrophy question: Does the literary-continuity frame still solve the problem it was built for, or has it become a luxury claim maintained by intellectuals even as the underlying demographic situation has changed? Modern Hebrew's status as an official language and near-universal second language in Israel may have shifted the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_production_as_proxy_for_vitality,
    'Does literary production actually measure linguistic vitality, or does it measure intellectual productivity and cultural prestige in a narrow class?',
    'Linguistic analysis of whether Hebrew literary texts from the Haskalah period demonstrate genuine innovation in the language''s structure, vocabulary, and expressive capacity, versus whether they are elite artifacts that track intellectual fashion more than the language''s reproductive health.',
    'If literary production is a genuine proxy for vitality, the constraint measures something real about the language''s adaptability. If it is primarily a class marker, the constraint is extractive definitional capture by intellectuals: it redescribes the language''s life as dependent on activities only a narrow elite can perform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_production_as_proxy_for_vitality, conceptual, 'Whether literary production measures linguistic vitality or intellectual class status.').

omega_variable(
    invisibility_of_vernacular_innovation,
    'What linguistic innovation and creative work are native speakers conducting in daily speech, and does the literary-continuity frame systematically exclude this evidence from the vitality calculation?',
    'Documentation of non-literary Hebrew speech communities, creoles, and vernacular adaptations from the same period; analysis of whether native speakers were innovating grammar, vocabulary, and communicative practices that the literary frame renders invisible.',
    'If native speakers were conducting substantial linguistic innovation, the constraint is extractive: it redefines the language''s life to exclude the work native speakers actually performed. If native speakers were genuinely stagnant and non-innovative, the constraint captures a real difference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(invisibility_of_vernacular_innovation, empirical, 'Whether the literary frame captures all linguistic vitality or systematically hides vernacular innovation.').

omega_variable(
    kernel_reading_contest_structure,
    'Are the three readings (literary-continuity, liturgical-preservation, native-generation) genuinely alternative framings of a single kernel, or do they instantiate three different commitments that cannot coexist in a single authority structure?',
    'Historical analysis of whether any Hebrew-speaking authority (religious, national, or intellectual) ever held all three readings simultaneously, or whether adoption of one reading has systematically required rejection of the others.',
    'If the readings are genuinely alternatives (coexistent in different parties but not in any single framework), the constraint is a reading of a contested kernel. If they foreclose each other (one adoption logically requires denying the others), the kernel is polarized and my reading_relations assignment may be wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the three readings are coexistent alternatives or mutually foreclosing commitments.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of non-literary speakers'' evidence structural (the frame simply does not count their speech as data) or internalized (non-literary speakers have come to accept that their speech does not matter)?',
    'Post-adoption analysis: Do non-literary native speakers, when given a platform outside the literary-frame context, continue to accept the subordination of their evidence, or do they assert counter-evidence of their own linguistic vitality?',
    'If suppression is purely structural (frame-based exclusion), it may be reversible by reframing. If internalized (native speakers have accepted subordination), the constraint carries suppression into post-frame contexts and is more deeply extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression of non-literary speakers'' linguistic authority is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__literary_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(livi_tr_t0, observed).
narrative_ontology:measurement(livi_tr_t5, living_language_status__literary_continuity_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(livi_tr_t5, observed).
narrative_ontology:measurement(livi_tr_t10, living_language_status__literary_continuity_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(livi_tr_t10, observed).
narrative_ontology:measurement(livi_tr_t15, living_language_status__literary_continuity_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(livi_tr_t15, observed).
narrative_ontology:measurement(livi_tr_t25, living_language_status__literary_continuity_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(livi_tr_t25, observed).
narrative_ontology:measurement(livi_tr_t40, living_language_status__literary_continuity_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(livi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__literary_continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(livi_be_t0, observed).
narrative_ontology:measurement(livi_be_t5, living_language_status__literary_continuity_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(livi_be_t5, observed).
narrative_ontology:measurement(livi_be_t10, living_language_status__literary_continuity_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(livi_be_t10, observed).
narrative_ontology:measurement(livi_be_t15, living_language_status__literary_continuity_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(livi_be_t15, observed).
narrative_ontology:measurement(livi_be_t25, living_language_status__literary_continuity_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(livi_be_t25, observed).
narrative_ontology:measurement(livi_be_t40, living_language_status__literary_continuity_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(livi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__literary_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(livi_su_t0, observed).
narrative_ontology:measurement(livi_su_t5, living_language_status__literary_continuity_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(livi_su_t5, observed).
narrative_ontology:measurement(livi_su_t10, living_language_status__literary_continuity_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(livi_su_t10, observed).
narrative_ontology:measurement(livi_su_t15, living_language_status__literary_continuity_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(livi_su_t15, observed).
narrative_ontology:measurement(livi_su_t25, living_language_status__literary_continuity_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(livi_su_t25, observed).
narrative_ontology:measurement(livi_su_t40, living_language_status__literary_continuity_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(livi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__literary_continuity_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel admits three structurally distinct constraint readings. This file instantiates the literary_continuity_reading; the liturgical_preservation_reading and native_generation_reading are separate stories with their own ε values, stakeholder structures, and classifications. The three are linked via network.affects_constraints to show constraint-family membership. The literary-continuity reading influences (and coexists with) the liturgical and native readings: it redefines what counts as evidence of vitality, which changes the evidential status of ritual transmission and native speech. The three readings are not progressive refinements of each other; they are simultaneous, incompatible frames held by different parties in an ongoing dispute about what makes a language alive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, powerless, 0.95).
constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
