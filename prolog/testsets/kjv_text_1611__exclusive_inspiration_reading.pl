% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration and Textual Authority (One Reading)
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   The KJV Exclusive Inspiration reading is ONE interpretation of the
 *   contested kernel: the 1611 King James Version as English scripture
 *   authority. This reading claims the KJV is the exclusively inspired,
 *   inerrant English Bible and that all other translations are corrupted or
 *   inferior. The reading emerged in the 20th century among fundamentalist
 *   and evangelical groups as a response to the proliferation of modern
 *   translations. It functions as an institutional gatekeeping mechanism:
 *   KJV-Only leadership controls doctrinal authority, pulpit access, and
 *   publishing in their denominations by maintaining an exclusive claim to
 *   textual inspiration. The constraint exhibits classic snare properties —
 *   high suppression (barriers to accessing alternative evidence), high
 *   extractiveness (institutional gatekeeping privileges a specific group),
 *   and identity-locked victims (congregants whose identity is fused with
 *   belief in KJV exclusivity). The measurement trajectory shows
 *   acceleration: base extractiveness rose from 0.35 (early fundamentalist
 *   period, less institutional enforcement) to 0.62 (contemporary, with
 *   gatekeeping fully matured). Suppression requirement increased from 0.45
 *   to 0.68, indicating that maintaining the exclusive claim requires
 *   increasing censoring of counter-evidence (modern textual criticism,
 *   manuscript discoveries, linguistic analysis). Theater ratio rose from
 *   0.38 to 0.55, indicating the constraint's intellectual apparatus became
 *   increasingly performative — elaborate textual arguments defending a
 *   conclusion reached on institutional rather than empirical grounds.
 *
 * KEY AGENTS:
 *   - KJV-Only Institutional Leadership: Primary beneficiary (institutional/arbitrage) — controls denominational doctrine, publishing, pulpit access; nets material benefit through membership stability and theological gate-keeping
 *   - Fundamentalist Congregants: Primary victim (powerless/identity_locked) — identity fused with belief in KJV exclusivity; structurally mobile but cognitively trapped by decades of teaching that other translations compromise spiritual truth
 *   - Modern Translation Scholars: Secondary victim (moderate/constrained) — face publication rejection, career damage, loss of pulpit access if they advocate for modern translations; constrained by institutional gatekeeping within KJV-Only networks
 *   - Evangelical Denominations Using Other Translations: Secondary victim (powerful/mobile) — experience pressure to defend their translation choices against KJV-Only doctrinal attacks; have exit options but face reputational cost and congregant loss
 *   - Textual Criticism Field: Victim (analytical/arbitrage) — early manuscript evidence and linguistic analysis are systematically devalued or reinterpreted to defend the exclusive claim; scientific authority is subordinated to theological commitment
 *   - Translation Innovation: Victim (institutional/constrained) — new translation projects face doctrinal opposition; translators are pressured to justify why they are 'needed' given the KJV's claimed perfection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.62).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.68).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, snare).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration and Textual Authority (One Reading)").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious_studies/textual_criticism/theology").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, 'd164772a-8a86-42d2-aa8c-a40bfdd8e0fc').
narrative_ontology:cs_kernel_codification('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', fixed_text).
narrative_ontology:cs_authority_grounding('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', extraction).
narrative_ontology:cs_interpretation_layer_present('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc').
narrative_ontology:cs_reading_relation('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', foundational, divine_inspiration_localized_1611_translators).
narrative_ontology:cs_axiom_status(divine_inspiration_localized_1611_translators, holdable).
narrative_ontology:cs_axiom_grounding('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', divine_inspiration_localized_1611_translators, theological).
narrative_ontology:cs_axiom('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', foundational, modern_translation_methodology_inherently_corrupts).
narrative_ontology:cs_axiom_status(modern_translation_methodology_inherently_corrupts, overridden).
narrative_ontology:cs_axiom_grounding('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', modern_translation_methodology_inherently_corrupts, empirically_contingent).
narrative_ontology:cs_reference_frame('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', divinely_preserved_1611_text).
narrative_ontology:cs_drift_state('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', contemporary_manuscript_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d164772a-8a86-42d2-aa8c-a40bfdd8e0fc', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutional_leadership).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_scholars).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, evangelical_congregations_using_other_translations).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, textual_criticism_field).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, translation_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUNDAMENTALIST CONGREGANT (SNARE) — Identity fused with KJV-Only theology; believes using other translations is spiritual compromise. Structurally mobile (could switch translations or churches) but identity-locked by decades of teaching that the KJV is 'God's preserved word' in English. High extraction: suppression of alternative information (scholarly consensus on textual sources, translation methodology), constrained exit due to community expulsion risk, maximum experienced coercion.
constraint_indexing:constraint_classification(kjv_text_1611__exclusive_inspiration_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: MODERN TRANSLATION SCHOLAR (SNARE) — Constrained by institutional gatekeeping: KJV-Only gatekeepers control publishing venues, seminary curricula, and pulpit access in their denominations. Scholars face career damage, publication rejection, and loss of speaking platforms if they advocate for modern translations. High extraction: resource asymmetry, suppression of competing evidence (textual criticism scholarship), severe cost to exit professional networks aligned with KJV-Only leadership.
constraint_indexing:constraint_classification(kjv_text_1611__exclusive_inspiration_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KJV-ONLY INSTITUTIONAL LEADERSHIP (ROPE) — Primary beneficiary. Controls doctrinal authority, publishing, and pulpit access within their denominational network. Experiences the constraint as coordination: enforcing textual uniformity solves the legitimate coordination problem of maintaining denominational identity and scripture-based authority. Net beneficiary: extraction flows toward this agent through membership tithes, book sales, and institutional stability.
constraint_indexing:constraint_classification(kjv_text_1611__exclusive_inspiration_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: BROADER EVANGELICAL MOVEMENT (TANGLED_ROPE) — Mobile exit (use other translations without institutional penalty in non-KJV-Only denominations), but also beneficiaries and victims simultaneously. Coordination function: KJV-Only gatekeeping creates pressure to maintain distinct textual identity in a religiously pluralistic landscape. Extraction function: KJV-Only leadership uses exclusive authority claims to recruit from other evangelical groups. Mixed: genuine coordination (textual stability) paired with asymmetric extraction (doctrinal gatekeeping).
constraint_indexing:constraint_classification(kjv_text_1611__exclusive_inspiration_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER — DIVINE PRESERVATION (MOUNTAIN) — From a civilizational theological perspective, some readings hold that God's word is inherently preserved through any faithful translation; the KJV-Only claim is that one specific translation carries exclusive divine inspiration. This perspective risks naturalizing the exclusive inspiration claim as a transcendent fact rather than a historically contingent reading. Engine false summit detector will flag this as potential naturalization of an institutional arrangement.
constraint_indexing:constraint_classification(kjv_text_1611__exclusive_inspiration_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: VESTIGIAL KJV ESTABLISHMENT (PITON) — High theater ratio (0.55). The exclusive inspiration claim is performatively maintained through selective textual arguments, appeal to 'Majority Text' theories, and rejection of early manuscript evidence — the scholarly apparatus appears extensive but is functionally theatrical, designed to defend a conclusion reached on non-textual grounds (theological commitment, institutional inertia, identity preservation). The constraint persists through institutional inertia and community identity maintenance, not through intellectual force.
constraint_indexing:constraint_classification(kjv_text_1611__exclusive_inspiration_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kjv_text_1611__exclusive_inspiration_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_text_1611__exclusive_inspiration_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, TR),
    TR >= 0.70.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62): High. The KJV-Only reading creates significant asymmetric benefit for institutional leadership and cost for victims. The leadership controls interpretation authority and membership access; the victims face suppression of alternative information and constrained exit due to community identity fusion. The extractiveness is lower than in a pure predatory snare (0.75+) because the reading does provide some genuine coordination benefit: textual uniformity within denominations, shared liturgical language, and doctrinal coherence. But the primary mechanism is extraction via gatekeeping, not coordination. Suppression (0.68): High. Substantial barriers exist to accessing counter-evidence: KJV-Only institutions teach that modern scholarship is inherently corrupt ('compromised by naturalism,' 'denies biblical authority'), training congregants to dismiss academic textual criticism before encountering it. Early manuscript evidence is reframed as 'corrupted texts' rather than evaluated empirically. Modern translations are presented as spiritually dangerous rather than as legitimate translation alternatives. These barriers are sufficient to prevent most congregants from encountering the counter-narrative. Theater ratio (0.55): Moderate-High. The intellectual apparatus defending exclusive inspiration is performative in structure. Textual arguments (Majority Text theory, appeals to 'Received Text' authority) appear rigorous but are selected post-hoc to defend a conclusion already reached on theological grounds. The translators' original methodology is not replicated or empirically tested; instead, it is invoked as authoritative without examination. Modern translations are critiqued through selective evidence (problems in specific verses) rather than systematic comparison. The theater has increased over time as counter-evidence has accumulated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence. The institutional leadership sees a rope (legitimate coordination through shared textual authority). The fundamentalist congregant sees a mountain (the KJV's inspiration feels immutable, divinely guaranteed) but this is a false summit — the 'immutability' is cognitive, not structural. The modern translation scholar sees a snare (high extraction, severe career cost to exit, gatekeeping of scientific authority). The broader evangelical movement sees tangled rope (they benefit from textual stability but are also victims of exclusive gatekeeping). The analytical observer sees a mountain (divine preservation as a transcendent law) but the false summit detector flags this as naturalization of a contingent institutional reading. The piton perspective captures that the intellectual apparatus is theatrical — elaborate but functionally degraded, maintained through institutional inertia rather than rational persuasion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the exclusive inspiration claim. KJV-Only institutional leadership is a beneficiary with arbitrage options (they can always claim textual authority; their exit cost is zero because they control the game). Their derived d is low (~0.10-0.15), producing negative or minimal effective extraction from their perspective — they experience the constraint as beneficial coordination. Fundamentalist congregants are victims with identity-locked exit (they can structurally leave the church or switch translations but cannot psychologically exit because their identity is fused with belief in KJV exclusivity). Their d is high (~0.85-0.90), producing maximum effective extraction — they bear the full suppression cost. Modern translation scholars are victims with constrained exit (they face material career cost but not total barrier). Their d is moderate-high (~0.70-0.75), producing severe effective extraction — they lose professional opportunities and institutional access. The analytical observer's d (canonical 0.73) produces severe effective extraction from that perspective because the observer sees the institutional mechanisms clearly and experiences the false summit as a violation of epistemic standards.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by identifying that the snare classification is correct at the institutional and victim levels but the mountain classification (divine preservation as natural law) is a false summit. The resolution chain: (1) KJV-Only leadership benefits materially and institutionally from the exclusive claim, (2) the claim's defensibility depends on suppressing counter-evidence (early manuscripts, modern linguistics), (3) the suppression mechanism is institutional gatekeeping, not empirical weakness, therefore (4) the constraint is a snare with false-summit risk, not a mountain. The mandatrophy is resolved by separating the theological claim (divine preservation is real) from the institutional reading (only the KJV 1611 translation manifests divine preservation). The theological claim might be defensible across multiple readings; the institutional reading is snare-class extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_versus_empirical_grounding,
    'Is the exclusive inspiration claim grounded in empirical textual evidence or in prior theological commitment to divine preservation?',
    'Genealogical analysis of the claim''s history: does it precede or follow the textual evidence? Examination of whether counter-evidence (early manuscripts, linguistic analysis) is evaluated by the same standards as supporting evidence.',
    'If empirical: constraint is a rope (legitimate coordination around best evidence). If theological: constraint is a snare with identity-locked victims (victims'' identity is fused with the claim regardless of evidence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_versus_empirical_grounding, empirical, 'Whether exclusive inspiration is empirically or theologically grounded').

omega_variable(
    manuscript_evidence_interpretation,
    'Do early Greek New Testament manuscripts (p45, p66, p75, Codex Sinaiticus, Codex Vaticanus) represent corruptions from a lost ''original inspired text'' or legitimate textual variants inherent to manuscript transmission?',
    'Comparative analysis of textual variants; examination of whether variants affect doctrine or are neutral; assessment of whether the claimed ''original inspired text'' was ever empirically identical across all manuscript witnesses.',
    'If variants are corruptions: KJV-Only exclusive claim is defensible (other translations are based on corrupted manuscripts). If variants are inherent: KJV-Only claim rests on selecting one manuscript tradition arbitrarily (constraint is extraction via gatekeeping, not truth discovery).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manuscript_evidence_interpretation, empirical, 'Whether early manuscript variants represent corruptions or inherent transmission processes').

omega_variable(
    translation_versus_transcription_conflation,
    'Is the KJV-Only claim distinguishing between (a) discovering the original inspired transcription and (b) asserting that a translation 370+ years removed from original composition is itself divinely inspired?',
    'Historical analysis of KJV-Only theological writings; examination of whether the claim addresses the gap between original Greek/Aramaic and 1611 English translation; assessment of how identity-locked agents respond to the translation-versus-transcription distinction.',
    'If conflated: the constraint naturalizes a category error (claiming a translation is as inspired as the original) and uses the conflation to gate-keep authority. This deepens the snare and identity-lock mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_versus_transcription_conflation, conceptual, 'Conceptual conflation of discovering original text with claiming translation is divinely inspired').

omega_variable(
    institutional_gatekeeping_versus_doctrinal_truth,
    'To what degree is the extractiveness in this constraint driven by genuine belief in KJV exclusive inspiration versus institutional need to maintain control over scripture interpretation and membership identity?',
    'Ethnographic analysis of KJV-Only institutional decision-making; examination of whether gatekeeping intensity tracks with doctrinal conviction or with competitive pressure from other translation movements; analysis of who benefits materially from the exclusive claim.',
    'If driven by genuine belief: constraint is mixed (rope + snare, identity-locked victims, weak extraction). If driven by institutional gatekeeping: constraint is primarily snare with secondary institutional benefit (stronger extraction, performative theology).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_versus_doctrinal_truth, empirical, 'Degree to which constraint is driven by institutional gatekeeping versus theological conviction').

omega_variable(
    false_summit_mountain_candidate,
    'Is the ''divine preservation'' principle a genuine theological law that applies universally (mountain) or a commitment specific to KJV-Only institutional leadership that naturalizes a particular reading (false summit)?',
    'Cross-denominational theological survey: do other evangelical, reformed, and pentecostal traditions affirm divine preservation while accepting multiple translations as equally inspired? If yes, the mountain claim is false-summit (naturalization of a reading that benefits KJV-Only leadership).',
    'If false summit confirmed: KJV-Only reading is snare with beneficiaries (institutional leadership); alternative readings are coexisting live positions, not heresy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_candidate, empirical, 'Whether divine preservation doctrine supports exclusive KJV inspiration or is compatible with multiple translations').

omega_variable(
    reading_commission_drift,
    'Has the KJV-Only reading''s authority claim drifted from historical defense (the 1611 translators produced an inspired translation given their available sources) to anachronistic claim (the 1611 translation is inspired by modern standards despite being based on incomplete Greek manuscripts)?',
    'Historical textual analysis: did the 1611 translators have access to the early manuscripts now known to support modern translations? If no, the original reading was defensible as ''best available.'' If yes (or later editions incorporated changes), the claim has drifted to a position the translators would not have held.',
    'If drift confirmed: the reading has shifted from a reasonable historical claim to an identity-locked theological commitment. This deepens both the false summit risk and the snare mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commission_drift, empirical, 'Historical drift in the KJV-Only reading''s authority claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_excl_tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(kjv_excl_tr_t50, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement(kjv_excl_tr_t100, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(kjv_excl_be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kjv_excl_be_t50, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(kjv_excl_be_t100, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(kjv_excl_su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(kjv_excl_su_t50, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(kjv_excl_su_t100, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% The KJV text kernel decomposes into three structurally distinct constraints representing three readings: (1) EXCLUSIVE_INSPIRATION_READING (this file, ε=0.62, snare) — the KJV is divinely inspired and all others are corrupted; (2) REVISABLE_TRANSLATION_READING (separate file, expected ε~0.30, rope or scaffold) — the KJV is faithful given its era but must be updated; (3) FUNCTIONAL_EQUIVALENCE_READING (separate file, expected ε~0.15, rope) — all translations conveying the original meaning are equally inspired. Each reading has different victim sets, different extractiveness values, and different institutional beneficiaries. The exclusive inspiration reading carries the highest extractiveness because it requires the most institutional gatekeeping and suppression of counter-evidence. The readings are not empirically competing (each could be true); they are institutionally competing (adherents derive different authority structures from them). Decomposition is required by the ε-invariance principle: the observable (textual evidence, theological reasoning, institutional gatekeeping intensity) changes ε. Three readings, three stories, three ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
