% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV as Revisable Translation (Textual Criticism Reading)
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   The King James Version, published in 1611, has occupied a unique position
 *   in English-speaking Christianity: venerated as a literary and cultural
 *   monument, treated as scripturally authoritative by conservative
 *   Protestant denominations, yet increasingly recognized by biblical
 *   scholars as a 17th-century translation reflecting the manuscript evidence
 *   and linguistic knowledge available to its translators. The
 *   revisable-translation reading instantiates one interpretation of how the
 *   KJV should be understood: as a historically important but improvable
 *   translation, subject to revision as better manuscripts are discovered and
 *   linguistic knowledge advances. This reading creates a constraint that
 *   operates across theological, academic, and commercial institutions. It
 *   frames translation work as an objective scholarly task (recovering the
 *   'best' text) while masking the extractive dynamics: beneficiary academic
 *   scholars and publishers gain authority and market share; victim
 *   conservative denominations see their textual foundation destabilized; the
 *   abstract good of textual fidelity is invoked to justify continuous
 *   revision cycles. The constraint exhibits genuine coordination function
 *   (how should communities engage with historical textual evidence?)
 *   alongside extraction (whose authority gets recognized as legitimate? who
 *   controls the reading practices of congregations?). The
 *   revisable-translation reading is one of three major interpretations of
 *   the KJV kernel, each with different structural implications for textual
 *   authority, denominational autonomy, and the relationship between
 *   historical discovery and theological commitment.
 *
 * KEY AGENTS:
 *   - Academic Biblical Scholars: Organized/arbitrage beneficiary — gain interpretive authority and professional advancement through textual criticism; coordinate via peer review and critical editions.
 *   - Conservative Protestant Denominations: Moderate-to-powerless/trapped victim — face erosion of textual certainty and denominational control over biblical interpretation; identity fused with KJV authority.
 *   - Modern Translation Publishers: Institutional/arbitrage beneficiary — capture market demand for revised translations; extract value through publication cycles while coordinating genuine translation work.
 *   - Mainline Protestant Congregations: Moderate/constrained — caught between institutional tradition and scholarly legitimacy; benefit from improved understandability but pay costs of navigating translation diversity.
 *   - Ecumenical Textual Criticism Movement: Organized/constrained — coordinate across denominational boundaries to revise texts based on cumulative evidence; see their work as temporary (scaffold) with natural sunset as consensus emerges.
 *   - Traditional KJV Cultural Authority: Institutional/arbitrage (piton) — persists through inertia and literary citation despite functional authority atrophy.
 *   - Analytical Observer: Analytical/analytical — risks naturalizing the revisable-translation reading as an immutable textual law rather than seeing it as a contested institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.38).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.35).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV as Revisable Translation (Textual Criticism Reading)").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious_studies/textual_criticism/theology").

domain_priors:requires_active_enforcement(kjv_text_1611__revisable_translation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, '21932abf-70b0-4173-9ad8-27233ac0f2db').
narrative_ontology:cs_kernel_codification('21932abf-70b0-4173-9ad8-27233ac0f2db', fixed_text).
narrative_ontology:cs_authority_grounding('21932abf-70b0-4173-9ad8-27233ac0f2db', extraction).
narrative_ontology:cs_interpretation_layer_present('21932abf-70b0-4173-9ad8-27233ac0f2db').
narrative_ontology:cs_reading_relation('21932abf-70b0-4173-9ad8-27233ac0f2db', kjv_text_1611__exclusive_inspiration_reading, coexists_with).
narrative_ontology:cs_reading_relation('21932abf-70b0-4173-9ad8-27233ac0f2db', kjv_text_1611__functional_equivalence_reading, influences).
narrative_ontology:cs_axiom('21932abf-70b0-4173-9ad8-27233ac0f2db', foundational, manuscript_evidence_primary_authority).
narrative_ontology:cs_axiom_status(manuscript_evidence_primary_authority, holdable).
narrative_ontology:cs_axiom_grounding('21932abf-70b0-4173-9ad8-27233ac0f2db', manuscript_evidence_primary_authority, empirically_contingent).
narrative_ontology:cs_axiom('21932abf-70b0-4173-9ad8-27233ac0f2db', foundational, scholarly_consensus_legitimate_arbiter).
narrative_ontology:cs_axiom_status(scholarly_consensus_legitimate_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('21932abf-70b0-4173-9ad8-27233ac0f2db', scholarly_consensus_legitimate_arbiter, conventional).
narrative_ontology:cs_reference_frame('21932abf-70b0-4173-9ad8-27233ac0f2db', manuscript_recovery_imperative).
narrative_ontology:cs_drift_state('21932abf-70b0-4173-9ad8-27233ac0f2db', contemporary_post_digital_manuscript_analysis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('21932abf-70b0-4173-9ad8-27233ac0f2db', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, modern_translation_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, ecumenical_denominations).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, kjv_textual_authority).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, conservative_protestant_denominations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSERVATIVE DENOMINATIONAL BELIEVER (SNARE) — Trapped within an identity fused to KJV textual authority. The revisable-translation reading presents a direct threat to the believer's framework: if the KJV is merely 'one translation among many,' the believer must either abandon the security of a fixed textual foundation or engage in ongoing critical apparatus review (high cognitive cost). The suppression is strong: denominational authority structures actively discourage engagement with textual criticism, treating it as a threat to faith. The extraction flows from the believer's lock into a particular reading tradition toward the broader scholarly epistemic system.
constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAINLINE PROTESTANT CONGREGATION (TANGLED ROPE) — Constrained by institutional tradition and membership composition but also benefits from the revisable-translation reading: modern translations offer better access to the biblical text and align with scholarly consensus. The constraint extracts some value (congregations must navigate denominational expectations about which translations to use; some members feel disoriented by translation diversity) but also coordinates genuine benefit (improved understandability, ecumenical bridge-building). Moderate power because denominations have agency in translation selection; constrained exit because changing institutional reading traditions is slow and carries social cost.
constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACADEMIC BIBLICAL SCHOLARSHIP COMMUNITY (ROPE) — Primary beneficiary. The revisable-translation reading creates a pure coordination function: it enables communication, comparison, and debate about textual choices using shared frameworks (manuscript evidence, linguistic analysis, historical-critical method). The scholarly community has arbitrage access — scholars can adopt, adapt, or critique the reading without organizational or career barriers. Extraction from the scholarly perspective is minimal; the constraint solves a real collective action problem (how to evaluate 16th-century translation choices using 21st-century philological knowledge). The scholar sees this as genuine coordination machinery.
constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERN TRANSLATION PUBLISHING INDUSTRY (TANGLED ROPE) — Institutional beneficiary with arbitrage options. The revisable-translation reading creates market demand for competing modern translations (ESV, NRSV, NIV, CSB, NASB, etc.). Publishers benefit from the reading's justification of translation multiplicity — each publisher can market their version as addressing specific scholarly or devotional needs. But the reading also extracts: the constraint forces publishers to engage in constant translation revision cycles, justify methodological choices to scholarly gatekeepers, and manage the performative theater of translation committees and textual apparatus publication. High institutional power but the arbitrage options mean publishers can exit to alternative markets. The effective extraction is real but not maximal.
constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ECUMENICAL TEXTUAL CRITICISM MOVEMENT (SCAFFOLD) — Organized denominations and scholars coordinating to revise biblical texts based on cumulative evidence. This perspective sees the constraint as a temporary coordination mechanism with a natural sunset: as critical editions (UBS, Nestle-Aland) achieve near-consensus on manuscript hierarchy and textual decisions, the 'need' for translation revision will plateau. The movement has agency and sees a clear exit: once the textual decisions stabilize, translation work becomes maintenance rather than pioneering. Low theater because the work is intellectually genuine; low extraction because the outcome is a public good (universally accessible critical text). Sunset clause is real: textual criticism's core work was concentrated in the 19th-20th centuries; by the 22nd century, variant readings will be fully cataloged and the primary innovation work will be exhausted.
constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL KJV CULTURAL AUTHORITY (PITON) — The King James Version's 400+ year cultural authority has degraded into theatrical deference. The text is cited for its literary and historical significance, but its functional authority (what believers actually read and memorize) has largely shifted to modern translations. The constraint persists because institutional inertia — churches maintain pew Bibles, literary allusions invoke KJV phrasing, aesthetic deference continues — but the primary function (as a living translation for active biblical study) is largely atrophied. Theater ratio 0.48 reflects this: some genuine coordination around literary heritage and historical continuity remains, but much is performative citation of the KJV's cultural status rather than engagement with its textual substance. The reading sees traditional KJV authority as hollowed out.
constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, textual drift is inevitable: any transmitted text accumulates copyist errors, variant readings, and scribal harmonizations. The analytical observer sees the 'need' for revision as an immutable feature of textual transmission rather than a contingent institutional choice. Better manuscripts will always be discovered; linguistic knowledge will always improve. Therefore, translation revision is simply what responsible stewardship of ancient texts requires — a natural law, not a extractive constraint. However, this perspective risks naturalizing what is actually a contested institutional reading: the claim that textual discovery SHOULD drive translation revision rather than accepting a stable text as fixed. The engine will likely classify this as a false summit.
constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_text_1611__revisable_translation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, TR),
    TR >= 0.70.

:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading generates real extraction — conservative denominations bear significant costs to denominational autonomy and textual authority, and the benefit (access to 'better' manuscripts) accrues primarily to academic institutions and publishers. However, the extraction is not maximal because (a) genuine scholarly coordination exists (textual criticism solves real problems about variant readings), (b) congregations have agency in which translations to use, and (c) the extraction flows through the frame of 'objective scholarly discovery' rather than overt coercion. The measured value (0.38) reflects that the constraint is hybrid: real coordination value plus real asymmetric extraction. Theater ratio (0.48): Moderate. Textual criticism itself is intellectually genuine — scholars are doing real analytical work on variant readings and manuscript relationships. But the communication of textual criticism to congregations exhibits theater: the 'need' for revision is presented as objective scholarly imperative (following the evidence) when it is actually a contested institutional choice about epistemic authority. The theater is not maximal (not 0.7+) because the underlying scholarly work is not performative, but the social function (how denominations relate to textual authority) is partly theatrical. Suppression (0.35): Moderate. Barriers to exit include denominational authority structures that discourage engagement with textual criticism (presenting it as a threat), the cognitive cost of abandoning textual certainty, and the social cost of breaking from established reading traditions. But suppression is not high (not 0.6+) because individual congregation members can access modern translations independently, ecumenical movements provide organizational alternative to conservative denominational gates, and scholarly consensus is increasingly visible through popular-level biblical studies resources.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The conservative denominational believer sees a snare: the reading destabilizes their faith foundation. The academic scholar sees rope: pure coordination without extraction. The publisher sees tangled rope: both real market opportunity and genuine extraction from translation work. The ecumenical movement sees a scaffold: temporary coordination with a natural sunset. The cultural authority sees a piton: the work persists through inertia, not function. The analytical observer risks a false summit: naturalizing the reading as inevitable textual law. These gaps reveal that the classification depends entirely on the observer's structural position — there is no neutral or 'correct' classification. The presheaf over the observation site is the answer: the constraint IS the set of divergent perspectival readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the agent's structural position: (1) Victim status + trapped/identity-locked exit → high d (powerless believer). (2) Victim status + constrained exit → moderate-high d (mainline congregations, though they also benefit from modern translations). (3) Beneficiary status + arbitrage exit → low d (academic scholars, publishers). (4) Organized status with constrained exit but genuine coordination function → lower d than victims despite coordination role. The revisable-translation reading's extraction mechanism is embedded in the framing of 'objective scholarly discovery' — this masks the directionality by presenting asymmetric benefit (academic authority gains) as epistemic necessity (truth about ancient manuscripts). The suppression value (0.35) reflects that conservative denominations face real barriers to resisting the reading (scholarly prestige, institutional ecumenical pressure) but not insurmountable barriers (they retain agency in translation selection, maintain alternative institutional structures).
 *
 * MANDATROPHY ANALYSIS:
 *   The revisable-translation reading resolves mandatrophy by being explicit about its epistemic commitments: it declares that historical-critical scholarship provides the primary authority for textual decisions, that better manuscript evidence justifies translation revision, and that denominational textual traditions are subordinate to scholarly consensus. These commitments are NOT empirically false — the scholarly work on manuscripts is real and rigorous — but they are NOT empirically necessary. An alternative reading (the exclusive-inspiration reading) makes contrary commitments: that the received text embodies providential stability, that denominational continuity has epistemic weight equal to manuscript discovery, that translation revision risks spiritual harm. Both readings are internally coherent; they differ on foundational epistemic authority. The mandatrophy is resolved by naming the axiomatically irreducible disagreement: the readings constitute different commitments about how historical evidence and theological authority relate. No amount of additional manuscript evidence will resolve this disagreement because it is not an empirical question — it is a question about which epistemic frameworks get recognized as legitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manuscript_hierarchy_sufficiency,
    'Does cumulative manuscript evidence provide sufficient epistemic grounds to declare some variant readings ''incorrect'' and others ''authentic,'' or does textual criticism always remain probabilistic and subject to reinterpretation?',
    'Longitudinal study of how scholarly consensus on specific variant readings has changed (e.g., the longer ending of Mark, the Pericope Adulterae); assess whether new discoveries are revising prior consensus or merely refining probabilities.',
    'If sufficient (binary authenticity): the revisable-translation reading is justified — revisions represent genuine textual recovery. If probabilistic: revisions are aesthetic/interpretive choices reframed as objective recovery, and the reading undergoes classification shift toward snare (extraction masked as scholarship). Confidence in translation authority collapses either way, but for different structural reasons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manuscript_hierarchy_sufficiency, empirical, 'Whether manuscript evidence provides sufficient epistemic grounds for binary textual authenticity judgments').

omega_variable(
    reading_tradition_versus_manuscript_authority,
    'When textual criticism recovers a reading that contradicts centuries of theological interpretation (e.g., shorter ending of Mark, non-subordinationist Colossians readings), which carries greater authority: the manuscript evidence or the received interpretive tradition?',
    'Case studies of specific textual recovery moments (Hort vs. Tregelles priority, discovery effects of early Coptic witnesses); analysis of how denominations have adopted or resisted manuscript-based revisions that challenged doctrinal tradition.',
    'If manuscript authority is primary: the revisable-translation reading is validated — modern knowledge corrects historical contingency. If interpretive tradition has equal or greater weight: the reading is exposed as privileging one epistemic framework (academic historicism) over another (theological continuity). This is the core distinction between the revisable-translation and exclusive-inspiration readings — it is fundamentally about epistemic authority, not empirical facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_tradition_versus_manuscript_authority, conceptual, 'Relative epistemic authority: manuscript evidence vs. received interpretive tradition').

omega_variable(
    translation_multiplicity_market_capture,
    'Does the explosion of modern English Bible translations (ESV, NIV, NRSV, CSB, NASB, NET, NKJV, etc.) represent genuine consumer choice based on scholarly differences, or market segmentation that artificially creates ''differences'' to support publishing revenue?',
    'Comparative textual analysis: measure actual divergence between major modern translations at disputed readings; correlate translation release dates and market strategy announcements; survey translation committee compositions for ideological/denominational patterns.',
    'If genuine choice: the publishing industry''s tangled-rope classification is accurate — some extraction but real coordination benefit (customers get translations matched to needs). If market capture: reclassify toward snare — the revisable-translation reading justifies perpetual translation cycles that serve publisher interests, not scholarly or congregational needs. Theater ratio would rise accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_multiplicity_market_capture, empirical, 'Whether translation multiplicity serves consumer choice or publisher market segmentation').

omega_variable(
    reading_as_kernel_instantiation,
    'Is the revisable-translation reading a singular constraint, or is it one instantiation of a deeper kernel about the relationship between historical discovery and theological authority?',
    'Mapping sibling readings'' core premises and identifying whether they share a common kernel (KJV as stabilized commitment) or represent alternative stable commitments. Examining whether the same structural dynamics apply to other historical-critical challenges (archaeological findings contradicting biblical narratives, literary-critical challenges to Mosaic authorship, etc.).',
    'If shared kernel: the reading family (exclusive-inspiration, functional-equivalence, revisable-translation) represents a single contested commitment with multiple readings. The mandatrophy between readings reflects irreducible disagreement about epistemic authority. If alternative kernels: the readings are fundamentally incommensurable — not negotiable through scholarly debate but rooted in different theological commitments. This determines whether classification disagreement is empirically resolvable or axiomatically irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_kernel_instantiation, conceptual, 'Whether this reading instantiates a shared kernel with identifiable sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_revisable_tr_t0, kjv_text_1611__revisable_translation_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(kjv_revisable_tr_t100, kjv_text_1611__revisable_translation_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(kjv_revisable_tr_t200, kjv_text_1611__revisable_translation_reading, theater_ratio, 200, 0.48).

% Extraction over time
narrative_ontology:measurement(kjv_revisable_be_t0, kjv_text_1611__revisable_translation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(kjv_revisable_be_t100, kjv_text_1611__revisable_translation_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(kjv_revisable_be_t200, kjv_text_1611__revisable_translation_reading, base_extractiveness, 200, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, information_standard).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% The KJV kernel (kjv_text_1611) decomposes into three structurally distinct constraints, one for each reading. Each reading has its own epsilon value, beneficiary/victim structure, and perspectival classification: the revisable-translation reading (this file, ε≈0.38) frames textual work as scholarly recovery; the exclusive-inspiration reading (sibling, ε≈0.32) frames textual work as defending a fixed tradition; the functional-equivalence reading (sibling, ε≈0.25) frames textual work as preserving a living text's use-value rather than recovering an 'original.' The three readings compete as institutional interpretations of the same inherited text. Each reading creates different extraction dynamics and different perspectival gaps. They are not alternative observations of one constraint but alternative institutional commitments regarding what the KJV constraint IS.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__revisable_translation_reading, institutional, 0.28).
constraint_indexing:directionality_override(kjv_text_1611__revisable_translation_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
