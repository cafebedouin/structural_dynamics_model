% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Vedic Spiritual Corpus as Non-Prescriptive Metaphorical Teaching
 *   domain: religious_studies/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   The reformist_spiritual_reading instantiates one interpretation of the
 *   Vedic corpus that holds Vedic texts describe spiritual unity and
 *   metaphorical cosmology with no prescriptive social content. This reading
 *   permits contemporary practitioners and reform movements to draw on Vedic
 *   authority while rejecting varna-based hierarchy. It is instantiated and
 *   maintained by reform theologians, academic scholars of Hinduism,
 *   interfaith dialogue organizations, and practitioners who seek spiritual
 *   value without endorsing caste. It is contested by orthodox interpreters
 *   who hold that the Vedas literally mandate the varna system, and it was
 *   shaped historically by colonial-era encounters and indigenous reform
 *   movements. This story generates the constraint AS THIS READING SEES IT:
 *   low extractiveness (no victim set, no coercive hierarchy imposed through
 *   textual reinterpretation), low suppression (the reading is increasingly
 *   institutionalized), low theater (the interpretive work is genuine
 *   scholarship, not performative), and substantial resistance (orthodox
 *   institutional authorities oppose it and conservative practitioners doubt
 *   its legitimacy). The claim (rope) and metrics (low extraction, organized
 *   beneficiaries, no victims) are aligned because this reading does not
 *   extract from anyone — it coordinates spiritual practitioners around a
 *   shared textual authority without imposing hierarchy or coercion.
 *
 * KEY AGENTS:
 *   - reform_interpreters (institutional, agenda-setter): maintain and defend the reading; author commentaries and educational materials; argue in public discourse
 *   - spiritual_practitioners (organized, beneficiary): draw on this reading for spiritual practice compatible with contemporary ethics
 *   - egalitarian_movements (organized, beneficiary): use this reading to counter brahminical varna authority
 *   - orthodox_varna_interpreters (institutional, excluded): dispute the reading and defend literal varna prescription
 *   - brahminical_institutional_authority (institutional, excluded): institutional legitimacy partially depends on the orthodox varna reading; this reading threatens that legitimacy
 *   - historical_scholarly_consensus (analytical observer): provides independent corroboration through historical-critical analysis of Vedic texts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Spiritual Corpus as Non-Prescriptive Metaphorical Teaching").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '4c2922dd-31e4-430b-9f33-a2acef069830').
narrative_ontology:cs_kernel_codification('4c2922dd-31e4-430b-9f33-a2acef069830', fixed_text).
narrative_ontology:cs_authority_grounding('4c2922dd-31e4-430b-9f33-a2acef069830', lineage).
narrative_ontology:cs_interpretation_layer_present('4c2922dd-31e4-430b-9f33-a2acef069830').
narrative_ontology:cs_reading_relation('4c2922dd-31e4-430b-9f33-a2acef069830', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c2922dd-31e4-430b-9f33-a2acef069830', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('4c2922dd-31e4-430b-9f33-a2acef069830', foundational, vedic_texts_soteriological_not_prescriptive).
narrative_ontology:cs_axiom_status(vedic_texts_soteriological_not_prescriptive, holdable).
narrative_ontology:cs_axiom_grounding('4c2922dd-31e4-430b-9f33-a2acef069830', vedic_texts_soteriological_not_prescriptive, empirically_contingent).
narrative_ontology:cs_axiom('4c2922dd-31e4-430b-9f33-a2acef069830', foundational, varna_system_later_dharmashastra_imposition).
narrative_ontology:cs_axiom_status(varna_system_later_dharmashastra_imposition, holdable).
narrative_ontology:cs_axiom_grounding('4c2922dd-31e4-430b-9f33-a2acef069830', varna_system_later_dharmashastra_imposition, empirically_contingent).
narrative_ontology:cs_reference_frame('4c2922dd-31e4-430b-9f33-a2acef069830', spiritual_liberation_cosmology).
narrative_ontology:cs_drift_state('4c2922dd-31e4-430b-9f33-a2acef069830', contemporary_critical_scholarship, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4c2922dd-31e4-430b-9f33-a2acef069830', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reform_interpreters).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, egalitarian_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practitioners who engage Vedic texts for spiritual practice (meditation, yoga, philosophical inquiry) benefit from a reading that treats the cosmology as metaphorical and soteriological rather than as a mandate for social hierarchy. This reading permits them to extract spiritual value without endorsing or perpetuating caste-based social structures. They choose this reading because it aligns their practice with contemporary ethical commitments.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners, beneficiary,
    organized, generational, mobile, global).

% Scholars, theologians, and institutional interpreters (reform movements, academic religious studies departments, interfaith dialogue organizations) who actively promote and defend the reading that Vedic texts contain no prescriptive social content. They author commentaries, teach this interpretation, and argue for it in public discourse. They maintain the intellectual and institutional infrastructure that sustains this reading.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reform_interpreters, agenda_setter,
    institutional, generational, mobile, global).

% Social movements advocating for caste abolition and gender equality benefit from this reading because it permits the claim that Vedic authority does not mandate hierarchy, thereby undercutting the legitimacy of caste as 'divinely ordained.' They use this reading as a counter-resource against orthodox invocations of Vedic varna doctrine.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, egalitarian_movements, beneficiary,
    organized, generational, mobile, global).

% Interpreters who hold that Vedic texts literally mandate varna hierarchy as cosmic order and who frame this mandate as immutable. They are excluded from the consensus space where this reading holds authority. They dispute that the texts are 'merely metaphorical' and argue that such reinterpretation amounts to textual distortion motivated by colonial guilt or modernist apologetics.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_interpreters, excluded,
    institutional, generational, constrained, global).

% Traditional brahminical authorities (temple hierarchies, sampradaya leadership, Vedic schools) whose institutional legitimacy rests partly on the claim that they preserve Vedic teaching in its original prescriptive form. This reading challenges that legitimacy by asserting that 'original Vedic teaching' contains no social prescription. Their structural interest is in maintaining the authority-grounding that claims Vedic texts mandate the brahminical role.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, brahminical_institutional_authority, excluded,
    institutional, civilizational, trapped, regional).

% The academic historical-critical study of Vedic texts and their authorship, redaction, and social context. Scholarly consensus has increasingly held that the Vedas emerged from specific historical periods, were composed and compiled by human authors with particular interests, and that the varna system was codified later in texts like the Dharmaśāstras, not in the oldest Vedic layers. This reading aligns with that scholarly consensus.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, historical_scholarly_consensus, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, reform_interpreters).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__reformist_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared textual authority that permits spiritual practitioners and egalitarian reformers to draw on Vedic texts for soteriological (spiritual liberation) and ethical value while simultaneously rejecting hierarchical social prescription. Coordinates believers around a version of textual authority that does not entail caste acceptance.
% TRANSFER_FUNCTION: Transfers interpretive authority from brahminical institutional gatekeepers to a distributed set of reform interpreters and scholarly authorities. Moves the legitimacy status of Vedic texts from a source of prescriptive social law to a source of metaphorical spiritual insight. No direct economic transfer; the movement is one of hermeneutic and institutional authority.
% ABSENT_VOICES: Orthodox varna interpreters and brahminical institutional authorities are structurally excluded from the consensus space. They would argue that the reading is a distortion of the texts' actual prescriptive content and that it undermines the authority of the Vedas as guidance for social order. Excluded from the deliberative space where this reading's legitimacy is established and defended.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and only the orthodox varna reading held institutional authority, spiritual practitioners who wish to draw on Vedic texts without endorsing caste would lose a major interpretive resource; reform movements would lose a counter-claim against brahminical authority; interfaith dialogue would be constrained by the reassertion of Vedic varna doctrine. The social and institutional landscape of religious authority would shift toward brahminical institutional control and away from pluralistic interpretation.
% FOUNDING_PROBLEM: How can practitioners and reformers draw spiritual and ethical value from Vedic texts while rejecting the varna-based social hierarchy that brahminical authorities claim the texts mandate? How can the Vedas be 'salvaged' as a legitimate resource for contemporary spiritual and egalitarian movements?
% FOUNDING_PROBLEM_CORROBORATION: This reading is corroborated by historical-critical scholarship on Vedic authorship and composition (the varna system emerges in later Dharmaśāstra texts, not the oldest Vedic layers); by comparative religious studies emphasizing the metaphorical and soteriological dimensions of the cosmology; by testimony from Hindu reform movements and interfaith dialogue participants who affirm this reading as essential to their ethical commitments; and by the lived practice of millions of contemporary practitioners who engage Vedic texts through this interpretive lens. Corroboration comes from outside the benefiting parties: secular historians, non-Hindu scholars, and practitioners across denominations affirm the textual-historical evidence.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading generates no victim set and imposes no hierarchy through its textual interpretation — it redistributes interpretive authority from brahminical gatekeepers to reform scholars and practitioners, but that is a hermeneutic shift, not economic extraction. The measurement trajectory shows extractiveness rising slightly from 1800 (0.02, when the reading was marginal) to 2026 (0.15), reflecting gradual institutionalization and mainstream acceptance; as the reading becomes accepted, it requires less defensive work and the 'cost' of maintaining it becomes visible. Suppression is low (0.08) and declining (1800: 0.15, 2026: 0.08) because institutional orthodoxy no longer holds monopoly authority over Vedic interpretation; the reading faces resistance but not active coercion. Theater is low (0.12) and stable: the interpretive work is genuine scholarship grounded in textual analysis and historical evidence, not performative maintenance of a degraded function. Resistance is high (0.58) because orthodox authorities and conservative practitioners actively dispute this reading and defend the prescriptive varna interpretation — the reading is contested, not settled. Accessibility_collapse is low (0.22) because alternatives to this reading remain institutionally visible and defended; the orthodox varna reading is not eliminated, only minoritized in academic and reform circles. All metrics are authored on a single shared time grid (1800, 1880, 1920, 1960, 1990, 2026) so the engine samples all metrics at every point. Early measurements are marked 'projected' because direct historical records are sparse before 1880; later measurements are 'observed' from documented reform movements, academic scholarship, and practitioner discourse.
 *
 * PERSPECTIVAL GAP:
 *   Reform interpreters and mainstream practitioners who adopt this reading experience it as liberation and authentic reinterpretation. Orthodox authorities and brahminical institutional leadership experience it as delegitimization and textual distortion. Scholars across traditions experience it as grounded in historical evidence and sound textual analysis. The engine computes per-seat classifications from the structural data: a reform interpreter's seat should compute as beneficiary (gains authority/reputation, shares textual legitimacy); a practitioner's seat should compute as beneficiary (gains uncoerced access); an orthodox interpreter's seat should compute as payer (loses monopoly authority, bears social delegitimization). The constraint is NOT a snare to the orthodox seats because there is no coercive mechanism enforcing the reform reading on them — they can (and do) continue to defend the orthodox reading. But the institutional and social consequence is real: as the reform reading spreads, the orthodox reading loses authority. This is a genuine instance of constraint-type divergence across seats: rope/coordination from the reform seat, asymmetric extraction/delegitimization from the orthodox seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform interpreters (agenda_setter, institutional power) benefit from maintaining this reading because their institutional authority and scholarly reputation depend on it; they have high mobility (they can shift interpretive positions if evidence warrants). Spiritual practitioners (organized, beneficiary) benefit by gaining access to Vedic texts without endorsing hierarchy; they have mobile exit (they can adopt different readings if spiritually unsatisfying). Egalitarian movements (organized, beneficiary) benefit by acquiring a counter-claim against brahminical authority; they have constrained but growing exit (as the reading becomes mainstream, the cost of adopting it drops). Orthodox varna interpreters (institutional, excluded) bear the cost of delegitimization as the reform reading gains ground; their exit is trapped (their institutional authority is constituted by the orthodox reading, so abandoning it means abandoning their role). Brahminical institutional authority (institutional, excluded) bears the structural cost of declining monopoly over interpretation; their exit is trapped (institutional legitimacy is tied to varna authority). The directionality is genuinely symmetric at the reform_interpreter seat (they choose this reading for reasons of scholarship and consistency, not for extraction); it is asymmetrically beneficiary-sided at the practitioner and movement seats (they choose it for value without bearing costs); it is asymmetrically targeted at the orthodox seats (they bear delegitimization without choosing it). This asymmetry should produce differentiated per-seat classifications: from the reform seat it is coordination; from the practitioner seat it is subsidized access to authority; from the orthodox seat it is extraction/delegitimization. The measurement series show rising extractiveness relative to orthodox authority (as the reading wins ground, orthodox interpreters experience rising d-value toward full target), but the raw extractiveness to practitioners remains zero.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how can practitioners draw on Vedic authority while rejecting caste?) remains live and unresolved — the reading is the answer reformers propose, but the founding problem persists because caste-based hierarchy continues in practice (despite the textual reinterpretation, institutional and social caste structures persist). The reading does not eliminate caste; it supplies a counter-narrative within the texts themselves. From the engine's perspective: the foundational mandate (spiritual liberation compatible with egalitarianism) is still contested — the orthodox reading disputes that the Vedas support this mandate at all. So the constraint is not mandatrophy-resolved; the founding problem is live and the answer is contested, exactly as 'live + contested' should describe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorial_intent_ambiguity,
    'What was the actual soteriological vs. prescriptive intent of the Vedic composers? Did they intend the cosmology as literal social law, metaphorical spiritual teaching, or both?',
    'Historical-textual analysis comparing the oldest Vedic layers with later dharmaśāstra texts; cross-cultural comparison of cosmological vs. legal discourse in ancient religious texts; analysis of compositional patterns and addressee assumptions.',
    'If the oldest Vedic composers intended primarily soteriological teaching with no social prescription, this reading is ''faithful'' to original intent and the orthodox varna reading is a later imposition. If mixed intent is evident, the boundary between readings becomes more ambiguous. If prescriptive social intent is evident in the oldest layers, the reformist reading involves some degree of reinterpretation rather than recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_intent_ambiguity, empirical, 'Whether Vedic composers intended primarily spiritual or prescriptive social content.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the reformist_spiritual_reading logically foreclose the orthodox_varna_reading within a single interpretive framework, or do they coexist as rival readings that parties hold simultaneously?',
    'Examine whether a single reader can hold both readings coherently (e.g., by distinguishing levels of meaning, historical development, or different Vedas), or whether the readings genuinely contradict at the framework level.',
    'If they coexist, this is a case of ''coexists_with'' and the kernel permits multiple readings. If the reformist reading''s core premise (Vedas contain no prescriptive social content) directly contradicts the orthodox premise (Vedas mandate varna order), then ''forecloses'' applies and only one framework can hold both readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the reformist and orthodox readings can coexist in one interpretive framework or whether they are logically exclusive.').

omega_variable(
    institutional_closure_ambiguity,
    'How much of the measured suppression (0.08) is structural resistance from orthodox institutional authorities, and how much is internalized doubt among reform interpreters about whether their reading is ''authorized'' or ''revisionist''?',
    'Tracking post-institutional shifts: if suppression persists after orthodox institutional authority declines (e.g., in diaspora or secular contexts), suppression is partly internalized. If suppression declines with institutional authority, it is primarily structural.',
    'If mostly structural, the constraint is genuinely low-suppression and the reading is sustainable once institutional barriers erode. If partly internalized, practitioners may carry doubt or delegitimization even in contexts where orthodox authority is absent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_closure_ambiguity, empirical, 'Whether suppression against this reading is structural (external institutional resistance) or internalized (self-doubt about legitimacy of reinterpretation).').

omega_variable(
    colonial_imposition_ambiguity,
    'To what extent did the reformist_spiritual_reading emerge as a genuinely indigenous reinterpretation vs. as a colonial-influenced rationalization designed to make Vedic texts palatable to Western audiences and colonial administrators?',
    'Genealogical analysis of the reading''s emergence: when did reform interpreters adopt this framing? Did it emerge in response to colonial critique, or did indigenous interpreters advance it independently? Compare the timing and motivations of reformist readings across colonial and non-colonial contexts.',
    'If primarily colonial imposition, the reading''s legitimacy is partly delegitimized (it appears as an ''inauthentic'' Western compromise). If genuinely indigenous and independent of colonial pressure, the reading''s authority as an authentic voice within Hindu tradition is strengthened. Either way, this reading remains distinct from the colonial_orientalist_reading (which treats texts as administrative ''law code''), but the pedigree question affects how the reading is perceived by practitioners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_imposition_ambiguity, empirical, 'Whether the reformist reading is a genuine indigenous reinterpretation or a colonial-influenced rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1800, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1800, 0.04).
narrative_ontology:measurement_basis(vedi_tr_t1800, projected).
narrative_ontology:measurement(vedi_tr_t1880, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement_basis(vedi_tr_t1880, observed).
narrative_ontology:measurement(vedi_tr_t1920, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1920, 0.09).
narrative_ontology:measurement_basis(vedi_tr_t1920, observed).
narrative_ontology:measurement(vedi_tr_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1960, 0.11).
narrative_ontology:measurement_basis(vedi_tr_t1960, observed).
narrative_ontology:measurement(vedi_tr_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement_basis(vedi_tr_t1990, observed).
narrative_ontology:measurement(vedi_tr_t2026, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2026, 0.12).
narrative_ontology:measurement_basis(vedi_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1800, 0.02).
narrative_ontology:measurement_basis(vedi_be_t1800, projected).
narrative_ontology:measurement(vedi_be_t1880, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1880, 0.06).
narrative_ontology:measurement_basis(vedi_be_t1880, observed).
narrative_ontology:measurement(vedi_be_t1920, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1920, 0.09).
narrative_ontology:measurement_basis(vedi_be_t1920, observed).
narrative_ontology:measurement(vedi_be_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement_basis(vedi_be_t1960, observed).
narrative_ontology:measurement(vedi_be_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1990, 0.14).
narrative_ontology:measurement_basis(vedi_be_t1990, observed).
narrative_ontology:measurement(vedi_be_t2026, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2026, 0.15).
narrative_ontology:measurement_basis(vedi_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement_basis(vedi_su_t1800, projected).
narrative_ontology:measurement(vedi_su_t1880, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1880, 0.12).
narrative_ontology:measurement_basis(vedi_su_t1880, observed).
narrative_ontology:measurement(vedi_su_t1920, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1920, 0.1).
narrative_ontology:measurement_basis(vedi_su_t1920, observed).
narrative_ontology:measurement(vedi_su_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1960, 0.09).
narrative_ontology:measurement_basis(vedi_su_t1960, observed).
narrative_ontology:measurement(vedi_su_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement_basis(vedi_su_t1990, observed).
narrative_ontology:measurement(vedi_su_t2026, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2026, 0.08).
narrative_ontology:measurement_basis(vedi_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.05).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vedic_corpus_social_prescription kernel. The reformist_spiritual_reading decomposes from the contested kernel because the three readings have significantly different ε values and victim/beneficiary structures: the orthodox reading is high-extractiveness (0.68+) and mandates hierarchy; the colonial reading is high-extractiveness (0.72+) and serves administrative governance; the reformist reading is low-extractiveness (0.15) and coordinates spiritual practice without hierarchy. These are not one constraint viewed from three angles — they are three distinct constraints with different structural properties, linked by their shared kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
