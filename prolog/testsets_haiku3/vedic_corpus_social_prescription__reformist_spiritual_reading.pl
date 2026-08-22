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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Vedic Spiritual Unity Coordination (Reformist Reading)
 *   domain: religious/hermeneutic/metaphysical
 *
 * SUMMARY:
 *   This constraint story instantiates the reformist spiritual reading of the
 *   Vedic corpus: an interpretation that frames the Vedas as describing
 *   unified metaphysical reality and states of consciousness without
 *   prescriptive social content, thereby dissolving the apparent Vedic
 *   foundation for caste hierarchy. This is ONE reading of a contested kernel
 *   (the Vedic corpus itself and its social meaning). The sibling
 *   readings—orthodox_varna_reading and
 *   colonial_orientalist_reading—interpret the same texts but derive
 *   different social conclusions. This story focuses exclusively on the
 *   reformist reading as a coherent constraint: what coordination it
 *   accomplishes, what extraction persists, what hermeneutic work it
 *   requires. The claim (rope) and metrics (low extractiveness, minimal
 *   suppression) are authored independently; they describe the reformist
 *   reading's structural character on its own terms.
 *
 * KEY AGENTS:
 *   - Vedantic scholars: institutional actors who set the interpretive agenda by teaching, publishing, and institutionalizing the reformist reading
 *   - Spiritual practitioners: benefit from a coherent metaphysical framework without social-hierarchy implications
 *   - Reform movements: deploy the reading to argue for social change without repudiating Vedic authority
 *   - Orthodox interpreters: excluded from reformist spaces; their hermeneutic claims are contested
 *   - Low-caste communities: benefit from delegitimization of hierarchy-justifying Vedic claims, though material position unchanged
 *   - Colonial administrators: historically attempted to codify caste from Dharmashastra; the reformist reading undermines their project
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.18).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Spiritual Unity Coordination (Reformist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious/hermeneutic/metaphysical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '375cf23c-8cfd-462f-b144-07ae1be21928').
narrative_ontology:cs_kernel_codification('375cf23c-8cfd-462f-b144-07ae1be21928', fixed_text).
narrative_ontology:cs_authority_grounding('375cf23c-8cfd-462f-b144-07ae1be21928', lineage).
narrative_ontology:cs_interpretation_layer_present('375cf23c-8cfd-462f-b144-07ae1be21928').
narrative_ontology:cs_reading_relation('375cf23c-8cfd-462f-b144-07ae1be21928', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('375cf23c-8cfd-462f-b144-07ae1be21928', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('375cf23c-8cfd-462f-b144-07ae1be21928', foundational, vedic_texts_metaphysical_not_prescriptive).
narrative_ontology:cs_axiom_status(vedic_texts_metaphysical_not_prescriptive, holdable).
narrative_ontology:cs_axiom_grounding('375cf23c-8cfd-462f-b144-07ae1be21928', vedic_texts_metaphysical_not_prescriptive, empirically_contingent).
narrative_ontology:cs_axiom('375cf23c-8cfd-462f-b144-07ae1be21928', foundational, varna_hierarchy_later_corruption).
narrative_ontology:cs_axiom_status(varna_hierarchy_later_corruption, holdable).
narrative_ontology:cs_axiom_grounding('375cf23c-8cfd-462f-b144-07ae1be21928', varna_hierarchy_later_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('375cf23c-8cfd-462f-b144-07ae1be21928', vedic_unity_spiritual_framework).
narrative_ontology:cs_drift_state('375cf23c-8cfd-462f-b144-07ae1be21928', contemporary_academic_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('375cf23c-8cfd-462f-b144-07ae1be21928', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, vedantic_scholars).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, low_caste_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage with Vedic texts as sources of spiritual methodology and metaphysical insight. Under the reformist reading, they find a unified cosmology describing states of consciousness and the nature of ultimate reality without prescriptive social hierarchy. They benefit from a coherent interpretive framework that does not bind spiritual liberation to social status.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Interpret, teach, and transmit the Vedic corpus under this reading. They set the scholarly agenda by producing commentaries, translations, and educational frameworks that present the texts as describing spiritual states and metaphorical cosmology rather than social prescription. They maintain this interpretive lineage through publication, teaching, and institutional positioning.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, vedantic_scholars, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__reformist_spiritual_reading, vedantic_scholars, agenda_setter).

% Deploy the reformist reading to argue for social change: that Vedic authority does not sanction caste hierarchy, that spiritual equality is the true Vedic doctrine, that social inequalities are later corruptions. They benefit from a legitimate traditional foundation for egalitarian reform without needing to repudiate the texts themselves.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reform_movements, beneficiary,
    organized, generational, constrained, national).

% Hold the orthodox reading that Varna hierarchy is divinely mandated and textually prescribed. The reformist reading displaces their hermeneutic authority by reinterpreting the same texts to deny prescriptive social content. They would dispute the coherence of the reading and the legitimacy of the interpretive moves it makes, but are largely excluded from the spaces where the reformist reading operates (secular academia, reform-movement leadership).
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_interpreters, excluded,
    institutional, generational, constrained, regional).

% Historically attempted to codify Hindu law from the Dharmashastra corpus. The reformist reading contests their strategy by denying that Vedic texts constitute prescriptive law at all, thereby removing the textual foundation for caste-based administrative categories and loosening colonial efforts to freeze customary law into static categories.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_administrators, observer,
    institutional, biographical, analytical, national).

% Have historically been told their low status is Vedically ordained and cosmically justified. The reformist reading offers a counter-narrative: the texts do not prescribe their subordination, and caste hierarchy is a later corruption of original spiritual egalitarianism. They benefit from the delegitimization of hierarchy-justifying Vedic claims, though their material and institutional position remains constrained by the same social order.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, low_caste_communities, beneficiary,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, vedantic_scholars).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__reformist_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive framework for understanding Vedic texts as coherent descriptions of metaphysical reality and spiritual states without prescriptive social content. Solves the coordination problem: 'How can we honor Vedic authority while rejecting caste hierarchy?' by reframing the texts as non-prescriptive.
% TRANSFER_FUNCTION: Transfers hermeneutic authority from orthodox interpreters to reformist scholars and movements; transfers legitimacy from caste-hierarchy defenses to egalitarian reform; shifts meaning-space so that appealing to Vedic authority no longer entails endorsing caste. The constraint moves interpretive frameworks, not material goods.
% ABSENT_VOICES: Orthodox interpreters whose hermeneutic claims are contested and whose institutional spaces are displaced are largely absent from the reformist academic and reform-movement contexts where this reading operates. Indigenous non-Brahminical Vedic traditions with their own interpretations are often excluded from both the orthodox and reformist scholarly circles. Dalit scholars producing counter-readings that do not fit the reformist framing are often unheard.
% DISAPPEARANCE_RATIONALE: The reformist reading is itself a relatively recent historical development (19th century onward). If it disappeared, orthodox Vedic hierarchy-justifications would reassert dominant interpretive authority in scholarly and religious spaces, and the egalitarian appeal to Vedic authority would lose its foundation. Reform movements would need alternative strategies or would retreat. The scholarly landscape would rearrange toward pre-reformist hermeneutics. Some communities would lose a source of dignifying reinterpretation; others would reassert hierarchy-supporting readings. The verdict is contested because different parties (reformers, orthodox communities, Dalit critics) would experience the disappearance differently.
% FOUNDING_PROBLEM: Reconcile Vedic textual authority—foundational to Hindu identity and legitimacy—with the egalitarian values and human rights commitments of contemporary India and global reform movements. The founding problem is a hermeneutic tension: either Vedic texts are authoritative and prescribe hierarchy, or they do not prescribe hierarchy and must be reinterpreted.
% FOUNDING_PROBLEM_CORROBORATION: Vedantic scholars, reform movements, and international scholars of Hinduism attest the founding problem is live: the tension between Vedic authority and egalitarian values generates ongoing scholarly work, reform discourse, and communal contestation. Dalit scholars and caste-critical scholars contest whether the reformist reading adequately resolves the problem or merely displaces it. No source outside the reformist movement attests that the reformist reading has settled the question; the founding problem persists as the reading is contested by sibling readings.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, contested).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The reformist reading operates as a rope—coordination with minimal extraction—because: (1) Extractiveness is low (0.18 endpoint) because the reading solves a genuine coordination problem (reconciling Vedic authority with egalitarianism) without systematically enriching one party at another's structural expense. Reformist scholars and movements benefit by gaining hermeneutic authority and social legitimacy, but they do not extract material resources through the mechanism of the reading itself. (2) Suppression is minimal (0.12) because the reading propagates through voluntary scholarly engagement, educational choice, and ideological alignment rather than coercion. Institutional positioning matters, but is not enforcement machinery in the snare sense. (3) Theater is very low (0.08) because scholarly interpretation is genuinely the function; the interpretive work is not a cover for extraction. The measurement series show modest rise over 200 time-units: as the reading institutionalizes and becomes more dominant in academic and reform spaces, the suppression requirement rises slightly (orthodox voices must be marginalized more actively), theater rises as institutional legitimacy performs alongside intellectual work, and extractiveness creeps upward as hermeneutic authority concentrates. But the terminal values remain consistent with a rope: no victim set, coordination cost absorbed as scholarly labor, no systematic asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist scholars' and practitioners' seats, this is genuine coordination: a coherent reading that honors textual authority while enabling egalitarian values and practice. From orthodox interpreters' seats, the reading is hermeneutically illegitimate—a distortion that evacuates Vedic prescriptive content to serve modern political agendas. From Dalit scholars' seats, the reading may be experienced as a reformist displacement that still centers Vedic authority and Brahminical interpretive spaces, leaving material hierarchies and Dalit exclusion from hermeneutic authority untouched. From low-caste communities' seats (powerless, trapped), the reading offers some delegitimization of hierarchy but does not materially restructure social position. The engine computes per-seat classifications from the structural data: reformist and orthodox seats should diverge substantially (different beneficiary/victim structures, different exit options); Dalit and low-caste seats should show lower benefit perception despite formal inclusion in the beneficiary set.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional mapping: Vedantic scholars (institutional power, mobile exit, beneficiary role) sit near d=0.2 (beneficiary/favorable directionality). Spiritual practitioners (moderate power, mobile exit) sit near d=0.3 (moderate, net beneficiary). Reform movements (organized, generational, constrained exit) sit near d=0.35 (moderate-target side, because their exit options are constrained by ideological commitment to the reading they depend on). Orthodox interpreters (institutional, displaced hermeneutic authority) sit near d=0.65 (target side: they bear the cost of hermeneutic displacement without choosing it). Low-caste communities (powerless, trapped, formal beneficiary but material position unchanged) sit near d=0.55 (symmetric-to-target: they benefit from delegitimization rhetoric but remain structurally excluded and materially unchanged). No directionality override needed; the structural data derives appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy—the divergence between founding problem and constraint persistence—is addressed via the (founding_problem_status x disappearance_verdict) mismatch: founding_problem_status=live (the hermeneutic tension is not resolved, only reframed), disappearance_verdict=contested (different parties experience the reading's role differently). This mismatch prevents false-mandatrophy diagnosis—the reading persists because the founding problem persists, not because the problem is gone and only institutional inertia remains. The rope classification is stable: the reading solves coordination (egalitarian spiritual practice compatible with Vedic authority) without producing the victim/extraction structure that would flip it to snare or tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_legitimacy_boundary,
    'Are the interpretive moves the reformist reading makes (reframing Varna as symbolic, declaring prescriptive passages as later corruption) hermeneutically legitimate derivations from the texts, or are they modern impositions that distort the texts'' actual social content?',
    'Comparative philological analysis: does the Sanskrit grammar and context support the reinterpretation? Does scholarly consensus across traditions converge or diverge? Historical analysis: when and why did the reformist reading emerge (19th-century response to colonial and social-reform pressure, or rediscovery of suppressed tradition)?',
    'If hermeneutically sound, the reading is a genuine coordination solution and remains a rope. If a modern imposition, the reading itself becomes extractive (using Vedic authority to legitimize reform while distorting the texts), reclassifying toward snare for scholarly seats and tangled_rope for reform movements (who depend on a delegitimized reading to justify their position).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_legitimacy_boundary, empirical, 'Whether the reformist reading''s interpretive moves are textually grounded or modern impositions.').

omega_variable(
    materiality_gap_between_hermeneutic_and_social_change,
    'Does the reformist reading produce actual social change (reduced caste discrimination, material redistribution), or does it function primarily as a legitimacy framework that leaves material hierarchies intact?',
    'Empirical assessment: compare social indicators (educational access, economic mobility, intermarriage, violence) in communities where the reformist reading is dominant vs. where traditional or orthodox readings persist. Ethnographic study of how the reading translates (or fails to translate) into practice change.',
    'If the reading produces material change, it is a genuine rope solving both hermeneutic and social coordination. If primarily a legitimacy framework with no material effect, the reading extracts benefits for scholars and reform movements (hermeneutic authority, institutional positioning) without delivering goods to low-caste communities, reclassifying from rope toward tangled_rope (asymmetric: beneficiaries are scholars/movements; victims are those told the problem is solved while material position unchanged).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(materiality_gap_between_hermeneutic_and_social_change, empirical, 'Whether the reformist reading produces material social change or remains a legitimacy framework.').

omega_variable(
    dalit_interpretive_exclusion,
    'Does the reformist reading''s emphasis on Vedantic unity and metaphysical spirituality suppress or exclude Dalit and non-Brahminical Vedic interpretive traditions that offer alternative readings (e.g., Phule''s rejection of Vedic authority entirely, or Ambedkar''s critical hermeneutics)?',
    'Institutional analysis: are Dalit scholars'' interpretations cited and taught in reformist academic and movement spaces, or marginalized as ''too critical'' or ''not scholarly''? Textual analysis: do reformist commentaries engage with Dalit hermeneutics or treat the field as containing only orthodox and reformist positions?',
    'If exclusionary, the reading functions as an identity-coordination mechanism (Brahminical and upper-caste reformist authority) with a victim set (Dalit and non-Brahminical interpreters), reclassifying from rope toward tangled_rope or snare (beneficiaries: Brahminical reformists; victims: Dalit scholars whose interpretations are marginal). If inclusive, the reading retains rope character by coordinating multiple interpretive traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dalit_interpretive_exclusion, empirical, 'Whether the reformist reading suppresses Dalit and non-Brahminical Vedic interpretations.').

omega_variable(
    sibling_reading_foreclosure_asymmetry,
    'Does the reformist reading''s success in academic and reform-movement spaces functionally foreclose the orthodox reading despite logical claims to coexistence, through mechanisms like exclusion from curricula, funding, publication, and institutional positions?',
    'Institutional audit: track Orthodox scholars'' representation in universities, publication venues, and reform leadership. Track citation patterns: do reformist texts cite orthodox interpretations seriously, or dismiss them? Track resource flows: which reading receives funding, graduate positions, book contracts?',
    'If the foreclosure is functional rather than logical, the reading operates as more extractive than its own theoretical framework admits: it generates victim costs (for orthodox interpreters, institutional displacement) beyond the minimal suppression required for coordination. The classification would move from rope toward tangled_rope (active enforcement required to maintain reformist dominance) or snare (if the victim set is substantial and suppression is coercive rather than mere institutional positioning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_asymmetry, conceptual, 'Whether reformist institutional dominance functionally forecloses the orthodox reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement_basis(vedi_tr_t40, observed).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 80, 0.06).
narrative_ontology:measurement_basis(vedi_tr_t80, observed).
narrative_ontology:measurement(vedi_tr_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 120, 0.07).
narrative_ontology:measurement_basis(vedi_tr_t120, observed).
narrative_ontology:measurement(vedi_tr_t160, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 160, 0.08).
narrative_ontology:measurement_basis(vedi_tr_t160, observed).
narrative_ontology:measurement(vedi_tr_t200, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement_basis(vedi_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement_basis(vedi_be_t40, observed).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 80, 0.16).
narrative_ontology:measurement_basis(vedi_be_t80, observed).
narrative_ontology:measurement(vedi_be_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 120, 0.17).
narrative_ontology:measurement_basis(vedi_be_t120, observed).
narrative_ontology:measurement(vedi_be_t160, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 160, 0.18).
narrative_ontology:measurement_basis(vedi_be_t160, observed).
narrative_ontology:measurement(vedi_be_t200, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 200, 0.18).
narrative_ontology:measurement_basis(vedi_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 40, 0.07).
narrative_ontology:measurement_basis(vedi_su_t40, observed).
narrative_ontology:measurement(vedi_su_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 80, 0.1).
narrative_ontology:measurement_basis(vedi_su_t80, observed).
narrative_ontology:measurement(vedi_su_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 120, 0.11).
narrative_ontology:measurement_basis(vedi_su_t120, observed).
narrative_ontology:measurement(vedi_su_t160, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 160, 0.12).
narrative_ontology:measurement_basis(vedi_su_t160, observed).
narrative_ontology:measurement(vedi_su_t200, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 200, 0.12).
narrative_ontology:measurement_basis(vedi_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.1).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vedic_corpus_social_prescription kernel. The three readings share the same textual kernel but derive different social conclusions. Constraint family: vedic_corpus_social_prescription__orthodox_varna_reading (Mountain to Snare range, high extraction, victim set), vedic_corpus_social_prescription__colonial_orientalist_reading (Tangled Rope, administrative extraction), vedic_corpus_social_prescription__reformist_spiritual_reading (Rope, minimal extraction, no victim set). The readings are linked through network.affects_constraints: reformist reading influences both orthodox and colonial readings by displacing their hermeneutic authority in academic and reform-movement contexts. The epsilon values differ substantially across readings: orthodox reading high (prescriptive hierarchy entrenches extraction), colonial reading moderate-high (administrative categories extract rents), reformist reading low (spiritual coordination with minimal extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
