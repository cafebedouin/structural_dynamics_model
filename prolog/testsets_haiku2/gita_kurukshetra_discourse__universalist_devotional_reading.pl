% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita Universalist Devotional Reading: Caste-Transcendent Dharma as Surrender
 *   domain: religious/textual/ethical
 *
 * SUMMARY:
 *   The Bhagavad Gita is a contested kernel: a sacred Sanskrit text at the
 *   heart of Hindu tradition, claimed by multiple interpretive lineages. This
 *   constraint instantiates the universalist devotional reading, which
 *   teaches that Krishna opens a path of devotional surrender (bhakti) to all
 *   seekers regardless of birth status, and that dharma means surrender to
 *   divine will rather than adherence to caste-prescribed social role. This
 *   reading democratizes access to salvation and fundamentally undermines the
 *   authority of caste-based gatekeeping. The reading is one of three live
 *   interpretive positions within the Hindu tradition: it coexists with the
 *   orthodox literal reading (which reads the text as mandating caste
 *   hierarchy and righteous violence) and the gandhian allegorical reading
 *   (which reads the battlefield as inner struggle). This constraint
 *   describes the coordination problem the universalist reading solves and
 *   the authority structure it instantiates.
 *
 * KEY AGENTS:
 *   - Universalist devotional interpreters (organized authority): lineage scholars, modern theologians, monastic institutions and ashrams that transmit this reading
 *   - Low-caste, marginalized, and women practitioners (beneficiaries): those historically excluded from Brahminical authority who find this reading opens devotional access
 *   - Orthodox Brahminical authority (payer): institutional keepers of the orthodox reading whose textual and spiritual monopoly is undermined
 *   - Modern secular interpreters (observers): academic scholars who engage the reading as philosophically coherent and ethically sound
 *   - Gandhian allegorical interpreters (observers/coexisting): alternative reading that shares anti-violence and democratizing impulses but different textual focus
 *   - Orthodox literal interpreters (excluded): those committed to the orthodox reading are structurally foreclosed by this reading's premises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.18).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.12).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita Universalist Devotional Reading: Caste-Transcendent Dharma as Surrender").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious/textual/ethical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '9c2bcfac-5820-463c-8224-7803f8ba551b').
narrative_ontology:cs_kernel_codification('9c2bcfac-5820-463c-8224-7803f8ba551b', fixed_text).
narrative_ontology:cs_authority_grounding('9c2bcfac-5820-463c-8224-7803f8ba551b', lineage).
narrative_ontology:cs_interpretation_layer_present('9c2bcfac-5820-463c-8224-7803f8ba551b').
narrative_ontology:cs_reading_relation('9c2bcfac-5820-463c-8224-7803f8ba551b', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('9c2bcfac-5820-463c-8224-7803f8ba551b', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('9c2bcfac-5820-463c-8224-7803f8ba551b', foundational, bhakti_transcends_caste).
narrative_ontology:cs_axiom_status(bhakti_transcends_caste, holdable).
narrative_ontology:cs_axiom_grounding('9c2bcfac-5820-463c-8224-7803f8ba551b', bhakti_transcends_caste, empirically_contingent).
narrative_ontology:cs_axiom('9c2bcfac-5820-463c-8224-7803f8ba551b', foundational, dharma_as_surrender_not_role).
narrative_ontology:cs_axiom_status(dharma_as_surrender_not_role, holdable).
narrative_ontology:cs_axiom_grounding('9c2bcfac-5820-463c-8224-7803f8ba551b', dharma_as_surrender_not_role, deontological).
narrative_ontology:cs_axiom('9c2bcfac-5820-463c-8224-7803f8ba551b', secondary, universal_salvation_access).
narrative_ontology:cs_axiom_status(universal_salvation_access, holdable).
narrative_ontology:cs_axiom_grounding('9c2bcfac-5820-463c-8224-7803f8ba551b', universal_salvation_access, deontological).
narrative_ontology:cs_reference_frame('9c2bcfac-5820-463c-8224-7803f8ba551b', krishna_egalitarian_devotion).
narrative_ontology:cs_drift_state('9c2bcfac-5820-463c-8224-7803f8ba551b', contemporary_post_colonial_globalized_hinduism, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9c2bcfac-5820-463c-8224-7803f8ba551b', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, low_caste_practitioners).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, women_practitioners).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_brahminical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A distributed lineage of religious scholars, modern theologians (Ramakrishna Paramahamsa, Swami Vivekananda, Aurobindo Ghosh, contemporary bhakti leaders), and institutional actors (Ramakrishna Mission, ISKCON, independent ashrams, university departments of religious studies) who maintain and transmit the universalist reading. They interpret the Gita as teaching that Krishna offers devotional salvation to all, regardless of birth or social status. They exercise authority through lineage succession, textual scholarship, spiritual teaching, and institutional platforms. This reading has become the dominant interpretation in diaspora Hindu communities, academic contexts, and modern reform institutions in South Asia.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universalist_devotional_interpreters, agenda_setter,
    organized, generational, mobile, global).

% Historically excluded from Brahminical ritual authority and Vedic recitation, these practitioners (Dalit communities, scheduled castes, so-called 'untouchables') find in the universalist reading explicit textual warrant for direct devotional access without caste mediation. The reading legitimates their spiritual seeking and delegitimizes institutional exclusion. Their exit options are constrained by economic dependency, caste discrimination, and cultural isolation; the reading's accessibility represents genuine liberation from institutional barriers. They do not control the interpretation but directly benefit from it.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, low_caste_practitioners, beneficiary,
    powerless, biographical, constrained, regional).

% Restricted from Vedic recitation, priesthood, and certain ritual roles in orthodox systems, women find in the universalist reading (particularly Krishna's statement that all reach him regardless of gender) warrant for full spiritual authority and practice. The reading enables female gurus, female renunciates, and women's leadership in devotional communities. Their exit is similarly constrained by cultural norms and economic dependency; the reading opens spiritual paths previously closed. They are substantially benefited by the constraint.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, women_practitioners, beneficiary,
    powerless, biographical, constrained, regional).

% Traditional caste-conservative interpreters and ritual authorities (orthodox pandits, certain Vedantic lineages, caste-based priesthoods) whose institutional legitimacy rests partly on the claim that the Gita mandates caste-based dharma and ritual hierarchy. The universalist reading directly undermines this textual foundation, withdraws the claim that the text supports caste gatekeeping, and redistributes interpretive authority away from caste-based institutions. They bear substantial cost: loss of exclusive textual authority, delegitimation of caste gatekeeping, institutional erosion as practitioners access the universalist reading. Their exit is identity-locked because their institutional identity is constitutively tied to the orthodox reading's truth; renouncing it would dissolve their authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_brahminical_authority, payer,
    institutional, civilizational, identity_locked, regional).

% Academic researchers, comparative religionists, and secular philosophers who engage the Gita as philosophical and historical text. They tend to find the universalist reading more coherent with the text's actual language, more philosophically sound, and more aligned with modern ethical frameworks (universal dignity, rejection of birth-based hierarchy). They do not collect rents from the reading but analyze its validity and influence. Their endorsement lends secular legitimacy to the universalist reading and complicates orthodox authority's claim to exclusive possession. Their analytical position permits them to remain relatively neutral while still engaging the interpretive contest.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, modern_secular_scholars, observer,
    moderate, generational, mobile, global).

% Interpret the Kurukshetra battle as metaphor for the internal spiritual struggle; they read Krishna as teaching non-violence and inner duty rather than physical combat. This reading coexists with the universalist reading (both reject the orthodox reading's claim that the text mandates violence, and both democratize spiritual access), but occupies a distinct interpretive position focused on non-violence as the text's central message. The gandhian reading emphasizes that the battle's violence is not the point; the universalist reading emphasizes that devotion transcends questions of violence and caste. They are allies against the orthodox reading but distinct in focus.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_allegorical_interpreters, observer,
    moderate, generational, mobile, regional).

% Are systematically excluded from this reading's interpretive framework because their core claim (caste is mandated by the Gita's teaching of dharma as social role) is logically incompatible with this reading's foundation (dharma is devotional surrender that dissolves caste). They would argue the universalist reading falsifies the text, erases Arjuna's kshatriya specificity, and mistakes spirituality for moral philosophy. Their exclusion is structural: the reading itself forecloses their position. This is not accidental; it is the reading's defining move against the orthodox interpretation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_interpreters, excluded,
    institutional, civilizational, identity_locked, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__universalist_devotional_reading, universalist_devotional_interpreters).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__universalist_devotional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how salvific spiritual knowledge and devotional practice are accessed: the universalist reading establishes that access is open to all seekers regardless of birth, caste, or social role, mediated by devotion and inner surrender rather than by institutional gatekeeping or ritual status. This contrasts with the orthodox coordination solution (caste-hierarchical gatekeeping) by removing the gate.
% TRANSFER_FUNCTION: Transfers interpretive authority from caste-based institutional authorities (Brahminical priests and conservative pandits) to a democratized lineage of universalist interpreters and to devotees themselves. It also transfers spiritual status from those born into high castes to those who practice devotion regardless of birth. The transfer is not primarily economic (the reading does not accumulate wealth) but is instead structural — redistribution of authoritative voice, spiritual legitimacy, and access to the text's sacred content.
% ABSENT_VOICES: Orthodox literal interpreters are structurally excluded rather than merely absent — their core premise contradicts this reading's foundation. Historically absent from textual interpretation authority are the low-caste devotees and practitioners who lived bhakti outside the Sanskrit textual tradition; the universalist reading claims to recover and honor their voices, though non-literate folk practitioners and oral traditions remain under-represented in formal textual scholarship. Also under-represented: practitioners who resist both the orthodox and universalist readings, seeking alternative sources (other traditions, secular frameworks). Women practitioners' voices have been historically silent in formal interpretation; the universalist reading includes them, but decision-making authority remains unevenly distributed.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and only orthodox and gandhian readings remained available, the spiritual and institutional landscape would substantially reorganize. Modern bhakti movements, contemporary Brahmo Samaj and Hindu reform institutions, diaspora Hindu communities, and much of contemporary academic Hindu studies rest on the foundation that this reading provides. Low-caste and women practitioners would lose explicit textual warrant for their devotional seeking; they would face renewed institutional resistance and gatekeeping. The decentralization of interpretive authority that this reading enabled would contract back toward institutional control and caste-based limitations. Many contemporary spiritual communities would fragment or migrate to other textual sources. The institutional landscape of global Hinduism would shift substantially.
% FOUNDING_PROBLEM: In pre-modern and traditional Hindu society, caste hierarchy and Brahminical ritual monopoly created structural spiritual exclusion: salvation knowledge was claimed to be accessible only through caste-prescribed roles, brahminical initiation, and orthodox institutional mediation. Low-caste and non-caste-marked individuals seeking spiritual liberation faced institutional rejection. Women were excluded from Vedic recitation and priesthood. The universalist reading addresses this by offering textual warrant that Krishna opens a devotional path that dissolves caste barriers and is accessible to all.
% FOUNDING_PROBLEM_CORROBORATION: Universalist interpreters attest that the founding problem remains live: orthodox institutions continue gatekeeping in certain regions, low-caste practitioners remain excluded from some temples and ritual authorities, and women still face restrictions on Vedic recitation and priesthood. Low-caste and women practitioners themselves corroborate this ongoing exclusion. However, scholars external to the benefiting parties — historians of religion, sociologists of Hinduism, and comparative religionists — provide corroboration: they document the historical reality of caste-based exclusion and the universalist reading's emergence as a response. But orthodox authorities contest the founding problem itself, arguing that caste and spirituality are separable categories and that the universalist reading misreads the text's actual claims. This mixed corroboration reflects the reading's contested status: external scholars support the founding problem diagnosis; orthodox authorities deny it.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading is claimed as rope because it solves a genuine coordination problem (how to access salvific knowledge across caste barriers) with minimal suppression. The low extractiveness (0.18) reflects that the universalist reading does not concentrate gains in a narrow beneficiary set — it aims toward universal access. Suppression is low (0.12) because the reading does not rely primarily on coercion to persist; it persists through textual interpretation, spiritual community practice, and the appeal of its egalitarian logic. Theater ratio is low (0.08) because the reading's functional core (devotional surrender as the salvific path) has remained stable across centuries of practice; there is minimal performative maintenance distinct from actual spiritual practice. The measurement trajectory shows suppression declining over time (0.35 to 0.12) as the reading gained institutional legitimacy through modern reform movements and as orthodox gatekeeping weakened in post-colonial contexts. Extractiveness plateaus (0.08 to 0.18) because while the reading's reach expands, it does not accumulate extraction mechanisms — it remains focused on spiritual access rather than rent-collection. Accessibility collapse is low (0.15) because alternatives remain: practitioners can still choose orthodox readings, other traditions, secular frameworks. Resistance is high (0.72) because orthodox authorities and certain conservative institutions actively contest this reading and attempt to suppress its transmission, particularly regarding caste and women's roles.
 *
 * PERSPECTIVAL GAP:
 *   The universalist interpreters (agenda-setters) and the low-caste practitioners (beneficiaries) both experience this reading as liberating and as revealing the text's true message, yet their seats are structurally different. The interpreters maintain authority through lineage and scholarship; the practitioners gain access through the interpreters' work but are not themselves authorities over the text. The orthodox authorities experience this reading as a corruption of the text's real meaning and a threat to institutional legitimacy; from their seat, the reading is extractive (it removes their authority) and destabilizing. The modern secular observers experience the reading as philosophically coherent and empirically accurate to the text's language, yet they do not collect from it spiritually — their seat is analytical. The gandhian interpreters coexist with this reading but occupy a distinct interpretive position focused on non-violence as the central salvific claim rather than devotional surrender. The engine computes per-seat types from these structural differences: the beneficiary seats should experience lower effective extraction (or even subsidy) from the reading, while the orthodox authority seat should experience higher extraction (loss of gatekeeping power). The observer seats should show minimal extraction either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Low-caste and marginalized practitioners are beneficiaries with powerless power and constrained exit; they gain spiritual access through the reading and lack alternatives (their exit would require leaving spiritual seeking or accepting caste gatekeeping). Women practitioners are similarly positioned. Their directionality is toward beneficiary (low d, near 0.0) because the reading's coordination opens access they previously lacked and they have limited exit. Orthodox Brahminical authority is the victim/payer: they lose interpretive monopoly and institutional legitimacy that rested on caste gatekeeping. Their power is institutional, their exit options are identity-locked (their authority is constitutively tied to the orthodox reading), so their directionality is toward target (high d, near 1.0). Universalist interpreters are beneficiaries (they gain authority and institutional presence) with organized power and mobile exit options (they could adopt other readings or exit the interpretation business), placing their directionality at the moderate beneficiary end (low d, ~0.2). Secular observers have analytical power and analytical exit, placing their directionality at symmetric (d ~0.5). The directed values feed the engine's χ computation: beneficiary seats receive damped or inverted (negative) effective extraction; target seats receive amplified effective extraction. Scope is regional to global depending on stakeholder reach; universalist interpreters have global reach, low-caste practitioners have more localized regional reach in South Asia, though the reading has spread globally through diaspora and academic institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not present a mandatrophy case in the classic sense (where an arrangement persists after its founding problem has been solved). The founding_problem_status is contested precisely because the reading's own success is at issue: has the universalist reading actually achieved spiritual democratization and caste dissolution, or does institutional caste gatekeeping persist despite the reading's doctrinal claims? The reading's mandatrophy potential lies in a different direction: if institutional contexts genuinely adopted caste-blind spiritual authority and women gained full liturgical authority as a result of this reading, the founding problem would be dead and the reading's functional core would be achieved. At that point, the constraint would risk becoming piton-like — maintained for historical or theological reasons even after the problem it solved is resolved. Current measurement data suggests this is not yet the case: suppression remains necessary (high resistance from orthodox authorities, ongoing institutional gatekeeping in many contexts), indicating the founding problem remains live. However, in diaspora contexts and contemporary reform movements, the reading has achieved substantial success in decoupling spiritual authority from caste, suggesting regional variation in mandatrophy status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dharma_meaning_contest,
    'Does dharma refer primarily to caste-prescribed social role (orthodox reading) or to devotional surrender and inner duty to the divine (universalist reading)?',
    'Philological analysis of the term dharma across the Gita''s 119 occurrences in context, cross-referenced with other Hindu texts. Comparative analysis of how different interpretive lineages historically parsed the term. Examination of whether Krishna''s explicit statements about devotion override statements about duty.',
    'If dharma is primarily social-role-based, the orthodox reading''s core claim is stronger and the universalist reading is a reinterpretation rather than a recovery of original meaning. If dharma can coherently refer to both social duty AND devotional surrender with the latter as the higher path, the universalist reading''s hierarchy stands. This determines whether the reading is a genuine coordination discovery or a reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharma_meaning_contest, empirical, 'Philological contest over dharma''s primary referent in the Gita.').

omega_variable(
    caste_spirituality_ambiguity,
    'In the universalist reading, is caste treated as spiritually irrelevant (a mere social fact) or as actively undermined by the text''s spiritual teaching?',
    'Textual close reading of Krishna''s statements about birth, action, and spiritual status. Examination of whether the text is silent on caste (irrelevance) or actively subversive (negation). Comparison with how later universalist interpreters (Vivekananda, Ramakrishna) explicitly argued for caste dissolution versus those who treated caste and devotion as orthogonal.',
    'If caste is merely irrelevant, the reading is compatible with an external social hierarchy that persists alongside spiritual equality. If caste is actively negated spiritually, the reading makes stronger demands for institutional reorganization. This determines whether the reading''s suppression of caste is textually mandated or culturally imposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_spirituality_ambiguity, empirical, 'Whether the universalist reading treats caste as orthogonal or as actively negated.').

omega_variable(
    reading_authority_source_ambiguity,
    'Does this reading''s authority derive from faithful recovery of the text''s original meaning, or from a creative reinterpretation that privileges certain passages over others and reads against the text''s apparent surface meaning?',
    'Genealogical tracing of when the universalist reading emerged (primarily 18th–19th century in reform movements, not earlier textual traditions). Examination of whether the reading requires reading certain passages non-literally (e.g., treating Arjuna''s kshatriya duty as irrelevant to the text''s ultimate message). Comparison with how practitioners in medieval bhakti traditions actually engaged the text versus how modern universalist interpreters claim they did.',
    'If the reading is a faithful recovery of original meaning, its legitimacy is textually grounded. If it is a creative reinterpretation, its legitimacy rests on whether that reinterpretation is philosophically sound and spiritually fruitful. This determines whether the reading can claim to restore suppressed meaning or is advancing a new interpretation that may differ from what the text actually says.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authority_source_ambiguity, empirical, 'Whether the universalist reading is textual recovery or creative reinterpretation.').

omega_variable(
    institutional_capture_ambiguity,
    'Has the universalist reading''s institutionalization through modern reform movements, ashrams, and academic departments resulted in new forms of authority gatekeeping that functionally replicate the exclusions it claimed to dissolve?',
    'Empirical examination of who controls interpretive authority in universalist institutions. Analysis of whether women and low-caste practitioners hold authoritative positions or remain peripheral. Interviews and participatory observation in contemporary bhakti communities to assess whether spiritual access is truly democratized or whether new hierarchies (educational, institutional, linguistic) have replaced old ones.',
    'If new gatekeeping has replaced old caste gatekeeping, the reading''s extraction level (currently 0.18) is underestimated — the coordination function would be revealed as partial and asymmetrically distributing authority. If genuine democratization occurred, the low extraction level is accurate. This determines whether the reading represents true coordination or a reconfigured form of exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_ambiguity, empirical, 'Whether universalist institutionalization has created new forms of authority gatekeeping.').

omega_variable(
    gandhian_coexistence_boundary,
    'Can the universalist devotional reading and the gandhian allegorical reading coherently coexist within a single interpretive framework, or do they ultimately compete for textual authority?',
    'Examination of whether a practitioner or scholar can hold both the universalist claim (bhakti dissolves caste) and the gandhian claim (the battle is metaphor for inner struggle) simultaneously without contradiction. Analysis of historical moments when these readings have been held jointly versus when they have competed.',
    'If they coexist, the reading_relations entry ''coexists_with'' is accurate. If they compete, a more adversarial relation might be warranted. This affects how the engine models the constraint''s position within the Gita kernel''s interpretive ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gandhian_coexistence_boundary, conceptual, 'Whether universalist and gandhian readings are truly coexistent or fundamentally competitive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(gita_tr_t30, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(gita_be_t30, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 50, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(gita_su_t30, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(gita_su_t50, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__universalist_devotional_reading, 0.06).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, hindu_reform_movement_caste_dissolution).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_tradition_authority_decentralization).

% DUAL FORMULATION NOTE:
% This constraint is part of the gita_kurukshetra_discourse kernel family. The three stories (orthodox_literal_reading, gandhian_allegorical_reading, universalist_devotional_reading) decompose the contested kernel into three structurally distinct interpretive claims, each with its own epsilon, beneficiary structure, and classification. The ε-invariance principle requires separate stories because the three readings measure the same text but instantiate fundamentally different constraints: the orthodox reading's referent is caste-based dharma and justified violence; the gandhian reading's referent is the inner-struggle interpretation and non-violence; the universalist reading's referent is caste-transcendent devotion and divine surrender. Each reading has different beneficiaries, different victim structures, and different survival mechanisms. They are linked by network.affects_constraints to model that each reading's success or failure influences the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__universalist_devotional_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
