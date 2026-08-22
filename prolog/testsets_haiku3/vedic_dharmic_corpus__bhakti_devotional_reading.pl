% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Access to Divine Authority
 *   domain: religious/social/interpretive
 *
 * SUMMARY:
 *   The Vedic dharmic corpus is contested on the question of spiritual
 *   authority and caste requirement. The bhakti devotional reading claims
 *   that sincere emotional devotion (bhakti) to the divine grants direct
 *   spiritual access and authority independent of birth into Brahmin caste.
 *   This reading coexists with the hereditary monopoly reading (authority
 *   derives from Brahmin birth and Vedic ritual mastery) and the reformist
 *   egalitarian reading (varna hierarchy is illegitimate; rationalist
 *   critique supersedes tradition). The bhakti reading does not argue for
 *   caste abolition but for spiritual authority that bypasses
 *   birth-determined gatekeeping—a middle position that opens religious
 *   participation without dismantling the caste hierarchy itself.
 *   Extractiveness is moderate (0.40) because the arrangement transfers
 *   religious authority without offering structural equality; suppression is
 *   relatively low (0.35) because the bhakti reading does not require active
 *   suppression of alternatives to persist—multiple readings coexist. Theater
 *   ratio is modest (0.22), indicating that devotional practice carries
 *   genuine spiritual function alongside its role in authority contestation.
 *
 * KEY AGENTS:
 *   - sincere_devotees_across_varna: Beneficiaries of the bhakti reading; claim direct divine access through devotion (moderate power, mobile exit, regional scope)
 *   - hereditary_brahmin_ritualists: Institutional authority holders under hereditary monopoly reading; experience bhakti reading as erosion of exclusive ritual gatekeeping (institutional power, constrained exit, regional scope)
 *   - lower_varna_devotional_practitioners: Shudra, vaishya practitioners; beneficiaries of devotional spiritual authority opening (moderate power, mobile exit, regional scope)
 *   - women_devotional_participants: Beneficiaries of devotional authority recognition; remain constrained by gender restrictions in some traditions (moderate power, constrained exit, regional scope)
 *   - traditional_authority_lineages: Institutional guardians of both textual and devotional traditions; adjudicate which readings are legitimate within their lineage (institutional power, trapped exit, regional scope)
 *   - reformist_egalitarian_interpreters: Excluded from bhakti framework; argue for constitutional equality over traditional authority structures (organized power, mobile exit, regional scope)
 *   - theological_analytical_seat: Scholars observing multiple coexisting readings of the kernel (analytical power, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access to Divine Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/social/interpretive").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '34112023-3632-4989-9170-dfb447673731').
narrative_ontology:cs_kernel_codification('34112023-3632-4989-9170-dfb447673731', fixed_text).
narrative_ontology:cs_authority_grounding('34112023-3632-4989-9170-dfb447673731', lineage).
narrative_ontology:cs_interpretation_layer_present('34112023-3632-4989-9170-dfb447673731').
narrative_ontology:cs_reading_relation('34112023-3632-4989-9170-dfb447673731', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('34112023-3632-4989-9170-dfb447673731', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('34112023-3632-4989-9170-dfb447673731', foundational, sincere_devotion_grants_spiritual_authority).
narrative_ontology:cs_axiom_status(sincere_devotion_grants_spiritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('34112023-3632-4989-9170-dfb447673731', sincere_devotion_grants_spiritual_authority, deontological).
narrative_ontology:cs_axiom('34112023-3632-4989-9170-dfb447673731', foundational, interior_sincerity_supersedes_exterior_birth).
narrative_ontology:cs_axiom_status(interior_sincerity_supersedes_exterior_birth, holdable).
narrative_ontology:cs_axiom_grounding('34112023-3632-4989-9170-dfb447673731', interior_sincerity_supersedes_exterior_birth, deontological).
narrative_ontology:cs_reference_frame('34112023-3632-4989-9170-dfb447673731', devotional_authority_framework).
narrative_ontology:cs_drift_state('34112023-3632-4989-9170-dfb447673731', contemporary_institutional_incorporation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('34112023-3632-4989-9170-dfb447673731', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotees_across_varna).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, lower_varna_devotional_practitioners).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotional_participants).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_ritualists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in devotional worship and claim direct access to the divine through sincere emotional attachment (bhakti) without requiring birth into Brahmin caste or mastery of Vedic ritual. They testify that sincere devotion—emotional surrender, prayer, moral intent—suffices for spiritual authority and divine grace. Their spiritual standing is determined by the authenticity of their devotional practice, not by hereditary lineage.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotees_across_varna, beneficiary,
    organized, generational, mobile, regional).

% Hold institutional authority over Vedic interpretation, ritual conduct, and spiritual legitimacy under the hereditary monopoly reading. The bhakti reading transfers legitimate spiritual authority to non-Brahmin devotees, eroding the institutional monopoly on religious authority and reducing the exclusive ritual gatekeeping function their birth provides. They retain custodial authority over texts but lose exclusive claim to devotional access.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_ritualists, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_ritualists, observer).

% Members of shudra, vaishya, and other non-Brahmin varnas who practice devotional worship (singing hymns, pilgrimage, emotional prayer) and experience this reading as validating their spiritual agency. Their capacity for direct divine access is recognized; they are not ritual-excluded by birth status. However, they remain structurally subordinate in social hierarchy—the reading addresses spiritual authority, not caste rank itself.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, lower_varna_devotional_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Women (across varnas) are often excluded from Vedic ritual authority and may be prohibited from certain scriptural studies under traditional hereditary readings. The bhakti reading opens devotional authority to women through sincere practice—bhakti movements historically honored women mystics and saint-poets as spiritual authorities. Their exclusion is not eliminated but substantially reduced in the devotional sphere.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotional_participants, beneficiary,
    moderate, biographical, constrained, regional).

% Intellectuals and activists arguing that the Vedic corpus must be read through the lens of constitutional equality and rational critique, that caste hierarchy is historical accretion not scriptural essence. The bhakti reading does not adopt their premise (constitutional rationalism) but does move partway toward their practical outcome (opening spiritual authority beyond hereditary Brahmins). They remain outside the devotional framework's internal logic.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_interpreters, excluded,
    organized, generational, mobile, regional).

% Religious institutions, temple authorities, and interpretive communities that preserve and transmit both the Vedic texts and devotional traditions. They adjudicate which readings of the Vedic dharmic corpus are legitimate within their lineage and which devotional practices conform to their understanding of proper spiritual authority. Some lineages embrace bhakti readings; others defend hereditary monopoly readings; the same institution may hold both readings in productive tension.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_authority_lineages, agenda_setter,
    institutional, civilizational, trapped, regional).

% Scholars and historians analyzing the Vedic corpus as a contested kernel with multiple legitimate readings. They observe that no single reading exhausts the textual materials; the kernel admits both hereditary and devotional framings from the same source texts.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, theological_analytical_seat, observer,
    analytical, civilizational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a pathway for spiritual authority grounded in sincere devotional practice (emotional commitment, moral integrity, heartfelt prayer) rather than hereditary privilege. Solves the problem of how non-Brahmin individuals can claim legitimate spiritual standing and direct relationship with the divine without access to Vedic ritual training or Brahmin birth.
% TRANSFER_FUNCTION: Transfers spiritual authority recognition from birth-determined Brahmin monopoly to practice-determined devotional sincerity; transfers legitimacy judgment from ritual mastery to emotional authenticity and devotional behavior. In practical terms, recognition and social honor flow to sincere devotees regardless of varna, reducing but not eliminating the prestige and authority reserved for hereditary Brahmins.
% ABSENT_VOICES: Reformist egalitarian interpreters (constitutional rationalists) are excluded from the bhakti framework's internal logic—they would argue the entire varna system is illegitimate, not merely that devotional access should bypass it. Lower-caste communities experiencing devotional practice as insufficient without structural caste equality are also not fully represented in the devotional reading alone.
% DISAPPEARANCE_RATIONALE: If the bhakti devotional reading vanished from lived practice, the hereditary monopoly reading would reassert full Brahmin institutional control over spiritual authority; the social recognition of non-Brahmin devotees would collapse. However, the disappearance might leave in place the devotional practices and emotional relationships to the divine—communities might continue devotion as private prayer even if public spiritual authority reverted to hereditary gatekeeping. The world would rearrange institutionally but not entirely disappear in lived experience.
% FOUNDING_PROBLEM: How can sincere individuals of any birth status approach the divine directly without intermediaries, ritual specialists, or hereditary gatekeeping? The founding problem is theological and existential: the claim that the Vedic dharmic corpus permits direct, unmediated devotional access independent of birth-determined ritual authority.
% FOUNDING_PROBLEM_CORROBORATION: This founding problem is attested by devotional practitioners themselves (bhakti movements across South Asia, saint-poetry traditions, pilgrimage practices documented over centuries). It is corroborated by textual scholars outside the Brahmin institutional beneficiary class: historians and religious studies scholars observe that the Upanishads and bhakti poetry in the Vedic tradition contain explicit passages supporting direct devotional access (e.g., Katha Upanishad, Bhagavad Gita's framing of devotion to Krishna, Bhakti Sutras). The founding problem remains contested by hereditary monopoly readers who interpret the same texts as supporting Brahmin ritual exclusivity.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, contested).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness stabilizes at 0.40 by interval point 15 because the bhakti reading transfers authority recognition to non-Brahmins (extractive from hereditary Brahmin monopoly) but does not eliminate caste hierarchy or grant structural equality—the constraint accommodates institutional Brahminism while opening a parallel devotional pathway. This is extraction with coordination: the constraint creates new authority pathways (rope-like coordination function) while extracting monopoly rents from the institutional Brahmin class (asymmetric transfer). Suppression remains relatively low (0.35, slightly rising) because alternative readings (hereditary monopoly, reformist egalitarian) coexist and are not actively suppressed—they are competing interpretations within a contested kernel. Theater ratio stays modest (0.22) because devotional sincerity is the genuine substance of the reading, not performative cover. The measurement series reflects equilibration: extractiveness and suppression rise slightly early as the reading gains institutional recognition, then plateau as a stable coexistence arrangement emerges across multiple lineages.
 *
 * PERSPECTIVAL GAP:
 *   The bhakti reading produces different classifications from different seats: For sincere non-Brahmin devotees, the constraint is a rope—genuine coordination solving the problem of devotional access. For hereditary Brahmins, it is a tangled rope or snare—coordination of devotees paired with extraction from their ritual monopoly. For institutional lineages that hold BOTH readings, it is a contested constraint that shifts meaning depending on which reading they activate. The engine should compute these divergences from the structural data: beneficiary position (sincere devotees), payer position (Brahmin institutional monopoly), and the coexistence without active suppression. No single seat has full view of the constraint—each seat's experience is partial.
 *
 * DIRECTIONALITY LOGIC:
 *   Sincere devotees across varna are structural beneficiaries (d near 0.0): they gain authority recognition and direct divine access. Hereditary Brahmin ritualists are structural payers (d near 1.0): they lose exclusive institutional monopoly and face erosion of ritual gatekeeping authority. Lower-varna practitioners are moderate beneficiaries (d ~0.25): they gain devotional authority but remain caste-subordinate. Women participants are moderate beneficiaries (d ~0.30): they gain spiritual authority recognition but face persistent gender constraints. Reformist egalitarian interpreters sit outside the reading's internal logic (excluded role, analytical exit—not included in d computation). Traditional authority lineages that adjudicate readings are institutional agenda-setters (d ~0.50): they benefit from managing multiple readings simultaneously but bear the cost of internal contestation. The key directionality claim: this constraint extracts from institutional Brahminism while coordinating sincere devotees—asymmetric but not coercive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (direct devotional access independent of birth) remains live, not dead. The devotional reading continues to serve its original function: enabling non-Brahmin spiritual agency. Mandatrophy does NOT apply. However, the constraint shows incipient drift: as institutional lineages incorporate bhakti readings alongside hereditary authority, the devotional pathway risks absorption into a composite orthodoxy that maintains caste structure while gesturing toward spiritual equality. The theater ratio, though modest, is rising—some institutional adoption of bhakti framing may be performative legitimacy-washing rather than genuine authority transfer. The measurement series should trigger monitoring for the theater_ratio crossing 0.35; if it does, the reading risks mandatrophy: the founding problem (devotional access) persists, but the constraint gradually becomes a vessel for institutional performance of egalitarianism rather than actual authority transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_kernel_ambiguity,
    'Does the Vedic corpus itself support the bhakti reading, or does the reading impose a devotional framework onto textual materials originally designed to protect Brahmin authority?',
    'Philological analysis of specific passages (Upanishads, Bhagavad Gita, Brahma Sutras) comparing devotional and hereditary interpretations; examination of the historical composition and transmission of the texts.',
    'If the bhakti reading is textually grounded, it represents a legitimate interpretation of the kernel and a genuine coordination function. If imposed, the reading becomes a cover story for erosion of institutional authority—reclassifying it from rope to tangled_rope with higher extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_kernel_ambiguity, empirical, 'Whether the bhakti reading is textually supported or interpretively constructed.').

omega_variable(
    institutional_absorption_risk,
    'Will traditional authority lineages gradually absorb the bhakti reading as a subordinate legitimacy gesture while maintaining caste-hierarchical structure, converting genuine authority transfer into performative egalitarianism?',
    'Longitudinal observation of how institutional lineages frame devotional and hereditary readings over generational time; measurement of whether non-Brahmin devotees experience actual authority recognition or symbolic inclusion.',
    'If absorption occurs, theater_ratio rises sharply and the constraint drifts toward piton status: the founding problem (devotional access) persists, but the reading becomes maintenance theater. The fixed 0.40 extractiveness at interval end (plateau at t=15) may mask this drift—post-interval monitoring is critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_absorption_risk, empirical, 'Whether institutional adoption represents genuine authority transfer or performative legitimation.').

omega_variable(
    caste_hierarchy_persistence,
    'Does the bhakti reading''s opening of spiritual authority constitute an implicit endorsement of caste structure elsewhere (social hierarchy, labor distribution, ritual purity rules), or does it genuinely decouple spiritual from social authority?',
    'Ethnographic study of bhakti-practicing communities: do they maintain caste-segregated labor, marriage rules, and purity restrictions even while recognizing devotional authority across varna? Does the reading reduce caste enforcement or merely reframe it?',
    'If bhakti authority genuinely decouples from caste hierarchy, extractiveness should be lower (~0.25) and the constraint is cleaner as rope. If it coexists with maintained caste structure, extractiveness may be understated—the constraint coordinates spiritual authority while leaving social extraction intact (true extractiveness ~0.55+), reclassifying as tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caste_hierarchy_persistence, empirical, 'Whether bhakti authority transfer implies structural caste equality or coexists with maintained hierarchy.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.35) structural (active institutional suppression of bhakti readings) or internalized (non-Brahmin devotees internalize constraints and limit their own authority claims)?',
    'Comparison of suppression profiles between bhakti-dominant lineages (where the reading is endorsed) and hereditary-monopoly-dominant lineages (where it is suppressed); observation of whether devotees experience external barriers or self-imposed limitation.',
    'If internalized, the constraint''s true suppressive force is higher than measured; bhakti practitioners carry caste subordination into their devotional practice regardless of institutional endorsement. If structural, the 0.35 rating is accurate and post-exit suppression drops. This affects whether the constraint should be reclassified toward snare (high internalized suppression) or confirmed as rope (structural suppression only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether measured suppression is structural enforcement or internalized subordination.').

omega_variable(
    reading_foreclosure_empirical,
    'Will accumulating pressure from reformist egalitarian rationalism eventually foreclose the bhakti reading by rendering it obsolete (a quaint devotional practice no longer grounding spiritual authority claims)?',
    'Long-term observation (30+ years) of whether constitutional egalitarianism supplants both bhakti and hereditary readings as the dominant legitimacy frame in institutional religious authority.',
    'If foreclosure occurs, the bhakti reading drifts from rope to scaffold (transitional, function atrophying). The cs_structure.reading_relations should currently be coexists_with (both live); if reformist rationalism becomes hegemonic, the relation drifts toward influences→forecloses. This is a slow drift but structurally consequential.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical, empirical, 'Whether modern constitutionalism will eventually foreclose the bhakti reading''s legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vedi_tr_t5, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(vedi_tr_t15, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(vedi_tr_t25, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(vedi_be_t5, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(vedi_be_t15, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(vedi_be_t25, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 25, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(vedi_su_t5, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(vedi_su_t15, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(vedi_su_t25, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 25, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The vedic_dharmic_corpus kernel admits three structurally distinct readings, each with different extractiveness, beneficiary structures, and institutional consequences. The bhakti_devotional_reading transfers spiritual authority from birth-determined Brahmins to sincere devotees across varna, creating a middle position between hereditary monopoly (exclusive Brahmin authority) and reformist egalitarianism (authority grounded in constitutional equality). All three readings instantiate from the same textual kernel but have different epsilon values, different victim/beneficiary sets, and different type classifications. They are linked via network.affects_constraints as a constraint family—each reading influences the others by competing for institutional legitimacy and by shifting what devotional, hereditary, and egalitarian authority mean in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
