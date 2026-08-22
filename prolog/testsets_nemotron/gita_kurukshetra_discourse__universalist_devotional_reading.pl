% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Universalist Devotional Reading of the Gita's Kurukshetra Discourse
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint story models the universalist devotional reading of the
 *   Bhagavad Gita's Kurukshetra discourse — the reading that teaches
 *   path-independent bhakti accessible to all regardless of caste, and
 *   redefines dharma as surrender to divine will rather than performance of
 *   varna-based duty. The reading emerged from the medieval Bhakti movements,
 *   was systematized by Ramanuja and later acaryas, and was redeployed by
 *   19th-20th century Hindu reformers (Dayananda Saraswati, Vivekananda,
 *   Gandhi in his later writings, Ambedkar's brief engagement) as scriptural
 *   warrant for caste annihilation. The constraint is CLAIMED as rope — a
 *   genuine coordination mechanism that solves the problem of spiritual
 *   access without centralized mediation — while the authored metrics
 *   describe a constraint with moderate historical extraction (Bhakti
 *   movements often developed their own hierarchies), declining suppression
 *   (the reading no longer requires active enforcement to persist), and
 *   rising theatricality (institutional performances of egalitarianism that
 *   mask persistent caste structure). The claim/metric gap is deliberate: the
 *   engine measures that divergence; do not reconcile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.15).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Universalist Devotional Reading of the Gita's Kurukshetra Discourse").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '41292406-aad8-450e-8f81-3276305587f8').
narrative_ontology:cs_kernel_codification('41292406-aad8-450e-8f81-3276305587f8', fixed_text).
narrative_ontology:cs_authority_grounding('41292406-aad8-450e-8f81-3276305587f8', lineage).
narrative_ontology:cs_interpretation_layer_present('41292406-aad8-450e-8f81-3276305587f8').
narrative_ontology:cs_reading_relation('41292406-aad8-450e-8f81-3276305587f8', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('41292406-aad8-450e-8f81-3276305587f8', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('41292406-aad8-450e-8f81-3276305587f8', foundational, caste_irrelevant_to_moksha).
narrative_ontology:cs_axiom_status(caste_irrelevant_to_moksha, holdable).
narrative_ontology:cs_axiom_grounding('41292406-aad8-450e-8f81-3276305587f8', caste_irrelevant_to_moksha, deontological).
narrative_ontology:cs_axiom('41292406-aad8-450e-8f81-3276305587f8', foundational, bhakti_as_svadharma_universal).
narrative_ontology:cs_axiom_status(bhakti_as_svadharma_universal, holdable).
narrative_ontology:cs_axiom_grounding('41292406-aad8-450e-8f81-3276305587f8', bhakti_as_svadharma_universal, deontological).
narrative_ontology:cs_axiom('41292406-aad8-450e-8f81-3276305587f8', secondary, krishna_as_universal_guru_not_kshatriya_charioteer).
narrative_ontology:cs_axiom_status(krishna_as_universal_guru_not_kshatriya_charioteer, holdable).
narrative_ontology:cs_axiom_grounding('41292406-aad8-450e-8f81-3276305587f8', krishna_as_universal_guru_not_kshatriya_charioteer, deontological).
narrative_ontology:cs_reference_frame('41292406-aad8-450e-8f81-3276305587f8', bhakti_soteriological_universalism).
narrative_ontology:cs_drift_state('41292406-aad8-450e-8f81-3276305587f8', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('41292406-aad8-450e-8f81-3276305587f8', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_caste_practitioners).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, women_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, non_brahmin_spiritual_aspirants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, modern_hindu_reform_movements).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literalist_practitioners).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_as_universal_path).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, divine_surrender_as_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, caste_irrelevant_to_salvation).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, violence_not_mandated_by_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All persons regardless of birth, gender, or social standing who approach the text seeking spiritual liberation through devotion. They gain an egalitarian path that requires no ritual mediation, priestly initiation, or caste qualification. Exit is open — they can adopt or abandon the reading without material penalty, though communal belonging creates soft identity costs.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    moderate, generational, mobile, global).

% Historically excluded from Vedic ritual and Sanskrit textual authority, this reading grants them direct access to the text's soteriological promise. Their exit options are constrained by social embeddedness — leaving the devotional community may mean losing its protective social capital — but the reading itself imposes no barriers.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_caste_practitioners, beneficiary,
    powerless, biographical, constrained, regional).

% Traditionally barred from upanayana and Vedic study, women gain a scripturally grounded claim to equal spiritual standing. The reading's egalitarian logic undermines gendered ritual exclusions. Exit is constrained by patriarchal family and community structures that may penalize independent religious interpretation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, women_devotees, beneficiary,
    powerless, biographical, constrained, global).

% Those outside the priestly varna who seek authoritative spiritual guidance without Brahminical gatekeeping. The reading validates their independent engagement with the text. They can move between interpretive communities with relative freedom, though institutional recognition (temple roles, guru lineages) may remain gated.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, non_brahmin_spiritual_aspirants, beneficiary,
    moderate, biographical, mobile, global).

% Traditional custodians of textual interpretation whose authority rests on varna-based adhikara (qualification). This reading structurally displaces their gatekeeping role by declaring the text's message accessible without their mediation. They are excluded from the reading's interpretive community not by force but by the reading's own logic — their identity is fused to the very gatekeeping the reading dissolves.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_orthodox_authority, excluded,
    institutional, generational, identity_locked, national).

% Institutional actors (Arya Samaj, Ramakrishna Mission, ISKCON, contemporary progressive Hindu organizations) who actively promulgate this reading as scriptural warrant for caste reform and gender equity. They administer temples, publish translations, train teachers, and lobby for legal recognition of the reading's egalitarian implications. They benefit from the reading's moral capital and institutional legitimacy.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, modern_hindu_reform_movements, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, modern_hindu_reform_movements, beneficiary).

% Scholars, activists, and legal actors outside the tradition who deploy this reading in constitutional equality arguments, anti-caste litigation, and human rights discourse. They neither collect nor pay within the devotional economy but amplify the reading's structural effects in the public sphere.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, secular_liberal_interpreters, observer,
    analytical, civilizational, analytical, global).

% Communities committed to the orthodox_literal_reading who experience this reading as a threat to their interpretive framework and social order. They invest resources in counter-publications, institutional policing, and legal challenges to prevent this reading from gaining canonical status. Their costs are defensive — maintaining boundary integrity against a reading that dissolves the caste-textual linkage their authority depends on.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literalist_practitioners, payer,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, textually grounded spiritual path that coordinates diverse practitioners across caste, gender, and regional lines without requiring centralized institutional mediation — the text itself becomes the accessible guru.
% TRANSFER_FUNCTION: Transfers interpretive authority from Brahminical gatekeepers (who control ritual access and textual explication) to the universal devotee class (who read and realize the text directly). Moves soteriological agency from mediated ritual performance to unmediated devotional surrender.
% ABSENT_VOICES: Traditional Smarta orthodoxy (living Advaita matha lineages, Vedic pandit communities) who hold that the Gita's teaching presupposes varnasrama-dharma and that bhakti is a lower path for the unqualified. They are structurally excluded from this reading's interpretive community because their epistemic framework treats the reading's core claim (caste-irrelevant access) as category error. Also absent: Adivasi and Dalit communities who reject the Gita entirely as a Brahminical text — their objection is not to the reading but to the kernel itself.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the scriptural warrant for caste-egalitarian Hindu reform would collapse. Modern reform movements would lose their strongest textual anchor. Anti-caste litigation citing the Gita would lose doctrinal grounding. The universal devotee class would revert to dependence on mediated ritual or seek alternative texts. The Brahminical orthodox authority would face one fewer challenger to its interpretive monopoly.
% FOUNDING_PROBLEM: How to make the Gita's soteriological promise available to those excluded by the varnasrama system's ritual and textual gatekeeping — women, Shudras, 'untouchables,' and non-dvija men — without abandoning the text's authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the historical record of Bhakti movement saints (Ramanuja, Kabir, Ravidas, Mirabai, Chaitanya, Tukaram) who explicitly cited the Gita's universalist verses (9.29, 9.32, 18.66) as warrant for ignoring caste barriers. Contemporary Dalit Buddhist and Dalit Christian movements also attest the problem is live — they left Hinduism precisely because the orthodox reading's gatekeeping persisted despite the universalist reading's scriptural presence.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.28) reflects that the reading's primary operation is coordinative — it enables access — but historical Bhakti institutions did extract labor, resources, and devotional surplus from adherents. The slight uptick at 2025 captures contemporary guru movements that deploy universalist rhetoric while building personality cults. Suppression (0.15) is low because the reading spreads by attraction and textual self-evidence, not coercion; its historical suppression requirement was higher when it faced orthodox censorship. Theater ratio (0.32) captures the gap between institutional declarations of caste equality and the lived reality of caste-endogamous temple communities, guru lineages, and marriage networks. Accessibility collapse (0.25) is low — alternatives (orthodox reading, secular rejection, other texts) remain fully available. Resistance (0.45) is moderate — the reading faces active counter-exegesis from orthodox institutions and rejection from anti-caste movements that view the Gita as irredeemably Brahminical.
 *
 * PERSPECTIVAL GAP:
 *   The universal_devotee_class (moderate power, mobile exit) experiences this as pure coordination — a rope. The marginalized_caste_practitioners and women_devotees (powerless, constrained exit) experience it as a rope with residual extraction from the devotional institutions that mediate it — still rope, but with higher effective extraction. The brahminical_orthodox_authority (institutional, identity_locked) experiences it as a snare — it extracts their interpretive monopoly and threatens their structural position. The modern_hindu_reform_movements (organized, mobile) are agenda_setters who benefit from the reading's moral capital while managing its institutional theatricality. The orthodox_literalist_practitioners (organized, identity_locked) are payers who bear defensive costs. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (universal devotees, marginalized castes, women, non-Brahmin aspirants) are declared in base_properties — they gain access, agency, and scriptural warrant. No victims are declared — the reading does not extract from a defined victim class. The brahminical_orthodox_authority is excluded, not victimized: their loss of gatekeeping is a structural displacement, not an extraction transfer. The orthodox_literalist_practitioners are payers — they bear costs defending their framework — but the reading itself does not target them; their costs are self-incurred in resistance. The modern_hindu_reform_movements are dual-positioned (agenda_setter/beneficiary) — they administer the reading's institutional form and collect its legitimacy rents. Directionality derives from these declarations: beneficiaries → low d; payers → moderate d (defensive, not targeted); excluded → identity_locked pushes d high but they are not in the constraint's extraction path.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (spiritual access for the excluded) remains live — caste barriers persist in temple entry, priesthood, marriage, and social dignity. The reading has not atrophied into piton: it still coordinates genuine devotional communities and fuels active reform movements. It has not become tangled_rope: its coordination function (universal access) and any extraction (institutional devotional surplus) are not structurally fused — the extraction is institutional accretion, not the reading's logic. It is not scaffold: no sunset clause, the problem is not transitional. The classification as rope is structurally stable: a genuine coordination mechanism whose historical accretions are separable from its core logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bhakti_institution_extraction,
    'To what extent do historical and contemporary Bhakti institutions (mathas, sampradayas, guru lineages) that claim this reading actually extract surplus from devotees, versus operating as genuine coordination mechanisms?',
    'Comparative economic ethnography of devotional institutions: measure resource flows (donations, labor, land) against services provided (education, healthcare, ritual, community). Track whether universalist rhetoric correlates with lower or higher extraction.',
    'If extraction is systematically high, the reading''s historical trajectory is tangled_rope (coordination + extraction fused institutionally) rather than rope. If extraction is low and localized, the reading remains rope with institutional accretions as separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_institution_extraction, empirical, 'Whether the universalist reading''s institutional vehicles are extractive').

omega_variable(
    orthodox_foreclosure_boundary,
    'Does the orthodox_literal_reading''s core premise (varnasrama-dharma as prerequisite for Gita adhikara) logically foreclose the universalist_devotional_reading within a single hermeneutic framework, or do they merely coexist as competing interpretations?',
    'Formal analysis of the two readings'' presupposition structures: if the orthodox reading''s adhikara doctrine entails that verses 9.32 and 18.66 cannot mean what the universalist reading claims, then foreclosure holds. If the orthodox reading can accommodate those verses as ''lower path for the unqualified'' without contradiction, coexistence holds.',
    'If forecloses: the two readings cannot be held by the same interpretive community — the kernel splits into mutually exclusive frameworks. If coexists_with: both remain live options for different parties within overlapping communities. This determines the reading_relations value and the kernel''s structural unity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orthodox_foreclosure_boundary, conceptual, 'Whether the orthodox and universalist readings foreclose each other or coexist').

omega_variable(
    gandhian_influence_vector,
    'Does the gandhian_allegorical_reading (Kurukshetra as internal struggle) create structural downstream pressure on the universalist_devotional_reading by legitimizing non-literal hermeneutics that the universalist reading also depends on?',
    'Genealogical analysis: trace whether Gandhi''s allegorical method (which universalizes the text by metaphorizing its violence) opened interpretive space for the universalist reading''s caste-dissolving move, or whether the universalist reading''s Bhakti lineage predates and operates independently of Gandhian hermeneutics.',
    'If influences: the gandhian reading''s cultural dominance in 20th century India structurally enabled the universalist reading''s modern reform deployment. If coexists_with: they are independent lineages that happen to align on egalitarian outcomes. Affects the reading_relations declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gandhian_influence_vector, empirical, 'Whether the Gandhian allegorical reading influences the universalist devotional reading''s modern viability').

omega_variable(
    universalist_reading_caste_performative,
    'Is the contemporary institutional performance of this reading (temple declarations, reform organization statements, legal briefs) primarily performative — theatrical maintenance of an egalitarian self-image while caste endogamy and hierarchy persist in the same communities?',
    'Longitudinal study of caste practices (marriage, dining, priesthood, leadership) in institutions that explicitly endorse this reading vs. those that do not. Measure gap between doctrinal commitment and behavioral markers over 50+ years.',
    'If theater ratio is understated (actual theatricality > 0.32), the constraint may be piton (degraded coordination maintained theatrically) rather than rope. If theatricality is confined to specific institutions while grassroots devotional communities remain genuine, the reading stays rope with localized piton accretions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universalist_reading_caste_performative, empirical, 'Whether the reading''s contemporary institutional form is performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_univ_dev_tr_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1800, 0.55).
narrative_ontology:measurement(gita_univ_dev_tr_t1850, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1850, 0.48).
narrative_ontology:measurement(gita_univ_dev_tr_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement(gita_univ_dev_tr_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(gita_univ_dev_tr_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(gita_univ_dev_tr_t2025, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(gita_univ_dev_be_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1800, 0.45).
narrative_ontology:measurement(gita_univ_dev_be_t1850, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1850, 0.38).
narrative_ontology:measurement(gita_univ_dev_be_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1900, 0.32).
narrative_ontology:measurement(gita_univ_dev_be_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(gita_univ_dev_be_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement(gita_univ_dev_be_t2025, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gita_univ_dev_su_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1800, 0.35).
narrative_ontology:measurement(gita_univ_dev_su_t1850, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1850, 0.28).
narrative_ontology:measurement(gita_univ_dev_su_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1900, 0.22).
narrative_ontology:measurement(gita_univ_dev_su_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(gita_univ_dev_su_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(gita_univ_dev_su_t2025, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, hindu_personal_law_reform).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, temple_entry_movements).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, anti_caste_constitutional_litigation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the gita_kurukshetra_discourse constraint family (kernel_id: gita_kurukshetra_discourse). The three readings instantiate three constraints with different ε values and different beneficiary/victim structures: orthodox_literal_reading (high extraction, Brahminical beneficiaries, Shudra/women victims, claimed_type: snare); gandhian_allegorical_reading (low extraction, nonviolent practitioners as beneficiaries, no clear victims, claimed_type: rope); universalist_devotional_reading (this story: moderate declining extraction, universal devotees as beneficiaries, no victims, claimed_type: rope). They are linked by network.affects_constraints because the universalist reading's scriptural warrant is cited against the orthodox reading in legal and reform contexts, and the Gandhian reading's allegorical method historically enabled the universalist reading's modern deployment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__universalist_devotional_reading, institutional, 0.35).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__universalist_devotional_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
