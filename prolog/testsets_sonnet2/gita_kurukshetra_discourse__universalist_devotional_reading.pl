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
 *   human_readable: Bhakti-Universalist Reading of the Bhagavad Gita's Dharma Teaching
 *   domain: religious/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the universalist-devotional reading of the
 *   Bhagavad Gita's Kurukshetra discourse: the claim that Krishna's teaching
 *   of bhakti (devotional surrender) offers a path to liberation independent
 *   of caste status, and that dharma properly understood is surrender to
 *   divine will rather than fidelity to hereditary social role (svadharma).
 *   This reading underlies much of the historical bhakti movement (Chaitanya
 *   Vaishnavism, various Sant traditions) and later reformist and modern
 *   popular readings of the text. It is one of at least three structurally
 *   distinct readings of the same kernel text: the orthodox literal reading
 *   (which holds the text mandates caste duty and sanctions righteous
 *   violence) and the Gandhian allegorical reading (which treats the
 *   battlefield itself as metaphor for internal moral struggle) are separate
 *   constraints with their own ε values, authored as sibling stories and
 *   linked via network.affects_constraints. This story's ε is authored
 *   strictly for the universalist-devotional reading's own account of the
 *   standing arrangement: the historical and ongoing tension between
 *   devotional-access institutions and caste-gatekeeping ritual authority,
 *   not for the egalitarian end-state the reading aspires to.
 *
 * KEY AGENTS:
 *   - low_caste_devotees: primary beneficiary (powerless/constrained) — gain textually sanctioned salvific access
 *   - bhakti_movement_teachers: agenda_setter (organized/mobile) — propagate and institutionalize the reading
 *   - hereditary_brahmin_ritualists: primary target (powerful/constrained) — lose interpretive and ritual monopoly
 *   - caste_based_temple_authorities: secondary target (powerful/constrained) — lose gatekeeping control over sacred access
 *   - orthodox_commentarial_tradition: excluded voice (institutional/trapped) — textual counter-arguments sidelined
 *   - comparative_religion_scholars: analytical observer (analytical) — documents the reading's social function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.38).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.42).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhakti-Universalist Reading of the Bhagavad Gita's Dharma Teaching").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__universalist_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, 'c162023f-9e84-4f70-8326-b963cde36c02').
narrative_ontology:cs_kernel_codification('c162023f-9e84-4f70-8326-b963cde36c02', fixed_text).
narrative_ontology:cs_authority_grounding('c162023f-9e84-4f70-8326-b963cde36c02', practice).
narrative_ontology:cs_interpretation_layer_present('c162023f-9e84-4f70-8326-b963cde36c02').
narrative_ontology:cs_reading_relation('c162023f-9e84-4f70-8326-b963cde36c02', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('c162023f-9e84-4f70-8326-b963cde36c02', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('c162023f-9e84-4f70-8326-b963cde36c02', foundational, devotion_supersedes_caste_qualification).
narrative_ontology:cs_axiom_status(devotion_supersedes_caste_qualification, holdable).
narrative_ontology:cs_axiom_grounding('c162023f-9e84-4f70-8326-b963cde36c02', devotion_supersedes_caste_qualification, deontological).
narrative_ontology:cs_axiom('c162023f-9e84-4f70-8326-b963cde36c02', foundational, dharma_is_surrender_not_social_role).
narrative_ontology:cs_axiom_status(dharma_is_surrender_not_social_role, holdable).
narrative_ontology:cs_axiom_grounding('c162023f-9e84-4f70-8326-b963cde36c02', dharma_is_surrender_not_social_role, deontological).
narrative_ontology:cs_reference_frame('c162023f-9e84-4f70-8326-b963cde36c02', premodern_bhakti_commentarial_tradition).
narrative_ontology:cs_drift_state('c162023f-9e84-4f70-8326-b963cde36c02', contemporary_global_hindu_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c162023f-9e84-4f70-8326-b963cde36c02', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, low_caste_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_movement_teachers).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, reformist_vaishnava_lineages).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, hereditary_brahmin_ritualists).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, caste_based_temple_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically excluded from Vedic ritual access and temple entry on caste grounds. Under this reading, Krishna's promise that devotion (bhakti) alone secures liberation regardless of birth gives them a textually sanctioned path to salvation that bypasses Brahminical ritual gatekeeping. They gain scriptural standing but still face social enforcement of caste hierarchy outside the devotional frame.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, low_caste_devotees, beneficiary,
    powerless, generational, constrained, regional).

% Vernacular poet-saints and devotional lineage founders who read and teach the Gita as authorizing surrender-based devotion open to all. They actively propagate this reading through vernacular commentary, hymn, and popular preaching, and their institutional and social standing grows as the reading spreads.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_movement_teachers, agenda_setter,
    organized, generational, mobile, regional).

% Devotional institutions and monastic orders built around this reading collect disciples, patronage, and legitimacy by offering an alternative to caste-restricted ritual religion. They administer temples, texts, and initiation that operationalize the universalist claim.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, reformist_vaishnava_lineages, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, reformist_vaishnava_lineages, agenda_setter).

% Their monopoly on Vedic ritual competence and scriptural interpretation is the thing this reading structurally erodes: if devotion supersedes ritual and caste qualification, their gatekeeping function loses its textual warrant. They lose interpretive authority and the fee/status structures built on ritual exclusivity.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, hereditary_brahmin_ritualists, payer,
    powerful, generational, constrained, regional).

% Administer temple access and ritual privilege along caste lines. The universalist reading is cited by reform movements and, later, legal reformers to challenge their exclusionary practices, directly threatening their control over sacred space and its revenues.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, caste_based_temple_authorities, payer,
    powerful, generational, constrained, regional).

% The literalist and caste-affirming commentarial lineages (e.g., strict Mimamsa-adjacent readings) hold that this reading dissolves textually mandated svadharma obligations. They are not erased but are structurally sidelined wherever the universalist reading gains institutional traction; their objection is rarely represented in devotional retellings.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_commentarial_tradition, excluded,
    institutional, civilizational, trapped, national).

% Study the historical emergence and social function of the bhakti-universalist reading, tracing its relationship to caste reform movements, colonial-era reinterpretation, and modern Hindu reform. They document who adopted the reading and to what social effect without adjudicating its theological truth.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared devotional path to liberation that does not require ritual competence, caste qualification, or Sanskrit literacy — coordinating a much larger population around a single accessible practice (surrender/devotion to Krishna) than ritual religion could include.
% TRANSFER_FUNCTION: Moves religious authority and status from hereditary ritual specialists (whose standing depended on caste-gated ritual competence) to devotional teachers and movements who can initiate and instruct across caste lines; moves the locus of salvific legitimacy from birth-right to devotional practice.
% ABSENT_VOICES: Orthodox commentarial traditions holding strict svadharma readings are structurally sidelined in popular transmission of this reading; their textual counter-arguments (that Krishna nowhere explicitly abolishes caste duty, and reaffirms it in chapter 18) rarely appear in the devotional retelling that carries this interpretation to lay audiences.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a live interpretive option, bhakti movements and reform Vaishnava institutions built on its textual warrant would lose their primary scriptural legitimation for cross-caste religious access; low-caste devotees would lose a textually sanctioned claim to salvation independent of ritual/caste status, and orthodox ritualist authority would regain uncontested interpretive ground over the text.
% FOUNDING_PROBLEM: The historical problem of a large population excluded from Vedic ritual religion by caste birth, lacking any textually sanctioned path to liberation that did not require ritual competence or twice-born status.
% FOUNDING_PROBLEM_CORROBORATION: Bhakti movement teachers and their devotee communities attest the problem (caste exclusion from salvation) was real and is substantially addressed by this reading. Orthodox commentarial traditions and hereditary ritualists dispute that the text ever intended to abolish caste-based duty, holding chapter 18's reaffirmation of svadharma as evidence the founding problem as bhakti reformers construe it was never the text's own problem. Independent historians of religion corroborate that the bhakti reading emerged historically as a documented response to caste exclusion, without adjudicating whether that response is textually correct.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate (0.38) and rising slightly over the interval: the reading does real coordination work (opening salvation to excluded populations, generating durable devotional institutions), but as those institutions matured they also began extracting allegiance, tithes, and interpretive authority in their own right — a dynamic distinct from, but structurally continuous with, the ritual monopolies they displaced. Suppression is authored moderate-to-declining (0.55 to 0.42): early bhakti movements faced real resistance and occasional persecution from orthodox authorities that has diminished as the reading gained mainstream acceptance in much of contemporary Hinduism, though it has not vanished — orthodox institutions in some regions still contest devotional-access claims. Theater ratio is authored low-to-moderate and rising (0.10 to 0.28): as devotional institutions professionalized, some of the movement's egalitarian practice has become more performative (symbolic openness without full structural equality) even as its textual claim remains substantively contested.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of low-caste devotees and bhakti teachers, this reading is close to a rope: a genuine coordination mechanism opening salvation to those excluded by birth, with real historical beneficiaries and minimal coercion of anyone. From the seat of hereditary ritualists and temple authorities, the same reading functions as an extractive reframing that dismantles their interpretive monopoly and diverts allegiance and patronage toward rival devotional institutions — they experience active enforcement (through legal reform, social pressure, and competitive institution-building) working against their prior position. The engine computes these as different effective extraction values from the same base ε because directionality differs sharply by seat; this divergence is the phenomenon under study, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-caste devotees and bhakti teachers/institutions are declared beneficiaries — the reading's own textual warrant subsidizes their claim to religious standing, so directionality sits toward the beneficiary end. Hereditary ritualists and temple authorities are declared victims — their historical interpretive and gatekeeping monopoly is what the reading structurally erodes, so directionality sits toward the target end, moderated by their continuing powerful institutional position (they are far from powerless even as targets). This is the inverse power arrangement from most snares: here the powerful party is the target of the reframing, not the beneficiary — a useful case for testing whether the engine's directionality derivation handles high-power targets correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — caste exclusion from salvific religious practice — is genuinely contested rather than settled: bhakti communities regard it as substantially solved by their own reading, while orthodox commentarial traditions dispute that it was ever the text's problem to solve, and caste-based exclusion persists in many devotional institutions themselves despite the universalist textual claim. This divergence is exactly the kind of question the founding_problem_status field is designed to surface rather than resolve: the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals a live, unresolved genealogy rather than a settled or zombie arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    does_gita_actually_abolish_svadharma,
    'Does the Gita''s bhakti teaching (especially chapters 9, 12, and 18) actually supersede or abolish caste-based svadharma, or does it operate alongside continued affirmation of caste duty (as chapter 18''s closing reaffirmation of varna-based duty suggests to orthodox readers)?',
    'Close textual-critical comparison of the bhakti passages against the explicit svadharma passages in chapter 18, cross-referenced with the historical reception record of how various commentarial traditions (Shankara, Ramanuja, Madhva, and later bhakti commentators) resolved the apparent tension.',
    'If the abolition reading is textually strained, this constraint''s claimed_type and beneficiary structure represent a later reformist imposition on the text rather than the text''s own teaching — which would not change ε for this reading (ε is authored for the standing arrangement under contest as this reading''s own lights see it) but would sharpen the omega''s resolution toward the orthodox sibling reading''s account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(does_gita_actually_abolish_svadharma, conceptual, 'Whether the universalist reading''s core textual claim is well-supported or a later reformist overlay.').

omega_variable(
    reading_selection_and_colonial_reception_history,
    'To what extent did the universalist-devotional reading''s modern prominence result from colonial-era and nationalist-era reinterpretation (e.g., readings that emphasized the Gita''s universal ethical appeal for apologetic or nation-building purposes) rather than continuous premodern bhakti tradition?',
    'Historical tracing of the reading''s textual lineage from medieval bhakti commentators (Chaitanya, various Sants) through colonial-era reformers (Vivekananda, Aurobindo) to contemporary popular usage, distinguishing continuous devotional tradition from colonial-apologetic innovation.',
    'If substantially a modern construction, the reading''s claim to represent the text''s ''original'' universal message is weaker than its claim to represent a legitimate but historically situated interpretive tradition — this would not change the reading''s ε but would inform the founding_problem_corroboration assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_and_colonial_reception_history, empirical, 'The historical provenance of the universalist reading''s claim to textual fidelity.').

omega_variable(
    framing_choice_kernel_versus_institution,
    'Should this story''s kernel be framed as the Gita text itself, or as the layered institutional authority of bhakti lineages that administer and propagate the reading? The obvious framing treats the text as kernel and this reading as one interpretation; a less obvious framing treats the devotional institutions'' claim to represent authentic tradition as itself the kernel under contest.',
    'Compare classification outcomes under each framing: text-as-kernel treats extraction as located in interpretive authority over meaning; institution-as-kernel would treat extraction as located in the devotional institutions'' own claims to legitimate succession and disciple loyalty.',
    'Under the text-as-kernel framing (used here), the primary tension is interpretive authority over the Gita itself. Under an institution-as-kernel framing, additional extraction from disciples/devotees by the bhakti institutions themselves would become the central ε driver, likely raising extractiveness and shifting toward a distinct constraint story about guru-lineage authority rather than textual interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_kernel_versus_institution, conceptual, 'Alternative framings of what counts as the kernel: the text vs. the institutional tradition built on this reading of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the Bhagavad Gita's teaching on dharma and caste' per the epsilon-invariance principle: orthodox_literal_reading (caste duty and righteous violence mandated; high suppression of alternative readings, favors hereditary ritual authority), gandhian_allegorical_reading (battlefield as internal moral struggle; largely agnostic on caste, low extraction, closer to rope/mountain), and this universalist_devotional_reading (bhakti dissolves caste as spiritual barrier; moderate extraction as devotional institutions themselves professionalize). Each reading has a distinct ε because each reading's own account of the standing textual-interpretive arrangement differs: orthodox reading's ε is highest (active enforcement of caste hierarchy via textual warrant), gandhian reading's ε is lowest (metaphorical reading has few material stakeholders), and this reading sits in between (real coordination benefit for excluded populations, real extraction as bhakti institutions consolidate authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
