% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Universalist Devotional Reading of the Bhagavad Gita
 *   domain: religious_studies/textual_hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the universalist devotional reading of
 *   the Bhagavad Gita kernel, in which the text is read as teaching that
 *   devotion (bhakti) offers direct spiritual access to all regardless of
 *   caste, and that true dharma consists of surrender to divine will rather
 *   than performance of birth-prescribed social role. This reading undermines
 *   traditional Brahminical gatekeeping authority and redistributes spiritual
 *   legitimacy to a universal devotee class. It is one of three structurally
 *   distinct readings of the same kernel; the orthodox literal reading and
 *   Gandhian allegorical reading instantiate separate constraints with
 *   different Îµ values and stakeholder structures.
 *
 * KEY AGENTS:
 *   - universal_devotee_class: Primary beneficiary (organized/identity_locked) â gains egalitarian spiritual access
 *   - brahminical_gatekeepers: Primary target (institutional/constrained) â bears loss of ritual monopoly
 *   - bhakti_lineage_teachers: Agenda-setter (organized/mobile) â propagates and administers the doctrinal framework
 *   - religious_studies_scholars: Analytical observer (analytical/analytical) â tracks hermeneutic contest and authority redistribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.58).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.45).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Universalist Devotional Reading of the Bhagavad Gita").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__universalist_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '9362c88f-161c-4186-9b14-714d6a225ebc').
narrative_ontology:cs_kernel_codification('9362c88f-161c-4186-9b14-714d6a225ebc', fixed_text).
narrative_ontology:cs_authority_grounding('9362c88f-161c-4186-9b14-714d6a225ebc', lineage).
narrative_ontology:cs_interpretation_layer_present('9362c88f-161c-4186-9b14-714d6a225ebc').
narrative_ontology:cs_reading_relation('9362c88f-161c-4186-9b14-714d6a225ebc', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('9362c88f-161c-4186-9b14-714d6a225ebc', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('9362c88f-161c-4186-9b14-714d6a225ebc', foundational, divine_surrender_transcends_caste_duty).
narrative_ontology:cs_axiom_status(divine_surrender_transcends_caste_duty, holdable).
narrative_ontology:cs_axiom_grounding('9362c88f-161c-4186-9b14-714d6a225ebc', divine_surrender_transcends_caste_duty, theological).
narrative_ontology:cs_axiom('9362c88f-161c-4186-9b14-714d6a225ebc', foundational, unmediated_divine_access_universal).
narrative_ontology:cs_axiom_status(unmediated_divine_access_universal, holdable).
narrative_ontology:cs_axiom_grounding('9362c88f-161c-4186-9b14-714d6a225ebc', unmediated_divine_access_universal, theological).
narrative_ontology:cs_reference_frame('9362c88f-161c-4186-9b14-714d6a225ebc', devotional_universalism_classical).
narrative_ontology:cs_drift_state('9362c88f-161c-4186-9b14-714d6a225ebc', contemporary_bhakti_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9362c88f-161c-4186-9b14-714d6a225ebc', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeepers).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_as_supreme_path).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, caste_transcendence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain direct scriptural and spiritual legitimacy without dependence on hereditary priestly mediation; participate in devotional communities where birth status is theoretically transcended; their religious identity is constituted by this reading's promise of egalitarian divine access.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    organized, generational, identity_locked, national).

% Bear the loss of exclusive scriptural interpretive authority and ritual mediation monopoly; their hereditary social prestige and economic support from ritual services declines as devotees access divine directly through bhakti; they are structurally displaced by a reading that treats their specialized knowledge as unnecessary for salvation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeepers, payer,
    institutional, generational, constrained, national).

% Propagate the universalist devotional interpretation through teaching, commentary, and institutional formation; they set the hermeneutic agenda that treats caste as irrelevant to spiritual worth; their authority derives from devotion and lineage transmission rather than birth, but they administer the doctrinal framework that organizes the community.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_lineage_teachers, agenda_setter,
    organized, generational, mobile, national).

% Analyze the hermeneutic contest between readings from outside the devotional commitment; they track how the universalist reading functions to redistribute religious authority and how it compares structurally to other egalitarian religious movements.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, religious_studies_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__universalist_devotional_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__universalist_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates spiritual community and practice across caste, gender, and sectarian lines by establishing devotion (bhakti) as a direct, unmediated path to divine realization that does not require birth-based ritual qualification.
% TRANSFER_FUNCTION: Transfers religious authority, scriptural interpretive legitimacy, and social capital from hereditary Brahminical priestly gatekeepers to individual devotees and devotional teacher lineages; moves the locus of spiritual validation from ritual performance to inward surrender.
% ABSENT_VOICES: Orthodox literalist interpreters who read the Gita as mandating caste-based varna-dharma and righteous warrior violence are structurally absent from devotional community discourse; their hermeneutic is treated as preliminary or misguided within this reading's soteriological framework.
% DISAPPEARANCE_RATIONALE: The organizational structures of major bhakti movements, the self-understanding of millions of devotees who ground their spiritual equality in this text, and the authority of non-Brahmin teacher lineages all depend on this reading; its disappearance would force a rearrangement of religious authority back toward ritual-specialist mediation.
% FOUNDING_PROBLEM: The exclusion of lower-caste persons, women, and non-Brahmins from direct spiritual salvation and scriptural access in a religious economy dominated by birth-based priestly gatekeepers controlling ritual and textual mediation.
% FOUNDING_PROBLEM_CORROBORATION: Dalit studies scholars, sociologists of caste and religion, and the historical testimony of low-caste bhakti saints themselves attest to the structural exclusion; corroboration exists outside the beneficiary set.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial transfer of religious authority from hereditary specialists to devotees; suppression (0.45) captures the active hermeneutic and institutional work required to maintain this reading against the orthodox literal alternative; theater_ratio (0.28) acknowledges that while genuine coordination (devotional community) dominates, some performative proof-texting and ritual display serves to reinforce the egalitarian claim against ongoing caste practice. Accessibility_collapse (0.75) is high because once the devotional surrender frame is adopted, the caste-duty framework becomes hermeneutically inaccessible; resistance (0.60) reflects sustained opposition from orthodox authorities.
 *
 * PERSPECTIVAL GAP:
 *   The universal devotee seat experiences this constraint as liberatory coordination (d near beneficiary), while the brahminical gatekeeper seat experiences it as extractive displacement of their traditional authority (d near target). The agenda-setter teachers occupy an intermediate position: they coordinate the community but also depend on the reading's persistence for their own charismatic authority. The engine computes this divergence from structural data; the authored claim (tangled_rope) asserts that both coordination and extraction are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (universal_devotee_class) receive spiritual authority and community inclusion, placing them at low directionality; victims (brahminical_gatekeepers) lose exclusive interpretive and ritual authority, placing them at high directionality. The devotional teachers have moderate directionality because they both coordinate and depend on the framework. No overrides are needed because the structural derivation from beneficiary/victim declarations plus exit options (identity_locked for devotees, constrained for gatekeepers) produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by requiring both coordination (genuine collective-action solution: egalitarian community across castes) and extraction (asymmetric cost-bearing: Brahminical authority loss). Without the coordination component, it would be a snare (pure extraction of status from priests); without the extraction component, it would be a rope (pure coordination). The active enforcement requirement (hermeneutic maintenance against the orthodox reading) is the hinge that makes it tangled rope rather than rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'How does the universalist devotional reading''s classification change if the kernel is instead framed as a fixed military text rather than a theological discourse open to soteriological interpretation?',
    'Comparative analysis across all three readings of the kernel; philological and archaeological assessment of the text''s compositional layers and historical sitz im leben.',
    'If the text is irreducibly a military discourse, the universalist devotional reading''s extraction metric (authority transfer) increases and its coordination function becomes more performative; if the text is genuinely polyphonic, the reading is one valid coordination mechanism among several and its Îµ is moderated by the kernel''s openness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Uncertainty about kernel nature affects reading classification').

omega_variable(
    caste_transcendence_or_reinscription,
    'Does the universalist devotional reading genuinely dissolve caste hierarchy in practice, or does it reinscribe caste distinctions under a veneer of spiritual equality?',
    'Ethnographic study of devotional communities: measure caste endogamy, temple access, leadership composition, and resource distribution within sects claiming universalist bhakti.',
    'If caste is reinscribed, the constraint''s theater_ratio is higher than authored and its coordination function is partly cover for social reproduction; if genuinely dissolved, the extraction from brahminical_gatekeepers is the primary structural effect and the coordination is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_transcendence_or_reinscription, empirical, 'Whether egalitarian theology produces egalitarian practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gita_tr_t25, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(gita_tr_t75, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gita_be_t25, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(gita_be_t75, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gita_su_t25, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(gita_su_t50, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(gita_su_t75, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 75, 0.5).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% The gita_kurukshetra_discourse kernel decomposes into three structurally distinct constraints (readings) per the Îµ-invariance principle. Each reading has a distinct beneficiary/victim structure, Îµ profile, and coordination type. This reading structurally influences its siblings by altering the legitimacy conditions for hermeneutic authority in the shared religious field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
