% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew Marketplace-Pidgin Definition of Linguistic Life
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This constraint story models the marketplace_pidgin_reading of the
 *   hebrew_linguistic_life kernel: the standing arrangement in which Hebrew
 *   linguistic vitality is defined by continuous practical, inter-communal
 *   coordination function â specifically the modified Medieval Hebrew
 *   pidgin used in Jerusalem markets before 1880 â rather than by sacred
 *   transmission or native mother-tongue acquisition. The kernel is contested
 *   by two sibling readings: liturgical_preservation_reading (vitality equals
 *   sacred textual continuity) and native_generational_reading (vitality
 *   equals mother-tongue acquisition). This reading treats the Jerusalem
 *   marketplace as the locus of linguistic life, validating functionalist
 *   sociolinguistics and post-Zionist continuity historiography while
 *   asymmetrically marginalizing institutions built on rupture-and-revival or
 *   sacred-exclusivity narratives.
 *
 * KEY AGENTS:
 *   - functionalist_sociolinguists (institutional/arbitrage): set the definitional agenda through journals and conferences
 *   - continuity_historians (moderate/mobile): benefit from historiographical validation of non-revival continuity
 *   - zionist_revival_foundations (institutional/constrained): pay through erosion of their foundational myth
 *   - rabbinic_academies (organized/constrained): pay through loss of sacred monopoly on Hebrew legitimacy
 *   - diaspora_vernacular_communities (powerless/trapped): excluded from the definitional debate despite possessing relevant historical practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.45).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.55).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew Marketplace-Pidgin Definition of Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'd2e0c870-d230-420b-b4ab-16253927f592').
narrative_ontology:cs_kernel_codification('d2e0c870-d230-420b-b4ab-16253927f592', distributed).
narrative_ontology:cs_authority_grounding('d2e0c870-d230-420b-b4ab-16253927f592', distributed).
narrative_ontology:cs_reading_relation('d2e0c870-d230-420b-b4ab-16253927f592', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2e0c870-d230-420b-b4ab-16253927f592', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('d2e0c870-d230-420b-b4ab-16253927f592', foundational, practical_coordination_defines_linguistic_life).
narrative_ontology:cs_axiom_status(practical_coordination_defines_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('d2e0c870-d230-420b-b4ab-16253927f592', practical_coordination_defines_linguistic_life, empirically_contingent).
narrative_ontology:cs_axiom('d2e0c870-d230-420b-b4ab-16253927f592', foundational, native_speaker_status_irrelevant_to_vitality).
narrative_ontology:cs_axiom_status(native_speaker_status_irrelevant_to_vitality, holdable).
narrative_ontology:cs_axiom_grounding('d2e0c870-d230-420b-b4ab-16253927f592', native_speaker_status_irrelevant_to_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('d2e0c870-d230-420b-b4ab-16253927f592', marketplace_practice_as_vitality_t0).
narrative_ontology:cs_drift_state('d2e0c870-d230-420b-b4ab-16253927f592', native_revival_narrative_dominance_t1, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2e0c870-d230-420b-b4ab-16253927f592', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, functionalist_sociolinguists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, continuity_historians).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, zionist_revival_foundations).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, rabbinic_academies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control editorial boards, peer-review networks, and graduate curricula that enshrine practical inter-communal coordination as the criterion for language vitality. They set the definitional agenda and collect paradigm rents through citations, conference prestige, and funding for functionalist research programs.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, functionalist_sociolinguists, agenda_setter,
    institutional, generational, arbitrage, global).

% Draw academic capital and institutional support from historiographical narratives that treat Hebrew as continuously alive in pre-1880 Jerusalem markets. Their work is validated by the functionalist definition, distinguishing them from rupture-and-revival scholarship.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, continuity_historians, beneficiary,
    moderate, biographical, mobile, national).

% Administer museums, language-education programs, and commemorative institutions built on the narrative of Hebrew's death and modern rebirth. The marketplace-pidgin reading erodes their foundational myth and threatens the stream of state and philanthropic funding tied to revival heroism.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, zionist_revival_foundations, payer,
    institutional, generational, constrained, national).

% Maintain that Hebrew's uninterrupted life is anchored in sacred textual study and liturgical transmission. The functionalist reading dismisses sacred function as irrelevant to vitality, shrinking their epistemic authority and the cultural prestige of Torah-study institutions.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, rabbinic_academies, payer,
    organized, generational, constrained, national).

% Historical communities whose Hebrew vernaculars do not map neatly onto either the Jerusalem marketplace pidgin or the liturgical canon. Their lived practice is absent from the archival record driving the functionalist claim, and they possess no voice in the scholarly or institutional contest over definitions.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, diaspora_vernacular_communities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, functionalist_sociolinguists).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared research paradigm among sociolinguists and historians who must classify vitality in situations of diglossia and multilingualism where native-speaker populations are absent but communicative function persists.
% TRANSFER_FUNCTION: Moves academic legitimacy, publishing priority, curriculum space, and historiographical authority from institutions invested in rupture-revival or sacred-continuity narratives to scholars and programs validating continuous practical usage.
% ABSENT_VOICES: Traditional diaspora vernacular communities whose Hebrew practices fall outside both marketplace-pidgin and liturgical-transmission boxes are excluded; they would testify to other continuous vernacular forms but are absent from archives and conferences.
% DISAPPEARANCE_RATIONALE: If the functionalist definitional constraint vanished overnight, sociolinguistic curricula would reorganize around native-speaker or sacred-text criteria, funding would shift back toward revival institutions and rabbinic academies, and the historiography of Hebrew would revert to rupture-and-revival or liturgical-continuity frameworks.
% FOUNDING_PROBLEM: How to define and measure language vitality in contexts of diglossia, religious literacy, and multilingual markets where a language is widely used for practical coordination but lacks a community of mother-tongue acquirers.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO language-vitality frameworks and general sociolinguistic literature outside the Hebrew debate corroborate the problem as live; however, its specific application to pre-1880 Jerusalem is contested, with Zionist historians and rabbinic authorities disputing the premise that a marketplace pidgin constitutes genuine linguistic life.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).
:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because the functionalist definition genuinely coordinates research on multilingual marketplaces and diglossia, but it also diverts legitimacy and funding from rival paradigms. Suppression (0.55) reflects active marginalization of revival and liturgical frameworks in mainstream sociolinguistic venues â not censorship, but paradigm gatekeeping through peer review and hiring. Theater_ratio (0.25) captures performative citation of 'practical coordination' in work that never examines actual market data. Accessibility_collapse (0.40) is moderate because alternative definitions remain available in theology and nationalist education. Resistance (0.60) is substantial because Zionist and religious scholars actively contest the reading. Measurements trace the gradual rise of the functionalist paradigm from 1980â2020 on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (functionalist sociolinguists) experiences the constraint as genuine coordination â a shared standard enabling cumulative research and interdisciplinary dialogue. The payer seats (revival institutions, rabbinic academies) experience the same constraint as epistemic extraction that renders their core missions irrelevant to the definition of linguistic life. The engine computes this divergence from structural data: agenda_setters possess arbitrage-grade exit (can shift paradigms or move to adjacent disciplines), while payers are constrained by institutional identity-lock and sunk costs in physical infrastructure and sacred curricula.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to low directionality: functionalist sociolinguists and continuity historians are subsidized by the constraint's operation through prestige, citations, and funding. Victims map to high directionality: zionist revival foundations and rabbinic academies are targeted for extraction of legitimacy and institutional support. The excluded diaspora communities sit at high directionality by default (powerless, trapped) but are not actively governed by the constraint â their exclusion is structural omission rather than direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â how to classify language vitality under diglossia without native speakers â remains live and is corroborated by external frameworks such as UNESCO, preventing automatic mandatrophy. If the reading were to persist solely through citation ritual after empirical research on Jerusalem pidgins stalled, it would degrade toward piton. The temporal measurements show theater_ratio flattening after an initial rise, suggesting the coordination function is maturing rather than atrophying, though continued monitoring is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the marketplace_pidgin_reading of the hebrew_linguistic_life kernel; how would classification shift if the liturgical_preservation_reading or native_generational_reading were adopted instead?',
    'Compare the compiled constraint stories for all three readings; the structural delta is in beneficiary sets, victim sets, coordination function, and directionality of all seats.',
    'Adopting a sibling reading would shift beneficiaries to liturgical authorities or native-speaker institutions, redirect the transfer function toward sacred transmission or generational education, and reclassify the directionality of Zionist revival foundations from payer to beneficiary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest for Hebrew linguistic life').

omega_variable(
    pidgin_empirical_robustness,
    'Is the historical evidence for a continuous Hebrew marketplace pidgin in pre-1880 Jerusalem robust enough to support the vitality claim, or does it rely on sparse, ambiguous sources?',
    'Archival philological review of merchant ledgers, travelogues, and court records from 17thâ19th century Jerusalem.',
    'A weak empirical basis would raise theater_ratio and lower the genuine coordination score, pushing the constraint toward snare or piton by revealing the functionalist definition as a cover story for paradigm extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_empirical_robustness, empirical, 'Empirical basis of the Jerusalem marketplace pidgin claim').

omega_variable(
    academic_suppression_mechanism,
    'Is the marginalization of revival and liturgical readings structural (peer-review gatekeeping, funding bias, hiring discrimination) or internalized (the assumption that functionalism is the only scientific approach)?',
    'Track citation patterns, editorial board composition, and tenure outcomes over time; if gatekeeping persists despite explicit pluralism rhetoric, suppression is structural.',
    'If internalized, the constraint''s effective suppression exceeds structural measures because the paradigm reproduces itself even without explicit enforcement â the targets carry the suppression with them into every venue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_suppression_mechanism, conceptual, 'Structural vs internalized suppression in academic paradigm enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t10, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(hebr_tr_t30, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hebr_be_t10, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(hebr_be_t30, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hebr_su_t10, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(hebr_su_t30, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, native_generational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the hebrew_linguistic_life kernel, decomposed per the epsilon-invariance principle because each reading assigns a different epsilon, beneficiary structure, and coordination function to the same colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
