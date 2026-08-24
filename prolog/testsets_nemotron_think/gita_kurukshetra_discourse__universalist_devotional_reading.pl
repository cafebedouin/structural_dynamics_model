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
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Universalist Devotional Reading of the Kurukshetra Discourse
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The universalist devotional reading of the Bhagavad Gita's Kurukshetra
 *   discourse claims that the text teaches path-independent bhakti accessible
 *   to all regardless of caste, redefining dharma as surrender to divine will
 *   rather than performance of caste-duty. This reading emerged prominently
 *   in the colonial period through reformers like Ram Mohan Roy, Vivekananda,
 *   and Gandhi (though Gandhi's allegorical reading differs), and became
 *   dominant in global Hinduism via ISKCON and neo-Vedanta. It presents
 *   itself as the Gita's own universal teaching (a Mountain claim), but
 *   structurally it functions as a hermeneutic constraint that redistributes
 *   spiritual authority from Brahminical gatekeepers to a universal devotee
 *   class — creating identifiable beneficiaries (historically excluded
 *   groups, modern reform movements) and victims (traditional authorities
 *   whose gatekeeping is delegitimized). The claim/metric independence is
 *   deliberate: the reading claims natural-law status (emerges_naturally:
 *   true) while the metrics describe a historically constructed interpretive
 *   position with modest but real extractive dynamics against orthodox power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.18).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.25).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, mountain).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Universalist Devotional Reading of the Kurukshetra Discourse").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:emerges_naturally(gita_kurukshetra_discourse__universalist_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, 'abf0cbd5-106e-4601-a50f-751e1037075a').
narrative_ontology:cs_kernel_codification('abf0cbd5-106e-4601-a50f-751e1037075a', fixed_text).
narrative_ontology:cs_authority_grounding('abf0cbd5-106e-4601-a50f-751e1037075a', lineage).
narrative_ontology:cs_interpretation_layer_present('abf0cbd5-106e-4601-a50f-751e1037075a').
narrative_ontology:cs_reading_relation('abf0cbd5-106e-4601-a50f-751e1037075a', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('abf0cbd5-106e-4601-a50f-751e1037075a', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('abf0cbd5-106e-4601-a50f-751e1037075a', foundational, universal_access_to_bhakti).
narrative_ontology:cs_axiom_status(universal_access_to_bhakti, holdable).
narrative_ontology:cs_axiom_grounding('abf0cbd5-106e-4601-a50f-751e1037075a', universal_access_to_bhakti, deontological).
narrative_ontology:cs_axiom('abf0cbd5-106e-4601-a50f-751e1037075a', foundational, dharma_as_devotional_surrender).
narrative_ontology:cs_axiom_status(dharma_as_devotional_surrender, holdable).
narrative_ontology:cs_axiom_grounding('abf0cbd5-106e-4601-a50f-751e1037075a', dharma_as_devotional_surrender, deontological).
narrative_ontology:cs_reference_frame('abf0cbd5-106e-4601-a50f-751e1037075a', universal_devotional_revelation).
narrative_ontology:cs_drift_state('abf0cbd5-106e-4601-a50f-751e1037075a', contemporary_global_hinduism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('abf0cbd5-106e-4601-a50f-751e1037075a', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, historically_excluded_groups).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeepers).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, historically_excluded_groups).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, spiritual_equality_of_all_beings).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_as_independent_path_to_liberation).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, divine_will_as_sole_dharma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practitioners who access bhakti directly without caste mediation; gain spiritual authorization and community belonging through the universalist reading. Their devotion is validated by the text itself rather than by priestly certification.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    moderate, biographical, mobile, global).

% Dalit, Adivasi, and non-dvija communities historically barred from Vedic study and temple ritual. The reading grants them textual warrant for full spiritual participation, but lived caste oppression persists — they pay the cost of the gap between hermeneutic inclusion and social reality.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, historically_excluded_groups, beneficiary,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, historically_excluded_groups, payer).

% Traditional priestly and scholarly authorities whose interpretive monopoly and ritual gatekeeping are undermined by the claim that bhakti requires no caste qualification. They lose control over spiritual access and the material benefits (dakshina, temple authority, pedagogical lineage) that accompany it.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeepers, payer,
    institutional, generational, constrained, national).

% Communities and institutions committed to varnasrama-dharma as the Gita's teaching. Their identity fuses with the orthodox reading; the universalist reading threatens not just their authority but their self-understanding as custodians of the tradition. Exit would require abandoning their constitutive framework.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_traditionalists, payer,
    organized, biographical, identity_locked, national).

% Neo-Vedanta movements (Ramakrishna Mission, Arya Samaj, ISKCON), colonial-era reformers, and contemporary activists who advance the universalist reading as the Gita's true message. They build institutions, publish translations, and lobby for legal recognition of caste-egalitarian Hinduism. They can shift between interpretive frames strategically.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, modern_hindu_reformers, agenda_setter,
    institutional, biographical, arbitrage, global).

% Hold the allegorical reading (Kurukshetra as internal struggle). They share the universalist reading's anti-caste and nonviolent commitments but differ on the text's literal sense. They neither gain nor lose directly from the universalist reading's success but observe its interpretive moves.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_interpreters, observer,
    organized, generational, mobile, global).

% Academic historians, philologists, and scholars of religion who study the Gita's reception history. They analyze the universalist reading as a historical construction without endorsing its theological claims. Their authority derives from methodological rigor, not devotional commitment.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, secular_scholars, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal spiritual path accessible to all, coordinating diverse practitioners around shared devotional practice without caste mediation; solves the problem of how a text rooted in a hierarchical society can authorize egalitarian spirituality.
% TRANSFER_FUNCTION: Transfers spiritual authority from caste-based gatekeepers to the direct divine-devotee relationship; moves interpretive power from Brahminical tradition to universal access; transfers the warrant for spiritual legitimacy from birth to devotion.
% ABSENT_VOICES: Traditional Sanskrit pandits committed to varnasrama-dharma as the Gita's explicit teaching; indigenous ritual specialists whose authority derives from caste-exclusive transmission lineages; these voices are excluded from modern academic and reformist conversations but remain authoritative in orthodox institutions.
% DISAPPEARANCE_RATIONALE: The universalist reading is the primary hermeneutic resource for caste-egalitarian spirituality in the Gita tradition; its loss would remove the text's main warrant for spiritual equality and return interpretive authority entirely to varnasrama-dharma frameworks.
% FOUNDING_PROBLEM: The problem of spiritual exclusion: how to make liberation accessible to those barred by caste from Vedic ritual, Upanishadic study, and the traditional guru-shishya parampara.
% FOUNDING_PROBLEM_CORROBORATION: B.R. Ambedkar (engaged the Gita's caste politics from outside the Hindu fold, rejecting its authority but confirming the exclusion problem); Jotirao Phule (19th-century anti-caste reformer who identified Brahminical texts as instruments of oppression); contemporary Dalit theologians (e.g., James Massey, S.J. Samartha) who read the Gita against the grain; colonial-era missionary scholars who documented caste-based scriptural exclusion — all from outside the beneficiary set of the universalist reading.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, ExtMetricName, E),
    domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gita_kurukshetra_discourse__universalist_devotional_reading),
    narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the reading's primary operation is interpretive inclusion, not material extraction; the 'cost' to traditionalists is loss of interpretive monopoly, not direct resource transfer. Suppression is low (0.25) because the reading does not actively silence the orthodox reading — both persist in parallel institutions. Theater ratio is low (0.12) because the devotional practice it authorizes is genuine and functionally central to millions. Accessibility collapse is moderate (0.45) because alternative readings (orthodox, allegorical) remain live and structurally viable. Resistance is high (0.68) because orthodox institutions actively contest the reading's textual warrant and social implications. The measurement series shows the reading's growing institutional presence from 1800–1950, stabilizing thereafter.
 *
 * PERSPECTIVAL GAP:
 *   From the universalist seat, the constraint is a Mountain — the text's own teaching, naturally emerging, liberating all. From the orthodox seat, the same constraint is a Snare — a modernist imposition that extracts their tradition's coherence and authority while claiming to be the text's true meaning. From the historically excluded seat, it is a Rope — a genuine coordination mechanism that solves their spiritual exclusion, though the coordination is incomplete because social caste persists. The engine computes this divergence from the declared power, exit, and beneficiary/victim structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Universal devotee class and historically excluded groups are structural beneficiaries (d near 0.0) — they gain spiritual access and textual warrant without paying the cost of caste mediation. Brahminical gatekeepers and orthodox traditionalists are structural payers (d near 1.0) — they lose authority, material support, and identity-coherence. Modern Hindu reformers are agenda-setters with arbitrage-grade exit (they can shift frames). Gandhian interpreters and secular scholars are observers with analytical exit. The identity_locked exit of orthodox traditionalists is critical: their self-concept is constituted through the varnasrama reading, making exit structurally unavailable even though no physical barrier exists.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (spiritual exclusion by caste) remains live — caste-based temple entry restrictions, priestly monopolies, and ritual exclusion persist in 2025. The arrangement has not outlived its function; rather, its function remains unrealized in practice. Mandatrophy is not resolved. The reading continues to do coordinative work for anti-caste spirituality, but the gap between hermeneutic inclusion and social reality creates a persistent tension that the reading itself cannot resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_universalism,
    'Is the universalist devotional reading a genuine natural law of spirit (the Gita''s own eternal teaching) or a modern construction that selectively reads the text to serve anti-caste and universalist commitments?',
    'Philological analysis of the Sanskrit text''s semantic range for ''bhakti'', ''varna'', ''svadharma'' across commentarial traditions; historical tracing of the universalist reading''s emergence in colonial modernity vs. pre-colonial commentaries.',
    'If natural law, the reading is a Mountain with zero extractiveness; if constructed, it is a False Summit Mountain (FSM candidate) whose beneficiaries (reformers, excluded groups) gain interpretive authority through a claim of textual inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_universalism, conceptual, 'FSM omega: whether the Mountain claim of natural emergence masks a historically constructed universalism that benefits identifiable agents.').

omega_variable(
    textual_warrant_for_universalism,
    'Does the Sanskrit text of the Gita, read without modern presuppositions, actually support caste-independent bhakti as its primary teaching, or does the universalist reading require selective emphasis on verses 9.29, 9.32, 18.66 while downplaying 18.41-47 (varna-dharma)?',
    'Comparative commentarial analysis: Shankara (Advaita, hierarchical), Ramanuja (Vishishtadvaita, qualified access), Madhva (Dvaita, strict hierarchy), vs. modern commentaries (Tilak, Aurobindo, Prabhupada). Assess whether universalist reading is philologically defensible or hermeneutically selective.',
    'If the text''s plain sense supports hierarchy, the universalist reading''s extractiveness toward orthodox traditionalists is higher (it imposes an alien frame); if the text genuinely supports universalism, the orthodox reading is the extractive imposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_warrant_for_universalism, empirical, 'Whether the universalist reading''s textual warrant is robust or requires hermeneutic violence against the Sanskrit.').

omega_variable(
    caste_persistence_despite_universalist_reading,
    'Why does caste-based spiritual and social exclusion persist in Hindu societies despite two centuries of universalist Gita readings dominating reformist and global discourse?',
    'Sociological study of the gap between textual hermeneutics and lived caste structure; analysis of whether the universalist reading functions as symbolic inclusion without material redistribution of ritual authority.',
    'If the reading fails to dismantle caste in practice, its claimed coordination function is partially theatrical — the theater_ratio may be understated. The beneficiary structure (historically excluded groups as secondary payers) reflects this gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_persistence_despite_universalist_reading, empirical, 'The persistence of caste despite the reading''s universalist claim — does the reading actually coordinate egalitarian practice or merely authorize a discourse of equality?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(gita_tr_t1850, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(gita_tr_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(gita_tr_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(gita_tr_t2025, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(gita_be_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement(gita_be_t1850, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(gita_be_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(gita_be_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(gita_be_t2025, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(gita_su_t1850, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1850, 0.15).
narrative_ontology:measurement(gita_su_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(gita_su_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(gita_su_t2025, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__universalist_devotional_reading, 0.08).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the Gita Kurukshetra discourse constraint family. The kernel 'gita_kurukshetra_discourse' decomposes into three readings with divergent ε: orthodox_literal (low ε, Mountain-claimed), gandhian_allegorical (low ε, Scaffold-claimed), universalist_devotional (moderate ε, Mountain-claimed but FSM candidate). The universalist reading's ε (0.18) exceeds the orthodox reading's because it actively delegitimizes the traditional authority structure, creating extraction toward gatekeepers. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__universalist_devotional_reading, organized, 0.85).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__universalist_devotional_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
