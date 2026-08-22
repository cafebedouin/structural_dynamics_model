% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Reformist Spiritual Reading of Vedic Corpus — No Prescriptive Social Content
 *   domain: religious_studies/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   This constraint story instantiates the reformist_spiritual_reading of the
 *   vedic_corpus_social_prescription kernel. The reading holds that Vedic
 *   texts articulate spiritual unity (ekam sat vipra bahudha vadanti) and
 *   metaphorical cosmology (purusha sukta as cosmogonic myth, not social
 *   blueprint) with zero prescriptive social content. Varna references are
 *   read as either symbolic (qualities of consciousness) or as later
 *   corruptions interpolated into the text. The constraint operates as a
 *   rope: it coordinates spiritual practice across diverse traditions without
 *   extracting from practitioners, suppressing alternatives, or mandating
 *   social hierarchy. Beneficiaries are reformist practitioners and spiritual
 *   unity traditions who gain a non-hierarchical hermeneutic. No victim set
 *   exists — the reading does not extract from orthodox adherents, though it
 *   contests their interpretation. The kernel context: this is one of three
 *   declared readings. The colonial_orientalist_reading treats the corpus as
 *   unified 'Hindu law' for administrative codification
 *   (authority_grounding=extraction). The orthodox_varna_reading treats varna
 *   as divinely mandated cosmic order (authority_grounding=lineage). This
 *   reading stands apart by denying prescriptive force entirely.
 *
 * KEY AGENTS:
 *   - reformist_practitioners: Primary beneficiaries (organized/mobile) — gain non-hierarchical spiritual hermeneutic
 *   - spiritual_unity_traditions: Beneficiaries (organized/biographical) — traditions emphasizing advaita/non-duality gain textual support
 *   - orthodox_varna_adherents: Excluded voice (institutional/identity_locked) — would contest the reading's denial of prescriptive varna
 *   - colonial_administrators: Excluded voice (institutional/arbitrage) — relied on corpus as codifiable law; reformist reading undermines that project
 *   - hermeneutic_scholars: Observers (analytical/analytical) — analyze the contest without structural stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of Vedic Corpus — No Prescriptive Social Content").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '2675c951-07e7-47dc-b284-84bcc9a1b214').
narrative_ontology:cs_kernel_codification('2675c951-07e7-47dc-b284-84bcc9a1b214', fixed_text).
narrative_ontology:cs_authority_grounding('2675c951-07e7-47dc-b284-84bcc9a1b214', lineage).
narrative_ontology:cs_interpretation_layer_present('2675c951-07e7-47dc-b284-84bcc9a1b214').
narrative_ontology:cs_reading_relation('2675c951-07e7-47dc-b284-84bcc9a1b214', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('2675c951-07e7-47dc-b284-84bcc9a1b214', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('2675c951-07e7-47dc-b284-84bcc9a1b214', foundational, vedic_texts_no_prescriptive_social_content).
narrative_ontology:cs_axiom_status(vedic_texts_no_prescriptive_social_content, holdable).
narrative_ontology:cs_axiom_grounding('2675c951-07e7-47dc-b284-84bcc9a1b214', vedic_texts_no_prescriptive_social_content, empirically_contingent).
narrative_ontology:cs_axiom('2675c951-07e7-47dc-b284-84bcc9a1b214', foundational, spiritual_unity_as_core_vedic_teaching).
narrative_ontology:cs_axiom_status(spiritual_unity_as_core_vedic_teaching, holdable).
narrative_ontology:cs_axiom_grounding('2675c951-07e7-47dc-b284-84bcc9a1b214', spiritual_unity_as_core_vedic_teaching, deontological).
narrative_ontology:cs_reference_frame('2675c951-07e7-47dc-b284-84bcc9a1b214', reformist_non_hierarchical_hermeneutic).
narrative_ontology:cs_drift_state('2675c951-07e7-47dc-b284-84bcc9a1b214', contemporary_scholarly_reception, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2675c951-07e7-47dc-b284-84bcc9a1b214', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_unity_traditions).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_spiritual_unity).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, metaphorical_cosmology_interpretation).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, non_prescriptive_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practitioners of reformist Vedic traditions (Arya Samaj, Brahmo Samaj, neo-Vedanta organizations, contemporary non-sectarian Vedic study groups). They adopt the reformist hermeneutic to coordinate spiritual practice across caste, gender, and sectarian lines. The reading gives them textual authority for non-hierarchical spirituality. Exit is mobile — they can shift to other spiritual frameworks without material cost. They collect no rents from the constraint; they gain a coordination tool.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_practitioners, beneficiary,
    organized, biographical, mobile, global).

% Traditions emphasizing advaita/non-duality (Ramakrishna Mission, Chinmaya Mission, Advaita Vedanta monasteries, contemporary yoga lineages). They gain textual support for the unity-of-all-beings doctrine from the reformist reading. Their institutional forms are committed to this hermeneutic over generations — exit is constrained by institutional identity and lineage continuity. They benefit from the constraint's coordination of cross-tradition spiritual discourse.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_unity_traditions, beneficiary,
    organized, generational, constrained, global).

% Orthodox institutions (traditional mathas, Smarta orthodoxy, varna-ashrama-dharma proponents) that hold the Vedic corpus as literally prescribing varna hierarchy. They are not governed by the reformist reading — they reject its hermeneutic. But they are excluded from the coordination space this reading creates (inter-tradition spiritual dialogue on non-hierarchical terms). Their identity is fused with the varna hierarchy; exit would dissolve their self-concept and institutional legitimacy. They would object to the reformist reading's denial of prescriptive varna if present in the conversation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_adherents, excluded,
    institutional, generational, identity_locked, national).

% Historical colonial administrators and their institutional successors (Anglo-Hindu law architects, personal law boards) who treated the Vedic/Dharmashastra corpus as a unified codifiable 'Hindu law.' The reformist reading undermines their project by denying the corpus has prescriptive legal content. They are excluded from this constraint's coordination space because their project requires a different reading. Exit is arbitrage-grade — they can shift to other textual bases for codification (customary law, legislative enactment). They would contest the reformist reading's hermeneutic as destabilizing their legal framework.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_administrators, excluded,
    institutional, biographical, arbitrage, global).

% Philologists, historians of religion, Indologists, and comparative hermeneutics scholars who analyze the contest over Vedic interpretation without structural stake in any reading. They provide the empirical and conceptual tools (textual criticism, reception history, comparative methodology) that inform the kernel's dispute. Their seat is analytical — they neither collect nor pay, but their work shapes the legitimacy conditions for all three readings.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, hermeneutic_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__reformist_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables spiritual practitioners across diverse traditions to engage shared Vedic texts without hierarchical friction — coordinates practice compatibility by removing prescriptive social content from the hermeneutic.
% TRANSFER_FUNCTION: No material transfer. The arrangement moves interpretive authority from hierarchical institutions to individual practitioners and reformist communities, but this is a shift in discursive legitimacy, not resource extraction.
% ABSENT_VOICES: Orthodox varna adherents (traditional mathas, Smarta orthodoxy) and colonial-era legal architects (Anglo-Hindu law framers) are structurally excluded from the reformist coordination space. The former would assert textual prescriptivity of varna; the latter would assert the corpus's codifiability as law. Both are absent because their readings instantiate different constraints.
% DISAPPEARANCE_RATIONALE: If the reformist reading vanished overnight, the coordination space it enables — cross-tradition spiritual dialogue on non-hierarchical terms — would collapse. Reformist practitioners would lose their primary textual authority for non-hierarchical spirituality. Orthodox and colonial readings would become the only live hermeneutics in institutional spaces, restructuring the discursive field.
% FOUNDING_PROBLEM: Vedic texts are weaponized to prescribe and legitimize caste hierarchy (varna), contradicting their own articulation of spiritual unity (ekam sat). The arrangement was built to reclaim the corpus for non-hierarchical spirituality.
% FOUNDING_PROBLEM_CORROBORATION: Philological scholarship on textual layers (e.g., Witzel, Jamison, Olivelle) corroborates that varna references in the Rigveda are sparse, metaphorical, and cosmogonic — not prescriptive social law. Independent textual critics outside the reformist beneficiary set confirm the prescriptive hierarchy appears in later Dharmashastra interpolation. Living traditions of non-hierarchical Vedic practice (certain sannyasa lineages, bhakti movements) corroborate the founding problem's continuity.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.12) because the reading imposes no mandatory transfers, tithes, or resource allocations. The coordination function is spiritual practice compatibility — practitioners of different traditions can engage shared texts without hierarchical friction. Suppression is minimal (0.08): the reading does not ban alternative readings; it contests them discursively. Theater_ratio is low (0.15): the reading's institutional forms (e.g., Arya Samaj, Brahmo Samaj) perform the hermeneutic they teach, with modest ritual overhead. Accessibility_collapse is low (0.25): the metaphorical reading leaves the social field open — practitioners can adopt any social arrangement compatible with spiritual unity. Resistance is moderate (0.4): the reading faces contested status from orthodox institutions that claim exclusive interpretive authority, but this resistance is discursive, not coercive. The claimed_type=rope is independent of these metrics — the engine will compute per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist practitioners and spiritual unity traditions are beneficiaries (d near 0.0): they gain a hermeneutic that coordinates practice without extracting from them. Orthodox varna adherents are not victims — they bear no cost from this reading's operation except discursive contestation. Their exit_options=identity_locked reflects that their self-concept is fused with the varna hierarchy, but the reformist reading does not enforce anything upon them. Colonial administrators are excluded: their codification project loses textual authority if this reading prevails, but they are not governed by this constraint. Directionality is symmetric for all governed parties because the constraint is opt-in spiritual coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist reading was founded to solve the problem of textual authority being used to legitimize social hierarchy (founding_problem: 'Vedic texts are weaponized to prescribe caste hierarchy'). That problem remains contested (founding_problem_status=contested): orthodox institutions maintain the hierarchy is textual; reformists maintain it is interpolation. Corroboration comes from outside the beneficiary set: philological scholarship on textual layers (independent textual critics), and the lived reality of traditions that have practiced non-hierarchical Vedic spirituality for centuries (e.g., certain sannyasa lineages). The constraint is not mandatrophic — its founding problem persists, and the arrangement remains a live coordination mechanism, not an atrophied shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the reformist_spiritual_reading structurally foreclose the orthodox_varna_reading within a single commitment framework, or do they coexist as live positions held by different parties?',
    'Examine whether a single institutional or communal body can simultaneously maintain both readings without internal contradiction. If the reformist reading''s core premise (no prescriptive social content) logically eliminates the orthodox reading''s core premise (divinely mandated hierarchy), foreclosure obtains. If different factions within the same tradition hold each reading without the framework resolving it, coexistence obtains.',
    'If forecloses: the reformist reading displaces the orthodox reading as a live option within any single Vedic commitment framework. If coexists_with: both remain live across different factions, and the kernel remains contested. If influences: the reformist reading creates structural pressure (legitimacy erosion for varna-based authority) without logically eliminating the orthodox reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between reformist and orthodox readings of the Vedic corpus kernel').

omega_variable(
    colonial_reading_influence,
    'Does the reformist_spiritual_reading create downstream structural pressure on the colonial_orientalist_reading by undermining its textual authority claims?',
    'Trace whether the reformist reading''s hermeneutic (metaphorical cosmology, no prescriptive law) weakens the colonial reading''s claim that the corpus constitutes a codifiable ''Hindu law'' system. If the reformist reading gains institutional uptake (legal, academic, religious), does the colonial reading''s administrative utility degrade?',
    'If influences: the reformist reading erodes the colonial reading''s operational legitimacy without foreclosing it (colonial administrators could still impose a codification). If coexists_with: both persist as separate institutional projects. The colonial reading''s authority_grounding=extraction makes it resilient to hermeneutic challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonial_reading_influence, empirical, 'Downstream pressure of reformist hermeneutic on colonial codification project').

omega_variable(
    beneficiary_verification,
    'Are the declared beneficiaries (reformist_practitioners, spiritual_unity_traditions) genuine coordination beneficiaries, or does the reading serve as cover for a different beneficiary structure?',
    'Audit whether the reformist reading''s institutional instantiations (e.g., Arya Samaj, Brahmo Samaj, modern neo-Vedanta organizations) collect rents, control resources, or exclude dissenters in ways that mirror extractive patterns. If the coordination function (spiritual practice compatibility) operates without asymmetric extraction, the rope claim holds. If extraction emerges, reclassify toward tangled_rope.',
    'If genuine coordination: claimed_type=rope is structurally true. If cover for extraction: the constraint is a false summit or tangled_rope, and the declared low extractiveness is an artifact of the reading''s self-description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_verification, empirical, 'Verification that the reformist reading''s low extractiveness is not a false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_reformist_spiritual_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_tr_t0, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_tr_t50, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_tr_t50, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_tr_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 100, 0.16).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_tr_t100, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_tr_t150, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 150, 0.15).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(vedic_reformist_spiritual_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_be_t0, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_be_t50, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 50, 0.14).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_be_t50, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_be_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 100, 0.13).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_be_t100, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_be_t150, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 150, 0.12).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedic_reformist_spiritual_su_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_su_t0, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_su_t50, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 50, 0.09).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_su_t50, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_su_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 100, 0.08).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_su_t100, observed).
narrative_ontology:measurement(vedic_reformist_spiritual_su_t150, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 150, 0.08).
narrative_ontology:measurement_basis(vedic_reformist_spiritual_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the vedic_corpus_social_prescription kernel. This reading (reformist_spiritual) has epsilon ~0.12 (rope). The orthodox_varna_reading has substantially higher epsilon (tangled_rope or snare — varna hierarchy extracts from lower varnas). The colonial_orientalist_reading has high epsilon (snare — codification extracts from colonized subjects via imposed legal categories). The three readings share the same textual corpus but instantiate structurally distinct constraints with different beneficiary/victim structures. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
