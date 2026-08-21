% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Doctrine
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint represents the 'ontological fusion' reading of the
 *   simultaneous veneration kernel, specifically the honji-suijaku (original
 *   ground and trace manifestation) theory. It posits that kami (indigenous
 *   Japanese deities) are merely local manifestations of universal buddhas
 *   and bodhisattvas. This doctrine, promoted by the Buddhist institutional
 *   hierarchy, served to integrate kami worship into a Buddhist metaphysical
 *   framework, thereby asserting Buddhist interpretive supremacy and
 *   consolidating its power. The constraint is claimed as a 'tangled_rope'
 *   because it provided a coordination function (religious unity) but also
 *   involved significant, asymmetric extraction from indigenous kami
 *   traditions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.78).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.85).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Doctrine").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '9ca613b9-abb0-438b-881c-4f33f56225a3').
narrative_ontology:cs_kernel_codification('9ca613b9-abb0-438b-881c-4f33f56225a3', formalized).
narrative_ontology:cs_authority_grounding('9ca613b9-abb0-438b-881c-4f33f56225a3', lineage).
narrative_ontology:cs_interpretation_layer_present('9ca613b9-abb0-438b-881c-4f33f56225a3').
narrative_ontology:cs_reading_relation('9ca613b9-abb0-438b-881c-4f33f56225a3', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ca613b9-abb0-438b-881c-4f33f56225a3', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('9ca613b9-abb0-438b-881c-4f33f56225a3', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('9ca613b9-abb0-438b-881c-4f33f56225a3', kami_are_buddha_manifestations, theological).
narrative_ontology:cs_axiom('9ca613b9-abb0-438b-881c-4f33f56225a3', foundational, buddhist_interpretive_supremacy).
narrative_ontology:cs_axiom_status(buddhist_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('9ca613b9-abb0-438b-881c-4f33f56225a3', buddhist_interpretive_supremacy, conventional).
narrative_ontology:cs_reference_frame('9ca613b9-abb0-438b-881c-4f33f56225a3', unified_buddhist_shinto_cosmology).
narrative_ontology:cs_drift_state('9ca613b9-abb0-438b-881c-4f33f56225a3', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('9ca613b9-abb0-438b-881c-4f33f56225a3', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, imperial_court_aristocracy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, local_kami_cults).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, shinto_priests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgated and enforced the honji-suijaku theory, integrating kami into the Buddhist pantheon as manifestations of buddhas and bodhisattvas. This secured their interpretive monopoly and expanded their influence and patronage across Japan.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Represents the independent spiritual authority and distinct identity of indigenous kami traditions. Its autonomy was subsumed under the Buddhist framework, losing its unique metaphysical standing and becoming secondary to Buddhist deities.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy, payer,
    powerless, generational, identity_locked, local).

% Local communities and practitioners who venerated kami. They were compelled to accept the Buddhist interpretation of their deities, often leading to the syncretic merging of shrines and temples, and the reinterpretation of local myths through a Buddhist lens.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, local_kami_cults, payer,
    powerless, biographical, constrained, local).

% Benefited from the unified religious framework provided by honji-suijaku, which helped to stabilize the political order by integrating diverse local religious practices under a coherent, centrally sanctioned doctrine. They patronized both Buddhist temples and kami shrines, often seeing them as complementary.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, imperial_court_aristocracy, beneficiary,
    powerful, generational, mobile, national).

% Hereditary guardians of kami shrines who were often forced to adopt Buddhist rituals and interpretations, or even become Buddhist monks, to maintain their positions and patronage. Their distinct religious identity and practices were diluted or subsumed.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shinto_priests, payer,
    moderate, biographical, constrained, local).

% Analyze the historical development and metaphysical claims of honji-suijaku theory, examining its role in Japanese religious history and its implications for understanding religious syncretism and power dynamics.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To integrate foreign Buddhism with indigenous kami worship, resolving perceived conflicts and establishing a unified religious-political order in Japan, providing a coherent metaphysical framework for diverse spiritual practices.
% TRANSFER_FUNCTION: Transfers interpretive authority, spiritual prestige, and material resources (e.g., temple endowments, imperial patronage) from indigenous kami traditions to the Buddhist institutional hierarchy, by subsuming kami within a Buddhist metaphysical framework.
% ABSENT_VOICES: Early indigenous kami practitioners who resisted the subsumption of their deities, and later Shinto revivalists (e.g., Kokugaku scholars) who sought to re-establish kami as distinct, supreme entities, free from Buddhist influence. Their perspectives were marginalized or suppressed by the dominant Buddhist-centric narrative.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku ontological fusion doctrine vanished overnight, the historical and contemporary religious landscape of Japan would be fundamentally reinterpreted. The legitimacy of many syncretic practices and institutions would be challenged, leading to a profound reorganization of religious identity, institutional power, and historical narratives.
% FOUNDING_PROBLEM: The need to reconcile the powerful new religion of Buddhism, imported from the continent, with the deeply rooted indigenous kami worship in Japan, to prevent religious conflict and create a unified spiritual foundation for the emerging state.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist scholars and institutional histories attest that the problem of religious integration was successfully resolved by honji-suijaku, leading to a harmonious syncretism. Shinto scholars and historians of indigenous religion, corroborated by archaeological evidence of pre-Buddhist kami worship and later Shinto revival movements, argue that the 'problem' was largely a power dynamic, and the 'solution' was a form of cultural and spiritual subjugation, rather than a genuine resolution of distinct traditions.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the doctrine fundamentally reinterpreted and subsumed indigenous kami autonomy, transferring spiritual authority and resources to the Buddhist establishment. Suppression is very high (0.85) as the Buddhist hierarchy actively enforced this interpretive monopoly, often through institutional means like merging shrines and temples, and re-educating local priests. Accessibility collapse is high (0.70) because alternative, independent interpretations of kami were systematically marginalized. Resistance is moderate (0.60), reflecting ongoing, though often localized, efforts to maintain distinct kami identities. Theater ratio is moderate (0.40) as while genuine syncretic practices emerged, a significant portion of the 'fusion' was performative, masking the underlying power dynamics. The temporal measurements reflect the gradual institutionalization and hardening of this doctrine over centuries, leading to increased extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Buddhist institutional hierarchy, this doctrine was a brilliant act of religious coordination, harmonizing two distinct traditions and creating a richer spiritual landscape. From the perspective of indigenous kami traditions and local cults, it was a form of spiritual colonization and cultural subjugation, where their unique identities were diminished and their autonomy eroded. The imperial court aristocracy might have viewed it as a pragmatic tool for political stability and cultural integration.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy is a clear beneficiary, gaining interpretive authority, prestige, and material resources. The imperial court aristocracy also benefited from the resulting religious stability. Indigenous kami autonomy, local kami cults, and Shinto priests are victims, bearing the costs of subsumption, loss of distinct identity, and forced reinterpretation. Comparative religion scholars act as analytical observers, assessing the structural dynamics without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as pure coordination (Rope) by highlighting the asymmetric extraction and active enforcement. It also avoids mislabeling it as pure extraction (Snare) by acknowledging the genuine, albeit contested, coordination function of unifying diverse religious practices. The Mandatrophy analysis would focus on whether the 'founding problem' of religious integration remained 'live' or if the doctrine's persistence became primarily about maintaining the Buddhist hierarchy's interpretive monopoly, even as the initial integration challenges evolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_truth_vs_power_play,
    'To what extent did honji-suijaku theory genuinely capture a metaphysical truth about the nature of kami and buddhas, versus serving as an institutional power play by the Buddhist hierarchy?',
    'Analysis of theological arguments independent of institutional outcomes, and comparative studies of religious syncretism in other cultures where power dynamics were different.',
    'If primarily a power play, the extractiveness and suppression metrics are more accurately interpreted as coercive; if genuinely metaphysical, the coordination function is stronger, and some ''extraction'' might be re-read as necessary integration cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_power_play, conceptual, 'Ambiguity between genuine metaphysical claim and institutional power consolidation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of indigenous kami autonomy primarily structural (institutional enforcement, legal mandates) or internalized (the belief that kami were indeed lesser manifestations, leading to self-subsumption)?',
    'Post-Meiji Restoration analysis: if kami traditions rapidly reasserted independence after structural enforcement was removed, suppression was largely structural. If internalized beliefs persisted, it suggests a deeper, identity-level suppression.',
    'If internalized, the constraint''s effective suppression was higher and more pervasive than structural measures suggest, making the ''identity_locked'' exit option for kami autonomy more profound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for kami autonomy.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''ontological fusion'' framing the most appropriate for understanding simultaneous veneration, or do alternative framings (domain partition, pragmatic incoherence) offer a more accurate structural account?',
    'Comparative analysis of historical evidence and textual sources through the lens of each reading, assessing which framing best explains the observed religious practices and power dynamics over time.',
    'If the ''domain partition'' reading were adopted, the constraint would likely compute as a Rope or Scaffold, with lower extraction. If ''pragmatic incoherence'' were adopted, it might compute as a Piton or even a non-constraint, as it would imply a lack of active enforcement or coherent structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Under-determination of the kernel''s primary structural framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t700, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 700, 0.2).
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 900, 0.25).
narrative_ontology:measurement(simu_tr_t1100, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1100, 0.3).
narrative_ontology:measurement(simu_tr_t1300, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1300, 0.35).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1868, 0.4).

% Extraction over time
narrative_ontology:measurement(simu_be_t700, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 700, 0.5).
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 900, 0.6).
narrative_ontology:measurement(simu_be_t1100, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1100, 0.7).
narrative_ontology:measurement(simu_be_t1300, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1300, 0.75).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1500, 0.77).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1868, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t700, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 700, 0.55).
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 900, 0.65).
narrative_ontology:measurement(simu_su_t1100, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1100, 0.75).
narrative_ontology:measurement(simu_su_t1300, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1300, 0.8).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1500, 0.83).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1868, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'simultaneous_veneration' kernel. Its ε value (high extraction) differs significantly from the 'domain_partition_reading' (lower extraction, more coordination-focused) and directly contradicts the 'pragmatic_incoherence_reading' (negligible extraction, no coherent structure). Each reading represents a distinct structural claim about the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
