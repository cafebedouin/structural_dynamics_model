% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Shinbutsu Ontological Substrate: Syncretic Fusion Reading
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the 'syncretic fusion' reading of the shinbutsu
 *   ontological substrate, where kami and buddhas are understood as
 *   metaphysically unified, and honji suijaku (original ground and manifest
 *   trace) describes this deep truth rather than a mere institutional
 *   arrangement. This reading emphasizes the inherent compatibility and
 *   shared essence of the two traditions, resisting attempts at strict
 *   separation. It is one reading of the 'shinbutsu_ontological_substrate'
 *   kernel, alongside 'domain_partition_reading' and
 *   'incoherent_bundle_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.35).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.2).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Shinbutsu Ontological Substrate: Syncretic Fusion Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e34bf0c4-cb0f-4694-8fa0-979ec4af7777').
narrative_ontology:cs_kernel_codification('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', formalized).
narrative_ontology:cs_authority_grounding('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', lineage).
narrative_ontology:cs_interpretation_layer_present('e34bf0c4-cb0f-4694-8fa0-979ec4af7777').
narrative_ontology:cs_reading_relation('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', foundational, kami_buddha_ontological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', kami_buddha_ontological_unity, deontological).
narrative_ontology:cs_axiom('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', foundational, honji_suijaku_metaphysical_truth).
narrative_ontology:cs_axiom_status(honji_suijaku_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', honji_suijaku_metaphysical_truth, deontological).
narrative_ontology:cs_reference_frame('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', classical_syncretic_tradition).
narrative_ontology:cs_drift_state('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', contemporary_academic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e34bf0c4-cb0f-4694-8fa0-979ec4af7777', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_religious_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, traditional_shinto_buddhist_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_shinto_purists).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their spiritual practice and worldview are deeply integrated, seeing kami and buddhas as manifestations of a single truth. This reading validates their lived experience and provides a coherent framework for their rituals and beliefs.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_religious_practitioners, beneficiary,
    moderate, biographical, identity_locked, local).

% These institutions have historically coexisted and often merged, with shared sacred sites and rituals. This reading provides a theological justification for their historical entanglement and continued institutional cooperation, resisting attempts at strict separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, traditional_shinto_buddhist_institutions, beneficiary,
    institutional, generational, constrained, national).

% Analyze the historical and theological development of shinbutsu shūgō. They evaluate different readings based on textual evidence, archaeological findings, and philosophical coherence, without being bound by institutional commitments.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, academic_scholars_of_religion, observer,
    analytical, generational, analytical, global).

% Advocate for a clear distinction between Shinto and Buddhism, often viewing syncretism as a corruption of 'pure' Shinto. This reading challenges their efforts to establish a distinct Shinto identity and requires them to actively argue against the historical fusion.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_shinto_purists, payer,
    organized, generational, constrained, national).

% Examine the historical development of shinbutsu shūgō as a social and political phenomenon, often focusing on institutional arrangements and state policies rather than metaphysical claims. They may view the 'ontological unity' as a post-hoc justification for historical power dynamics.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, secular_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent metaphysical framework that unifies diverse spiritual practices and beliefs, allowing for the harmonious coexistence and integration of Shinto and Buddhist traditions within a single worldview.
% TRANSFER_FUNCTION: Transfers spiritual legitimacy and interpretive authority to the syncretic worldview, reinforcing the integrated identity of practitioners and institutions, and resisting attempts at doctrinal or institutional separation.
% ABSENT_VOICES: Strict doctrinal separatists (e.g., some early Meiji-era Shinto nationalists) who would argue for the absolute distinctness of kami and buddhas are largely absent from contemporary discourse that accepts some form of historical syncretism as a given. Their arguments for complete ontological separation are marginalized.
% DISAPPEARANCE_RATIONALE: If the belief in the ontological unity of kami and buddhas vanished, the spiritual landscape of Japan would fundamentally rearrange. Many traditional practices, integrated shrines/temples, and personal worldviews would lose their foundational coherence, leading to a re-evaluation of religious identity and institutional structures.
% FOUNDING_PROBLEM: To reconcile the indigenous Japanese kami worship with the imported Buddhist tradition, providing a theological basis for their coexistence and mutual influence, particularly through the honji suijaku theory.
% FOUNDING_PROBLEM_CORROBORATION: Syncretic practitioners and traditional institutions attest that the problem of integrating diverse spiritual experiences remains live. Academic scholars, while acknowledging historical shifts, corroborate that the conceptual challenge of reconciling these traditions persists as a central theme in Japanese religious thought, even if the specific historical context has changed.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) as this reading imposes a specific metaphysical framework, which can marginalize alternative interpretations or those seeking clearer distinctions. Suppression is low (0.20) because this reading is largely maintained through cultural transmission and theological argument rather than overt coercion, though it does suppress purely separatist views. Theater ratio is low (0.10) as the claim of ontological unity is genuinely held by many practitioners and institutions, not merely performed. Accessibility collapse is high (0.70) because once this syncretic worldview is adopted, alternatives that posit strict separation become conceptually difficult to access or integrate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of syncretic practitioners, this constraint is a natural and harmonious truth, a 'rope' that unifies their spiritual world. From the perspective of Shinto purists, it is a 'tangled rope' or even a 'snare' that obscures the true nature of Shinto and extracts its distinct identity. The engine's classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic practitioners and traditional institutions are beneficiaries, as this reading validates their integrated worldview and historical entanglement. Modern Shinto purists are payers, as this reading directly challenges their efforts to establish a distinct Shinto identity. Academic scholars and secular historians act as observers, analyzing the phenomenon without direct commitment to its truth claims.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine ontological commitment as mere institutional inertia or extraction. While other readings might see the historical entanglement as a 'piton' (incoherent_bundle_reading) or 'tangled rope' (domain_partition_reading) sustained by power, this reading asserts a live, coherent metaphysical mandate. The low theater ratio and moderate extractiveness reflect this genuine, if contested, function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_syncretism,
    'Is the observed syncretism primarily an ontological truth (as this reading claims) or a functional/institutional arrangement (as the domain_partition_reading claims)?',
    'Analysis of primary theological texts and practitioner testimonies for explicit metaphysical claims versus historical records of institutional mergers and state policies (e.g., Meiji-era shinbutsu bunri).',
    'If primarily functional, this constraint''s extractiveness might be higher (as it masks power dynamics), and its claimed type might shift towards ''tangled_rope'' or ''snare'' for those who resist the institutional arrangement. If ontological, the ''rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_syncretism, conceptual, 'Distinguishing metaphysical truth from institutional function in shinbutsu shūgō.').

omega_variable(
    coherence_of_the_kernel,
    'Is the ''shinbutsu_ontological_substrate'' a genuinely coherent kernel, or is it an ''incoherent bundle'' of disparate practices and beliefs, as the incoherent_bundle_reading suggests?',
    'Philosophical analysis of internal consistency within syncretic doctrines and empirical study of practitioner understanding. If significant internal contradictions or widespread lack of coherent understanding are found, the kernel''s coherence is challenged.',
    'If the kernel is incoherent, this reading''s claim of ''rope'' (coordination) would be undermined, potentially reclassifying it as a ''piton'' (inertial performance) or ''snare'' (if maintained by extraction without genuine coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coherence_of_the_kernel, conceptual, 'Assessing the internal coherence of the syncretic kernel itself.').

omega_variable(
    identity_lock_mechanism,
    'For syncretic practitioners, is the ''identity_locked'' exit option due to genuine spiritual conviction (internalized) or due to social/institutional pressure (structural)?',
    'Longitudinal studies of individuals who attempt to disentangle their Shinto and Buddhist identities, observing the persistence of internal resistance versus external social consequences.',
    'If primarily structural, the effective suppression for these practitioners is higher than measured, as external barriers reinforce the identity lock. If internalized, the constraint is more deeply embedded in their worldview.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized identity lock for syncretic practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 40, 0.33).
narrative_ontology:measurement(shin_be_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(shin_be_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 20, 0.19).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(shin_su_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(shin_su_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_substrate' kernel. This 'syncretic_fusion_reading' asserts ontological unity, influencing and coexisting with other interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
