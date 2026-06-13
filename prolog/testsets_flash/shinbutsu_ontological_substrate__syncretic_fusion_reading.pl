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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   This constraint represents the 'syncretic fusion' reading of the
 *   shinbutsu ontological substrate kernel, asserting that kami and buddhas
 *   are fundamentally unified at a metaphysical level, and that the honji
 *   suijaku theory describes this deep truth rather than a mere institutional
 *   arrangement. This reading emphasizes the inherent compatibility and
 *   ultimate non-duality of the two traditions, leading to high institutional
 *   entanglement and resistance to separation. The constraint is claimed as a
 *   Mountain due to its perceived naturalness and deep integration into the
 *   Japanese worldview for centuries, though beneficiaries exist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.3).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.4).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, mountain).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Shinbutsu Ontological Substrate: Syncretic Fusion Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:emerges_naturally(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'f0f89ddd-8482-4106-a331-a2583630c105').
narrative_ontology:cs_kernel_codification('f0f89ddd-8482-4106-a331-a2583630c105', implicit).
narrative_ontology:cs_authority_grounding('f0f89ddd-8482-4106-a331-a2583630c105', lineage).
narrative_ontology:cs_interpretation_layer_present('f0f89ddd-8482-4106-a331-a2583630c105').
narrative_ontology:cs_reading_relation('f0f89ddd-8482-4106-a331-a2583630c105', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0f89ddd-8482-4106-a331-a2583630c105', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('f0f89ddd-8482-4106-a331-a2583630c105', foundational, kami_buddha_metaphysical_unity).
narrative_ontology:cs_axiom_status(kami_buddha_metaphysical_unity, holdable).
narrative_ontology:cs_axiom_grounding('f0f89ddd-8482-4106-a331-a2583630c105', kami_buddha_metaphysical_unity, deontological).
narrative_ontology:cs_axiom('f0f89ddd-8482-4106-a331-a2583630c105', foundational, honji_suijaku_describes_truth).
narrative_ontology:cs_axiom_status(honji_suijaku_describes_truth, holdable).
narrative_ontology:cs_axiom_grounding('f0f89ddd-8482-4106-a331-a2583630c105', honji_suijaku_describes_truth, conventional).
narrative_ontology:cs_reference_frame('f0f89ddd-8482-4106-a331-a2583630c105', pre_meiji_syncretic_consensus).
narrative_ontology:cs_drift_state('f0f89ddd-8482-4106-a331-a2583630c105', contemporary_academic_discourse, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('f0f89ddd-8482-4106-a331-a2583630c105', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_religious_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, practitioners_of_fused_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, purist_shinto_revivalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (temples, shrines, and hybrid complexes) derive their legitimacy and operational structure from the deep ontological unity of kami and buddhas. Their rituals, iconography, and funding are intertwined, making separation difficult and costly. They benefit from the stability and perceived depth of this unified worldview.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_religious_institutions, beneficiary,
    institutional, generational, identity_locked, national).

% Individuals whose spiritual practice and worldview are deeply integrated, seeing no fundamental distinction between kami and buddhas. Their identity is fused with this syncretic understanding, and any attempt to disentangle it would be a profound existential challenge. They experience spiritual coherence and continuity.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, practitioners_of_fused_traditions, beneficiary,
    moderate, biographical, identity_locked, local).

% Analyze the historical, theological, and sociological dimensions of shinbutsu shūgō. They can articulate different readings of the kernel and assess their empirical and conceptual coherence, but do not directly participate in the religious practice or institutional maintenance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, academic_scholars_of_religion, observer,
    analytical, generational, analytical, global).

% Historically, pre-Meiji state authorities often supported and even enforced the syncretic understanding, as it provided a stable religious landscape. They benefited from the social cohesion and legitimacy derived from a unified spiritual framework, even if their primary interest was political stability rather than theological truth.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, state_authorities_pre_meiji, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for a 'pure' Shinto tradition, free from Buddhist influence, viewing the syncretic fusion as a corruption. They bear the cost of resisting the deeply entrenched syncretic worldview and institutions, facing social and intellectual friction in their efforts to disentangle kami from buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, purist_shinto_revivalists, payer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified spiritual framework for individuals and institutions, integrating diverse religious practices and beliefs into a coherent worldview, reducing friction between different spiritual paths.
% TRANSFER_FUNCTION: Transfers spiritual authority and legitimacy between kami and buddhas, allowing institutions and practitioners to draw on both traditions without perceived contradiction. It also transfers social cohesion and stability to the broader society.
% ABSENT_VOICES: Early Buddhist missionaries who sought to establish a distinct Buddhist identity in Japan, and later, radical Shinto purists who were suppressed during periods of syncretic dominance. They would argue for the distinctness of each tradition.
% DISAPPEARANCE_RATIONALE: If the ontological unity vanished, the entire religious landscape of Japan would undergo a profound rearrangement. Many temples and shrines would lose their foundational justification, rituals would become incoherent, and the spiritual identities of millions would be fractured. It would necessitate a complete re-evaluation of religious history and practice.
% FOUNDING_PROBLEM: The need to reconcile indigenous Japanese kami worship with the newly introduced and powerful Buddhist tradition, providing a coherent framework for spiritual practice and institutional coexistence.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing academic and theological debates, as well as the continued existence of syncretic practices despite historical attempts at separation, corroborate that the problem of reconciling these traditions remains live. Scholars of Japanese religion, outside of direct institutional beneficiaries, attest to its enduring conceptual and practical relevance.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, ExtMetricName, E),
    domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(shinbutsu_ontological_substrate__syncretic_fusion_reading),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is low, reflecting the perception that this fusion is a natural and beneficial state, not a coercive one. Suppression (0.4) is moderate, as alternative readings or attempts at separation (e.g., by purist Shinto groups) were historically met with resistance, but not always outright coercion. Theater ratio (0.1) is low, as the syncretic practices were genuinely believed to reflect metaphysical truth, not merely performative. Accessibility collapse (0.7) is high because for those holding this view, alternatives (separate Shinto or Buddhist paths) are seen as less complete or even incoherent. Resistance (0.15) is low because the fusion was widely accepted for centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of syncretic institutions and practitioners, this is a natural and beneficial 'Mountain' that provides spiritual coherence. From the perspective of purist Shinto revivalists, it is a 'Snare' or 'Tangled Rope' that suppresses a distinct indigenous tradition. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic religious institutions and practitioners of fused traditions are primary beneficiaries (d near 0.0), as their existence and spiritual coherence are grounded in this ontological unity. State authorities pre-Meiji also benefited from the social stability it provided. Purist Shinto revivalists are payers (d near 1.0), as they bear the cost of challenging this deeply entrenched worldview. Academic scholars are observers (d=0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_fusion,
    'Is the ontological unity of kami and buddhas a genuine metaphysical truth (Mountain), or a constructed theological and institutional framework that benefits identifiable agents (Tangled Rope)?',
    'Comparative theological analysis across diverse religious traditions, historical examination of the political and social forces that shaped syncretism, and analysis of the persistence of the concept after state enforcement ceased.',
    'If primarily constructed, the classification would shift from Mountain to Tangled Rope, highlighting the extractive and coordinative functions of the framework for its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_fusion, conceptual, 'Ambiguity between natural metaphysical truth and constructed religious framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative readings structural (institutional entanglement, state support) or internalized (identity fusion, theological conviction)?',
    'Post-Meiji Restoration analysis: if resistance to separation persisted strongly even after state-mandated shinbutsu bunri (separation), it suggests a higher degree of internalized suppression.',
    'If internalized suppression is dominant, the constraint''s effective suppression is higher than the structural measure suggests, as practitioners carry the suppression with them even after external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative religious interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 600, 0.05).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1868, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 600, 0.2).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 900, 0.25).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1500, 0.29).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1868, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 600, 0.3).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 900, 0.35).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1200, 0.38).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1500, 0.39).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1868, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
