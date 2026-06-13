% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Pragmatic Incoherence of Simultaneous Veneration (Pre-Meiji)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the pre-Meiji state of simultaneous veneration
 *   in Japan, interpreted through the 'pragmatic incoherence' reading. It
 *   posits that the syncretic practices of Shinto and Buddhism were not
 *   underpinned by a coherent theological or ontological framework, but
 *   rather by a pragmatic tolerance of contradiction, sustained by a lack of
 *   institutional pressure to resolve these inconsistencies. The Meiji-era
 *   shinbutsu-bunri (separation of kami and buddhas) is seen not as an
 *   arbitrary rupture, but as a revelation of this latent incoherence, which
 *   then became actively suppressed. The constraint is claimed as a Snare
 *   because it extracted a cost in theological clarity and intellectual
 *   honesty from practitioners, while benefiting institutions that profited
 *   from the ambiguity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.7).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.8).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Pragmatic Incoherence of Simultaneous Veneration (Pre-Meiji)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '3c8d32fa-3423-443c-97f5-f9d8ce874a91').
narrative_ontology:cs_kernel_codification('3c8d32fa-3423-443c-97f5-f9d8ce874a91', implicit).
narrative_ontology:cs_authority_grounding('3c8d32fa-3423-443c-97f5-f9d8ce874a91', practice).
narrative_ontology:cs_interpretation_layer_present('3c8d32fa-3423-443c-97f5-f9d8ce874a91').
narrative_ontology:cs_reading_relation('3c8d32fa-3423-443c-97f5-f9d8ce874a91', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c8d32fa-3423-443c-97f5-f9d8ce874a91', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('3c8d32fa-3423-443c-97f5-f9d8ce874a91', foundational, theological_contradiction_is_tolerated).
narrative_ontology:cs_axiom_status(theological_contradiction_is_tolerated, holdable).
narrative_ontology:cs_axiom_grounding('3c8d32fa-3423-443c-97f5-f9d8ce874a91', theological_contradiction_is_tolerated, conventional).
narrative_ontology:cs_axiom('3c8d32fa-3423-443c-97f5-f9d8ce874a91', secondary, lack_of_enforcement_sustains_incoherence).
narrative_ontology:cs_axiom_status(lack_of_enforcement_sustains_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('3c8d32fa-3423-443c-97f5-f9d8ce874a91', lack_of_enforcement_sustains_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('3c8d32fa-3423-443c-97f5-f9d8ce874a91', pragmatic_syncretic_tolerance).
narrative_ontology:cs_drift_state('3c8d32fa-3423-443c-97f5-f9d8ce874a91', meiji_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('3c8d32fa-3423-443c-97f5-f9d8ce874a91', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, local_elites).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, theological_coherence).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) stems from the cognitive burden and theological confusion imposed on practitioners, who were implicitly required to hold contradictory beliefs without resolution. Suppression (0.8) was high due to the absence of any institutional mechanism or intellectual tradition that actively challenged or sought to resolve these contradictions, effectively trapping practitioners in an incoherent system. The theater ratio (0.6) reflects the performative aspect of simultaneous veneration, where rituals and practices continued without a deep, shared understanding of their underlying meaning, serving more to maintain social order and institutional power than genuine spiritual coherence. Accessibility collapse (0.7) was high because alternative coherent theological frameworks were not readily available or actively suppressed, and resistance (0.3) was low due to the diffuse nature of the incoherence and the lack of a clear 'agenda-setter' to resist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious institutions and local elites, the system was a functional 'rope' or 'tangled_rope' that maintained social harmony and their own authority. From the perspective of practitioners seeking theological coherence, it was a 'snare' that extracted intellectual honesty and clarity. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and local elites were beneficiaries (d near 0.0) as the ambiguity allowed them to consolidate power and resources without needing to resolve complex theological disputes. Practitioners were victims (d near 1.0) as they bore the cognitive cost of incoherence. Theological coherence itself is a victim, as its absence was the primary extraction. The lack of active enforcement pressure meant the 'snare' was sustained by inertia and the absence of alternatives, rather than overt coercion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''pragmatic_incoherence_reading'' of the ''simultaneous_veneration'' kernel?',
    'Analysis of primary historical and theological texts, focusing on explicit statements of belief and practice by pre-Meiji practitioners and scholars.',
    'If this reading is incorrect, the classification of the pre-Meiji religious landscape would shift significantly, potentially towards a ''rope'' or ''tangled_rope'' if coherence or functional partitioning is established.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific interpretation of the simultaneous veneration kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'To what extent was the suppression of theological contradiction structural (lack of institutional enforcement) versus internalized (cognitive patterns of holding contradictory beliefs)?',
    'Examination of individual practitioner diaries and local religious records for evidence of internal conflict or explicit attempts to reconcile beliefs, versus external pressures or lack thereof.',
    'If suppression was primarily internalized, the effective suppression for practitioners was higher than the structural measure suggests, as they carried the incoherence within their own belief systems. If purely structural, the incoherence was merely latent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological incoherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(simu_tr_t25, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(simu_tr_t50, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(simu_tr_t75, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 75, 0.55).
narrative_ontology:measurement(simu_tr_t100, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(simu_be_t25, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(simu_be_t50, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(simu_be_t75, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 75, 0.65).
narrative_ontology:measurement(simu_be_t100, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(simu_su_t25, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(simu_su_t50, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(simu_su_t75, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 75, 0.75).
narrative_ontology:measurement(simu_su_t100, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, meiji_shinbutsu_bunri_edict).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'simultaneous_veneration' kernel, focusing on the pre-Meiji pragmatic incoherence. It contrasts with readings that posit ontological fusion or domain partitioning, which represent alternative interpretations of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
