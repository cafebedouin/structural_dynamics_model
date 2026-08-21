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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Theory (Ontological Fusion Reading)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint represents the 'ontological fusion' reading of
 *   honji-suijaku theory, which posited that indigenous Japanese kami were
 *   manifestations (suijaku) of universal Buddhist deities (honji). This
 *   reading, primarily promoted by the Buddhist institutional hierarchy,
 *   served to integrate and subordinate local kami cults into a broader
 *   Buddhist cosmology. It is presented as a Tangled Rope because it offered
 *   a coordination function (religious unity) but involved significant
 *   asymmetric extraction of authority and resources from indigenous
 *   traditions. The constraint persisted through active theological
 *   development and institutional enforcement until the Meiji Restoration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.85).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.75).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Theory (Ontological Fusion Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8').
narrative_ontology:cs_kernel_codification('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', formalized).
narrative_ontology:cs_authority_grounding('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', lineage).
narrative_ontology:cs_interpretation_layer_present('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8').
narrative_ontology:cs_reading_relation('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', simultaneous_veneration__domain_partition_reading, influences).
narrative_ontology:cs_reading_relation('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', kami_are_buddha_manifestations, theological).
narrative_ontology:cs_axiom('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', secondary, buddhist_cosmology_is_universal).
narrative_ontology:cs_axiom_status(buddhist_cosmology_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', buddhist_cosmology_is_universal, theological).
narrative_ontology:cs_reference_frame('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', unified_buddhist_kami_cosmology).
narrative_ontology:cs_drift_state('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('c0747f3a-eb2d-4021-a7c4-99cfbfe22fb8', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_scholars).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_cults).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, shinto_priests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, lay_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, buddhist_universalism).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, metaphysical_unity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgated and enforced honji-suijaku theory, integrating kami into a Buddhist cosmology. Benefited from expanded influence, landholdings, and interpretive authority over indigenous beliefs. Actively suppressed alternative interpretations that asserted kami autonomy.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Developed and refined the theoretical framework of honji-suijaku, gaining intellectual prestige and career advancement within the Buddhist establishment. Their interpretive work reinforced the ontological fusion, providing theological justification for the hierarchy's claims.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_scholars, beneficiary,
    organized, biographical, constrained, national).

% Saw their local deities reinterpreted as manifestations of universal buddhas, losing autonomous spiritual authority and often having their shrines absorbed into Buddhist temple complexes. Their identity was fused with the new syncretic framework, making direct resistance difficult.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_cults, payer,
    powerless, generational, identity_locked, local).

% Were often integrated into Buddhist temple structures, performing rituals for kami now understood as Buddhist manifestations. While retaining some ritual function, their independent authority and theological distinctiveness were diminished. Exit meant abandoning their traditional roles and communities.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shinto_priests, payer,
    moderate, biographical, constrained, local).

% Benefited from a unified religious framework that allowed simultaneous veneration without perceived contradiction, offering a comprehensive path to salvation and worldly blessings. However, they also bore the costs of supporting the expanded Buddhist institutions and their interpretive monopoly.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, lay_practitioners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, lay_practitioners, payer).

% Later observed this syncretic system and, in the Meiji era, actively dismantled it through the Shinbutsu-bunri (separation of kami and buddhas) policy, viewing the fusion as an impediment to national identity and a source of Buddhist power. Their intervention fundamentally altered the constraint.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a coherent theological framework that integrated indigenous kami worship with imported Buddhism, allowing for simultaneous veneration and preventing religious conflict or fragmentation within a unified Japanese spiritual landscape.
% TRANSFER_FUNCTION: Transferred spiritual authority, interpretive control, and material resources (land, offerings) from autonomous indigenous kami cults and Shinto institutions to the dominant Buddhist institutional hierarchy, in exchange for theological legitimation and integration.
% ABSENT_VOICES: Early indigenous kami practitioners and proto-Shinto thinkers who asserted the absolute autonomy and distinctiveness of kami would have objected to their reinterpretation as Buddhist manifestations. Their voices were absorbed or suppressed by the dominant Buddhist narrative.
% DISAPPEARANCE_RATIONALE: If the ontological fusion reading of honji-suijaku theory had never taken hold, the religious landscape of Japan would have remained far more fragmented, with distinct and potentially competing kami and Buddhist traditions. The Meiji separation would have had a different character, as there would have been no deeply integrated syncretic system to dismantle.
% FOUNDING_PROBLEM: The problem of integrating a powerful, universalizing foreign religion (Buddhism) with deeply entrenched indigenous spiritual beliefs (kami worship) without causing widespread cultural or religious conflict.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutions historically attested to the problem's live status, emphasizing the need for a unified spiritual path. However, the Meiji government's later successful separation of kami and buddhas, and the subsequent development of modern Shinto, demonstrate that the 'problem' of integration was a historical contingency, not a permanent spiritual necessity. Independent historical and anthropological analyses corroborate that the problem was resolved (or dissolved) by state intervention, not by inherent theological necessity.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the ontological fusion fundamentally reinterpreted and subsumed indigenous spiritual authority under Buddhist doctrine, leading to a significant transfer of power and resources. Suppression (0.75) was necessary to maintain this interpretive monopoly against potential resistance from kami cults and proto-Shinto elements. The theater ratio is low (0.20) because the theological work and institutional integration were genuinely functional in establishing and maintaining the syncretic system, rather than merely performative. The historical measurements show a clear increase in extractiveness and suppression as the theory became more entrenched and Buddhist institutions gained power over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Buddhist hierarchy, this was a necessary and benevolent act of spiritual integration. From the perspective of indigenous kami cults, it was a form of cultural and spiritual colonization. The engine's classification as Tangled Rope captures this dual nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy and scholars were clear beneficiaries, gaining authority and intellectual capital. Indigenous kami cults and Shinto priests were victims, losing autonomy and having their traditions reinterpreted. Lay practitioners were mixed, benefiting from a unified spiritual system but also bearing the costs of its maintenance and interpretive control. The Meiji government acted as an external observer, eventually dismantling the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to integrate and unify disparate religious traditions. While it achieved this, the 'founding problem' of integration became 'dead' with the Meiji separation, yet the underlying theological claims persisted in some forms. The classification as Tangled Rope prevents mislabeling this as pure coordination, highlighting the extractive dimension that became more pronounced over time. The later state-mandated separation (Shinbutsu-bunri) effectively resolved the mandatrophy by dismantling the constraint, rather than allowing it to atrophy into a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_distinction,
    'Is the identity of kami and buddhas an ontological truth, a functional distinction, or a pragmatic incoherence?',
    'Analysis of primary religious texts and archaeological evidence for pre-Buddhist kami worship, alongside comparative studies of religious syncretism and cognitive dissonance in belief systems.',
    'If a functional distinction (domain_partition_reading) or pragmatic incoherence (pragmatic_incoherence_reading) is established, the ''ontological_fusion_reading'' would be reclassified as a Snare, as its coordination function would be revealed as a cover for pure extraction of spiritual authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_functional_distinction, conceptual, 'Ambiguity regarding the true nature of kami-buddha relationship.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of kami autonomy primarily structural (institutional power, land absorption) or internalized (theological persuasion, identity fusion)?',
    'Post-Meiji suppression trajectory: if kami autonomy re-emerged rapidly after Shinbutsu-bunri, it suggests structural suppression. If internalized, the re-emergence would have been slower and more contested, as practitioners carried the fusion with them.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as the identity fusion bound practitioners even after external barriers were removed. This would push the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for kami autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t700, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 700, 0.1).
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 900, 0.15).
narrative_ontology:measurement(simu_tr_t1100, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1100, 0.2).
narrative_ontology:measurement(simu_tr_t1300, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1300, 0.2).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1868, 0.2).

% Extraction over time
narrative_ontology:measurement(simu_be_t700, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 700, 0.4).
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 900, 0.55).
narrative_ontology:measurement(simu_be_t1100, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1100, 0.7).
narrative_ontology:measurement(simu_be_t1300, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1300, 0.8).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1500, 0.85).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1868, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t700, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 700, 0.3).
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 900, 0.45).
narrative_ontology:measurement(simu_su_t1100, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1100, 0.6).
narrative_ontology:measurement(simu_su_t1300, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1300, 0.7).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1500, 0.75).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1868, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.08).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, meiji_shinbutsu_bunri_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'simultaneous_veneration' kernel. This 'ontological_fusion_reading' asserts the metaphysical identity of kami and buddhas, influencing and competing with the 'domain_partition_reading' and 'pragmatic_incoherence_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
