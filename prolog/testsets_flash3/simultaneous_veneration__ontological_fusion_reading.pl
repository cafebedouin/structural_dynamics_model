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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Ontological Fusion of Kami and Buddhas (Honji-Suijaku Theory)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.8).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.75).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Ontological Fusion of Kami and Buddhas (Honji-Suijaku Theory)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'c3ffb73d-50df-4e78-908f-ebbb283fc3fd').
narrative_ontology:cs_kernel_codification('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', formalized).
narrative_ontology:cs_authority_grounding('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', lineage).
narrative_ontology:cs_interpretation_layer_present('c3ffb73d-50df-4e78-908f-ebbb283fc3fd').
narrative_ontology:cs_reading_relation('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', kami_are_buddha_manifestations, theological).
narrative_ontology:cs_axiom('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', foundational, buddhist_cosmology_is_universal).
narrative_ontology:cs_axiom_status(buddhist_cosmology_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', buddhist_cosmology_is_universal, theological).
narrative_ontology:cs_reference_frame('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', buddhist_hegemony_through_syncretism).
narrative_ontology:cs_drift_state('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('c3ffb73d-50df-4e78-908f-ebbb283fc3fd', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_scholars).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, shinto_priests).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, local_kami_cults).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promoted and enforced honji-suijaku theory, integrating kami into the Buddhist pantheon as manifestations of buddhas. Benefited from expanded influence, landholdings, and interpretive authority over indigenous beliefs. Actively suppressed alternative interpretations that asserted kami independence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Developed and articulated the complex theological arguments for honji-suijaku, gaining intellectual prestige and institutional support. Their careers and academic standing were often tied to the propagation of this theory.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_scholars, beneficiary,
    powerful, biographical, constrained, national).

% The conceptual autonomy and distinct identity of indigenous kami traditions were subsumed and reinterpreted as secondary to Buddhist deities. This led to a loss of independent theological development and ritual practice.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy, payer,
    powerless, civilizational, identity_locked, local).
narrative_ontology:stakeholder_non_agent(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy).

% Often found their shrines and practices integrated into Buddhist temple complexes, with their deities reinterpreted through a Buddhist lens. While some adapted and thrived, others lost independent authority and resources, becoming subordinate to Buddhist institutions.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shinto_priests, payer,
    moderate, generational, constrained, local).

% Practitioners of local kami worship were encouraged or compelled to adopt Buddhist interpretations of their deities, leading to a gradual erosion of distinct local traditions and beliefs. Their identity was fused with the syncretic framework.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, local_kami_cults, payer,
    powerless, generational, identity_locked, local).

% Later observed the syncretic system and, during the Meiji Restoration, actively dismantled it through the Shinbutsu-bunri (separation of kami and buddhas) policy, viewing the fusion as an impediment to national identity and modernization. Their intervention fundamentally altered the constraint.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(simultaneous_veneration__ontological_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a unified theological framework for diverse indigenous kami traditions and imported Buddhism, facilitating the spread of Buddhism across Japan by making it compatible with existing beliefs and integrating local deities into a broader cosmology.
% TRANSFER_FUNCTION: Transferred interpretive authority, ritual control, and material resources (land, offerings) from independent kami cults and Shinto institutions to the Buddhist institutional hierarchy, in exchange for theological legitimacy and integration into a dominant religious system.
% ABSENT_VOICES: Early indigenous kami practitioners and those who resisted Buddhist theological dominance were largely unrecorded or suppressed. Their voices would have emphasized the distinctness and autonomy of kami, challenging the ontological fusion.
% DISAPPEARANCE_RATIONALE: If honji-suijaku theory had never gained dominance, the religious landscape of Japan would be fundamentally different, with a more distinct and potentially competing relationship between Shinto and Buddhism, rather than the deep syncretism that characterized much of its history. The Meiji separation would have had a different target.
% FOUNDING_PROBLEM: The challenge of integrating an imported, universalist religion (Buddhism) with diverse, localized indigenous animistic beliefs (kami worship) without outright conflict or rejection.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from both Buddhist and Shinto sources, as well as modern religious studies scholarship, corroborate the initial problem of religious integration. However, the 'live' status of the problem is contested, as the Meiji separation policies effectively dismantled the institutional framework of honji-suijaku, suggesting the original problem was 'solved' by state intervention, not by the theory's inherent coordination function.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_distinction,
    'Is the distinction between kami and buddhas fundamentally ontological (as this reading claims) or primarily functional/phenomenological (as the domain_partition_reading suggests)?',
    'Analysis of pre-syncretic texts and archaeological evidence for independent kami worship, alongside comparative theological studies of indigenous Japanese beliefs vs. imported Buddhist metaphysics.',
    'If the distinction is primarily functional, this ''ontological fusion'' reading''s high extractiveness would be further amplified, as its core premise (ontological identity) would be revealed as a theological justification for power transfer rather than a genuine metaphysical insight. If ontological identity is strongly supported, the extractiveness might be seen as a necessary cost of a ''true'' integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_distinction, conceptual, 'Ambiguity regarding the fundamental nature of kami-buddha relationship.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of kami autonomy primarily structural (institutional power, land ownership) or internalized (theological persuasion, identity fusion)?',
    'Post-Meiji separation trajectory: if kami cults rapidly reasserted independent identities and practices after institutional separation, it suggests structural suppression. If the syncretic identity persisted, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as the target carried the suppression with them after formal removal of the extractive mechanism. This would amplify the Snare-like qualities of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for kami autonomy.').

omega_variable(
    kernel_reading_ontological_fusion,
    'This constraint is the ''ontological_fusion_reading'' of the ''simultaneous_veneration'' kernel. How would the classification change if a sibling reading, such as ''domain_partition_reading'' or ''pragmatic_incoherence_reading'', were adopted?',
    'Adopting the ''domain_partition_reading'' would likely lower extractiveness and suppression, reclassifying towards a Rope or even Mountain, as it posits a more harmonious coexistence. Adopting the ''pragmatic_incoherence_reading'' would likely emphasize the lack of genuine coordination and highlight the eventual state-enforced separation, potentially reclassifying towards a Piton or Snare.',
    'The classification is highly sensitive to the chosen reading, reflecting the contested nature of the kernel. This reading emphasizes the extractive aspects of the syncretic project.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ontological_fusion, conceptual, 'Impact of alternative readings of the simultaneous veneration kernel.').


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
narrative_ontology:measurement(simu_be_t1300, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1300, 0.75).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1500, 0.8).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1868, 0.8).

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

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'simultaneous_veneration' kernel. Its siblings are 'domain_partition_reading' and 'pragmatic_incoherence_reading', each representing a different structural interpretation of the kami-buddha relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
