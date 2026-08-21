% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Functional Domain Partition between Kami and Buddhas
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the functional, rather than ontological,
 *   partition between Kami (Shinto deities) and Buddhas in pre-Meiji Japan.
 *   It posits that the two religious systems coexisted by governing separate
 *   domains—Kami for this-worldly affairs and Buddhas for the
 *   afterlife—allowing for pragmatic integration without requiring a deep
 *   metaphysical fusion. This reading emphasizes the coordination function of
 *   the arrangement, which facilitated social stability and distinct
 *   institutional roles for Shinto and Buddhist clergy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.25).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.3).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Functional Domain Partition between Kami and Buddhas").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '4abe9423-d535-43f6-bc79-c2968f758b6f').
narrative_ontology:cs_kernel_codification('4abe9423-d535-43f6-bc79-c2968f758b6f', formalized).
narrative_ontology:cs_authority_grounding('4abe9423-d535-43f6-bc79-c2968f758b6f', practice).
narrative_ontology:cs_interpretation_layer_present('4abe9423-d535-43f6-bc79-c2968f758b6f').
narrative_ontology:cs_reading_relation('4abe9423-d535-43f6-bc79-c2968f758b6f', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('4abe9423-d535-43f6-bc79-c2968f758b6f', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('4abe9423-d535-43f6-bc79-c2968f758b6f', foundational, kami_govern_this_world).
narrative_ontology:cs_axiom_status(kami_govern_this_world, holdable).
narrative_ontology:cs_axiom_grounding('4abe9423-d535-43f6-bc79-c2968f758b6f', kami_govern_this_world, conventional).
narrative_ontology:cs_axiom('4abe9423-d535-43f6-bc79-c2968f758b6f', foundational, buddhas_govern_afterlife).
narrative_ontology:cs_axiom_status(buddhas_govern_afterlife, holdable).
narrative_ontology:cs_axiom_grounding('4abe9423-d535-43f6-bc79-c2968f758b6f', buddhas_govern_afterlife, conventional).
narrative_ontology:cs_reference_frame('4abe9423-d535-43f6-bc79-c2968f758b6f', complementary_domain_sovereignty).
narrative_ontology:cs_drift_state('4abe9423-d535-43f6-bc79-c2968f758b6f', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4abe9423-d535-43f6-bc79-c2968f758b6f', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, imperial_court_aristocracy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, local_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, distinct_religious_identities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, pragmatic_coexistence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains Shinto rituals and shrines, benefiting from a distinct domain of authority over indigenous deities and this-worldly affairs. Their institutional identity is tied to this separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Administers Buddhist temples and rites, benefiting from a distinct domain of authority over karmic retribution, the afterlife, and spiritual salvation. Their institutional identity is tied to this separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Historically patronized both traditions, benefiting from their distinct roles in legitimizing imperial rule and providing complementary spiritual services without internal conflict.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, imperial_court_aristocracy, beneficiary,
    institutional, generational, constrained, national).

% Participate in both Shinto and Buddhist practices, benefiting from the functional division of labor (e.g., Shinto for life events, Buddhism for death). They bear the costs of maintaining both sets of institutions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, local_communities, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, local_communities, payer).

% Analyze the historical and theological development of Shinbutsu relations, seeking to understand the structural logic of their coexistence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To integrate two distinct religious traditions (indigenous Shinto and introduced Buddhism) into a single society by assigning them complementary functional domains (this-worldly vs. other-worldly), thereby preventing ontological conflict and allowing each to serve specific social and spiritual needs.
% TRANSFER_FUNCTION: Transfers social roles, ritual responsibilities, and spiritual authority to distinct religious institutions, preventing overlap and conflict, and channeling patronage and devotion to each according to its domain.
% ABSENT_VOICES: Those who advocate for a complete ontological fusion of Kami and Buddhas (the syncretic_fusion_reading) or those who deny any coherent underlying structure (the incoherent_bundle_reading) are not part of the functional partitioning framework, as their premises challenge its very foundation.
% DISAPPEARANCE_RATIONALE: If the functional partition vanished overnight, the distinct roles and institutional structures of Shinto and Buddhism would collapse into conflict or forced fusion, requiring a complete reorganization of religious practice, social legitimation, and spiritual understanding in Japan.
% FOUNDING_PROBLEM: Integrating newly introduced Buddhism with indigenous Shinto beliefs and practices in early Japan without undermining either tradition or causing social instability, while also leveraging both for state legitimation and popular spiritual welfare.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts on early Japanese religious policy, anthropological studies of religious practice, and contemporary theological discussions within both Shinto and Buddhist institutions (outside of specific syncretic schools) corroborate the historical and ongoing need for functional distinction. The Meiji Restoration's forced separation (Shinbutsu-bunri) itself attests to the prior existence and functional importance of this partition.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because its primary function is coordination: enabling two distinct religious systems to coexist and provide complementary services. Extractiveness (0.25) and suppression (0.30) are low, reflecting that the functional partition was largely accepted and beneficial to most participants, with minimal coercive overhead inherent to the partition itself (though later political events would impose severe suppression). Theater ratio is low (0.15) as the functional division was genuinely operative. Resistance is low (0.10) because this reading focuses on the period of functional coexistence, not later periods of forced separation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Shinto and Buddhist clergy, the arrangement provided clear institutional boundaries and spheres of influence, ensuring their respective authority. Local communities experienced it as a natural division of spiritual labor. Scholarly observers analyze the historical and structural logic of this coexistence, often contrasting it with later political interventions.
 *
 * DIRECTIONALITY LOGIC:
 *   The Shinto priesthood, Buddhist clergy, and imperial court aristocracy are beneficiaries and agenda-setters, as they directly benefit from and administer the distinct domains of authority and legitimation. Local communities are beneficiaries of the coordinated spiritual services but also payers through their support of both institutions. There are no explicit victims in this functional partition, as the arrangement is understood to be mutually beneficial for coexistence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine functional partition, or is it better understood as an ontological fusion or an incoherent bundle?',
    'Analysis of primary historical and theological texts, focusing on explicit statements of functional division versus claims of metaphysical unity or lack of underlying coherence.',
    'If reclassified as ''syncretic_fusion_reading'', the constraint''s extractiveness might be lower (as fusion implies less internal friction); if ''incoherent_bundle_reading'', the constraint''s coherence and coordination function would be fundamentally challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''shinbutsu_ontological_substrate'' kernel, emphasizing functional separation.').

omega_variable(
    impact_of_syncretic_fusion_reading,
    'How would the structural properties of this constraint change if the ''syncretic_fusion_reading'' (Kami and Buddhas are ontologically unified) were adopted?',
    'Comparative analysis of religious practices and institutional structures in periods or regions where ontological fusion was more dominant, examining changes in institutional entanglement and perceived necessity of boundary maintenance.',
    'If the fusion reading were adopted, the need for active enforcement of domain separation would likely decrease, potentially lowering suppression and extractiveness, as the underlying ontological distinction would be dissolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_syncretic_fusion_reading, conceptual, 'Structural delta if the ''syncretic_fusion_reading'' were to prevail.').

omega_variable(
    impact_of_incoherent_bundle_reading,
    'How would the structural properties of this constraint change if the ''incoherent_bundle_reading'' (no coherent kernel exists) were adopted?',
    'Examination of historical periods or scholarly interpretations that emphasize the fragmented, ad-hoc, or politically enforced nature of Shinbutsu relations, rather than a coherent functional logic.',
    'If the incoherent bundle reading were adopted, the constraint''s claimed coordination function would be undermined, potentially reclassifying it as a Piton (if maintained by inertia) or a Snare (if maintained by state coercion without genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_incoherent_bundle_reading, conceptual, 'Structural delta if the ''incoherent_bundle_reading'' were to prevail.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 1068).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 400, 0.15).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement(shin_tr_t1068, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1068, 0.15).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 200, 0.22).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 400, 0.23).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 600, 0.24).
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 800, 0.25).
narrative_ontology:measurement(shin_be_t1068, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1068, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 200, 0.27).
narrative_ontology:measurement(shin_su_t400, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 400, 0.28).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 600, 0.29).
narrative_ontology:measurement(shin_su_t800, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 800, 0.3).
narrative_ontology:measurement(shin_su_t1068, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1068, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_substrate' kernel, alongside 'syncretic_fusion_reading' and 'incoherent_bundle_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
