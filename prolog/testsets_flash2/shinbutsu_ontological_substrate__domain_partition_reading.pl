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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami and Buddhas Govern Separate Domains (Domain Partition Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents a 'domain partition' reading of the
 *   relationship between Kami (Shinto deities) and Buddhas in Japanese
 *   religious history. It posits that Shinto and Buddhism, while coexisting,
 *   functionally govern separate spiritual domains (e.g., this-worldly purity
 *   vs. afterlife salvation), rather than being ontologically fused or
 *   forming an incoherent bundle. This reading emphasizes pragmatic
 *   institutional arrangements and complementarity over deep metaphysical
 *   syncretism. The constraint is classified as a Rope because it facilitates
 *   coordination with relatively low extraction, benefiting both religious
 *   institutions and local communities by providing a clear division of
 *   spiritual labor.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.25).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.15).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami and Buddhas Govern Separate Domains (Domain Partition Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, 'cf577e24-22ae-4713-8f71-da41bd9432b0').
narrative_ontology:cs_kernel_codification('cf577e24-22ae-4713-8f71-da41bd9432b0', distributed).
narrative_ontology:cs_authority_grounding('cf577e24-22ae-4713-8f71-da41bd9432b0', practice).
narrative_ontology:cs_interpretation_layer_present('cf577e24-22ae-4713-8f71-da41bd9432b0').
narrative_ontology:cs_reading_relation('cf577e24-22ae-4713-8f71-da41bd9432b0', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf577e24-22ae-4713-8f71-da41bd9432b0', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('cf577e24-22ae-4713-8f71-da41bd9432b0', foundational, functional_complementarity_of_kami_buddha).
narrative_ontology:cs_axiom_status(functional_complementarity_of_kami_buddha, holdable).
narrative_ontology:cs_axiom_grounding('cf577e24-22ae-4713-8f71-da41bd9432b0', functional_complementarity_of_kami_buddha, conventional).
narrative_ontology:cs_axiom('cf577e24-22ae-4713-8f71-da41bd9432b0', secondary, institutional_autonomy_of_shinto_buddhism).
narrative_ontology:cs_axiom_status(institutional_autonomy_of_shinto_buddhism, holdable).
narrative_ontology:cs_axiom_grounding('cf577e24-22ae-4713-8f71-da41bd9432b0', institutional_autonomy_of_shinto_buddhism, conventional).
narrative_ontology:cs_reference_frame('cf577e24-22ae-4713-8f71-da41bd9432b0', pragmatic_institutional_coexistence).
narrative_ontology:cs_drift_state('cf577e24-22ae-4713-8f71-da41bd9432b0', contemporary_religious_studies, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cf577e24-22ae-4713-8f71-da41bd9432b0', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, local_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, functional_coexistence_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, distinct_spiritual_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Shinto rituals and shrines, focusing on this-worldly blessings and purity. Benefits from a clear demarcation of spiritual authority that avoids direct competition with Buddhism.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood, agenda_setter,
    institutional, generational, mobile, national).

% Administers Buddhist temples and rites, focusing on afterlife salvation and karmic merit. Benefits from a clear demarcation of spiritual authority that avoids direct competition with Shinto.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy, agenda_setter,
    institutional, generational, mobile, national).

% Utilize both Shinto and Buddhist institutions for different life events (e.g., Shinto for birth/marriage, Buddhist for funerals). Benefits from the functional complementarity and lack of conflict between the two traditions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, local_communities, beneficiary,
    organized, biographical, constrained, local).

% Analyze the historical and theological development of Japanese religious traditions, often seeking to identify underlying structural principles or their absence. This reading aligns with certain academic interpretations of religious separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual landscape of Japan by assigning distinct, complementary roles to Shinto and Buddhist deities and institutions, preventing ontological conflict and allowing for pragmatic coexistence.
% TRANSFER_FUNCTION: Transfers spiritual authority and ritual responsibility for specific life domains (e.g., this-worldly vs. afterlife) to either Shinto or Buddhist institutions, ensuring comprehensive spiritual coverage without overlap.
% ABSENT_VOICES: Those who seek a deeper, unified metaphysical understanding of Kami and Buddhas, or those who view the syncretism as purely a product of political coercion, would object to this functional separation. Their voices are often marginalized in favor of pragmatic institutional arrangements.
% DISAPPEARANCE_RATIONALE: If the understanding of Kami and Buddhas as governing separate domains vanished, it would disrupt centuries of institutional practice, ritual division of labor, and community engagement with religious sites. The functional harmony would collapse, forcing a re-evaluation of religious identity and practice.
% FOUNDING_PROBLEM: The historical challenge of integrating indigenous Japanese beliefs (Shinto) with imported Buddhism without causing irreconcilable conflict or undermining either tradition's legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion and anthropologists studying contemporary religious practices corroborate that managing the relationship between Shinto and Buddhism remains a live, if often implicit, challenge for religious institutions and practitioners, even if the specific 'problem' has evolved.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.25) because the arrangement primarily facilitates functional coexistence rather than imposing significant costs. Suppression is also low (0.15) as this reading emphasizes a natural, complementary division of labor, requiring minimal coercion to maintain. Theater ratio is low (0.1) because the functional separation is largely genuine and serves a clear purpose. The historical measurements show a stable, low-extraction profile, consistent with a long-standing, functional coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This reading, while functional, might be contested by those who perceive a deeper ontological unity (syncretic_fusion_reading) or those who see the historical relationship as primarily driven by state power and institutional accretion (incoherent_bundle_reading). The low extractiveness and suppression reflect this reading's emphasis on functional harmony, which would be challenged by alternative interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Both Shinto and Buddhist clergy are beneficiaries and agenda-setters, as they maintain their distinct institutional roles and authority within this framework. Local communities are beneficiaries, gaining access to comprehensive spiritual services without conflict. Scholarly observers are analytical, seeking to understand the structural principles at play.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_separation,
    'Is the separation between Kami and Buddhas truly ontological (distinct beings) or merely functional (distinct roles within a shared spiritual reality)?',
    'Analysis of theological texts and ritual practices for explicit statements on the nature of Kami and Buddhas, and their interrelationship, beyond pragmatic institutional arrangements.',
    'If the separation is purely functional, the constraint''s ''naturalness'' is reduced, potentially increasing its perceived extractiveness for those seeking ontological unity. If ontological, the constraint is more Mountain-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_separation, conceptual, 'Ambiguity regarding the depth of separation between Kami and Buddhas.').

omega_variable(
    state_coercion_influence,
    'To what extent was this ''domain partition'' a natural evolution of religious practice versus a result of state-imposed policies (e.g., Shinbutsu-bunri) that enforced separation?',
    'Historical analysis of pre-Meiji Restoration religious policies and local practices, comparing regions with strong state influence to those with less, to discern the degree of organic vs. imposed separation.',
    'If state coercion was a primary driver, the constraint''s suppression and extractiveness would be higher, shifting it towards a Tangled Rope or Snare, as the ''coordination'' would be less voluntary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_coercion_influence, empirical, 'Role of state power in establishing or maintaining the domain partition.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the ''shinbutsu_ontological_substrate'' kernel, or does it misrepresent the historical relationship?',
    'Comparison with sibling readings (syncretic_fusion_reading, incoherent_bundle_reading) and their supporting evidence. The engine''s cross-reading consistency checks will highlight structural divergences.',
    'If this reading is found to be structurally inconsistent with the kernel or historical evidence, its classification as a Rope would be challenged, potentially reclassifying it as a Snare (if coercive) or Piton (if merely theatrical).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''shinbutsu_ontological_substrate'' kernel, emphasizing functional domain partition. Sibling readings (syncretic_fusion_reading, incoherent_bundle_reading) offer alternative interpretations of the Kami-Buddha relationship, which would structurally alter the constraint''s properties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 300, 0.09).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 600, 0.09).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1500, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 300, 0.22).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 600, 0.23).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 900, 0.24).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1200, 0.25).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1500, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 300, 0.12).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 600, 0.13).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 900, 0.14).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1200, 0.15).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1500, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_substrate' kernel. This 'domain_partition_reading' emphasizes functional separation and pragmatic coexistence, contrasting with the 'syncretic_fusion_reading' (ontological unity) and 'incoherent_bundle_reading' (state-enforced accretion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
