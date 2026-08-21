% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Coexistence: Domain Partition Reading
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the historical and practical arrangement in
 *   Japan where Kami (Shinto deities) and Buddhist deities govern separate,
 *   complementary existential domains. Kami are associated with life, purity,
 *   harvest, and local well-being, while Buddhas are associated with death,
 *   ancestors, and salvation in the afterlife. This reading emphasizes a
 *   functional partition rather than ontological fusion, allowing for
 *   peaceful coexistence and comprehensive spiritual coverage without
 *   requiring deep theological reconciliation. The constraint is claimed as a
 *   Rope due to its genuine coordination function and relatively low
 *   extraction, but the metrics reflect a slight, historical increase in
 *   institutional overhead.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.25).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.15).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Coexistence: Domain Partition Reading").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/philosophy_of_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '238fd85c-d92b-404c-b3c1-4d29ff41beb2').
narrative_ontology:cs_kernel_codification('238fd85c-d92b-404c-b3c1-4d29ff41beb2', implicit).
narrative_ontology:cs_authority_grounding('238fd85c-d92b-404c-b3c1-4d29ff41beb2', practice).
narrative_ontology:cs_interpretation_layer_present('238fd85c-d92b-404c-b3c1-4d29ff41beb2').
narrative_ontology:cs_reading_relation('238fd85c-d92b-404c-b3c1-4d29ff41beb2', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('238fd85c-d92b-404c-b3c1-4d29ff41beb2', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('238fd85c-d92b-404c-b3c1-4d29ff41beb2', foundational, functional_domain_autonomy).
narrative_ontology:cs_axiom_status(functional_domain_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('238fd85c-d92b-404c-b3c1-4d29ff41beb2', functional_domain_autonomy, conventional).
narrative_ontology:cs_axiom('238fd85c-d92b-404c-b3c1-4d29ff41beb2', foundational, popular_practice_as_legitimacy).
narrative_ontology:cs_axiom_status(popular_practice_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('238fd85c-d92b-404c-b3c1-4d29ff41beb2', popular_practice_as_legitimacy, conventional).
narrative_ontology:cs_reference_frame('238fd85c-d92b-404c-b3c1-4d29ff41beb2', pre_meiji_functional_coexistence).
narrative_ontology:cs_drift_state('238fd85c-d92b-404c-b3c1-4d29ff41beb2', contemporary_religious_pluralism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('238fd85c-d92b-404c-b3c1-4d29ff41beb2', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, local_communities).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_priests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a practical religious system that addresses all aspects of life and death through distinct but coexisting traditions. They experience low doctrinal friction and high functional utility, but are constrained by the established practices.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, local_communities, beneficiary,
    organized, generational, constrained, local).

% Administer rituals for death, ancestors, and salvation, maintaining their distinct domain. They benefit from the clear division of labor and the stable flow of patronage for these services. Their institutional power is tied to this partition.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Administer rituals for life, purity, harvest, and local kami, maintaining their distinct domain. They benefit from the clear division of labor and the stable flow of patronage for these services. Their institutional power is tied to this partition.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_priests, agenda_setter,
    institutional, generational, constrained, national).

% Analyze the historical and philosophical implications of this domain partition, often seeking to understand its coherence or lack thereof. They are outside the practical administration of the system.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, theologians_philosophers, observer,
    analytical, civilizational, analytical, universal).

% Historically sought to purify Shinto from Buddhist influence and establish it as the state religion, rejecting the domain partition in favor of a unified, Shinto-centric system. Their perspective was suppressed during periods of forced separation.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_state_shinto_advocates, excluded,
    institutional, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious practice by assigning distinct, non-overlapping functional domains to Kami and Buddhist deities, allowing both traditions to flourish without requiring theological synthesis or conflict.
% TRANSFER_FUNCTION: Transfers spiritual services (e.g., rites of passage, blessings, funerary services) to local communities from Shinto priests and Buddhist clergy, with each tradition specializing in its designated domain.
% ABSENT_VOICES: Theological purists or advocates for a single, unified religious system (e.g., some Meiji-era State Shinto proponents) would object, arguing for ontological consistency or a hierarchical ordering of deities. Their voices were often marginalized or suppressed in favor of practical coexistence.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, the existing religious infrastructure would face immediate chaos. Communities would lose clear guidance on rituals for life and death, and the institutional roles of priests and clergy would become ambiguous, leading to significant reorganization of religious practice and authority.
% FOUNDING_PROBLEM: How to integrate or coexist with newly introduced Buddhism without displacing indigenous Kami worship, and how to provide comprehensive spiritual services for all aspects of life and death.
% FOUNDING_PROBLEM_CORROBORATION: Historians and religious scholars attest that the problem of religious pluralism and functional specialization remains relevant in many contexts, even if the specific historical solution of shinbutsu-shugo has evolved. The continued existence of distinct Shinto shrines and Buddhist temples, often serving the same communities, corroborates the ongoing need for such coordination.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary function is coordination of religious services, with institutional overhead being a relatively small part of the overall benefit. Suppression is also low (0.15) as the system largely relies on cultural acceptance and functional utility rather than overt coercion, though historical periods saw some enforcement of boundaries. Theater ratio is minimal (0.1) as the practices are generally seen as genuinely functional by practitioners. Accessibility collapse is moderate (0.3) because while the system provides a comprehensive framework, alternative spiritual paths or unified theological systems were not entirely foreclosed, but were less accessible within the dominant cultural context. Resistance is very low (0.05) as the arrangement was widely accepted for centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of local communities, this is a highly functional and beneficial arrangement (low extraction). From the perspective of theological purists, it might appear as an unsatisfactory compromise or even an incoherent system. The engine's classification will reflect the practical coordination function, while omegas address the conceptual ambiguities.
 *
 * DIRECTIONALITY LOGIC:
 *   Local communities are beneficiaries, receiving comprehensive spiritual services. Buddhist clergy and Shinto priests are agenda-setters and beneficiaries, as they administer their respective domains and receive patronage. Theologians and philosophers are observers, analyzing the system from an external perspective. Meiji State Shinto advocates are excluded, as their attempts to dismantle this partition were resisted or reversed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_partition,
    'Is the observed domain partition a reflection of a genuine ontological separation between Kami and Buddhas, or is it primarily a functional arrangement for practical coexistence?',
    'Analysis of primary theological texts and ritual practices across different historical periods to discern explicit claims of ontological distinction versus pragmatic divisions of labor.',
    'If primarily ontological, the constraint is closer to a Mountain (natural law of the divine realm). If primarily functional, it is a more constructed Rope, susceptible to shifts in social or political will.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_functional_partition, conceptual, 'Distinguishing between an ontological and a functional basis for the domain partition.').

omega_variable(
    meiji_era_impact_reversibility,
    'To what extent did the Meiji-era Shinbutsu-bunri (separation of Kami and Buddhas) permanently alter the underlying commitment to domain partition, and is the pre-Meiji functional coexistence fully recoverable?',
    'Comparative study of post-Meiji religious practices and institutional structures, examining the persistence of functional separation despite official theological shifts.',
    'If the separation was largely superficial and functional coexistence quickly re-emerged, the constraint is robust. If it caused a lasting rupture, the constraint''s resilience is lower, and its current form is a new, weaker iteration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_era_impact_reversibility, empirical, 'Assessing the long-term impact and reversibility of the Meiji-era forced separation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 300, 0.09).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 600, 0.09).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(shin_be_t300, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 300, 0.22).
narrative_ontology:measurement(shin_be_t600, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 600, 0.23).
narrative_ontology:measurement(shin_be_t900, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 900, 0.24).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(shin_su_t300, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 300, 0.12).
narrative_ontology:measurement(shin_su_t600, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 600, 0.13).
narrative_ontology:measurement(shin_su_t900, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 900, 0.14).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1200, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel, focusing on the functional domain partition. It is linked to sibling readings that emphasize ontological fusion or an incoherent bundle, as these represent competing interpretations of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
