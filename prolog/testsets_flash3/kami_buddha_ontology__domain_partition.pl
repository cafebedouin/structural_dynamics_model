% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami and Buddha Ontological Domain Partition
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes the 'domain partition' reading of kami and
 *   buddha ontology in Japan, where kami are distinct entities associated
 *   with life and purity (Shinto), and buddhas with death and impurity
 *   (Buddhism). This reading emphasizes functional complementarity without
 *   ontological fusion or hierarchy, allowing both traditions to thrive by
 *   serving different aspects of human experience. It emerged particularly
 *   strongly after the Meiji Restoration's Shinbutsu Bunri (separation of
 *   kami and buddhas) policies, which sought to disentangle the previously
 *   syncretic Shinbutsu-shūgō.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.15).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.2).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.15).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami and Buddha Ontological Domain Partition").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/philosophy_of_religion/japanese_cultural_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '2a6f0e2b-6fb7-44db-af73-eba1bb7f4553').
narrative_ontology:cs_kernel_codification('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', implicit).
narrative_ontology:cs_authority_grounding('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', practice).
narrative_ontology:cs_interpretation_layer_present('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553').
narrative_ontology:cs_reading_relation('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', foundational, kami_buddha_ontological_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', kami_buddha_ontological_distinctness, deontological).
narrative_ontology:cs_axiom('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', foundational, functional_complementarity_without_hierarchy).
narrative_ontology:cs_axiom_status(functional_complementarity_without_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', functional_complementarity_without_hierarchy, conventional).
narrative_ontology:cs_reference_frame('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', post_meiji_functional_separation).
narrative_ontology:cs_drift_state('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2a6f0e2b-6fb7-44db-af73-eba1bb7f4553', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_clergy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, japanese_households).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, shinto_purity_doctrine).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, buddhist_impermanence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Shinto rituals, maintains shrines, and interprets kami ontology. Benefits from the clear demarcation of its domain (life, purity) which ensures its continued relevance and funding. Would face existential crisis if kami were subsumed by buddhas.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Administers Buddhist rites, maintains temples, and interprets buddha ontology. Benefits from the clear demarcation of its domain (death, impurity, deceased) which ensures its continued relevance and funding. Would face existential crisis if buddhas were subsumed by kami.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Navigate life events (births, weddings, festivals) through Shinto and death events (funerals, ancestral rites) through Buddhism. Benefit from a clear, complementary division of religious labor that provides comprehensive spiritual coverage without conflict. Can choose to emphasize one tradition over the other, but typically engage both.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, japanese_households, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the historical and philosophical development of kami and buddha concepts in Japan. Their work often highlights the complexities and historical shifts in these ontological distinctions, sometimes challenging the neat domain partition.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, religious_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, complementary division of religious labor and ontological domains for kami and buddhas, allowing two distinct religious traditions to coexist and serve different aspects of human experience (life/death) without direct conflict or hierarchical subsumption.
% TRANSFER_FUNCTION: Transfers spiritual services and ritual authority for life-affirming events to Shinto institutions and for death-related events to Buddhist institutions, from the general populace to the respective clergy.
% ABSENT_VOICES: Historical proponents of Honji Suijaku theory (who argued for the ontological identity of kami and buddhas) are absent from this reading, as their view directly contradicts the domain partition. Their arguments would challenge the distinctness of the entities.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, the clear functional complementarity would collapse. Shinto and Buddhist institutions would either enter direct competition for all life and death rituals, or one would attempt to subsume the other, leading to significant institutional and cultural reorganization in Japan.
% FOUNDING_PROBLEM: How to integrate or differentiate indigenous Japanese kami worship with the imported Buddhist tradition, avoiding conflict and ensuring the continued relevance of both in a comprehensive spiritual system.
% FOUNDING_PROBLEM_CORROBORATION: Japanese households and both Shinto and Buddhist clergy continue to operate under this functional division, corroborating its live status through ongoing practice. Religious scholars also attest to its historical and contemporary significance as a working model of religious coexistence.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).
:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint primarily facilitates coordination between two religious systems, rather than extracting resources. Suppression is also low (0.2) as adherence is largely voluntary and culturally embedded, rather than coercively enforced. The 'separation' was initially enforced by the state (Meiji era), but its persistence today is more due to cultural habit and institutional inertia. Theater ratio is very low (0.05) as the functional division is genuinely operative for most practitioners. Accessibility collapse is high (0.8) because once this framework is adopted, alternative ways of understanding the kami-buddha relationship (e.g., fusion) become conceptually difficult to integrate into practice.
 *
 * PERSPECTIVAL GAP:
 *   While the clergy and households experience this as a functional, beneficial coordination, scholars might highlight the historical contingency of this 'partition' and the suppression of alternative, more syncretic readings (like Honji Suijaku) during the Meiji era. The constraint is claimed as a Rope from the perspective of its current functional operation, but its historical imposition involved elements of Snare-like state enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Both Shinto and Buddhist clergy are beneficiaries, as the domain partition ensures their distinct institutional roles and revenue streams. Japanese households are also beneficiaries, gaining a clear, comprehensive spiritual framework for life and death. Religious scholars act as observers, analyzing the system without direct benefit or cost from its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_contingency_vs_naturalness,
    'Is the domain partition a natural, inherent feature of Japanese religious thought, or a historically contingent construct, particularly influenced by the Meiji-era Shinbutsu Bunri policies?',
    'Detailed historical and textual analysis of pre-Meiji Shinbutsu-shūgō practices and theological arguments, assessing the degree of ontological distinction prior to state intervention.',
    'If historically contingent, the ''naturalness'' of the partition is undermined, suggesting it could be re-negotiated or re-integrated. If inherent, its persistence is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_vs_naturalness, empirical, 'Examines the historical origins and ''naturalness'' of the kami-buddha domain partition.').

omega_variable(
    functional_complementarity_vs_ontological_fusion,
    'Does the observed functional complementarity (Shinto for life, Buddhism for death) imply an underlying ontological distinction, or could it coexist with a more fused or hierarchical ontological understanding (e.g., Honji Suijaku)?',
    'Philosophical analysis of the logical relationship between functional division and ontological status, and comparative study of other religious traditions with similar functional divisions but different ontological claims.',
    'If functional complementarity does not necessitate ontological distinction, then the ''domain_partition'' reading is conceptually weaker and more vulnerable to alternative interpretations like ''honji_suijaku_monism''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_complementarity_vs_ontological_fusion, conceptual, 'Investigates whether functional division logically requires ontological distinction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t1868, kami_buddha_ontology__domain_partition, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(kami_tr_t1900, kami_buddha_ontology__domain_partition, theater_ratio, 1900, 0.06).
narrative_ontology:measurement(kami_tr_t1945, kami_buddha_ontology__domain_partition, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(kami_tr_t1980, kami_buddha_ontology__domain_partition, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(kami_tr_t2024, kami_buddha_ontology__domain_partition, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(kami_be_t1868, kami_buddha_ontology__domain_partition, base_extractiveness, 1868, 0.1).
narrative_ontology:measurement(kami_be_t1900, kami_buddha_ontology__domain_partition, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(kami_be_t1945, kami_buddha_ontology__domain_partition, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(kami_be_t1980, kami_buddha_ontology__domain_partition, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(kami_be_t2024, kami_buddha_ontology__domain_partition, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t1868, kami_buddha_ontology__domain_partition, suppression_requirement, 1868, 0.3).
narrative_ontology:measurement(kami_su_t1900, kami_buddha_ontology__domain_partition, suppression_requirement, 1900, 0.25).
narrative_ontology:measurement(kami_su_t1945, kami_buddha_ontology__domain_partition, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(kami_su_t1980, kami_buddha_ontology__domain_partition, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(kami_su_t2024, kami_buddha_ontology__domain_partition, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, japanese_funeral_rites).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, shinto_shrine_maintenance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
