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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Coexistence: Domain Partition Reading
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the 'domain partition' reading of Shinbutsu
 *   coexistence, where Kami (associated with life, purity, harvest, local
 *   protection) and Buddhist deities (associated with death, salvation,
 *   afterlife, universal truth) govern separate, complementary existential
 *   domains without requiring deep ontological unification. This reading
 *   emphasizes functional coexistence and boundary maintenance, with popular
 *   religious practice often serving as the de facto authority for this
 *   arrangement. It contrasts with readings that emphasize ontological fusion
 *   (honji suijaku) or view the entire system as an incoherent bundle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.2).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.1).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Coexistence: Domain Partition Reading").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '84b94283-139e-4daa-ad84-018dde2e2386').
narrative_ontology:cs_kernel_codification('84b94283-139e-4daa-ad84-018dde2e2386', implicit).
narrative_ontology:cs_authority_grounding('84b94283-139e-4daa-ad84-018dde2e2386', practice).
narrative_ontology:cs_interpretation_layer_present('84b94283-139e-4daa-ad84-018dde2e2386').
narrative_ontology:cs_reading_relation('84b94283-139e-4daa-ad84-018dde2e2386', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('84b94283-139e-4daa-ad84-018dde2e2386', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('84b94283-139e-4daa-ad84-018dde2e2386', foundational, functional_differentiation_is_primary).
narrative_ontology:cs_axiom_status(functional_differentiation_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('84b94283-139e-4daa-ad84-018dde2e2386', functional_differentiation_is_primary, conventional).
narrative_ontology:cs_axiom('84b94283-139e-4daa-ad84-018dde2e2386', foundational, ontological_unification_is_not_required).
narrative_ontology:cs_axiom_status(ontological_unification_is_not_required, holdable).
narrative_ontology:cs_axiom_grounding('84b94283-139e-4daa-ad84-018dde2e2386', ontological_unification_is_not_required, deontological).
narrative_ontology:cs_reference_frame('84b94283-139e-4daa-ad84-018dde2e2386', pre_meiji_functional_coexistence).
narrative_ontology:cs_drift_state('84b94283-139e-4daa-ad84-018dde2e2386', contemporary_religious_landscape, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('84b94283-139e-4daa-ad84-018dde2e2386', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, local_communities).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrines).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, functional_pluralism).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, cultural_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a comprehensive religious system that addresses both daily life (Kami) and death/afterlife (Buddhas), providing cultural continuity and spiritual comfort. Their identity is deeply intertwined with these practices.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, local_communities, beneficiary,
    organized, generational, identity_locked, local).

% Benefit from a clear role in funerary rites, ancestor veneration, and salvation, without direct competition with Shinto shrines over life-cycle events. They maintain their institutional identity and receive patronage for these services.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temples, beneficiary,
    institutional, generational, constrained, national).

% Benefit from a clear role in agricultural rites, purification, and local protection, without direct competition with Buddhist temples over death-related practices. They maintain their institutional identity and receive patronage for these services.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrines, beneficiary,
    institutional, generational, constrained, national).

% Analyze and interpret the historical and doctrinal relationships between Shinto and Buddhism. They may debate the coherence or historical accuracy of the domain partition but do not directly participate in its maintenance or enforcement.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, theologians_and_scholars, observer,
    analytical, generational, analytical, global).

% Historically attempted to enforce a strict separation (Shinbutsu Bunri) to elevate Shinto as a national religion, thereby disrupting the traditional domain partition. Their actions highlight the constructed nature of the partition but are external to its internal logic.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_state, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious life of Japanese society by assigning distinct, complementary roles to Shinto (life, purity, local) and Buddhism (death, salvation, universal), allowing both traditions to flourish without requiring deep theological synthesis.
% TRANSFER_FUNCTION: Transfers spiritual services and cultural meaning related to life events (Shinto) and death/afterlife (Buddhism) to local communities, ensuring comprehensive religious coverage without overlap or conflict.
% ABSENT_VOICES: Strict monotheistic or exclusivist religious traditions would object to the pluralistic and functionally divided nature of this arrangement, arguing for a single, ontologically unified truth. However, such voices are largely absent from the historical Japanese religious landscape that shaped this constraint.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, the clear functional roles of Shinto shrines and Buddhist temples would collapse, leading to confusion, competition, and a loss of cultural continuity in how life and death events are ritually managed. Religious identity and practice would undergo significant reorganization.
% FOUNDING_PROBLEM: The need to integrate a newly introduced universalist religion (Buddhism) with indigenous animistic beliefs (Shinto) in a way that maintained social harmony and provided comprehensive spiritual services for all aspects of life and death.
% FOUNDING_PROBLEM_CORROBORATION: Historians and religious scholars widely corroborate the historical problem of integrating new and indigenous religious systems. The continued functional specialization of shrines and temples in contemporary Japan, despite periods of forced separation, attests to the enduring 'liveness' of the need for such coordination, even if the specific mechanisms evolve.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).

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
 *   The constraint is classified as a Rope because it facilitates coordination between distinct religious traditions, allowing them to coexist and serve different societal functions without significant conflict or doctrinal extraction. Extractiveness is low (0.2) as no single party systematically extracts rents from this partition; rather, it enables both traditions to thrive. Suppression is low (0.1) because the partition is largely maintained by cultural practice and mutual recognition, not active coercion. Theater ratio is low (0.05) as the functional separation is generally accepted and enacted in practice. Accessibility collapse is high (0.8) because, within this framework, the idea of a single, unified religious system that fully integrates both domains is largely foreclosed by the established functional division.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of practitioners, this domain partition is a natural and beneficial arrangement, allowing for a rich religious life. From a purely theological perspective, the lack of ontological unification might be seen as a conceptual challenge, but within this reading, practical coexistence takes precedence over strict doctrinal consistency.
 *
 * DIRECTIONALITY LOGIC:
 *   Local communities, Buddhist temples, and Shinto shrines are all beneficiaries (d near 0.0-0.1). Local communities benefit from a comprehensive religious system that addresses both worldly and otherworldly concerns. Temples and shrines benefit from clearly defined roles and a lack of direct competition over core functions. There are no clear victims, as the system is designed for mutual benefit and functional specialization.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_vs_fusion_ambiguity,
    'Is the relationship between Kami and Buddhist deities truly a domain partition, or is it better understood as an ontological fusion (honji suijaku) or an incoherent bundle?',
    'Analysis of primary theological texts and popular religious practices across different historical periods, focusing on explicit statements of ontological status versus functional separation.',
    'If fusion is dominant, the constraint shifts towards a more unified, potentially more extractive (if fusion is imposed) or more robust (if fusion is genuinely accepted) structure. If incoherent, the constraint''s stability is lower than this reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_vs_fusion_ambiguity, conceptual, 'Ambiguity between domain partition, syncretic fusion, and incoherent bundling as the primary mode of Shinbutsu coexistence.').

omega_variable(
    popular_practice_as_authority_ambiguity,
    'To what extent does popular religious practice genuinely constitute the authority for this domain partition, versus being a consequence of elite theological or political imposition?',
    'Sociological and historical research into the agency of common practitioners in shaping religious norms, as opposed to top-down directives from religious institutions or state powers.',
    'If practice is genuinely authoritative, the constraint is more resilient and less extractive. If imposed, the constraint is more fragile and potentially more extractive, relying on suppression of alternative framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(popular_practice_as_authority_ambiguity, empirical, 'The true source of authority for the domain partition: popular practice vs. elite imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 50, 0.04).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(shin_be_t50, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(shin_be_t100, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 100, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(shin_su_t50, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 50, 0.09).
narrative_ontology:measurement(shin_su_t100, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel, focusing on the functional domain partition. It is linked to sibling readings that emphasize ontological fusion or an incoherent bundle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
