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
 *   human_readable: Kami and Buddhas: Domain Partition Reading
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the 'domain partition' reading of
 *   Shinbutsu-shūgō, where Kami and Buddhas are understood to govern
 *   distinct, non-overlapping spiritual domains (this-world vs. afterlife).
 *   This reading emphasizes functional coexistence and institutional
 *   separation, rather than ontological fusion or incoherent bundling. It
 *   posits that syncretism is a pragmatic arrangement for managing religious
 *   pluralism, not a statement about the ultimate nature of the deities. The
 *   constraint is claimed as a Rope because it facilitates coordination
 *   between distinct religious systems for mutual benefit, with low inherent
 *   extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.2).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.1).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami and Buddhas: Domain Partition Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '0abdbef2-9b68-4913-9c9c-41d9160c8f40').
narrative_ontology:cs_kernel_codification('0abdbef2-9b68-4913-9c9c-41d9160c8f40', distributed).
narrative_ontology:cs_authority_grounding('0abdbef2-9b68-4913-9c9c-41d9160c8f40', practice).
narrative_ontology:cs_interpretation_layer_present('0abdbef2-9b68-4913-9c9c-41d9160c8f40').
narrative_ontology:cs_reading_relation('0abdbef2-9b68-4913-9c9c-41d9160c8f40', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('0abdbef2-9b68-4913-9c9c-41d9160c8f40', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('0abdbef2-9b68-4913-9c9c-41d9160c8f40', foundational, kami_govern_this_world_buddhas_afterlife).
narrative_ontology:cs_axiom_status(kami_govern_this_world_buddhas_afterlife, holdable).
narrative_ontology:cs_axiom_grounding('0abdbef2-9b68-4913-9c9c-41d9160c8f40', kami_govern_this_world_buddhas_afterlife, conventional).
narrative_ontology:cs_axiom('0abdbef2-9b68-4913-9c9c-41d9160c8f40', foundational, functional_coexistence_is_pragmatic).
narrative_ontology:cs_axiom_status(functional_coexistence_is_pragmatic, holdable).
narrative_ontology:cs_axiom_grounding('0abdbef2-9b68-4913-9c9c-41d9160c8f40', functional_coexistence_is_pragmatic, instrumental).
narrative_ontology:cs_reference_frame('0abdbef2-9b68-4913-9c9c-41d9160c8f40', pre_meiji_functional_separation).
narrative_ontology:cs_drift_state('0abdbef2-9b68-4913-9c9c-41d9160c8f40', contemporary_scholarly_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0abdbef2-9b68-4913-9c9c-41d9160c8f40', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, local_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, religious_pluralism_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, functional_coexistence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Shinto rituals and shrines, focusing on this-worldly blessings and purity. Benefits from a clear division of labor that avoids direct competition with Buddhism for spiritual authority over the afterlife.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood, agenda_setter,
    organized, generational, mobile, national).

% Administers Buddhist temples and rites, focusing on funerary practices, ancestral veneration, and salvation in the afterlife. Benefits from a clear domain that allows it to specialize in its core spiritual offerings without direct conflict with Shinto.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy, agenda_setter,
    organized, generational, mobile, national).

% Participate in both Shinto and Buddhist practices, benefiting from a system where different spiritual needs are met by distinct, yet coexisting, traditions. They experience the partition as a natural division of religious labor.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, local_communities, beneficiary,
    moderate, biographical, constrained, local).

% Analyze the historical and theological development of Shinbutsu-shūgō, often seeking to delineate distinct ontological or functional domains. Their analysis reinforces the idea of a partition, even if historically fluid.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, scholarly_interpreters, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious practice by assigning distinct, non-overlapping spiritual domains (this-worldly purity and blessings to Kami; afterlife salvation and funerary rites to Buddhas), allowing both traditions to flourish without direct competition for adherents or authority.
% TRANSFER_FUNCTION: Facilitates the flow of spiritual services and ritual patronage to distinct religious institutions based on the specific needs (e.g., Shinto for festivals and blessings, Buddhism for funerals and ancestral rites).
% ABSENT_VOICES: Strict monotheistic or exclusivist religious traditions, which would challenge the very premise of functional coexistence and domain partitioning, are absent from the internal discourse of this reading.
% DISAPPEARANCE_RATIONALE: If the understanding of Kami and Buddhas as governing separate domains vanished, the existing institutional structures of Shinto shrines and Buddhist temples would face immediate ontological and functional crises, leading to significant redefinition or collapse of their roles and patronage. Communities would lose a clear framework for religious practice.
% FOUNDING_PROBLEM: The historical challenge of integrating or distinguishing indigenous Kami worship with the imported Buddhist tradition without one subsuming the other, ensuring the continued relevance and institutional stability of both.
% FOUNDING_PROBLEM_CORROBORATION: Scholarly interpreters and historical records attest to the ongoing challenge of managing religious pluralism and institutional boundaries in Japan, corroborating that the problem of functional coexistence remains relevant, even if its specific manifestations evolve.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.2) because the constraint primarily serves to coordinate distinct religious practices, minimizing direct competition and allowing both Shinto and Buddhism to maintain their institutional integrity and patronage. Suppression is also low (0.1) as this reading emphasizes a natural, functional division rather than coercive enforcement. Theater ratio is minimal (0.05) as the functional separation is largely genuine within this interpretive framework. The historical measurements show a stable, low-extraction profile, consistent with a coordination mechanism that has adapted over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the arrangement is a functional and beneficial coordination. Other readings (e.g., syncretic_fusion_reading) would emphasize a deeper ontological unity, while the incoherent_bundle_reading would see the arrangement as a product of state-enforced institutional drift. This constraint focuses solely on the domain partition perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the Shinto priesthood and Buddhist clergy act as agenda-setters and beneficiaries, as the domain partition allows them to maintain their distinct institutional roles and authority. Local communities are beneficiaries, gaining access to a comprehensive spiritual framework. Scholarly interpreters act as observers, often reinforcing this partition through their analytical work. No party is a victim, as the arrangement is seen as mutually beneficial.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vs_doctrinal_partition,
    'To what extent does the ''domain partition'' reflect historical institutional arrangements versus a coherent theological doctrine?',
    'Comparative historical analysis of pre-Meiji religious practices and explicit theological treatises from both Shinto and Buddhist traditions. Examination of state policies (e.g., Shinbutsu-bunri) that enforced separation.',
    'If primarily historical/institutional, the ''naturalness'' of the partition is reduced, potentially shifting the constraint towards a more constructed (Tangled Rope) type. If doctrinally robust, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_vs_doctrinal_partition, empirical, 'Ambiguity between historical practice and explicit theological grounding of the domain partition.').

omega_variable(
    functional_vs_ontological_distinction,
    'Is the distinction between Kami and Buddhas purely functional (different roles) or does it imply a deeper ontological separation?',
    'Analysis of philosophical texts and ritual practices for explicit statements or implicit assumptions about the fundamental nature of Kami and Buddhas. Comparison with the ''syncretic_fusion_reading'' to identify points of irreconcilable difference.',
    'If a deeper ontological separation is implied, the ''Mountain'' aspect of the constraint (irreducible difference) is strengthened. If purely functional, it remains a ''Rope'' of coordination, but with less ''natural'' grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_vs_ontological_distinction, conceptual, 'The nature of the distinction between Kami and Buddhas: functional or ontological.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 500, 0.04).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1500, 0.05).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(shin_be_t500, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 500, 0.18).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1000, 0.2).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1500, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(shin_su_t500, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 500, 0.09).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1500, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_ontological_substrate' kernel. It emphasizes a functional domain partition between Kami and Buddhas, contrasting with readings that posit ontological fusion or institutional incoherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
