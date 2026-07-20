% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Shinbutsu Domain Partition Commitment
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint story models the domain_partition_reading of the
 *   contested kernel shinbutsu_coexistence_commitment: the historical
 *   arrangement in Japan whereby kami worship and Buddhism maintained a
 *   functional division of existential domains without ontological
 *   unification. Kami governed life, purity, and harvest; Buddhas governed
 *   death, salvation, and the afterlife. The reading holds that this
 *   partition was maintained by popular practice and mutual institutional
 *   recognition rather than by theological resolution or active enforcement.
 *
 * KEY AGENTS:
 *   - buddhist_clergy (institutional/constrained): beneficiaries of the death-afterlife ritual monopoly
 *   - shrine_priests (institutional/constrained): beneficiaries of the life-purity-harvest ritual monopoly
 *   - lay_communities (moderate/mobile): ritual consumers receiving functional clarity
 *   - syncretic_theologians (moderate/constrained): excluded voices advocating ontological unification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.25).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.2).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Domain Partition Commitment").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '797c8ece-abb4-4bda-8358-8ada860c1f96').
narrative_ontology:cs_kernel_codification('797c8ece-abb4-4bda-8358-8ada860c1f96', distributed).
narrative_ontology:cs_authority_grounding('797c8ece-abb4-4bda-8358-8ada860c1f96', practice).
narrative_ontology:cs_interpretation_layer_present('797c8ece-abb4-4bda-8358-8ada860c1f96').
narrative_ontology:cs_reading_relation('797c8ece-abb4-4bda-8358-8ada860c1f96', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('797c8ece-abb4-4bda-8358-8ada860c1f96', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('797c8ece-abb4-4bda-8358-8ada860c1f96', foundational, functional_domain_irreducibility).
narrative_ontology:cs_axiom_status(functional_domain_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('797c8ece-abb4-4bda-8358-8ada860c1f96', functional_domain_irreducibility, empirically_contingent).
narrative_ontology:cs_axiom('797c8ece-abb4-4bda-8358-8ada860c1f96', foundational, practice_based_authority).
narrative_ontology:cs_axiom_status(practice_based_authority, holdable).
narrative_ontology:cs_axiom_grounding('797c8ece-abb4-4bda-8358-8ada860c1f96', practice_based_authority, conventional).
narrative_ontology:cs_reference_frame('797c8ece-abb4-4bda-8358-8ada860c1f96', functional_complementary_domain_order).
narrative_ontology:cs_drift_state('797c8ece-abb4-4bda-8358-8ada860c1f96', meiji_restoration_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('797c8ece-abb4-4bda-8358-8ada860c1f96', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, lay_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer funerals, ancestral rites, and afterlife salvation rituals. Hold a guaranteed institutional domain over death and other-worldly concerns, which is reciprocally recognized by shrine priests. Their livelihood and authority depend on this domain remaining uncontested by Shinto institutions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_clergy, beneficiary,
    institutional, generational, constrained, national).

% Administer harvest festivals, purity rites, and this-worldly blessings. Hold a guaranteed institutional domain over life, fertility, and state ceremony, reciprocally recognized by Buddhist clergy. Their standing relies on the boundary that reserves death-related ritual to temples.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priests, beneficiary,
    institutional, generational, constrained, national).

% Navigate ritual life by routing death-related needs to Buddhist temples and life-state needs to Shinto shrines. Receive functional clarity about which institution to approach for which concern, reducing transactional ambiguity in a multi-religious environment.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, lay_communities, beneficiary,
    moderate, biographical, mobile, regional).

% Advocate for ontological unification of kami and Buddhas through doctrines such as honji suijaku. Their theological synthesis is acknowledged in discourse but operationally subordinated to the functional domain partition; they do not set the practical boundaries of ritual life.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, syncretic_theologians, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the coexistence of two distinct religious institutions by allocating exclusive ritual domains: Buddhism manages death, ancestors, and afterlife salvation while Shinto manages life, purity, harvest, and state ceremony. Prevents zero-sum competition for religious authority and reduces transactional ambiguity for lay practitioners in a multi-religious society.
% TRANSFER_FUNCTION: Moves ritual patronage, social legitimacy, and institutional resources between domains: lay communities and the state direct death-related resources to Buddhist temples and life-state resources to Shinto shrines, while both priesthoods accrue domain-specific authority and economic support.
% ABSENT_VOICES: Syncretic theologians advocating ontological unification (honji suijaku) and state reformers seeking a single national cult are operationally marginalized during the constraint's active interval. Their programs are acknowledged but do not override the functional partition maintained by popular practice.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, Buddhist temples and Shinto shrines would compete directly for the same ritual functions, lay practitioners would lose clear routing between institutions, and the complementary institutional ecology would collapse into either syncretic fusion, sectarian conflict, or state-imposed separation.
% FOUNDING_PROBLEM: How do imported Buddhist institutions and indigenous kami worship coexist without mutual destruction or absorption, given incompatible ontologies and overlapping ritual claims in the Japanese archipelago?
% FOUNDING_PROBLEM_CORROBORATION: Imperial court diaries, provincial administration records, and independent historiography from outside the beneficiary priesthoods attest that the functional partition predated its theological justification and was maintained as a practical arrangement. Modern academic religious studies corroborate that the problem of coexistence remained salient throughout the interval.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.25, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.25) because the domain partition solves a genuine coordination problem with minimal asymmetric transfer; suppression is low (0.20) because the arrangement relied on mutual institutional interest and customary practice rather than active coercion. Theater_ratio is low (0.15) because boundary maintenance was functionally real and not primarily performative. Accessibility_collapse is moderate (0.40) because alternatives such as syncretic fusion or sectarian exclusivism existed and were tolerated, but were operationally subordinated to the partition. Resistance is low (0.15) because both beneficiary institutions and lay communities accepted the arrangement as mutually beneficial.
 *
 * PERSPECTIVAL GAP:
 *   The Buddhist clergy and shrine priests experience the constraint as a mutually beneficial division of labor that preserves their institutional domains and reduces conflict. Lay communities experience it as a low-friction ritual routing system. Analytical observers may classify it as coordination, while competing scholarly readings (syncretic fusion, incoherent bundle) see the same historical evidence differently. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist clergy and shrine priests are both beneficiaries with low directionality (the constraint subsidizes their institutional domains through reciprocal recognition). Lay communities are near-symmetric beneficiaries with low directionality (they gain functional clarity). Syncretic theologians are excluded and bear no direct extraction, though their exclusion from agenda-setting is structurally necessary to maintain the partition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a piton because it maintained a live coordination function throughout its interval and was terminated by external political force (Meiji restoration) rather than institutional atrophy. It is not a snare because there are no identifiable victims of concentrated extraction. It is not a scaffold because it carried no sunset clause and was not designed as a transitional measure. The rope classification captures the stable, mutually beneficial institutional complementarity without forcing it into extraction or inertia categories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_vs_syncretic_framing,
    'Is the shinbutsu coexistence commitment best understood as a functional domain partition (this reading) or as an ontological syncretic fusion (syncretic_fusion_reading)?',
    'Archaeological and documentary analysis of institutional records to determine whether ritual domains were strictly partitioned or cross-penetrated in practice, and whether honji suijaku operated as theological superstructure or practical guide.',
    'If syncretic fusion governed practice, the constraint''s coordination type shifts toward theological absorption and beneficiary structure concentrates under Buddhist institutions; if domain partition governed, the current rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_vs_syncretic_framing, empirical, 'Empirical ambiguity between functional partition and syncretic fusion readings of the same kernel.').

omega_variable(
    coherence_vs_incoherence_of_kernel,
    'Does the domain partition reading impose a false coherence on an historically incoherent bundle of practices (as the incoherent_bundle_reading claims)?',
    'Prosopographical study of institutional records to test whether boundary maintenance was systematic or ad hoc, and whether popular practice was unified enough to constitute a coherent commitment.',
    'If the bundle reading is correct, the constraint dissolves into an extraction pattern of institutional power without genuine coordination function; if the partition reading holds, the rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_incoherence_of_kernel, conceptual, 'Conceptual ambiguity between analytical coherence and historical incoherence.').

omega_variable(
    natural_emergence_vs_institutional_construction,
    'Did the domain partition emerge naturally from the functional division of ritual labor, or was it actively constructed by Buddhist and Shinto institutions to secure domain monopolies?',
    'Comparative analysis with other religious contact situations to distinguish emergent functionalism from elite boundary-work, combined with study of patronage flows between court, temples, and shrines.',
    'If actively constructed for monopoly, the constraint''s extractiveness is higher than authored and the coordination function risks being read as cover for rent-seeking; if emergent, the rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_emergence_vs_institutional_construction, empirical, 'Whether the partition was emergent coordination or constructed extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t20, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t40, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t60, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t80, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 80, 0.13).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t100, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(shinbutsu_domain_partition_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t20, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t40, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t60, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 60, 0.23).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t80, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 80, 0.24).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t100, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 100, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_coexistence_commitment__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is the domain_partition_reading of kernel shinbutsu_coexistence_commitment. It decomposes the colloquial label 'shinbutsu-shugo' into a structurally precise claim: the historical arrangement was a functionally partitioned coexistence commitment, not an ontological fusion nor an incoherent bundle. Sibling constraints instantiate the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
