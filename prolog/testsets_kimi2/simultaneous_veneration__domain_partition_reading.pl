% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition Reading
 *   domain: religious/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint instantiates the domain_partition_reading of the
 *   simultaneous_veneration kernel in Japanese religious history. It treats
 *   the long-standing practice of honoring both kami and buddhas as a
 *   coherent coordination mechanism: each class of being is assigned a
 *   non-overlapping functional domain (kami for this-worldly prosperity and
 *   protection, buddhas for afterlife salvation and ancestor
 *   memorialization). The reading asserts that simultaneous veneration was
 *   not theological confusion or hierarchical subordination, but
 *   domain-appropriate specialization that allowed practitioners to engage
 *   both traditions without contradiction. As a rope, the constraint solves a
 *   genuine collective-action problem (ritual jurisdictional conflict) with
 *   minimal coercive overhead and no identifiable extraction from one party
 *   by another.
 *
 * KEY AGENTS:
 *   - lay_practitioners (moderate/constrained): Coordinate ritual participation across shrine and temple domains, benefiting from reduced cognitive dissonance.
 *   - temple_shrine_institutions (institutional/constrained): Administer the jurisdictional boundaries and maintain doctrinal stability.
 *   - shinto_priests (moderate/constrained): Specialists in the this-worldly domain, coordinated with Buddhist clergy through stable partition.
 *   - buddhist_clergy (moderate/constrained): Specialists in the afterlife domain, benefiting from recognized ritual jurisdiction.
 *   - exclusivist_reformers (moderate/mobile): Argue against syncretism and are excluded from the pre-modern consensus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.12).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Kami-Buddha Domain Partition Reading").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '773ce154-14cd-414f-a6e6-b092535532c1').
narrative_ontology:cs_kernel_codification('773ce154-14cd-414f-a6e6-b092535532c1', distributed).
narrative_ontology:cs_authority_grounding('773ce154-14cd-414f-a6e6-b092535532c1', practice).
narrative_ontology:cs_interpretation_layer_present('773ce154-14cd-414f-a6e6-b092535532c1').
narrative_ontology:cs_reading_relation('773ce154-14cd-414f-a6e6-b092535532c1', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('773ce154-14cd-414f-a6e6-b092535532c1', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('773ce154-14cd-414f-a6e6-b092535532c1', foundational, domain_purview_legitimacy).
narrative_ontology:cs_axiom_status(domain_purview_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('773ce154-14cd-414f-a6e6-b092535532c1', domain_purview_legitimacy, theological).
narrative_ontology:cs_axiom('773ce154-14cd-414f-a6e6-b092535532c1', foundational, syncretic_jurisdictional_partition).
narrative_ontology:cs_axiom_status(syncretic_jurisdictional_partition, holdable).
narrative_ontology:cs_axiom_grounding('773ce154-14cd-414f-a6e6-b092535532c1', syncretic_jurisdictional_partition, conventional).
narrative_ontology:cs_reference_frame('773ce154-14cd-414f-a6e6-b092535532c1', dual_domain_ritual_system).
narrative_ontology:cs_drift_state('773ce154-14cd-414f-a6e6-b092535532c1', meiji_shinbutsu_bunri, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('773ce154-14cd-414f-a6e6-b092535532c1', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, lay_practitioners).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, temple_shrine_institutions).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, shinto_priests).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, buddhist_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary Japanese who engage in both Shinto rites for this-worldly concerns and Buddhist rites for ancestors and afterlife. They benefit from clear jurisdictional boundaries that reduce cognitive conflict between traditions. Exit would require rejecting one tradition entirely, which is socially and familially costly but not violently suppressed.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, lay_practitioners, beneficiary,
    moderate, biographical, constrained, national).

% Buddhist temples and Shinto shrines that administer the respective ritual domains. They maintain doctrinal and practical boundaries, ensuring funerary Buddhism does not compete with shrine-based harvest festivals. Their authority rests on stable jurisdictional partition and mutual non-interference.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, temple_shrine_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, temple_shrine_institutions, beneficiary).

% Specialists in kami ritual who derive institutional role from the this-worldly domain. They coordinate with Buddhist clergy for community religious life, conceding funerary and soteriological functions. Exit would require abandoning the syncretic system for state-sponsored exclusivity.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shinto_priests, beneficiary,
    moderate, generational, constrained, national).

% Specialists in Buddhist soteriology who administer funerals and memorial rites. They benefit from stable domain partition that assigns afterlife jurisdiction to Buddhism without requiring suppression of kami worship. Exit would mean asserting exclusive Buddhist control over all ritual, which invites institutional conflict.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, buddhist_clergy, beneficiary,
    moderate, generational, constrained, national).

% Religious reformers and later Meiji-era ideologues who argue for exclusive commitment to either Shinto or Buddhism. They are excluded from the pre-modern syncretic consensus and would dismantle the domain partition in favor of unitary religious identity.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, exclusivist_reformers, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves potential conflict between Shinto and Buddhist ritual obligations by assigning each tradition a non-overlapping functional domain: kami govern this-worldly prosperity and protection, buddhas govern afterlife salvation and ancestor memorialization. This allows practitioners to engage both traditions without theological contradiction or institutional competition.
% TRANSFER_FUNCTION: Moves ritual attention and material offerings from practitioners to the appropriate institutional domain depending on the concernâharvest, illness, and protection to shrines; death, ancestry, and salvation to temples. The transfer is domain-matched and reciprocal, not asymmetric extraction.
% ABSENT_VOICES: Exclusivist reformers who reject syncretism entirely, and modern scholars of religion who read the partition as an elite rationalization rather than folk practice, are largely absent from the pre-modern discourse that stabilized the constraint.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, the stable arrangement of Japanese religious practice would reorganize: either sectarian competition over ritual jurisdiction would intensify, or a hierarchical fusion (honji-suijaku) would subsume one tradition under the other. The practical coordination that allowed simultaneous veneration to proceed without conflict would collapse.
% FOUNDING_PROBLEM: How to maintain devotion to both indigenous kami and imported buddhas without theological incoherence or institutional conflict over ritual jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: Practitioner testimony, ritual manuals (e.g., RyÅbu ShintÅ texts), and shrine-temple administrative records from the Heian through Edo periods attest to the live coordination problem and the domain-based solution, corroborated by modern ethnohistorians working outside the benefiting institutions.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.08, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the domain partition operates as mutual coordination rather than asymmetric transfer. Suppression is low (0.12) because the constraint persists through shared practice and institutional convention rather than active coercion; alternatives (exclusive Shinto, exclusive Buddhism, ontological fusion) were historically available and sometimes practiced. Theater ratio is low (0.10) because the ritual functions assigned to each domain are operationally genuine. Accessibility collapse is moderate (0.35): once the partition is understood, cognitively alternatives remain visible, though socially embedded. Resistance is low (0.15) because the arrangement is broadly Pareto-improving for the coordinated parties. Temporal measurements show flat, stable operation consistent with a durable coordination equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is narrow for this rope. From the lay practitioner seat, the constraint appears as convenient ritual clarity. From the institutional seat, it appears as stable jurisdictional order. Both experience low extraction and benefit from coordination. The exclusivist reformer seat, if admitted, would experience the constraint as suppressive of unitary religious truth, but this seat is historically excluded from the pre-modern consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are either beneficiaries (lay practitioners, priests, clergy) or agenda-setters (temple-shrine institutions) who maintain the coordination. There are no victims because the constraint does not extract from one agent to benefit another; the 'transfer' is of ritual attention to the appropriate domain, matched by reciprocal service. Directionality for all seated agents is near the beneficiary end (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy because its founding problemâhow to coordinate dual ritual obligations without institutional conflictâremains live throughout the interval. There is no evidence that the arrangement persisted by inertia after its function decayed; rather, it was actively disrupted by external state action (Meiji shinbutsu bunri) while still functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_historical_accuracy,
    'Does the domain partition reading accurately describe historical practitioner belief, or is it a retrospective scholarly rationalization imposed on heterogeneous folk practice?',
    'Ethnohistorical analysis of pre-modern practitioner testimony, folk ritual manuals, and village-level religious records to determine whether domain-specialization was explicit in practitioner cognition or inferred by later analysts.',
    'If the partition was largely scholarly and not practitioner-held, the actual constraint may have been closer to the pragmatic_incoherence_reading (higher extraction via cognitive dissonance) or a piton of unexamined custom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_historical_accuracy, empirical, 'Whether the domain partition was live in practitioner belief or retrospective').

omega_variable(
    kernel_reading_relation_ambiguity,
    'Can the domain partition reading coexist with ontological fusion in a single theological framework, or do they represent mutually exclusive commitments?',
    'Doctrinal analysis of RyÅbu ShintÅ and medieval Tendai/Shingon texts to determine whether functional domain distinction was held alongside or in contradiction to honji-suijaku ontology.',
    'If they cannot coexist, the kernel''s actual history involved more theological contestation than the rope model suggests, potentially raising extraction through doctrinal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_ambiguity, conceptual, 'Whether domain partition and ontological fusion are mutually exclusive readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(simu_tr_t20, simultaneous_veneration__domain_partition_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(simu_tr_t40, simultaneous_veneration__domain_partition_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(simu_tr_t60, simultaneous_veneration__domain_partition_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(simu_tr_t80, simultaneous_veneration__domain_partition_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(simu_tr_t100, simultaneous_veneration__domain_partition_reading, theater_ratio, 100, 0.11).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(simu_be_t20, simultaneous_veneration__domain_partition_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(simu_be_t40, simultaneous_veneration__domain_partition_reading, base_extractiveness, 40, 0.09).
narrative_ontology:measurement(simu_be_t60, simultaneous_veneration__domain_partition_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(simu_be_t80, simultaneous_veneration__domain_partition_reading, base_extractiveness, 80, 0.09).
narrative_ontology:measurement(simu_be_t100, simultaneous_veneration__domain_partition_reading, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The simultaneous_veneration kernel decomposes into three structurally distinct readings. This reading (domain_partition) treats the kernel as a coordination rope across dual ritual domains with independent Îµ values. The ontological_fusion_reading treats it as hierarchical extraction (buddhas as essence, kami as trace). The pragmatic_incoherence_reading treats it as a piton of cognitive dissonance. Each has independent Îµ, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
