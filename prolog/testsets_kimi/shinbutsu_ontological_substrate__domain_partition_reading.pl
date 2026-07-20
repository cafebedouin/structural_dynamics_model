% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Shinbutsu Functional Domain Partition
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the domain_partition_reading of the
 *   shinbutsu_ontological_substrate kernel. It instantiates the claim that
 *   kami and buddhas govern separate functional domainsâthis-world concerns
 *   versus afterlife salvationâand that their historical coexistence in
 *   Japan was pragmatic and institutional rather than ontologically unified.
 *   Sibling readings include syncretic_fusion_reading (honji suijaku as
 *   metaphysical truth) and incoherent_bundle_reading (no coherent kernel,
 *   only accumulated institutional drift). The expected structural delta is
 *   low institutional entanglement and easy separation: the partition
 *   operates as a coordination device with minimal coercive overhead.
 *
 * KEY AGENTS:
 *   - Shinto priesthood (agenda_setter / beneficiary): administers this-world rites, benefits from clear jurisdictional boundaries.
 *   - Buddhist clergy (agenda_setter / beneficiary): administers afterlife rites, benefits from clear jurisdictional boundaries.
 *   - Lay practitioners (beneficiary): navigate plural religious life using the partition as a low-cost routing mechanism.
 *   - Syncretic theologians (excluded): argue for ontological unity; structurally marginalized by the functional partition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.12).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.08).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Shinbutsu Functional Domain Partition").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '2e8de36a-6eee-4b63-bdea-ea851c7bcb98').
narrative_ontology:cs_kernel_codification('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', distributed).
narrative_ontology:cs_authority_grounding('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', practice).
narrative_ontology:cs_interpretation_layer_present('2e8de36a-6eee-4b63-bdea-ea851c7bcb98').
narrative_ontology:cs_reading_relation('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', foundational, sacred_domain_partition_is_institutional).
narrative_ontology:cs_axiom_status(sacred_domain_partition_is_institutional, holdable).
narrative_ontology:cs_axiom_grounding('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', sacred_domain_partition_is_institutional, conventional).
narrative_ontology:cs_axiom('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', foundational, kami_buddha_coexistence_requires_no_metaphysics).
narrative_ontology:cs_axiom_status(kami_buddha_coexistence_requires_no_metaphysics, holdable).
narrative_ontology:cs_axiom_grounding('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', kami_buddha_coexistence_requires_no_metaphysics, conventional).
narrative_ontology:cs_reference_frame('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', functional_domain_partition).
narrative_ontology:cs_drift_state('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', post_meiji_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2e8de36a-6eee-4b63-bdea-ea851c7bcb98', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers shrine rites for this-world benefits, harvests, and local protection. Their jurisdiction is bounded to worldly concerns; they do not administer funerary or soteriological rites. The functional partition secures their ritual monopoly over the living without contesting Buddhist claims to the dead.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood, agenda_setter,
    organized, generational, mobile, national).

% Administers funeral rites, memorial services, and afterlife salvation. Their jurisdiction is bounded to other-worldly concerns. The partition secures their role as primary intermediaries for the dead without competition from shrine priests in the soteriological domain.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy, agenda_setter,
    organized, generational, mobile, national).

% Routinely turn to shrines for this-world petitions and to temples for funerals and ancestral rites. The partition provides a clear, low-friction map of which institution to approach for which concern, reducing cognitive and social cost in a plural religious environment.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Advance doctrinal frameworks such as honji suijaku that posit ontological unity between kami and buddhas. Their voices are structurally muted in the domain-partition arrangement because the functional separation treats such metaphysical commitments as superfluous to institutional practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_theologians, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of ritual labor between Shinto and Buddhist traditions so that practitioners know which institution to approach for which concern, preventing jurisdictional conflict and redundant competition in a plural religious field.
% TRANSFER_FUNCTION: Moves ritual authority over life-stages and concern-types: this-world petitions and harvest rites to shrine priests, afterlife care and ancestral rites to Buddhist clergy. Lay practitioners transfer donations, labor, and affiliation to the appropriate institution without needing to resolve theological contradictions.
% ABSENT_VOICES: Syncretic theologians who argue for ontological unity through honji suijaku, and village practitioners of hybrid rites that do not respect the partition. They are absent because the functional arrangement treats metaphysical unification as institutionally unnecessary, pushing theological debate to the margins of the operative system.
% DISAPPEARANCE_RATIONALE: If the partition vanished, lay practitioners would face immediate ambiguity about which institution handles which concern; shrine and temple economies would face destabilizing competition as clergy encroach on each other's ritual territory, and the low-friction pluralism of Japanese religious life would require new coordination mechanisms.
% FOUNDING_PROBLEM: How to accommodate the simultaneous presence of two major religious traditionsâindigenous kami worship and imported Buddhismâwithout destructive competition, forced assimilation, or endless theological conflict over ultimate authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion and comparative religion scholars attest to the pluralistic challenge from outside the benefiting priesthoods; ethnographers of pre-modern lay practice corroborate that the partition solved a genuine coordination problem for practitioners navigating multiple ritual systems.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.12, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.12) because the constraint moves ritual authority and donations to whichever institution naturally provides the service, without systematic overcharge or rent. Suppression is minimal (0.08) because the partition does not actively suppress alternative practices; hybrid rites persist at the margins. Theater ratio is negligible (0.05) because maintenance of the partition requires little performative workâit is sustained by mutual interest and habit rather than ideological enforcement. Accessibility collapse is moderate (0.30): once the partition is understood, it becomes the default routing logic, though alternatives remain available. Resistance is near-zero (0.05) because all major parties are net beneficiaries of the coordination.
 *
 * PERSPECTIVAL GAP:
 *   The priesthood and lay practitioner seats compute the constraint as coordination (low d, low Ï) because they directly benefit from reduced competition and clear jurisdictional maps. The syncretic theologian seat, excluded from the institutional conversation, experiences the partition as a barrier to their integrative theological project; from that seat the constraint may compute as moderately extractive because it subsidizes a bifurcated regime that treats their core metaphysical commitments as superfluous.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priesthood and Buddhist clergy are structural beneficiaries of the partition (low d): it grants each a ritual monopoly in their respective domain without requiring costly ecumenical negotiation. Lay practitioners are beneficiaries (low d): they receive a clear, low-friction coordination mechanism for navigating plural religious life. Syncretic theologians are neither beneficiaries nor declared victims; their constrained exit and excluded role place them at a higher d, though without victim declaration the engine will revert to the moderate power atom fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The low theater ratio, near-zero resistance, and absence of victims prevent misclassification as a piton or snare. The presence of multiple beneficiaries without identifiable victims, combined with low suppression, satisfies the rope gate: the constraint solves a genuine coordination problem and does not depend on coercion or asymmetric extraction for its persistence. Were suppression or theater to rise significantly, the classification would drift toward tangled rope or piton; the flat temporal measurements guard against that drift going unobserved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_as_historical_idealization,
    'Does the domain partition reading accurately describe pre-modern Japanese religious practice, or does it impose a modern functionalist grid on a more entangled shrine-temple reality?',
    'Archival study of pre-Meiji shrine-temple interactions and ethnographic reconstruction of village-level ritual practice to test whether the partition was lived or retrospectively constructed.',
    'If the partition was never fully realized, the reading''s rope classification remains accurate but its coordination function is overstated; if village practice was always hybrid, the constraint may be a snare or tangled rope at the local level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_as_historical_idealization, empirical, 'Whether the functional partition is historical reality or scholarly idealization.').

omega_variable(
    meiji_rupture_vs_continuity,
    'Does the Meiji-era shinbutsu bunri represent a codification of the pre-existing domain partition, or a state-enforced rupture that replaced a functional rope with an actively enforced scaffold or tangled rope?',
    'Comparative analysis of institutional entanglement, legal enforcement, and resource flows before and after 1868.',
    'If the Meiji period introduced active enforcement and state extraction, the constraint story splits into two intervals with different types; if the state merely made explicit a pre-existing functional separation, the rope classification extends across the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_rupture_vs_continuity, conceptual, 'Whether Meiji separation was continuity or rupture in constraint type.').

omega_variable(
    kernel_incoherence_uncertainty,
    'Is the shinbutsu ontological substrate a genuinely coherent kernel that admits distinct readings, or is it an incoherent bundle that only appears to support readings because scholars project structure onto accumulated institutional drift?',
    'Comparative evaluation of all three readings against the full historical record; if no reading achieves stable fit, the kernel itself may be incoherent.',
    'If the kernel is incoherent, the domain partition reading is a scholarly construct without historical counterpart, and the entire constraint family may be a projection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_incoherence_uncertainty, conceptual, 'Whether the kernel itself is coherent or an artifact of scholarly projection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(shin_be_t5, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 5, 0.11).
narrative_ontology:measurement(shin_be_t10, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(shin_be_t15, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(shin_be_t25, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(shin_be_t30, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 30, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_substrate__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the shinbutsu ontological substrate kernel into three structurally distinct readings: domain_partition (functional separation), syncretic_fusion (ontological unity), and incoherent_bundle (drift without kernel). Each reading carries a distinct epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
