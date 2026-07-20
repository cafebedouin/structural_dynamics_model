% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Domain-Partition Reading of Simultaneous Kami-Buddha Veneration
 *   domain: religious/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint story instantiates the domain_partition_reading of the
 *   simultaneous_veneration kernel in Japanese religious history. The reading
 *   holds that kami and buddhas are functionally distinct entities governing
 *   separate domainsâkami for this-worldly prosperity and communal ritual,
 *   buddhas for afterlife salvation and funerary careâand that simultaneous
 *   veneration represents domain-appropriate specialization rather than
 *   metaphysical contradiction or ontological fusion. It operated as a
 *   pragmatic coordination mechanism that allowed shrines and temples to
 *   coexist without destructive competition, giving households a stable map
 *   for ritual life-cycle obligations.
 *
 * KEY AGENTS:
 *   - shrine_priests (beneficiary/organized): gain a protected ritual domain for kami worship, avoiding absorption into Buddhist cosmology
 *   - buddhist_clergy (beneficiary/institutional): retain funerary and memorial monopolies without needing to suppress shrine practice
 *   - lay_householders (beneficiary/powerless): receive a low-friction, pragmatic framework for navigating dual ritual obligations
 *   - ontological_fusion_theologians (excluded/organized): hold a competing metaphysical reading that is set aside by the pragmatic partition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.15).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Domain-Partition Reading of Simultaneous Kami-Buddha Veneration").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '6f690288-5581-4ce0-a42c-bc5e0d5c6b8b').
narrative_ontology:cs_kernel_codification('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', distributed).
narrative_ontology:cs_authority_grounding('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', practice).
narrative_ontology:cs_interpretation_layer_present('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b').
narrative_ontology:cs_reading_relation('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', foundational, functional_distinctness_governs_ritual_life).
narrative_ontology:cs_axiom_status(functional_distinctness_governs_ritual_life, holdable).
narrative_ontology:cs_axiom_grounding('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', functional_distinctness_governs_ritual_life, conventional).
narrative_ontology:cs_reference_frame('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', ritual_domain_partition).
narrative_ontology:cs_drift_state('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', meiji_state_separation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6f690288-5581-4ce0-a42c-bc5e0d5c6b8b', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, shrine_priests).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, lay_householders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami rituals for this-worldly prosperity, agricultural festivals, and community purification. The domain partition reading secures a distinct ritual sphere for kami veneration, preventing doctrinal absorption into Buddhist cosmology while permitting institutional coexistence. Exit would require abandoning the shrine tradition or accepting subordination within a Buddhist-dominant framework.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shrine_priests, beneficiary,
    organized, generational, constrained, national).

% Administer funerary and afterlife rituals, holding a historical near-monopoly on death-related religious services. The domain partition reading legitimizes their specialization in salvation and memorialization without requiring the suppression of kami worship, reducing inter-institutional doctrinal conflict. Exit would mean surrendering funeral services to competing ritual providers.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, buddhist_clergy, beneficiary,
    institutional, generational, constrained, national).

% Engage with both shrine and temple practices across the life cycle: birth and marriage rites at shrines, funerals and memorials at temples. The domain partition provides a pragmatic map that relieves them of needing to resolve competing metaphysical claims. Exit is difficult because ritual obligations are embedded in family and community continuity.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, lay_householders, beneficiary,
    powerless, biographical, constrained, local).

% Advance the honji-suijaku reading that kami are local manifestations of universal buddhas. Within the domain partition framework, their ontological claims are treated as metaphysically secondary or metaphorical, excluding them from the pragmatic coordination that governs everyday ritual practice.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, ontological_fusion_theologians, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious practice across two coexisting traditions by assigning functionally distinct domains: kami govern this-worldly prosperity, communal purification, and agricultural fertility; buddhas govern afterlife salvation, funerary care, and memorialization. This partition prevents doctrinal conflict and allows practitioners to access both ritual systems without needing to resolve metaphysical contradiction.
% TRANSFER_FUNCTION: Moves ritual obligation and patronage between specialist domains: households direct specific life-cycle needs to the appropriate institution (shrine or temple), and religious specialists receive stable congregational ties without competing for the same ritual functions.
% ABSENT_VOICES: Ontological fusion theologians who assert honji-suijaku metaphysical identity, and exclusive monotheistic observers who regard any simultaneous worship as idolatrous contradiction, are excluded from the pragmatic consensus. They would argue either for hierarchical subsumption of one tradition under the other or for the fundamental incoherence of dual veneration.
% DISAPPEARANCE_RATIONALE: If the functional partition reading vanished, the ritual economy would destabilize as shrines and temples competed directly for the same services; households would face pressure to choose a single tradition or construct new integrative theologies; the low-friction coexistence of the two ritual systems would collapse into doctrinal contest or state-imposed separation.
% FOUNDING_PROBLEM: How to maintain simultaneous devotion to kami and buddhas in a society where both ritual systems are socially necessary, without falling into doctrinal contradiction or destructive institutional competition.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic and administrative records from the Tokugawa and early Meiji periods, produced by government officials and outside chroniclers rather than the religious beneficiary institutions, attest that households routinely utilized both shrine and temple services for distinct life-cycle needs, corroborating the functional specialization independent of the beneficiary seats' self-interest.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.15 at interval end) because the constraint moves households to appropriate ritual specialists without creating surplus extraction; the transfer is functional, not rent-bearing. Suppression is minimal (0.08) because the arrangement persists through mutual benefit and practical convenience rather than coercion. Theater ratio is low (0.12): most activity is the actual coordination of ritual life, though some late-period doctrinal formalization adds minor performative overhead. Accessibility collapse is moderate (0.35): once understood, alternatives (exclusive Buddhism, exclusive Shinto, or fusion theology) do not disappear but remain live options, indicating the constraint coordinates without suppressing. Resistance is negligible (0.05) because all parties are net beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   The practitioner seat experiences smooth coordination: householders, priests, and clergy all benefit from clear ritual mapping. The analytical observer seat (comparative religion) sees a constructed convention rather than a natural law, and the excluded ontological-fusion seat experiences the reading as a marginalization of deeper metaphysical truth. The engine will compute low directionality for the beneficiary seats (subsidized by coordination) and higher directionality for the excluded seat (bearing the cost of interpretive marginalization), though no party experiences significant extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priests, Buddhist clergy, and lay householders are all beneficiaries of the coordination: the constraint reduces conflict and assigns stable roles. None are victims. The ontological fusion theologians are excluded from the pragmatic consensus but are not structurally exploited; their directionality is elevated relative to beneficiaries because they pay an epistemic cost (their reading is sidelined), but this does not translate into material extraction. Effective extraction is therefore negligible across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope rather than piton or snare is warranted because the coordination function remains live throughout the interval: households genuinely need both shrine and temple services, and the domain partition continues to solve that dual-affiliation problem without atrophy. There is no concentrated beneficiary capturing rents, no enforcement machinery primarily devoted to maintaining the partition, and no victim group. A piton reading would require theatrical maintenance without function; here the function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_historicity,
    'Is the domain partition reading a description of actual historical practitioner belief, or a post-hoc rationalization imposed by modern scholars?',
    'Archaeological and textual analysis of pre-modern practitioner records, sermons, and household ritual manuals to determine whether functional distinctness was explicitly articulated or inferred retrospectively.',
    'If the reading is largely retrospective, its epsilon should rise as a constructed constraint; if it was operative in practice, it remains a low-extraction coordination rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_historicity, empirical, 'Whether the domain partition was lived or inferred').

omega_variable(
    doctrine_vs_practice_gap,
    'Did the domain partition reading operate at the level of explicit doctrine, or as an implicit practical logic that practitioners followed without formal articulation?',
    'Comparative analysis of doctrinal texts versus administrative and ethnographic records of shrine-temple interactions and household ritual calendars.',
    'If implicit only, the constraint''s classification as rope relies on practice-based authority rather than formalized coordination, affecting the Boltzmann floor and coupling analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_practice_gap, conceptual, 'Whether the partition was doctrinal or practical').

omega_variable(
    meiji_rupture_nature,
    'Does the Meiji state separation (shinbutsu bunri) represent an external disruption of the domain partition reading, or the logical conclusion of its functional distinctness logic?',
    'Historical analysis of pre-Meiji separation rhetoric and post-Meiji practitioner resistance to determine whether the state enforced a pre-existing tendency or imposed a novel rupture.',
    'If the latter, the drift state''s severe practice_drift is externally imposed; if the former, the reading contained internal momentum toward institutional separation that would affect its coordination classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_rupture_nature, conceptual, 'Whether Meiji separation was internal or external to the reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(simu_tr_t100, simultaneous_veneration__domain_partition_reading, theater_ratio, 100, 0.03).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__domain_partition_reading, theater_ratio, 200, 0.05).
narrative_ontology:measurement(simu_tr_t300, simultaneous_veneration__domain_partition_reading, theater_ratio, 300, 0.08).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__domain_partition_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(simu_tr_t500, simultaneous_veneration__domain_partition_reading, theater_ratio, 500, 0.12).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(simu_be_t100, simultaneous_veneration__domain_partition_reading, base_extractiveness, 100, 0.06).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(simu_be_t300, simultaneous_veneration__domain_partition_reading, base_extractiveness, 300, 0.1).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 400, 0.12).
narrative_ontology:measurement(simu_be_t500, simultaneous_veneration__domain_partition_reading, base_extractiveness, 500, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__domain_partition_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(simu_su_t100, simultaneous_veneration__domain_partition_reading, suppression_requirement, 100, 0.02).
narrative_ontology:measurement(simu_su_t200, simultaneous_veneration__domain_partition_reading, suppression_requirement, 200, 0.03).
narrative_ontology:measurement(simu_su_t300, simultaneous_veneration__domain_partition_reading, suppression_requirement, 300, 0.05).
narrative_ontology:measurement(simu_su_t400, simultaneous_veneration__domain_partition_reading, suppression_requirement, 400, 0.07).
narrative_ontology:measurement(simu_su_t500, simultaneous_veneration__domain_partition_reading, suppression_requirement, 500, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The simultaneous_veneration kernel decomposes into three structurally distinct constraint stories because the natural-language label conflates three different empirical claims about practitioner belief, ritual function, and metaphysical ontology. Each reading has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
