% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhist Ritual Domain Partition (Partition Reading)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   In the partition reading of the shinbutsu ontological commitment kernel,
 *   Shinto and Buddhism are understood as functionally partitioning Japanese
 *   religious life: Shinto governs birth, marriage, and this-worldly
 *   communion with kami, while Buddhism governs death, funerals, and the
 *   afterlife. The two traditions operate side-by-side without doctrinal
 *   integration, and practitioner autonomy is preserved. This reading
 *   contests the syncretic reading (which sees kami and buddhas as
 *   manifestations of a single order under honji-suijaku) and the incoherence
 *   reading (which sees no stable commitment at all). The constraint story
 *   models the standing arrangement of ritual-domain partition as a
 *   coordination mechanism.
 *
 * KEY AGENTS:
 *   - Shinto priesthood (organized/constrained) â beneficiary of life-cycle domain boundary
 *   - Buddhist clergy (organized/constrained) â beneficiary of afterlife domain boundary
 *   - Lay community (moderate/constrained) â beneficiary of clear ritual jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.18).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.15).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhist Ritual Domain Partition (Partition Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, 'd21ccc42-cfc3-4bc9-890f-6bfc5db0abdf').
narrative_ontology:cs_kernel_codification('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', implicit).
narrative_ontology:cs_authority_grounding('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', practice).
narrative_ontology:cs_interpretation_layer_present('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf').
narrative_ontology:cs_reading_relation('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', foundational, kami_and_buddhas_occupy_non_overlapping_domains).
narrative_ontology:cs_axiom_status(kami_and_buddhas_occupy_non_overlapping_domains, holdable).
narrative_ontology:cs_axiom_grounding('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', kami_and_buddhas_occupy_non_overlapping_domains, conventional).
narrative_ontology:cs_axiom('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', secondary, ritual_function_trumps_ontological_unification).
narrative_ontology:cs_axiom_status(ritual_function_trumps_ontological_unification, holdable).
narrative_ontology:cs_axiom_grounding('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', ritual_function_trumps_ontological_unification, conventional).
narrative_ontology:cs_reference_frame('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', ritual_domain_partition).
narrative_ontology:cs_drift_state('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', tokugawa_danka_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d21ccc42-cfc3-4bc9-890f-6bfc5db0abdf', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, lay_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers life-cycle and this-worldly rituals at shrines; maintains kami worship and seasonal festivals. Benefits from clear jurisdictional boundaries that protect shrine practice from Buddhist doctrinal subsumption.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_priesthood, beneficiary,
    organized, generational, constrained, national).

% Administers funerary, memorial, and afterlife rituals through temples; maintains sutra recitation and ancestral rites. Benefits from recognized domain monopoly over death and the other-worldly without Shinto interference.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_clergy, beneficiary,
    organized, generational, constrained, national).

% Engages shrine priests for birth, marriage, and seasonal rites, and Buddhist clergy for funerals and memorial services. Moves between institutions as life-cycle demands without being required to resolve their doctrinal differences or adopt a single faith.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, lay_community, beneficiary,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables two distinct religious traditions with incompatible cosmologies to coexist in the same society by assigning each exclusive jurisdiction over complementary life domains: Shinto governs life, fertility, and this-worldly affairs; Buddhism governs death, afterlife, and other-worldly affairs.
% TRANSFER_FUNCTION: Moves ritual authority and patronage from undifferentiated competition to domain-specialized coordination: each tradition receives exclusive jurisdiction over its designated life-stage domain, and lay practitioners transfer their ritual engagement to the appropriate specialist.
% ABSENT_VOICES: Syncretic theologians who advocated honji-suijaku metaphysical unification of kami and buddhas are present in the broader discourse but not central to this reading's operational logic. State actors seeking unified religious control are also backgrounded.
% DISAPPEARANCE_RATIONALE: If the partition vanished, the clear jurisdictional boundaries between shrine and temple would dissolve; practitioners would face unresolved competition or confusion over ritual authority, and the stable pattern of moving between shrine and temple across the life cycle would require renegotiation.
% FOUNDING_PROBLEM: How do Shinto and Buddhist traditions with distinct cosmologies and institutional bases coexist in a single society without theological conflict, ritual redundancy, or institutional warfare?
% FOUNDING_PROBLEM_CORROBORATION: Historical records from both shrine and temple institutions attest to negotiated jurisdictional boundaries. Independent historiography and anthropological study of Japanese religious practice corroborate the functional specialization from outside the benefiting priestly classes.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the partition reallocates ritual jurisdiction without systematically extracting surplus from any party; each seat receives a coordination benefit. Suppression is low (0.15) because the arrangement persists through mutual interest and convention rather than coercion. Theater ratio is minimal (0.08) as maintenance of the partition is largely operational, not performative. Accessibility collapse is moderate-low (0.25): alternatives (syncretic practice, personal eclecticism) remain thinkable but are made unnecessary by the clarity of the partition. Resistance is low (0.1) because the coordinated parties broadly accept the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is narrow: all named stakeholders are beneficiaries in this reading. The Shinto priesthood and Buddhist clergy each experience the constraint as protecting their respective domains, while the lay community experiences it as simplifying navigation between two religious systems. There is no structural payer seat in this reading; the engine should compute low effective extraction for all indices.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared agents are beneficiaries. The Shinto priesthood and Buddhist clergy each occupy a symmetric beneficiary position relative to their respective ritual monopolies. The lay community is also a net beneficiary, gaining clarity and autonomy. No victim group is declared, and no directionality override is required: the derivation chain naturally places all seats near the full-beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition arrangement does not appear to be a mandate that outlived its function. The founding problem â how two distinct religious traditions coexist â remained live throughout the interval, and the constraint persisted because the coordination problem it solved was structurally recurrent (birth and death are perennial). There is no evidence that the arrangement decayed into theatrical maintenance; its persistence tracks the continued relevance of the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_syncretism_historical_accuracy,
    'Does the partition reading accurately capture the dominant historical structure of shinbutsu relations, or does it project modern categorical distinctions onto a more fluid medieval reality?',
    'Archaeological and textual analysis of medieval temple-shrine complexes to determine whether jurisdictional separation or honji-suijaku integration was the operative framework.',
    'If the historical record shows pervasive integration, the partition reading is a retroactive idealization and the constraint''s coordination function is overstated; if separation was operative, the reading is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_syncretism_historical_accuracy, empirical, 'Whether the partition reading is historically accurate or anachronistic').

omega_variable(
    state_enforcement_vs_autonomous_coordination,
    'Was the domain partition maintained primarily by mutual coordination benefit between priesthoods, or by state enforcement (e.g., temple registration systems) that the partition reading backgrounds?',
    'Comparative analysis of state edicts and temple records across the medieval and early modern periods to isolate state intervention from autonomous practice.',
    'If state enforcement was primary, the constraint is not a pure rope but a tangled rope or scaffold with active enforcement and a hidden beneficiary (the state); if autonomous, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_vs_autonomous_coordination, empirical, 'Whether the partition was self-enforcing or state-imposed').

omega_variable(
    kernel_reading_contest_irreducibility,
    'Is the shinbutsu ontological commitment kernel fundamentally contested among the three readings (partition, syncretic, incoherence), or is one reading descriptively superior?',
    'Synthesis of historiographical consensus; if the readings reflect different periods or regions rather than one contested object, the kernel should be decomposed temporally or geographically.',
    'If irreducibly contested, the three constraints remain distinct; if resolvable by decomposition, the network edges should be restructured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_irreducibility, conceptual, 'Whether the kernel is irreducibly contested or decomposable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_partition_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(shinbutsu_partition_tr_t25, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(shinbutsu_partition_tr_t50, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement(shinbutsu_partition_tr_t75, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 75, 0.07).
narrative_ontology:measurement(shinbutsu_partition_tr_t100, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(shinbutsu_partition_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(shinbutsu_partition_be_t25, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(shinbutsu_partition_be_t50, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement(shinbutsu_partition_be_t75, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 75, 0.17).
narrative_ontology:measurement(shinbutsu_partition_be_t100, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
