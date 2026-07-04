% ============================================================================
% CONSTRAINT STORY: vocabulary_collision_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vocabulary_collision_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vocabulary_collision_reading
 *   human_readable: Vocabulary Collision Reading of Seat/Gauge Orientation
 *   domain: philosophy_of_measurement/epistemology/formal_systems
 *
 * SUMMARY:
 *   The Deferential Realism framework underwent a vocabulary correction from
 *   v7 to v8 to eliminate a collision between two uses of the term 'seat':
 *   observer-position (who is measuring) versus content-position (what
 *   structural role an agent occupies). This reading treats the change as a
 *   coordination mechanism that solves a genuine ambiguity problem with
 *   minimal extractive overhead. The constraint is the vocabulary standard
 *   itself — the requirement that framework users adopt the v8 terminology to
 *   participate in the shared analytical practice.
 *
 * KEY AGENTS:
 *   - framework_users: moderate/mobile — apply the framework; benefit from reduced ambiguity
 *   - documentation_maintainers: organized/mobile — set and maintain the vocabulary standard
 *   - seat_theorem_formalism: analytical/analytical (non-agent) — the formal structure whose consistency the vocabulary protects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vocabulary_collision_reading, 0.18).
domain_priors:suppression_score(vocabulary_collision_reading, 0.12).
domain_priors:theater_ratio(vocabulary_collision_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vocabulary_collision_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(vocabulary_collision_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(vocabulary_collision_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vocabulary_collision_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(vocabulary_collision_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vocabulary_collision_reading, rope).
narrative_ontology:human_readable(vocabulary_collision_reading, "Vocabulary Collision Reading of Seat/Gauge Orientation").
narrative_ontology:topic_domain(vocabulary_collision_reading, "philosophy_of_measurement/epistemology/formal_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vocabulary_collision_reading, '0d997550-3f0f-4178-85f0-fb4218a6c801').
narrative_ontology:cs_kernel_codification('0d997550-3f0f-4178-85f0-fb4218a6c801', formalized).
narrative_ontology:cs_authority_grounding('0d997550-3f0f-4178-85f0-fb4218a6c801', expertise).
narrative_ontology:cs_interpretation_layer_present('0d997550-3f0f-4178-85f0-fb4218a6c801').
narrative_ontology:cs_reading_relation('0d997550-3f0f-4178-85f0-fb4218a6c801', seat_gauge_orientation_kernel__ontological_commitment_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d997550-3f0f-4178-85f0-fb4218a6c801', seat_gauge_orientation_kernel__measurement_architecture_reading, coexists_with).
narrative_ontology:cs_axiom('0d997550-3f0f-4178-85f0-fb4218a6c801', foundational, vocabulary_collision_is_technical_defect).
narrative_ontology:cs_axiom_status(vocabulary_collision_is_technical_defect, holdable).
narrative_ontology:cs_axiom_grounding('0d997550-3f0f-4178-85f0-fb4218a6c801', vocabulary_collision_is_technical_defect, conventional).
narrative_ontology:cs_axiom('0d997550-3f0f-4178-85f0-fb4218a6c801', secondary, formalism_unchanged_by_relabeling).
narrative_ontology:cs_axiom_status(formalism_unchanged_by_relabeling, holdable).
narrative_ontology:cs_axiom_grounding('0d997550-3f0f-4178-85f0-fb4218a6c801', formalism_unchanged_by_relabeling, empirically_contingent).
narrative_ontology:cs_reference_frame('0d997550-3f0f-4178-85f0-fb4218a6c801', v7_vocabulary_state).
narrative_ontology:cs_drift_state('0d997550-3f0f-4178-85f0-fb4218a6c801', v8_vocabulary_adoption, gap(codification_collapse, minor, true)).
narrative_ontology:cs_created_at('0d997550-3f0f-4178-85f0-fb4218a6c801', '').
narrative_ontology:cs_kernel_id(vocabulary_collision_reading, seat_gauge_orientation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vocabulary_collision_reading, framework_users).
narrative_ontology:constraint_beneficiary(vocabulary_collision_reading, documentation_maintainers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Apply the Deferential Realism framework to analyze constraints. They read documentation, interpret terminology, and map concepts to their analytical work. The v7-to-v8 vocabulary shift reduces collision between 'seat' as observer-position and 'seat' as content-position, making the framework easier to apply consistently.
narrative_ontology:constraint_stakeholder(vocabulary_collision_reading, framework_users, beneficiary,
    moderate, biographical, mobile, local).

% Maintain the framework's formal specification and teaching materials. They identified the vocabulary collision as a source of systematic misreading and implemented the v8 rename to eliminate ambiguity. They bear the coordination cost of updating documentation and managing the transition.
narrative_ontology:constraint_stakeholder(vocabulary_collision_reading, documentation_maintainers, agenda_setter,
    organized, biographical, mobile, local).

% The formal structure relating observer position to measurement content. The vocabulary collision created false conflicts where 'seat' in different contexts appeared to reference the same entity; the v8 rename preserves the formalism's internal consistency.
narrative_ontology:constraint_stakeholder(vocabulary_collision_reading, seat_theorem_formalism, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vocabulary_collision_reading, seat_theorem_formalism).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes consistent terminology across the framework so that 'seat' unambiguously refers to content-position in the seat theorem and observer-position is named separately, preventing systematic conflation.
% TRANSFER_FUNCTION: Imposes a one-time coordination cost on documentation maintainers and framework users to learn the new vocabulary; in return, eliminates ongoing interpretive friction from the collision.
% ABSENT_VOICES: None structurally excluded — the vocabulary change is a technical correction with no asymmetric extraction or suppressed alternatives.
% DISAPPEARANCE_RATIONALE: If the v8 vocabulary correction disappeared and the framework reverted to v7 terminology, the 'seat' collision would return, systematic misreading would resume, and users would again conflate observer-position with content-position in seat-theorem contexts.
% FOUNDING_PROBLEM: The v7 framework used 'seat' to mean both observer-position (who is looking) and content-position (what structural role an agent occupies in the constraint), creating systematic ambiguity where the same term referenced different analytical objects.
% FOUNDING_PROBLEM_CORROBORATION: Framework users and documentation maintainers both attest the collision was a live interpretive problem; independent code review and teaching experience confirm the ambiguity caused real confusion in application.
narrative_ontology:disappearance_verdict(vocabulary_collision_reading, world_rearranges).
narrative_ontology:founding_problem_status(vocabulary_collision_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vocabulary_collision_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(vocabulary_collision_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vocabulary_collision_reading, 0.18, 'claude-sonnet-4-5-20250929', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vocabulary_collision_reading_tests).
:- end_tests(vocabulary_collision_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the coordination cost is a one-time learning burden with no ongoing rent collection. Suppression is low (0.12) because alternatives are not suppressed — users can fork the framework or use different terminology in their own work; the standard only coordinates those who want to communicate within the shared practice. Theater is low (0.08) because the vocabulary change serves its stated function (disambiguation) with minimal performative overhead. The declining trajectory reflects the transition period: initial adoption friction decreases as the new vocabulary becomes established.
 *
 * PERSPECTIVAL GAP:
 *   No significant perspectival gap expected — all seats experience this as coordination. The claimed type (rope) and the metrics align: low extraction, low suppression, genuine coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework users and documentation maintainers are both beneficiaries: users get clearer terminology, maintainers get a more teachable framework. The coordination cost is symmetric and temporary. No party is structurally targeted for extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vocabulary_vs_ontology,
    'Is the v8 change purely terminological (renaming to eliminate collision) or does it reflect a deeper shift in what the framework takes to be the primary analytical object?',
    'Examine whether engine behavior changed between v7 and v8: if the formalism is identical and only labels changed, the shift is terminological; if classification logic or measurement procedures changed, the shift is ontological.',
    'If purely terminological, this reading is correct and the constraint is a low-extraction coordination mechanism. If ontological, the ontological_commitment_reading is correct and the constraint reflects a substantive reframing of the framework''s commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vocabulary_vs_ontology, empirical, 'Whether the v8 change is terminological or ontological.').

omega_variable(
    transition_cost_distribution,
    'Does the one-time coordination cost of adopting v8 terminology fall symmetrically on all framework users, or does it disproportionately burden certain user classes?',
    'Survey framework users by experience level and application domain to measure adoption friction; if advanced users or specific domains face higher transition costs, the coordination is asymmetric.',
    'If transition costs are symmetric, the low extractiveness score is accurate. If costs are asymmetric, effective extraction may be higher for burdened user classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_distribution, empirical, 'Whether transition costs are symmetrically distributed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vocabulary_collision_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(voca_tr_t0, vocabulary_collision_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(voca_tr_t5, vocabulary_collision_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(voca_tr_t10, vocabulary_collision_reading, theater_ratio, 10, 0.08).

% Extraction over time
narrative_ontology:measurement(voca_be_t0, vocabulary_collision_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(voca_be_t5, vocabulary_collision_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(voca_be_t10, vocabulary_collision_reading, base_extractiveness, 10, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(voca_su_t0, vocabulary_collision_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(voca_su_t5, vocabulary_collision_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(voca_su_t10, vocabulary_collision_reading, suppression_requirement, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
