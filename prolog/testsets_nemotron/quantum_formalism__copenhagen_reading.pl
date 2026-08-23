% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__copenhagen_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Interpretation: Collapse as Primitive Epistemic Boundary
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The Copenhagen interpretation asserts that wavefunction collapse is a
 *   fundamental physical process constituting an absolute epistemic boundary:
 *   measurement produces irreducible indeterminism that is not merely
 *   epistemic ignorance but ontological openness. Measurement enters as a
 *   primitive category that cannot be derived from the unitary evolution; the
 *   observer's role is non-eliminable; determinism is abandoned at
 *   measurement events. This reading structures the standard textbook
 *   presentation of quantum mechanics and the operational framework used by
 *   practicing physicists. The claimed_type is mountain because from the
 *   reading's own seat, the collapse postulate appears as a fixed feature of
 *   nature — the formalism works, predictions are verified, and no deeper
 *   mechanism is required. However, the kernel context reveals this is one of
 *   three live readings of the same formalism.
 *
 * KEY AGENTS:
 *   - quantum_mechanics_practitioners: Primary beneficiaries (institutional/biographical/arbitrage) — use the formalism daily, career-embedded in its pedagogy
 *   - textbook_pedagogy_institutions: Secondary beneficiaries (institutional/generational/arbitrage) — reproduce the interpretation through curricula and certification
 *   - alternative_interpretation_researchers: Excluded (moderate/biographical/constrained) — bear career costs for pursuing many-worlds, pilot-wave, or objective collapse programs
 *   - philosophers_of_physics: Observers (analytical/civilizational/analytical) — analyze the interpretive structure from outside the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.02).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.03).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Interpretation: Collapse as Primitive Epistemic Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '3c857a87-a2c2-4b97-afeb-668e75a852a5').
narrative_ontology:cs_kernel_codification('3c857a87-a2c2-4b97-afeb-668e75a852a5', formalized).
narrative_ontology:cs_authority_grounding('3c857a87-a2c2-4b97-afeb-668e75a852a5', lineage).
narrative_ontology:cs_interpretation_layer_present('3c857a87-a2c2-4b97-afeb-668e75a852a5').
narrative_ontology:cs_reading_relation('3c857a87-a2c2-4b97-afeb-668e75a852a5', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c857a87-a2c2-4b97-afeb-668e75a852a5', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('3c857a87-a2c2-4b97-afeb-668e75a852a5', foundational, collapse_is_primitive_physical_process).
narrative_ontology:cs_axiom_status(collapse_is_primitive_physical_process, holdable).
narrative_ontology:cs_axiom_grounding('3c857a87-a2c2-4b97-afeb-668e75a852a5', collapse_is_primitive_physical_process, conventional).
narrative_ontology:cs_axiom('3c857a87-a2c2-4b97-afeb-668e75a852a5', foundational, indeterminism_is_irreducible_ontological).
narrative_ontology:cs_axiom_status(indeterminism_is_irreducible_ontological, holdable).
narrative_ontology:cs_axiom_grounding('3c857a87-a2c2-4b97-afeb-668e75a852a5', indeterminism_is_irreducible_ontological, deontological).
narrative_ontology:cs_axiom('3c857a87-a2c2-4b97-afeb-668e75a852a5', secondary, observer_role_non_eliminable_in_quantum_theory).
narrative_ontology:cs_axiom_status(observer_role_non_eliminable_in_quantum_theory, holdable).
narrative_ontology:cs_axiom_grounding('3c857a87-a2c2-4b97-afeb-668e75a852a5', observer_role_non_eliminable_in_quantum_theory, conventional).
narrative_ontology:cs_reference_frame('3c857a87-a2c2-4b97-afeb-668e75a852a5', copenhagen_interpretive_framework).
narrative_ontology:cs_drift_state('3c857a87-a2c2-4b97-afeb-668e75a852a5', post_decoherence_theory, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3c857a87-a2c2-4b97-afeb-668e75a852a5', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, quantum_mechanics_practitioners).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, textbook_pedagogy_institutions).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, irreducible_indeterminism).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_as_primitive_ontological_category).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, observer_role_non_eliminable).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, determinism_abandoned_at_measurement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the Copenhagen formalism daily for calculations, teaching, and research. The interpretation provides a shared language and pedagogical framework that coordinates global physics practice. They can use the mathematics without committing to the interpretive claims (shut up and calculate), so exit from the interpretation is trivial while staying in the field.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_mechanics_practitioners, beneficiary,
    institutional, biographical, arbitrage, global).

% Reproduce the Copenhagen interpretation through standard curricula, examinations, and certification. The interpretation's dominance in textbooks creates a self-reinforcing pipeline: students learn it, become teachers, write the next textbooks. They benefit from the stability of a single canonical presentation.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, textbook_pedagogy_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Pursue many-worlds, pilot-wave, objective collapse, or QBist research programs. They face structural barriers: fewer dedicated positions, marginal citation networks, difficulty placing students, funding bias toward mainstream approaches. Their exit options are constrained — leaving quantum foundations for another subfield is possible but costs accumulated expertise.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers, excluded,
    moderate, biographical, constrained, global).

% Analyze the interpretive structure, its history, and its epistemic status from outside the physics practice. They have no material stake in which interpretation dominates but map the conceptual landscape and its drift.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophers_of_physics, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, diffuse).
narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, shared calculational and pedagogical framework for quantum mechanics that allows physicists worldwide to communicate, teach, and apply the theory without resolving the measurement problem. The collapse postulate acts as a semantic stop-sign that makes the formalism operationally closed.
% TRANSFER_FUNCTION: Moves interpretive authority and pedagogical centrality to the Copenhagen framework, while alternative interpretations bear the cost of non-standardness (career friction, citation marginalization, funding difficulty). No direct material transfer; the currency is academic legitimacy and institutional access.
% ABSENT_VOICES: Alternative interpretation researchers (many-worlds, pilot-wave, objective collapse, QBism) are structurally excluded from mainstream quantum mechanics positions, major conference plenaries, and core curriculum committees. They would argue the measurement problem is solved or solvable without primitive collapse, but their seats are not at the table where the default interpretation is reproduced.
% DISAPPEARANCE_RATIONALE: If the Copenhagen interpretation vanished overnight, physics practice would continue (the mathematics is unchanged) but the pedagogical framework, textbook presentation, and default interpretive language would need replacement. A new consensus interpretation would emerge from the existing alternatives, reshaping how quantum mechanics is taught and discussed. The world of physics practice rearranges; the formalism does not.
% FOUNDING_PROBLEM: In the 1920s, quantum mechanics needed an interpretation that made the formalism calculable and conceptually coherent for working physicists. The Copenhagen interpretation provided a pragmatic rule (collapse on measurement) that delivered unambiguous predictions without requiring a solution to the measurement problem.
% FOUNDING_PROBLEM_CORROBORATION: Copenhagen proponents (textbook authors, established departments) attest the problem is live: decoherence does not solve the measurement problem, outcomes are still singular, the Born rule remains postulated. Critics (Wallace, Vaidman, proponents of decoherence-based interpretations) attest the problem is dead or transformed: decoherence explains the appearance of collapse, the remaining issues are philosophical not physical. Experimentalists testing macroscopic superposition (Leggett-Garg, matter-wave interferometry) corroborate that the empirical boundary keeps moving. No single authority outside the beneficiary set settles the status.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, ExtMetricName, E),
    domain_priors:suppression_score(quantum_formalism__copenhagen_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is near-zero (0.02) because the reading's referent is the standing arrangement under contest — the Copenhagen interpretation as a calculational and pedagogical framework — assessed by the reading's own lights. The formalism works empirically; the collapse rule generates correct predictions; practitioners are not extracted from in any material sense. Suppression is low (0.03) because the constraint does not actively coerce — alternative interpretations exist and are published, though they remain minority positions. Theater ratio is low (0.08) because the coordination function (shared calculational framework) is genuine and the interpretive overlay is minimal in daily practice. Accessibility collapse is high (0.92) because once the collapse postulate is accepted as the boundary, alternative accounts of measurement appear as category errors. Resistance is low but non-zero (0.15) because the measurement problem persists as a recognized open question and alternative interpretations maintain research programs.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat (beneficiary, institutional power, arbitrage exit), the constraint is a mountain — the formalism is nature. From the alternative interpretation researcher seat (excluded, moderate power, constrained exit), the same structure may appear as a tangled_rope: genuine coordination function (shared mathematics) with asymmetric extraction (career penalties, funding barriers, citation marginalization). The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Quantum mechanics practitioners and textbook institutions are beneficiaries: they collect the coordination surplus of a shared language, established pedagogy, and institutional legitimacy. Their exit is arbitrage — they can use the formalism without committing to the interpretation. Alternative interpretation researchers are excluded: they would object to the epistemic boundary claim but are structurally kept out of mainstream positions and funding. Philosophers of physics are observers: analytical seat with no material stake. No victims are declared because from this reading's lights, no one bears costs — the interpretation is presented as what physics *is*. The omega variables document the contest over whether this presentation is accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making quantum mechanics calculable and conceptually coherent for 1920s physics) is contested — the reading's proponents say the problem is live (measurement remains mysterious), critics say it is dead (decoherence and alternative interpretations have superseded the need for a primitive collapse). Corroboration comes from outside the beneficiary set: decoherence theory, experimental tests of macroscopic superposition, and the sustained viability of many-worlds and pilot-wave programs all attest that the founding problem has shifted. The mandate has not resolved; the interpretation persists as the default pedagogical framework while its physical necessity is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the Copenhagen reading a genuine mountain (physics) or a contingent interpretive framework maintained by sociological forces?',
    'Trace whether the interpretive commitments survive when stripped of institutional authority: if physics practice would adopt a different reading without the historical weight of the Copenhagen tradition, the mountain claim is contested.',
    'If institutional rather than physical, the constraint is a false summit with beneficiaries (textbook publishers, established departments) and the effective extraction is non-zero for students and alternative interpretation researchers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the epistemic boundary is a feature of nature or a feature of the institutionalized interpretation').

omega_variable(
    extraction_via_pedagogical_monopoly,
    'Does the Copenhagen reading extract from alternative interpretation research through textbook monopoly and citation gatekeeping?',
    'Measure citation and funding flows to non-Copenhagen interpretations (many-worlds, pilot-wave, objective collapse) relative to Copenhagen-dominant departments over time.',
    'If extraction is documented, the constraint operates as a tangled_rope: coordination function (shared calculational framework) + asymmetric extraction (alternative interpretations bear career costs).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_via_pedagogical_monopoly, empirical, 'Whether the interpretive hegemony functions as career-extractive gatekeeping').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (funding, hiring, publication gates) or internalized (physicists genuinely believe alternatives are not physics)?',
    'Post-exit suppression trajectory: track researchers who switch to alternative interpretations — if suppression persists after institutional barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative quantum interpretations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.05).
narrative_ontology:measurement(quan_tr_t1950, quantum_formalism__copenhagen_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(quan_tr_t1980, quantum_formalism__copenhagen_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(quan_tr_t2000, quantum_formalism__copenhagen_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(quan_tr_t2020, quantum_formalism__copenhagen_reading, theater_ratio, 2020, 0.08).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.02).
narrative_ontology:measurement(quan_be_t1950, quantum_formalism__copenhagen_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(quan_be_t1980, quantum_formalism__copenhagen_reading, base_extractiveness, 1980, 0.02).
narrative_ontology:measurement(quan_be_t2000, quantum_formalism__copenhagen_reading, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement(quan_be_t2020, quantum_formalism__copenhagen_reading, base_extractiveness, 2020, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1927, quantum_formalism__copenhagen_reading, suppression_requirement, 1927, 0.05).
narrative_ontology:measurement(quan_su_t1950, quantum_formalism__copenhagen_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(quan_su_t1980, quantum_formalism__copenhagen_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(quan_su_t2000, quantum_formalism__copenhagen_reading, suppression_requirement, 2000, 0.03).
narrative_ontology:measurement(quan_su_t2020, quantum_formalism__copenhagen_reading, suppression_requirement, 2020, 0.03).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.02).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_measurement_problem).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_decoherence_theory).

% DUAL FORMULATION NOTE:
% Part of the quantum_formalism constraint family. This reading (Copenhagen) treats collapse as primitive; many_worlds_reading treats it as apparent (decoherence); pilot_wave_reading restores determinism via hidden variables. The three readings share the same mathematical formalism (the kernel) but instantiate different constraints with different ε, beneficiaries, and types. Copenhagen claims mountain; many-worlds claims rope (coordination via decoherence); pilot-wave claims mountain (deterministic ontology). The ε-invariance principle requires separate stories because the extractiveness profile differs by reading: Copenhagen extracts near-zero from practitioners but potentially from alternative researchers; many-worlds extracts coordination cost from ontological extravagance; pilot-wave extracts from theoretical complexity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
