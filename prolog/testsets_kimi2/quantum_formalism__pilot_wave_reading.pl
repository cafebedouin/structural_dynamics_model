% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__pilot_wave_reading, []).

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
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot Wave (Bohmian) Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   This constraint story models the pilot wave (de Broglie-Bohm) reading of
 *   quantum formalism as an epistemic authority structure coordinating a
 *   minority research tradition in physics. The reading claims that particles
 *   possess definite positions at all times, guided by a physically real
 *   wavefunction, restoring determinism and eliminating observer-dependent
 *   collapse. As a contested kernel reading, it presents itself as a
 *   description of natural law, but structurally operates as a tangled rope:
 *   it provides genuine conceptual coordination (solving the measurement
 *   problem, supplying a clear ontology) while asymmetrically extracting
 *   career costs from junior scholars who specialize in its demanding
 *   formalism, with senior framework architects capturing the institutional
 *   status and funding that sustain the tradition against mainstream
 *   marginalization.
 *
 * KEY AGENTS:
 *   - Bohmian research leaders (agenda_setter/institutional/arbitrage): administer the interpretive framework, train successors, and derive status from its persistence.
 *   - Bohmian junior scholars (payer/moderate/constrained): bear the career costs of specialization in a marginalized tradition.
 *   - Mainstream quantum physicists (excluded/institutional/analytical): control hiring and funding but exclude Bohmian research from serious engagement.
 *   - Philosophy of physics community (observer/organized/mobile): analytically engage without bearing career costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.52).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.48).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot Wave (Bohmian) Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '68021542-fc3e-4352-9240-49ba28dbdf91').
narrative_ontology:cs_kernel_codification('68021542-fc3e-4352-9240-49ba28dbdf91', formalized).
narrative_ontology:cs_authority_grounding('68021542-fc3e-4352-9240-49ba28dbdf91', lineage).
narrative_ontology:cs_interpretation_layer_present('68021542-fc3e-4352-9240-49ba28dbdf91').
narrative_ontology:cs_reading_relation('68021542-fc3e-4352-9240-49ba28dbdf91', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('68021542-fc3e-4352-9240-49ba28dbdf91', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('68021542-fc3e-4352-9240-49ba28dbdf91', foundational, definite_position_realism).
narrative_ontology:cs_axiom_status(definite_position_realism, holdable).
narrative_ontology:cs_axiom_grounding('68021542-fc3e-4352-9240-49ba28dbdf91', definite_position_realism, empirically_contingent).
narrative_ontology:cs_axiom('68021542-fc3e-4352-9240-49ba28dbdf91', secondary, wavefunction_ontic_status).
narrative_ontology:cs_axiom_status(wavefunction_ontic_status, holdable).
narrative_ontology:cs_axiom_grounding('68021542-fc3e-4352-9240-49ba28dbdf91', wavefunction_ontic_status, empirically_contingent).
narrative_ontology:cs_reference_frame('68021542-fc3e-4352-9240-49ba28dbdf91', classical_particle_ontology).
narrative_ontology:cs_drift_state('68021542-fc3e-4352-9240-49ba28dbdf91', contemporary_quantum_foundations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('68021542-fc3e-4352-9240-49ba28dbdf91', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_research_leaders).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, bohmian_junior_scholars).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, scientific_realism).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, deterministic_ontology).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, observer_independence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior physicists and philosophers who define the research agenda for Bohmian mechanics, adjudicate legitimate extensions of the pilot wave framework, supervise theses, and organize dedicated conferences and journals. They derive institutional status, research funding, and intellectual authority from sustaining this interpretive tradition as a live alternative to orthodox quantum mechanics.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_research_leaders, agenda_setter,
    institutional, generational, arbitrage, global).

% Graduate students and postdoctoral researchers specializing in the mathematical techniques of Bohmian mechanics. They master formally demanding methods with limited applicability outside foundational circles and face a constrained academic job market where their expertise is rarely valued in mainstream physics departments.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_junior_scholars, payer,
    moderate, biographical, constrained, global).

% Representatives of the Copenhagen and decoherence orthodoxies that control mainstream hiring, funding, and curriculum. They treat interpretive ontology as secondary to predictive formalism and do not engage substantively with pilot wave proposals, structurally excluding Bohmian researchers from institutional power.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, mainstream_quantum_physicists, excluded,
    institutional, generational, analytical, global).

% Philosophers who use Bohmian mechanics as a case study for scientific realism, determinism, and nonlocality. They engage with the framework analytically without bearing the career costs of its empirical defense or institutional marginalization.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, philosophy_of_physics_community, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__pilot_wave_reading, bohmian_research_leaders).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores a deterministic, observer-independent single-world ontology to quantum mechanics by postulating that particles always have definite positions guided by a physically real wavefunction, thereby eliminating the measurement problem without invoking wavefunction collapse or branching.
% TRANSFER_FUNCTION: Moves epistemic authority, research labor, and career investment from mainstream quantum research paths into the Bohmian tradition; senior leaders capture institutional status and funding while junior specialists bear the costs of professional marginalization.
% ABSENT_VOICES: Operationalist and instrumentalist physicists who regard interpretive ontology as irrelevant to physics; collapse-model theorists whose stochastic alternatives are excluded by the deterministic commitment; quantum information theorists who prioritize computational applications over ontological foundations.
% DISAPPEARANCE_RATIONALE: The Bohmian research program, its dedicated journals, conferences, and training pipelines depend on this ontological commitment. If it vanished, these scholars would need to migrate to operationalist, collapse, or many-worlds frameworks, and the realist revival in quantum foundations would lose its central deterministic case study.
% FOUNDING_PROBLEM: The measurement problem in orthodox quantum mechanics, where the apparent necessity of observer-dependent wavefunction collapse threatened the possibility of a complete, objective physical description.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of physics across interpretive camps attest that the measurement problem remains conceptually unresolved, though they dispute whether the pilot wave approach is the correct solution. Mainstream working physicists largely regard the problem as solved by decoherence or as philosophically inconsequential, corroborating the contested status from outside the benefiting parties.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__pilot_wave_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__pilot_wave_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate-to-high because the framework extracts significant career investment and epistemic loyalty from its adherents while returning ontological clarity that is professionally undervalued. Suppression (0.48) reflects both internal boundary maintenance (no collapse, no branching permitted within the research program) and external marginalization by the mainstream. Theater_ratio (0.22) is low because most activity is genuine mathematical and conceptual research, though some performative defense of nonlocality and realism is required. Accessibility_collapse (0.35) is low because alternatives (Copenhagen, Many Worlds, QBism, GRW) are readily available and well-developed. Resistance (0.68) is high because mainstream physics largely ignores or resists engaging with the framework. The temporal grid is shared across all metrics to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (senior Bohmian leaders) experiences the constraint as a genuine rope: a coherent research program solving real conceptual problems, with manageable costs. The payer seat (junior scholars) experiences it as substantially extractive: they pay with constrained career prospects and limited exit options. The excluded seat (mainstream physicists) sees it as irrelevant. These divergences are structurally derived from the power and exit asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior leaders are declared beneficiaries with arbitrage-grade exit (they can move to philosophy departments, foundational institutes, or gain status from uniqueness), so their directionality sits near the beneficiary end (low d). Junior scholars are declared victims with constrained exit (specialized skills, narrow job market), so their directionality sits near the target end (high d). The exclusion of mainstream physicists is structural but they are not governed by this constraint, so they do not feed the extraction calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a rope because the coordination is not symmetrically beneficial: junior scholars bear net career costs. It is not a snare because the coordination function is genuine and not cover for extractionâthe measurement problem is a real conceptual issue and the pilot wave framework offers a substantive solution. The active enforcement requirement (boundary maintenance against mainstream absorption, training continuity) confirms tangled rope rather than degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_equivalence_underdetermination,
    'Does the persistent empirical equivalence between pilot wave mechanics and orthodox quantum mechanics make the ontological commitment to definite trajectories merely conventional, or does it reflect a discoverable physical fact?',
    'Discovery of empirical divergence in nonequilibrium regimes, or continued equivalence across novel experimental tests.',
    'If equivalence is permanent and in-principle, the constraint''s extraction is purely epistemic and its coordination function is underdetermined; if divergence is discovered, the extraction may be reclassified as investment in empirical discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_equivalence_underdetermination, conceptual, 'Whether the interpretive commitment is conventional or factual under empirical equivalence.').

omega_variable(
    suppression_direction_ambiguity,
    'Is the measured suppression driven by mainstream physics excluding Bohmian researchers, or by the Bohmian framework''s own boundary maintenance against competing realist interpretations?',
    'Comparative analysis of citation networks, hiring decisions, and funding patterns distinguishing external exclusion from internal boundary enforcement.',
    'Determines whether the constraint extracts from its own adherents through inward-facing suppression, or from the broader field through outward-facing contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_direction_ambiguity, empirical, 'Internal versus external suppression mechanism ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(quan_tr_t15, quantum_formalism__pilot_wave_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(quan_tr_t30, quantum_formalism__pilot_wave_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(quan_tr_t45, quantum_formalism__pilot_wave_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__pilot_wave_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(quan_tr_t70, quantum_formalism__pilot_wave_reading, theater_ratio, 70, 0.22).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(quan_be_t15, quantum_formalism__pilot_wave_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(quan_be_t30, quantum_formalism__pilot_wave_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(quan_be_t45, quantum_formalism__pilot_wave_reading, base_extractiveness, 45, 0.4).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__pilot_wave_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement(quan_be_t70, quantum_formalism__pilot_wave_reading, base_extractiveness, 70, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__pilot_wave_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(quan_su_t15, quantum_formalism__pilot_wave_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(quan_su_t30, quantum_formalism__pilot_wave_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(quan_su_t45, quantum_formalism__pilot_wave_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement(quan_su_t60, quantum_formalism__pilot_wave_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(quan_su_t70, quantum_formalism__pilot_wave_reading, suppression_requirement, 70, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.08).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel decomposes into three structurally distinct readings (copenhagen, many_worlds, pilot_wave) due to epsilon-invariance violation: each reading assigns different ontological status to the wavefunction and different directionality to the observer, producing different epsilon values and stakeholder structures. They share the same mathematical kernel but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
