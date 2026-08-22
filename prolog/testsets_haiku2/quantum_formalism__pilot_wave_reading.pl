% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Pilot Wave Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The pilot-wave (de Broglie-Bohm) reading of quantum mechanics claims that
 *   particles have definite positions at all times, guided by the
 *   wavefunction as a physical field. Measurement reveals pre-existing
 *   positions rather than creating them. This reading is one of three major
 *   interpretations of the quantum formalism kernel, competing with
 *   Copenhagen (wavefunction collapse, observer-dependent) and many-worlds
 *   (universal deterministic evolution, decoherence-induced branching). The
 *   pilot-wave reading presents itself as restoring classical ontology
 *   (realism, determinism, observer-independence) while accepting a cost:
 *   nonlocal guidance. The constraint story frames this reading as extracting
 *   theoretical authority and research attention from Copenhagen
 *   institutional gatekeeping, while vindicating deterministic-ontology and
 *   hidden-variable research programs.
 *
 * KEY AGENTS:
 *   - Pilot-wave theorists: moderate power, set the reading's research agenda, develop mathematical frameworks, propose tests.
 *   - Deterministic-ontology advocates: powerful seats (philosophers, some physicists), benefit from a reading that preserves classical causation and realism.
 *   - Copenhagen institutional gatekeepers: institutional power, control textbooks, curricula, journals; pay the cost of reduced authority.
 *   - Measurement-problem skeptics: organized, regard the measurement problem itself as pseudo-problem; pay the cost of reconceptualizing foundations.
 *   - Quantum foundations journals: institutional agenda-setters, framing pilot-wave as minority curiosity rather than serious competitor.
 *   - Experimentalists and philosophers: observer seats, report data and analyze commitments but do not prefer interpretations on principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.68).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.71).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot Wave Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '2eb815ad-b675-4a75-8f8f-2e8a7365c13e').
narrative_ontology:cs_kernel_codification('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', distributed).
narrative_ontology:cs_authority_grounding('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', expertise).
narrative_ontology:cs_reading_relation('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', quantum_formalism__many_worlds_reading, influences).
narrative_ontology:cs_axiom('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', foundational, particles_definite_positions_all_times).
narrative_ontology:cs_axiom_status(particles_definite_positions_all_times, holdable).
narrative_ontology:cs_axiom_grounding('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', particles_definite_positions_all_times, empirically_contingent).
narrative_ontology:cs_axiom('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', foundational, wavefunction_physical_field_not_collapse).
narrative_ontology:cs_axiom_status(wavefunction_physical_field_not_collapse, holdable).
narrative_ontology:cs_axiom_grounding('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', wavefunction_physical_field_not_collapse, deontological).
narrative_ontology:cs_axiom('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', secondary, determinism_nonlocal_guidance_recoverable).
narrative_ontology:cs_axiom_status(determinism_nonlocal_guidance_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', determinism_nonlocal_guidance_recoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', classical_realism_determinism_framework).
narrative_ontology:cs_drift_state('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', contemporary_quantum_foundations_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2eb815ad-b675-4a75-8f8f-2e8a7365c13e', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, deterministic_ontology_advocates).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, hidden_variable_research_programs).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, copenhagen_institutional_gatekeepers).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, measurement_problem_skeptics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, quantum_foundations_journals).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, classical_determinism_recoverable).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, observer_independence).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, wavefunction_physical_reality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and advocate the de Broglie-Bohm interpretation: particles follow deterministic trajectories guided by the wavefunction as a physical field. They set the research agenda by formulating precise mathematical frameworks, conducting pedagogical outreach, and proposing experimental tests. They maintain that this reading restores classical determinism and observer-independence to quantum mechanics.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, pilot_wave_theorists, agenda_setter,
    moderate, generational, arbitrage, global).

% Benefit from a reading that preserves determinism, locality of ontology (particles have definite positions), and observer-independence. This reading vindicates philosophical commitments to classical causation and realism. It attracts researchers with foundational intuitions favorable to hidden variables and classical-like mechanisms.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, deterministic_ontology_advocates, beneficiary,
    powerful, civilizational, arbitrage, global).

% Curate textbooks, grant funding, journal editorial boards, and doctoral curricula around the Copenhagen reading. The pilot-wave reading's success would displace the institutional authority of Copenhagen interpretation gatekeeping — it calls into question whether measurement-induced collapse is fundamental. Their costs are reputational and resource-allocation shifts.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, copenhagen_institutional_gatekeepers, payer,
    institutional, generational, mobile, global).

% Regard the measurement problem as a pseudo-problem generated by Copenhagen's observer-dependent framing. The pilot-wave reading dissolves the measurement problem by denying wavefunction collapse is physical — it shifts the conceptual terrain in a way that undermines skeptics' objections. They bear the cost of rethinking foundational premises they have relied on.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, measurement_problem_skeptics, payer,
    organized, biographical, constrained, global).

% Also reject wavefunction collapse but advocate universal deterministic evolution without hidden-variable guidance. They are not barred by rules but by differential institutional attention and perceived empirical tractability — the pilot-wave reading's nonlocality is controversial, but many-worlds' branch-structure cardinality is equally contested.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, many_worlds_theorists, excluded,
    moderate, generational, arbitrage, global).

% Manage the formal literature on quantum interpretations. They benefit from publication volume and the appearance of genuine scholarly debate. Pilot-wave papers are published but often framed as minority positions or mathematical curiosities rather than serious competitors to Copenhagen. The reading survives but with reduced legitimacy weight.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, quantum_foundations_journals, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, quantum_foundations_journals, beneficiary).

% Design experiments that might distinguish interpretations (Bell tests, weak measurement, interference experiments). They occupy an analytical seat: they report results that all interpretations must accommodate, but the framing of which interpretation 'explains' the results varies by institutional authority and textbook consensus.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, experimentalists_testing_interpretations, observer,
    organized, biographical, analytical, global).

% Analyze the epistemological and ontological commitments of each interpretation. They see the pilot-wave reading as restoring realism and determinism, but also highlight its nonlocality (action-at-a-distance guidance) and unobservable particles, raising questions about hidden variables' explanatory status.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, philosophy_of_science_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__pilot_wave_reading, deterministic_ontology_advocates).
narrative_ontology:fixing_cost_class(quantum_formalism__pilot_wave_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified framework for quantum mechanics without wavefunction collapse: all quantum phenomena follow deterministic equations; interpretation is uniform across all contexts rather than requiring observer-dependent boundary conditions.
% TRANSFER_FUNCTION: Transfers theoretical authority and research attention from Copenhagen-framed textbooks and institutional curricula toward hidden-variable and deterministic-ontology research programs. Also transfers conceptual coherence: the reading claims to eliminate the measurement problem, shifting the burden of explanation.
% ABSENT_VOICES: Quantum mechanics' operational effectiveness in engineering and technology does not voice any preference among interpretations (all make the same predictions for experiments). Non-physicists affected by quantum technology (semiconductor engineers, quantum-computer developers) are not in the room; their engineering success is cited as evidence by all readings.
% DISAPPEARANCE_RATIONALE: If the pilot-wave reading vanished as a research program, Copenhagen and many-worlds would persist, and the foundational debate would continue but narrower. The mathematical machinery of quantum mechanics and its experimental predictions remain unchanged. What disappears is one conception of what quantum mechanics MEANS — but physics education, engineering, and technology would reorganize unchanged (they do not depend on interpretation).
% FOUNDING_PROBLEM: The measurement problem: quantum mechanics gives definite predictions for measurement outcomes, but the theory before measurement describes superpositions; interpreting what happens AT measurement requires either collapse (Copenhagen), branching (many-worlds), or hidden guidance. The pilot-wave reading was founded to solve this by recovering determinism through hidden variables.
% FOUNDING_PROBLEM_CORROBORATION: Pilot-wave theorists attest the measurement problem is live and their reading solves it. Copenhagen gatekeepers attest the measurement problem is ill-posed (observation is primitive). Many-worlds theorists attest collapse is unnecessary. Philosophy of physics literature corroborates that the measurement problem remains contested across all readings; no consensus exists that any reading has solved it.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures how the reading's adoption and authority displace Copenhagen's institutional hegemony. It is high (0.68 at interval end) because the reading vindicates an alternative ontology that challenges Copenhagen's foundational assumptions; adoption of pilot-wave would require rewriting textbooks, shifting grant funding, and reconceiving the measurement problem. Suppression is comparably high (0.71) because institutional gatekeeping actively maintains Copenhagen's authority by treating pilot-wave as mathematically equivalent but ontologically speculative, rather than as a genuine competitor. Theater ratio (0.42) is moderate: pilot-wave publications emphasize mathematical rigor and empirical equivalence (real function), but also spend significant effort on philosophical defense and reframing of the measurement problem (performative dimension). The measurements show gentle growth over the 40-year interval: extractiveness rises from 0.45 to 0.68 as pilot-wave research accumulates and institutional challenges to Copenhagen mount (Bell experiments, quantum foundations revival), but growth plateaus after t=25 as Copenhagen remains entrenched in pedagogy.
 *
 * PERSPECTIVAL GAP:
 *   Copenhagen gatekeepers experience this reading as threatening: it calls their foundational premises into question (is collapse real? is the measurement problem genuine?). Pilot-wave theorists experience it as vindicating: they are building a coherent alternative. Deterministic-ontology advocates experience it as liberating: classical causation and realism are recoverable. The engine should compute different effective extraction (χ) for each seat from the same structural (ε) because directionality differs: gatekeepers are targets (high d), theorists are beneficiaries (low d), advocates are beneficiaries (low d). The claim (tangled_rope) reflects the actual asymmetry: genuine coordination function (solving the measurement problem in a unified way without collapse) coupled with asymmetric extraction (displacing Copenhagen's authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Pilot-wave theorists (agenda_setter, moderate power) sit as beneficiaries: the reading vindicates their research program and offers a coherent alternative to Copenhagen. Deterministic-ontology advocates (powerful) are clear beneficiaries: the reading recovers classical causal intuitions. Copenhagen institutional gatekeepers (institutional power) are targets: their authority diminishes if pilot-wave is taken seriously. Measurement-problem skeptics are victims: they must reconsider their premises if the measurement problem is real rather than pseudo-problem. Experimentalists and philosophers are observer seats (analytical power) — they report results and analyze structure but do not adopt interpretations on principle. The coordination function (unified framework without observer-dependence) is real but carries asymmetric extraction (displacement of existing authority), justifying tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (measurement problem, solution sought via hidden variables) remains live in quantum foundations: no consensus exists that any reading has solved it definitively. However, the problem has shifted terrain: modern quantum foundations treats it as a legitimate research question rather than a pseudo-problem, and all three readings remain viable. The pilot-wave reading has not become obsolete, but it also has not achieved institutional dominance. The mandatrophy test is the (founding_problem_status × disappearance_verdict) mismatch: founding_problem_status=live + disappearance_verdict=contested + the measured extraction suggests the reading extracts authority without permanent institutional capture. This is piton territory (atrophied primary function, maintained by inertia), not tangled_rope per the measuring gate alone, but the claimed_type reflects the structural presence of both coordination and extraction at the moment of authoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nonlocality_vs_realism_tradeoff,
    'Does pilot-wave nonlocality (instantaneous guidance across space) genuinely recover classical realism, or does it trade the measurement problem for a different action-at-distance problem?',
    'Detailed analysis of whether nonlocal guidance satisfies modern interpretations of realism and separability; comparison with relativity-compatible reformulations of pilot-wave theory (Lorentz-covariant extensions).',
    'If nonlocality is as problematic as wavefunction collapse, the reading''s claim to restore classical ontology is weakened. If it is tractable, the reading''s advantage over Copenhagen and many-worlds is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nonlocality_vs_realism_tradeoff, conceptual, 'Whether pilot-wave''s nonlocality is an acceptable cost of recovery of realism and determinism.').

omega_variable(
    hidden_variable_empirical_reach,
    'Can pilot-wave theory be extended to relativistic regimes, many-body systems, and quantum field theory without accumulating excess mathematical machinery that undermines its simplicity advantage?',
    'Survey of relativistic and field-theoretic extensions of de Broglie-Bohm mechanics; empirical testability of predictions in these domains compared to Copenhagen and many-worlds.',
    'If extensions remain tractable and predictively equivalent, the reading maintains its appeal as a viable interpretation. If they become baroque or predict differently, the reading''s scope shrinks or its empirical status changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_variable_empirical_reach, empirical, 'Scope and tractability of pilot-wave theory''s extensions to modern physics.').

omega_variable(
    quantum_reading_kernel_incommensurability,
    'Are the three readings (Copenhagen, many-worlds, pilot-wave) genuinely alternative readings of one kernel (quantum formalism), or do they constitute three distinct theories with different predictions masquerading as interpretive choices?',
    'Rigorous empirical program testing each reading''s predictions in controlled regimes where they diverge (Bell experiments, quantum metrology, foundations tests); analysis of whether differences are merely instrumental or reflect incompatible ontologies.',
    'If incommensurable, the ''kernel'' framing dissolves and the three readings are three separate constraints. If equivalent under all empirical tests, the readings are genuinely interpretive variants and the committer frame is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_reading_kernel_incommensurability, empirical, 'Whether the three quantum readings are interpretive variants of one kernel or distinct theories.').

omega_variable(
    institutional_suppression_mechanism,
    'Is the pilot-wave reading''s reduced institutional adoption due to genuine scientific merit concerns (nonlocality, field ontology problems) or partly due to Copenhagen''s established gatekeeping and textbook authority?',
    'Historical analysis of citation patterns, grant funding, textbook inclusion, and hiring criteria; counterfactual: if Copenhagen had lost early institutional advantage, would pilot-wave have achieved parity?',
    'If suppression is partly institutional, the measured extractiveness may partially reflect institutional capture rather than pure scientific evaluation. If merit-based, the low institutional standing reflects genuine concerns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_mechanism, empirical, 'Whether pilot-wave''s institutional marginalization reflects scientific merit or institutional suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t5, quantum_formalism__pilot_wave_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(quan_tr_t5, observed).
narrative_ontology:measurement(quan_tr_t10, quantum_formalism__pilot_wave_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(quan_tr_t10, observed).
narrative_ontology:measurement(quan_tr_t15, quantum_formalism__pilot_wave_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(quan_tr_t15, observed).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__pilot_wave_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(quan_tr_t20, observed).
narrative_ontology:measurement(quan_tr_t25, quantum_formalism__pilot_wave_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(quan_tr_t25, observed).
narrative_ontology:measurement(quan_tr_t30, quantum_formalism__pilot_wave_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(quan_tr_t30, observed).
narrative_ontology:measurement(quan_tr_t40, quantum_formalism__pilot_wave_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(quan_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t5, quantum_formalism__pilot_wave_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(quan_be_t5, observed).
narrative_ontology:measurement(quan_be_t10, quantum_formalism__pilot_wave_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(quan_be_t10, observed).
narrative_ontology:measurement(quan_be_t15, quantum_formalism__pilot_wave_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(quan_be_t15, observed).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__pilot_wave_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(quan_be_t20, observed).
narrative_ontology:measurement(quan_be_t25, quantum_formalism__pilot_wave_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(quan_be_t25, observed).
narrative_ontology:measurement(quan_be_t30, quantum_formalism__pilot_wave_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(quan_be_t30, observed).
narrative_ontology:measurement(quan_be_t40, quantum_formalism__pilot_wave_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(quan_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__pilot_wave_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t5, quantum_formalism__pilot_wave_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(quan_su_t5, observed).
narrative_ontology:measurement(quan_su_t10, quantum_formalism__pilot_wave_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(quan_su_t10, observed).
narrative_ontology:measurement(quan_su_t15, quantum_formalism__pilot_wave_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(quan_su_t15, observed).
narrative_ontology:measurement(quan_su_t20, quantum_formalism__pilot_wave_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(quan_su_t20, observed).
narrative_ontology:measurement(quan_su_t25, quantum_formalism__pilot_wave_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(quan_su_t25, observed).
narrative_ontology:measurement(quan_su_t30, quantum_formalism__pilot_wave_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(quan_su_t30, observed).
narrative_ontology:measurement(quan_su_t40, quantum_formalism__pilot_wave_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(quan_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% Quantum formalism kernel constraint family: three readings are three separate constraint stories, each with its own ε, beneficiary/victim structure, and type. Copenhagen_reading (mountain-like: institutional gatekeeping justifies itself as natural): high suppression, low accessibility (alternatives are conceptually collapsed by measurement-problem framing). Many-worlds_reading (rope-like: universal determinism plus decoherence, no collapse): low extraction relative to pilot-wave, no institutional suppression. Pilot-wave_reading (tangled_rope: restores determinism and realism but carries nonlocal extraction cost). The three are linked: adoption of one shifts the empirical and philosophical status of the others. All three remain live in contemporary physics, but Copenhagen retains institutional hegemony.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, powerful, 0.25).
constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
