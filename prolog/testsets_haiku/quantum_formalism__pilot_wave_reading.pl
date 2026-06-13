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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot-Wave Interpretation of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The pilot-wave reading of quantum formalism asserts that particles have
 *   definite positions at all times, guided by a real physical wavefunction
 *   field. This is one of three live competing interpretations of the quantum
 *   mechanical formalism (alongside Copenhagen and many-worlds). The reading
 *   offers determinism and classical ontology but purchases these by
 *   accepting nonlocal guidance—instantaneous influence of the wavefunction
 *   on particle trajectories. The pilot-wave reading does NOT extract
 *   resources from other readings in a financial or institutional sense;
 *   rather, it competes for interpretive legitimacy and research funding. The
 *   extractiveness score (0.31) reflects the cognitive labor and theoretical
 *   resources required to defend nonlocal hidden variables against empirical
 *   equivalence arguments and to sustain a minority research program against
 *   the institutional weight of Copenhagen pedagogy.
 *
 * KEY AGENTS:
 *   - pilot_wave_theorists: Minority research program defending deterministic hidden variables; moderate power, mobile exit (can shift to other interpretations or to applied quantum work)
 *   - copenhagen_adherents: Institutional incumbent; organized power, mobile exit but strong career incentives to stay
 *   - many_worlds_adherents: Rival minority program; organized power, mobile exit
 *   - quantum_measurement_experimentalists: Agnostic observers; institutional power, analytical exit (their data constrains all readings equally)
 *   - foundational_physics_funding_bodies: Agenda-setters; institutional power, analytical position (allocate resources but do not commit to interpretation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.31).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.18).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave Interpretation of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '915da500-75c6-4b5a-9348-fc5ddd5cb0fc').
narrative_ontology:cs_kernel_codification('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', fixed_text).
narrative_ontology:cs_authority_grounding('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', lineage).
narrative_ontology:cs_interpretation_layer_present('915da500-75c6-4b5a-9348-fc5ddd5cb0fc').
narrative_ontology:cs_reading_relation('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', foundational, particles_definite_positions_always).
narrative_ontology:cs_axiom_status(particles_definite_positions_always, holdable).
narrative_ontology:cs_axiom_grounding('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', particles_definite_positions_always, deontological).
narrative_ontology:cs_axiom('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', foundational, measurement_reveals_preexisting_values).
narrative_ontology:cs_axiom_status(measurement_reveals_preexisting_values, holdable).
narrative_ontology:cs_axiom_grounding('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', measurement_reveals_preexisting_values, deontological).
narrative_ontology:cs_axiom('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', secondary, wavefunction_ontologically_real_field).
narrative_ontology:cs_axiom_status(wavefunction_ontologically_real_field, holdable).
narrative_ontology:cs_axiom_grounding('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', wavefunction_ontologically_real_field, empirically_contingent).
narrative_ontology:cs_axiom('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', foundational, determinism_fundamental_not_emergent).
narrative_ontology:cs_axiom_status(determinism_fundamental_not_emergent, holdable).
narrative_ontology:cs_axiom_grounding('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', determinism_fundamental_not_emergent, deontological).
narrative_ontology:cs_reference_frame('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', classical_ontology_restoration).
narrative_ontology:cs_drift_state('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', contemporary_quantum_foundations, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('915da500-75c6-4b5a-9348-fc5ddd5cb0fc', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, deterministic_realism_adherents).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, hidden_variable_research_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, pilot_wave_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, quantum_information_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, physics_pedagogy_community).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, copenhagen_adherents).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, many_worlds_adherents).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, physics_pedagogy_community).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, classical_ontological_realism).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, determinism_locality_separability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and defend the pilot-wave (de Broglie-Bohm) interpretation, claiming it restores classical determinism and particle ontology while reproducing all quantum predictions. They argue measurement outcomes are pre-determined by particle positions, not indeterminate until observation. Their research programs depend on the reading remaining coherent and empirically viable.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, pilot_wave_theorists, beneficiary,
    moderate, generational, mobile, global).

% Defend the Copenhagen interpretation, treating the wavefunction as an epistemic tool (not ontologically real) and measurement as an irreducible physical process that produces genuine indeterminism. The pilot-wave reading directly contradicts their axiom that the wavefunction is not a real physical field. They argue pilot-wave determinism is purchased at unacceptable costs (nonlocality, redundant hidden variables, empirical equivalence).
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, copenhagen_adherents, payer,
    organized, generational, mobile, global).

% Defend the many-worlds interpretation, where the universal wavefunction evolves deterministically and measurement is branch-structure decoherence, not collapse. The pilot-wave reading competes with their position by offering determinism without branching. They argue pilot-wave nonlocality violates relativistic locality more seriously than branching violates classical ontology.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, many_worlds_adherents, payer,
    organized, generational, mobile, global).

% Conduct experiments designed to test interpretations: quantum erasure, weak measurement, Bell tests, decoherence timescales. No experiment has yet distinguished pilot-wave from Copenhagen or many-worlds predictions. They collect data that all three readings must accommodate without changing their core axioms.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, quantum_measurement_experimentalists, observer,
    institutional, biographical, analytical, global).

% Apply quantum mechanics to computing, cryptography, and communication. They are agnostic on interpretation—all three readings make identical operational predictions for these domains. The pilot-wave reading's determinism offers no practical advantage here, making it orthogonal to their research programs.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, quantum_information_theorists, beneficiary,
    powerful, biographical, arbitrage, global).

% Teach quantum mechanics to students. The standard approach emphasizes Copenhagen with side remarks on alternatives. Pilot-wave requires teaching nonlocal guidance and hidden variables, complicating the pedagogical story but offering students a deterministic ontology. Adoption depends on whether the extra complexity aids or hinders conceptual understanding.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, physics_pedagogy_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, physics_pedagogy_community, payer).

% Would argue that ontological claims about particles, fields, and determinism are meaningless—only empirical predictions matter. Since pilot-wave and Copenhagen make identical predictions, the positivist position dissolves both interpretations as pseudo-problems. They are excluded from the debate because they reject its framing entirely.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, empiricist_positivists, excluded,
    moderate, generational, mobile, global).

% Allocate research funding to quantum foundational studies, including work on alternative interpretations. They enforce the constraint that all funded work must be scientifically rigorous (subject to peer review) and empirically motivated. Pilot-wave research competes with Copenhagen and many-worlds for scarce resources.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, foundational_physics_funding_bodies, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an interpretive framework for reconciling the empirical predictions of quantum mechanics with classical determinism and particle ontology. Solves the conceptual problem: how can a deterministic physical world produce the appearance of indeterminism in measurement outcomes?
% TRANSFER_FUNCTION: Moves interpretive authority and epistemic legitimacy from Copenhagen (measurement produces indeterminism) to pilot-wave (particles follow deterministic hidden trajectories; measurement reveals pre-existing values). Also redirects research energy and funding from testing indeterminism to exploring nonlocal guidance mechanisms.
% ABSENT_VOICES: Philosophers of physics who argue the interpretation question is empirically vacuous (positivists, functionalists) are excluded by design—their objection is not to this reading but to the entire frame. Physicists in industry and quantum information fields, while nominally part of the discipline, have no stake in the interpretive choice since all readings make identical predictions for their applications.
% DISAPPEARANCE_RATIONALE: If the pilot-wave reading disappeared and only Copenhagen and many-worlds remained, quantum mechanical predictions and experimental outcomes would be indistinguishable. The formalism, mathematics, and physical results stay the same. The dispute is purely about the ontological story told *after* calculating the empirical predictions. No experiment fails, no technology breaks, no practical outcome shifts.
% FOUNDING_PROBLEM: Early quantum mechanics (1920s–1930s) presented an interpretive crisis: the formalism predicts probabilities of measurement outcomes, but the theory is silent on what exists between measurements. Copenhagen treats this silence as a feature (measurement is irreducible). Pilot-wave restored classical ontology by proposing particles have definite positions at all times, guided by a real physical wavefunction.
% FOUNDING_PROBLEM_CORROBORATION: Pilot-wave theorists (de Broglie, Bohm, Bell, contemporary researchers) attest the founding problem motivates their approach. Copenhagen adherents (Heisenberg, Bohr tradition, modern textbook authors) attest the founding problem is misconceived—the appearance of indeterminism is not a crisis but a feature reflecting the limits of classical ontology. The dispute is framed by professional historians and philosophers of physics (e.g., David Albert, Paul Teller, Tim Maudlin) who document both readings and their structural incompatibility without endorsing either.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_unchanged).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__pilot_wave_reading_tests).
:- end_tests(quantum_formalism__pilot_wave_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 at interval end) because the reading generates no direct rent or material extraction; the 'cost' is mainly cognitive—sustaining a deterministic alternative narrative against Copenhagen's institutional momentum and many-worlds' conceptual parsimony. The measurement series show slow rise from 0.15 to 0.31 over the interval (0–100, calibrated to post-1992 [Bell's theorem and Bohm's revival] through ~2025). This reflects growing research interest (e.g., foundational quantum mechanics prizes, experimental Bell tests, quantum revival conferences) providing incremental institutional recognition without displacing Copenhagen as the default textbook reading. Theater ratio climbs slowly (0.08 to 0.22) because the reading's main defensive activity is rhetorical—emphasizing the 'nonlocality problem' in Copenhagen to motivate pilot-wave as a solution, while performing philosophical rigor (Bell's theorem, guidance equation derivations) to legitimize the approach. Accessibility collapse is moderate (0.62): once a physicist understands the three interpretations, alternatives remain conceptually accessible (each is mathematically coherent; switching requires only retraining, not institutional exit). Resistance is substantial (0.58) because Copenhagen and many-worlds researchers actively argue against pilot-wave nonlocality and redundancy.
 *
 * PERSPECTIVAL GAP:
 *   The Copenhagen-to-pilot-wave seat divergence is the clearest. From Copenhagen's seat (institutional baseline), the constraint appears as a troublemaker interpretation that refuses to accept the measurement axiom. From pilot-wave's seat, Copenhagen is a philosophical capitulation that treats measurement as fundamental rather than emergent. Both perceptions are structurally grounded in different answers to: What is fundamental—the wavefunction or the measurement outcome? Is indeterminism a feature of reality or a failure of classical intuition? Does observer-dependence solve problems or create them? These are not empirically resolvable—they are conceptual commitments prior to empirical testing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is most interesting at the 'payer' and 'beneficiary' boundaries. Pilot-wave theorists are beneficiaries (the reading advances their research programs, offers them a coherent alternative narrative, and increasingly provides institutional recognition). Copenhagen and many-worlds adherents experience pilot-wave as a cost: the reading competes for cognitive attention, research funding, and student interest. They pay through the effort required to argue against it and through the marginal loss of institutional monopoly on 'reasonable interpretations.' Experimentalists and quantum information theorists are roughly symmetric (neither gains nor loses from the choice; they benefit incidentally from the conceptual clarity the debate produces). Funding bodies sit at the agenda-setter position: they allocate resources to all live interpretations but experience no direct extraction. The reading's beneficiaries are structural minorities; its payers are incumbents defending a position of authority rather than losing material goods.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy trap by remaining contested. If the founding problem (classical-ontology realism vs. measurement indeterminism) were universally agreed to be *dead*, the pilot-wave reading would become a Piton—an interpretive framework maintained theatrically despite no longer motivating it. Currently, the founding problem status is actively contested (some physicists argue it is live, others that it is misconceived), which keeps the reading as a live Rope among specialists. The measurement data (rising extractiveness, rising theater ratio, stable accessibility collapse, stable resistance) track a constraint that is neither collapsing into irrelevance nor gaining dominant institutional power—it persists as a specialist alternative sustained by research programs and philosophical commitment. The absence of mandatrophy (no sudden theater-ratio spike, no collapse in accessibility) suggests the reading will either (a) crystallize into a permanent minority position (modern Piton if the founding problem becomes universally dismissed as pseudo-problem), or (b) gain institutional weight if empirical breakthroughs favor deterministic interpretations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_equivalence_problem,
    'Can any future experiment distinguish pilot-wave from Copenhagen or many-worlds predictions? If not, is the interpretive choice meaningful or merely conventional?',
    'Theoretical discovery of a scenario (e.g., a quantum observable) that pilot-wave and Copenhagen predict differently, or formal proof that empirical equivalence is necessary given the mathematics.',
    'If empirical equivalence is permanent, the constraint becomes purely conventional (a matter of taste); if distinguishable experiments exist, pilot-wave gains empirical traction and may cease to be a minority reading. Either outcome would shift the classification from Rope (interpretive minority) toward either Piton (aesthetic convention) or Mountain (empirically grounded).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_equivalence_problem, empirical, 'Whether empirical equivalence to Copenhagen and many-worlds is fundamental or contingent.').

omega_variable(
    nonlocality_cost_tolerance,
    'Is nonlocal guidance (instantaneous influence of wavefunction on particle trajectories) physically acceptable in a relativistic world, or does it constitute a fundamental violation of relativity''s causal structure?',
    'Relativistic reformulation of pilot-wave mechanics that preserves Lorentz covariance without sacrificing determinism, or formal proof of incompatibility between relativistic locality and pilot-wave determinism.',
    'If nonlocality is shown to be compatible with relativity (or merely non-covariant without violation), pilot-wave loses one of its primary criticisms and may gain institutional credibility. If nonlocality remains genuinely incompatible with relativistic principles, Copenhagen''s objection stands and pilot-wave remains a coherent but unacceptable trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonlocality_cost_tolerance, empirical, 'Whether nonlocal guidance is fundamentally compatible with relativistic field theory.').

omega_variable(
    ontological_preference_irreducibility,
    'Is the preference for determinism over indeterminism a fact about physical reality, or a fact about human intuition and cognitive architecture? Can the question itself be answered scientifically?',
    'Meta-analysis of reasoning patterns in physics communities, cognitive science of mathematical concept formation, or historical study of why certain ontologies become dominant in scientific communities.',
    'If determinism-preference is empirically grounded (humans are better at reasoning about deterministic systems; causality emerges from underlying determinism; quantum indeterminism is intrinsic and requires explanation), pilot-wave gains philosophical grounding. If the preference is psychological artifact, the constraint becomes a projection of human cognitive bias rather than a discovery of physical structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_preference_irreducibility, preference, 'Whether determinism is a physical requirement or a psychological predisposition.').

omega_variable(
    reading_vs_formalism_distinction,
    'Is the pilot-wave reading a *reading* of a fixed quantum formalism, or a *different formalism* that happens to yield identical predictions?',
    'Formal demonstration that pilot-wave and Copenhagen are mathematically equivalent reformulations, or discovery of structural differences in their mathematical foundations that go beyond interpretive gloss.',
    'If they are equivalent reformulations, pilot-wave is a notational choice (Piton-like—sustained by aesthetic preference). If they are structurally different formalisms, pilot-wave is an alternative theory that merits independent assessment. This affects whether the constraint is fundamentally about interpretation (choice among readings) or about formalism (choice among equivalent mathematical packages).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_formalism_distinction, conceptual, 'Whether pilot-wave is an interpretation or a distinct formalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__pilot_wave_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(quan_tr_t20, observed).
narrative_ontology:measurement(quan_tr_t40, quantum_formalism__pilot_wave_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(quan_tr_t40, observed).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__pilot_wave_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(quan_tr_t60, observed).
narrative_ontology:measurement(quan_tr_t80, quantum_formalism__pilot_wave_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(quan_tr_t80, observed).
narrative_ontology:measurement(quan_tr_t100, quantum_formalism__pilot_wave_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(quan_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__pilot_wave_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(quan_be_t20, observed).
narrative_ontology:measurement(quan_be_t40, quantum_formalism__pilot_wave_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement_basis(quan_be_t40, observed).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__pilot_wave_reading, base_extractiveness, 60, 0.29).
narrative_ontology:measurement_basis(quan_be_t60, observed).
narrative_ontology:measurement(quan_be_t80, quantum_formalism__pilot_wave_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement_basis(quan_be_t80, observed).
narrative_ontology:measurement(quan_be_t100, quantum_formalism__pilot_wave_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement_basis(quan_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__pilot_wave_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.04).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% The pilot-wave reading is one of three live readings of the quantum formalism kernel. Copenhagen and many-worlds are authored as separate constraints with their own ε values, stakeholder structures, and foundational axioms. All three are linked via network.affects_constraints to enable constraint-family analysis and cross-reading comparison. The decomposition respects ε-invariance: each reading instantiates a distinct claim (what the wavefunction *is*, what measurement *does*, whether indeterminism is *fundamental*), and empirical equivalence does not unify them—their ε values differ because the extractiveness and suppression metrics apply to the *institutional persistence* of the reading, not to empirical predictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
