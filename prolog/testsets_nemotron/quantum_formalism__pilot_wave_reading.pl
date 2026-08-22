% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot-Wave (Bohmian) Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The pilot-wave (Bohmian) reading of the quantum formalism asserts that
 *   particles possess definite positions at all times, guided by a physically
 *   real wavefield (the wavefunction) that evolves unitarily and never
 *   collapses. Measurement reveals pre-existing properties; the observer is
 *   eliminable; determinism is restored at the fundamental level. The cost is
 *   explicit nonlocality: the guidance equation couples particle velocities
 *   instantaneously across arbitrary distances, requiring a preferred
 *   foliation of spacetime in relativistic extensions. This reading has
 *   persisted since 1927 (de Broglie), was revived in 1952 (Bohm), and gained
 *   renewed attention after Bell's theorem (1964) and Bell's own advocacy
 *   (1980s). It remains a minority position institutionally but is
 *   structurally coherent and empirically equivalent to standard quantum
 *   mechanics. The constraint is the commitment to this reading as a
 *   legitimate interpretation of the shared kernel 'quantum formalism.'
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.18).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.25).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave (Bohmian) Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '32fe64e2-53f5-4631-81de-660c7212da53').
narrative_ontology:cs_kernel_codification('32fe64e2-53f5-4631-81de-660c7212da53', formalized).
narrative_ontology:cs_authority_grounding('32fe64e2-53f5-4631-81de-660c7212da53', lineage).
narrative_ontology:cs_interpretation_layer_present('32fe64e2-53f5-4631-81de-660c7212da53').
narrative_ontology:cs_reading_relation('32fe64e2-53f5-4631-81de-660c7212da53', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('32fe64e2-53f5-4631-81de-660c7212da53', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('32fe64e2-53f5-4631-81de-660c7212da53', foundational, particles_have_definite_positions_at_all_times).
narrative_ontology:cs_axiom_status(particles_have_definite_positions_at_all_times, holdable).
narrative_ontology:cs_axiom_grounding('32fe64e2-53f5-4631-81de-660c7212da53', particles_have_definite_positions_at_all_times, deontological).
narrative_ontology:cs_axiom('32fe64e2-53f5-4631-81de-660c7212da53', foundational, wavefunction_is_physical_guiding_field).
narrative_ontology:cs_axiom_status(wavefunction_is_physical_guiding_field, holdable).
narrative_ontology:cs_axiom_grounding('32fe64e2-53f5-4631-81de-660c7212da53', wavefunction_is_physical_guiding_field, deontological).
narrative_ontology:cs_axiom('32fe64e2-53f5-4631-81de-660c7212da53', secondary, measurement_reveals_pre_existing_properties).
narrative_ontology:cs_axiom_status(measurement_reveals_pre_existing_properties, holdable).
narrative_ontology:cs_axiom_grounding('32fe64e2-53f5-4631-81de-660c7212da53', measurement_reveals_pre_existing_properties, empirically_contingent).
narrative_ontology:cs_reference_frame('32fe64e2-53f5-4631-81de-660c7212da53', deterministic_single_world_ontology).
narrative_ontology:cs_drift_state('32fe64e2-53f5-4631-81de-660c7212da53', post_bell_theorem_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('32fe64e2-53f5-4631-81de-660c7212da53', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, realist_ontologists).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, deterministic_foundationalists).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, nonlocality_tolerant_physicists).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, operationalist_physicists).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, locality_purists).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, resource_constrained_modelers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, nonlocality_tolerant_physicists).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, particle_trajectories_exist).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, measurement_reveals_pre_existing_properties).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, wavefunction_is_physical_field).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, determinism_restored_at_fundamental_level).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain a clear ontology where particles have definite positions at all times and the wavefunction is a real physical field guiding them. They invest careers in developing and defending this picture against the dominant Copenhagen consensus. Exit means abandoning a research program that has survived decades of marginalization; the mathematical framework is mature but institutional recognition remains limited.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, realist_ontologists, beneficiary,
    organized, generational, constrained, global).

% Find in pilot-wave theory the only fully deterministic, observer-free formulation of quantum mechanics. They value the restoration of classical causality at the fundamental level. Exit requires adopting either irreducible indeterminism (Copenhagen) or ontological extravagance (many worlds), both of which violate their core metaphysical commitments.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, deterministic_foundationalists, beneficiary,
    moderate, biographical, constrained, global).

% Accept explicit nonlocality as the price of determinism and realism. They benefit from a clear causal story but pay in the form of a guidance equation that acts instantaneously across arbitrary distances — a feature that creates tension with relativistic causality and makes the theory difficult to extend to quantum field theory. They can exit to other interpretations more easily than the committed realists.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, nonlocality_tolerant_physicists, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, nonlocality_tolerant_physicists, payer).

% Dominate the institutional physics establishment; the Copenhagen/von Neumann framework is the standard curriculum, the language of textbooks, and the basis of quantum technology. They pay nothing to maintain their position — it is the default. Pilot-wave theory imposes a cost on them only when it demands curricular space, funding, or recognition as an equally valid interpretation. They can arbitrage between interpretations freely because the operational formalism (predictions) is identical.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, operationalist_physicists, payer,
    institutional, biographical, arbitrage, global).

% View the explicit nonlocality of the guidance equation as a fatal defect — not a feature but a bug that violates the spirit of relativity. For them, pilot-wave theory is not a viable option; the cost of accepting it is the abandonment of a core principle (local causality) that structures their entire research worldview. Exit from this position would mean a restructuring of identity; they are locked into opposition by the very architecture of their theoretical commitments.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, locality_purists, payer,
    organized, generational, identity_locked, global).

% Need to compute quantum many-body systems; the pilot-wave formulation adds a high-dimensional configuration space and a guidance equation that is computationally intractable for all but the simplest systems. They are trapped by the empirical adequacy of standard quantum mechanics and the practical impossibility of using Bohmian mechanics for real calculations. They bear the cost of a theory that makes the same predictions but offers no computational advantage.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, resource_constrained_modelers, payer,
    powerless, immediate, trapped, global).

% Study the landscape of quantum interpretations as a structural phenomenon. They see pilot-wave theory as a live, coherent reading of the quantum formalism that restores determinism and realism at the cost of explicit nonlocality and a preferred foliation in relativistic extensions. They track the sociological dynamics: why a mathematically complete theory remains marginalized, what counts as 'explanatory virtue' in different communities, and how the kernel 'quantum formalism' supports multiple incompatible readings.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, interpretation_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, deterministic, observer-free ontology for quantum phenomena: particles have definite trajectories guided by a real wavefield, reproducing all standard quantum predictions without collapse or branching.
% TRANSFER_FUNCTION: Moves explanatory burden from 'measurement creates reality' (Copenhagen) or 'all outcomes realized' (Many Worlds) to 'nonlocal guidance field steers particles' — the cost is explicit nonlocality and a preferred foliation; the benefit is a clear, deterministic particle ontology.
% ABSENT_VOICES: Quantum gravity researchers who need a background-independent formulation — pilot wave requires a preferred foliation, which conflicts with diffeomorphism invariance. They are structurally excluded because the theory's nonlocal guidance equation cannot be made generally covariant without major modification. Also absent: philosophers of science who reject 'unobservable trajectories' as metaphysically extravagant — they would object to the ontological commitment to particle positions that are, in principle, inaccessible to measurement beyond the equilibrium distribution.
% DISAPPEARANCE_RATIONALE: If the pilot-wave reading vanished overnight, the community of realist ontologists and deterministic foundationalists would lose their only mathematically complete, observer-free formulation of quantum mechanics. The standard formalism would remain empirically adequate, but the conceptual space of 'quantum mechanics without observers' would collapse — no other interpretation offers both determinism and a single-world ontology. Textbook physics would be unchanged; the interpretive landscape would be impoverished.
% FOUNDING_PROBLEM: The measurement problem: standard quantum mechanics posits a fundamental, irreducible distinction between unitary evolution and measurement-induced collapse, making the observer a primitive element of physical law. Pilot wave was built to eliminate this dualism by giving particles definite positions at all times and making the wavefunction a physical field that guides them deterministically.
% FOUNDING_PROBLEM_CORROBORATION: Bohm (1952) and Bell (1982, 1987) attest the founding problem is the measurement problem and that pilot wave solves it. Operationalist physicists (majority) attest the problem is either dissolved by decoherence or not a problem at all — 'shut up and calculate.' Many-worlds proponents attest the problem is real but solved by universal unitarity, not hidden variables. No consensus exists; the founding problem's status is structurally contested across the kernel's readings.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.18) is low but nonzero: the constraint extracts cognitive and institutional resources from physicists who must either engage with a marginalized framework or actively suppress it. It does not extract material rents. Suppression (0.25) is moderate: the constraint's persistence has historically required active defense against institutional neglect (1950s-1970s) and, more recently, against the claim that decoherence + many worlds makes hidden variables unnecessary. Theater ratio (0.12) is low: the mathematical core is substantive and the community does serious work, but a performative element exists in 'defending the one true ontology' rhetoric. Accessibility collapse (0.65) is moderately high: once the pilot-wave picture is understood, alternatives (collapse, branching) appear as ontological extravagance or instrumentalism — but the mathematical equivalence means operationalists can always retreat to 'same predictions.' Resistance (0.55) is significant: the dominant institutional framework resists curricular inclusion, funding parity, and equal status.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (realist ontologists, deterministic foundationalists), the constraint is a Rope: a genuine coordination solution to the measurement problem with minimal coercive overhead — they choose it freely. From the payer seats (locality purists, resource-constrained modelers), it registers as a Snare or Tangled Rope: the nonlocality is a coercive metaphysical cost, and the computational intractability is a practical extraction with no exit. The engine computes this divergence from the structural data; the claimed_type 'tangled_rope' reflects the hybrid character — genuine coordination function (deterministic ontology) plus asymmetric extraction (nonlocality burden on locality purists, computational burden on modelers) requiring active enforcement (institutional defense against marginalization).
 *
 * DIRECTIONALITY LOGIC:
 *   Realist ontologists and deterministic foundationalists are beneficiaries (d ~ 0.1-0.2): they gain a coherent ontology that matches their metaphysical commitments. Nonlocality-tolerant physicists are near-symmetric (d ~ 0.45): they accept the nonlocality cost for the determinism benefit. Operationalist physicists are agenda-setters who experience the constraint as a low-cost imposition (d ~ 0.15) — they control the institutional default. Locality purists are identity-locked payers (d ~ 0.9): the constraint violates a core principle that constitutes their theoretical identity. Resource-constrained modelers are trapped payers (d ~ 0.95): they bear computational intractability with no exit. Interpretation scholars are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (measurement problem) is contested, not dead. Pilot wave continues to solve a live problem for its adherents. However, the constraint shows mandatrophy dynamics: the original mandate (solve the measurement problem) has been partially superseded by decoherence theory, which explains the appearance of collapse without hidden variables. Yet the arrangement persists and even grows because it satisfies a deeper mandate (restore determinism and realism) that decoherence does not address. The constraint is not a piton — it has active defenders, growing mathematical development (relativistic extensions, quantum field theory attempts), and a coherent research program. It is a tangled rope: coordination + extraction, actively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the pilot-wave reading a distinct constraint with its own ε, or merely a different observable on the same constraint ''quantum formalism''?',
    'Apply the ε-invariance test: if evaluating the constraint via ''empirical predictions'' gives ε ≈ 0 (all readings predict the same), but evaluating via ''ontological commitments'' gives ε > 0 (nonlocality burden, computational cost), then the readings are distinct constraints. The referent is the standing arrangement (the interpretive landscape), not the formalism itself.',
    'If distinct constraints, each reading gets its own story, ε, and classification linked by network.affects_constraints. If one constraint, the ε-invariance principle is violated and the framework must model observable-dependent classification — which it forbids.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to ε-invariance forces decomposition of the quantum_formalism kernel into separate constraint stories per reading.').

omega_variable(
    nonlocality_as_extraction_or_coordination,
    'Is the explicit nonlocality of the guidance equation a coordination cost (necessary for deterministic single-world ontology) or an extractive burden imposed on locality-purist physicists?',
    'Compare with Many Worlds: both are deterministic single-formalism readings. Many Worlds pays ontological extravagance (many worlds); Pilot Wave pays nonlocality. If locality purists are a coherent victim class who bear costs without consent, the nonlocality functions as extraction. If nonlocality is the necessary price of the coordination function (determinism + realism), it is a Boltzmann floor cost.',
    'If coordination cost, ε decreases and the constraint moves toward Rope. If extractive burden on a victim class, ε increases and Tangled Rope/Snare classification is reinforced. Affects Boltzmann floor calculation for coordination_type ''identity_coordination''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonlocality_as_extraction_or_coordination, conceptual, 'Whether the theory''s defining structural feature (nonlocality) is functional overhead or asymmetric extraction.').

omega_variable(
    relativistic_extension_viability,
    'Can pilot-wave theory be extended to a fully relativistic quantum field theory without a preferred foliation?',
    'Ongoing research: Dürr et al.''s ''Bohmian quantum field theory'' proposals, Struyve''s reviews, and the ''no-go'' arguments (e.g., Myrvold 2019). Empirical test: if a relativistic extension requires a preferred foliation that conflicts with empirical Lorentz invariance, the theory faces a structural crisis.',
    'If no relativistic extension exists without preferred foliation, the constraint''s coordination function degrades for fundamental physics (becomes a non-relativistic effective theory only), increasing theater_ratio and potentially triggering mandatrophy reclassification toward Piton. If a viable extension exists, the constraint strengthens as a full Rope/Tangled Rope for quantum gravity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relativistic_extension_viability, empirical, 'The central technical open question determining the constraint''s long-term viability in fundamental physics.').

omega_variable(
    cs_framing_underdetermination,
    'Does the quantum_formalism kernel instantiate a commitment-system structure (fixed kernel + authoritative interpretive layer), or is it a distributed epistemic community with no designated authority?',
    'Examine whether any institution or practice functions as the authoritative interpreter of ''what quantum mechanics means.'' The Copenhagen reading has textbook authority (Bohr/Heisenberg lineage); Many Worlds has no central authority (distributed); Pilot Wave has a lineage (de Broglie → Bohm → Bell → contemporary researchers) but no institutional authority. The framing choice changes authority_grounding and interpretation_layer_present.',
    'If authority_grounding = lineage with interpretation_layer_present = true, the kernel has CS structure and drift_state analysis applies. If authority_grounding = distributed/none, CS structure is absent and the constraint is a standard interpretive claim. Affects whether cs_structure fields are warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Framing under-determination: is there an authoritative interpreter of the quantum formalism kernel?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 1927, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__pilot_wave_reading, theater_ratio, 1927, 0.02).
narrative_ontology:measurement(quan_tr_t1952, quantum_formalism__pilot_wave_reading, theater_ratio, 1952, 0.05).
narrative_ontology:measurement(quan_tr_t1966, quantum_formalism__pilot_wave_reading, theater_ratio, 1966, 0.1).
narrative_ontology:measurement(quan_tr_t1982, quantum_formalism__pilot_wave_reading, theater_ratio, 1982, 0.12).
narrative_ontology:measurement(quan_tr_t1995, quantum_formalism__pilot_wave_reading, theater_ratio, 1995, 0.11).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__pilot_wave_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__pilot_wave_reading, base_extractiveness, 1927, 0.05).
narrative_ontology:measurement(quan_be_t1952, quantum_formalism__pilot_wave_reading, base_extractiveness, 1952, 0.12).
narrative_ontology:measurement(quan_be_t1966, quantum_formalism__pilot_wave_reading, base_extractiveness, 1966, 0.08).
narrative_ontology:measurement(quan_be_t1982, quantum_formalism__pilot_wave_reading, base_extractiveness, 1982, 0.15).
narrative_ontology:measurement(quan_be_t1995, quantum_formalism__pilot_wave_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__pilot_wave_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1927, quantum_formalism__pilot_wave_reading, suppression_requirement, 1927, 0.1).
narrative_ontology:measurement(quan_su_t1952, quantum_formalism__pilot_wave_reading, suppression_requirement, 1952, 0.2).
narrative_ontology:measurement(quan_su_t1966, quantum_formalism__pilot_wave_reading, suppression_requirement, 1966, 0.35).
narrative_ontology:measurement(quan_su_t1982, quantum_formalism__pilot_wave_reading, suppression_requirement, 1982, 0.25).
narrative_ontology:measurement(quan_su_t1995, quantum_formalism__pilot_wave_reading, suppression_requirement, 1995, 0.22).
narrative_ontology:measurement(quan_su_t2025, quantum_formalism__pilot_wave_reading, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.08).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the quantum_formalism kernel. The kernel decomposes into three constraint stories (pilot_wave, copenhagen, many_worlds) with different ε values, beneficiary/victim structures, and claimed types. Pilot wave: ε=0.18, Tangled Rope, coordination_type=identity_coordination. Copenhagen: ε≈0.05 (institutional default), Rope or Mountain from institutional seat, coordination_type=enforcement_mechanism. Many Worlds: ε≈0.12, Tangled Rope, coordination_type=identity_coordination. The ε-invariance principle requires separate stories because measuring 'extraction' via 'ontological cost' vs. 'empirical adequacy' gives different ε — the label 'quantum formalism' conflates structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, organized, 0.15).
constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, powerless, 0.95).
constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
