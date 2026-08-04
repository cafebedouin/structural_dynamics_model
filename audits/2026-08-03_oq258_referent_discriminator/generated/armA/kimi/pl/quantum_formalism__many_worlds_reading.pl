% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__many_worlds_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading of Quantum Formalism
 *   domain: philosophy of physics / quantum foundations / interpretive epistemology
 *
 * SUMMARY:
 *   This constraint instantiates the many-worlds reading of the quantum
 *   formalism kernel: the claim that the universal wavefunction evolves
 *   deterministically, that measurement is emergent from decoherence-induced
 *   apparent branching, and that all outcomes are realized in separate
 *   worlds. The reading is presented by its advocates as the minimal, natural
 *   reading of the formalism—simply 'taking the Schrödinger equation
 *   seriously'—but imposes a radical ontological expansion that competing
 *   readings reject as extravagant. As a contested interpretive commitment
 *   system within physics, it coordinates a substantial research program in
 *   quantum foundations and cosmology while asymmetrically extracting
 *   legitimacy and resources from collapse-model and instrumentalist
 *   alternatives.
 *
 * KEY AGENTS:
 *   - MWI research program (agenda_setter / organized / constrained exit): Sets the interpretive agenda in realist quantum foundations, trains graduate students in Everettian frameworks, and controls funding and publication channels.
 *   - Quantum cosmology program (beneficiary / organized / constrained exit): Depends on the no-collapse framework to treat the wavefunction of the universe literally; benefits from the ontological license MWI provides.
 *   - Collapse phenomenology researchers (payer / moderate / constrained exit): Bear the cost of marginalization as their research program is delegitimized by the MWI claim that collapse is unnecessary.
 *   - Instrumentalist physicists (payer / organized / mobile exit): Bear the diffuse cost of ontological confusion and curricular drift despite their preference for an operational stance.
 *   - Philosophy of physics observers (analytical seat): Document the interpretive contest without enforcing a particular reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.45).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.55).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy of physics / quantum foundations / interpretive epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '3e779f58-d1de-466d-906e-634bca70e6dd').
narrative_ontology:cs_kernel_codification('3e779f58-d1de-466d-906e-634bca70e6dd', formalized).
narrative_ontology:cs_authority_grounding('3e779f58-d1de-466d-906e-634bca70e6dd', expertise).
narrative_ontology:cs_interpretation_layer_present('3e779f58-d1de-466d-906e-634bca70e6dd').
narrative_ontology:cs_reading_relation('3e779f58-d1de-466d-906e-634bca70e6dd', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('3e779f58-d1de-466d-906e-634bca70e6dd', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('3e779f58-d1de-466d-906e-634bca70e6dd', foundational, universal_wavefunction_literal_reality).
narrative_ontology:cs_axiom_status(universal_wavefunction_literal_reality, holdable).
narrative_ontology:cs_axiom_grounding('3e779f58-d1de-466d-906e-634bca70e6dd', universal_wavefunction_literal_reality, empirically_contingent).
narrative_ontology:cs_axiom('3e779f58-d1de-466d-906e-634bca70e6dd', foundational, collapse_postulate_superfluous).
narrative_ontology:cs_axiom_status(collapse_postulate_superfluous, holdable).
narrative_ontology:cs_axiom_grounding('3e779f58-d1de-466d-906e-634bca70e6dd', collapse_postulate_superfluous, instrumental).
narrative_ontology:cs_reference_frame('3e779f58-d1de-466d-906e-634bca70e6dd', bare_formalism_literal_reading).
narrative_ontology:cs_drift_state('3e779f58-d1de-466d-906e-634bca70e6dd', contemporary_foundations_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3e779f58-d1de-466d-906e-634bca70e6dd', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, mwi_research_program).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_cosmology_program).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, collapse_phenomenology_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, instrumentalist_physicists).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, decoherence_as_measurement_mechanism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, unitary_evolution_universal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coordinates research in Everettian quantum mechanics, decoherence theory, and branching-world ontology. Sets the agenda for what counts as a legitimate realist solution to the measurement problem. Attracts funding, faculty positions, and graduate students through the promise of a collapse-free framework.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, mwi_research_program, agenda_setter,
    organized, generational, constrained, global).

% Treats the universal wavefunction as a literal object of study. Requires a no-collapse interpretation because there is no external observer for the universe. Benefits from the ontological license that MWI provides to quantize the entire cosmos.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_cosmology_program, beneficiary,
    organized, generational, constrained, universal).

% Develops and tests dynamical collapse models such as GRW and Penrose-Diosi. Their research program is delegitimized by the MWI claim that collapse is unnecessary or illusory. Bears the cost of marginalization in foundations journals, hiring committees, and funding competitions where MWI is treated as the default realist option.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, collapse_phenomenology_researchers, payer,
    moderate, biographical, constrained, global).

% Comprises the majority of practicing physicists who use quantum mechanics as a predictive tool without realist ontological commitment. The MWI reading imposes an ontological framework they regard as cognitively extravagant and pedagogically misleading. They bear the diffuse cost of popular confusion and the gradual shift of graduate curricula toward realist interpretations.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, instrumentalist_physicists, payer,
    organized, biographical, mobile, global).

% Analyzes the interpretive landscape without enforcing a particular reading. Documents the structural relationships between competing interpretations, the underdetermination of theory by data, and the sociological distribution of commitment across physics communities.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophy_of_physics_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, mwi_research_program).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a realist, deterministic interpretation of quantum mechanics by eliminating the measurement problem through decoherence: all physical systems, including observers, are treated as quantum systems evolving within the universal wavefunction, removing the need for a collapse postulate or privileged observers.
% TRANSFER_FUNCTION: Moves ontological commitment from a single collapsed classical outcome to a continuously branching multiverse; moves intellectual authority, graduate students, and research funding from collapse-model programs and instrumentalist physics to decoherence-based foundations and quantum cosmology.
% ABSENT_VOICES: Practicing physicists who regard interpretive debates as metaphysically extravagant and prefer an instrumentalist stance; they would resist the ontological inflation of the multiverse but are largely excluded from foundations discourse where MWI is presented as the natural reading of the formalism.
% DISAPPEARANCE_RATIONALE: If the MWI reading vanished as a live interpretive option, Everettian quantum mechanics and much of quantum cosmology would lose their ontological framework. Decoherence theory would survive as a technical tool but would no longer be read as generating literal worlds. The measurement problem would return to center stage in foundations, and research funding and hiring would shift back toward collapse phenomenology, pilot-wave mechanics, or information-theoretic approaches.
% FOUNDING_PROBLEM: The measurement problem: how to reconcile the deterministic, linear Schrödinger evolution with the apparent randomness and definiteness of measurement outcomes without introducing an unphysical collapse process or privileging conscious observers.
% FOUNDING_PROBLEM_CORROBORATION: Researchers in dynamical collapse theory and information-theoretic interpretations attest from outside the MWI beneficiary set that the measurement problem remains live and that MWI's solution is purchased at the cost of ontological extravagance and the unresolved preferred-basis problem; historians of physics note that the problem was framed differently in the Copenhagen era and that its current formulation is shaped by the realist commitments of the MWI program.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).
:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: MWI captures significant intellectual authority, funding, and graduate labor in foundations, but the 'payment' is ontological and disciplinary rather than directly economic. Suppression (0.55) is moderate-to-high: Copenhagen remains dominant in pedagogy, but within realist foundations venues MWI exercises substantial gatekeeping through peer review and hiring, while alternatives face higher burden of proof. Theater ratio (0.28) is low-moderate: the rhetoric of 'just taking the formalism seriously' is partly performative, but the underlying decoherence mathematics is genuinely productive. Accessibility collapse (0.60) reflects that once inside the MWI framework, collapse appears conceptually confused and alternatives seem unmotivated. Resistance (0.50) captures sustained opposition from Copenhagen, pilot-wave, and instrumentalist camps.
 *
 * PERSPECTIVAL GAP:
 *   From the MWI research seat, the constraint is a Rope: it restores determinism, eliminates the artificial collapse postulate, and solves the measurement problem at no empirical cost. From the collapse phenomenology seat, the same structure is a Snare: it imposes an unobservable multiverse, diverts funding from testable collapse models, and dresses up ontological extravagance as theoretical modesty. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The MWI research program and the quantum cosmology program are structural beneficiaries: the constraint subsidizes their existence by providing a legitimate research framework and ontological license (low d, toward the beneficiary end). Collapse phenomenology researchers are structural targets: the constraint extracts from them by delegitimizing their core research object (high d). Instrumentalist physicists are diffuse targets: they pay through curricular and popular confusion but have mobile exit (moderate-high d). The cosmology program is effectively identity-locked to a no-collapse framework because cosmology lacks an external observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—the measurement problem—remains contested. MWI claims to have solved it, but critics argue it merely displaces the problem into the preferred-basis issue or purchases the solution with ontological extravagance. Because the founding problem is contested rather than dead, and because the constraint still has concentrated beneficiaries actively maintaining it, the reading has not atrophied into a Piton. If decoherence theory were shown insufficient to define branches uniquely, the coordination function would degrade and the constraint would drift toward pure extraction or collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    basis_problem_resolution,
    'Does decoherence theory fully resolve the preferred-basis problem, or does MWI surreptitiously rely on additional structure such as the environment''s Hilbert-space decomposition to define branches?',
    'Rigorous derivation showing whether the branching structure is uniquely determined by the Hamiltonian and initial conditions alone, or if basis choice remains underdetermined.',
    'If the basis problem is unresolved, MWI''s coordination value (solving the measurement problem) is weakened and its extraction (imposing a multiverse ontology) increases relative to its genuine function; if resolved, the coordination claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basis_problem_resolution, conceptual, 'Ambiguity about whether decoherence alone yields unique branching').

omega_variable(
    empirical_distinguishability,
    'Will any empirical test ever distinguish the many-worlds reading from Copenhagen or pilot-wave, or are the readings permanently underdetermined by observational data?',
    'Future experiments searching for collapse signatures, quantum-gravitational effects, or deviations from unitary evolution; if none are found after exhaustive search, underdetermination is effectively permanent.',
    'If permanently underdetermined, the constraint remains a contested commitment system; if empirical discrimination becomes possible, the reading could move toward Mountain (confirmed) or collapse entirely (falsified).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_distinguishability, empirical, 'Whether competing quantum interpretations are empirically distinguishable').

omega_variable(
    sociological_capture_vs_argumentative_force,
    'Is MWI''s growing prominence in quantum foundations driven by the argumentative force of its formalism, or by sociological capture through citation networks, prestigious advocates, and training pipelines?',
    'Bibliometric analysis of hiring and funding in foundations combined with ethnographic study of graduate training and peer-review norms.',
    'If sociological capture dominates, suppression and theater_ratio should be revised upward, strengthening the extraction profile; if argumentative force dominates, the coordination function is primary and the constraint is closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sociological_capture_vs_argumentative_force, empirical, 'Ambiguity about whether MWI dominance is intellectual or sociological').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(quan_tr_t14, quantum_formalism__many_worlds_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(quan_tr_t28, quantum_formalism__many_worlds_reading, theater_ratio, 28, 0.15).
narrative_ontology:measurement(quan_tr_t42, quantum_formalism__many_worlds_reading, theater_ratio, 42, 0.2).
narrative_ontology:measurement(quan_tr_t56, quantum_formalism__many_worlds_reading, theater_ratio, 56, 0.25).
narrative_ontology:measurement(quan_tr_t70, quantum_formalism__many_worlds_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(quan_be_t14, quantum_formalism__many_worlds_reading, base_extractiveness, 14, 0.22).
narrative_ontology:measurement(quan_be_t28, quantum_formalism__many_worlds_reading, base_extractiveness, 28, 0.3).
narrative_ontology:measurement(quan_be_t42, quantum_formalism__many_worlds_reading, base_extractiveness, 42, 0.37).
narrative_ontology:measurement(quan_be_t56, quantum_formalism__many_worlds_reading, base_extractiveness, 56, 0.42).
narrative_ontology:measurement(quan_be_t70, quantum_formalism__many_worlds_reading, base_extractiveness, 70, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__many_worlds_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% Many-worlds is one reading of the quantum formalism kernel. It shares the same mathematical kernel with Copenhagen and pilot-wave readings but differs in ontological commitments. The ε values differ because the readings impose distinct beneficiary/victim structures and coordination costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
