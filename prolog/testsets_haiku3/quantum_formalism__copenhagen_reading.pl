% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Copenhagen Interpretation: Wavefunction Collapse as Physical Boundary
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The Copenhagen interpretation of quantum mechanics claims that
 *   wavefunction collapse is a physical process occurring at measurement,
 *   introducing an irreducible epistemic boundary between quantum system and
 *   classical apparatus. This reading treats 'measurement' as a primitive
 *   ontological category and abandons determinism as a foundational
 *   principle. It is one of at least three empirically equivalent readings of
 *   the quantum formalism (many-worlds and pilot-wave being the primary
 *   siblings). The Copenhagen reading's institutional dominance—its status as
 *   the 'standard' interpretation taught in textbooks—is historically
 *   contingent but structurally entrenched. The constraint operates as
 *   tangled rope: it coordinates the practical problem of using quantum
 *   mechanics for predictions (genuine coordination benefit for
 *   experimentalists and engineers) while simultaneously extracting a
 *   conceptual monopoly over interpretation from deterministic realist
 *   programs that are empirically equivalent but institutionally
 *   disadvantaged. The claim-metric divergence is intentional: Copenhagen
 *   frames itself as pragmatic and empirically neutral (coordination story),
 *   while its metrics reveal active enforcement of a particular ontological
 *   stance (extraction machinery).
 *
 * KEY AGENTS:
 *   - copenhagen_interpretive_school — Agenda-setter (institutional): maintains textbook authority and peer-review gatekeeping
 *   - deterministic_ontology_advocates — Payer (organized): identity-locked against Copenhagen's indeterminism; must choose to work within the framework or exit quantum mechanics
 *   - hidden_variable_program — Payer (moderate): structurally suppressed by Copenhagen's declaration that hidden variables are conceptually unnecessary
 *   - measurement_apparatus_engineers — Beneficiary (powerful): the framework's practical operational rules work; no requirement to settle deeper questions
 *   - many_worlds_program — Excluded (organized): alternative interpretive program disadvantaged by Copenhagen's institutional dominance despite mathematical equivalence
 *   - interpretive_pluralism_advocates — Observer (moderate): analytical seat noting that Copenhagen's authority is social fact, not logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.68).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.71).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Interpretation: Wavefunction Collapse as Physical Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '0f60ed17-f22b-43d1-8ade-d80fa77c87aa').
narrative_ontology:cs_kernel_codification('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', fixed_text).
narrative_ontology:cs_authority_grounding('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', extraction).
narrative_ontology:cs_interpretation_layer_present('0f60ed17-f22b-43d1-8ade-d80fa77c87aa').
narrative_ontology:cs_reading_relation('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', foundational, measurement_is_primitive_ontological_category).
narrative_ontology:cs_axiom_status(measurement_is_primitive_ontological_category, holdable).
narrative_ontology:cs_axiom_grounding('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', measurement_is_primitive_ontological_category, deontological).
narrative_ontology:cs_axiom('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', foundational, indeterminism_is_fundamental_physical_fact).
narrative_ontology:cs_axiom_status(indeterminism_is_fundamental_physical_fact, holdable).
narrative_ontology:cs_axiom_grounding('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', indeterminism_is_fundamental_physical_fact, empirically_contingent).
narrative_ontology:cs_axiom('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', secondary, observer_role_essential_to_theory).
narrative_ontology:cs_axiom_status(observer_role_essential_to_theory, holdable).
narrative_ontology:cs_axiom_grounding('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', observer_role_essential_to_theory, conventional).
narrative_ontology:cs_reference_frame('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', measurement_as_fundamental_boundary).
narrative_ontology:cs_drift_state('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', contemporary_decoherence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0f60ed17-f22b-43d1-8ade-d80fa77c87aa', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_interpretive_school).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, deterministic_ontology_advocates).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, hidden_variable_program).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, measurement_apparatus_engineers).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, physics_textbook_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the Copenhagen reading through textbook authority, institutional teaching hierarchy, and peer-review gatekeeping. Controls which quantum mechanics formalism is taught as the 'standard' or 'orthodox' interpretation. The reading's legitimacy derives from its institutional entrenchment and its framing as pedagogically pragmatic (the formalism works; questions about 'what really happens' are dismissed as metaphysics). Collects prestige and research program funding by defending the interpretation against alternative readings.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_interpretive_school, agenda_setter,
    institutional, generational, arbitrage, global).

% Researchers and philosophers committed to a deterministic, realist account of physical reality who interpret the Copenhagen reading as abandoning the physical worldview. They must either work within the Copenhagen formalism (which requires accepting measurement as fundamental and indeterminism as real) or exit the quantum mechanical framework entirely by pursuing alternative interpretations (pilot-wave, many-worlds, objective collapse). Their identity as deterministic realists conflicts with the Copenhagen framework's core commitments, creating psychological and career-path pressure.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, deterministic_ontology_advocates, payer,
    organized, generational, identity_locked, global).

% The research program seeking deterministic completion of quantum mechanics through hidden-variable theories. The Copenhagen reading actively suppresses this program by declaring measurement-induced indeterminism fundamental and treating hidden variables as metaphysical excess. Researchers pursuing hidden-variable approaches face publication barriers in mainstream journals, reduced access to institutional positions, and difficulty securing funding—not because the mathematics is flawed, but because the Copenhagen reading treats the research agenda itself as conceptually misguided.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, hidden_variable_program, payer,
    moderate, biographical, constrained, global).

% Practitioners who design, build, and operate quantum experiments. The Copenhagen reading's treatment of measurement as fundamental provides them genuine operational guidance: the formalism tells them exactly how to calculate probabilities of outcomes and when to invoke collapse. They need not settle questions about what 'really' happens in the apparatus; the framework pragmatically works. Their benefit is that the Copenhagen reading supplies the exact operational rules they need without requiring commitment to a deeper ontology.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, measurement_apparatus_engineers, beneficiary,
    powerful, biographical, mobile, global).

% Institutions and authors who control pedagogical authority through textbooks and courses. The Copenhagen reading's institutional status (as the 'standard' interpretation taught first) gives textbook authors and departments a canonical frame that requires no interpretation justification—measurement is simply how quantum mechanics works. This authority persists even though alternative interpretations are mathematically equivalent.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, physics_textbook_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, physics_textbook_authority, beneficiary).

% The alternative interpretive program claiming universal determinism through branching worlds. The Copenhagen reading's framing of measurement-induced collapse as fundamental makes many-worlds appear ontologically extravagant (all possible outcomes realized) and unnecessary. Many-worlds researchers face pressure to justify why branch proliferation is preferable to Copenhagen's simplicity, even though both formalism and empirical predictions are identical.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, many_worlds_program, excluded,
    organized, generational, constrained, global).

% Philosophers and historians of physics who argue that multiple interpretations of quantum mechanics are equally valid and that no interpretation can claim privileged status. They observe that the Copenhagen reading's institutional dominance is a historical accident and social fact, not a logical consequence of the formalism. They provide analytical perspective on how the reading's enforcement machinery (textbook authority, peer review, institutional teaching practices) sustains itself independent of empirical evidence.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, interpretive_pluralism_advocates, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, copenhagen_interpretive_school).
narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified operational framework for calculating quantum mechanical predictions: measurement is treated as a well-defined primitive concept, collapse converts superposition to definite outcomes, and probabilities are calculated from the Born rule. This solves the coordination problem of teaching and practicing quantum mechanics without requiring agreement on deeper ontology—practitioners can 'shut up and calculate' without resolving philosophical disputes.
% TRANSFER_FUNCTION: Moves the right to set research agendas and define 'legitimate' physics from deterministic realist programs (hidden variables, objective collapse) to the Copenhagen institutional apparatus. Resources (funding, journal space, academic positions) flow to researchers working within the Copenhagen framework and away from those pursuing alternatives, regardless of empirical equivalence. The transfer is enforced through textbook authority and peer-review gatekeeping rather than empirical superiority.
% ABSENT_VOICES: Deterministic realists and hidden-variable researchers are present but structurally disadvantaged. The completely excluded voices are pre-quantum-mechanics deterministic ontology as a foundational assumption—the reading presents indeterminism as physically necessary, making deterministic physical completion impossible by fiat rather than by evidence.
% DISAPPEARANCE_RATIONALE: If the Copenhagen reading vanished and alternative interpretations gained equal institutional standing, research agendas would immediately shift: hidden-variable programs would receive funding, textbooks would teach multiple interpretations as equivalent, and the apparent authority of 'measurement creates indeterminism' would evaporate. The quantum formalism itself would not change, but its legitimacy structure and the distribution of research resources would reorganize around interpretive pluralism.
% FOUNDING_PROBLEM: Early quantum mechanics produced paradoxes and measurement problems. The Copenhagen reading solved the practical problem of making predictions by treating measurement as a fundamental, irreducible boundary between quantum system and classical apparatus. It avoided the infinite regress of asking what 'really' happens during measurement by declaring that question unanswerable and outside physics.
% FOUNDING_PROBLEM_CORROBORATION: The Copenhagen school attests the founding problem remains live: quantum measurement remains puzzling and all alternative approaches add interpretive complexity. Independent philosophers and historians of physics (Paul Teller, Tim Maudlin, David Wallace) attest that the founding problem is substantially solved by modern decoherence theory and that the Copenhagen reading persists as institutional convention, not as a response to an unsolved physical puzzle. Empirical equivalence of interpretations is universally conceded by specialists outside the Copenhagen school.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the Copenhagen reading enforces a particular ontological stance (measurement as fundamental, indeterminism as real) that is not empirically distinguishable from alternatives but claims conceptual necessity. Suppression is even higher (0.71) because the constraint actively prevents competing research programs (hidden variables, many-worlds, objective collapse) from receiving equal institutional standing through textbook authority and peer-review gatekeeping. Theater ratio is moderate (0.42) because the constraint does provide genuine operational value—the formalism gives correct predictions—but an increasing share of enforcement activity defends the ontological monopoly rather than the practical calculation machinery. Accessibility collapse is high (0.79) because once a researcher accepts the quantum formalism, the Copenhagen reading appears as the only 'standard' option; alternatives are presented as exotic speculative add-ons rather than equivalent foundations. Resistance is moderate (0.58) because the hidden-variable and many-worlds programs continue to exist and attract researchers, even under institutional pressure, and because decoherence theory increasingly undermines the need for primitive measurement. The measurement series show rising extractiveness over the interval: from t=0 (early quantum era, when alternative interpretations had more equal standing) to t=100 (contemporary era, when Copenhagen's textbook monopoly is nearly complete). Theater ratio rises more slowly, indicating that operational pragmatism remains the constraint's primary legitimacy, but the increasing gap between theater and extractiveness marks the growing share of enforcement dedicated to ontological enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Copenhagen institutional apparatus) and the payer seats (deterministic realists, hidden-variable researchers) compute radically different types from this constraint. From the agenda-setter's position, the constraint is rope (genuine coordination on practical prediction-making rules that all must use) with minimal extraction (the framework is empirically justified). From the payer seats, the constraint is snare (metaphysical choice enforced through institutional monopoly, with legitimate alternatives suppressed). From the engineer's position, the constraint is nearly rope-pure (operational rules that work; no requirement to commit to the ontology). The engine computes per-seat classifications from the structural data: the Copenhagen apparatus has high directionality-as-beneficiary (d near 0.0), deterministic realists have high directionality-as-targets (d near 1.0), engineers sit near d=0.5 (symmetric benefit from practical rules, no cost from ontological enforcement). This divergence is not a measurement error; it is the structural truth the classification system exists to expose.
 *
 * DIRECTIONALITY LOGIC:
 *   The Copenhagen interpretive school is the clear beneficiary: it controls textbook authority, receives institutional prestige as the 'standard' interpretation, and benefits directly from the constraint's enforcement machinery (expanded funding, publication dominance, teaching authority). Its directionality is near 0.0 (full beneficiary). Deterministic ontology advocates are the primary victims: they are identity-locked against the Copenhagen framework by the foundational principle of determinism, forced to either work within a framework that contradicts their core commitment or exit the research program entirely. Their directionality is near 1.0 (full target). Hidden-variable researchers are secondary victims: their research program is not precluded by the formalism, but the Copenhagen reading's claim that hidden variables are 'unnecessary' creates publication and funding barriers. Their exit is constrained rather than identity-locked, placing d in the 0.75–0.85 range. Measurement apparatus engineers are near-symmetric (d ≈ 0.5): they benefit from Copenhagen's clear operational rules and suffer no cost from accepting the ontological framework if they choose not to, but they are passively pushed toward Copenhagen by its institutional dominance and by textbooks that present it as the only option. Many-worlds and pilot-wave researchers are excluded, not payers: they are structurally barred from equivalence with Copenhagen at the institutional level, despite empirical equivalence. Directionality overrides are not needed here; the structural data (beneficiary/victim declarations + exit options + power atoms) derive the correct directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of the Copenhagen reading was the measurement paradox in early quantum mechanics: how to reconcile the reversible, deterministic evolution of the wavefunction (the Schrödinger equation) with the irreversible, apparently random outcomes of measurement. The reading solved this by declaring measurement a primitive, irreducible boundary between quantum system and classical apparatus, and abandoning determinism as a foundational assumption. The founding problem status is contested: the Copenhagen school claims measurement remains fundamentally puzzling and all alternatives add ontological complexity; deterministic realist critics claim the founding problem is substantially solved by modern decoherence theory (which derives apparent measurement-like transitions from universally quantum principles without primitive collapse) and that Copenhagen's persistence is institutional inertia, not response to unresolved physics. Disappearance verdict is world_rearranges: if the Copenhagen reading lost institutional dominance, research agendas would immediately shift toward alternative interpretations, textbooks would teach multiple readings as equivalent, and the apparent necessity of indeterminism would evaporate. The constraint's classification as tangled_rope rests on the tension between its genuine coordination function (shared mathematical framework, clear prediction rules) and its extractive machinery (ontological enforcement, suppression of alternatives). The mandatrophy ambiguity is acute: if the founding problem is genuinely live, the constraint is justified as a response to an open physical question, and the extraction is a side effect of defending necessary physics. If the founding problem is dead (decoherence solves it), the constraint is a zombie: the original purpose is satisfied, but the institutional enforcement persists because Copenhagen benefits from its entrenched authority. The measurement series showing rising extractiveness (while theater ratio rises more slowly) suggests the latter: the constraint is becoming increasingly extractive and decreasingly coordinative as decoherence theory matures, indicating mandatrophy drift. An omega variable captures this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_foundationality_underdetermined,
    'Is measurement a physically fundamental category, or a pragmatically useful boundary relative to classical apparatus?',
    'A complete theory of quantum decoherence that explains classical/quantum boundary emergence from universally quantum principles (no primitive measurement postulate) would suggest measurement is instrumental, not foundational. Alternatively, a proof that some form of measurement collapse is necessary to explain empirical facts (e.g., the irreversibility of information loss) would support foundationality.',
    'If measurement is foundational, the Copenhagen reading''s core claim holds and indeterminism is irreducible. If measurement emerges from decoherence, the constraint''s foundational premise collapses and the reading becomes a limiting-case approximation of many-worlds or other deterministic frameworks. This is the central contestation between this reading and its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_foundationality_underdetermined, conceptual, 'Whether measurement is a primitive ontological category or an emergent epistemic boundary.').

omega_variable(
    observer_role_necessity_ambiguity,
    'Does the Copenhagen reading genuinely require an ''observer'' role as a fundamental physical distinction, or is this a residual artifact of its historical formulation?',
    'Modern decoherence theory shows that quantum-classical boundary can be defined by entanglement and information-theoretic criteria without invoking an observer. If a fully observer-free formulation produces identical empirical predictions, the observer role is instrumental, not ontological. Conversely, if any observer-free completion either fails or requires additional hidden-variable assumptions, observer presence is constitutive.',
    'If the observer role is genuine, the Copenhagen reading is truly distinctive (measurement cannot be eliminated from theory). If observer-free formulations work, the observer role is a residual scaffolding from Copenhagen''s pedagogical function, and the reading conflates epistemological convenience with ontology. This affects the constraint''s extractiveness: genuine observer role → measurement as primitive → constraint is partly natural law; instrumental role → constraint is pure institutional gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observer_role_necessity_ambiguity, empirical, 'Whether the observer/measurement distinction is ontologically necessary or pragmatically convenient.').

omega_variable(
    indeterminism_empirical_content,
    'Does the Copenhagen claim of irreducible indeterminism at measurement have any empirical content, or does it make exactly the same predictions as fully deterministic alternatives?',
    'Bell-type experiments and loophole-free tests already establish that local hidden variables are empirically ruled out. However, deterministic non-local hidden-variable theories (pilot-wave with particle trajectories) reproduce all Copenhagen predictions. The question is whether any experimental scenario exists where Copenhagen indeterminism (multiple outcomes genuinely realized) differs empirically from deterministic branching. If not, the claim of indeterminism is purely metaphysical, not physical.',
    'Empirical equivalence would mean the Copenhagen reading''s core claim—''indeterminism is real''—is not a physical claim but a metaphysical choice. The constraint would then be purely extractive: enforcing adoption of an underdetermined ontology through institutional means rather than empirical evidence. This would shift classification toward snare (pure extraction under a coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indeterminism_empirical_content, empirical, 'Whether indeterminism is empirically distinguishable from deterministic branching or is metaphysical choice.').

omega_variable(
    reading_identity_kernel_ambiguity,
    'Is this constraint a reading of a single contested kernel (quantum formalism) or are Copenhagen, many-worlds, and pilot-wave three different physical theories that happen to share mathematical structure?',
    'If the three readings are interpretations of one kernel (the same formalism with different physical referents), they should have identical empirical predictions and differ only on ontology—which is the case. If they are distinct theories, they should differ empirically or require different formal apparatus. They do not. This confirms kernel-reading status, but leaves open the question of whether the shared kernel is a physical law or an institutional artifact (the mathematical formalism as a conventionally defined object).',
    'If the kernel is physical law, the three readings are genuinely different ways to complete the physics; the Copenhagen reading is one legitimate metaphysical option. If the kernel is institutional convention, the three readings are equally valid epistemic frames, and Copenhagen''s dominance is purely social, not physical. This affects whether the constraint is a natural law defended against misunderstanding (mountain-like) or an institutional monopoly over interpretation (snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_kernel_ambiguity, conceptual, 'Whether the quantum formalism kernel is physical law or institutional convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__copenhagen_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(quan_tr_t40, quantum_formalism__copenhagen_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__copenhagen_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(quan_tr_t80, quantum_formalism__copenhagen_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(quan_tr_t100, quantum_formalism__copenhagen_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__copenhagen_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(quan_be_t40, quantum_formalism__copenhagen_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__copenhagen_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(quan_be_t80, quantum_formalism__copenhagen_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(quan_be_t100, quantum_formalism__copenhagen_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(quan_su_t20, quantum_formalism__copenhagen_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(quan_su_t40, quantum_formalism__copenhagen_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(quan_su_t60, quantum_formalism__copenhagen_reading, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(quan_su_t80, quantum_formalism__copenhagen_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(quan_su_t100, quantum_formalism__copenhagen_reading, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The quantum formalism kernel admits three structurally distinct readings, each instantiated as a separate constraint story. Copenhagen, many-worlds, and pilot-wave readings are empirically equivalent but impose different ontological structures. The three stories are linked as a constraint family via affects_constraints edges. Each story models the constraint from the reading's own frame: Copenhagen (this story) treats measurement as fundamental and indeterminism as real; many-worlds treats deterministic branching as fundamental; pilot-wave treats particle positions and hidden variables as fundamental. No reading is empirically privileged; all are live options. The constraint family together model the interpretive pluralism that contemporary physics recognizes in practice but suppresses in pedagogy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
