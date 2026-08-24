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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Copenhagen Interpretation: Collapse as Physical Epistemic Boundary
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The Copenhagen interpretation presents wavefunction collapse as a
 *   fundamental physical process that marks an absolute epistemic boundary —
 *   measurement irreducibly produces indeterminism. This reading of the
 *   quantum formalism kernel became textbook orthodoxy through institutional
 *   capture (Bohr's Copenhagen institute, postwar US physics dominance,
 *   textbook standardization) rather than empirical superiority over
 *   alternatives. The constraint operates as a Mountain claim ('this is how
 *   nature works') while extracting professional and epistemic resources from
 *   alternative interpretations. The FSM (false summit mountain) signature
 *   applies: beneficiaries are identifiable, the natural-law claim is
 *   contested, and omegas document the ambiguity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.45).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.55).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Interpretation: Collapse as Physical Epistemic Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, 'b457dcbd-b86b-4599-b1c4-34ea49e15d27').
narrative_ontology:cs_kernel_codification('b457dcbd-b86b-4599-b1c4-34ea49e15d27', fixed_text).
narrative_ontology:cs_authority_grounding('b457dcbd-b86b-4599-b1c4-34ea49e15d27', lineage).
narrative_ontology:cs_interpretation_layer_present('b457dcbd-b86b-4599-b1c4-34ea49e15d27').
narrative_ontology:cs_reading_relation('b457dcbd-b86b-4599-b1c4-34ea49e15d27', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('b457dcbd-b86b-4599-b1c4-34ea49e15d27', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('b457dcbd-b86b-4599-b1c4-34ea49e15d27', foundational, measurement_is_primitive_ontological_category).
narrative_ontology:cs_axiom_status(measurement_is_primitive_ontological_category, holdable).
narrative_ontology:cs_axiom_grounding('b457dcbd-b86b-4599-b1c4-34ea49e15d27', measurement_is_primitive_ontological_category, conventional).
narrative_ontology:cs_axiom('b457dcbd-b86b-4599-b1c4-34ea49e15d27', foundational, wavefunction_collapse_is_physical_process).
narrative_ontology:cs_axiom_status(wavefunction_collapse_is_physical_process, holdable).
narrative_ontology:cs_axiom_grounding('b457dcbd-b86b-4599-b1c4-34ea49e15d27', wavefunction_collapse_is_physical_process, conventional).
narrative_ontology:cs_axiom('b457dcbd-b86b-4599-b1c4-34ea49e15d27', secondary, determinism_abandoned_at_measurement).
narrative_ontology:cs_axiom_status(determinism_abandoned_at_measurement, holdable).
narrative_ontology:cs_axiom_grounding('b457dcbd-b86b-4599-b1c4-34ea49e15d27', determinism_abandoned_at_measurement, conventional).
narrative_ontology:cs_reference_frame('b457dcbd-b86b-4599-b1c4-34ea49e15d27', bohr_heisenberg_copenhagen_1927).
narrative_ontology:cs_drift_state('b457dcbd-b86b-4599-b1c4-34ea49e15d27', contemporary_quantum_foundations, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b457dcbd-b86b-4599-b1c4-34ea49e15d27', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_physicists).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, textbook_authors).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, standard_model_practitioners).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, quantum_computing_orthodoxy).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, students_taught_as_settled).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, foundational_physics_funding_panels).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_problem_as_dissolved).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, complementarity_principle).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, born_rule_as_fundamental).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founders and inheritors of the Copenhagen interpretation (Bohr, Heisenberg, Pauli, and their institutional descendants). They established the interpretive framework that became textbook orthodoxy, controlling graduate training, journal editorships, and Nobel-recognized research programs. Their professional identity and institutional authority are fused with the interpretation's status as settled physics.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_physicists, beneficiary,
    institutional, generational, arbitrage, global).

% Authors and publishers of standard quantum mechanics textbooks (Griffiths, Sakurai, Nielsen & Chuang, etc.) who present the Copenhagen interpretation as the only coherent reading. They shape what generations of students learn as 'quantum mechanics' rather than 'an interpretation of quantum mechanics.' Their commercial and pedagogical incentives align with stability of the orthodoxy.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, textbook_authors, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, textbook_authors, agenda_setter).

% The vast majority of working physicists who use quantum formalism instrumentally for particle physics, condensed matter, quantum chemistry, and quantum computing. They benefit from a stable, uncontested interpretive framework that lets them calculate without foundational distraction. Their exit is mobile — they can ignore foundations entirely — but their daily practice reinforces the orthodoxy.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, standard_model_practitioners, beneficiary,
    organized, biographical, mobile, global).

% The quantum information/computing community that treats the Copenhagen measurement postulate as operational definition (measurement = basis projection + Born rule). Their funding, architectures, and error-correction paradigms are built on this operational reading. Challenging it threatens the conceptual coherence of their entire field.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_computing_orthodoxy, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, quantum_computing_orthodoxy, agenda_setter).

% Physicists and philosophers pursuing Many Worlds, pilot-wave, QBism, collapse models, and other non-Copenhagen interpretations. They bear professional costs: marginal publication venues, difficulty placing students, funding disadvantages, and the 'crackpot' stigma that attaches to foundational dissent. Their exit is identity-locked — leaving the field means abandoning their research identity; staying means permanent structural disadvantage.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers, payer,
    moderate, biographical, identity_locked, global).

% Graduate and undergraduate students who encounter quantum mechanics only through the Copenhagen lens in required courses. They pay an epistemic cost: the measurement problem is presented as resolved, alternative readings are omitted or caricatured, and the interpretive nature of the framework is concealed. Their exit is trapped — they must pass exams on this material to advance, and most never learn alternatives exist.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, students_taught_as_settled, payer,
    powerless, biographical, trapped, global).

% NSF, DOE, ERC, and national agency panels that allocate foundational physics funding. They are agenda-setters by controlling resources, but also payers in the sense that the Copenhagen orthodoxy's dominance creates a self-reinforcing funding logic: 'interpretations are philosophy, not physics' becomes a criterion that excludes alternatives. Their exit is constrained by bureaucratic inertia and peer-review capture.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundational_physics_funding_panels, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, foundational_physics_funding_panels, payer).

% Philosophers who analyze the interpretive landscape as a structural object of study. They map the commitments, costs, and power dynamics of each reading without being professionally bound to any single one. Their analytical seat sees the full constraint family — Copenhagen, Many Worlds, pilot wave, and others — as live options in an unresolved dispute.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophy_of_physics_community, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, calculation-ready interpretive framework that lets the physics community agree on what counts as a prediction, a measurement, and a result — enabling cumulative progress in applications without perpetual foundational debate.
% TRANSFER_FUNCTION: Transfers epistemic authority and professional resources (jobs, funding, publication access, student placement) from alternative interpretation researchers to the Copenhagen orthodoxy; transfers the burden of proof onto dissenters who must justify questioning 'settled physics.'
% ABSENT_VOICES: Physicists in non-Western traditions who might develop distinct interpretive frameworks; early-career researchers who leave physics rather than accept the orthodoxy; historians of science who document how the Copenhagen interpretation achieved dominance through institutional power rather than empirical superiority — all structurally excluded from the physics mainstream's interpretive conversation.
% DISAPPEARANCE_RATIONALE: If the Copenhagen reading vanished overnight, the physics community would not revert to a pre-interpretive state — it would fracture into competing interpretive camps (Many Worlds, pilot wave, QBism, collapse models, etc.), funding and hiring would reorganize around pluralism, textbooks would need rewriting, and quantum computing's conceptual foundations would become contested. The world rearranges because the constraint actively structures the field's social epistemology.
% FOUNDING_PROBLEM: The 1920s crisis of classical ontology in atomic physics: how to make sense of quantum formalism's probabilistic predictions without a classical particle trajectory. The Copenhagen reading was built to dissolve this problem by declaring measurement a primitive that needs no further explanation — the epistemic boundary is the solution.
% FOUNDING_PROBLEM_CORROBORATION: Bohr and Heisenberg's own writings attest the problem was classical ontology's failure; contemporary philosophers of physics (e.g., Maudlin, Wallace, Norsen) attest the measurement problem persists and the founding dissolution is contested; the alternative interpretation communities (Many Worlds, pilot wave) exist precisely because they reject the founding problem as dissolved. No corroboration from outside the beneficiary set accepts the problem as solved.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) is moderate — the constraint doesn't extract money but extracts epistemic authority and career capital. Suppression (0.55) is moderate — alternatives aren't banned but are structurally marginalized (funding, hiring, publication, pedagogy). Theater ratio (0.15) is low — the interpretive framework genuinely coordinates calculation and prediction for working physicists. Accessibility collapse (0.88) is high — once you accept the Copenhagen framework, alternatives appear incoherent or metaphysical. Resistance (0.12) is low — the orthodoxy faces little organized resistance within mainstream physics. The measurement series show extractiveness and suppression rising as the interpretation hardened into orthodoxy (1927-1970), then stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (Copenhagen physicists, textbook authors), the constraint appears as a Mountain — a genuine discovery about nature's epistemic limits. From the payer seats (alternative researchers, students), it appears as a Snare — an enforced interpretive monopoly that suppresses dissent. From the practitioner seat (standard model physicists), it appears as a Rope — a useful coordination device they'd rather not question. The engine computes this divergence from the structural data; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Copenhagen physicists and textbook authors are structural beneficiaries (d near 0.0) — they collect authority and resources from the orthodoxy. Standard model practitioners are near-symmetric (d ~ 0.5) — they benefit from coordination but pay no direct cost. Quantum computing orthodoxy is a beneficiary (d ~ 0.2) — their field's coherence depends on the operational reading. Alternative interpretation researchers are full targets (d near 1.0) — identity-locked, they bear the extraction. Students are trapped targets (d = 1.0) — no exit, full epistemic cost. Funding panels are agenda-setters with constrained exit (d ~ 0.3) — they administer the constraint but are also captured by it. Philosophers are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classical ontology's failure in atomic physics) was live in 1927. Today it is contested: the Copenhagen reading claims dissolution; alternative readings claim the problem persists. The constraint persists not because the founding problem is live, but because the interpretive framework has become the professional infrastructure of physics. Mandatrophy is unresolved — the arrangement's coordination function (stable calculation framework) is real, but its extraction function (marginalizing alternatives) has outlived any epistemic justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Copenhagen reading a genuine discovery of nature''s epistemic structure, or an institutional convention that achieved dominance through historical contingency and power?',
    'Historical analysis of the 1927-1950 period: whether the Copenhagen interpretation''s dominance resulted from empirical adjudication or from Bohr''s institutional authority, postwar US physics hegemony, and textbook capture. Counterfactual: if Many Worlds had been proposed in 1927 with equal institutional backing, would it have become orthodoxy?',
    'If conventional, the Mountain claim is a false summit — the constraint is a constructed social epistemic boundary with identifiable beneficiaries, not a natural law. FSM signature would reclassify as tangled_rope. If genuine discovery, the Mountain classification stands and beneficiaries are incidental to truth-tracking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the Copenhagen reading''s Mountain status reflects nature or power.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of alternative interpretations structural (funding, hiring, publication gatekeeping) or internalized (physicists genuinely believe alternatives are unscientific)?',
    'Survey data on physicist beliefs about interpretations; analysis of whether suppression persists in domains with reduced gatekeeping (e.g., quantum foundations conferences, philosophy of physics journals, independent research). If internalized, the constraint''s effective suppression is higher than structural measures suggest.',
    'If internalized, the constraint operates as a deeper epistemic trap — practitioners carry the suppression with them. This would increase effective extraction for payer seats and strengthen the FSM case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative interpretations.').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (stable calculation framework) separable from the extraction function (marginalizing alternatives), or does the coordination require the extraction?',
    'Natural experiment: quantum computing and quantum information theory use the Copenhagen operational rules without ontological commitment to collapse. If these fields thrive without the interpretive orthodoxy, coordination and extraction are separable. Historical analysis of whether textbook pedagogy could present formalism agnostically.',
    'If separable, the extraction is gratuitous — a Tangled Rope where coordination is real but extraction is parasitic. If inseparable, some extraction is the price of coordination — a genuine Mountain with unavoidable epistemic boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.05).
narrative_ontology:measurement(quan_tr_t1950, quantum_formalism__copenhagen_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__copenhagen_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(quan_tr_t1990, quantum_formalism__copenhagen_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__copenhagen_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__copenhagen_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.25).
narrative_ontology:measurement(quan_be_t1950, quantum_formalism__copenhagen_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__copenhagen_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(quan_be_t1990, quantum_formalism__copenhagen_reading, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__copenhagen_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__copenhagen_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1927, quantum_formalism__copenhagen_reading, suppression_requirement, 1927, 0.3).
narrative_ontology:measurement(quan_su_t1950, quantum_formalism__copenhagen_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(quan_su_t1970, quantum_formalism__copenhagen_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(quan_su_t1990, quantum_formalism__copenhagen_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__copenhagen_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(quan_su_t2025, quantum_formalism__copenhagen_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.03).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_computing_measurement_postulate).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, textbook_quantum_mechanics_pedagogy).

% DUAL FORMULATION NOTE:
% This constraint is one member of the quantum_formalism constraint family. The kernel 'quantum formalism' admits multiple structurally distinct readings with different ε values: Copenhagen (this story, ε=0.45, claimed Mountain, FSM candidate), Many Worlds (ε≈0.15, claimed Mountain, genuine coordination), Pilot Wave (ε≈0.25, claimed Tangled Rope). They are linked by network.affects_constraints because the upstream kernel's empirical success is cited as evidence for each downstream reading, and the readings compete for the same professional resources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__copenhagen_reading, institutional, 0.15).
constraint_indexing:directionality_override(quantum_formalism__copenhagen_reading, moderate, 0.85).
constraint_indexing:directionality_override(quantum_formalism__copenhagen_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
