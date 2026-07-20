% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-14
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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This constraint is the copenhagen_reading of the contested
 *   quantum_formalism kernel. It treats wavefunction collapse as a physical
 *   process and measurement as a primitive ontological category that produces
 *   irreducible indeterminism. The reading functions as both a genuine
 *   coordination deviceâallowing physicists to calculate without solving
 *   the measurement problemâand an asymmetric extraction mechanism that
 *   marginalizes foundational inquiry and alternative interpretations.
 *   Sibling readings (many_worlds_reading, pilot_wave_reading) are linked as
 *   a constraint family.
 *
 * KEY AGENTS:
 *   - physics_academy: Primary agenda-setter (institutional/constrained) â administers curriculum and norms that enforce the reading
 *   - mainstream_quantum_practitioners: Primary beneficiary (organized/constrained) â collects calculational closure and career advantages from operational identity
 *   - alternative_interpretation_researchers: Primary payer (moderate/constrained) â bears marginalization costs despite empirical equivalence
 *   - foundational_inquiry_students: Excluded voice (powerless/trapped) â trapped in operational paradigm by pedagogical foreclosure
 *   - philosophy_of_physics_community: Analytical observer (analytical/analytical) â tracks epistemic foreclosures from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.45).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.48).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '5c5ff507-f3c1-4ea7-9455-36e009082c3c').
narrative_ontology:cs_kernel_codification('5c5ff507-f3c1-4ea7-9455-36e009082c3c', formalized).
narrative_ontology:cs_authority_grounding('5c5ff507-f3c1-4ea7-9455-36e009082c3c', lineage).
narrative_ontology:cs_interpretation_layer_present('5c5ff507-f3c1-4ea7-9455-36e009082c3c').
narrative_ontology:cs_reading_relation('5c5ff507-f3c1-4ea7-9455-36e009082c3c', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c5ff507-f3c1-4ea7-9455-36e009082c3c', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('5c5ff507-f3c1-4ea7-9455-36e009082c3c', foundational, measurement_primitive_ontology).
narrative_ontology:cs_axiom_status(measurement_primitive_ontology, holdable).
narrative_ontology:cs_axiom_grounding('5c5ff507-f3c1-4ea7-9455-36e009082c3c', measurement_primitive_ontology, instrumental).
narrative_ontology:cs_axiom('5c5ff507-f3c1-4ea7-9455-36e009082c3c', foundational, irreducible_physical_indeterminism).
narrative_ontology:cs_axiom_status(irreducible_physical_indeterminism, holdable).
narrative_ontology:cs_axiom_grounding('5c5ff507-f3c1-4ea7-9455-36e009082c3c', irreducible_physical_indeterminism, empirically_contingent).
narrative_ontology:cs_reference_frame('5c5ff507-f3c1-4ea7-9455-36e009082c3c', measurement_primitive_framework).
narrative_ontology:cs_drift_state('5c5ff507-f3c1-4ea7-9455-36e009082c3c', post_decoherence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c5ff507-f3c1-4ea7-9455-36e009082c3c', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, mainstream_quantum_practitioners).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls physics curriculum, journal editorial norms, and hiring criteria. Transmits the Copenhagen reading as the default interpretation through textbooks and graduate training. Enforces the boundary between operational physics and foundational inquiry via peer review and tenure evaluation. Change would require collective revision of pedagogical infrastructure.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, physics_academy, agenda_setter,
    institutional, generational, constrained, global).

% Employ the standard quantum formalism to calculate predictions without resolving the ontology of measurement. Benefit from shared conventions that close debate and allow resource concentration on calculational and experimental problems. Their professional identity is tied to operational proficiency; foundational questioning is treated as a hobby or departure.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, mainstream_quantum_practitioners, beneficiary,
    organized, biographical, constrained, global).

% Develop pilot-wave, many-worlds, or other interpretations. Face structural disadvantages in funding, hiring, and publication because their work is categorized as philosophical rather than physical. Bear the cost of reduced career mobility and smaller research communities despite empirical equivalence of predictions.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers, payer,
    moderate, biographical, constrained, global).

% Graduate students interested in the measurement problem or quantum ontology find these topics absent from core curriculum or actively discouraged by advisors. Their questions are redirected toward calculational technique, trapping them in operational identity unless they switch subfields or exit physics.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundational_inquiry_students, excluded,
    powerless, biographical, trapped, global).

% Analyzes the interpretive commitments and epistemic foreclosures of the physics community. Documents how the Copenhagen reading functions as a boundary mechanism and tracks the historical marginalization of alternatives. Does not participate in physics hiring or funding.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophy_of_physics_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, mainstream_quantum_practitioners).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified calculational framework that allows physicists to make predictions without resolving the quantum measurement problem; establishes shared rules for when and how to apply the Born rule and when to invoke collapse.
% TRANSFER_FUNCTION: Moves epistemic authority, research attention, and career viability from ontological questions about quantum states toward predictive calculation; transfers legitimacy from foundational researchers to operational practitioners.
% ABSENT_VOICES: Alternative interpretation researchers and foundational inquiry students are present in the discipline but structurally excluded from mainstream curriculum, funding panels, and tenure evaluation; their objections are pre-empted by framing the measurement problem as merely philosophical.
% DISAPPEARANCE_RATIONALE: If the Copenhagen reading vanished overnight, physics pedagogy would reorganize around explicit interpretation, foundational researchers would gain parity in hiring and funding, and the boundary between physics and philosophy of physics would shift; the shared closure mechanism that enables shut-up-and-calculate culture would dissolve.
% FOUNDING_PROBLEM: The quantum formalism produced correct predictions but the physical meaning of the wavefunction and the mechanism of measurement were unclear; the reading was built to provide an epistemic closure that allowed physics to proceed without awaiting a solution to the measurement problem.
% FOUNDING_PROBLEM_CORROBORATION: History of physics scholarship attests the post-war consolidation of the Copenhagen reading as a response to foundational paralysis. Alternative interpretation researchers attest the problem was never solved, only declared unphysical; philosophy of physics community documents the epistemic foreclosure from outside the benefiting parties.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).
:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.45) because the reading diverts epistemic authority and resources from foundational inquiry toward operational calculation without formal bans. Suppression is moderate (0.48) because alternatives are structurally disadvantaged in hiring and funding but not legally prohibited. Theater is moderate-high (0.38) because a significant share of pedagogical defense consists in ritualized dismissal of the measurement problem as unphysical rather than solving it. Accessibility collapse is moderate (0.55): alternatives exist but are not visible in standard training. Resistance is moderate (0.42) due to persistent foundational and philosophical challenge. The temporal series shows extraction and suppression peaking mid-century and modestly declining as alternative readings gain legitimacy, while theater remains elevated.
 *
 * PERSPECTIVAL GAP:
 *   The mainstream practitioner seat experiences the reading as enabling coordinationâa shared calculational framework that closes paralyzing debateâwhile the alternative researcher seat experiences it as an enforced epistemic boundary that extracts career viability. The student seat experiences it as an apparently absolute natural boundary because alternatives are absent from core curriculum. The academy sits between: it administers the constraint and benefits from institutional stability, but is itself constrained by generational inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream practitioners are beneficiaries with constrained exit, yielding low directionality (the reading subsidizes their operational practice). Alternative researchers are declared victims with constrained exit, yielding high directionality (the reading extracts from them through marginalization). Foundational inquiry students have the highest effective extraction because they are powerless and trapped in an identity-locked operational paradigm. The physics academy has moderate directionality as both administrator and partial beneficiary of institutional authority. The philosophy of physics community holds an analytical seat with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading was built to solve the paralysis of the early quantum revolutionâhow to proceed while the measurement problem remained unsolved. It provided genuine coordination then. The classification as tangled rope captures that the same structure now both enables calculation and marginalizes inquiry. If the founding problem were clearly dead, the reading would drift toward piton; because the measurement problem remains conceptually live and the coordination function still operates, the structure retains tangled rope character rather than pure inertial persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copenhagen_vs_siblings_foreclosure,
    'Does the Copenhagen reading''s claim of irreducible indeterminism and measurement primitiveness logically foreclose the pilot-wave and many-worlds readings, or do they merely coexist as rival factions within a shared formalism?',
    'Analysis of whether the interpretations share a common predictive formalism sufficient to permit agnostic operational practice, or whether ontological commitment to collapse renders the siblings logically impossible within a single framework.',
    'If foreclosed, the reading operates as an exclusive commitment system with higher extraction; if coexisting, the suppression is sociological preference rather than logical necessity, shifting classification toward rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copenhagen_vs_siblings_foreclosure, conceptual, 'Whether Copenhagen logically excludes sibling readings or merely rivals them.').

omega_variable(
    measurement_primitive_status,
    'Is measurement a primitive ontological boundary, or can it be derived from unitary dynamics and decoherence?',
    'Consensus on a physically closed derivation of measurement outcomes from SchrÃ¶dinger evolution alone, or experimental demonstration of objective collapse.',
    'If measurement is derivable, the Copenhagen reading''s foundational axiom is overridden and extraction collapses; if measurement must remain primitive, the reading is structurally vindicated as a genuine epistemic boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_primitive_status, empirical, 'Whether measurement is fundamental or derivable from unitary evolution.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problemâtheoretical paralysis in the face of the measurement problemâbeen solved by subsequent physics, or does the Copenhagen reading persist as a coordination device for a still-live conceptual emergency?',
    'Historical and sociological analysis of whether contemporary physics practice would be impaired without the Copenhagen closure mechanism, versus whether decoherence and quantum information have rendered the measurement problem tractable.',
    'If the founding problem is dead, the reading''s persistence indicates mandatrophy or piton drift; if live, it remains a tangled rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the Copenhagen reading''s founding problem is obsolete.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__copenhagen_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(quan_tr_t40, quantum_formalism__copenhagen_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__copenhagen_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(quan_tr_t80, quantum_formalism__copenhagen_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(quan_tr_t95, quantum_formalism__copenhagen_reading, theater_ratio, 95, 0.38).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__copenhagen_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(quan_be_t40, quantum_formalism__copenhagen_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__copenhagen_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(quan_be_t80, quantum_formalism__copenhagen_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement(quan_be_t95, quantum_formalism__copenhagen_reading, base_extractiveness, 95, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(quan_su_t20, quantum_formalism__copenhagen_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(quan_su_t40, quantum_formalism__copenhagen_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(quan_su_t60, quantum_formalism__copenhagen_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(quan_su_t80, quantum_formalism__copenhagen_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement(quan_su_t95, quantum_formalism__copenhagen_reading, suppression_requirement, 95, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel decomposes into three structurally distinct readings: copenhagen_reading (measurement primitive, collapse physical), many_worlds_reading (unitary evolution, branching), and pilot_wave_reading (deterministic hidden variables). Each reading has distinct epsilon, beneficiary/victim structure, and ontological commitments. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
