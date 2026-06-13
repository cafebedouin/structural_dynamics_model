% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Lived Catastrophe as Competence Kernel: Simulation Insufficient
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint instantiates the lived-catastrophe-necessity reading of
 *   the contested kernel 'exercise_as_competence_maintenance'. The reading
 *   asserts: only actual catastrophe exercises the full competence kernel;
 *   simulation is rehearsal but not the thing itself; competence atrophies
 *   without real-stakes activation. Under this reading, the constraint is
 *   tangled rope — it solves a genuine coordination problem (how to staff
 *   continuous critical infrastructure without requiring catastrophe-tested
 *   operators) while extracting a hidden cost (distributing undetected
 *   competence gaps to exposed populations). The beneficiary institutions are
 *   the safety authorities and certification bodies that avoid the
 *   operational and legal burden of requiring catastrophe exposure. The
 *   victims are exposed populations and first responders whose competence may
 *   not have been tested under the actual pressures that catastrophe imposes.
 *   The theater ratio rises over the interval because the elaborate
 *   simulation apparatus becomes increasingly performative as incident
 *   investigations repeatedly reveal competence gaps that simulation did not
 *   prevent.
 *
 * KEY AGENTS:
 *   - Safety authority institutions: set and maintain the certification standards; benefit from avoiding the requirement that competence be catastrophe-tested
 *   - Competence certification bodies: profit from and maintain professional authority through certification-by-simulation
 *   - Exposed populations (passengers, patients, residents): depend on operators; have no exit and no ability to assess whether simulation-certified competence will hold under catastrophe
 *   - Organizations deploying personnel (airlines, hospitals, military, emergency services): bear the liability risk while benefiting from the low cost of simulation-based readiness
 *   - First responders without catastrophe exposure: certified through simulation but face catastrophe without having rehearsed in the actual pressures of real stakes
 *   - Post-incident investigators: discover competence gaps after catastrophe has already occurred; their voice does not feed into pre-incident certification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.72).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived Catastrophe as Competence Kernel: Simulation Insufficient").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '41070b2e-049e-46ad-9e33-1e739b0a3aca').
narrative_ontology:cs_kernel_codification('41070b2e-049e-46ad-9e33-1e739b0a3aca', distributed).
narrative_ontology:cs_authority_grounding('41070b2e-049e-46ad-9e33-1e739b0a3aca', extraction).
narrative_ontology:cs_interpretation_layer_present('41070b2e-049e-46ad-9e33-1e739b0a3aca').
narrative_ontology:cs_reading_relation('41070b2e-049e-46ad-9e33-1e739b0a3aca', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('41070b2e-049e-46ad-9e33-1e739b0a3aca', exercise_as_competence_maintenance__hybrid_decay_reading, coexists_with).
narrative_ontology:cs_axiom('41070b2e-049e-46ad-9e33-1e739b0a3aca', foundational, real_stakes_irreducibly_distinct_from_simulation).
narrative_ontology:cs_axiom_status(real_stakes_irreducibly_distinct_from_simulation, holdable).
narrative_ontology:cs_axiom_grounding('41070b2e-049e-46ad-9e33-1e739b0a3aca', real_stakes_irreducibly_distinct_from_simulation, empirically_contingent).
narrative_ontology:cs_axiom('41070b2e-049e-46ad-9e33-1e739b0a3aca', foundational, competence_decay_covert_without_catastrophe_activation).
narrative_ontology:cs_axiom_status(competence_decay_covert_without_catastrophe_activation, holdable).
narrative_ontology:cs_axiom_grounding('41070b2e-049e-46ad-9e33-1e739b0a3aca', competence_decay_covert_without_catastrophe_activation, empirically_contingent).
narrative_ontology:cs_reference_frame('41070b2e-049e-46ad-9e33-1e739b0a3aca', genuine_competence_testing_framework).
narrative_ontology:cs_drift_state('41070b2e-049e-46ad-9e33-1e739b0a3aca', contemporary_certification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('41070b2e-049e-46ad-9e33-1e739b0a3aca', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_authority_institutions).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, competence_certification_bodies).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_populations_trained_only_by_simulation).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organizations_dependent_on_simulated_readiness).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, first_responders_without_catastrophe_exposure).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and rising because the constraint extracts a growing organizational benefit (staffing continuity, liability insulation, low certification cost) from exposed populations and first responders who bear the actual competence-gap risk. Suppression is also high (0.72) because the constraint's persistence requires actively suppressing the alternative framing — that catastrophe is the only genuine test — which would delegitimize the entire simulation-based certification apparatus. Theater ratio is notably high (0.58) and rising because the constraint increasingly operates as performative: elaborate simulation exercises produce certification with declining empirical connection to actual catastrophe outcomes. Post-incident investigations regularly document that simulation did not prevent the actual failure; yet certification continues unchanged. The theater ratio's rise reflects this growing gap between the claimed function (competence certification) and actual function (organizational liability management and resource control). Measurements are authored on a single shared time grid: every metric is examined at every time point (t=0,8,16,24,32,40), enabling lifecycle detection of when simulation-based certification transitioned from a genuine coordination mechanism to a primarily extractive theater.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (safety authority) perceives the constraint as coordination with appropriate liability allocation: 'We have set standards, operators meet them through rigorous simulation, organizations deploy personnel at their own risk.' The exposed population and first responder seats perceive the constraint as unilateral risk transfer: 'We are certified through something that was never tested against the actual pressures we face, and we discovered this only after catastrophe.' The engine computes this perspectival divergence from directionality: beneficiary seats near d=0.0 and target seats near d=0.9+ produce sharply different type classifications from the same structural data. This divergence is the core measurement the corpus takes: the claim-metric gap reveals that beneficiary perception of 'coordination' rides on target seats' bearing an uncompensated competence-gap risk.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety authorities and certification bodies sit at d near 0.1–0.2 (full beneficiary): they set the rules, control the certification, avoid the operational cost of requiring catastrophe testing, and face no exit. Exposed populations sit at d near 0.95 (full target): they are trapped into dependence on simulation-certified operators, have zero ability to assess or exit the arrangement, and bear the catastrophe-risk transfer. Organizations deploying personnel sit at d near 0.65 (moderate target): they benefit from low certification cost but bear some liability risk and face constrained ability to demand higher standards without reorganizing their entire operation. First responders sit at d near 0.75 (target with identity lock): certified through simulation, professionally obligated to perform under real stakes, trapped by their role identity and career dependence even if they sense the simulation did not prepare them. The structural asymmetry is pronounced because the beneficiary seats control the certification mechanism and the target seats have no equivalent power to set counter-standards.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to maintain continuous critical infrastructure operation without requiring each operator to have survived a catastrophe — was a genuine coordination problem at the kernel's origin. The lived-catastrophe-necessity reading identifies mandatrophy: the founding problem has become partially obsolete as simulation technology matured, but the constraint persists because the beneficiary institutions have captured the apparatus of certification and now extract benefit (low cost, liability insulation, professional authority) from maintaining the simulation-sufficiency claim. The measured mandatrophy does not appear as complete function atrophy — simulation does provide *some* competence maintenance — but rather as the constraint's primary function shifting from 'exercise competence' to 'manage liability and control deployment costs.' The rising theater ratio directly tracks this shift: the constraint begins as coordinator (t=0, theater_ratio=0.38) and drifts toward performative theater (t=40, theater_ratio=0.58) as the elaborate simulation exercises become increasingly disconnected from actual catastrophe outcomes. This trajectory is the signature of mandatrophy: the original function persists formally while the actual operative function becomes increasingly about resource control and risk allocation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency_boundary,
    'Is there a threshold of simulation fidelity beyond which simulation-trained competence is empirically indistinguishable from catastrophe-tested competence under actual catastrophic conditions?',
    'Prospective study comparing performance of simulation-only-trained vs. catastrophe-exposed operators under controlled high-stakes conditions (e.g., critical incident simulation with real-time stress measurement, decision accuracy, reflex timing). Or analysis of incident data isolating operator-competence-gap variables from other failure modes.',
    'If threshold exists and is reachable, simulation-sufficiency reading gains empirical ground and the lived-catastrophe-necessity reading becomes overstated. If no threshold exists, the reading holds that competence gaps remain covert until catastrophe and certification-by-simulation is structurally insufficient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency_boundary, empirical, 'Whether simulation fidelity can actually substitute for real-stakes competence testing.').

omega_variable(
    covert_competence_decay_mechanism,
    'What is the time course and detection mechanism for competence decay in simulation-trained operators who have not been activated under real catastrophic stakes?',
    'Longitudinal competence assessment studies of operators at different points in their careers; incident investigation data isolating operator-judgment failures that emerge for the first time in catastrophes; physiological and cognitive studies of how expertise is maintained or degraded under stress deprivation.',
    'If decay is slow and undetectable until catastrophe, the constraint is highly extractive because competence gaps are institutional secrets. If decay is rapid and detectable through alternative assessment, certification bodies could implement continuous catastrophe-equivalent challenge exercises to maintain competence without requiring actual catastrophe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_competence_decay_mechanism, empirical, 'Whether competence decay in simulation-trained operators is detectable before catastrophe or remains hidden until real-stakes activation.').

omega_variable(
    reading_boundary_procedural_vs_judgment,
    'Are procedural competence and judgment-under-stakes structurally separable as two components of the competence kernel, or is judgment inherently unexercisable except under real stakes?',
    'The hybrid-decay reading asserts they are separable (simulation exercises procedure, catastrophe exercises judgment). The lived-catastrophe-necessity reading implicitly asserts inseparability (catastrophe exercises the whole kernel at once). Research on stress-induced decision degradation and the role of real consequences in expert judgment would discriminate these.',
    'If separable, hybrid-decay reading gains ground; a two-component approach could require catastrophe-level judgment exercises (high-consequence scenario training) without requiring actual catastrophe. If inseparable, lived-catastrophe-necessity reading holds and no simulation can fully exercise judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_procedural_vs_judgment, conceptual, 'Whether judgment-under-stakes is a separable component of the competence kernel or inseparable from real-stakes activation.').

omega_variable(
    suppression_of_catastrophe_necessity_claim,
    'To what extent does the institutional persistence of simulation-based certification depend on active suppression of the alternative claim that only catastrophe exercises competence?',
    'Content analysis of professional certifying bodies'' treatment of post-incident investigation findings that reveal simulation gaps; budget allocation patterns (spending on simulation vs. spending on catastrophe-readiness preparation); hiring and promotion patterns in organizations where incident investigators advocate for catastrophe-competence requirements.',
    'If suppression is substantial, the constraint is actively maintained against contradicting evidence, indicating high extractiveness and low accessibility of the catastrophe-necessity alternative. If suppression is minimal, the simulation-sufficiency claim is holding against challenge, indicating the alternative is less empirically compelling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_catastrophe_necessity_claim, empirical, 'How much institutional effort is devoted to suppressing the claim that only catastrophe exercises true competence.').

omega_variable(
    victim_set_specification_ambiguity,
    'Who exactly bears the competence-gap risk: only exposed populations in incidents where simulation-trained operators fail, or also organizations that must absorb the liability cost, or also first responders who discover inadequate preparation under real stakes?',
    'Analysis of liability allocation, incident investigation findings, and organizational cost distribution post-catastrophe. Who actually pays for the failure — the population, the organization, the operator, the certifier?',
    'Clarifies whether the constraint is primarily extractive for beneficiary institutions or whether the cost is distributed more broadly. Affects classification of beneficiary-vs-victim alignment and whether the constraint benefits a narrow institutional set at broad population cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_specification_ambiguity, empirical, 'Precise identification of which parties bear the competence-gap risk and liability cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 24, 0.54).
narrative_ontology:measurement(exer_tr_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 32, 0.57).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(exer_be_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(exer_su_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'exercise_as_competence_maintenance'. The lived-catastrophe-necessity reading asserts that only actual catastrophe exercises the competence kernel. Sibling readings (simulation_sufficiency_reading and hybrid_decay_reading) advance alternative decompositions of the same kernel commitment. All three stories share the same domain (safety/preparedness) but differ in their ε values and stakeholder structures: simulation-sufficiency reading has lower ε (simulation training is sufficient), hybrid-decay reading has moderate ε (two components, different exercise requirements), lived-catastrophe-necessity reading has high ε (simulation cannot substitute for catastrophe). The three stories are linked through network.affects_constraints: each reading influences the others' operating environment by either reinforcing or challenging the simulation-based certification framework. Decomposition is necessary because the three readings instantiate structurally distinct constraints with different victim sets and beneficiary profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
