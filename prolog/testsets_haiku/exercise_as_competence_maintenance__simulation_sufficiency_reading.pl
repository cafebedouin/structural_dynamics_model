% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulation-Based Competence Maintenance (Sufficiency Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   A regulatory and organizational framework treats high-fidelity
 *   simulations as sufficient exercise of the competence kernel in disaster
 *   response — that is, sufficient to maintain professional judgment under
 *   the stresses of actual catastrophe. The sufficiency claim rests on the
 *   premise that if simulation fidelity is high enough (measuring
 *   environmental realism, time pressure, uncertainty, irreversible
 *   consequences), the neural and cognitive substrates activated in the
 *   simulator are isomorphic to those activated under real stakes, and thus
 *   competence exercised in simulation transfers completely to real-event
 *   performance. This reading is one of three in a contested kernel: the
 *   lived-catastrophe reading denies simulation can ever be the thing itself;
 *   the hybrid-decay reading allows simulation exercises procedure but not
 *   judgment; the simulation-sufficiency reading (this one) asserts fidelity
 *   determines effectiveness. The constraint instantiated by this reading is
 *   substantially extractive — it moves regulatory authority to simulator
 *   metrics, benefits vendors and management, and harms responders and served
 *   populations who experience judgment gaps under true stakes that the
 *   simulator did not prepare for.
 *
 * KEY AGENTS:
 *   - regulatory_authorities: institutional agenda-setter, set and enforce simulation mandates; analytical position allows them to declare what counts as competence via measurement protocol
 *   - simulation_vendors: organized beneficiary, supply systems and certification; revenue depends on mandate persistence and expansion
 *   - organizational_management: powerful beneficiary/payer, fund exercises; benefit from cost avoidance of real-stakes exposure while maintaining compliance
 *   - frontline_responders: moderate-power payers, participate in drills; experience gap between simulator and real-event judgment demands; objections reframed as requests for better training
 *   - vulnerable_populations_in_real_events: powerless victims, depend on responder competence; harmed when simulator-trained judgment fails under true stakes; entirely absent from exercise design
 *   - research_community: moderate-power observer; produces mixed evidence on training transfer; selectively cited depending on whether findings support or challenge sufficiency frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.45).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation-Based Competence Maintenance (Sufficiency Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '2b381db8-cc24-4de0-9e0d-a0b01162d4f9').
narrative_ontology:cs_kernel_codification('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', formalized).
narrative_ontology:cs_authority_grounding('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', extraction).
narrative_ontology:cs_interpretation_layer_present('2b381db8-cc24-4de0-9e0d-a0b01162d4f9').
narrative_ontology:cs_reading_relation('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', foundational, high_fidelity_simulation_transfers_judgment).
narrative_ontology:cs_axiom_status(high_fidelity_simulation_transfers_judgment, holdable).
narrative_ontology:cs_axiom_grounding('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', high_fidelity_simulation_transfers_judgment, empirically_contingent).
narrative_ontology:cs_axiom('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', secondary, competence_maintenance_compatible_with_zero_real_stakes_exposure).
narrative_ontology:cs_axiom_status(competence_maintenance_compatible_with_zero_real_stakes_exposure, holdable).
narrative_ontology:cs_axiom_grounding('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', competence_maintenance_compatible_with_zero_real_stakes_exposure, instrumental).
narrative_ontology:cs_reference_frame('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', fidelity_sufficient_for_transfer).
narrative_ontology:cs_drift_state('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', contemporary_post_pandemic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2b381db8-cc24-4de0-9e0d-a0b01162d4f9', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_authorities).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, organizational_management).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_responders).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, vulnerable_populations_in_real_events).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) and rising over the interval because the constraint transfers authority from real-event assessment to simulator metrics, benefits a concentrated set of beneficiaries (vendors, regulators, management), and harms a diffuse set of payers and an invisible set of victims. The suppression requirement (0.45, rising from 0.20) is the active work of maintaining the sufficiency frame against mounting real-world counterevidence: each responder failure under true stakes that contradicts simulator performance, each post-event investigation that reveals judgment gaps, requires suppression via reframing as 'fidelity insufficient' (driving more simulation investment) rather than 'simulation insufficient' (challenging the kernel reading itself). Theater ratio is high (0.58, rising from 0.25) because drill compliance becomes decoupled from authentic judgment testing — organizations perform exercises to satisfy metrics, not to discover gaps. The grid shows accessibility_collapse rising steeply at the structural level (0.40 to 0.80): the alternative of real-stakes training or mixed modalities becomes increasingly unavailable as regulatory mandates consolidate around simulation. Stakes_inflation also rises steeply (0.20 to 0.70 at structural level) because the cost of non-compliance grows (regulatory penalties, organizational liability) while the cost of compliance is buried in operational budgets. Resistance remains moderate (0.62-0.75 across levels) because frontline responders and research communities push back, but their voices are captured within the sufficiency frame (reframed as demands for better fidelity, not as challenges to the kernel reading itself). The measurements show a smooth extraction accumulation trajectory — the constraint does not oscillate; it ratchets.
 *
 * PERSPECTIVAL GAP:
 *   Regulatory authorities and management experience this constraint as coordination — a solution to a genuine problem (maintaining competence without waiting for disasters). Vendors experience it as a legitimate business opportunity. Frontline responders experience it as a suppressed critique: their post-exercise intuition that the simulator did not prepare them for true-stakes judgment is reframed as lack of fidelity, driving more simulator investment rather than acknowledging sufficiency limits. Vulnerable populations (entirely absent) would experience it as a transferred risk: they depend on responder competence exercised and tested only in simulators. The engine computes these divergent directionalities from the structural beneficiary/victim declarations — the authored metrics do not smooth this divergence into agreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities derive legitimacy from declaring what counts as competence; they benefit from a frame that makes competence measurable via simulator metrics (d toward beneficiary: ~0.20, moderate). Simulation vendors capture direct revenue from mandate expansion (d toward beneficiary: ~0.10, strong beneficiary). Organizational management benefits from cost avoidance (real-stakes exposure is expensive, dangerous, and reputationally risky) while maintaining compliance (d toward beneficiary: ~0.25, moderate). Frontline responders pay via time, attention, and the cognitive dissonance of knowing the simulator does not prepare them for true-stakes judgment (d toward target: ~0.75, moderate target). Vulnerable populations pay with their safety when responder judgment fails in ways the simulator did not prepare for (d toward target: ~0.95, strong target, but structurally absent from directionality computation). Research communities resist but are selectively cited (d toward observer: ~0.50, symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence decay in low-frequency domains) is live, but the sufficiency reading is contestable. The constraint's persistence depends on active suppression of the lived-catastrophe and hybrid-decay readings — not through explicit refutation, but through regulatory consolidation, vendor investment, organizational habit, and selective citation of research. The mandatrophy risk is HIGH: if a catastrophic real event exposes judgment gaps the simulation did not prepare for, and if post-event investigations attribute the failure to simulator insufficiency (rather than to inadequate fidelity or responder error), the reading collapses. The theatrical layer is the enforcement object: as long as exercises are performed and metrics are met, the reading persists; when performance metrics diverge sharply from real-event outcomes, suppression fails and the reading shifts or the mandate unravels. Current trajectory: extraction and theater are both rising, suggesting the constraint is hardening, not decaying — more vendor investment, more regulatory detail, more organizational compliance infrastructure. But resistance is also rising, particularly in research and responder communities, suggesting a fragile equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_transfer_gap,
    'Does neural and cognitive activation in high-fidelity simulation transfer completely to judgment under true stakes, or does the irreversibility of real consequence trigger decision-making processes the simulator cannot replicate?',
    'Neurocognitive studies comparing brain activation in simulators vs. real-event decision-making under similar pressures; post-event analyses comparing simulator-trained vs. mixed-modal trained responder performance in actual catastrophes; controlled judgment-under-pressure experiments with real vs. simulated consequences.',
    'If transfer is complete, the sufficiency reading holds and simulation is a genuine competence-maintenance mechanism. If transfer is partial, the sufficiency reading is falsified and the hybrid-decay or lived-catastrophe reading gains structural plausibility — competence may require real-stakes exposure or judgment-specific training beyond procedure drill.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_transfer_gap, empirical, 'Whether simulation fully transfers judgment competence to true-stakes performance.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is responder acceptance of simulator sufficiency structural (organizational mandate, career cost of dissent) or internalized (responders genuinely believe simulator is adequate)?',
    'Anonymous responder surveys about confidence in simulator preparation; post-event debriefs comparing pre-event beliefs (surveyed) with post-event assessments; exit interviews from responders who leave the field citing training inadequacy.',
    'If suppression is primarily structural, removing the mandate could shift the reading rapidly. If internalized, the reading has deeper institutional roots and would require explicit evidence of failure to dislodge. Internalized suppression is a sign the constraint is moving toward piton-territory (maintained by inertia and theater rather than by beneficiary/victim power asymmetry).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether responder acceptance of simulator sufficiency is structural or internalized.').

omega_variable(
    kernel_contest_reading_stability,
    'Is the simulation_sufficiency_reading stable as a kernel reading, or is it a transitional frame collapsing under mounting real-event evidence?',
    'Time-series analysis of academic consensus, regulatory language, vendor marketing, and responder community framing across the interval. Count how often the hybrid-decay or lived-catastrophe frames appear in post-event investigations, research publications, and policy debates. Track when (if) a major real-event failure attributed to simulator insufficiency occurs and whether it shifts the reading.',
    'If the reading is stable, this constraint story will remain valid for decades. If it is collapsing, this story documents a false summit in progress — a regulatory frame that looked like competence maintenance but was actually rent collection and risk transfer. The terminal state would be reading-shift to hybrid-decay or lived-catastrophe, or constraint dissolution and reordering of training modalities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_reading_stability, conceptual, 'Whether the simulation_sufficiency_reading is a stable kernel interpretation or a transitional frame.').

omega_variable(
    vulnerable_population_harm_visibility,
    'Are real-event harms to vulnerable populations visibly attributable to responder judgment gaps the simulator did not prepare for, or are they attributed to other causes (resource scarcity, statistical inevitability, responder error)?',
    'Post-event investigations with explicit attention to what responder training would have changed. Comparative analysis of real-event outcomes in jurisdictions with different training modalities (pure simulation vs. hybrid vs. high-stakes-mentored). Public attribution of specific deaths or injuries to training insufficiency vs. other causes.',
    'If harms are visibly attributed to simulator insufficiency, the reading collapses and vulnerable populations become a visible victim class, strengthening alternatives. If harms are attributed elsewhere or remain invisible (no investigation layer, no attribution), the reading persists and the constraint continues to extract via risk transfer to vulnerable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_harm_visibility, empirical, 'Whether real-event harms to vulnerable populations are visibly attributed to simulator insufficiency.').

omega_variable(
    kernel_committer_context_reading_distinctness,
    'Does this reading instantiate a genuinely distinct committal from its siblings, or does it collapse into the hybrid-decay reading under scrutiny (simulation + fidelity is procedure, and procedure is not the full kernel)?',
    'Axiomatic analysis: the simulation_sufficiency reading assumes fidelity fully transfers judgment; the hybrid-decay reading assumes procedure and judgment are separable; the lived-catastrophe reading assumes only irreversibility activates the kernel. Ask whether a defender of sufficiency can hold the position without conceding that judgment remains untested until real stakes. If they cannot, the reading is not distinct and collapses.',
    'If the reading is genuinely distinct, this story captures a stable kernel reading. If it collapses into hybrid-decay, the constraint dissolves and reorganizes around a two-component competence model. If it collapses into lived-catastrophe, simulation becomes acknowledged as rehearsal, not the thing itself, and the mandate unravels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_context_reading_distinctness, conceptual, 'Whether the simulation_sufficiency_reading is a genuinely distinct kernel interpretation or collapses into a sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t1995, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(exer_tr_t2003, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 2003, 0.35).
narrative_ontology:measurement(exer_tr_t2011, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 2011, 0.45).
narrative_ontology:measurement(exer_tr_t2018, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 2018, 0.53).
narrative_ontology:measurement(exer_tr_t2022, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 2022, 0.56).
narrative_ontology:measurement(exer_tr_t2026, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(exer_be_t1995, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(exer_be_t2003, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 2003, 0.45).
narrative_ontology:measurement(exer_be_t2011, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 2011, 0.54).
narrative_ontology:measurement(exer_be_t2018, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 2018, 0.59).
narrative_ontology:measurement(exer_be_t2022, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement(exer_be_t2026, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t1995, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(exer_su_t2003, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 2003, 0.3).
narrative_ontology:measurement(exer_su_t2011, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 2011, 0.38).
narrative_ontology:measurement(exer_su_t2018, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement(exer_su_t2022, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 2022, 0.44).
narrative_ontology:measurement(exer_su_t2026, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.22).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the exercise_as_competence_maintenance kernel. The kernel question is whether simulated catastrophe constitutes genuine exercise of the full competence kernel in disaster response. Three readings instantiate three structurally distinct constraints: simulation_sufficiency_reading (this file) asserts fidelity determines effectiveness; lived_catastrophe_necessity_reading asserts only real catastrophe exercises judgment; hybrid_decay_reading asserts simulation exercises procedure but not judgment. The three readings are not perspectives on the same constraint — they are separate constraints with different ε values, beneficiary/victim structures, and classifications. They are linked via the kernel: the sufficiency reading's persistence depends on suppressing the sibling readings' structural plausibility. The constraint family decomposes because the core contest is about what counts as valid competence exercise — different answers produce different victim sets, different regulatory regimes, and different revenue structures for vendors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
