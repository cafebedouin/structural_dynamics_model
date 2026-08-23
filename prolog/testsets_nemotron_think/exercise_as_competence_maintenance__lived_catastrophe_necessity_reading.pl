% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Competence Maintenance Requires Real Catastrophe (Lived Catastrophe Necessity Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint story models the competence maintenance system in
 *   safety-critical domains (aviation, nuclear, healthcare, chemical) through
 *   the 'lived catastrophe necessity' reading: the claim that only actual
 *   catastrophic events genuinely exercise the judgment, emotional
 *   regulation, and improvisational competence that prevents disasters, while
 *   simulation — however high-fidelity — is merely rehearsal that cannot
 *   replicate the stakes-dependent cognitive and physiological states of real
 *   catastrophe. The system coordinates around simulation as a necessary
 *   baseline (every operator trains in simulators) but extracts
 *   asymmetrically: the covert decay of stakes-calibrated competence is borne
 *   by the exposed public, while regulators, training institutions, and
 *   simulation vendors benefit from the perpetual 'necessary but
 *   insufficient' framing that secures their authority, revenue, and
 *   insulation from accountability. The constraint requires active
 *   enforcement (regulatory mandates for simulation hours, fidelity
 *   standards, recurrency) and suppresses alternatives (distributed
 *   low-fidelity practice, structured near-miss learning, cross-domain crisis
 *   exposure) by defining them as insufficiently 'real.'
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.72).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Competence Maintenance Requires Real Catastrophe (Lived Catastrophe Necessity Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'f6430785-718e-4282-a369-90821bd7c6af').
narrative_ontology:cs_kernel_codification('f6430785-718e-4282-a369-90821bd7c6af', distributed).
narrative_ontology:cs_authority_grounding('f6430785-718e-4282-a369-90821bd7c6af', practice).
narrative_ontology:cs_interpretation_layer_present('f6430785-718e-4282-a369-90821bd7c6af').
narrative_ontology:cs_reading_relation('f6430785-718e-4282-a369-90821bd7c6af', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('f6430785-718e-4282-a369-90821bd7c6af', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('f6430785-718e-4282-a369-90821bd7c6af', foundational, only_real_catastrophe_exercises_judgment_kernel).
narrative_ontology:cs_axiom_status(only_real_catastrophe_exercises_judgment_kernel, holdable).
narrative_ontology:cs_axiom_grounding('f6430785-718e-4282-a369-90821bd7c6af', only_real_catastrophe_exercises_judgment_kernel, empirically_contingent).
narrative_ontology:cs_axiom('f6430785-718e-4282-a369-90821bd7c6af', secondary, simulation_necessary_insufficient_for_competence_maintenance).
narrative_ontology:cs_axiom_status(simulation_necessary_insufficient_for_competence_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('f6430785-718e-4282-a369-90821bd7c6af', simulation_necessary_insufficient_for_competence_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('f6430785-718e-4282-a369-90821bd7c6af', foundational, covert_competence_decay_without_stakes_activation).
narrative_ontology:cs_axiom_status(covert_competence_decay_without_stakes_activation, holdable).
narrative_ontology:cs_axiom_grounding('f6430785-718e-4282-a369-90821bd7c6af', covert_competence_decay_without_stakes_activation, empirically_contingent).
narrative_ontology:cs_reference_frame('f6430785-718e-4282-a369-90821bd7c6af', simulation_as_primary_competence_maintenance).
narrative_ontology:cs_drift_state('f6430785-718e-4282-a369-90821bd7c6af', post_high_fidelity_simulation_saturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f6430785-718e-4282-a369-90821bd7c6af', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_regulators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, training_institutions).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, high_fidelity_simulation_vendors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_public).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, patients_in_high_risk_healthcare).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, aviation_passengers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, communities_near_hazardous_facilities).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, competence_decay_without_real_stakes).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_fidelity_gap).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, covert_competence_atrophy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce training standards for safety-critical operators. They set the requirements for simulation fidelity, recurrency intervals, and what counts as 'real-stakes' experience. They benefit from the narrative that only real catastrophe validates competence because it justifies their authority to mandate expensive, stringent requirements and insulates them from blame when untested operators fail.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Provide certification, simulation, and training programs. They benefit from the 'simulation is necessary but insufficient' framing because it creates permanent demand for their services while insulating them from accountability — if competence decays without real catastrophe, no amount of training can be deemed fully adequate, securing their institutional relevance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, training_institutions, beneficiary,
    organized, biographical, mobile, regional).

% Sell advanced simulation platforms to regulated industries. They benefit from the 'necessary but insufficient' framing — it guarantees baseline demand (simulation is necessary) while creating an open-ended upgrade path (higher fidelity might approach 'real stakes'). Their exit is arbitrage-grade: they sell to multiple domains globally.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, high_fidelity_simulation_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Patients, passengers, and communities who bear the consequences when operators whose competence has only been exercised in simulation face real catastrophe. They have no choice in the operators they depend on, no visibility into the operators' actual competence state, and no exit from the systems (healthcare, aviation, industrial) that expose them.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_public, payer,
    powerless, immediate, trapped, local).

% Undergo high-risk procedures (surgery, anesthesia, emergency care) performed by clinicians whose crisis competence has never been tested in real catastrophe. They cannot choose their provider in emergencies, cannot assess the clinician's covert competence decay, and bear the full cost of the simulation-only training model.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, patients_in_high_risk_healthcare, payer,
    powerless, immediate, trapped, local).

% Rely on flight crews whose emergency handling competence has been exercised only in simulators. The rarity of actual in-flight catastrophes means most crews never face real stakes. Passengers have no alternative to air travel for many routes and no information about individual crew's real-event experience.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, aviation_passengers, payer,
    powerless, immediate, trapped, global).

% Live near nuclear plants, chemical facilities, refineries where operator competence in genuine emergencies has never been tested. They can organize politically (moderate power) but cannot relocate easily (constrained exit). They bear the latent risk of covert competence atrophy in facility control rooms.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, communities_near_hazardous_facilities, payer,
    moderate, generational, constrained, local).

% Provide lower-cost, lower-fidelity simulation tools. They are excluded from the 'necessary but insufficient' consensus because their products are deemed inadequate even for the rehearsal function. They would argue that distributed, frequent low-fidelity practice beats rare high-fidelity sessions, but the regulatory framework locks them out.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_vendors_low_fidelity, excluded,
    moderate, biographical, constrained, global).

% Study how competence is acquired, maintained, and decays in safety-critical domains. They observe the structural tension between simulation investment and real-event rarity, the covert decay dynamics, and the institutional incentives that shape training standards. They do not bear costs nor collect rents from the constraint.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organizational_learning_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for defining 'adequate' competence maintenance across safety-critical industries: simulation is the standardized, scalable, auditable baseline that all operators must complete, creating a common language and minimum bar for regulatory oversight.
% TRANSFER_FUNCTION: Moves the burden of covert competence decay from regulated organizations and training institutions onto the exposed public. Organizations pay for simulation (visible, auditable cost); the public pays for the gap between simulation and real-stakes competence (invisible, catastrophic cost) when untested operators face actual emergencies.
% ABSENT_VOICES: Front-line operators who have survived real catastrophes — their testimony about the simulation-reality gap is filtered through institutional debriefing processes that normalize the experience. Communities that have suffered actual disasters (Bhopal, Chernobyl, Deepwater Horizon) are heard only in post-hoc inquiries, not in the ongoing standard-setting that treats their catastrophe as an outlier rather than the necessary exercise.
% DISAPPEARANCE_RATIONALE: If the 'only real catastrophe exercises competence' constraint vanished overnight, regulators would have to explicitly define what simulation fidelity suffices, training institutions would lose the open-ended upgrade mandate, simulation vendors would face commoditization pressure, and the exposed public would gain a clearer (though possibly falsely reassuring) account of operator readiness. The entire competence maintenance economy would reorganize around a declared simulation sufficiency standard.
% FOUNDING_PROBLEM: After early industrial disasters (e.g., Texas City 1947, Flixborough 1974), it became clear that operators with only classroom training froze or erred catastrophically in real emergencies. Simulation was introduced as the scalable way to give operators 'near-real' experience without the cost and risk of actual catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (operators need emergency experience) is corroborated by accident investigation boards (CSB, NTSB, AAIB) across domains. The status is contested: regulators and training institutions attest the problem remains live (new failure modes emerge, simulation fidelity must rise); simulation vendors attest it is live (their business depends on it); researchers like Woods, Hollnagel, and Dekker attest the problem has shifted — the gap is now not 'no experience' but 'covert decay of judgment under stakes that simulation cannot replicate,' a different problem than the one simulation was built to solve.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the gap between simulation and real-stakes competence is systematic, covert, and borne entirely by those with no voice in the system. Suppression (0.72) is high because the regulatory framework actively defines what counts as valid competence exercise, excluding lower-cost, higher-frequency alternatives. Theater ratio (0.45) is moderate and rising: simulation ceremonies (recurrency checks, scripted scenarios) increasingly substitute for the judgment-under-stakes they claim to develop. Accessibility collapse (0.78) is high because once you accept the premise that only real catastrophe works, no alternative can be validated without a catastrophe — the constraint makes its own verification impossible. Resistance (0.55) is moderate: simulation vendors push for higher fidelity (which deepens the constraint), researchers document the gap (which the system absorbs as 'need for better simulation'), and occasional post-disaster reforms briefly puncture the narrative before it reasserts.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/training institution seat (agenda_setter/beneficiary), the constraint appears as a Rope: simulation coordinates a shared minimum standard across a fragmented industry, and the 'necessary but insufficient' framing is honest about limitations. From the exposed public seat (payer, trapped), it appears as a Snare: the coordination story is cover for a system that systematically under-prepares operators while certifying them as ready. From the simulation vendor seat (beneficiary, arbitrage), it appears as a Scaffold: simulation is a transitional technology on a path to ever-higher fidelity. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the coordination/extraction hybrid that all seats partially inhabit.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators and training institutions are structural beneficiaries (d near 0.0): they collect authority, funding, and insulation from the constraint's operation. High-fidelity simulation vendors are beneficiaries with arbitrage exit (d ~ 0.15): they profit but can pivot across domains. The exposed public (patients, passengers, fence-line communities) are full targets (d near 1.0): they bear the catastrophic cost of covert competence decay with zero exit, zero voice, and zero visibility. Front-line operators sit in a complex position: they are formally beneficiaries of training (d ~ 0.3) but informally targets of the system's false certification (they are the ones who actually face the catastrophe unprepared). The derivation chain captures this through power atoms (institutional/organized/powerless), exit options (analytical/mobile/arbitrage vs trapped/constrained), and the beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (operators need emergency experience) was real and live in the 1970s-80s. Simulation was a genuine coordination solution to an intractable problem: how to give operators crisis experience without catastrophes. But the problem has mutated. The current constraint maintains the original mandate ('simulation maintains competence') while the actual competence kernel (judgment under existential stakes) has decoupled from the simulation regime. The mandatrophy is not that the original problem is solved — it's that the solution has become a substitute for the problem it was meant to solve. The constraint persists because the institutions that administer it (regulators, training bodies) benefit from its persistence, and the victims (the public) cannot detect the decay until a catastrophe reveals it — at which point the system treats the catastrophe as validation ('see, the system works, we learn from accidents') rather than falsification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_construction,
    'Is the claim ''competence atrophies without real-stakes activation'' a genuine natural law of human cognition, or an institutional construction that benefits regulators and training institutions?',
    'Cross-domain comparison of competence decay curves in domains with frequent real events (wildland firefighting, combat medicine, disaster response) vs. domains with rare events (nuclear control room, commercial aviation, deep-sea drilling). If decay patterns are identical regardless of real-event frequency, the claim is constructed. If decay is measurably slower in high-real-event domains, the claim has natural-law grounding.',
    'If natural law, the constraint is a Mountain (or false summit Mountain with FSM triggering on beneficiaries). If constructed, it is a Tangled Rope or Snare. The classification determines whether the remedy is ''accept and mitigate'' (mountain) or ''restructure the competence maintenance system'' (rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_construction, empirical, 'Whether the core claim reflects cognitive reality or institutional interest.').

omega_variable(
    covert_decay_measurability,
    'Can covert competence decay (judgment under stakes) be measured independently of real catastrophe occurrence?',
    'Development of validated psychophysiological markers of stakes-calibrated decision quality that correlate with real-event performance, tested in longitudinal studies of operators who later face real catastrophes.',
    'If measurable, the constraint''s extraction becomes visible and auditable — regulators could mandate decay monitoring, shifting the constraint toward Rope. If inherently unmeasurable, the constraint''s extraction remains covert and the Tangled Rope/Snare classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covert_decay_measurability, empirical, 'Whether the extraction mechanism (covert decay) can be instrumented.').

omega_variable(
    kernel_reading_foreclosure_simulation_sufficiency,
    'Does this reading''s core premise (only real catastrophe exercises competence) logically foreclose the simulation_sufficiency_reading within any single competence maintenance framework?',
    'Formal analysis of the logical structure of both readings: if a framework adopts ''only real catastrophe exercises competence'' as axiomatic, can it simultaneously hold ''simulation fidelity determines retention effectiveness'' without contradiction?',
    'If forecloses, the two readings are mutually exclusive commitments — organizations must choose one framework. If coexists_with, both can operate in parallel (e.g., simulation for procedures, real-event acceptance for judgment), changing the network topology of the kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_simulation_sufficiency, conceptual, 'Logical relationship between this reading and the simulation_sufficiency_reading.').

omega_variable(
    kernel_reading_influence_hybrid_decay,
    'Does this reading''s emphasis on covert judgment decay create structural pressure on the hybrid_decay_reading to specify decay dynamics for the judgment component?',
    'Trace citations and standard-setting debates: does the hybrid_decay_reading''s procedural/judgment distinction evolve in response to pressure from the lived_catastrophe_necessity_reading''s ''covert decay'' claim?',
    'If influences, the kernel family has a directional influence structure (this reading → hybrid_decay_reading) that shapes how the contested kernel evolves. If coexists_with, the three readings form a stable triangular dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_influence_hybrid_decay, empirical, 'Directional influence from this reading to the hybrid_decay_reading.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of alternatives (low-fidelity distributed practice, near-miss learning, cross-domain crisis exposure) structural (regulatory prohibition) or internalized (operators and trainers believe only high-fidelity simulation ''counts'')?',
    'Survey operators and trainers in domains where regulatory minimums are met but optional alternatives exist: do they voluntarily adopt alternatives, or do they treat regulatory minimums as the ceiling of legitimacy?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the suppression travels with the agents even if regulations change. This would increase the constraint''s extractiveness classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism in competence maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exercise_competence_lived_catastrophe_tr_t1980, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_tr_t1990, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_tr_t2000, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_tr_t2010, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_tr_t2020, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_tr_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(exercise_competence_lived_catastrophe_be_t1980, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_be_t1990, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_be_t2000, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_be_t2010, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_be_t2020, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_be_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(exercise_competence_lived_catastrophe_su_t1980, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_su_t1990, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_su_t2000, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_su_t2010, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2010, 0.66).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_su_t2020, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(exercise_competence_lived_catastrophe_su_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.1).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'exercise_as_competence_maintenance' into three readings with divergent ε values. This reading (lived_catastrophe_necessity) has ε=0.68 (high extraction: covert decay borne by public). The simulation_sufficiency_reading has ε≈0.15 (low extraction: simulation genuinely retains competence). The hybrid_decay_reading has ε≈0.45 (moderate extraction: procedural retention via simulation, judgment decay without real stakes). The ε-invariance principle requires separate stories because the competence kernel's operationalization differs structurally across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutional, 0.1).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organized, 0.2).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, powerful, 0.15).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, powerless, 0.95).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, moderate, 0.85).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
