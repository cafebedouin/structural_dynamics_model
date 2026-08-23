% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulation as Adequate Competence Exercise Requirement
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint story captures the reading that high-fidelity simulation
 *   with structured debriefing constitutes adequate exercise of the
 *   competence kernel — the regulatory position codified in ICAO Annex 1, FAA
 *   Part 121/135, and EASA FCL. The reading treats the simulator as a
 *   functional equivalent of the aircraft for the purposes of currency,
 *   recency, and type-rating proficiency. The claimed type is rope (pure
 *   coordination: a globally harmonized, auditable training standard). The
 *   authored metrics describe a constraint that has accumulated extraction
 *   (simulation industry revenue, regulatory convenience, airline cost
 *   avoidance) while suppressing the live-operations alternative through
 *   escalating fidelity requirements that only the simulation industry can
 *   meet. The engine will compute per-seat types from this structural data;
 *   the divergence between the claimed rope and the computed types across
 *   seats is the measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.48).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.62).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.48).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulation as Adequate Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '4328bc63-2594-4ceb-9e26-6b0f92a0be2d').
narrative_ontology:cs_kernel_codification('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', formalized).
narrative_ontology:cs_authority_grounding('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', extraction).
narrative_ontology:cs_interpretation_layer_present('4328bc63-2594-4ceb-9e26-6b0f92a0be2d').
narrative_ontology:cs_reading_relation('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_reading_relation('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', foundational, high_fidelity_simulation_suffices_for_competence).
narrative_ontology:cs_axiom_status(high_fidelity_simulation_suffices_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', high_fidelity_simulation_suffices_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', secondary, regulatory_compliance_demonstrates_competence).
narrative_ontology:cs_axiom_status(regulatory_compliance_demonstrates_competence, holdable).
narrative_ontology:cs_axiom_grounding('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', regulatory_compliance_demonstrates_competence, conventional).
narrative_ontology:cs_reference_frame('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', simulation_based_competence_framework).
narrative_ontology:cs_drift_state('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', contemporary_evidence_based_training_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4328bc63-2594-4ceb-9e26-6b0f92a0be2d', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, traveling_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, standardized_training_equivalence).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, competence_via_procedural_compliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce the regulatory standard that high-fidelity simulation hours count equivalently to line operations for currency and recency requirements. They benefit from auditable, predictable compliance metrics and avoid the political risk of mandating expensive live operations. Their authority rests on the claim that simulation fidelity has reached parity with reality for competence maintenance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_regulators, agenda_setter,
    institutional, generational, analytical, global).

% Manufacturers of full-flight simulators, training device software, and synthetic environment providers. They capture the revenue stream from mandated recurrent training cycles (typically 2-4 sessions per crew per year). Their business model depends on the regulatory equivalence holding; they fund fidelity research that supports the reading.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Carriers pay for simulator slots and instructor time but avoid the vastly higher cost and scheduling disruption of line-check flights, revenue ferry legs, or non-jeopardy audit sectors. They gain predictable budgeting and crew availability. They also bear the residual risk if simulation proves inadequate — but that risk is diffuse, delayed, and externalized to crews and passengers.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management, payer).

% Pilots and cabin crew whose license currency depends on simulator events. They invest career-critical time in scenarios that may not transfer to the startle, physiological stress, and consequential decision-making of real operations. Exit means leaving the profession; identity is fused to the license the simulator validates. They are told the sim is equivalent; dissent is treated as resistance to modernization.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews, payer,
    organized, biographical, identity_locked, global).

% Passengers who bear the ultimate consequence if simulator-trained crews encounter a scenario outside the validated envelope. They have no voice in the standard-setting process, no visibility into the fidelity gap, and no alternative transport mode for long-distance travel. Their exclusion is structural — the constraint's legitimacy rests on their absence from the room.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, traveling_public, excluded,
    powerless, immediate, trapped, global).

% Academic and industry scientists studying transfer of training, startle response, and simulator fidelity metrics. They produce the evidence base that regulators cite — but their findings on fidelity gaps are selectively absorbed. They see the full structure but cannot compel regulatory revision.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, human_factors_researchers, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, auditable, globally harmonized method for maintaining crew currency without requiring each operator to run live line checks, ferry flights, or non-jeopardy sectors — solves the coordination problem of 'how do we verify competence at scale across 100+ airlines and 50+ regulators?'
% TRANSFER_FUNCTION: Moves the cost and risk of competence exercise from live operations (fuel, scheduling, weather, aircraft wear, passenger exposure) into simulator sessions purchased from the simulation industry; moves regulatory legitimacy from outcome-based oversight to process-based compliance.
% ABSENT_VOICES: The traveling public — who would demand evidence that simulator equivalence holds for the tail events that kill — and line-check airmen who have seen the fidelity gap in practice but are not consulted when the equivalence standard is renewed.
% DISAPPEARANCE_RATIONALE: If the simulation-equivalence rule vanished overnight, airlines would need to reconstruct line-check programs, regulators would need new oversight methods, crews would need actual aircraft time for currency, and the simulation industry would lose its regulatory moat. The global aviation training architecture would reorganize around live operations and hybrid models.
% FOUNDING_PROBLEM: In the 1970s–1990s, the explosion of jet transport created a competence verification crisis: too many crews, too few check airmen, too much schedule pressure to conduct meaningful line checks. Simulation offered a controlled, repeatable, weather-independent alternative that could be standardized globally.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and the simulation industry attest the problem remains live (fleet growth, new types, pilot shortage). Independent safety boards (NTSB, AAIB, BEA) and human-factors meta-analyses attest the original verification crisis has been solved — but the solution has fossilized into a substitute for the thing it was meant to verify. No corroborating source outside the beneficiary set affirms that simulation alone is adequate for the full competence kernel.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.22 to 0.48 over 40 years as simulator hours replace line operations one-for-one without fidelity validation for low-probability high-consequence events. Suppression rises from 0.35 to 0.62 as regulators add fidelity qualifications (Level D, UPRT, EBT) that raise the barrier for any non-simulator alternative. Theater ratio rises from 0.18 to 0.42: early sims were crude but honestly marketed as supplements; modern EBT programs perform competence through scripted scenarios while the fidelity gap for startle, physiological arousal, and consequence management widens. Accessibility collapse at 0.58 reflects that alternative competence pathways (line checks, operational experience) exist legally but are economically and regulatorily inaccessible. Resistance at 0.44 captures crew pushback (ASAP reports, union positions) that is channeled into fidelity demands rather than structural challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/sim-industry seat, the constraint is a rope: it coordinates global training, solves the check-airman shortage, and produces auditable records. From the crew seat, it is a snare: the equivalence claim is the cover; the reality is extraction of time and risk-bearing without the physiological and consequential fidelity that maintains the kernel. From the public seat, it is a snare with no voice. The engine computes this divergence; the authored claim (rope) records the reading's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators sit at d ≈ 0.1 (beneficiary: they get auditable compliance without outcome risk). Simulation industry at d ≈ 0.05 (pure beneficiary: revenue scales with mandate). Airline management at d ≈ 0.4 (dual: they pay but avoid higher live-op costs). Flight crews at d ≈ 0.85 (target: identity-locked into the license the sim validates, bear the competence risk). Traveling public at d ≈ 0.95 (trapped target: bears residual risk with zero exit). Researchers at d ≈ 0.5 (analytical: symmetric observer). The derivation chain reads these positions from beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verification crisis of the 1970s–90s) is contested: the verification capacity gap has been solved by technology and staffing, but the simulation mandate persists and has expanded. The arrangement now extracts from crews and the public while the coordination function (standardized verification) could be achieved with a hybrid model. Mandatrophy is unresolved — the constraint's mandate has outlived its founding function but the regulatory/sim-industry coalition maintains it. The founding_problem_status = contested and disappearance_verdict = world_rearranges flags this as a zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the simulation-as-adequate reading a coherent commitment-system constraint distinct from its siblings, or a strategic framing that collapses under evidence?',
    'Track regulatory rulemaking dockets: if the simulation-equivalence standard is revised to require live-anchoring events (as EASA EBT and FAA AQP are beginning to do), the reading''s structural coherence fractures. The kernel itself persists; this reading''s authority_grounding shifts from extraction toward lineage or practice.',
    'If the reading is a strategic framing, its claimed_type (rope) is a cover for tangled_rope extraction; if it is a coherent commitment-system constraint, the divergence between claim and computed type measures the kernel''s drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether this reading instantiates a distinct constraint or is a defensible framing of the kernel.').

omega_variable(
    transfer_of_training_evidence_gap,
    'Does the empirical evidence on simulator-to-aircraft transfer support the equivalence claim for the full competence kernel (including startle, stress, consequence), or only for procedural skills?',
    'Meta-analysis of LOFT/LOSA/line-check outcome data correlated with simulator fidelity levels and scenario types. Natural experiments from operators who voluntarily exceed simulator minima with live line checks.',
    'If transfer evidence supports only procedural skills, the equivalence claim is empirically_contingent and overridden — the constraint''s extraction (risk transfer to crews/public) is not justified by coordination benefit. If evidence supports full-kernel equivalence, the reading''s axioms are holdable and the constraint is a genuine rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transfer_of_training_evidence_gap, empirical, 'Whether the fidelity-equivalence claim survives empirical scrutiny for the full competence kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of live-operations alternatives structural (regulatory mandate, economic lock-in) or internalized (crews and managers believe simulation is genuinely equivalent)?',
    'Post-exit suppression trajectory: survey crews who have transitioned to operators with live-check programs — if perceived competence gap persists after regulatory mandate is removed, suppression has an internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the belief in equivalence becomes a self-reinforcing barrier to alternative competence pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the live-operations alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cer_sae_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cer_sae_tr_t8, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 8, 0.24).
narrative_ontology:measurement(cer_sae_tr_t16, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 16, 0.31).
narrative_ontology:measurement(cer_sae_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cer_sae_tr_t32, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 32, 0.4).
narrative_ontology:measurement(cer_sae_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cer_sae_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cer_sae_be_t8, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(cer_sae_be_t16, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(cer_sae_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(cer_sae_be_t32, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(cer_sae_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cer_sae_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cer_sae_su_t8, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(cer_sae_su_t16, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(cer_sae_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(cer_sae_su_t32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(cer_sae_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__simulation_as_adequate_exercise, 0.08).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the competence_exercise_requirement kernel into three readings with different ε values. simulation_as_adequate_exercise claims low ε (rope) but metrics show moderate extraction. catastrophe_as_necessary_anchor would show high ε (snare) if instantiated as a mandate. hybrid_dependency would show moderate ε with coordination (tangled_rope). The upstream kernel (competence maintenance) is Mountain-like; the readings are the contested operationalizations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__simulation_as_adequate_exercise, institutional, 0.1).
constraint_indexing:directionality_override(competence_exercise_requirement__simulation_as_adequate_exercise, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
