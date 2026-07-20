% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Competence Exercise Requirement (Aviation/HRO)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_dependency reading of the
 *   contested competence_exercise_requirement kernel in high-reliability
 *   organizations (principally commercial aviation). The kernel asks what
 *   kind of exercise sustains operational competence. This reading claims
 *   that simulation is necessary but insufficient, and that periodic
 *   real-world anchoring through line operations, non-jeopardy audits, and
 *   actual aircraft time is essential. The constraint is enforced by aviation
 *   regulators through recurrent training mandates and certificate
 *   requirements. It generates genuine coordination (safer operations)
 *   alongside asymmetric extraction (costs and time burdens concentrated on
 *   crews and airlines). It is claimed as tangled_rope: a genuine
 *   coordination function with active enforcement and asymmetric cost
 *   distribution. The sibling readings are simulation_as_adequate_exercise
 *   and catastrophe_as_necessary_anchor.
 *
 * KEY AGENTS:
 *   - aviation_regulators (agenda_setter, institutional/constrained): set and enforce the hybrid training minima
 *   - flight_crews (payer, moderate/constrained): bear direct time and cost burdens of recurrent training and currency maintenance
 *   - commercial_airlines (payer/beneficiary, powerful/constrained): fund infrastructure and downtime; benefit from insurability and certificate retention
 *   - flying_public (beneficiary, organized/analytical): diffuse beneficiary of accident prevention
 *   - simulation_training_providers (beneficiary, powerful/mobile): capture revenue from mandated simulator hours
 *   - safety_science_researchers (observer, analytical/analytical): provide empirical justification without operational stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.48).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.55).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.48).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Exercise Requirement (Aviation/HRO)").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, 'ecea5817-a383-4c9e-bedb-e8b5f41b67d4').
narrative_ontology:cs_kernel_codification('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', distributed).
narrative_ontology:cs_authority_grounding('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', expertise).
narrative_ontology:cs_interpretation_layer_present('ecea5817-a383-4c9e-bedb-e8b5f41b67d4').
narrative_ontology:cs_reading_relation('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', competence_exercise_requirement__simulation_as_adequate_exercise, coexists_with).
narrative_ontology:cs_reading_relation('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', competence_exercise_requirement__catastrophe_as_necessary_anchor, influences).
narrative_ontology:cs_axiom('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', foundational, simulation_insufficiency_for_operational_competence).
narrative_ontology:cs_axiom_status(simulation_insufficiency_for_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', simulation_insufficiency_for_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', foundational, ethical_imperative_against_catastrophe_dependence).
narrative_ontology:cs_axiom_status(ethical_imperative_against_catastrophe_dependence, holdable).
narrative_ontology:cs_axiom_grounding('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', ethical_imperative_against_catastrophe_dependence, deontological).
narrative_ontology:cs_reference_frame('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', hybrid_competence_maintenance).
narrative_ontology:cs_drift_state('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', contemporary_cost_pressure_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ecea5817-a383-4c9e-bedb-e8b5f41b67d4', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flying_public).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, simulation_training_providers).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, flight_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, commercial_airlines).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, commercial_airlines).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set minimum simulator and flight-time requirements for certificate holders. Audit compliance through check rides and record review. Bound by international standards and accident-driven political pressure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, aviation_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Spend required hours in simulators and maintain line-flying currency. Pay direct costs for type ratings and recurrent training when not fully employer-funded. Face loss of certification if requirements lapse.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flight_crews, payer,
    moderate, biographical, constrained, global).

% Pay for simulator construction, maintenance, and crew downtime. Receive insurability discounts and regulatory operating authority in return. Cannot exit the requirement without surrendering their air operator certificate.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, commercial_airlines, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, commercial_airlines, beneficiary).

% Fly with crews whose competence is regulated through mandated training. Bear no direct costs but rely on the safety outcomes.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flying_public, beneficiary,
    organized, civilizational, analytical, global).

% Build and operate flight simulators and training centers. Sell mandated recurrent training to airlines and individual crew members. Revenue depends on regulatory training minima.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulation_training_providers, beneficiary,
    powerful, biographical, mobile, global).

% Conduct studies on skill decay, simulator transfer effectiveness, and accident causation. Advise regulators and industry on training policy. Hold no operational stake in the requirement.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, safety_science_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, simulation_training_providers).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational competence in high-risk domains by ensuring practitioners retain both automated skills through repetition and contextual, adaptive judgment through real-world exposure, preventing catastrophic error from skill decay or startle.
% TRANSFER_FUNCTION: Moves time, labor, and capital from operators and operating organizations into recurrent training infrastructure, and moves safety assurance from regulatory authority to the traveling public.
% ABSENT_VOICES: Practitioners in resource-constrained settings such as small cargo operators and developing-nation airlines who cannot afford the full hybrid regime and would advocate for simulation-only pathways; also, insurers who might prefer either more stringent requirements or less.
% DISAPPEARANCE_RATIONALE: If the hybrid requirement vanished, organizations would immediately shift toward simulation-only currency to reduce costs; skill transfer gaps would widen; accident rates would likely rise after a lag; the training industry's revenue model would restructure around lower-fidelity, higher-volume offerings.
% FOUNDING_PROBLEM: High-risk operational domains where prolonged absence from real-world performance leads to measurable skill decay and catastrophic failure modes, yet real-world exercise is costly, dangerous, or ethically fraught.
% FOUNDING_PROBLEM_CORROBORATION: Safety science researchers and accident investigation boards such as the NTSB and BEA attest that competence decay contributes to accident chains; international regulatory harmonization through ICAO corroborates the persistent nature of the problem outside any single nation's regulatory interest.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the substantial, ongoing transfer of time and money from crews and airlines into training infrastructure, much of which is mandated rather than voluntarily chosen. Suppression (0.55) captures the degree to which alternative competence-maintenance models (simulation-only, catastrophe-only) are ruled out by regulatory force and professional norm. Theater ratio (0.28) is moderate: most activity is functional, but a growing share of recurrent training sessions serve checkbox compliance over genuine skill challenge. Accessibility collapse (0.70) is high because, within the aviation safety paradigm, arguing against any recurrent exercise requirement is structurally illegitimate. Resistance (0.25) is modest: individual crews and cost-pressured airlines grumble, but the HRO culture broadly accepts the mandate.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulators) experiences the constraint as a necessary safety architecture preventing catastrophic decay. The payer seats (crews, airlines) experience it as a costly, non-negotiable burden with limited exit. The beneficiary seat (flying_public) experiences it as an invisible background assurance. The simulation training providers experience it as a revenue stream. These divergences are structurally derived from the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (flying_public, simulation_training_providers) receive low directionality: the public is subsidized by the safety outcome, and providers are subsidized by mandated demand. Payers (flight_crews, commercial_airlines) receive high directionality: they bear the costs and have constrained exit. The dual-positioned airline seat sits in the middle, with net extraction still positive because compliance costs exceed insurability benefits at the margin.
 *
 * MANDATROPHY ANALYSIS:
 *   If the founding problem (skill decay in high-risk domains) were solved by perfect simulation or by autonomous systems, this constraint would face mandatrophy. Currently the founding problem is live: accident investigations continue to cite manual handling and startle as factors, and empirical studies show decay curves. The hybrid regime is therefore not yet a piton. However, cost pressures that erode the real-world component while keeping the formal shell could eventually produce a theater-heavy piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_boundary,
    'What is the empirical threshold at which high-fidelity simulation fully substitutes for real-world operational experience without measurable competence degradation?',
    'Longitudinal performance studies comparing crews with reduced real-world time against full-hybrid cohorts, using operational performance metrics and incident rates.',
    'If transferable, the real-world anchoring component may be extractive overhead rather than necessary coordination cost, shifting classification toward snare. If not transferable, the hybrid regime''s extraction is the necessary price of safety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_boundary, empirical, 'Empirical boundary of simulator substitution for real flight.').

omega_variable(
    kernel_reading_contest,
    'Does the hybrid regime represent an empirically optimal balance or a politically negotiated compromise between simulation-adequate and catastrophe-necessary readings?',
    'Comparative analysis of jurisdictions with differing hybrid ratios; correlation with accident rates and cost structures.',
    'If politically negotiated, the specific hybrid ratio may serve stakeholder interests such as training providers and regulators rather than pure safety optimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Nature of the hybrid reading within the competence kernel contest.').

omega_variable(
    cost_drift_erosion,
    'Is the real-world component of the hybrid regime structurally eroding under cost pressure, creating a growing gap between formal requirements and actual practice?',
    'Audit of actual versus mandated flight hours across operators; comparison with simulator hour trends over the last two decades.',
    'If eroding, the constraint operates more extractively in practice than on paper, as organizations purchase simulator hours while minimizing costly line operations and non-jeopardy audits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_drift_erosion, empirical, 'Gap between formal hybrid requirements and operational reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__hybrid_dependency, theater_ratio, 8, 0.13).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__hybrid_dependency, theater_ratio, 16, 0.17).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__hybrid_dependency, theater_ratio, 24, 0.21).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__hybrid_dependency, theater_ratio, 32, 0.24).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__hybrid_dependency, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_requirement kernel. The kernel decomposes into three structurally distinct claims about how operational competence is maintained: pure simulation, pure catastrophe exposure, or hybrid anchoring. Each reading carries a different epsilon and stakeholder geometry, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
