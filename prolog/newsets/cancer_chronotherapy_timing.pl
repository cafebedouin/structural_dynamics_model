% ============================================================================
% CONSTRAINT STORY: cancer_chronotherapy_timing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cancer_chronotherapy_timing, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cancer_chronotherapy_timing
 *   human_readable: The Circadian Lifeline
 *   domain: health/technological
 *
 * SUMMARY:
 *   Cancer chronotherapy leverages a biological reality: the body's circadian
 *   rhythms modulate the immune system, causing T-cells to congregate around
 *   tumors more effectively at certain times of day (e.g., before 3 pm). This
 *   constraint story models the human healthcare system's interaction with
 *   this biological fact. While the underlying biology is a Mountain, the
 *   implementation creates a complex system of coordination and extraction.
 *   The failure to align clinical practice with this knowledge leads to
 *   measurably worse patient outcomes, turning institutional inertia into a
 *   mechanism of extraction.
 *
 * KEY AGENTS:
 *   - Uninformed/Disempowered Patients: Primary victims (powerless/trapped) — bear the full cost of suboptimal timing through reduced treatment efficacy.
 *   - Informed Patients: Primary beneficiaries (moderate/mobile) — use the knowledge as a coordination tool to improve their health outcomes.
 *   - Healthcare Providers/Hospitals: Institutional actors (organized/trapped or institutional/arbitrage) — face a conflict between clinical best practices and logistical/financial constraints.
 *   - Research Scientists: Analytical observers (analytical/analytical) — view the underlying biological rhythm as a natural law (Mountain).
 *   - Insurers: Institutional actors (institutional/arbitrage) — may see it as a cost-saving measure to be temporarily incentivized (Scaffold).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cancer_chronotherapy_timing, 0.55).
domain_priors:suppression_score(cancer_chronotherapy_timing, 0.65).
domain_priors:theater_ratio(cancer_chronotherapy_timing, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, extractiveness, 0.55).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cancer_chronotherapy_timing, tangled_rope).
narrative_ontology:human_readable(cancer_chronotherapy_timing, "The Circadian Lifeline").
narrative_ontology:topic_domain(cancer_chronotherapy_timing, "health/technological").

domain_priors:requires_active_enforcement(cancer_chronotherapy_timing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, informed_patients).
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, adopting_clinics).
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, pharmaceutical_companies).
narrative_ontology:constraint_victim(cancer_chronotherapy_timing, uninformed_patients).
narrative_ontology:constraint_victim(cancer_chronotherapy_timing, rigid_healthcare_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINFORMED PATIENT (SNARE) — Trapped by the scheduling logic of their provider, they receive less effective treatment without knowing an alternative exists. The system extracts potential months or years of life due to institutional inertia. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFORMED PATIENT (ROPE) — Aware of the research, they can advocate for a morning appointment or switch providers. For them, the biological fact is a pure coordination tool to improve their own outcome. As a beneficiary with exit options, d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.005.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the full structure: a genuine coordination function (timing improves efficacy) combined with severe asymmetric extraction (uninformed patients suffer). The high suppression from rigid scheduling and high extraction from lost life-years define it as a hybrid system. This is the system's claimed type.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: RESEARCH SCIENTIST (MOUNTAIN) — Focuses on the underlying biological mechanism. The circadian rhythm of T-cell activity is a fixed, unchangeable feature of human physiology, a 'law of nature'. The engine will flag this as a false summit, as the base properties (ε=0.55) reflect the human system's interaction with the law, not the law itself.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: OVERBURDENED HOSPITAL (PITON) — Lacking the resources to reschedule all oncology patients to morning slots, the 'best practice' of chronotherapy becomes an inert, aspirational goal. It is discussed in meetings but not implemented, making it a high-theater, low-functionality constraint. theater_ratio=0.75 satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: INSURANCE PROVIDER (SCAFFOLD) — May implement a temporary incentive program for clinics to adopt chronotherapy, aiming to reduce long-term costs from less effective treatments. This program is a temporary support with a sunset clause: once chronotherapy is standard practice, the incentives are withdrawn.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cancer_chronotherapy_timing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cancer_chronotherapy_timing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cancer_chronotherapy_timing, TR),
    TR >= 0.70.

:- end_tests(cancer_chronotherapy_timing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): High. This value represents the significant, quantifiable cost of ignoring the constraint, measured in reduced progression-free survival and overall survival. It is a direct extraction of potential life-years from patients scheduled suboptimally. Suppression (0.65): High. Patients face significant institutional friction in trying to alter prescribed treatment times, which are often dictated by rigid scheduling systems, staff availability, and resource allocation. Theater Ratio (0.75): High. As awareness of chronotherapy grows faster than its implementation, it becomes a topic of performative discussion in under-resourced institutions. They acknowledge the 'best practice' without having the capacity to enact it, turning the principle into institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. A research scientist sees an elegant, unchangeable Mountain of biology. An informed patient sees a simple, life-saving Rope. An uninformed patient is caught in a deadly Snare created by systemic inertia. An overburdened hospital sees an aspirational but inert Piton. This divergence highlights a core DR principle: a single set of facts can generate the full spectrum of constraint types depending on the observer's power, knowledge, and exit options relative to the system built upon those facts.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by access to information and institutional flexibility. Beneficiaries are those who can act on the knowledge (informed patients, agile clinics). Victims are those trapped by systems that have not adapted to the knowledge (uninformed patients, rigid hospitals). The system extracts health outcomes from the latter to subsidize the convenience and operational status quo of the institution.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that a 'natural law' (the Mountain of biology) is not the same as the human system that interacts with it. Labeling the situation a 'Mountain' (i.e., 'it's just a tragic, unavoidable reality') would be a misclassification that masks the severe, contingent extraction occurring within the healthcare system (the Snare/Tangled Rope). The framework correctly identifies that the object of analysis is the *implementation*, and its high ε and suppression values reveal its true, non-Mountain nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_chronotype_variance,
    'Is the optimal treatment window universal, or does it vary significantly based on an individual''s chronotype (e.g., ''morning larks'' vs. ''night owls'')?',
    'Personalized monitoring of patient circadian markers (e.g., cortisol, body temperature) correlated with treatment outcomes.',
    'If universal, the constraint is a simple Rope/Snare. If individual, it becomes a far more complex Tangled Rope requiring expensive diagnostics, potentially increasing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_chronotype_variance, empirical, 'Whether the optimal treatment window is universal or requires personalization.').

omega_variable(
    systemic_reorganization_cost,
    'Is the aggregate cost of reorganizing national oncology scheduling greater than the aggregate benefit in life-years gained?',
    'Large-scale health economic modeling comparing implementation costs (staffing, facilities) against long-term treatment cost savings and quality-adjusted life years (QALYs).',
    'If benefits outweigh costs, the current system is a Snare (needless extraction). If costs outweigh benefits, the system is a tragic Mountain (an unavoidable, costly trade-off).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_reorganization_cost, empirical, 'Cost-benefit analysis of systemic scheduling reorganization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_chronotherapy_timing, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(canc_tr_t0, cancer_chronotherapy_timing, theater_ratio, 0, 0.15).
narrative_ontology:measurement(canc_tr_t5, cancer_chronotherapy_timing, theater_ratio, 5, 0.5).
narrative_ontology:measurement(canc_tr_t10, cancer_chronotherapy_timing, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(canc_be_t0, cancer_chronotherapy_timing, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(canc_be_t5, cancer_chronotherapy_timing, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(canc_be_t10, cancer_chronotherapy_timing, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cancer_chronotherapy_timing, resource_allocation).
narrative_ontology:affects_constraint(cancer_chronotherapy_timing, hospital_staffing_shortages).
narrative_ontology:affects_constraint(cancer_chronotherapy_timing, clinical_trial_recruitment_bias).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
