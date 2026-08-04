% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Competence Occupation: Real Incident Necessity Reading
 *   domain: organizational_theory/safety_science
 *
 * SUMMARY:
 *   This constraint represents a reading of 'competence occupation' that
 *   posits only actual catastrophic incidents provide the authentic
 *   conditions necessary to truly occupy the competence kernel. It is
 *   classified as a Mountain because it describes an irreducible,
 *   unchangeable condition of reality from this perspective. The high
 *   extractiveness and suppression reflect the inherent, unresolvable paradox
 *   it imposes on high-reliability organizations: they are perpetually
 *   'un-competent' by this standard, and any attempt to prove otherwise is
 *   suppressed by the very nature of the claim. There are no beneficiaries,
 *   as catastrophes are universally undesirable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.95).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.98).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.95).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Competence Occupation: Real Incident Necessity Reading").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational_theory/safety_science").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '15978eba-880e-4a09-a0bb-eb2c97e814e5').
narrative_ontology:cs_kernel_codification('15978eba-880e-4a09-a0bb-eb2c97e814e5', implicit).
narrative_ontology:cs_authority_grounding('15978eba-880e-4a09-a0bb-eb2c97e814e5', diffuse_epistemic).
narrative_ontology:cs_reading_relation('15978eba-880e-4a09-a0bb-eb2c97e814e5', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('15978eba-880e-4a09-a0bb-eb2c97e814e5', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('15978eba-880e-4a09-a0bb-eb2c97e814e5', foundational, authenticity_requires_catastrophe).
narrative_ontology:cs_axiom_status(authenticity_requires_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('15978eba-880e-4a09-a0bb-eb2c97e814e5', authenticity_requires_catastrophe, deontological).
narrative_ontology:cs_reference_frame('15978eba-880e-4a09-a0bb-eb2c97e814e5', unoccupied_competence_paradox).
narrative_ontology:cs_drift_state('15978eba-880e-4a09-a0bb-eb2c97e814e5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('15978eba-880e-4a09-a0bb-eb2c97e814e5', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, safety_regulators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These organizations (e.g., nuclear power plants, air traffic control) are structurally committed to maintaining competence in extreme conditions. This reading implies they can never truly achieve or prove competence without experiencing the very catastrophes they exist to prevent, creating an unresolvable paradox.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, trapped, global).

% Tasked with ensuring safety and competence, regulators are caught in the bind of this reading: they cannot certify 'occupied competence' without real incidents, which are unacceptable. This forces them into a position of perpetual uncertainty or reliance on proxies they know are insufficient.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_regulators, payer,
    institutional, generational, constrained, national).

% The individuals who must perform competently in a crisis. This reading implies their training and simulation, however rigorous, are fundamentally inadequate to 'occupy the competence kernel' until they face an actual catastrophe, leading to profound psychological burden and a sense of inherent inadequacy.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Analysts who study high-reliability systems and competence. This reading, while stark, provides a clear (if tragic) empirical boundary for their theories, highlighting the limits of proxy measures for true competence in extreme domains.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, academic_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts that the 'competence kernel' is a natural, irreducible property of extreme conditions, which can only be truly 'occupied' by direct experience. It doesn't coordinate human action but describes a fundamental limitation on competence acquisition.
% TRANSFER_FUNCTION: It transfers an unresolvable burden of proof and an inherent state of 'unoccupied competence' from the natural world to high-reliability organizations and their personnel. It extracts a psychological and operational cost of perpetual inadequacy.
% ABSENT_VOICES: The victims of actual catastrophic incidents, whose experiences would provide the 'authentic conditions' but at an unacceptable human cost. Their voices are silenced by the very nature of the constraint.
% DISAPPEARANCE_RATIONALE: If this constraint (the idea that only real incidents confer competence) disappeared, the underlying physical and psychological realities of extreme competence would remain. Organizations would still struggle with competence maintenance, but the conceptual barrier to 'occupying the kernel' through other means would be removed, allowing for different approaches to validation.
% FOUNDING_PROBLEM: The problem of ensuring genuine competence in high-stakes, low-frequency, high-consequence events, where the cost of failure is catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: This problem is attested by the historical record of catastrophic failures in complex systems (e.g., Chernobyl, Challenger, Fukushima) and by the ongoing challenges faced by high-reliability organizations in maintaining readiness. Academic safety researchers and accident investigators corroborate the persistent difficulty of proving competence without real-world tests.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_unchanged).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.95) is near maximal because this reading imposes an impossible standard, extracting a constant cost of unresolvable competence deficit from organizations and individuals. Suppression (0.98) is also near maximal because the claim itself suppresses any alternative means of 'occupying the kernel' (e.g., simulation) as fundamentally inauthentic. Theater ratio is minimal (0.05) because there's little performative maintenance; the constraint is a stark, unyielding truth from this perspective. Accessibility collapse is near total (0.99) as no alternative path to 'authentic competence' is recognized. Resistance is minimal (0.02) because, from this perspective, the claim is an unassailable truth, not something to be resisted.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap among human actors; all experience this constraint as a fundamental, unyielding truth that imposes an impossible burden. The 'gap' is between the human desire for competence and the natural conditions for its 'authentic occupation' as defined by this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations, safety regulators, and frontline operators are all structural targets (victims) of this constraint. They bear the full burden of its impossible standard, with no viable exit that would allow them to 'occupy the competence kernel' without incurring catastrophic costs. Their directionality is near 1.0, reflecting this maximal extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a Mountain, is not subject to mandatrophy in the conventional sense. Its 'mandate' is a statement about reality, not a human-designed function that can atrophy. The classification prevents mislabeling this fundamental (if tragic) truth as a human-constructed Snare or Tangled Rope, which would imply a solvable problem or an identifiable beneficiary, neither of which exists here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural law, or a constructed constraint that benefits identifiable agents?',
    'Analysis of the ''competence_occupation'' kernel across all its readings: if other readings reveal a constructed beneficiary structure, this reading''s ''naturalness'' is challenged.',
    'If this reading is found to be a constructed constraint, its classification would shift from Mountain to a more extractive type (e.g., Snare or Tangled Rope), and its high extractiveness would be reinterpreted as a product of human design rather than natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between natural law and constructed constraint for this reading of competence occupation.').

omega_variable(
    simulation_validity_ambiguity,
    'To what extent can high-fidelity simulations genuinely replicate the ''authentic conditions'' of a real incident for competence occupation?',
    'Empirical studies comparing performance in high-fidelity simulations to performance in actual incidents, accounting for psychological and physiological stressors.',
    'If simulations are found to be highly effective, this reading''s claim of ''real incident necessity'' is weakened, potentially shifting the overall kernel''s interpretation towards ''simulation_sufficiency'' or ''hybrid_occupation''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_validity_ambiguity, empirical, 'The empirical validity of simulation as a proxy for real-incident competence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.05).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.05).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__real_incident_necessity, theater_ratio, 30, 0.05).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.05).
narrative_ontology:measurement(comp_tr_t50, competence_occupation__real_incident_necessity, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.95).
narrative_ontology:measurement(comp_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.95).
narrative_ontology:measurement(comp_be_t30, competence_occupation__real_incident_necessity, base_extractiveness, 30, 0.95).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(comp_be_t50, competence_occupation__real_incident_necessity, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.98).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.98).
narrative_ontology:measurement(comp_su_t20, competence_occupation__real_incident_necessity, suppression_requirement, 20, 0.98).
narrative_ontology:measurement(comp_su_t30, competence_occupation__real_incident_necessity, suppression_requirement, 30, 0.98).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.98).
narrative_ontology:measurement(comp_su_t50, competence_occupation__real_incident_necessity, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
