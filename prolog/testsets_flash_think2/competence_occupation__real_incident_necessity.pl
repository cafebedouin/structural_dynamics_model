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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Competence Occupation: Real Incident Necessity
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint represents the grim truth that, for certain high-stakes,
 *   low-frequency events, authentic competence can only be truly forged and
 *   proven under the conditions of an actual catastrophic incident. It is a
 *   reading of the 'competence_occupation' kernel, asserting the inherent
 *   insufficiency of simulations alone. High-Reliability Organizations (HROs)
 *   are caught in an ethical dilemma: they must prevent incidents, yet this
 *   constraint implies their ultimate competence remains unproven. The
 *   metrics reflect the high cost of this unresolvable problem and the
 *   societal suppression of this uncomfortable truth.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.9).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.85).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.9).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Competence Occupation: Real Incident Necessity").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '7a694479-ccf2-4019-a561-309b2cb0fe45').
narrative_ontology:cs_kernel_codification('7a694479-ccf2-4019-a561-309b2cb0fe45', implicit).
narrative_ontology:cs_authority_grounding('7a694479-ccf2-4019-a561-309b2cb0fe45', self_enforcing).
narrative_ontology:cs_reading_relation('7a694479-ccf2-4019-a561-309b2cb0fe45', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('7a694479-ccf2-4019-a561-309b2cb0fe45', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('7a694479-ccf2-4019-a561-309b2cb0fe45', foundational, competence_is_incident_bound).
narrative_ontology:cs_axiom_status(competence_is_incident_bound, holdable).
narrative_ontology:cs_axiom_grounding('7a694479-ccf2-4019-a561-309b2cb0fe45', competence_is_incident_bound, empirically_contingent).
narrative_ontology:cs_axiom('7a694479-ccf2-4019-a561-309b2cb0fe45', secondary, simulation_is_insufficient_for_authentic_competence).
narrative_ontology:cs_axiom_status(simulation_is_insufficient_for_authentic_competence, holdable).
narrative_ontology:cs_axiom_grounding('7a694479-ccf2-4019-a561-309b2cb0fe45', simulation_is_insufficient_for_authentic_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('7a694479-ccf2-4019-a561-309b2cb0fe45', authentic_incident_competence).
narrative_ontology:cs_drift_state('7a694479-ccf2-4019-a561-309b2cb0fe45', contemporary_safety_culture, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7a694479-ccf2-4019-a561-309b2cb0fe45', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, public_safety_regulators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, public_at_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, simulation_training_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations (e.g., nuclear power, aviation, critical care) whose core mission is to prevent catastrophic failure. They are existentially bound to maintain competence but cannot ethically seek the 'authentic' conditions this constraint claims are necessary. They bear the cost of unproven competence.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, identity_locked, global).

% Mandate and oversee competence standards for HROs, but also actively work to prevent the very incidents this constraint claims are necessary for authentic competence. They are caught in the dilemma, enforcing a system that, by this reading, cannot fully achieve its stated goal.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, public_safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Provide training and simulation environments intended to build and maintain competence. While this reading asserts their offerings are insufficient for 'authentic' competence, they benefit from the societal imperative to *attempt* to solve the problem, even if imperfectly.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_training_providers, beneficiary,
    organized, biographical, mobile, global).

% The ultimate victims of any catastrophic failure, bearing the direct human cost if HRO competence proves insufficient. They are unaware of the deep structural dilemma this constraint poses.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, public_at_risk, payer,
    powerless, immediate, trapped, local).

% Academics, philosophers, and critical safety researchers who articulate and analyze the structural dilemma of competence maintenance in HROs, often recognizing the grim truth this constraint represents.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This constraint describes a fundamental, unyielding truth about the nature of competence under extreme conditions, rather than a human-designed coordination mechanism.
% TRANSFER_FUNCTION: Transfers the burden of unresolvable competence maintenance onto high-reliability organizations, and the risk of unproven competence onto the public. It highlights an inherent, unavoidable cost.
% ABSENT_VOICES: The voices of those who have experienced catastrophic incidents and survived, or the families of those who did not. Their testimony would underscore the unique, irreducible demands of real-world failure conditions that simulations cannot replicate.
% DISAPPEARANCE_RATIONALE: This constraint describes an underlying reality of human competence and the conditions required to truly 'occupy' it. If the *recognition* of this truth vanished, the truth itself would remain, and the structural dilemma for HROs would persist, albeit unacknowledged.
% FOUNDING_PROBLEM: The inherent difficulty of reliably maintaining and proving competence for rare, high-consequence events in environments where actual incidents must be prevented at all costs.
% FOUNDING_PROBLEM_CORROBORATION: Historical analysis of major industrial and technological disasters, philosophical arguments on the nature of expertise and 'tacit knowledge' under extreme duress, and some critical safety researchers (e.g., those in the 'Safety-II' movement who emphasize resilience in practice over compliance with procedures). This corroboration comes from outside the direct beneficiaries of simulation training or the HROs themselves.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_unchanged).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the 'cost' of this truth is the unresolvable dilemma for HROs and the inherent risk to the public. Suppression is high because this truth is often denied or downplayed in safety cultures, which prefer to believe that simulations are fully sufficient. Theater ratio is high for simulation-based training, as this reading views them as performative substitutes for the 'real thing,' even if necessary. Accessibility collapse is high because, by definition, there are no true alternatives to real incidents for authentic competence. Resistance is high because HROs and society resist acknowledging this grim reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of HROs and regulators, this constraint is a deeply problematic, unacknowledged truth that undermines their mission. From the perspective of simulation providers, it's a challenge to be overcome by better technology. From the analytical observer's seat, it's a structural feature of reality that must be understood, regardless of its ethical implications.
 *
 * DIRECTIONALITY LOGIC:
 *   HROs and public safety regulators are victims/payers, as they bear the burden of this unresolvable dilemma. The public at risk are also victims, as they face the consequences of unproven competence. Simulation training providers are beneficiaries, as they offer the only ethically permissible (though, by this reading, insufficient) means to address the competence gap. Analytical observers are outside the direct flow, observing the structural truth.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''real_incident_necessity'' reading of the ''competence_occupation'' kernel?',
    'Analysis of foundational texts and practices within HRO safety discourse to confirm the explicit or implicit assertion that only real incidents confer authentic competence.',
    'If misidentified, the entire structural analysis of this constraint''s relationship to its siblings would be invalid, requiring re-classification and re-linking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verification of kernel and reading identification.').

omega_variable(
    simulation_authenticity_threshold,
    'At what point, if any, could simulation fidelity and psychological realism approach the ''authentic conditions'' of a real incident to sufficiently occupy the competence kernel?',
    'Empirical studies comparing performance in high-fidelity simulations to performance in actual, rare, high-consequence events (where ethically possible, e.g., through post-incident analysis or natural experiments).',
    'If a sufficiently high threshold is found, this reading''s core premise (''only real incidents'') would be challenged, potentially shifting its relationship to ''simulation_sufficiency'' from ''forecloses'' to ''influences'' or ''coexists_with''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_authenticity_threshold, empirical, 'The potential for advanced simulations to bridge the gap to ''authentic'' competence.').

omega_variable(
    ethical_dilemma_resolution,
    'Is the ethical dilemma of needing incidents for competence while preventing them truly unresolvable, or are there alternative ethical frameworks that reconcile this tension?',
    'Philosophical and ethical inquiry into the nature of responsibility, risk, and knowledge acquisition in high-stakes domains, potentially drawing on ''Safety-II'' or resilience engineering principles.',
    'If a robust ethical reconciliation is found, the ''extractiveness'' and ''suppression'' metrics might decrease, as the inherent ''cost'' and ''denial'' of the truth would be mitigated by a new understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_dilemma_resolution, conceptual, 'The fundamental ethical conflict inherent in this competence model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1970, competence_occupation__real_incident_necessity, theater_ratio, 1970, 0.5).
narrative_ontology:measurement(comp_tr_t1980, competence_occupation__real_incident_necessity, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(comp_tr_t1990, competence_occupation__real_incident_necessity, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(comp_tr_t2000, competence_occupation__real_incident_necessity, theater_ratio, 2000, 0.65).
narrative_ontology:measurement(comp_tr_t2010, competence_occupation__real_incident_necessity, theater_ratio, 2010, 0.68).
narrative_ontology:measurement(comp_tr_t2020, competence_occupation__real_incident_necessity, theater_ratio, 2020, 0.7).

% Extraction over time
narrative_ontology:measurement(comp_be_t1970, competence_occupation__real_incident_necessity, base_extractiveness, 1970, 0.8).
narrative_ontology:measurement(comp_be_t1980, competence_occupation__real_incident_necessity, base_extractiveness, 1980, 0.83).
narrative_ontology:measurement(comp_be_t1990, competence_occupation__real_incident_necessity, base_extractiveness, 1990, 0.86).
narrative_ontology:measurement(comp_be_t2000, competence_occupation__real_incident_necessity, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(comp_be_t2010, competence_occupation__real_incident_necessity, base_extractiveness, 2010, 0.89).
narrative_ontology:measurement(comp_be_t2020, competence_occupation__real_incident_necessity, base_extractiveness, 2020, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1970, competence_occupation__real_incident_necessity, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(comp_su_t1980, competence_occupation__real_incident_necessity, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(comp_su_t1990, competence_occupation__real_incident_necessity, suppression_requirement, 1990, 0.81).
narrative_ontology:measurement(comp_su_t2000, competence_occupation__real_incident_necessity, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(comp_su_t2010, competence_occupation__real_incident_necessity, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(comp_su_t2020, competence_occupation__real_incident_necessity, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
