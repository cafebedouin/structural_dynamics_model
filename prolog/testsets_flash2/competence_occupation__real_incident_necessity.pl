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
 *   human_readable: Competence Occupation: Real Incident Necessity
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint represents a reading of the 'competence occupation'
 *   kernel, asserting that only actual catastrophic incidents provide the
 *   authentic conditions necessary to truly occupy the competence kernel in
 *   high-reliability domains. It is framed as a 'mountain' due to its
 *   perceived natural, irreducible quality, where the conditions for 'true'
 *   competence are inherently tied to real-world, high-consequence events.
 *   This reading implies an unresolvable dilemma for high-reliability
 *   organizations: they must prevent incidents, yet only incidents can fully
 *   validate their competence. The metrics reflect the high 'cost'
 *   (extractiveness) of this inherent limitation and the near-complete
 *   'suppression' of alternatives for achieving full competence without real
 *   incidents.
 *
 * KEY AGENTS:
 *   - high_reliability_organizations: Primary target (institutional/trapped) — bears the unresolvable competence problem.
 *   - safety_regulators: Secondary target (institutional/trapped) — faces regulatory dilemma.
 *   - frontline_operators: Direct target (moderate/identity_locked) — experiences personal competence gap.
 *   - training_and_simulation_industry: Excluded (organized/constrained) — their solutions are deemed insufficient.
 *   - analytical_observers: Analytical observer (analytical/analytical) — sees the full structural dilemma.
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
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Competence Occupation: Real Incident Necessity").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '1603232a-a4be-4956-8f91-2c6fe261a4e8').
narrative_ontology:cs_kernel_codification('1603232a-a4be-4956-8f91-2c6fe261a4e8', implicit).
narrative_ontology:cs_authority_grounding('1603232a-a4be-4956-8f91-2c6fe261a4e8', diffuse_epistemic).
narrative_ontology:cs_reading_relation('1603232a-a4be-4956-8f91-2c6fe261a4e8', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('1603232a-a4be-4956-8f91-2c6fe261a4e8', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('1603232a-a4be-4956-8f91-2c6fe261a4e8', foundational, authentic_conditions_are_irreducible).
narrative_ontology:cs_axiom_status(authentic_conditions_are_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('1603232a-a4be-4956-8f91-2c6fe261a4e8', authentic_conditions_are_irreducible, deontological).
narrative_ontology:cs_axiom('1603232a-a4be-4956-8f91-2c6fe261a4e8', secondary, simulation_is_inherently_incomplete).
narrative_ontology:cs_axiom_status(simulation_is_inherently_incomplete, holdable).
narrative_ontology:cs_axiom_grounding('1603232a-a4be-4956-8f91-2c6fe261a4e8', simulation_is_inherently_incomplete, empirically_contingent).
narrative_ontology:cs_reference_frame('1603232a-a4be-4956-8f91-2c6fe261a4e8', authentic_experience_primacy).
narrative_ontology:cs_drift_state('1603232a-a4be-4956-8f91-2c6fe261a4e8', contemporary_safety_science, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1603232a-a4be-4956-8f91-2c6fe261a4e8', '').
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

% These organizations operate complex, high-risk systems where failure is catastrophic. Under this reading, they are perpetually unable to fully occupy the competence kernel without experiencing actual incidents, which they strive to prevent. This creates an unresolvable tension and a constant state of 'not-quite-competent' for the most critical skills.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, trapped, global).

% Tasked with ensuring safety, regulators are caught in the dilemma that the 'true' test of competence requires incidents they are mandated to prevent. This reading renders their preventative measures as inherently insufficient for full competence occupation, leading to a perpetual state of regulatory anxiety and an inability to certify 'full' competence.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_regulators, payer,
    institutional, generational, trapped, national).

% The individuals who must perform under extreme pressure. This reading implies their training and simulation, while valuable, can never fully prepare them for the 'real thing,' leading to a sense of inadequacy or a fatalistic acceptance that only a real incident will truly test their mettle. Their professional identity is tied to being competent, but the conditions for 'true' competence are catastrophic.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Provides tools and methods for competence maintenance, but under this reading, their offerings are fundamentally limited. They are excluded from providing the 'authentic conditions' necessary for full competence occupation, rendering their efforts as perpetually secondary or incomplete.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, training_and_simulation_industry, excluded,
    organized, biographical, constrained, global).

% Academics and researchers who study high-reliability systems. They observe the inherent contradiction and the systemic challenges this reading poses for safety and competence, recognizing the 'natural law' aspect of the claim while also seeing its devastating implications.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint, by its nature, does not coordinate human activity but rather describes an irreducible condition for a specific type of competence. It highlights a fundamental limitation in how competence can be achieved and maintained in high-stakes environments.
% TRANSFER_FUNCTION: It 'transfers' an unresolvable competence deficit onto high-reliability organizations and their operators, forcing them into a perpetual state of 'not-quite-ready' for the most critical scenarios. It also transfers the burden of 'true' competence validation to catastrophic events.
% ABSENT_VOICES: The training and simulation industry, along with proponents of 'proactive safety' and 'resilience engineering,' would vehemently object. They would argue that this reading is fatalistic and undermines efforts to build competence through non-catastrophic means. They are excluded by the very premise of the constraint.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the entire framework for competence maintenance in high-reliability organizations would fundamentally shift. It would open the door for alternative methods (like advanced simulation or continuous training) to be considered 'sufficient' for competence occupation, leading to a complete re-evaluation of safety protocols, training regimes, and regulatory standards. The current state of perpetual anxiety and the implicit acceptance of 'learning from failure' would be replaced by a more optimistic and proactive approach to competence.
% FOUNDING_PROBLEM: The problem of ensuring genuine, tested competence in high-stakes, low-frequency event environments, where the cost of failure is catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: This problem is attested by historical records of major incidents, post-accident investigations, and the ongoing challenge faced by high-reliability organizations in maintaining readiness for events that rarely occur. Analytical observers and safety historians corroborate the persistence of this fundamental challenge, noting that despite advances in training, the 'first time' for a real catastrophe remains a unique test.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is extremely high (0.95) because the 'cost' of achieving full competence under this reading is the occurrence of catastrophic incidents, which are inherently unacceptable. Suppression is also extremely high (0.98) because this reading fundamentally 'suppresses' the possibility of any alternative (simulations, drills, theoretical knowledge) being sufficient for 'true' competence. Theater ratio is very low (0.05) because there is little performative maintenance; the constraint is seen as an unyielding reality. Accessibility collapse is near total (0.99) as no other path to 'true' competence is recognized. Resistance is minimal (0.01) because, while the implications are dire, the 'natural law' framing makes direct resistance against the premise itself seem futile.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of high-reliability organizations, this constraint is a tragic, unavoidable truth – a 'mountain' that dictates the limits of their preparedness. From the perspective of the training industry, it's a conceptual 'snare' that devalues their efforts and traps organizations in a fatalistic loop. The engine's classification will highlight this divergence, showing how a 'mountain' for some is a 'snare' for others due to the inherent 'victim' status of those caught in its logic.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations, safety regulators, and frontline operators are all victims (d near 1.0) because they bear the unresolvable cost of this constraint: the inability to achieve full competence without catastrophic incidents. There are no direct beneficiaries, as no party 'benefits' from the necessity of catastrophe. The constraint itself is the 'extractor' of peace of mind and certifiable readiness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, if accepted, prevents mislabeling the inherent difficulty of competence maintenance in high-risk environments as a solvable coordination problem. It forces an acknowledgment of an irreducible limitation, rather than assuming that all competence gaps can be closed through human-designed interventions. It guards against the 'mandatrophy' of safety programs that might claim to achieve full competence through simulations, when this reading asserts such claims are inherently incomplete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_conceptual_trap,
    'Is this constraint a genuine natural law governing competence in extreme environments, or a conceptual trap that prevents the development of alternative, non-catastrophic competence validation methods?',
    'Empirical evidence from organizations that successfully maintain competence for rare, high-consequence events without experiencing actual incidents, or a philosophical re-evaluation of what ''authentic conditions'' truly entail.',
    'If a natural law, the classification as ''mountain'' holds, highlighting an irreducible limitation. If a conceptual trap, it reclassifies as a ''snare'' or ''tangled_rope'' where the ''extraction'' is the perpetual anxiety and devaluing of proactive safety measures, maintained by a rigid conceptual framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_conceptual_trap, conceptual, 'Ambiguity between an inherent limitation and a self-imposed conceptual barrier to competence.').

omega_variable(
    observability_of_competence,
    'To what extent can ''true'' competence for catastrophic events be observed and measured in non-catastrophic settings (e.g., advanced simulations, stress tests, theoretical knowledge)?',
    'Development of validated metrics and predictive models that correlate performance in simulated extreme conditions with actual performance in rare incidents, or a consensus shift in the scientific community regarding the fidelity of simulation.',
    'If high observability is possible, the ''accessibility collapse'' and ''suppression'' metrics would decrease, potentially shifting the constraint away from a ''mountain'' towards a ''rope'' or ''scaffold'' that supports competence development through non-catastrophic means. If observability remains low, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observability_of_competence, empirical, 'The degree to which competence for extreme events can be assessed outside of actual incidents.').

omega_variable(
    identity_lock_of_operators,
    'Is the ''identity_locked'' exit option for frontline operators a structural consequence of the constraint, or an internalized belief that could be altered by different training philosophies?',
    'Longitudinal studies of operator cohorts exposed to different training philosophies (e.g., ''resilience engineering'' vs. ''failure-is-the-only-teacher'') to see if their self-perception of competence and willingness to exit changes.',
    'If internalized, the effective suppression is higher than structural measures suggest, as operators carry the belief that only real incidents validate them. If purely structural, the identity lock is a direct consequence of the constraint''s premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_operators, empirical, 'Structural vs. internalized identity lock for frontline operators regarding competence validation.').


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
