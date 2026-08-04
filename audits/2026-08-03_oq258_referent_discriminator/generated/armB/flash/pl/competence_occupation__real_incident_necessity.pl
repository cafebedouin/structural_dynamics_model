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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Competence Occupation: Real Incident Necessity
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint posits that only actual catastrophic incidents provide
 *   the authentic conditions necessary to truly occupy the 'competence
 *   kernel' in high-reliability organizations. This reading implies that all
 *   other forms of training, simulation, or procedural reinforcement are
 *   insufficient for genuine competence maintenance. It is a 'mountain' in
 *   the sense that it describes an irreducible epistemic limit on competence
 *   validation, but one with devastating 'victims' (the organizations
 *   themselves, their operators, and the public) and no viable
 *   'beneficiaries' (as no one benefits from catastrophe). The high
 *   extractiveness reflects the unresolvable latent risk and the cost of
 *   perpetual uncertainty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.95).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.88).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.95).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Competence Occupation: Real Incident Necessity").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'dbc21df6-7b7f-428c-81c3-6f191ba24d05').
narrative_ontology:cs_kernel_codification('dbc21df6-7b7f-428c-81c3-6f191ba24d05', implicit).
narrative_ontology:cs_authority_grounding('dbc21df6-7b7f-428c-81c3-6f191ba24d05', practice).
narrative_ontology:cs_interpretation_layer_present('dbc21df6-7b7f-428c-81c3-6f191ba24d05').
narrative_ontology:cs_reading_relation('dbc21df6-7b7f-428c-81c3-6f191ba24d05', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('dbc21df6-7b7f-428c-81c3-6f191ba24d05', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('dbc21df6-7b7f-428c-81c3-6f191ba24d05', foundational, authenticity_requires_catastrophe).
narrative_ontology:cs_axiom_status(authenticity_requires_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('dbc21df6-7b7f-428c-81c3-6f191ba24d05', authenticity_requires_catastrophe, deontological).
narrative_ontology:cs_axiom('dbc21df6-7b7f-428c-81c3-6f191ba24d05', foundational, simulation_is_insufficient).
narrative_ontology:cs_axiom_status(simulation_is_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('dbc21df6-7b7f-428c-81c3-6f191ba24d05', simulation_is_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('dbc21df6-7b7f-428c-81c3-6f191ba24d05', catastrophic_validation_paradigm).
narrative_ontology:cs_drift_state('dbc21df6-7b7f-428c-81c3-6f191ba24d05', contemporary_safety_science_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dbc21df6-7b7f-428c-81c3-6f191ba24d05', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, affected_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These organizations operate complex, high-consequence systems where failure is catastrophic. They are trapped by the belief that only real incidents can truly test and maintain competence, leading to an unresolvable problem of competence decay in the absence of such incidents. They bear the cost of this epistemic trap through latent risk and the inability to certify true readiness.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, trapped, global).

% The individuals who operate the high-consequence systems. Their professional identity is often tied to their ability to perform under extreme pressure, but this reading implies their true competence can only be validated by actual catastrophe, leading to chronic anxiety and a sense of unproven readiness. They bear the psychological and professional cost of this unresolvable dilemma.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% The public whose safety depends on the competence of high-reliability organizations. They are the ultimate victims of any catastrophic incident that might 'occupy the competence kernel,' bearing the direct human and societal costs. They are trapped by their reliance on these systems and the implicit acceptance of this competence model.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, affected_public, payer,
    powerless, immediate, trapped, regional).

% Tasked with ensuring public safety, but constrained by the epistemic claim that only real incidents provide true competence data. They struggle to design effective training and certification regimes, often defaulting to procedural compliance rather than genuine competence assessment. They administer a system that implicitly accepts this constraint.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Proponents of advanced simulation and training technologies. They are excluded from the core competence kernel by this reading, which devalues their offerings as 'not authentic.' They would argue for the sufficiency of high-fidelity simulation but are marginalized by the 'real incident' dogma.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_designers, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint implicitly coordinates the understanding of competence within high-reliability domains, defining what 'true' readiness means, even if that definition is catastrophic.
% TRANSFER_FUNCTION: It transfers the burden of competence validation onto the occurrence of rare, catastrophic events, effectively transferring latent risk from the theoretical realm to the real world, from organizations to the public.
% ABSENT_VOICES: Simulation designers and proponents of continuous, non-catastrophic competence maintenance would object, arguing that this reading creates an impossible standard and devalues proactive safety measures. They are excluded by the epistemic framing of 'authenticity' tied to real incidents.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, high-reliability organizations would be forced to fundamentally rethink competence maintenance, investing heavily in advanced simulation, continuous assessment, and alternative validation methods. The entire safety training and certification industry would reorganize around proactive, non-catastrophic competence occupation.
% FOUNDING_PROBLEM: The inherent difficulty of validating competence in systems where failure is rare but catastrophic, and where 'normal' operations do not stress the system to its limits.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by safety researchers, accident investigators, and organizational theorists who highlight the 'normalization of deviance' and the 'gap between espoused theory and theory-in-use' in high-reliability contexts. This corroboration comes from outside the direct beneficiaries of the status quo (as there are no true beneficiaries of catastrophe).
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
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
 *   The extractiveness is extremely high because the constraint imposes an impossible burden: true competence can only be 'occupied' through events that are, by definition, failures of the system. This creates a perpetual state of unvalidated competence and latent risk. Suppression is high because this epistemic claim is deeply embedded in some organizational cultures, making it difficult to challenge or propose alternatives without being seen as 'soft' on safety. Theater ratio is low because there's little performative maintenance; the constraint is a genuine, albeit tragic, epistemic limit.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of high-reliability organizations and frontline operators, this constraint is a tragic mountain: an unchangeable truth about the limits of human preparation. From an analytical observer's perspective, it's a conceptual trap that prevents effective competence maintenance by setting an unattainable and destructive standard.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations, frontline operators, and the affected public are all victims, bearing the costs of latent risk and actual catastrophe. Safety regulators are agenda-setters who administer a system shaped by this constraint, struggling to find solutions within its bounds. Simulation designers are excluded, as their offerings are deemed insufficient by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it describes an epistemic limit rather than a human-designed mandate. However, its persistence creates a 'mandatrophy of competence' where the mandate to be competent cannot be authentically fulfilled without unacceptable costs. The classification as a Mountain with high extractiveness and victims highlights the tragic nature of this epistemic trap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_vs_constructed_limit,
    'Is the necessity of real incidents for competence occupation a genuine epistemic limit (a Mountain), or a constructed belief sustained by organizational culture and a lack of imagination for alternative validation methods (a Snare)?',
    'Empirical demonstration of alternative, non-catastrophic methods (e.g., advanced simulation, predictive analytics, continuous micro-assessments) that reliably predict performance in real incidents, leading to a shift in organizational belief and practice.',
    'If a genuine epistemic limit, the classification as Mountain holds, highlighting an irreducible challenge. If constructed, it reclassifies as a Snare, revealing an extractive mechanism that perpetuates latent risk for the benefit of maintaining a particular (and flawed) understanding of competence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_vs_constructed_limit, conceptual, 'Ambiguity between a natural epistemic limit and a socially constructed belief about competence validation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''competence_occupation'' kernel. What would change structurally if a sibling reading (e.g., ''simulation_sufficiency'' or ''hybrid_occupation'') were adopted?',
    'Analysis of the structural implications of adopting a different reading, specifically how it would alter the declared beneficiaries/victims, extractiveness, and suppression of the competence maintenance system.',
    'Adopting ''simulation_sufficiency'' would likely lower extractiveness and suppression by providing a viable, non-catastrophic path to competence. Adopting ''hybrid_occupation'' would introduce a more complex, multi-faceted approach, potentially reducing extractiveness but increasing coordination costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the competence_occupation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1950, competence_occupation__real_incident_necessity, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(comp_tr_t1970, competence_occupation__real_incident_necessity, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(comp_tr_t1990, competence_occupation__real_incident_necessity, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(comp_tr_t2010, competence_occupation__real_incident_necessity, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(comp_tr_t2024, competence_occupation__real_incident_necessity, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comp_be_t1950, competence_occupation__real_incident_necessity, base_extractiveness, 1950, 0.9).
narrative_ontology:measurement(comp_be_t1970, competence_occupation__real_incident_necessity, base_extractiveness, 1970, 0.92).
narrative_ontology:measurement(comp_be_t1990, competence_occupation__real_incident_necessity, base_extractiveness, 1990, 0.93).
narrative_ontology:measurement(comp_be_t2010, competence_occupation__real_incident_necessity, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(comp_be_t2024, competence_occupation__real_incident_necessity, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1950, competence_occupation__real_incident_necessity, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(comp_su_t1970, competence_occupation__real_incident_necessity, suppression_requirement, 1970, 0.82).
narrative_ontology:measurement(comp_su_t1990, competence_occupation__real_incident_necessity, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(comp_su_t2010, competence_occupation__real_incident_necessity, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(comp_su_t2024, competence_occupation__real_incident_necessity, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
