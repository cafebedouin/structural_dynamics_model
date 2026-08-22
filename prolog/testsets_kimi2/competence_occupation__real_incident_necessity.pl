% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Real Incident Necessity for Competence Occupation
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint story instantiates the real_incident_necessity reading of
 *   the competence_occupation kernel: the claim that only actual catastrophic
 *   incidents provide the authentic conditions necessary to occupy the
 *   competence kernel in high-reliability organizations. The reading treats
 *   this as an irreducible epistemic boundaryâa natural law of competence
 *   formation under extreme conditionsârather than a contingent
 *   organizational choice. High-reliability organizations are thus trapped in
 *   a paradox: they must prevent catastrophes, yet authentic competence to
 *   prevent them requires catastrophic exposure. No viable beneficiary
 *   structure exists because catastrophes are unacceptable outcomes, not
 *   extractable rents. The constraint is contested by sibling readings
 *   (simulation_sufficiency, hybrid_occupation) that argue competence can be
 *   maintained without real incidents. As a kernel reading, the constraint is
 *   authored clean: other readings are not described inside this file but are
 *   linked via network.affects_constraints and documented in omegas.
 *
 * KEY AGENTS:
 *   - hro_operators (payer/institutional/trapped): Bear the unresolvable maintenance paradox
 *   - simulation_researchers (excluded/moderate/constrained): Marginalized advocates for synthetic training
 *   - safety_science_community (observer/analytical/analytical): Produces the contested readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.12).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.25).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.12).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Incident Necessity for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '8394dc2f-295b-4e79-b6c0-46d8328a09f1').
narrative_ontology:cs_kernel_codification('8394dc2f-295b-4e79-b6c0-46d8328a09f1', formalized).
narrative_ontology:cs_authority_grounding('8394dc2f-295b-4e79-b6c0-46d8328a09f1', expertise).
narrative_ontology:cs_interpretation_layer_present('8394dc2f-295b-4e79-b6c0-46d8328a09f1').
narrative_ontology:cs_reading_relation('8394dc2f-295b-4e79-b6c0-46d8328a09f1', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('8394dc2f-295b-4e79-b6c0-46d8328a09f1', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('8394dc2f-295b-4e79-b6c0-46d8328a09f1', foundational, authentic_competence_requires_catastrophe).
narrative_ontology:cs_axiom_status(authentic_competence_requires_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('8394dc2f-295b-4e79-b6c0-46d8328a09f1', authentic_competence_requires_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('8394dc2f-295b-4e79-b6c0-46d8328a09f1', foundational, simulation_lacks_existential_authenticity).
narrative_ontology:cs_axiom_status(simulation_lacks_existential_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('8394dc2f-295b-4e79-b6c0-46d8328a09f1', simulation_lacks_existential_authenticity, empirically_contingent).
narrative_ontology:cs_reference_frame('8394dc2f-295b-4e79-b6c0-46d8328a09f1', catastrophe_anchored_competence).
narrative_ontology:cs_drift_state('8394dc2f-295b-4e79-b6c0-46d8328a09f1', advanced_simulation_epoch, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8394dc2f-295b-4e79-b6c0-46d8328a09f1', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, hro_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate high-risk sociotechnical systems where competence must be maintained, but authentic competence conditions require catastrophic incidents they are structurally committed to preventing. Cannot exit the paradox without abandoning the domain or accepting inauthentic competence.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, hro_operators, payer,
    institutional, generational, trapped, global).

% Develop and advocate for high-fidelity simulation as a substitute for real incident exposure, but are marginalized in safety cultures and training regimes that treat lived catastrophe as the only authentic teacher.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_researchers, excluded,
    moderate, biographical, constrained, national).

% Observes and documents the competence maintenance paradox across high-risk domains; produces the contested readings of whether real incidents are necessary.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_science_community, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination function; this reading treats competence maintenance as bounded by a natural limit rather than solved by social arrangement.
% TRANSFER_FUNCTION: No transfer function; the constraint imposes a boundary condition without moving resources between agents.
% ABSENT_VOICES: Simulation researchers and technologists who argue that synthetic experience can replicate the stress and ambiguity of catastrophe are structurally excluded from training regime design when the real-incident-necessity doctrine dominates.
% DISAPPEARANCE_RATIONALE: If the necessity of real catastrophic incidents for competence vanished, high-reliability organizations could maintain authentic expertise through simulation and routine practice alone, eliminating the paradox that catastrophe prevention requires catastrophe exposure. Training regimes, resource allocation, and safety ethics would reorganize around synthetic and procedural rehearsal.
% FOUNDING_PROBLEM: How to maintain expert competence in domains where failure is catastrophic and extremely rare, such that practitioners may never encounter the conditions they must master.
% FOUNDING_PROBLEM_CORROBORATION: Safety researchers and HRO practitioners attest to the problem from operational experience; however, the specific claim that only real incidents solve it is contested by simulation researchers and hybrid-training advocates who attest the problem can be addressed through other means.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

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
 *   Extractiveness is very low (0.12) because the constraint is authored as a natural boundary condition, not an extractive arrangement; no party collects from its operation. Suppression is low-moderate (0.25) because the doctrine epistemically marginalizes simulation alternatives without active enforcement. Accessibility collapse is very high (0.90) because, if the reading holds, alternatives genuinely fail to produce authentic competence. Resistance is moderate (0.45) because the sibling readings constitute active epistemic contestation. Theater ratio is negligible (0.05). The claim/metric independence is maintained: the reading claims mountain status while the metrics honestly report the contested, paradoxical boundary it imposes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (HRO operators) experiences a severe paradox: they are structurally committed to preventing the very incidents their competence supposedly requires. The observer seat (safety science) sees a contested epistemic boundary. The excluded seat (simulation researchers) experiences suppression of their alternative. The engine will compute divergent per-seat classifications from this structural asymmetry: the human parties experience constraint differently depending on their structural position relative to the boundary condition.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared because the constraint's referent is a natural law, not a social arrangement with rent extraction. HRO operators are declared as victims/payers because they bear the structural cost of the paradox: they must maintain competence under conditions that require unacceptable failures. Simulation researchers are excluded, not victimsâtheir exclusion is epistemic marginalization rather than cost-bearing. Directionality for HRO operators is near full target because the constraint imposes severe boundary costs on them; however, because this is a mountain, effective extraction remains damped by the naturality of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint is competence maintenance. The reading holds that the mandate cannot be fulfilled without real incidentsâa tragic mandate. There is no mandatrophy in the usual sense because the constraint has not outlived its function; rather, its function is structurally unfulfillable under the prevention mandate. This distinguishes it from piton: a piton is a dead arrangement maintained theatrically, whereas this is a live, painful boundary condition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contested_naturality,
    'Is the necessity of real catastrophic incidents for authentic competence a genuine natural law of human learning, or a historically contingent doctrine rooted in available technology and institutional culture?',
    'Longitudinal studies comparing competence outcomes between practitioners trained exclusively on advanced simulation versus those with real incident exposure, where ethical review permits; analysis of historical variation in safety doctrine as simulation fidelity improves.',
    'If the constraint is contingent, it reclassifies from mountain to a constructed doctrine (likely piton or snare depending on beneficiary structure), dissolving the paradox and opening design space for simulation-centric training regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_naturality, empirical, 'Whether real-incident necessity is natural law or contingent doctrine').

omega_variable(
    paradox_resolution_impossibility,
    'Can the competence maintenance paradox (needing catastrophes to prevent them) be resolved through technological or organizational innovation, or is it structurally inescapable?',
    'Tracking of simulation fidelity curves and corresponding competence retention metrics in HROs over multi-decade intervals.',
    'If resolvable, the constraint''s accessibility_collapse falls and the mountain claim weakens; if inescapable, the mountain classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradox_resolution_impossibility, empirical, 'Whether the competence paradox is inescapable or resolvable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__real_incident_necessity, theater_ratio, 8, 0.05).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__real_incident_necessity, theater_ratio, 16, 0.05).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.05).
narrative_ontology:measurement(comp_tr_t32, competence_occupation__real_incident_necessity, theater_ratio, 32, 0.05).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.05).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(comp_be_t8, competence_occupation__real_incident_necessity, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(comp_be_t16, competence_occupation__real_incident_necessity, base_extractiveness, 16, 0.12).
narrative_ontology:measurement(comp_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.12).
narrative_ontology:measurement(comp_be_t32, competence_occupation__real_incident_necessity, base_extractiveness, 32, 0.12).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comp_su_t8, competence_occupation__real_incident_necessity, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(comp_su_t16, competence_occupation__real_incident_necessity, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(comp_su_t24, competence_occupation__real_incident_necessity, suppression_requirement, 24, 0.25).
narrative_ontology:measurement(comp_su_t32, competence_occupation__real_incident_necessity, suppression_requirement, 32, 0.25).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_occupation kernel, which decomposes into three structurally distinct claims about how high-reliability competence is maintained: real_incident_necessity (this file), simulation_sufficiency, and hybrid_occupation. Each reading carries a different epsilon, different stakeholder configurations, and different coordination/extraction profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
