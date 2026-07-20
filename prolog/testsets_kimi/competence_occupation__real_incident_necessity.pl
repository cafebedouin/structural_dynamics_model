% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Catastrophic Incident Necessity for Competence Occupation
 *   domain: organizational/safety/competence
 *
 * SUMMARY:
 *   This constraint instantiates the real_incident_necessity reading of the
 *   competence_occupation kernel in high-reliability organization theory.
 *   Under this reading, the rarity of catastrophic events in domains such as
 *   nuclear power, aviation, and process control creates an irreducible
 *   structural limit: authentic competence â the fully occupied competence
 *   kernel â can only be achieved and verified under the existential
 *   conditions of actual catastrophe. Simulation, procedural drill, and
 *   refresher training are viewed as structurally insufficient because they
 *   lack the stakes, uncertainty, and embodied stress of real failure. There
 *   is no viable beneficiary structure: catastrophes are universally
 *   unacceptable outcomes, not rents to be captured. The constraint is tragic
 *   rather than extractive â a natural feature of rare-event domains that
 *   imposes a competence-maintenance burden on operators and organizations
 *   without transferring value to any party.
 *
 * KEY AGENTS:
 *   - hro_operators: Primary structural targets (moderate/constrained) â must maintain readiness for events they cannot authentically practice; bear the psychological and operational burden of the rarity trap.
 *   - safety_researchers: Analytical observers (analytical/analytical) â document the rarity-competence dilemma and contest or defend the incident-necessity claim through empirical study.
 *   - simulation_technology_sector: Agenda setters for the sibling reading who are structurally designated insufficient under this doctrine, but who are not extractive victims because no party captures the avoided cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.65).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.1).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Catastrophic Incident Necessity for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety/competence").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'c8c51f12-55f4-43d8-993b-7973be7429de').
narrative_ontology:cs_kernel_codification('c8c51f12-55f4-43d8-993b-7973be7429de', formalized).
narrative_ontology:cs_authority_grounding('c8c51f12-55f4-43d8-993b-7973be7429de', practice).
narrative_ontology:cs_interpretation_layer_present('c8c51f12-55f4-43d8-993b-7973be7429de').
narrative_ontology:cs_reading_relation('c8c51f12-55f4-43d8-993b-7973be7429de', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('c8c51f12-55f4-43d8-993b-7973be7429de', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('c8c51f12-55f4-43d8-993b-7973be7429de', foundational, catastrophic_incident_as_necessary_condition).
narrative_ontology:cs_axiom_status(catastrophic_incident_as_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('c8c51f12-55f4-43d8-993b-7973be7429de', catastrophic_incident_as_necessary_condition, empirically_contingent).
narrative_ontology:cs_axiom('c8c51f12-55f4-43d8-993b-7973be7429de', foundational, simulation_fidelity_insufficient_for_existential_authenticity).
narrative_ontology:cs_axiom_status(simulation_fidelity_insufficient_for_existential_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('c8c51f12-55f4-43d8-993b-7973be7429de', simulation_fidelity_insufficient_for_existential_authenticity, empirically_contingent).
narrative_ontology:cs_reference_frame('c8c51f12-55f4-43d8-993b-7973be7429de', incident_based_competence_authority).
narrative_ontology:cs_drift_state('c8c51f12-55f4-43d8-993b-7973be7429de', contemporary_simulation_saturated_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c8c51f12-55f4-43d8-993b-7973be7429de', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the maintenance of operational competence in domains where the target event is too rare to permit naturalistic practice, by establishing that competence verification is tied to the occurrence of catastrophic incidents.
% TRANSFER_FUNCTION: Moves the burden of authentic competence verification from routine, safe training environments to the occurrence of unacceptable catastrophic events; moves risk from training-system inadequacy to potential operational system failure.
% ABSENT_VOICES: Proponents of high-fidelity simulation who argue that existential fidelity can be technologically achieved; operators who have maintained effective performance without ever experiencing a real catastrophe; victims of incidents that were tacitly treated as organizational learning opportunities.
% DISAPPEARANCE_RATIONALE: If the constraint vanished â if it became false that only catastrophes maintain competence â simulation advocates would argue HROs could finally achieve reliable competence without unacceptable losses. Defenders of the reading would counter that eliminating the doctrine does not eliminate the underlying rarity problem, it merely obscures it behind inadequate training. The world would rearrange around simulation investment, but whether competence would genuinely hold is disputed.
% FOUNDING_PROBLEM: Competence decay and atrophy in rare-event operational domains where operators lack opportunities to practice under authentic high-stakes conditions.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety researchers and regulatory investigators attest to the rarity-competence dilemma from seats that do not capture gains from either simulation sales or incident-justified budget allocations; high-reliability organization researchers document the problem from an analytical remove.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, contested).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.65 because the constraint imposes a severe intrinsic cost: organizations and operators must either accept degraded competence or accept catastrophic events as their only authentic training. This is not rent-seeking but a tragic structural burden. Suppression is low (0.10) because the constraint persists without active enforcement â it is a claimed natural limit, not a constructed barrier. Theater ratio is negligible (0.05) because there is little performative maintenance; the doctrine is either sincerely held as empirical truth or silently suffered as a domain feature. Accessibility collapse is very high (0.92) because, under this reading, once the existential fidelity gap of simulation is understood, alternatives to incident-based competence collapse completely. Resistance is moderate (0.40) because the simulation_sufficiency and hybrid_occupation readings actively contest the claim, representing genuine epistemic opposition rather than suppressed victimhood.
 *
 * PERSPECTIVAL GAP:
 *   From the operator seat, the constraint is a terrifying structural reality: they may be called upon to perform procedures they have never practiced under authentic conditions. From the analytical researcher seat, it is a contested empirical hypothesis about transfer of training. From the simulation-industry seat (a sibling-reading agenda-setter), it is an illegitimate doctrinal barrier. The engine computes these divergences from the structural data: operators sit at high directionality (bearing the cost of the rarity trap) while researchers sit near symmetric (observing without bearing or capturing the cost).
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary declarations are authored because, under this reading, there is no party that captures value from the constraint's operation â catastrophes are unacceptable, and the constraint's persistence is independent of anyone's defense of it. Victims are similarly absent because the cost is diffuse and intrinsic to the domain rather than targeted extraction. The directionality derivation therefore produces no concentrated beneficiary or victim seats; the constraint operates as a universal condition.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as mountain prevents mislabeling this constraint as a snare or piton. There is no agenda-setter who profits from maintaining the doctrine, no concentrated beneficiary capturing avoided training investment, and no enforcement apparatus suppressing simulation alternatives. If training administrators cited this doctrine to justify slashing simulation budgets while capturing the savings, the constraint would shift toward tangled rope or snare. Under the current structural data, no such capture is present â the doctrine is either sincerely held or tragically endured, not instrumentalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_incident_necessity_vs_contingency,
    'Is the necessity of actual catastrophic incidents for competence occupation an irreducible epistemic property of high-risk domains, or a contingent artifact of insufficient simulation fidelity?',
    'Longitudinal performance studies comparing operator decision-making in actual incidents versus high-fidelity simulation; convergent evidence from multiple HRO domains regarding transfer of training.',
    'If contingent on technology, this constraint is a false summit (constructed doctrine treated as natural law); if irreducible, the mountain classification holds and the sibling readings are well-intentioned but structurally insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_incident_necessity_vs_contingency, empirical, 'Whether the incident-necessity claim is natural or constructed.').

omega_variable(
    kernel_reading_contest_location,
    'Does the disagreement between real_incident_necessity and its sibling readings reside in empirical facts about cognition, in normative claims about acceptable risk, or in institutional interests in training modalities?',
    'Discourse analysis of HRO training budget justifications; epistemic network mapping of citation patterns between safety researchers and simulation vendors.',
    'If the disagreement is empirical, the kernel resolves toward whichever reading the evidence supports. If normative or interested, the kernel remains a commitment system with divergent authority structures rather than a resolvable empirical question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locates the kernel contest in empirical, normative, or institutional space.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_real_inc_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(comp_real_inc_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.05).
narrative_ontology:measurement(comp_real_inc_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.05).
narrative_ontology:measurement(comp_real_inc_tr_t30, competence_occupation__real_incident_necessity, theater_ratio, 30, 0.05).
narrative_ontology:measurement(comp_real_inc_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.05).

% Extraction over time
narrative_ontology:measurement(comp_real_inc_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(comp_real_inc_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(comp_real_inc_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(comp_real_inc_be_t30, competence_occupation__real_incident_necessity, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(comp_real_inc_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_occupation__real_incident_necessity, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_occupation kernel, instantiating the real_incident_necessity position against simulation_sufficiency and hybrid_occupation readings. The kernel decomposes into multiple constraints because the observable (what maintains competence) is contested and each reading carries a distinct epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
